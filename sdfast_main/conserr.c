/* Copyright 2026 Michael Sherman
 * Copyright 1989-2025 PTC Inc.; 1984-1988 Symbolic Dynamics, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"

/*
 * This file contains routines which generate the equations for
 * constraint errors.  There are separate equations for position,
 * velocity, and acceleration constraint errors.
 *
 * We deal with several kinds of constraints here:
 *
 *    1. Prescribed motion constraints
 *    2. Loop joint constraints
 *    3. User constraints
 *
 * The ordering of the constraint errors in perr, verr and aerr is:
 *
 * perr:  <---- np ---> <--- nlc --> <--- nu --->
 * verr: | Pres. const | Loop const | User const |
 * aerr:  --------------------------------------- 
 *           ^              ^           ^
 *     PresM |       FirstM |    Mindex |
 *
 * PresM, FirstM, and Mindex are in SysI and provide the needed indices
 * into the error arrays.  This ordering is the same for the multipliers,
 * of which there is one per constraint.
 *
 * Position and velocity errors for loops and user constraints are 
 * calculated in SDSTATE.  P&V errors for prescribed motion constraints
 * has to be pushed off into SDPERR and SDVERR, however, since the user
 * can call SDPRESVEL and SDPRESPOS *after* calling SDSTATE.  Prescribed
 * acceleration errors are calculated in SDAERR although they could have 
 * been done in SDRHS.
 *
 *
 * The following definitions are used by the routines in this file:
 *
 * We have two bodies, `i' and `o' for each loop joint `j'.  Each body has 
 * two vectors provided by the user (or defaulted) and a third calculated 
 * vector.  The vectors are
 *
 *   body i:   ipin    iref    iperp (= ipin X iref) 
 *   body o:   opin    oref    operp (= opin X oref)  
 *
 * In addition, each body has a vector which locates the hinge point:
 *
 *   body i:   itj
 *   body o:   btj
 * 
 * We have also already computed the direction cosine between bodies
 * i and o, in symbol Cio, that is,  i o.  We use the transpose of this, o i
 *                                    C                                   C
 * frequently, since the errors are reported in the inboard (i) frame.
 *
 * Although the specific constraint error calculations vary for each joint
 * type, there are many common terms in the equations.  We allocate a
 * big pile of nl X 3 vector temporaries, called vt1, vt2, etc.  The meaning 
 * of each of these is the same for any joint type in which it is used.
 * Not all temporaries are used by all joint types, and unused ones are
 * not calculated.  But not all uses of these temporaries are in the
 * constraint error calculations -- in
 * some cases a temporary is used in loop pseudocoordinate calculations
 * but not used in constraint error calculations.
 */

/* 
 * COMPUTE_lptemps
 *
 * These are vector temporaries which require only position information.
 *
 *  Temp   Formula                      Joint Types
 *
 *  vt1  = opin * Coi                    pin,u,gim,sli,cyl,pla,weld,bear,bush
 *  vt6  = rnb[i] - rnb[o]                pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *  vt7  = btj * Coi                    pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *  vt10 = oref * Coi                    sli,weld,gim,bear,bush
 *  vt10c = vt7 - vt6 * Cni         pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *  vt11 = itj - vt10c              pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *
 * Note: in the above, we must watch carefully for the ground body since
 * rnb and `Cni' (really cnb) are not available for ground.
 *
 * This is an Order(N) calculation, where N is the number of loop joints.
 */
void COMPUTE_lptemps(FILE *F)
{
    register Index_t j,inb,outb,ip,op;
    JointKind_t jk;
    expr Coix;
    expr vt1x,vt6x,vt7x,vt10x,vt10cx,vt11x;

    vt1x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt6x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt7x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt10x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt10cx = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt11x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        outb = SysI.LoopConst[j].jnt.OutbBody;
        /* pseudo-body numbers for inb and outb */
        if (!rbod_is_gnd(inb)) 
            ip = SysI.LastDOF[inb];        
        if (!rbod_is_gnd(outb)) 
            op = SysI.LastDOF[outb];
        jk = SysI.LoopConst[j].jnt.JointKind;
        Coix = INUSE(TRANSPOSE(VAL1(Cio,j)));

        /*vt1*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cSlidingJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
              FLUSH_VEC(F,vt1,j,vt1x, MATMUL(VAL1(opin,j),Coix));
              break;
        }
        /*vt6, vt7, vt10c, vt11*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt6,j,vt6x, 
                  SUB(inb ==cGroundBody ? VECTOR_ZERO() : VAL1(rnb, inb),
                      outb==cGroundBody ? VECTOR_ZERO() : VAL1(rnb, outb)));
              FLUSH_VEC(F,vt7,j,vt7x, MATMUL(VAL1(SysI.lbtj,j),Coix));
              FLUSH_VEC(F,vt10c,j,vt10cx, 
                  SUB(INDX(vt7x,j),
                      MATMUL(INDX(vt6x,j),
                             rbod_is_gnd(inb) ? 
                             MATRIX_IDENT() : VAL1(cnk,ip))));
              FLUSH_VEC(F,vt11,j,vt11x, 
                  SUB(VAL1(SysI.litj,j),INDX(vt10cx,j)));
              break;
        }
        /*vt10*/
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
            case c3dJoint:
            case cBearingJoint:
            case cBushingJoint:
              FLUSH_VEC(F,vt10,j,vt10x, MATMUL(VAL1(oref,j),Coix));
              break;
        }
        DISPOSE_EXPR(UNUSE(Coix));
    }

    ASSIGN(vt1, UNUSE(vt1x));
    ASSIGN(vt6, UNUSE(vt6x));
    ASSIGN(vt7, UNUSE(vt7x));
    ASSIGN(vt10, UNUSE(vt10x));
    ASSIGN(vt10c, UNUSE(vt10cx));
    ASSIGN(vt11, UNUSE(vt11x));
}

/* 
 * COMPUTE_lvtemps
 *
 * These are vector temporaries which require position and velocity information.
 *
 *  Temp   Formula                      Joint Types
 *
 *  vt2  = wk[i] X iref             pin,sli,cyl,pla,weld,bear,bush,6d
 *  vt2a = wk[i] X ipin2            bear,bush
 *  vt3  = wk[o] X opin             pin,u,gim,sli,cyl,pla,weld,bear,bush
 *  vt4  = vt3 * Coi                    pin,u,gim,sli,cyl,pla,weld,bear,bush
 *  vt5  = wk[i] X iperp            pin,sli,cyl,pla,weld,bear,bush,6d
 *  vt8  = wk[o] X btj                    pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *  vt9  = vnb[i] - vnb[o]            pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *  vt10a = wk[o] X oref            pin,u,gim,sli,cyl,pla,weld,bear,bush
 *  vt10b = vt10a * Coi             sli,weld
 *  vt12 = vt9 * Cni + wk[i] X itj  pin,u,gim,ball,sli,cyl,pla,weld,bear,bush,6d
 *                     - vt8 * Coi
 *  vt13 = wk[i] X ipin             u,gim,sli,cyl,pla,bear,bush,6d
 *  vt13a = wk[o] X operp            pin,u,gim,sli,cyl,pla,weld,bear,bush
 *
 * Note: in the above, we must watch carefully for the ground body since
 * wk, vnb, and `Cni' (really cnb) are not available for ground.
 *
 * This is an Order(N) calculation, where N is the number of loop joints.
 */
void COMPUTE_lvtemps(FILE *F)
{
    register Index_t j,inb,outb,ip,op;
    JointKind_t jk;
    expr Coix;
    expr vt2x,vt2ax,vt3x,vt4x,vt5x,vt8x,vt9x,vt12x,
         vt13x,vt10ax,vt10bx,vt13ax;

    vt2x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt2ax = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt3x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt4x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt5x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt8x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt9x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt10ax = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt10bx = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt12x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt13x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt13ax = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        outb = SysI.LoopConst[j].jnt.OutbBody;
        /* pseudo-body numbers for inb and outb */
        if (!rbod_is_gnd(inb)) 
            ip = SysI.LastDOF[inb];        
        if (!rbod_is_gnd(outb)) 
            op = SysI.LastDOF[outb];
        jk = SysI.LoopConst[j].jnt.JointKind;
        Coix = INUSE(TRANSPOSE(VAL1(Cio,j)));

        /*vt3, vt4, vt10a, vt13a*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cSlidingJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
              FLUSH_VEC(F,vt3,j,vt3x, 
                        rbod_is_gnd(outb) ? 
                        VECTOR_ZERO() : CROSS(VAL1(wk,op),VAL1(opin,j)));
              FLUSH_VEC(F,vt10a,j,vt10ax, 
                        rbod_is_gnd(outb) ? 
                        VECTOR_ZERO() : CROSS(VAL1(wk,op),VAL1(oref,j)));
              FLUSH_VEC(F,vt13a,j,vt13ax, 
                        rbod_is_gnd(outb) ? 
                        VECTOR_ZERO() : CROSS(VAL1(wk,op),VAL1(operp,j)));
              FLUSH_VEC(F,vt4,j,vt4x, MATMUL(INDX(vt3x,j),Coix));
              break;
        }
        /*vt2, vt5*/
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt2,j,vt2x, 
                        rbod_is_gnd(inb) ?
                        VECTOR_ZERO() : CROSS(VAL1(wk,ip),VAL1(iref,j)));
              FLUSH_VEC(F,vt5,j,vt5x, 
                        rbod_is_gnd(inb) ?
                        VECTOR_ZERO() : CROSS(VAL1(wk,ip),VAL1(iperp,j)));
              break;
        }
        /* vt2a */
        switch(jk) {
            case cBearingJoint:
            case cBushingJoint:
              FLUSH_VEC(F,vt2a,j,vt2ax, 
                        rbod_is_gnd(inb) ?
                        VECTOR_ZERO() : CROSS(VAL1(wk,ip),VAL1(ipin2,j)));
              break;

        }
        /*vt8, vt9, vt12*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt8,j,vt8x, 
                        rbod_is_gnd(outb) ?
                        VECTOR_ZERO() : CROSS(VAL1(wk,op),VAL1(SysI.lbtj,j)));
              FLUSH_VEC(F,vt9,j,vt9x, 
                  SUB(inb ==cGroundBody ? VECTOR_ZERO() : VAL1(vnb, inb),
                      outb==cGroundBody ? VECTOR_ZERO() : VAL1(vnb, outb)));
              FLUSH_VEC(F,vt12,j,vt12x, 
                  ADD(MATMUL(INDX(vt9x,j),
                             rbod_is_gnd(inb) ? 
                             MATRIX_IDENT() : VAL1(cnk,ip)),
                      SUB(rbod_is_gnd(inb) ? VECTOR_ZERO()
                          : CROSS(VAL1(wk,ip), VAL1(SysI.litj,j)),
                          MATMUL(INDX(vt8x,j),Coix))));
              break;
        }
        /*vt10b*/
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
              FLUSH_VEC(F,vt10b,j,vt10bx, MATMUL(INDX(vt10ax,j),Coix));
              break;
        }
        /*vt13*/
        switch(jk) {
            case cUjoint:
            case c3dJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt13,j,vt13x, 
                        rbod_is_gnd(inb) ?
                        VECTOR_ZERO() : CROSS(VAL1(wk,ip),VAL1(ipin,j)));
              break;
        }
        DISPOSE_EXPR(UNUSE(Coix));
    }

    ASSIGN(vt2, UNUSE(vt2x));
    ASSIGN(vt2a, UNUSE(vt2ax));
    ASSIGN(vt3, UNUSE(vt3x));
    ASSIGN(vt4, UNUSE(vt4x));
    ASSIGN(vt5, UNUSE(vt5x));
    ASSIGN(vt8, UNUSE(vt8x));
    ASSIGN(vt9, UNUSE(vt9x));
    ASSIGN(vt10a, UNUSE(vt10ax));
    ASSIGN(vt10b, UNUSE(vt10bx));
    ASSIGN(vt12, UNUSE(vt12x));
    ASSIGN(vt13, UNUSE(vt13x));
    ASSIGN(vt13a, UNUSE(vt13ax));
}

/*
 * Compute position and velocity errors in loop constraints.
 *
 * These two routines make heavy use of pre-calculated vector temporaries in
 * vt1, vt2, etc.  With the definitions of these temporaries as calculated
 * in COMPUTE_l[pv]temps, perr's and verr's are defined as follows.  The 
 * ordering is the same as the multipliers associated with the joint.  
 * The first one actually starts at SysI.FirstM[j] for joint j.
 *
 * Joint  Indx   Perr                  Verr
 *
 * pin       1   iref * vt1            vt2*vt1 + iref*vt4
 *           2   iperp * vt1           vt5*vt1 + iperp*vt4
 *           3   [1,0,0] * vt11        (wk[i] X [1,0,0])*vt11 + [1,0,0]*vt12
 *           4   [0,1,0] * vt11        (wk[i] X [0,1,0])*vt11 + [0,1,0]*vt12
 *           5   [0,0,1] * vt11        (wk[i] X [0,0,1])*vt11 + [0,0,1]*vt12
 *
 * slider    1   same as pin 1
 *           2   same as pin 2
 *           3   iperp * vt10          vt5*vt10 + iperp*vt10b
 *           4   iref * vt11           vt2*vt11 + iref*vt12
 *           5   iperp * vt11          vt5*vt11 + iperp*vt12
 *
 * ujoint    1   ipin * vt1            vt13*vt1 + ipin*vt4
 *           2   same as pin 3
 *           3   same as pin 4
 *           4   same as pin 5
 *
 * ball      1   same as pin 3
 * gimbal    2   same as pin 4
 *           3   same as pin 5
 *
 * cyl       1   same as pin 1
 *           2   same as pin 2
 *           3   same as slider 4
 *           4   same as slider 5
 *
 * planar    1   same as pin 1
 *           2   same as pin 2
 *           3   ipin * vt11           vt13*vt11 + ipin*vt12
 *
 * bearing   1   ipin2 * vt11          vt2a*vt11 + ipin2*vt12
 *           2   same as slider 5
 *
 * weld      1   same as pin 1
 *           2   same as pin 2
 *           3   same as slider 3
 *           4   same as pin 3
 *           5   same as pin 4
 *           6   same as pin 5
 *
 * Note that a 6-DOF joint (sixdof or bushing) has no multipliers, so does not 
 * require any error equations to be generated here.
 *
 * These are Order(N) calculations, where N is the number of loop joints.
 */

/*
 * Compute position constraint errors.  This routine uses only the
 * vector temporaries calculated in COMPUTE_lptemps.
 */
void COMPUTE_lperr(expr perrx)
{
    register Index_t i,j,m;
    JointKind_t jk;

    for (j = 0; j < SysI.nl; j++) {
        jk = SysI.LoopConst[j].jnt.JointKind;
        m = SysI.FirstM[j];

        /* First get everyone whose 1st 2 constraints look like the
           two pin joint orientation constraints. */
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
                SINDX(perrx,m, DOT(VAL1(iref,j),VAL1(vt1,j)));
                m++;
                SINDX(perrx,m, DOT(VAL1(iperp,j),VAL1(vt1,j)));
                m++;
                break;
        }

        /* Finish off the orientation constraints. */
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
                SINDX(perrx,m, DOT(VAL1(iperp,j),VAL1(vt10,j)));
                m++;
                break;
            case cUjoint:
                SINDX(perrx,m, DOT(VAL1(ipin,j),VAL1(vt1,j)));
                m++;
                break;
        }

        /* Now the position constraints. */
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cWeldJoint:
                /* Joints with no sliders have same 3 position constraints. */
                for (i=0; i<3; i++)
                    SINDX(perrx,m+i, INDX(VAL1(vt11,j),i));
                m += 3;
                break;

            /* Joints with 1 slider have 2 position constraints, nearly 
             * identical.  The middle pin is ipin2 for bearings, however,
             * but iref for sliders and cylinders.  The last constraint
             * is the same.
             */
            case cSlidingJoint:
            case cCylJoint:
                SINDX(perrx,m, DOT(VAL1(iref,j),VAL1(vt11,j)));
                m++;
                goto lastPosConst;

            case cBearingJoint:
                SINDX(perrx,m, DOT(VAL1(ipin2,j),VAL1(vt11,j)));
                m++;
                goto lastPosConst;

            lastPosConst:
                SINDX(perrx,m, DOT(VAL1(iperp,j),VAL1(vt11,j)));
                m++;
                break;

            case cPlanarJoint:
                /* Joint with 2 sliders has one position constraint. */
                SINDX(perrx,m, DOT(VAL1(ipin,j),VAL1(vt11,j)));
                m++;
                break;
        }
    }
}

/*
 * Compute position constraint errors.  This routine uses temporaries
 * from both COMPUTE_lptemps and _lvtemps.
 */
void COMPUTE_lverr(expr verrx)
{
    register Index_t i,j,inb,ip,m;
    JointKind_t jk;

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        /* get pseudo-body number for inb */
        if (!rbod_is_gnd(inb))
            ip = SysI.LastDOF[inb];        
        jk = SysI.LoopConst[j].jnt.JointKind;
        m = SysI.FirstM[j];

        /* First get everyone whose 1st 2 constraints look like the
           two pin joint orientation constraints. */
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
                SINDX(verrx,m, ADD(DOT(VAL1(vt2,j),VAL1(vt1,j)),
                                   DOT(VAL1(iref,j),VAL1(vt4,j))));
                m++;
                SINDX(verrx,m, ADD(DOT(VAL1(vt5,j),VAL1(vt1,j)),
                                   DOT(VAL1(iperp,j),VAL1(vt4,j))));
                m++;
                break;
        }

        /* Finish off the orientation constraints. */
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
                SINDX(verrx,m, ADD(DOT(VAL1(vt5,j),VAL1(vt10,j)),
                                   DOT(VAL1(iperp,j),VAL1(vt10b,j))));
                m++;
                break;
            case cUjoint:
                SINDX(verrx,m, ADD(DOT(VAL1(vt13,j),VAL1(vt1,j)),
                                   DOT(VAL1(ipin,j),VAL1(vt4,j))));
                m++;
                break;
        }

        /* Now the position constraints. */
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cWeldJoint:
                /* Joints with no sliders have same 3 position constraints. */
                for (i=0; i<3; i++) {
                    if (rbod_is_gnd(inb))
                        SINDX(verrx,m+i, INDX(VAL1(vt12,j),i));
                    else
                        SINDX(verrx,m+i, 
                           ADD(DOT(CROSS(VAL1(wk,ip),
                                         INDX(MATRIX_IDENT(),i)),
                                   VAL1(vt11,j)),
                               INDX(VAL1(vt12,j),i)));
                }
                m += 3;
                break;

            /* Joints with 1 slider have 2 position constraints, nearly 
             * identical.  The middle pin is ipin2 for bearings, however,
             * but iref for sliders and cylinders.  The last constraint
             * is the same.
             */
            case cSlidingJoint:
            case cCylJoint:
                SINDX(verrx,m, ADD(DOT(VAL1(vt2,j),VAL1(vt11,j)),
                                   DOT(VAL1(iref,j),VAL1(vt12,j))));
                m++;
                goto lastPosConst;

            case cBearingJoint:
                SINDX(verrx,m, ADD(DOT(VAL1(vt2a,j),VAL1(vt11,j)),
                                   DOT(VAL1(ipin2,j),VAL1(vt12,j))));
                m++;
                goto lastPosConst;

            lastPosConst:
                SINDX(verrx,m, ADD(DOT(VAL1(vt5,j),VAL1(vt11,j)),
                                   DOT(VAL1(iperp,j),VAL1(vt12,j))));
                m++;
                break;

            case cPlanarJoint:
                /* Joint with 2 sliders has one position constraint. */
                SINDX(verrx,m, ADD(DOT(VAL1(vt13,j),VAL1(vt11,j)),
                                   DOT(VAL1(ipin,j),VAL1(vt12,j))));
                m++;
                break;
        }
    }
}

/*
 * COMPUTE_latemps
 *
 * See the top of this file for definitions.  In addition to the quantities
 * named there, we expect to have defined an acceleration dyadic for 
 * each body, dyad[i] and dyad[o].
 *
 * Following are additional vector temporaries required for acceleration 
 * errors.  Not all temporaries are used by all joint types, and unused ones are
 * not calculated.
 *
 *  Temp   Formula                     Joint Types
 *
 *  vt14 = vt1 * dyad[i]             pin,sli,cyl,pla,weld
 *  vt15 = dyad[o] * opin            pin,sli,u,cyl,pla,weld
 *  vt16 = vt15 * Coi                pin,sli,u,cyl,pla,weld
 *  vt17 = anb[i] - anb[o]           pin,sli,u,3d,ball,cyl,pla,weld,bear,bush,6d
 *  vt18 = dyad[o] * btj             pin,sli,u,3d,ball,cyl,pla,weld,bear,bush,6d
 *  vt19 = vt17 * Cni + dyad[i]*itj  pin,sli,u,3d,ball,cyl,pla,weld,bear,bush,6d
 *                      - vt18 * Coi     
 *  vt20 = vt10 * dyad[i]             sli,weld
 *  vt21 = dyad[o] * oref             sli,weld
 *  vt22 = vt21 * Coi                     sli,weld
 *  vt23 = vt1 * dyad[i]             u
 *  vt24 = onk[o] - onk[i]*Cio       pin,u,gimbal,ball,cyl,pla,bear,bush,6d
 *  vt25 = dyad[i]*ipin                     sli,cyl,pla,bear,bush,6d
 *  vt26 = dyad[i]*iref              sli,cyl,pla,bear,bush,6d
 *  vt26a= dyad[i]*ipin2             bear,bush
 *  vt27 = dyad[i]*iperp             sli,cyl,pla,bear,bush,6d
 *
 * Note: in the above, we must watch carefully for the ground body since
 * dyad, anb and `Cni' (really cnb) are not available for ground.
 *
 * This is an Order(N) calculation, where N is the number of loop joints.
 */
void COMPUTE_latemps(FILE *F)
{
    register Index_t j,inb,outb,ip,op;
    JointKind_t jk;
    expr Coix;
    expr vt14x,vt15x,vt16x,vt17x,vt18x,vt19x,vt20x,vt21x,vt22x,vt23x,
         vt24x,vt25x,vt26x,vt26ax,vt27x;

    vt14x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt15x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt16x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt17x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt18x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt19x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt20x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt21x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt22x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt23x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt24x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt25x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt26x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt26ax = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));
    vt27x = INUSE(NEW_1dARRAY(cVectorVal, SysI.nl));

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        outb = SysI.LoopConst[j].jnt.OutbBody;
        /* get pseudo-body numbers for inb and outb */
        if (!rbod_is_gnd(inb))
            ip = SysI.LastDOF[inb];        
        if (!rbod_is_gnd(outb))
            op = SysI.LastDOF[outb];
        jk = SysI.LoopConst[j].jnt.JointKind;
        Coix = INUSE(TRANSPOSE(VAL1(Cio,j)));

        /*vt14*/
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
              FLUSH_VEC(F,vt14,j,vt14x, 
                        rbod_is_gnd(inb) ? 
                        VECTOR_ZERO() : MATMUL(VAL1(vt1,j),VAL1(dyad,inb)));
              break;
        }
        /*vt15, vt16*/
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cUjoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
              FLUSH_VEC(F,vt15,j,vt15x, 
                        rbod_is_gnd(outb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,outb),VAL1(opin,j)));
              FLUSH_VEC(F,vt16,j,vt16x, 
                        MATMUL(INDX(vt15x,j),Coix));
              break;
        }
        /*vt17, vt18, vt19*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt17,j,vt17x, 
                  SUB(inb ==cGroundBody ? VECTOR_ZERO() : VAL1(anb, inb),
                      outb==cGroundBody ? VECTOR_ZERO() : VAL1(anb, outb)));
              FLUSH_VEC(F,vt18,j,vt18x, 
                        rbod_is_gnd(outb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,outb),VAL1(SysI.lbtj,j)));
              FLUSH_VEC(F,vt19,j,vt19x, 
                        rbod_is_gnd(inb) ? 
                        SUB(INDX(vt17x,j),MATMUL(INDX(vt18x,j),Coix))
                        : ADD(MATMUL(INDX(vt17x,j),VAL1(cnk,ip)),
                              SUB(MATMUL(VAL1(dyad,inb),VAL1(SysI.litj,j)),
                                  MATMUL(INDX(vt18x,j),Coix))));
              break;
        }
        /*vt20, vt21, vt22*/
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
              FLUSH_VEC(F,vt20,j,vt20x, 
                        rbod_is_gnd(inb) ? 
                        VECTOR_ZERO() : MATMUL(VAL1(vt10,j),VAL1(dyad,inb)));
              FLUSH_VEC(F,vt21,j,vt21x, 
                        rbod_is_gnd(outb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,outb),VAL1(oref,j)));
              FLUSH_VEC(F,vt22,j,vt22x, 
                        MATMUL(INDX(vt21x,j),Coix));
              break;
        }
        /*vt23*/
        switch(jk) {
            case cUjoint:
              FLUSH_VEC(F,vt23,j,vt23x, 
                        rbod_is_gnd(inb) ? 
                        VECTOR_ZERO() : MATMUL(VAL1(vt1,j),VAL1(dyad,inb)));
              break;
        }
        /*vt24*/
        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt24,j,vt24x, 
                              rbod_is_gnd(inb) ? 
                          (rbod_is_gnd(outb)?VECTOR_ZERO():VAL1(onk,op))
                       :rbod_is_gnd(outb) ? NEG(MATMUL(VAL1(onk,ip),
                                                         VAL1(Cio,j)))
                       : SUB(VAL1(onk,op),MATMUL(VAL1(onk,ip),VAL1(Cio,j))));
              break;
        }
        /*vt25, vt26, vt27*/
        switch(jk) {
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cBearingJoint:
            case cBushingJoint:
            case c6dJoint:
              FLUSH_VEC(F,vt25,j,vt25x, 
                        rbod_is_gnd(inb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,inb),VAL1(ipin,j)));
              FLUSH_VEC(F,vt26,j,vt26x, 
                        rbod_is_gnd(inb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,inb),VAL1(iref,j)));
              FLUSH_VEC(F,vt27,j,vt27x, 
                        rbod_is_gnd(inb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,inb),VAL1(iperp,j)));
              break;
        }
        /*vt26*/
        switch(jk) {
            case cBearingJoint:
            case cBushingJoint:
              FLUSH_VEC(F,vt26a,j,vt26ax, 
                        rbod_is_gnd(inb) ? VECTOR_ZERO() 
                        : MATMUL(VAL1(dyad,inb),VAL1(ipin2,j)));
        }

        DISPOSE_EXPR(UNUSE(Coix));
    }

    ASSIGN(vt14, UNUSE(vt14x));
    ASSIGN(vt15, UNUSE(vt15x));
    ASSIGN(vt16, UNUSE(vt16x));
    ASSIGN(vt17, UNUSE(vt17x));
    ASSIGN(vt18, UNUSE(vt18x));
    ASSIGN(vt19, UNUSE(vt19x));
    ASSIGN(vt20, UNUSE(vt20x));
    ASSIGN(vt21, UNUSE(vt21x));
    ASSIGN(vt22, UNUSE(vt22x));
    ASSIGN(vt23, UNUSE(vt23x));
    ASSIGN(vt24, UNUSE(vt24x));
    ASSIGN(vt25, UNUSE(vt25x));
    ASSIGN(vt26, UNUSE(vt26x));
    ASSIGN(vt26a, UNUSE(vt26ax));
    ASSIGN(vt27, UNUSE(vt27x));
}

/*
 * COMPUTE_laerr
 *
 * Compute acceleration errors in loop constraints.
 *
 * With the additional vt* temporaries as defined above in COMPUTE_latemps,
 * aerr's are defined as follows.  The ordering is the
 * same as the multipliers associated with the joint.  The first one actually
 * starts at SysI.FirstM[j] for joint j.
 *
 * Joint  Indx   Aerr
 *
 * pin       1   vt14*iref  + 2*(vt2*vt4) + iref*vt16
 *           2   vt14*iperp + 2*(vt5*vt4) + iperp*vt16
 *           3   (dyad[i]*[1,0,0])*vt11 + 2*(wk[i]X[1,0,0])*vt12 + [1,0,0]*vt19
 *           4   (dyad[i]*[0,1,0])*vt11 + 2*(wk[i]X[0,1,0])*vt12 + [0,1,0]*vt19
 *           5   (dyad[i]*[0,0,1])*vt11 + 2*(wk[i]X[0,0,1])*vt12 + [0,0,1]*vt19
 *
 * slider    1   same as pin 1
 *           2   same as pin 2
 *           3   vt20*iperp    + 2*(vt5*vt10b)  + iperp*vt22
 *           4   vt26 + 2*(vt2*vt12) + iref*vt19
 *           5   vt27 + 2*(vt5*vt12) + iperp*vt19
 *
 * ujoint    1   vt23*ipin + 2*(vt13*vt4) + ipin*vt16
 *           2   same as pin 3
 *           3   same as pin 4
 *           4   same as pin 5
 *
 * ball      1   same as pin 3
 * gimbal    2   same as pin 4
 *           3   same as pin 5
 *
 * cyl       1   same as pin 1
 *           2   same as pin 2
 *           3   same as slider 4
 *           4   same as slider 5
 *
 * planar    1   same as pin 1
 *           2   same as pin 2
 *           3   vt25 + 2*(vt13*vt12) + ipin*vt19
 *
 * bearing   4   vt26a + 2*(vt2a*vt12) + ipin2*vt19
 *           2   same as slider 5
 *
 * weld      1   same as pin 1
 *           2   same as pin 2
 *           3   same as slider 3
 *           4   same as pin 3
 *           5   same as pin 4
 *           6   same as pin 5
 *
 * Note that a 6-DOF joint has no multipliers, so does not require any
 * equations to be generated here.
 *
 * This is an Order(N) calculation, where N is the number of loop joints.
 */
void COMPUTE_laerr(expr aerrx)
{
    register Index_t i,j,inb,ip,m;
    JointKind_t jk;
    expr temp;

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        /* get pseudo-body numbers for inb and outb */
        if (!rbod_is_gnd(inb))
            ip = SysI.LastDOF[inb];        
        jk = SysI.LoopConst[j].jnt.JointKind;
        m = SysI.FirstM[j];

        /* First get everyone whose 1st 2 constraints look like the
           two pin joint orientation constraints. */
        switch(jk) {
            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt14,j),VAL1(iref,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt2,j),VAL1(vt4,j))),
                                   DOT(VAL1(iref,j),VAL1(vt16,j)))));
                m++;
                SINDX(aerrx,m, ADD(DOT(VAL1(vt14,j),VAL1(iperp,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt5,j),VAL1(vt4,j))),
                                   DOT(VAL1(iperp,j),VAL1(vt16,j)))));
                m++;
                break;
        }

        /* Finish off the orientation constraints. */
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt20,j),VAL1(iperp,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt5,j),VAL1(vt10b,j))),
                                   DOT(VAL1(iperp,j),VAL1(vt22,j)))));
                m++;
                break;
            case cUjoint:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt23,j),VAL1(ipin,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt13,j),VAL1(vt4,j))),
                                   DOT(VAL1(ipin,j),VAL1(vt16,j)))));
                m++;
                break;
        }

        switch(jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
            case cWeldJoint:
                /* Joints with no sliders have same 3 position constraints. */
                for (i=0; i<3; i++) 
                    if (rbod_is_gnd(inb))
                        SINDX(aerrx,m+i, INDX(VAL1(vt19,j),i));
                    else {
                        temp = INUSE(INDX(MATRIX_IDENT(),i));
                        SINDX(aerrx,m+i, 
                            ADD(DOT(MATMUL(VAL1(dyad,inb),temp),VAL1(vt11,j)),
                            ADD(DOT(MUL(SC(2.0), CROSS(VAL1(wk,ip),temp)),
                                    VAL1(vt12,j)),
                                DOT(temp,VAL1(vt19,j)))));
                        DISPOSE_EXPR(UNUSE(temp));
                    }
                m += 3;
                break;

            /* Joints with 1 slider have 2 position constraints, nearly 
             * identical.  The middle pin is ipin2 for bearings, however,
             * but iref for sliders and cylinders.  The last constraint
             * is the same.
             */
            case cSlidingJoint:
            case cCylJoint:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt26,j),VAL1(vt11,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt2,j),VAL1(vt12,j))),
                                   DOT(VAL1(iref,j),VAL1(vt19,j)))));
                m++;
                goto lastPosConst;

            case cBearingJoint:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt26a,j),VAL1(vt11,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt2a,j),VAL1(vt12,j))),
                                   DOT(VAL1(ipin2,j),VAL1(vt19,j)))));
                m++;
                goto lastPosConst;

            lastPosConst:
                SINDX(aerrx,m, ADD(DOT(VAL1(vt27,j),VAL1(vt11,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt5,j),VAL1(vt12,j))),
                                   DOT(VAL1(iperp,j),VAL1(vt19,j)))));
                m++;
                break;

            case cPlanarJoint:
                /* Joint with 2 sliders has one position constraint. */
                SINDX(aerrx,m, ADD(DOT(VAL1(vt25,j),VAL1(vt11,j)),
                               ADD(MUL(SC(2.0),
                                       DOT(VAL1(vt13,j),VAL1(vt12,j))),
                                   DOT(VAL1(ipin,j),VAL1(vt19,j)))));
                m++;
                break;
        }
    }
}

/* COMPUTE_presperr
 *
 * Compute position errors in prescribed motion constraints.
 *
 * The information as to the correct position, if it is
 * available, was provided by the user in calls to sdprespos().
 * This routine just stuffs the data into the appropriate
 * global variables upos[s] or lpos[sl].
 *
 * Call this routine while generating SDPERR, before copying perr
 * to the output parameter.
 *
 * For loop joints, we reference the variable `lq' which is initialized
 * to internally-calculated loopq's in sdstate(), but which may have
 * been changed by a user's call to sdpsstate().
 */
void COMPUTE_presperr(FILE *F)
{
    int i,m;
    expr temp, perrx;
    char   str_flt0[20];

    perrx = VAL(perr);

    esprintf(str_flt0, "%r", 0.0); /* for use in `if' statement */

    m = 0; /* next prescribed motion multiplier */

    /* Prescribed motion in tree joints.  */
    for (i=0; i<SysI.s; i++) {
        if (!IS_ZERO(VAL1(SysI.pres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.pres,i)))
                SINDX(perrx, m, SUB(VAL1(q,i),VAL1(upos,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.pres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(VAL1(q,i),VAL1(upos,i));
                    PRINT_ASSN1(F, PRINTNAME(perr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(perr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(perrx, m, VREF1(perr,m));
            }
            m++;
        }
    }

    /* Prescribed motion in loop joints. */
    for (i=0; i<SysI.sl; i++) {
        if (!IS_ZERO(VAL1(SysI.lpres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.lpres,i)))
                SINDX(perrx, m, SUB(VAL1(lq,i),VAL1(lpos,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.lpres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(VAL1(lq,i),VAL1(lpos,i));
                    PRINT_ASSN1(F, PRINTNAME(perr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(perr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(perrx, m, VREF1(perr,m));
            }
            m++;
        }
    }

    ASSIGN_CLN(F, perr, perrx);
}

/* COMPUTE_presverr
 *
 * Compute velocity errors in prescribed motion constraints.
 *
 * The information as to the correct velocity, if it is
 * available, was provided by the user in calls to sdpresvel()
 * which just stuffs the data into the appropriate
 * global variables uvel[s] or lvel[sl].
 *
 * Call this routine while generating SDVERR, before copying verr
 * to the output parameter.
 *
 * Current loop joint velocities are available in lux.
 */
void COMPUTE_presverr(FILE *F,
                 expr lux)
{
    int i,m;
    expr temp, verrx;
    char   str_flt0[20];

    verrx = VAL(verr);

    esprintf(str_flt0, "%r", 0.0); /* for use in `if' statement */

    m = 0; /* next prescribed motion multiplier */

    /* Prescribed motion in tree joints.  */
    for (i=0; i<SysI.s; i++) {
        if (!IS_ZERO(VAL1(SysI.pres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.pres,i)))
                SINDX(verrx, m, SUB(VAL1(u,i),VAL1(uvel,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.pres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(VAL1(u,i),VAL1(uvel,i));
                    PRINT_ASSN1(F, PRINTNAME(verr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(verr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(verrx, m, VREF1(verr,m));
            }
            m++;
        }
    }

    /* Prescribed motion in loop joints. */
    for (i=0; i<SysI.sl; i++) {
        if (!IS_ZERO(VAL1(SysI.lpres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.lpres,i)))
                SINDX(verrx, m, SUB(INDX(lux,i),VAL1(lvel,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.lpres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(INDX(lux,i),VAL1(lvel,i));
                    PRINT_ASSN1(F, PRINTNAME(verr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(verr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(verrx, m, VREF1(verr,m));
            }
            m++;
        }
    }

    ASSIGN_CLN(F, verr, verrx);
}

/*
 * Compute acceleration errors in prescribed motion constraints.
 *
 * The information as to the correct acceleration, is either defaulted
 * (to 0) or provided by the user in calls to sdpresacc()
 * which just stuffs the data into the appropriate
 * global variables uacc[s] or lacc[sl].
 *
 * Call this routine while generating SDAERR, before copying aerr
 * to the output parameter.
 *
 * Current loop joint accelerations are available in ludotx.
 */
void COMPUTE_presaerr(FILE *F,
                 expr ludotx)
{
    int i,m;
    expr temp, aerrx;
    char   str_flt0[20];

    aerrx = VAL(aerr);

    esprintf(str_flt0, "%r", 0.0); /* for use in `if' statement */

    m = 0; /* next prescribed motion multiplier */

    /* Prescribed motion in tree joints.  */
    for (i=0; i<SysI.s; i++) {
        if (!IS_ZERO(VAL1(SysI.pres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.pres,i)))
                SINDX(aerrx, m, SUB(VAL1(udot,i),VAL1(uacc,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.pres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(VAL1(udot,i),VAL1(uacc,i));
                    PRINT_ASSN1(F, PRINTNAME(aerr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(aerr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(aerrx, m, VREF1(aerr,m));
            }
            m++;
        }
    }

    /* Prescribed motion in loop joints. */
    for (i=0; i<SysI.sl; i++) {
        if (!IS_ZERO(VAL1(SysI.lpres,i))) {
            /* may be prescribed */
            if (IS_ONE(VAL1(SysI.lpres,i))) 
                SINDX(aerrx, m, SUB(INDX(ludotx,i),VAL1(lacc,i)));
            else {
                /* runtime prescribed */
                efprintf(F, Lang->stmt_if2_b);
                PRINT_E(F, VAL1(SysI.lpres,i));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    temp = SUB(INDX(ludotx,i),VAL1(lacc,i));
                    PRINT_ASSN1(F, PRINTNAME(aerr), m, temp);
                    DISPOSE_EXPR(temp);
                efprintf(F, Lang->stmt_if2_else);
                    PRINT_ASSN1(F, PRINTNAME(aerr), m, SCALAR_ZERO());
                efprintf(F, Lang->stmt_if2_e);
                SINDX(aerrx, m, VREF1(aerr,m));
            }
            m++;
        }
    }

    ASSIGN_CLN(F, aerr, aerrx);
}

/*
 * These routines simply return the contents of perr, verr, and aerr.
 * However, the entries for prescribed motion are calculated at
 * runtime.
 */
void PRINT_SDPERR(FILE *F)
{
    register int i;

    declare_proc(F, 0, "perr",
      VT_USER, &SysI.type_Arr_nc, "errs", 
      0);

    if (!SysI.nc) {
        PRINT_STUB(F);
        return;
    }

    efprintf(F, "%{\n\
Return position constraint errors.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY,
                ROU_sdperr, ERR_sdstateMustBeCalledFirst);

    /* Compute prescribed motion position errors. */
    COMPUTE_presperr(F);

    for (i=0; i<SysI.nc; i++)
        PRINT_ASSN1(F, "errs", i, VAL1(perr, i));
    efprintf(F, Lang->proc_end);
}

/* Lux is the expression containing the loop u's, neatly arranged
 * by loop hinge number.  This is used in calculating loop prescribed
 * motion velocity errors. 
 */
void PRINT_SDVERR(FILE *F,
             expr lux)
{
    register int i;

    declare_proc(F, 0, "verr",
      VT_USER, &SysI.type_Arr_nc, "errs", 
      0);

    if (!SysI.nc) {
        PRINT_STUB(F);
        return;
    }

    efprintf(F, "%{\n\
Return velocity constraint errors.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY,
                ROU_sdverr, ERR_sdstateMustBeCalledFirst);

    /* Compute prescribed motion velocity errors. */
    COMPUTE_presverr(F,lux);

    for (i=0; i<SysI.nc; i++)
        PRINT_ASSN1(F, "errs", i, VAL1(verr, i));
    efprintf(F, Lang->proc_end);
}

/* Ludotx is the expression containing the loop udot's, neatly arranged
 * by loop hinge number.  This is used in calculating loop prescribed
 * motion acceleration errors. 
 */
void PRINT_SDAERR(FILE *F,
             expr ludotx)
{
    register int i;

    declare_proc(F, 0, "aerr",
      VT_USER, &SysI.type_Arr_nc, "errs", 
      0);

    if (!SysI.nc) {
        PRINT_STUB(F);
        return;
    }

    efprintf(F, "%{\n\
Return acceleration constraint errors.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE,
                ROU_sdaerr, ERR_sdderivMustBeCalledFirst);

    /* Compute prescribed motion acceleration errors. */
    COMPUTE_presaerr(F, ludotx);

    for (i=0; i<SysI.nc; i++)
        PRINT_ASSN1(F, "errs", i, VAL1(aerr, i));
    efprintf(F, Lang->proc_end);
}
