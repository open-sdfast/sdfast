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

/*
 * Kinematics routines.
 *
 * These compute quantities which give translational and rotational positions, 
 * velocities, and accelerations of each pseudo body, and for each
 * realbody, with respect to 
 * ground.  The realbody quantities are needed by runtime routines like
 * sdpos(), so they must be assigned even if IS_SIMPLE.  Pseudobody values
 * need not be printed out as they are for internal consumption only.  In fact,
 * for onk's and ank's, we don't even compute the components associated with
 * non-realbody pseudobodies (to do so would be expensive).  Don't reference 
 * them!
 *
 * Note that all translational quantities are expressed in the ground frame
 * and that all rotational quantities are in the body-fixed frame.  (Although
 * they are measured with respect to ground.)
 *
 * System center-of-mass computation is stuck in this file as well, although
 * it doesn't strictly speaking belong here.
 *
 * At the end of the file we use the pseudobody quantities to compute the 
 * equivalent
 * kinematic variables for the realbody system.  Note that some of these are 
 * non-trivial when welds are involved since the realbody COM will not be 
 * collocated with any pseudobody COM.
 *
 *     cnb  -- orientation of body b w.r.t. ground
 *     rnb  -- location in ground of body b's COM
 *     wb   -- angular velocity of body b w.r.t. ground
 *     vnb  -- velocity in ground of body b's COM
 *     onb  -- angular acceleration of body b w.r.t. ground
 *     anb  -- acceleration in ground of body b's COM
 *
 * (sherm 2/7/96) Bug 13712 was a PROTECT>10000 error caused by a
 * many-weld system referring repeatedly to the same "wb" element.
 * To fix this, I made the "wb" variable PERM.  It never gets 
 * de-allocated anyway.  As a prophylactic measure, I made all the
 * long-lived, frequently-used quantities here PERM as well. 
 */

#include "sdfast.h"
#include "sdfaprot.h"

/* Returns true if passed-in real (not pseudo-) body number is ground
 * or is a body welded to ground.  The LastDOF for a body welded to
 * ground is always -1.
 *
 * For pseudobodies, just check if the body number is cGroundBody, since
 * welded real bodies do not produce pseudobodies.
 */
int
rbod_is_gnd(int bnum)
{
    return (bnum == cGroundBody || SysI.LastDOF[bnum] == -1);
}

/* COMPUTE_cnk
 *
 * Compute direction cosines giving orientation of each pseudobody 
 * with respect to ground.
 *
 *     cnk(k) = Cik(k),               inb(k) == ground
 *     cnk(k) = cnk(inb(k))*Cik(k)    inb(k) != ground
 *               k=0..s-1
 *
 * This is an Order(N) computation.
 */
void COMPUTE_cnk(FILE *F)
{
    register Index_t i,j,k,inb;
    expr temp, cnk_expr;

    if (SysI.s == 0)
        return;

    cnk_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;
        if (inb == cGroundBody)
            temp = VAL1(Cik,k);
        else
            temp = MATMUL(INDX(cnk_expr,inb),VAL1(Cik,k));

        FLUSH_MAT(F, cnk, k, cnk_expr, temp);
    }

    ASSIGN(cnk, PERM(cnk_expr));
}


/* COMPUTE_rnk
 *
 * Compute location of each pseudobody's mass center with respect to ground,
 * expressed in the ground frame.
 *
 * First, the ground pseudobody's mass center location is put in rnkg. Then:
 *
 *     rnk(k) = rnkg   + ri(k)         + rpk(k)*cnkT(k),     inb(k) == ground
 *     rnk(k) = rnk(i) + ri(k)*cnkT(i) + rpk(k)*cnkT(k),     inb(k) != ground   
 *                            k=0..s-1, i=inb(k), cnkT = TRANSPOSE(cnk)
 *
 * These variables must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rnk(FILE *F)
{
    register Index_t i,k,inb;
    expr temp, rnk_expr;

    ASSIGN_CLN(F, rnkg, NEG(VAL(SysI.psrcomg)));

    if (SysI.s == 0)
        return;

    rnk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;
        temp = MATMUL(VAL1(rpk,k), TRANSPOSE(VAL1(cnk,k)));
        if (inb == cGroundBody)
            temp = ADD(temp, ADD(VAL(rnkg), VAL1(SysI.psri,k)));
        else 
            temp = ADD(temp,
                       ADD(INDX(rnk_expr,inb),
                           MATMUL(VAL1(SysI.psri,k),
                                  TRANSPOSE(VAL1(cnk,inb)))));

        FLUSH_VEC(F, rnk, k, rnk_expr, temp);
    }

    ASSIGN(rnk, PERM(rnk_expr));
}


/* COMPUTE_wk
 *
 * Compute angular velocity of each pseudobody with respect to ground,
 * but expressed in the local body frame.
 *
 *     wk(k) = Wik(k),                     inb(k) == ground
 *     wk(k) = wk(i)*Cik(k) + Wik(k),      inb(k) != ground
 *                   k=0..s-1, i=inb(k)
 *
 * This is an Order(N) computation.
 */
void COMPUTE_wk(FILE *F)
{
    register Index_t i,k,inb;
    expr temp, wk_expr;

    if (SysI.s == 0)
        return;

    wk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;
        if (inb == cGroundBody)
            temp = VAL1(Wik,k);
        else
            temp = ADD(MATMUL(INDX(wk_expr,inb),VAL1(Cik,k)),
                       VAL1(Wik,k));

        FLUSH_VEC(F, wk, k, wk_expr, temp);
    }

    ASSIGN(wk, PERM(wk_expr));
}

/* COMPUTE_vnk
 *
 * Compute linear velocity of each pseudobody's mass center with 
 * respect to ground, and expressed in the ground frame.
 *
 *     vnk[k] =          (Vik[k] + wk[k] X rpk[k])*cnkT[k], inb(k) == ground
 *
 *     vnk[k] = vnk[i] + (Vik[k] + wk[k] X rpk[k])*cnkT[k]
 *                     + (wk[i] X ri[k])*cnkT[i],           inb(k) != ground
 *
 *              k=0..s-1, i=inb[k], cnkT=TRANSPOSE(cnk)
 *
 * We use two temporaries to avoid redundant calculations.  These should
 * already have been computed.
 *
 *     VikWkr[k] = Vik[k] + wk[k] X rpk[k]
 *     Wirk[k] = 0,                   inb(k) == ground
 *     Wirk[k] = wk[i] X ri[k],       inb(k) != ground
 * 
 * This is an Order(N) computation.
 */
void COMPUTE_vnk(FILE *F)
{
    register Index_t i,k,inb;
    expr temp, vnk_expr;

    if (SysI.s == 0)
        return;

    vnk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;

        /* compute vnk(k) */
        temp = MATMUL(VAL1(VikWkr,k),TRANSPOSE(VAL1(cnk,k)));
        if (inb != cGroundBody)
            temp = ADD(temp, ADD(INDX(vnk_expr,inb),
                                 MATMUL(VAL1(Wirk,k),
                                        TRANSPOSE(VAL1(cnk,inb)))));

        FLUSH_VEC(F, vnk, k, vnk_expr, temp);
    }

    ASSIGN(vnk, PERM(vnk_expr));
}

/* COMPUTE_onk
 *
 * Compute angular acceleration with respect to ground, but expressed
 * in the body local frame.
 *
 *     Onkb[k] =                  Wkk[k]*udot[k],    inb(k) == ground
 *     Onkb[k] = Onkb[i]*Cik[k] + Wkk[k]*udot[k],    inb(k) != ground
 *                  k=0..s-1, i=inb(k)
 *
 *     onk[k] = Onkb[k] + Otk[k]
 *
 * Note: we only need onk's for real bodies, not pseudobodies.  So despite
 * the fact that onk's are indexed by pseudobody number, we only calculate
 * the ones corresponding to real bodies.  Don't reference the other ones!
 *
 * This is an Order(N) computation.
 */
void COMPUTE_onk(FILE *F)
{
    register Index_t k,inb;
    expr temp, Onkb_expr, onk_expr;

    if (SysI.s == 0)
        return;

    Onkb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    onk_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' Onkb as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;

        temp = MUL(VAL1(Wkk,k),VREF1(udot,k));
        if (inb != cGroundBody)
            temp = ADD(temp, MATMUL(INDX(Onkb_expr,inb),VAL1(Cik,k)));

        FLUSH_VEC(F,Onkb,k,Onkb_expr, temp);
    }
    ASSIGN(Onkb, UNUSE(Onkb_expr));

    /* `clean' onk as we go. */
    for (k = 0; k < SysI.s; k++) {
        if (SysI.PseudoBodies[k].realbody) {
            temp = ADD(VAL1(Onkb,k), VAL1(Otk,k));
            FLUSH_VEC(F,onk,k,onk_expr, temp);
        } 
    }
    ASSIGN(onk, PERM(onk_expr));
}

/* COMPUTE_ank
 *
 * Compute linear acceleration of each pseudobody's mass center with respect 
 * to ground, expressed in the ground frame.
 *
 * Ankb[k] = Ankinb[k] + Ankbod[k]
 * Ankinb[k] = 0,                                         inb(k) == ground
 *           = { Ankb[i] + Onkb[i] X ri[k] } * Cik[k],    inb(k) != ground
 *
 * Ankbod[k] = Onkb[k] X rpk[k] + Vkk[k]*udot[k],         all k
 *
 * ank[k] = ( Ankb[k] + Atk[k] ) * TRANSPOSE(cnk,k)
 *
 * The following intermediate variable is used to avoid redundant 
 * computations:
 *
 * AOnkri[k] = Ankb[i] + Onkb[i] X ri[k],           inb(k) != ground
 * AnkAtk[k] = Ankb[k] + Atk[k],                    all k
 *
 * Note: we only need ank's for real bodies, not pseudobodies.  So despite
 * the fact that ank's are indexed by pseudobody number, we only calculate
 * the ones corresponding to real bodies.  Don't reference the other ones!
 *
 * This is an Order(N) computation.
 */
void COMPUTE_ank(FILE *F)
{
    register Index_t k,inb;
    expr Ankb_expr, ank_expr, AOnkri_expr, AnkAtk_expr;
    expr temp,  Ankinb, Ankbod;

    if (SysI.s == 0)
        return;

    AOnkri_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    Ankb_expr   = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    AnkAtk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    ank_expr    = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' AOnkri and Ankb as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;

        /* Compute Ankinb */

        if (inb == cGroundBody)
            Ankinb = VECTOR_ZERO();
        else {
            /* compute AOnkri */
            temp = ADD(INDX(Ankb_expr,inb),
                       CROSS(VAL1(Onkb,inb), VAL1(SysI.psri,k)));
            FLUSH_VEC(F,AOnkri,k,AOnkri_expr,temp);

            Ankinb = MATMUL(INDX(AOnkri_expr,k),VAL1(Cik,k));
        }

        /* compute Ankbod */

        Ankbod = ADD(CROSS(VAL1(Onkb,k),VAL1(rpk,k)),
                     MUL(VAL1(Vkk,k), VREF1(udot,k)));

        /* compute Ankb[k] */
        FLUSH_VEC(F,Ankb,k,Ankb_expr, ADD(Ankinb,Ankbod));
    }
    ASSIGN(AOnkri, UNUSE(AOnkri_expr));
    ASSIGN(Ankb, UNUSE(Ankb_expr));

    /* `clean' AnkAtk and ank as we go.  Make sure that for ank[k] we print
       out a value for any k representing a real body.  This is so that
       SDKINE can get the value out of global or common.  */
    for (k = 0; k < SysI.s; k++) {
        if (!SysI.PseudoBodies[k].realbody)
            continue;

        /* compute AnkAtk[k] */
        temp = ADD(VAL1(Ankb, k), VAL1(Atk, k));
        FLUSH_VEC(F,AnkAtk,k,AnkAtk_expr,temp);

        /* compute ank[k] */
        temp = MATMUL(INDX(AnkAtk_expr,k), TRANSPOSE(VAL1(cnk,k)));
        FLUSH_VEC(F,ank,k,ank_expr,temp);
    }
    ASSIGN(AnkAtk, UNUSE(AnkAtk_expr));
    ASSIGN(ank, PERM(ank_expr));
}

/* COMPUTE_com
 *
 * Compute location of the system center of mass, expressed in the ground
 * frame.  This is used by sdmom() and sdsys(), but not directly in sdderiv().
 * The value is stuck into common or global to allow these routines to get
 * access.  We always print out an assignment even if the com location is 
 * IS_SIMPLE, just to make sure the global value is valid.  But, IS_SIMPLE
 * values are retained in the com symbol as usual.  If sdmom() and sdsys()
 * are careful, they can make use of those values of com which are IS_CONST
 * without fear of referencing a variable to which they have no access.
 * 
 * This is an Order(N) computation.
 */
void COMPUTE_com(FILE *F)
{
    register Index_t i,k;
    expr com_expr;

    /* Start with the contribution from bodies welded to ground.  If there
     * aren't any, this will be zero.
     */
    com_expr = MUL(VAL(SysI.psmkg),NEG(VAL(SysI.psrcomg)));

    /* Then add in the effect of each composite pseudo body. */
    for (k = 0; k < SysI.s; k++) 
        com_expr = ADD(com_expr,MUL(VAL1(SysI.psmk,k),VAL1(rnk,k)));
    com_expr = MUL(DVD(SCALAR_ONE(),VAL(mtot)),com_expr);

    com_expr = INUSE(com_expr);
    for (i = 0; i < 3; i++) {
        PRINT_ASSN1(F, "com", i, INDX(com_expr,i));
        if (!IS_SIMPLE(INDX(com_expr,i)))
            SINDX(com_expr,i,VREF1(com,i));
    }

    ASSIGN(com, PERM(com_expr));
}

/* Realbody-oriented kinematics.
 *
 * Every element of each of these symbols must be output so that they
 * can be accessed numerically, except Cio and various temps.
 */

/* COMPUTE_cnb
 *
 * Compute direction cosines giving orientation of each realbody
 * with respect to ground.
 *
 * Depends on cnk having already been computed.
 *
 * Computation:
 *
 *     cnb[b] = identity matrix,              pseudobody(b) == ground
 *            = cnk[k],                     k=pseudobody(b) != ground
 *
 * This variable must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_cnb(FILE *F)
{
    register int b,k;
    expr cnb_expr;

    cnb_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1)
            FLUSH_MAT_ALL_SAVE(F, cnb, b, cnb_expr, MATRIX_IDENT());
        else
            FLUSH_MAT_ALL_SAVE(F, cnb, b, cnb_expr, VAL1(cnk,k));
    }

    ASSIGN(cnb, PERM(cnb_expr));
}

/* COMPUTE_Cio
 *
 * Compute direction cosines giving relative orientation between `inboard'
 * and `outboard' bodies in loop joints.
 *
 *     inb  = inboard realbody of loop joint k
 *     outb = outboard realbody of loop joint k
 *
 *     Cio[k] = TRANSPOSE(cnb[inb])*cnb[outb])
 *
 * If either body is ground, the identity matrix is used as its cnk.
 *
 * Cio is used like this:
 *
 *     <vec in inb frame>  * Cio[k]            ===> <vec in outb frame>
 *  or
 *     <vec in outb frame> * TRANSPOSE(Cio[k]) ===> <vec in inb frame>
 *
 * This is an Order(N) computation, with N=number of loop joints.
 */
void COMPUTE_Cio(FILE *F)
{
    int inb,outb,k;
    expr Cio_expr;

    Cio_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.nl));

    for (k = 0; k < SysI.nl; k++) {
        inb = SysI.LoopConst[k].jnt.InbBody;
        outb = SysI.LoopConst[k].jnt.OutbBody;
        SINDX(Cio_expr, k, 
            MATMUL(inb==cGroundBody  ? MATRIX_IDENT() 
                                     : TRANSPOSE(VAL1(cnb, inb)),
                   outb==cGroundBody ? MATRIX_IDENT() 
                                     : VAL1(cnb,outb)));
    }

    ASSIGN_CLN(F, Cio, UNUSE(Cio_expr));
}

/* COMPUTE_rnb
 *
 * Compute location of each realbody's mass center with respect to ground,
 * expressed in the ground frame.
 *
 * Depends on rnk and cnb having already been computed.
 *
 * Computation:
 *
 *     rnb[b] = rnkg + rcom[b],               pseudobody(b) == ground
 *            = rnk[k] + rcom[b]*cnbT(k),   k=pseudobody(b) != ground
 *
 *     cnbT = TRANSPOSE(cnb)
 *
 * This variable must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rnb(FILE *F)
{
    register int b,k;
    expr rnb_expr;

    rnb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1)
            FLUSH_VEC_ALL_SAVE(F, rnb, b, rnb_expr, 
                               ADD(VAL(rnkg), VAL1(SysI.rcom, b)));
        else
            FLUSH_VEC_ALL_SAVE(F, rnb, b, rnb_expr, 
                               ADD(VAL1(rnk,k),
                                   MATMUL(VAL1(SysI.rcom, b),
                                          TRANSPOSE(VAL1(cnb, b)))));
    }

    ASSIGN(rnb, PERM(rnb_expr));
}

/* COMPUTE_wb
 *
 * Compute angular velocities of each real body with respect to ground,
 * expressed in the body's frame.
 *
 * Depends on wk having already been computed.
 *
 * Computation:
 *
 *      wb[b] = zero,                         pseudobody(b) == ground
 *            = wk[k],                      k=pseudobody(b) != ground
 *
 * This variable must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_wb(FILE *F)
{
    register int b,k;
    expr wb_expr;

    wb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1)
            FLUSH_VEC_ALL_SAVE(F, wb, b, wb_expr, VECTOR_ZERO());
        else
            FLUSH_VEC_ALL_SAVE(F, wb, b, wb_expr, VAL1(wk, k));
    }

    ASSIGN(wb, PERM(wb_expr));
}

/* COMPUTE_wbtemps
 *
 * This routine computes temporaries involving wb's, for use in the
 * SDRHS `dyad' computation.  Each of these is an array of n scalars.
 *
 * These computations depend on wb having already been computed.
 *
 * for b = 0 to n-1
 *     w0w0[b]   = wb[b][0] ** 2
 *     w1w1[b]   = wb[b][1] ** 2
 *     w2w2[b]   = wb[b][2] ** 2
 *     w0w1[b]   = wb[b][0] * wb[b][1]
 *     w0w2[b]   = wb[b][0] * wb[b][2]
 *     w1w2[b]   = wb[b][1] * wb[b][2]
 *     w00w11[b] = - (w0w0[b] + w1w1[b]) 
 *     w00w22[b] = - (w0w0[b] + w2w2[b]) 
 *     w11w22[b] = - (w1w1[b] + w2w2[b]) 
 * next b
 *
 * All the above quantities are zero if the real body is welded to ground.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_wbtemps(FILE *F)
{
    expr w0w0x,w1w1x,w2w2x,w0w1x,w0w2x,w1w2x,w00w11x,w00w22x,w11w22x;
    int  b;

    w0w0x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w1w1x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w2w2x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w0w1x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w0w2x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w1w2x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w00w11x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w00w22x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));
    w11w22x = INUSE(NEW_1dARRAY(cScalarVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        SINDX(w0w0x, b, MUL(INDX(VAL1(wb,b),0),INDX(VAL1(wb,b),0)));
        SINDX(w1w1x, b, MUL(INDX(VAL1(wb,b),1),INDX(VAL1(wb,b),1)));
        SINDX(w2w2x, b, MUL(INDX(VAL1(wb,b),2),INDX(VAL1(wb,b),2)));
        SINDX(w0w1x, b, MUL(INDX(VAL1(wb,b),0),INDX(VAL1(wb,b),1)));
        SINDX(w0w2x, b, MUL(INDX(VAL1(wb,b),0),INDX(VAL1(wb,b),2)));
        SINDX(w1w2x, b, MUL(INDX(VAL1(wb,b),1),INDX(VAL1(wb,b),2)));
    }

    ASSIGN_CLN(F, w0w0, UNUSE(w0w0x));
    ASSIGN_CLN(F, w1w1, UNUSE(w1w1x));
    ASSIGN_CLN(F, w2w2, UNUSE(w2w2x));
    ASSIGN_CLN(F, w0w1, UNUSE(w0w1x));
    ASSIGN_CLN(F, w0w2, UNUSE(w0w2x));
    ASSIGN_CLN(F, w1w2, UNUSE(w1w2x));

    for (b=0; b < SysI.n; b++) {
        SINDX(w00w11x, b, NEG(ADD(VAL1(w0w0,b), VAL1(w1w1,b))));
        SINDX(w00w22x, b, NEG(ADD(VAL1(w0w0,b), VAL1(w2w2,b))));
        SINDX(w11w22x, b, NEG(ADD(VAL1(w1w1,b), VAL1(w2w2,b))));
    }

    ASSIGN_CLN(F, w00w11, UNUSE(w00w11x));
    ASSIGN_CLN(F, w00w22, UNUSE(w00w22x));
    ASSIGN_CLN(F, w11w22, UNUSE(w11w22x));
}

/* COMPUTE_vnb
 *
 * Compute velocity of each realbody's mass center with respect to ground,
 * expressed in the ground frame.
 *
 * Depends on wb and cnb having already been computed.
 *
 * Computation:
 *
 *     vnb[b] = zero,                                   pseudobody(b) == ground
 *            = vnk[k] + (wb[b] X rcom[b])*cnbT(k),   k=pseudobody(b) != ground
 *
 *     cnbT = TRANSPOSE(cnb)
 *
 * Temporary wbrcom[b] is introduced to avoid extra computation:
 *  
 *     wbrcom[b] = wb[b] X rcom[b]
 *
 * The vnb variable (but not wbrcom) must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_vnb(FILE *F)
{
    register int b,k;
    expr vnb_expr, wbrcomx;

    wbrcomx  = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    vnb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1) {
            FLUSH_VEC(F, wbrcom, b, wbrcomx, VECTOR_ZERO());
            FLUSH_VEC_ALL_SAVE(F, vnb, b, vnb_expr, VECTOR_ZERO());
        } else {
            FLUSH_VEC(F, wbrcom, b, wbrcomx, 
                      CROSS(VAL1(wb, b), VAL1(SysI.rcom, b)));
            FLUSH_VEC_ALL_SAVE(F, vnb, b, vnb_expr, 
                               ADD(VAL1(vnk, k),
                                   MATMUL(INDX(wbrcomx, b),
                                          TRANSPOSE(VAL1(cnb, b)))));
        }
    }

    ASSIGN(wbrcom, UNUSE(wbrcomx));
    ASSIGN(vnb,    PERM(vnb_expr));
}

/* COMPUTE_onb
 *
 * Compute angular acceleration of each real body with respect to ground,
 * expressed in the body's frame.
 *
 * Depends on onk having already been computed.
 *
 * Computation:
 *
 *     onb[b] = zero,                         pseudobody(b) == ground
 *            = onk[k],                     k=pseudobody(b) != ground
 *
 * This variable must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_onb(FILE *F)
{
    register int b,k;
    expr onb_expr;

    onb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1)
            FLUSH_VEC_ALL_SAVE(F, onb, b, onb_expr, VECTOR_ZERO());
        else
            FLUSH_VEC_ALL_SAVE(F, onb, b, onb_expr, VAL1(onk, k));
    }

    ASSIGN(onb, PERM(onb_expr));
}

/* COMPUTE_dyad
 *
 * Compute acceleration dyadics, one per body.  These have the nice
 * property that 
 *          dyad[b] * r = onb[b] X r + wb[b] X (wb[b] X r)
 * which is useful for finding the acceleration of an arbitrary point.
 *
 *   dyad[b] =  
 *     -(w1**2 + w2**2)       -o2 + w0w1         o1 + w0w2
 *     o2 + w0w1          -(w0**2 + w2**2)       -o0 + w1w2
 *     -o1 + w0w2            o0 + w1w2        -(w0**2 + w1**2)
 *
 * The dyad for a body which is welded to ground is zero.
 *
 * This routine uses temporaries w0w1, w1w2, etc. ("wbtemps") calculated in 
 * SDLHS to avoid unneeded calculation in SDRHS.  Only the o0, o1, and o2
 * terms need be added in SDRHS (those are onb[b][0], onb[b][1], onb[b][2],
 * which must already have been calculated).
 *
 * Dyad is accessed numerically from the sdacc routine, so it must be
 * completely filled in in the appropriate global or common variable.
 * It is used elsewhere symbolically, so we properly set the `dyad'
 * symbol to VREF's and IS_SIMPLE elements.
 * 
 * Note that the dyad array is indexed by *real* body number.
 * This is an Order(N) computation.
 */
void COMPUTE_dyad(FILE *F)
{
    register Index_t b,i,j;
    expr dyad_expr,temp;

    dyad_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.n));

    for (b = 0; b < SysI.n; b++) {
        temp = INUSE(NEW_MATX(cScalarVal));
        SINDX2(temp,0,0, VAL1(w11w22,b));
        SINDX2(temp,0,1, SUB(VAL1(w0w1,b),INDX(VAL1(onb,b),2)));
        SINDX2(temp,0,2, ADD(VAL1(w0w2,b),INDX(VAL1(onb,b),1)));
        SINDX2(temp,1,0, ADD(VAL1(w0w1,b),INDX(VAL1(onb,b),2)));
        SINDX2(temp,1,1, VAL1(w00w22,b));
        SINDX2(temp,1,2, SUB(VAL1(w1w2,b),INDX(VAL1(onb,b),0)));
        SINDX2(temp,2,0, SUB(VAL1(w0w2,b),INDX(VAL1(onb,b),1)));
        SINDX2(temp,2,1, ADD(VAL1(w1w2,b),INDX(VAL1(onb,b),0)));
        SINDX2(temp,2,2, VAL1(w00w11,b));

        for (i=0; i<3; i++)
            for (j=0; j<3; j++) {
                PRINT_ASSN3(F, "dyad", b,i,j, INDX2(temp,i,j));
                if (!IS_SIMPLE(INDX2(temp,i,j))) 
                    SINDX2(temp,i,j, INDX2(VREF1(dyad,b),i,j));
            }
        SINDX(dyad_expr, b, UNUSE(temp));
    };

    ASSIGN(dyad, PERM(dyad_expr));
}

/* COMPUTE_anb
 *
 * Compute acceleration of each realbody's mass center with respect to ground,
 * expressed in the ground frame.
 *
 * Depends on cnb, ank, and dyad having already been computed.
 *
 * Computation:
 *
 *     anb[b] = zero,                                    pseudobody(b) == ground
 *            = ank[k] + (dyad[b]*rcom[b]) * cnbT(k),  k=pseudobody(b) != ground
 *
 *     cnbT = TRANSPOSE(cnb)
 *
 * Temporary dyrcom[b] is introduced to avoid extra computation:
 *  
 *     dyrcom[b] = dyad[b] * rcom[b]
 *
 * The anb variable (but not dyrcom) must be accessible numerically.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_anb(FILE *F)
{
    register int b,k;
    expr anb_expr, dyrcomx;

    dyrcomx  = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    anb_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b=0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) == -1) {
            FLUSH_VEC(F, dyrcom, b, dyrcomx, VECTOR_ZERO());
            FLUSH_VEC_ALL_SAVE(F, anb, b, anb_expr, VECTOR_ZERO());
        } else {
            FLUSH_VEC(F, dyrcom, b, dyrcomx, 
                      MATMUL(VAL1(dyad, b), VAL1(SysI.rcom, b)));
            FLUSH_VEC_ALL_SAVE(F, anb, b, anb_expr, 
                               ADD(VAL1(ank, k),
                                   MATMUL(INDX(dyrcomx, b),
                                          TRANSPOSE(VAL1(cnb, b)))));
        }
    }

    ASSIGN(dyrcom, UNUSE(dyrcomx));
    ASSIGN(anb,    PERM(anb_expr));
}
