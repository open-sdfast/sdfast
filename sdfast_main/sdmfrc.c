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
#include "../calc/gencode.h"

/*
 * This file contains routines for use in generating multiplier forces
 * as a function of multipliers.  These routines are *symbolic* subroutines;
 * they are used to generate similar symbolic code in multiple places.
 * As input, these take an expression which represents the array of
 * multipliers, and a structure containing a set of symbols for the
 * routines to operate on.  
 *
 * You must be extremely careful if you use these routines twice on the 
 * same set of symbols.  Since earlier values will get wiped out, disaster
 * will ensue if there is some not-yet-printed-out symbol containing a
 * reference to an earlier value.
 */

/* COMPUTE_mfrc
 *
 * Given the multiplier array and some symbols to use, set the 
 * output expressions to the multiplier forces which would be generated
 * by these multipliers.  
 *
 * This routine may generate some assignment statements which aren't
 * counted in CALC's stats.  The number of these is returned in lasg.
 * If any labels are generated here they are numbered starting with
 * nxtlab, and nxtlab is incremented.
 *
 * The calling environment should have variables i and j declared,
 * unless both SysI.n and SysI.s are small enough to unroll.
 */
void COMPUTE_mfrc(FILE *F,
             expr multx,
             mfrcsym_t *mfrcsym,
             expr fk_expr,
             expr tk_expr,
             expr tau_expr,
             int *lasg,
             int  *nxtlab )
{
    expr temp, mltaux;
    int  anyUser,i,j;
    char str_0[10], str_2[10], str_n1[10], str_s1[10], str_sl1[10], str_nc1[10];
    char str_lab[10], str_lab2[10];

    *lasg = 0;
    esprintf(str_0, "%@d", 0);
    esprintf(str_2, "%@d", 2);
    esprintf(str_n1, "%@d", SysI.n-1);
    esprintf(str_s1, "%@d", SysI.s-1);
    esprintf(str_sl1, "%@d", SysI.sl-1);
    esprintf(str_nc1, "%@d", SysI.nc-1);

    esprintf(str_lab, "%d", *nxtlab); /* keep this set to the first
                                         unused label */

    mltaux = INUSE(NEW_1dARRAY(cScalarVal, SysI.sl ? SysI.sl : 1));

    /* If any of the passed-in multipliers are non-zero in a user
     * constraint position, then we will have to call the user
     * constraint force function sduconsfrc().  In that case, we must
     * numerically set all the global mfk's, mtk's, mtau's and mltau's to zero
     * first (so the user doesn't have to set them all in his sduconsfrc() 
     * routine) and then
     * we must set our local fk_expr, etc. to VREFs to the globals,
     * any of which sduconsfrc() may have changed.  Note that mltau's (produced
     * by loop joint sdhinget() calls in sduconsfrc()) act like additional
     * prescribed motion multipliers and are handled with prescribed motion
     * below.
     *
     * In addition, we have to copy the values of the passed-in multipliers 
     * into temporary symbol umult so that we can pass them to sduconsfrc().
     */
    anyUser = 0;
    for (i = SysI.np+SysI.nlc; !anyUser && i < SysI.nc; i++)
        anyUser = !IS_ZERO(INDX(multx,i));

    if (anyUser) {
        efprintf(F, "%{\nInitialize all multiplier forces to zero.\n%}");
        esprintf(str_lab2, "%d", ++(*nxtlab));
        FOR(str_lab2, "i", str_0, str_n1);
            FOR(str_lab, "j", str_0, str_2);
                RSET("mfk%(i%,j%)",0.);
                RSET("mtk%(i%,j%)",0.);
            ENDFOR(str_lab);
        ENDFOR(str_lab2);
        esprintf(str_lab, "%d", ++(*nxtlab));
    
        FOR(str_lab, "i", str_0, str_s1);
            RSET("mtau%(i%)",0.);
        ENDFOR(str_lab);
        esprintf(str_lab, "%d", ++(*nxtlab));

        if (SysI.sl) {
            FOR(str_lab, "i", str_0, str_sl1);
                RSET("mltau%(i%)",0.);
            ENDFOR(str_lab);
            esprintf(str_lab, "%d", ++(*nxtlab));
        }

        *lasg = 6*SysI.n + SysI.s + SysI.sl;

        efprintf(F, "%{\nCompute user-generated multiplier forces\n%}");
        for (i = 0; i < SysI.nu; i++)
            PRINT_ASSN1(F, PRINTNAME(mfrcsym->umult_), i, 
                        INDX(multx, i+SysI.np+SysI.nlc));
        ISET("mfrcflg", 1);
        efprintf(F, "%s%Auconsfrc(curtim,q,u,%s)%;\n", 
                 Lang->proc_call,PRINTNAME(mfrcsym->umult_));
        ISET("mfrcflg", 0);

        for (i=0; i < SysI.n; i++) {
            temp = NEW_VECX(cScalarVal);
            for (j=0; j<3; j++)
                SINDX(temp, j, INDX(VREF1(mfk, i), j));
            SINDX(fk_expr, i, temp);
            temp = NEW_VECX(cScalarVal);
            for (j=0; j<3; j++)
                SINDX(temp, j, INDX(VREF1(mtk, i), j));
            SINDX(tk_expr, i, temp);
        }
        for (i=0; i < SysI.s; i++) 
            SINDX(tau_expr, i, VREF1(mtau, i));
        for (i=0; i < SysI.sl; i++) 
            SINDX(mltaux, i, VREF1(mltau, i));
    } else {
        /* no active user constraints */
        for (i=0; i < SysI.n; i++) {
            SINDX(fk_expr, i, VECTOR_ZERO());
            SINDX(tk_expr, i, VECTOR_ZERO());
        }
        for (i=0; i < SysI.s; i++) 
            SINDX(tau_expr, i, SCALAR_ZERO());
        for (i=0; i < SysI.sl; i++) 
            SINDX(mltaux, i, SCALAR_ZERO());
    }

    /* Call this even if there's no prescribed motion, to make sure the
       mltauti, mltauto, mltaufi, and mltaufo variables are zeroed. */
    COMPUTE_pmfrc(F, multx, mltaux, mfrcsym, fk_expr, tk_expr, tau_expr);

    if (SysI.nl)
        COMPUTE_lmfrc(F, multx, mfrcsym, fk_expr, tk_expr);

    DISPOSE_EXPR(UNUSE(mltaux));
}

/* PRINT_SDMFRC
 *
 * Generate the sdmfrc() subroutine.
 * This routine is for internal use only.  It takes an array of
 * multipliers as inputs and sets globals mfk, mtk, mtau to the
 * forces that result from these multipliers.  Both SD- and 
 * user-generated constraints are included.  All non-constant elements
 * of mfk,mtk,mtau are written here; the constant one's aren't so these
 * symbols should always be accessed using VAL, not directly by name.
 *
 * As a side effect, we generate loop joint bearing forces and torques
 * (lfc and ltc) here, leaving the values in globals for access during
 * generation of sdreac().  If multiple calls to sdmfrc() are made,
 * make sure the LAST call is the one which uses real multipliers!
 *
 * This routine must be generated AFTER sdstate() because it makes
 * use of quantities computed there, including some of the `vt' temporaries
 * use in constraint error calculations.
 *
 * Forces are accumulated as we go, since a single body may be involved
 * in more than one loop.
 *
 * If there are no constraints, we just set opcounts to 0 and generate a stub.
 *
 */
void PRINT_SDMFRC(FILE *F,
             opstats_t *opcnt)
{
    Index_t i;
    long lasg;
    expr mfk_expr,mtk_expr,mtau_expr,multx;
    sym imult,umult;
    char str_0[10], str_2[10], str_n1[10], str_s1[10], str_nc1[10];
    char str_lab[10];
    mfrcsym_t syms;
    int nasg,nxtlab=100;

    START_COMPSUB(opcnt);

    /* Declare the SDMFRC routine heading. */
    declare_proc(F, 0, "mfrc",
      VT_USER|VT_DSYM, &SysI.type_Arr_nc, "imult", &imult,
      0);

    if (!SysI.nc) {
        PRINT_STUB(F);
        return;
    }

    lasg = 0;

    esprintf(str_0, "%@d", 0);
    esprintf(str_2, "%@d", 2);
    esprintf(str_n1, "%@d", SysI.n-1);
    esprintf(str_s1, "%@d", SysI.s-1);
    esprintf(str_nc1, "%@d", SysI.nc-1);

    esprintf(str_lab, "%d", nxtlab); /* keep this set to the first
                                         unused label */

    efprintf(F, "%{\n\
Calculate forces due to constraint multipliers.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    /* COMPUTE_mfrc will need these if there are user constraints */
    if (SysI.nu)
        declare_vars(F, 0,
          VT_INTEGER, "i",
          VT_DUP,     "j",
          VT_ARRAY|VT_DSYM, "umult", SysI.nu, 0, &umult,
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs do\n");
        efprintf(F,"%>");
    }

    fflush(F);

    mfk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    mtk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    mtau_expr = INUSE(NEW_1dARRAY(cScalarVal, SysI.s?SysI.s:1));
    multx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nc));

    /* In the sdmfrc routine, we are using local symbols to compute values
     * for global variables.  These globals must be purged of any references
     * to locals before we're done or we risk "undefined variable" compilation
     * errors.  Watch out for imult, umult, tvec1, tvec2, and tvec3.
     * FLUSHNONCONST is our salvation.
     */

    for (i=0; i < SysI.nc; i++)
        SINDX(multx, i, VREF1(imult, i));

    syms.umult_ = umult;        /* local symbol */
    syms.lfci_  = lfci;
    syms.lfc_   = lfc;
    syms.ltc_   = ltc;
    syms.ltci_  = ltci;
    syms.Tinb_  = Tinb;
    syms.Toutb_ = Toutb;
    syms.ltaufi_ = mltaufi;
    syms.ltaufo_ = mltaufo;
    syms.ltauti_ = mltauti;
    syms.ltauto_ = mltauto;
    syms.tmpv1_ = tvec1;
    syms.tmpv2_ = tvec2;
    syms.tmpv3_ = tvec3;
    syms.fk_    = mfk;
    syms.tk_    = mtk;
    syms.tau_   = mtau;
    
    /* Compute all the forces. */
    COMPUTE_mfrc(F, multx, &syms, mfk_expr, mtk_expr, mtau_expr, 
                 &nasg, &nxtlab);
    esprintf(str_lab, "%d", nxtlab);
    lasg += nasg;

    /* print out all the non-constant elements of the forces so that
       the symbols mfk, mtk and mtau contain no references to any
       variables which may become out-of-date. */
    ASSIGN(mfk, UNUSE(mfk_expr));
    ASSIGN(mtk, UNUSE(mtk_expr)); 
    if (SysI.s) 
        ASSIGN(mtau, UNUSE(mtau_expr));
    else DISPOSE_EXPR(UNUSE(mtau_expr));
    CLEANVAR(F, mfk, CL_FLUSHNONCONST, CL_FORGET);
    CLEANVAR(F, mtk, CL_FLUSHNONCONST, CL_FORGET);
    if (SysI.s)
        CLEANVAR(F, mtau, CL_FLUSHNONCONST, CL_FORGET);

    /* flush out the other global symbols which may have been defined
       in terms of local ones (see syms array above) */
    if (SysI.nl > 0) {
        CLEANVAR(F, lfci, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, lfc, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, ltc, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, ltci, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, Tinb, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, Toutb, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, mltaufi, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, mltaufo, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, mltauti, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, mltauto, CL_FLUSHNONCONST, CL_FORGET);
    }

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,lasg);
    efprintf(F, Lang->proc_end);

    if (sdfast_opt.verbose)
        printf("Multiplier-generated forces computed.  (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());
}

/*
 * Compute multiplier-generated forces for prescribed motion.
 * For tree joints, the multiplier IS the force, applied as a
 * hinge torque at the prescribed hinge.  For loop joints, the
 * prescribed motion is implemented by forces and torques applied
 * to the two bodies connected by the loop joint.  For loop joints,
 * a "prescribed motion-like" force may be generated by a user constraint
 * which calls sdhinget() on a loop hinge.  These values, if any, are
 * in mltaux[sl].
 *
 * The expressions passed to this routine already have values to
 * which the prescribed motion forces should be added.
 *
 * We don't normally print the force assignments out here.  Instead, 
 * we just return the updated fk_, tk_, and tau_expr's.  This may
 * produce some temporaries that are printed, however, and in the
 * case of runtime prescribed motion the assignments to the variables
 * themselves must be made.
 */
void COMPUTE_pmfrc(FILE *F,
              expr multx, 
              expr mltaux, 
              mfrcsym_t *syms,
              expr fk_expr, 
              expr tk_expr, 
              expr tau_expr)
{
    int i,m;
    expr temp, ltaux;
    char   str_flt0[20];

    esprintf(str_flt0, "%r", 0.0); /* for use in `if' statement */

    m = 0; /* next prescribed motion multiplier */

    /* Prescribed motion in tree joints.  Forces are just hinge
       torques equal to the multiplier. */
    for (i=0; i<SysI.s; i++) 
        if (!IS_ZERO(VAL1(SysI.pres,i))) {
            /* may be prescribed */
            if (!IS_ZERO(INDX(multx,m))) {
                if (IS_ONE(VAL1(SysI.pres,i))) 
                    SINDX(tau_expr, i, ADD(INDX(tau_expr,i),INDX(multx,m)));
                else {
                    /* runtime prescribed */
                    efprintf(F, Lang->stmt_if2_b);
                    PRINT_E(F, VAL1(SysI.pres,i));
                    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                        temp = ADD(INDX(tau_expr,i),INDX(multx,m));
                        PRINT_ASSN1(F, PRINTNAME(syms->tau_), i, temp);
                        DISPOSE_EXPR(temp);
                    temp = VREF1(syms->tau_, i); 
                    if (!SAME_EXPR(INDX(tau_expr,i), temp)) {
                        efprintf(F, Lang->stmt_if2_else); 
                        PRINT_ASSN1(F, PRINTNAME(syms->tau_), i,
                            INDX(tau_expr,i));
                    }
                    efprintf(F, Lang->stmt_if2_e);
                    SINDX(tau_expr, i, temp);
                }
            }
            m++;
        }

    if (SysI.nl == 0)
        return;

    /* Prescribed motion in loop joints.  Forces are fk's and tk's
       applied to the bodies connected by the loop joint.  We have to
       take the multipliers and spread them out into an array indexed
       by loop hinge, with zeroes placed on non-prescribed hinges. */

    ltaux = INUSE(NEW_1dARRAY(cScalarVal, SysI.sl ? SysI.sl : 1));
    for (i=0; i < SysI.sl; i++) {
        if (!IS_ZERO(VAL1(SysI.lpres,i))) {
            /* may be prescribed so it gets a multiplier */
            SINDX(ltaux, i, 
                QUES(EQUAL(VAL1(SysI.lpres, i), SCALAR_ZERO()),
                     INDX(mltaux,i),
                     ADD(INDX(mltaux,i), INDX(multx,m))));
            m++;
        } else 
            SINDX(ltaux, i, INDX(mltaux,i));
    }

    COMPUTE_ltauforces(F, ltaux, 
                       syms->tmpv1_, syms->tmpv2_, syms->tmpv3_,
                       syms->ltaufi_, syms->ltaufo_,
                       syms->ltauti_, syms->ltauto_, 
                       fk_expr, tk_expr);

    DISPOSE_EXPR(UNUSE(ltaux));
}

/*
 * Compute the multiplier-generated forces due to loop joints.
 *
 * Each loop joint applies an fk and tk to its inboard and outboard
 * bodies.  These forces are a consequence of the multipliers for
 * the loop joint.  There are `c' multipliers for each joint, where
 * c = 6 - #dof.  Below, we refer to these as m1,m2,...mc.
 *
 * Fk's and tk's are computed as though applied at the body center
 * of mass, and in the body frame.  The forces and torque on the 
 * inboard body are called Fi and Ti below, with Fo and To representing
 * the force and torque on the outboard body.  In the case that body i
 * or o is ground, the corresponding force and torque are computed
 * but not applied.
 *
 * When we return fk_expr and tk_expr will have been modified to
 * reflect the additional forces and torques.  We do not output 
 * assignments to fk's and tk's here, although we may output assignments
 * to temporaries.
 *
 * (Below, vt10c is the vector from the inboard body's center of mass to the
 * outboard hinge point.)
 *
 *
 * Joint Type   Fi          Fo         Ti                  To
 *
 * pin      ltci= m0*(iref X vt1)+m1*(iperp X vt1)
 *          ltc = -ltci*Cio
 *
 *              m2,m3,m4   -Fi*Cio     ltci                ltc
 *                                   + vt10c X Fi        + btj X Fo
 *
 * ujoint   ltci=m0*(ipin X vt1)
 *          ltc = -ltci*Cio
 *
 *              m1,m2,m3   -Fi*Cio     ltci                ltc
 *                                   + vt10c X Fi        + btj X Fo
 *
 * slider   ltci=m0*(iref X vt1)+m1*(iperp X vt1)+m2*(iperp X vt10)
 *          ltc = -ltci*Cio
 *
 *              m3*iref    -Fi*Cio     ltci                ltc
 *            + m4*iperp             + vt10c X Fi        + btj X Fo  
 *
 * ball     ltci=0  ltc=0
 * gimbal
 *              m0,m1,m2   -Fi*Cio     vt10c X Fi          btj X Fo
 *
 * bearing  ltci=0  ltc=0
 *
 *              m0*iref    -Fi*Cio     vt10c X Fi          btj X Fo
 *            + m1*iperp           
 *       
 * cylinder ltci=<same as pin>
 *
 *              m2*iref    -Fi*Cio     ltci                ltc
 *            + m3*iperp             + vt10c X Fi        + btj X Fo
 *
 * planar   ltci=<same as pin>
 * 
 *              m2*ipin    -Fi*Cio     ltci                ltc
 *                                   + vt10c X Fi        + btj X Fo
 *
 * weld     ltci=<same as slider>
 *         
 *              m3,m4,m5   -Fi*Cio     ltci                ltc
 *                                   + vt10c X Fi        + btj X Fo
 *
 *
 * As a side effect, we get all of the computations necessary for loop
 * joint reaction forces and torques.  The correspondence is as follows:
 *
 *      lfci = Fi                reaction force on inb hinge pt, in inb frame
 *      lfc  = Fo                reaction force on outb hinge pt, in outb frame
 *      ltci = shown above      reaction torque on inb body, in inb frame
 *      ltc  = shown above      reaction torque on outb body, in outb frame
 *
 * Any element of these variables which is non-constant (and thus may
 * reference a non-global symbol) is assigned here so that the global
 * runtime variables contain all the non-constant values, while the
 * sdfast-time variables contain only constants and references to the
 * runtime variables.
 *    
 */
void COMPUTE_lmfrc(FILE *F,
              expr multx, 
              mfrcsym_t *syms,
              expr fk_expr, 
              expr tk_expr)
{
    int i,j,inb,outb,m;
    JointKind_t jk;
    expr ltcix, ltcx, temp;
    expr lfcix, lfcx, Tinbx, Toutbx;

    lfcix  = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    lfcx   = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    ltcx   = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    ltcix  = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    Tinbx  = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    Toutbx = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));

    for (j = 0; j < SysI.nl; j++) {
        inb = SysI.LoopConst[j].jnt.InbBody;
        outb = SysI.LoopConst[j].jnt.OutbBody;
        jk = SysI.LoopConst[j].jnt.JointKind;
        m = SysI.FirstM[j];

        /* First compute Fi (lfci) into temp */
        switch(jk) {
            case cWeldJoint:
                temp = NEW_VECX(cScalarVal);
                for (i=0; i<3; i++)
                    SINDX(temp, i, INDX(multx, m+3+i));
                break;
            case cPinJoint:
                temp = NEW_VECX(cScalarVal);
                for (i=0; i<3; i++)
                    SINDX(temp, i, INDX(multx, m+2+i));
                break;
            case cUjoint:
                temp = NEW_VECX(cScalarVal);
                for (i=0; i<3; i++)
                    SINDX(temp, i, INDX(multx, m+1+i));
                break;
            case cBallJoint:
            case c3dJoint:
                temp = NEW_VECX(cScalarVal);
                for (i=0; i<3; i++)
                    SINDX(temp, i, INDX(multx, m+i));
                break;
            case cSlidingJoint:
                temp = ADD(MUL(INDX(multx,m+3),VAL1(iref,j)),
                           MUL(INDX(multx,m+4),VAL1(iperp,j)));
                break;
            case cCylJoint:
                temp = ADD(MUL(INDX(multx,m+2),VAL1(iref,j)),
                           MUL(INDX(multx,m+3),VAL1(iperp,j)));
                break;
            case cBearingJoint:
                temp = ADD(MUL(INDX(multx,m+0),VAL1(ipin2,j)),
                           MUL(INDX(multx,m+1),VAL1(iperp,j)));
                break;
            case cPlanarJoint:
                temp = MUL(INDX(multx,m+2),VAL1(ipin,j));
                break;
            case c6dJoint:
            case cBushingJoint:
                temp = VECTOR_ZERO();
                break;
        }

        /* Put Fi into lfcix and write out a temp if necessary. */
        FLUSH_VEC_NONCONST(F, syms->lfci_, j, lfcix, temp);

        /* Add the Fi into the fk */
        if ( inb != cGroundBody ) 
            SINDX(fk_expr, inb, ADD(INDX(fk_expr,inb),INDX(lfcix,j)));

        /* Now calculate Fo (lfc) - same for all joint kinds. */
        temp = NEG(MATMUL(INDX(lfcix,j),VAL1(Cio,j)));

        FLUSH_VEC_NONCONST(F, syms->lfc_, j, lfcx, temp);
        if ( outb != cGroundBody ) 
            SINDX(fk_expr, outb, ADD(INDX(fk_expr,outb),INDX(lfcx,j)));

        /* Compute temporary ltci. */
        switch(jk) {
            case cSlidingJoint:
            case cWeldJoint:
                temp = ADD(MUL(INDX(multx,m+0),
                               CROSS(VAL1(iref,j),VAL1(vt1,j))),
                       ADD(MUL(INDX(multx,m+1),
                               CROSS(VAL1(iperp,j),VAL1(vt1,j))),
                           MUL(INDX(multx,m+2),
                               CROSS(VAL1(iperp,j),VAL1(vt10,j)))));
                break;
            case cPinJoint:
            case cCylJoint:
            case cPlanarJoint:
                temp = ADD(MUL(INDX(multx,m+0),
                               CROSS(VAL1(iref,j),VAL1(vt1,j))),
                           MUL(INDX(multx,m+1),
                               CROSS(VAL1(iperp,j),VAL1(vt1,j))));
                break;
            case cUjoint:
                temp = MUL(INDX(multx,m+0), 
                           CROSS(VAL1(ipin,j),VAL1(vt1,j)));
                break;
            default:
                temp = VECTOR_ZERO();
                break;
        }

        FLUSH_VEC_NONCONST(F, syms->ltci_, j, ltcix, temp);

        /* Compute ltc - same for all joints. */
        temp = NEG(MATMUL(INDX(ltcix,j),VAL1(Cio,j)));

        FLUSH_VEC_NONCONST(F, syms->ltc_, j, ltcx, temp);

        /* Now on to Ti - same for all joints since ltci is 0 for 
           ball and gimbal and lfci is zero for 6d joint */
        temp = ADD(INDX(ltcix,j), 
                   CROSS(VAL1(vt10c,j), INDX(lfcix,j)));

        FLUSH_VEC(F, syms->Tinb_, j, Tinbx, temp);
        if ( inb != cGroundBody ) 
            SINDX(tk_expr, inb, ADD(INDX(tk_expr,inb),INDX(Tinbx,j)));

        /* Finally To - same for all joint types since ltci is 0 for 
           ball and gimbal. */

        temp = ADD(INDX(ltcx,j),
                   CROSS(VAL1(SysI.lbtj,j),INDX(lfcx,j)));

        FLUSH_VEC(F, syms->Toutb_, j, Toutbx, temp);
        if ( outb != cGroundBody ) 
            SINDX(tk_expr, outb, ADD(INDX(tk_expr,outb),INDX(Toutbx,j)));
    }

    ASSIGN(syms->lfci_, UNUSE(lfcix));
    ASSIGN(syms->lfc_,  UNUSE(lfcx));
    ASSIGN(syms->ltci_, UNUSE(ltcix));
    ASSIGN(syms->ltc_,  UNUSE(ltcx));
    ASSIGN(syms->Tinb_, UNUSE(Tinbx));
    ASSIGN(syms->Toutb_,UNUSE(Toutbx));
}
