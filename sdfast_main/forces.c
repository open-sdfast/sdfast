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
#include "../calc/gencode.h"

/*
 * COMPUTE_gk
 *
 * Compute gk, the gravity acceleration vector transformed into each body's 
 * local frame.
 * If the user didn't provide a gravity vector, we'll set all the gk's to 0.
 * Gravity acts along the direction of the global axes, that is, you
 * have to give a negative value for gravity to make it go `down'.
 * (This is the opposite of the `A.x.y' versions of sdexact.)
 *
 * We'll output an appropriate comment if the user asked for gravity. 
 *
 * This is an Order(N) calculation.
 */
void COMPUTE_gk(FILE *F)
{
    register Index_t k,inb;
    expr temp,gk_expr;
    int  first = 1;

    if (SysI.s == 0)
        return;

    gk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go since gk(k) is defined in terms of gk(inb(k)) */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;
        if (inb == cGroundBody) {
            if (SysI.GravExpr) {
                if (first) {
                    efprintf(F, "%{\nCompute gravity\n%}");
                    first = 0;
                }
                temp = MATMUL(VAL(SysI.grav),VAL1(Cik,k));
            } else 
                temp = VECTOR_ZERO();
        } else
            temp = MATMUL(INDX(gk_expr,inb),VAL1(Cik,k));
        FLUSH_VEC(F, gk, k, gk_expr, temp);
    }

    ASSIGN(gk, UNUSE(gk_expr));
}

/* COMPUTE_Fstar
 *
 * Compute Fstar, the inertia force remainder term plus active forces.
 * Active forces come both from sdpointf() calls (ufk) and from loop taus
 * (ltaufk).
 *
 *    Fstar[k] = mk[k]*(Atk[k]-gk[k]) - sum(ufk[b]+ltaufk[b]),  
 *        summation is over all b for which k=headpseudo(b)
 *
 * This is an Order(n) calculation.
 */
void COMPUTE_Fstar(FILE *F)
{
    register Index_t b,k;
    expr Fstar_expr;

    if (SysI.s == 0)
        return;

    Fstar_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* This is zero for non-realbody pseudobodies k. */
    for (k = 0; k < SysI.s; k++)
        SINDX(Fstar_expr, k, 
                MUL(VAL1(SysI.psmk,k), SUB(VAL1(Atk,k),VAL1(gk,k))));

    /* Now update Fstar for each body having k as its "head" pseudobody. */
    for (b = 0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        if (SysI.sl)
            SINDX(Fstar_expr, k, 
               SUB(INDX(Fstar_expr,k), ADD(VAL1(ufk, b),VAL1(ltaufk,b))));
        else 
            SINDX(Fstar_expr, k, 
               SUB(INDX(Fstar_expr,k), VAL1(ufk, b)));
    }

    ASSIGN_CLN(F, Fstar, UNUSE(Fstar_expr));
}

/* COMPUTE_Tstar
 *
 * Compute Tstar, the inertia torque remainder term plus active torques.
 * Uses temporary variables 
 *            IkWk[k] = ik[k]*wk[k] 
 *          WkIkWk[k] = wk[k] X IkWk[k]
 * Active torques come from sdpointf() and sdbodyt() calls (utk), as well as
 * from loop taus (ltautk).
 *
 *    Tstar[k] = WkIkWk[k] + ik[k]*Otk[k] - sum(utk[b]+ltautk[b]+shft[b])
 *        summation is over all b for which k=headpseudo(b)
 *        shft[b] = rcom[b] X (ufk[b]+ltaufk[b]), that is, the torque
 *          produced by shifting applied forces to composite pseudobody COM
 *
 * This is an Order(n) calculation.
 */
void COMPUTE_Tstar(FILE *F)
{
    register Index_t b,k;
    expr Tstar_expr;

    if (SysI.s == 0)
        return;

    Tstar_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* This is zero for non-realbody pseudobodies k. */
    for (k = 0; k < SysI.s; k++)
        SINDX(Tstar_expr, k, 
                ADD(VAL1(WkIkWk,k), MATMUL(VAL1(SysI.psik,k),VAL1(Otk,k))));

    /* Now update Tstar for each body having k as its "head" pseudobody. */
    for (b = 0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        if (SysI.sl)
            SINDX(Tstar_expr, k, 
               SUB(INDX(Tstar_expr, k), 
                   ADD(ADD(VAL1(utk, b),VAL1(ltautk, b)),
                       CROSS(VAL1(SysI.rcom, b), 
                             ADD(VAL1(ufk, b), VAL1(ltaufk, b))))));
        else
            SINDX(Tstar_expr, k, 
               SUB(INDX(Tstar_expr,k), 
                   ADD(VAL1(utk, b),
                       CROSS(VAL1(SysI.rcom, b), VAL1(ufk, b)))));;
    }

    ASSIGN_CLN(F, Tstar, UNUSE(Tstar_expr));
}

/* COMPUTE_tau
 *
 * Compute all hinge torques for tree hinges.  These are normally
 * just the user's input torques unless they are prescribed, in which
 * case there is a multiplier torque as well.  The hinge torques
 * are generated into variable tauc, and we output an assignment
 * to every element of tauc even if IS_SIMPLE.  However, we'll leave
 * the IS_SIMPLE values in tauc for use by COMPUTE_fs and COMPUTE_reaction 
 * below.
 *
 *    tauc[k] = utau[k] + mtau[k]
 *
 * This is an Order(N) calculation, and is cheap enough that we
 * do it every time SDRHS is called so that it can be accessed
 * numerically from SDGETHT.
 */
void COMPUTE_tau(FILE *F)
{
    int  k;
    expr tauc_expr;

    if (SysI.s == 0)
        return;

    tauc_expr  = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));

    for (k=0; k < SysI.s; k++) {
        if (SysI.nc)
            SINDX(tauc_expr, k, ADD(VAL1(utau,k),VAL1(mtau,k)));
        else
            SINDX(tauc_expr, k, VAL1(utau,k));

        PRINT_ASSN1(F, PRINTNAME(tauc), k, INDX(tauc_expr,k));
        if (!IS_SIMPLE(INDX(tauc_expr, k)))
            SINDX(tauc_expr, k, VREF1(tauc, k));
    }

    ASSIGN(tauc, UNUSE(tauc_expr));
}

/* COMPUTE_ltau
 *
 * Compute all hinge torques for loop hinges.  These are normally
 * just the user's input torques unless they are prescribed, in which
 * case there is a multiplier torque as well.  The hinge torques
 * are generated into variable ltauc, and we output an assignment
 * to every element of ltauc even if IS_SIMPLE.  However, we'll leave
 * the IS_SIMPLE values in ltauc (not currently used).
 *
 *    ltauc[k] = ltau[k] + mult[p],  where p is the multiplier
 *                                   assoc. with this hinge if
 *                                   prescribed
 *
 * This is an Order(N) calculation, and is cheap enough that we
 * do it every time SDRHS is called so that it can be accessed
 * numerically from SDGETHT.  This routine depends on the global `mult'
 * having been set (numerically) to the current value of the multipliers.
 */
void COMPUTE_ltau(FILE *F)
{
    int  i,m;
    expr ltauc_expr;

    ltauc_expr  = INUSE(NEW_1dARRAY(cScalarVal, SysI.sl));

    /* First find multiplier index for first prescribed loop joint. */
    for (i=SysI.n; i<SysI.nj; i++)
        if ((m=SysI.PresM[i]) >= 0)
            break;
    
    /* Then set all the ltauc's. */
    for (i=0; i<SysI.sl; i++) {
        SINDX(ltauc_expr, i, VAL1(ltau, i));
        if (!IS_ZERO(VAL1(SysI.lpres,i))) {
            /* may be prescribed */
            SINDX(ltauc_expr, i, ADD(INDX(ltauc_expr,i),VAL1(mult,m)));
            m++;
        }
        PRINT_ASSN1(F, PRINTNAME(ltauc), i, INDX(ltauc_expr, i));
        if (!IS_SIMPLE(INDX(ltauc_expr,i)))
            SINDX(ltauc_expr, i, VREF1(ltauc, i));
    }

    ASSIGN(ltauc, UNUSE(ltauc_expr));
}

/* The next two routines compute a generic "fs" array using either
 * Kane's method or an Order(N) method.  These are symbolic subroutines
 * which are used to generate similar but different code in several
 * different places.
 */

/* generic_kane_fs
 *
 * Computes an fs array using Kane's method, i.e. Vpk's and Wpk's:
 *
 * fs(p) = tau(p) - SUM(Vpk[p,k]*fstar[k] + Wpk[p,k]*tstar[k]),    k=0..s-1.
 *
 * Tau, fstar and tstar are passed in as expressions and are not necessarily 
 * the same as similarly-named global symbols.
 *
 * This is an Order(n**2) calculation.
 */
void generic_kane_fs(FILE *F,
                int printall,
                expr taux, 
                expr fstx,
                expr  tstx,
                sym fs,
                expr  fsx)
{
    int k,p;
    expr temp;

    taux = INUSE(taux);
    fstx = INUSE(fstx);
    tstx = INUSE(tstx);

    for (p = 0; p < SysI.s; p++) {
        /* compute the summation shown above */
        temp = SCALAR_ZERO();
        for (k = p; k < SysI.s; k++)
            temp = ADD(temp, ADD(DOT(VAL2(Vpk,p,k),INDX(fstx,k)),
                                 DOT(VAL2(Wpk,p,k),INDX(tstx,k))));
        temp = SUB(INDX(taux,p), temp);
        if (printall || !IS_CONST(temp)) {
            PRINT_ASSN1(F, PRINTNAME(fs), p, temp);
            DISPOSE_EXPR(temp);
            SINDX(fsx, p, VREF1(fs,p));
        } else 
            SINDX(fsx, p, temp);

        if (sdfast_opt.magic_no && p == (SysI.s/2)) {
          IFCOND efprintf(F, "ii%s%d", NE, sdfast_opt.magic_no);
            THEN RETURN;
          ENDIF;
        }
    }

    taux = UNUSE(taux);
    fstx = UNUSE(fstx);
    tstx = UNUSE(tstx);
}

/* generic_ordern_fs
 *
 * Computes an fs array using an Order(N) method:
 *
 * Fstark = fstar
 * Tstark = tstar
 * for (k=s-1; k>=0; k--)
 *     fs[k] = tau - ((Vkk[k]+rkWkk[k])*Fstark[k] + Wkk[k]*Tstark[k])
 *     i = inb(k)                
 *     if (i>=0) {
 *         Fstark[i] += Fstark[k]*Cikt      
 *         tvec1 = Tstark[k] + rik[k] X Fstark[k]
 *         Tstark[i] += tvec1*Cikt
 *     }
 *
 * Tau, fstar and tstar are passed in as expressions and are not necessarily 
 * the same as similarly-named global symbols.  The symbols Fstark, Tstark,
 * and tvec1 are global temporaries.  They are set and used here, but no 
 * reference remains in the returned fs.
 *
 * This is an Order(n) calculation.
 */
void generic_ordern_fs(FILE *F,
                  int printall,
                  expr taux, 
                  expr fstx, 
                  expr tstx, 
                  sym fs,
                  expr fsx)
{
    register Index_t i,k;
    expr temp, Fstarkx, Tstarkx;

    taux = INUSE(taux);
    fstx = INUSE(fstx);
    tstx = INUSE(tstx);

    Fstarkx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    Tstarkx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k=0; k<SysI.s; k++) {
        SINDX(Fstarkx, k, INDX(fstx, k));
        SINDX(Tstarkx, k, INDX(tstx, k));
    }

    for (k=SysI.s-1; k>=0; k--) {
        /* Compute and output an element of fs, working down from the
           outermost body. */

        temp = SUB(INDX(taux,k),
                   ADD(DOT(ADD(VAL1(Vkk,k),VAL1(rkWkk,k)),
                           INDX(Fstarkx,k)),
                       DOT(VAL1(Wkk,k), INDX(Tstarkx,k))));
        if (printall || !IS_CONST(temp)) {
            PRINT_ASSN1(F, PRINTNAME(fs), k, temp);
            SINDX(fsx, k, VREF1(fs,k));
            DISPOSE_EXPR(temp);
        } else
            SINDX(fsx, k, temp);

        /* Now shift the forces and torques to the inboard body. */

        i = SysI.PseudoBodies[k].jnt.InbBody;
        if (i >= 0) {
            temp = ADD(INDX(Fstarkx,i), 
                       MATMUL(INDX(Fstarkx,k), TRANSPOSE(VAL1(Cik,k))));
            FLUSH_VEC_NONCONST(F,Fstark,i,Fstarkx,temp);
            ASSIGN_CLN(F, tvec1, ADD(INDX(Tstarkx,k),
                                     CROSS(VAL1(rik,k),INDX(Fstarkx,k))));
            temp = ADD(INDX(Tstarkx,i), MATMUL(VAL(tvec1),
                                               TRANSPOSE(VAL1(Cik,k))));
            FLUSH_VEC_NONCONST(F,Tstark,i,Tstarkx,temp);
        }

        if (sdfast_opt.magic_no && k == (SysI.s/2)) {
          IFCOND efprintf(F, "ii%s%d", NE, sdfast_opt.magic_no);
            THEN RETURN;
          ENDIF;
        }
    }

    DISPOSE_EXPR(UNUSE(Fstarkx));
    DISPOSE_EXPR(UNUSE(Tstarkx));

    taux = UNUSE(taux);
    fstx = UNUSE(fstx);
    tstx = UNUSE(tstx);
}

/* COMPUTE_fs0
 *
 * This routine computes f (the right hand side of M*udot=f) into 
 * global symbol fs0.  We use either Kane's method (Order(n**2)) or an 
 * Order(n) method, depending on the user-selected formulation.
 *
 * This version of the right hand side assumes that there are no multiplier-
 * generated forces.  These must be added in later before solving for
 * the udots (see COMPUTE_fs).
 *
 * All elements of fs0 are assigned in case someone wants to retrieve
 * this value numerically (see sdfrcmat()).
 *
 * Computes fs0 using the appropriate generic routine:
 * Here
 *    tau = utau (user supplied tree-joint hinges)
 *    fstar = Fstar (global Fstar symbol)
 *    tstar = Tstar (global Tstar symbol)
 * Output goes to global symbol fs0.  Every element is written out but
 * constant terms are retained in the symbol.
 */
void COMPUTE_fs0(FILE *F)
{
    expr fs0_expr;

    if (SysI.s == 0)
        return;

    fs0_expr = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));

    if (sdfast_opt.formulation == OPT_KANE)
        generic_kane_fs(F,1,VAL(utau),VAL(Fstar),VAL(Tstar),fs0,fs0_expr);
    else
        generic_ordern_fs(F,1,VAL(utau),VAL(Fstar),VAL(Tstar),fs0,fs0_expr);

    ASSIGN(fs0, UNUSE(fs0_expr)); /* already clean */
}

/* COMPUTE_fsmult
 * 
 * This routine computes an fs array representing
 * the force contribution from all the multiplier-generated forces.
 * The fk, tk, and tau are passed-in symbols, not necessarily representing
 * global ones.  Therefore, this routine can be used to generate similar
 * code in different places.
 *
 * Actually, fk and tk are indexed by real body number, so we have to
 * pull some shenanigans to get the indexing right.  The non-realbody
 * pseudobodies effectively have zero fk's and fk's.  Also, fk is really
 * applied at the real body COM, not the composite pseudobody COM.  Therefore,
 * it produces a torque about the composite COM which must be accounted for 
 * here.
 *
 * We use either Kane's method (Order(n**2)) or an Order(n) method
 * depending on the user-selected formulation.
 * We perform this computation using the appropriate generic routine
 * for Kane's or Order(N) method.
 *
 * Here
 *    tau = passed-in
 *    fstar = -SUM(passed-in fk)
 *    tstar = -SUM(passed-in tk + (rcom X fk))
 * where SUM is taken over all realbodies on the same composite pseudobody.
 *
 * Output goes to passed-in fsmult symbol and expression.  All non-constant
 * elements of fsmult are flushed out; constant elements are not.  The
 * returned expression thus contains either constants or vrefs.
 */
void COMPUTE_fsmult(FILE *F,
               sym fk,
               sym tk,
               sym tau,
               sym fsmult,
               expr fs_expr)
{
    register Index_t p,b;
    expr psfkx, pstkx;

    if (SysI.s == 0)
        return;

    /* These are just renumbered versions of -fk and -tk */
    psfkx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    pstkx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (p = 0; p < SysI.s; p++) {
        SINDX(psfkx, p, VECTOR_ZERO());
        SINDX(pstkx, p, VECTOR_ZERO());
    }

    for (b = 0; b < SysI.n; b++) {
        if ((p = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        SINDX(psfkx, p, SUB(INDX(psfkx, p), VAL1(fk, b)));
        SINDX(pstkx, p, SUB(INDX(pstkx, p), 
                            ADD(VAL1(tk, b),
                                CROSS(VAL1(SysI.rcom, b), VAL1(fk, b)))));
    }

    if (sdfast_opt.formulation == OPT_KANE)
        generic_kane_fs(F, 0, VAL(tau), psfkx, pstkx, fsmult, fs_expr);
    else
        generic_ordern_fs(F, 0, VAL(tau), psfkx, pstkx, fsmult, fs_expr);

    DISPOSE_EXPR(UNUSE(psfkx));
    DISPOSE_EXPR(UNUSE(pstkx));
}

/* COMPUTE_reaction
 *
 * Compute the reaction forces (fc) and torques (tc) at each tree joint.
 * Assumes that PRINT_SDRHS has already been called.
 *
 * Note: we define reaction loads to be *all* the loads felt by the 
 * outboard body at the joint, including loads from the joint constraints
 * and loads due to applied hinge torques.
 *
 * For joints in the (pseudo) tree system:
 *
 *    temporaries: ffkb[n], ttkb[n], ffk[s], ttk[s], fccikt[s]
 *
 *    initialize:
 *      ffkb[b] = ufk[b] + ltaufk[b] + mfk[b]
 *      ttkb[b] = utk[b] + ltautk[b] + mtk[b]    
 *
 *      ffk[k] = sum(ffkb[b])     
 *      ttk[k] = sum(ttkb[b] + rcom[b] X ffkb[b])
 * 
 *      sum over all bodies b such that k = headpseudo(b)
 *
 *    for k = s-1 downto 0
 *      fc[k] = mk[k]*(AnkAtk[k] - gk[k]) - ffk[k]
 *      tc[k] = ik[k]*onk[k] + WkIkWk[k] - (ttk[k] + rk[k] X fc[k])
 *      
 *      if ((i=inb(k)) == ground)
 *         continue;
 *
 *      fccikt[k] = fc[k]*cikt[k]
 *      ffk[i] = ffk[i] - fccikt[k]
 *      ttk[i] = ttk[i] - (tc[k]*cikt[k] + rpri[k] X fccikt[k])
 *    next k
 *
 * We ignore tree weld joints here, since they have no presence in the 
 * pseudobody system.  Also, forces applied to any real body which is
 * welded to ground are irrelevant here as well.
 *
 * This is an Order(N) algorithm.
 */
void COMPUTE_reaction(FILE *F)
{
    register Index_t i,k,inb,realbody;
    expr fc_expr, tc_expr, ffk_expr, ttk_expr, fccikt_expr, ffkbx, ttkbx;

    ffkbx       = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    ttkbx       = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    for (i = 0; i < SysI.n; i++) {
        SINDX(ffkbx, i, ADD(VAL1(ufk, i), 
                        ADD(SysI.nc ? VAL1(mfk, i)    : VECTOR_ZERO(),
                            SysI.sl ? VAL1(ltaufk, i) : VECTOR_ZERO())));
        SINDX(ttkbx, i, ADD(VAL1(utk, i), 
                        ADD(SysI.nc ? VAL1(mtk,i)     : VECTOR_ZERO(),
                            SysI.sl ? VAL1(ltautk, i) : VECTOR_ZERO())));
    }
    ASSIGN_CLN(F, ffkb, UNUSE(ffkbx));
    ASSIGN_CLN(F, ttkb, UNUSE(ttkbx));

    if (SysI.s == 0)
        return;

    fc_expr     = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    tc_expr     = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    ffk_expr    = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    ttk_expr    = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    fccikt_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++) {
        SINDX(ffk_expr, k, VECTOR_ZERO());
        SINDX(ttk_expr, k, VECTOR_ZERO());
    }

    /* initialize */
    for (i = 0; i < SysI.n; i++) {
        if (rbod_is_gnd(i))
            continue;
        k = SysI.LastDOF[i];

        /* collect all the active forces and torques on this pseudo body*/
        SINDX(ffk_expr, k, ADD(INDX(ffk_expr, k), VAL1(ffkb, i)));
        SINDX(ttk_expr, k, ADD(INDX(ttk_expr, k),
                               ADD(VAL1(ttkb, i),
                                   CROSS(VAL1(SysI.rcom, i), VAL1(ffkb, i)))));
    }

    /* Flush out the complicated elements of ffk and ttk. */
    for (k = 0; k < SysI.s; k++) {
        FLUSH_VEC(F, ffk, k, ffk_expr, INDX(ffk_expr, k));
        FLUSH_VEC(F, ttk, k, ttk_expr, INDX(ttk_expr, k));
    }

    /* `clean' as we go */
    for (k = SysI.s-1; k >= 0; k--) {
        /* ank & onk aren't defined for non-realbodies -- don't access them! */
        realbody = SysI.PseudoBodies[k].realbody;
        /* compute fc */
        FLUSH_VEC_NONCONST(F, fc, k, fc_expr, 
                      SUB(realbody ? MUL(VAL1(SysI.psmk,k), 
                                         SUB(VAL1(AnkAtk,k), VAL1(gk,k)))
                                   : VECTOR_ZERO(),
                          INDX(ffk_expr,k)));

        /* compute tc */
        FLUSH_VEC_NONCONST(F, tc, k, tc_expr,
                      SUB(realbody ? ADD(MATMUL(VAL1(SysI.psik,k), VAL1(onk,k)),
                                         VAL1(WkIkWk,k))
                                   : VECTOR_ZERO(),
                          ADD(INDX(ttk_expr,k), 
                              CROSS(VAL1(SysI.psrk,k),INDX(fc_expr,k)))));

        inb = SysI.PseudoBodies[k].jnt.InbBody;
        if (inb == cGroundBody)
            continue;

        /* compute fccikt */
        FLUSH_VEC_NONCONST(F, fccikt, k, fccikt_expr,
                        MATMUL(INDX(fc_expr,k), TRANSPOSE(VAL1(Cik,k))));

        /* modify ffk[inb] */
        FLUSH_VEC_NONCONST(F, ffk, inb, ffk_expr,
                        SUB(INDX(ffk_expr,inb),INDX(fccikt_expr,k)));

        /* modify ttk[inb] */
        FLUSH_VEC_NONCONST(F, ttk, inb, ttk_expr,
                        SUB(INDX(ttk_expr,inb),
                            ADD(MATMUL(INDX(tc_expr,k),TRANSPOSE(VAL1(Cik,k))),
                                CROSS(VAL1(rpri,k),INDX(fccikt_expr,k)))));
    }

    ASSIGN(ffk,    UNUSE(ffk_expr));
    ASSIGN(ttk,    UNUSE(ttk_expr));
    ASSIGN(fc,     UNUSE(fc_expr));
    ASSIGN(tc,     UNUSE(tc_expr));
    ASSIGN(fccikt, UNUSE(fccikt_expr));
}

/* COMPUTE_ltauforces
 *
 * This routine computes expressions representing the forces and torques
 * applied to the bodies by the loop joints as a result of `hinge torques' 
 * applied to the loop joints' `hinges'.  These torques may either be 
 * user-supplied or generated as a result of prescribed motion at loop hinges.
 *
 * Input is an expression of sl (number of loop hinges) scalars representing
 * the applied torques in loop hinge order and four symbols representing 
 * resp. the force and torque applied to the inboard and outboard bodies (these
 * are just temps).  The symbols are arrays of nl (number of loop joints) 
 * vectors.  On output we will have added all the forces and torques to 
 * the appropriate bodies in passed-in expressions fx and tx.
 *
 * You must make sure that the effects of runtime prescribed motion are
 * accounted for in the passed-in taux, because this routine will apply
 * all the taux's indiscriminately.
 */

#define TAUX(indx) INDX(taux, indx)

void COMPUTE_ltauforces(FILE *F,
                   expr taux, 
                   sym tmpv1,
                   sym tmpv2,
                   sym  tmpv3,
                   sym  fi,
                   sym  fo,
                   sym  ti,
                   sym  to,
                   expr fx, 
                   expr tx)
{
    Index_t frst, gfrst, lj, inb, outb;
    JointKind_t jk;
    expr fix,fox,tix,tox,tempx;

    fix = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    fox = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    tix = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    tox = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));

    taux = INUSE(taux);

    for (lj=0; lj < SysI.nl; lj++) {
        inb = SysI.LoopConst[lj].jnt.InbBody;
        outb = SysI.LoopConst[lj].jnt.OutbBody;
        jk = SysI.LoopConst[lj].jnt.JointKind;
        frst = SysI.FirstDOF[SysI.n+lj];

        switch (jk) {
            case cPinJoint:
                SINDX(fix, lj, VECTOR_ZERO());
                SINDX(fox, lj, VECTOR_ZERO());

/* for reaction torque, just add this `to' */
                FLUSH_VEC(F, to, lj, tox, 
                          MUL(TAUX(frst),VAL1(opin,lj)));

                FLUSH_VEC(F, ti, lj, tix, 
                          NEG(MATMUL(INDX(tox,lj),TRANSPOSE(VAL1(Cio,lj)))));
                break;
            
            case cUjoint:
                SINDX(fix, lj, VECTOR_ZERO());
                SINDX(fox, lj, VECTOR_ZERO());

/* changed to: (these are dual basis vectors)
 *   Watch out!  This blows up when denominators go to zero which means
 *   ipin*opin = 1.
 *   Test by comparing misaligned U to tree gimbal with same misalignment.
 *
 *   opini = opin*Coi = vt1
 *   n = ipin X opini          (tmpv1)
 *   vec1 = opini X n          (tmpv2)
 *   vec2 = n X ipin           (tmpv3)
 *   s1 = -tau1/(ipin*vec1)    (or 0 if u1*v1=1)
 *   s2 = -tau2/(opini*vec2)   (or 0 if u1*v1=1)
 *   ti = s1*vec1 + s2*vec2 
 *   to = -ti*Cio            <-- this gets added to reaction torque
 */  
                /* Make sure we don't print out all these temps if they're
                 * not going to be used!
                 */
                if (IS_ZERO(INDX(taux,frst)) && IS_ZERO(INDX(taux,frst+1)))
                    FLUSH_VEC(F, ti, lj, tix, VECTOR_ZERO());
                else {
                    ASSIGN_CLN(F, tmpv1, CROSS(VAL1(ipin,lj),VAL1(vt1,lj)));
                    ASSIGN_CLN(F, tmpv2, CROSS(VAL1(vt1,lj),VAL(tmpv1)));
                    ASSIGN_CLN(F, tmpv3, CROSS(VAL(tmpv1),VAL1(ipin,lj)));

                    /* Can't use FLUSH_VEC here because we have to FLUSHNONCONST
                       in order to avoid dangling references to the tmpv's. */

                    /* We have to do this very carefully to avoid a
                     * generation-time divide by zero.  We split out the 
                     * divide-free case under the QUES() below if we can make
                     * the determination at generation time.
                     * (sherm 981020)
                     */
                    tempx = NEARTO(ABS(DOT(VAL1(ipin,lj),VAL1(vt1,lj))),
                                   SCALAR_ONE(), cNearZero);
                    if (IS_ONE(tempx)) {
                        FLUSH_VEC_NONCONST(F, ti, lj, tix, 
                            VECTOR_ZERO());
                        DISPOSE_EXPR(tempx);
                    } else {
                        FLUSH_VEC_NONCONST(F, ti, lj, tix, 
                            QUES(tempx,
                                 VECTOR_ZERO(),
                                 NEG(ADD(MUL(DVD(TAUX(frst),
                                                 DOT(VAL1(ipin,lj),VAL(tmpv2))),
                                             VAL(tmpv2)),
                                         MUL(DVD(TAUX(frst+1),
                                                 DOT(VAL1(vt1,lj),VAL(tmpv3))),
                                             VAL(tmpv3))))));
                    }
                }

/* for reaction torque, just add this `to' */
                FLUSH_VEC(F, to, lj, tox, 
                         NEG(MATMUL(INDX(tix,lj),VAL1(Cio,lj))));
                break;
        
/* Handle all joints containing gimbals together, except that we'll
 * compute forces first since they're different for each joint.
 * We note which axis is the first gimbal axis as well.
 */
            case c3dJoint:
                SINDX(fix, lj, VECTOR_ZERO());
                gfrst = frst;
                goto doGimbal;

            case cBearingJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(MUL(TAUX(frst),VAL1(ipin,lj))));
                gfrst = frst+1;
                goto doGimbal;

            case cBushingJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(ADD(MUL(TAUX(frst),VAL1(ipin,lj)),
                              ADD(MUL(TAUX(frst+1),VAL1(ipin2,lj)),
                                  MUL(TAUX(frst+2),VAL1(iperp,lj))))));
                gfrst = frst+3;

       doGimbal:

/* for reaction force, just add this `fo' */
                FLUSH_VEC(F, fo, lj, fox, 
                          NEG(MATMUL(INDX(fix,lj),VAL1(Cio,lj))));

                /* This is very nasty, requiring dual basis vectors to
                   find the correct torques from the taus.  Furthermore,
                   there is a singular configuration when q2= +/- pi/2. 
                   In the singular configuration, we simply ignore any
                   torques applied to the first or third pins, and
                   directly transmit to each body the torque applied to 
                   the second pin. */

                /* Make sure we don't do anything if all the tau's are
                 * symbolically zero here.
                 */
                if (IS_ZERO(INDX(taux,gfrst)) && IS_ZERO(INDX(taux,gfrst+1)) 
                    && IS_ZERO(INDX(taux,gfrst+2)))
                    FLUSH_VEC(F, ti, lj, tix, VECTOR_ZERO());
                else {
                    ASSIGN_CLN(F, tmpv1, CROSS(VAL1(ipin2x,lj),VAL1(vt1,lj)));
                    ASSIGN_CLN(F, tmpv2, CROSS(VAL1(vt1,lj),VAL1(ipin,lj)));
                    ASSIGN_CLN(F, tmpv3, CROSS(VAL1(ipin,lj),VAL1(ipin2x,lj)));

                    /* First compute the inboard torque without accounting for
                       the torque generated by fi (we're using ti as a temp). */

                    /* Can't use FLUSH_VEC here because we have to FLUSHNONCONST
                       in order to avoid dangling references to the tmpv's. */

                    /* We have to do this very carefully to avoid a
                     * generation-time divide by zero.  We split out the 
                     * divide-free case under the QUES() below if we can make
                     * the determination at generation time.
                     * (sherm 981020)
                     */
                    tempx = NEARTO(ABS(DOT(VAL1(ipin,lj),VAL1(vt1,lj))),
                                   SCALAR_ONE(), cNearZero);
                    if (IS_ONE(tempx)) {
                        FLUSH_VEC_NONCONST(F, ti, lj, tix, 
                            NEG(MUL(TAUX(gfrst+1),VAL1(ipin2x,lj))));
                        DISPOSE_EXPR(tempx);
                    } else {
                        FLUSH_VEC_NONCONST(F, ti, lj, tix, 
                            NEG(QUES(NEARTO(ABS(DOT(VAL1(ipin,lj),VAL1(vt1,lj))),
                                            SCALAR_ONE(), cNearZero),
                                     MUL(TAUX(gfrst+1),VAL1(ipin2x,lj)),
                                     ADD(MUL(TAUX(gfrst),
                                             DVD(VAL(tmpv1),
                                                 DOT(VAL1(ipin,lj),VAL(tmpv1)))),
                                         ADD(MUL(TAUX(gfrst+1),
                                                 DVD(VAL(tmpv2),DOT(VAL1(ipin2x,lj),
                                                                    VAL(tmpv2)))),
                                             MUL(TAUX(gfrst+2),
                                                 DVD(VAL(tmpv3), DOT(VAL1(vt1,lj),
                                                                     VAL(tmpv3))))))
                        )));
                    }
                }
                
/* for reaction torque, just add (to - btj X fo) to ltc */
                /* Watch out for the reference to ti here, which will
                   be changed below -- we MUST ensure that we're referring
                   to the old value! Hence, NONCONST here. */
                FLUSH_VEC_NONCONST(F, to, lj, tox,
                          ADD(NEG(MATMUL(INDX(tix,lj),VAL1(Cio,lj))),
                              CROSS(VAL1(SysI.lbtj,lj),INDX(fox,lj))));

                /* Now adjust the inboard torque to reflect the fi 
                   contribution. */
                FLUSH_VEC(F, ti, lj, tix,
                          ADD(INDX(tix,lj), 
                              CROSS(VAL1(vt10c,lj),INDX(fix,lj))));
                break;

            case cBallJoint:
                SINDX(fix, lj, VECTOR_ZERO());
                SINDX(fox, lj, VECTOR_ZERO());
                tempx = NEW_VECX(cScalarVal);
                SINDX(tempx,0,TAUX(frst));
                SINDX(tempx,1,TAUX(frst+1));
                SINDX(tempx,2,TAUX(frst+2));

/* for reaction, add `to' to reaction torque */
                FLUSH_VEC(F, to, lj, tox, tempx);
                FLUSH_VEC(F, ti, lj, tix, 
                          NEG(MATMUL(INDX(tox,lj),TRANSPOSE(VAL1(Cio,lj)))));
                break;

            case c6dJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(ADD(MUL(TAUX(frst),VAL1(ipin,lj)),
                              ADD(MUL(TAUX(frst+1),VAL1(iref,lj)),
                                  MUL(TAUX(frst+2),VAL1(iperp,lj))))));
/* for reaction force, just add this `fo' */
                FLUSH_VEC(F, fo, lj, fox, 
                          NEG(MATMUL(INDX(fix,lj),VAL1(Cio,lj))));
                tempx = INUSE(NEW_VECX(cScalarVal));
                SINDX(tempx,0,TAUX(frst+3));
                SINDX(tempx,1,TAUX(frst+4));
                SINDX(tempx,2,TAUX(frst+5));

/* for reaction torque, just add (to - btj X fo) to ltc */
                FLUSH_VEC(F, to, lj, tox,
                          ADD(tempx, 
                              CROSS(VAL1(SysI.lbtj,lj),INDX(fox,lj))));
                FLUSH_VEC(F, ti, lj, tix, 
                          SUB(CROSS(VAL1(vt10c,lj),INDX(fix,lj)),
                              MATMUL(tempx,TRANSPOSE(VAL1(Cio,lj)))));
                DISPOSE_EXPR(UNUSE(tempx));
                break;


            case cSlidingJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(MUL(TAUX(frst),VAL1(ipin,lj))));

/* for reaction force, just add this `fo' */
                FLUSH_VEC(F, fo, lj, fox, 
                          NEG(MATMUL(INDX(fix,lj),VAL1(Cio,lj))));

                FLUSH_VEC(F, ti, lj, tix,
                          CROSS(VAL1(vt10c,lj),INDX(fix,lj)));
                FLUSH_VEC(F, to, lj, tox,
                          CROSS(VAL1(SysI.lbtj,lj),INDX(fox,lj)));
                break;

            case cCylJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(MUL(TAUX(frst),VAL1(ipin,lj))));

/* for reaction force, just add `fo' */
                FLUSH_VEC(F, fo, lj, fox, 
                          NEG(MATMUL(INDX(fix,lj),VAL1(Cio,lj))));

                FLUSH_VEC(F, ti, lj, tix,
                          SUB(CROSS(VAL1(vt10c,lj),INDX(fix,lj)),
                              MUL(TAUX(frst+1),
                                  MATMUL(VAL1(opin,lj),
                                         TRANSPOSE(VAL1(Cio,lj))))));

/* for reaction torque, just add (to - btj X fo) to ltc */
                FLUSH_VEC(F, to, lj, tox,
                          ADD(MUL(TAUX(frst+1), VAL1(opin,lj)),
                              CROSS(VAL1(SysI.lbtj,lj),INDX(fox,lj))));
                break;

            case cPlanarJoint:
                FLUSH_VEC(F, fi, lj, fix, 
                          NEG(ADD(MUL(TAUX(frst),VAL1(iref,lj)),
                                  MUL(TAUX(frst+1),VAL1(iperp,lj)))));
/* for reaction force, just add this `fo' */
                FLUSH_VEC(F, fo, lj, fox, 
                          NEG(MATMUL(INDX(fix,lj),VAL1(Cio,lj))));

                FLUSH_VEC(F, ti, lj, tix,
                          SUB(CROSS(VAL1(vt10c,lj),INDX(fix,lj)),
                              MUL(TAUX(frst+2),
                                      MATMUL(VAL1(opin,lj),
                                             TRANSPOSE(VAL1(Cio,lj))))));
/* for reaction torque, just add (to - btj X fo) to ltc */
                FLUSH_VEC(F, to, lj, tox,
                          ADD(MUL(TAUX(frst+2), VAL1(opin,lj)),
                              CROSS(VAL1(SysI.lbtj,lj),INDX(fox,lj))));
                break;

            case cWeldJoint:
                /* nothing to do - can't apply torques here */
                SINDX(fix, lj, VECTOR_ZERO());
                SINDX(fox, lj, VECTOR_ZERO());
                SINDX(tix, lj, VECTOR_ZERO());
                SINDX(tox, lj, VECTOR_ZERO());
                break;
        }
        if (inb != cGroundBody) { 
            SINDX(fx, inb, ADD(INDX(fx,inb), INDX(fix,lj)));
            SINDX(tx, inb, ADD(INDX(tx,inb), INDX(tix,lj)));
        }
        if (outb != cGroundBody) {  
            SINDX(fx, outb, ADD(INDX(fx,outb), INDX(fox,lj)));
            SINDX(tx, outb, ADD(INDX(tx,outb), INDX(tox,lj)));
        }
    }

    taux = UNUSE(taux);

    ASSIGN(fi, UNUSE(fix)); /* no clean necessary due to FLUSH_VECs above */
    ASSIGN(fo, UNUSE(fox));
    ASSIGN(ti, UNUSE(tix));
    ASSIGN(to, UNUSE(tox));
}

/* PRINT_SDEQUIVHT
 *
 * Generate the sdequivht() subroutine.  PRINT_SDLHS must already have 
 * been called.
 *
 * This routine can be called from state 2 (Kinematics Ready) or
 * state 3 (Dynamics Ready).  It returns the equivalent hinge torques
 * on the tree hinges which produce the same motion as all the applied
 * forces and torques.  This can be use, for example, for gravity 
 * compensation:  
 *    Call sdstate(), then without applying any forces other than 
 *    gravity, call sdequivht().  If you negate the returned hinge
 *    torques and apply them to all the tree hinges, you will cancel
 *    the effect of gravity.
 * sdequivht() reports the equivalent hinge torques to all the forces
 * and torques applied so far.  If more forces or torques are applied,
 * another call to sdequivht() will include the effects of the new ones
 * as well.
 *
 * No user-visible state change occurs as a result of a user's sdequivht() 
 * call.  
 */
void PRINT_SDEQUIVHT(FILE *F)
{
    Index_t b,k;
    sym  fstareq, tstareq, tau;
    expr fstx, tstx, taux;
    opstats_t opcnt;

    START_COMPSUB(&opcnt);

    declare_proc(F, 0, "equivht",
      VT_USER|VT_DSYM, &SysI.type_Arr_s,    "tau", &tau,
      0);

    efprintf(F, 
      "%{Compute tree hinge torques to match effect of applied loads\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    /* local symbols */
    declare_vars(F, 0,
      VT_USER|VT_DSYM, &SysI.type_Vec_s, "fstareq", &fstareq,
      VT_DUP|VT_DSYM,                    "tstareq", &tstareq,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdequivht, ERR_sdstateMustBeCalledFirst);

    if (SysI.sl) {
        efprintf(F, "%{\nCompute forces due to loop-joint hinge torques\n%}");
        CALL0("%Adoltau");
        opcnt.ndoltau++;
    }

    if (SysI.s == 0)
        goto allDone;

    /* Go look at COMPUTE_Fstar, COMPUTE_Tstar, and COMPUTE_fs0 
     * for what's happening here.  These are the same computations
     * as they would be if the u's were all zero in those routines.
     */
    efprintf(F, "%{\nCompute fstareq (forces)\n%}");
    fstx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    for (k = 0; k < SysI.s; k++)
        SINDX(fstx, k, NEG(MUL(VAL1(SysI.psmk,k),VAL1(gk,k))));
    for (b = 0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        if (SysI.sl)
            SINDX(fstx, k, 
                  SUB(INDX(fstx, k), ADD(VAL1(ufk, b),VAL1(ltaufk, b))));
        else 
            SINDX(fstx, k, 
                  SUB(INDX(fstx, k), VAL1(ufk, b)));
    }
    ASSIGN_CLN(F, fstareq, UNUSE(fstx));

    efprintf(F, "%{\nCompute tstareq (torques)\n%}");
    tstx  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    for (k = 0; k < SysI.s; k++)
        SINDX(tstx, k, VECTOR_ZERO());
    for (b = 0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        if (SysI.sl)
            SINDX(tstx, k, 
               SUB(INDX(tstx, k), 
                   ADD(ADD(VAL1(utk, b),VAL1(ltautk, b)),
                       CROSS(VAL1(SysI.rcom, b), 
                             ADD(VAL1(ufk, b), VAL1(ltaufk, b))))));
        else
            SINDX(tstx, k, 
               SUB(INDX(tstx,k), 
                   ADD(VAL1(utk, b),
                       CROSS(VAL1(SysI.rcom, b), VAL1(ufk, b)))));;
    }
    ASSIGN_CLN(F, tstareq, UNUSE(tstx));

    efprintf(F, 
        "%{\nCompute taus (RHS ignoring constraints and inertial forces)\n%}");

    taux = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));
    
    if (sdfast_opt.formulation == OPT_KANE) {
        CALL0("%Adovpk");         /* Wpk, Vpk */
        opcnt.ndovpk++;
        generic_kane_fs(F,1,VAL(utau),VAL(fstareq),VAL(tstareq),tau,taux);
    } else
        generic_ordern_fs(F,1,VAL(utau),VAL(fstareq),VAL(tstareq),tau,taux);

    DISPOSE_EXPR(UNUSE(taux));

  allDone:

    efprintf(F, "%{\nOp counts below do not include called subroutines\n%}");

    END_COMPSUB(F,&opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);
    fflush(F);
}

/* PRINT_SDFULLTRQ
 *
 * Generate the sdcomptrq(), sdfulltrq(), and sdmulttrq() subroutines.  
 *
 *    sdfulltrq(udot,mult) computes
 *                      T
 *        T = M udot + A mult - f
 *
 *    sdcomptrq(udot) computes
 *
 *        T = M udot - f
 *
 *    sdmulttrq(mult) computes
 *
 *             T
 *        T = A mult
 *
 * where M is the system mass matrix, A is the constraint matrix, and 
 * f is the hinge-torque equivalent of all applied and intertial
 * loads (but not any contraint-generated loads).
 *
 * These routines can be called from state 2 (Kinematics Ready) or
 * state 3 (Dynamics Ready).  
 *
 * For a given set of udots, sdfulltrq() and sdcomptrq() return the 
 * additional hinge torques which would be 
 * required to produce those udots.  Sdfulltrq() accepts udots and multipliers,
 * and accounts for both.  Sdcomptrq() (which is simply a call to sdfulltrq()
 * with all multipliers set to zero) computes torques which would produce
 * the udots if constraints were ignored.  In systems with no constraints,
 * the two routines are the same.
 *
 * This can be used, for example, for feedforward on a manipulator:
 *    Call sdstate(), then apply any forces (gravity, springs, etc.)
 *    Choose udots, call sdcomptrq().
 *    The hinge torques returned from sdcomptrq() will be those additional
 *    torques which would be required to produce the passed-in udots,
 *    assuming there were no constraints.
 * sdcomptrq() reports the computed torques appropriate to all the forces
 * and torques applied so far.  If more forces or torques are applied,
 * another call to sdcomptrq() will include the effects of the new ones
 * as well.
 *
 * Sdfulltrq() exists primarily for use by sdresid().
 *
 * Sdmulttrq() provides a very fast way to multiply anything by the
 *                    T
 * constraint matrix A.  
 *
 * No user-visible state change occurs as a result of a user's call to
 * any of these routines.  Despite the appearances of matrix-vector
 * products in the above equations, all of these routines are O(N)
 * when using the O(N) Formulation.
 */
void PRINT_SDFULLTRQ(FILE *F,
                opstats_t *opcnt)
{
    int k,b;
    sym udotin, Otkr, Atir, Atkr, fstarr, tstarr, trqout;
    expr fstarrx, tstarrx, trqoutx, taux, ftemp, ttemp;
    char str_nc[10], str_s[10];

    esprintf(str_nc, "%d", SysI.nc);
    esprintf(str_s,  "%d", SysI.s);

    START_COMPSUB(opcnt);

    /* SDFULLTRQ() */

    declare_proc(F, 0, "fulltrq",
      VT_USER|VT_DSYM, &SysI.type_Arr_s,            "udotin", &udotin,
      VT_USER, &SysI.type_Arr_nc,                   "multin",
      VT_USER|VT_DSYM, &SysI.type_Arr_s,            "trqout", &trqout,
      0);

    efprintf(F, 
      "%{Compute hinge torques which would produce indicated udots\n%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_USER|VT_DSYM, &SysI.type_Vec_s, "fstarr", &fstarr,
      VT_DUP|VT_DSYM,                    "tstarr", &tstarr,
      VT_DUP|VT_DSYM,                    "Otkr",   &Otkr,
      VT_DUP|VT_DSYM,                    "Atir",   &Atir,
      VT_DUP|VT_DSYM,                    "Atkr",   &Atkr,
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdfulltrq, ERR_sdstateMustBeCalledFirst);

    if (SysI.sl) {
        efprintf(F, "%{\nCompute forces due to loop-joint hinge torques\n%}");
        CALL0("%Adoltau");
        opcnt->ndoltau++;
    }

    if (SysI.nc) {
        efprintf(F, "%{\nCompute multiplier-generated forces\n%}");
        CALL("%Amfrc(multin)");
        opcnt->nmfrc++;
    } 

    if (SysI.s == 0)
        goto doneWithFullTrq;

    efprintf(F, 
        "%{\nAccount for inertial accelerations and supplied udots\n%}");

    /* Calculate Atk and Otk, but with extra udot term added in, into
     * Atkr and Otkr.
     */
    COMPUTE_generic_Otk(F,1,udotin,Otkr);
    COMPUTE_generic_Atk(F,1,udotin,Otkr,Atir,Atkr);

    efprintf(F, "%{\nAccumulate all forces and torques\n%}");

    /* These next two calculations are similar to those done by COMPUTE_Fstar
     * and COMPUTE_Tstar.  But the sign is opposite!
     *
     *    fstarr[k] =  mk[k]*(gk[k] - Atkr[k]) + sum(ufk[b]+mfk[b]+ltaufk[b]) 
     *
     *    tstarr[k] = - (WkIkWk[k] + ik[k]*Otkr[k]) 
     *                      + sum(utk[b]+mtk[b]+ltautk[b]+shft[b]) 
     *
     *    summation is over all b for which k=headpseudo(b)
     *    shft[b] = rcom[b] X (ufk[b]+mfk[b]+ltaufk[b]), that is, the torque
     *       produced by shifting the forces to composite pseudobody COM
     */
    fstarrx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    tstarrx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    for (k = 0; k <SysI.s; k++) {
        SINDX(fstarrx, k, MUL(VAL1(SysI.psmk,k), 
                              SUB(VAL1(gk,k), VAL1(Atkr,k))));
        SINDX(tstarrx, k, NEG(ADD(VAL1(WkIkWk,k),
                                  MATMUL(VAL1(SysI.psik,k),VAL1(Otkr,k)))));
    }
    for (b = 0; b < SysI.n; b++) {
        if ((k = SysI.LastDOF[b]) < 0)        /* ignore bodies welded to ground */
            continue;

        ftemp = INUSE(ADD(VAL1(ufk,b), 
                          ADD(SysI.nc ? VAL1(mfk,b) : VECTOR_ZERO(),
                              SysI.sl ? VAL1(ltaufk, b) : VECTOR_ZERO())));
        SINDX(fstarrx, k, ADD(INDX(fstarrx, k), ftemp));
        
        ttemp = ADD(VAL1(utk,b), 
                    ADD(SysI.nc ? VAL1(mtk,b) : VECTOR_ZERO(),
                        SysI.sl ? VAL1(ltautk, b) : VECTOR_ZERO()));
        SINDX(tstarrx, k, ADD(INDX(tstarrx, k),
                              ADD(ttemp,
                                  CROSS(VAL1(SysI.rcom, b), ftemp))));
        DISPOSE_EXPR(UNUSE(ftemp));
    }
    ASSIGN_CLN(F, fstarr, UNUSE(fstarrx));
    ASSIGN_CLN(F, tstarr, UNUSE(tstarrx));

    /* Now compute the torques (same calculation as for fs's) using the
     * generic order(n) routine, with the following as inputs:
     *   tau = -(utau+mtau)
     *   fstar = fstarr
     *   tstar = tstarr
     * Output goes to trqout. 
     */
    
    efprintf(F, "%{\nNow calculate the torques\n%}");
    taux = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));
    for (k=0; k<SysI.s; k++)
        SINDX(taux, k, NEG(ADD(VAL1(utau,k),
                               SysI.nc ? VAL1(mtau,k) : SCALAR_ZERO())));
    trqoutx = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));
    if (sdfast_opt.formulation == OPT_KANE) {
        CALL0("%Adovpk");         /* Wpk, Vpk */
        opcnt->ndovpk++;
        generic_kane_fs(F,1, taux, VAL(fstarr), VAL(tstarr), trqout, trqoutx);
    } else
        generic_ordern_fs(F,1, taux, VAL(fstarr), VAL(tstarr), trqout, trqoutx);
    
    DISPOSE_EXPR(UNUSE(taux));
    ASSIGN(fstarr, NULLEXPR);
    ASSIGN(tstarr, NULLEXPR);
    ASSIGN(Otkr, NULLEXPR);
    ASSIGN(Atir, NULLEXPR);
    ASSIGN(Atkr, NULLEXPR);

  doneWithFullTrq:

    efprintf(F, "%{\nOp counts below do not include called subroutines\n%}");

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);

    /* SDCOMPTRQ() */

    declare_proc(F, 0, "comptrq",
      VT_USER, &SysI.type_Arr_s,            "udotin",
      VT_USER, &SysI.type_Arr_s,            "trqout",
      0);

    efprintf(F, 
  "%{Compute hinge torques to produce these udots, ignoring constraints\n%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_vars(F, 0, 
      VT_INTEGER|VT_COND, SysI.nc,        "i",
      VT_USER, &SysI.type_Arr_nc,           "multin",
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdcomptrq, ERR_sdstateMustBeCalledFirst);

    if (SysI.nc) {
        FORCNT("100", "i", str_nc);
          RSET("multin%(i%)", 0.);
        ENDFOR("100");
    }

    CALL("%Afulltrq(udotin,multin,trqout)");
    efprintf(F, Lang->proc_end);
    
    /* SDMULTTRQ() */

    declare_proc(F, 0, "multtrq",
      VT_USER, &SysI.type_Arr_nc,           "multin",
      VT_USER, &SysI.type_Arr_s,            "trqout",
      0);

    efprintf(F, 
  "%{Compute hinge trqs which would be produced by these mults.\n%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_vars(F, 0, 
      VT_INTEGER,        "i",
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdmulttrq, ERR_sdstateMustBeCalledFirst);

    if (SysI.nc == 0) {
        FORCNT("100", "i", str_s);
          RSET("trqout%(i%)", 0.);
        ENDFOR("100");
    } else {
        CALL("%Amfrc(multin)");                /* SDMFRC(MULTIN) */
        CALL0("%Afsmult");                /* SDFSMULT() */
        FORCNT("100", "i", str_s);
          efprintf(F, "trqout%(i%)%=%s%(i%)%;\n", PRINTNAME(fs));
        ENDFOR("100");
    }

    efprintf(F, Lang->proc_end);
    
    fflush(F);
}

/*
 * This symbolic subroutine generates the code required to compute the
 * reaction forces & torques at a tree weld joint for a single real body b 
 * (whose inboard tree joint is a weld).  It is expected that all reactions
 * are known for any joint involving b (including loop joints) except the 
 * inboard one.  These reactions are available in the passed in symbols
 * "frc" and "trq" which are dimensioned nj vectors.  All vectors are given
 * in the frame of that joint's outboard part.
 *
 * The method used is to obtain all the loads being applied to b and its
 * motion.  F=ma tells us what the force should be to produce this motion;
 * anything not accounted for by the loads must be being provided by the
 * unknown reaction. 
 *
 * We shift everything to the body's COM for computation, then finally 
 * shift to the desired hinge point.
 */
void COMPUTE_weld_reaction(FILE *F, int b, sym frc, sym trq, 
                      expr *fexpr, expr *texpr)
{
    int  j,k,p;
    expr rx, fstarx, tstarx, f, t;

    /* First compute fstar and tstar.  These are the force and torque
     * at the body COM which must have been present to produce the observed
     * motion.  If b is welded to ground, fstar=tstar=0.  Otherwise,
     *
     *   comacc = AnkAtk[k] + dyad[b]*rcom[b]    (this is anb[b], but expr in b)
     *   fstar =  mk[b] * comacc
     *   tstar =  ik[b]*onk[k] + WkIkbWk[b]
     */

     if ((k = SysI.LastDOF[b]) < 0) {
         fstarx = VECTOR_ZERO();
         tstarx = VECTOR_ZERO();
     } else {
         fstarx = MUL(VAL1(SysI.mk, b), 
                      ADD(VAL1(AnkAtk,k), VAL1(dyrcom, b)));
         tstarx = ADD(MATMUL(VAL1(SysI.ik, b), VAL1(onk, k)),
                      VAL1(WkIkbWk, b));
     }

     /* Now compute f and t, the total forces and torques we can find
      * being applied to this body, at its COM.
      *
      *  f = mk[b]*gk[k] + ufk[b] + mfk[b] + ltaufk[b] + sum(RF[j])
      *
      *  t = utk[b] + mtk[b] + ltautk[b] + sum(RT[j] + r[j] X RF[j])
      *
      *    where RF[j] is the reaction force applied to b by joint j at
      *    the outboard hinge point, RT[j] is the reaction torque applied 
      *    to b by joint j, r is the vector from b's COM to the outboard
      *    hinge point (adjust for sliding if necessary), and
      *    the summation is for all tree joints j for which b is the 
      *    inboard body
      *  
      * Note that outboard reactions have to be negated and transformed to 
      * body b's frame, since they are initially in the outboard body's frame
      * and applied to the outboard body.
      *
      * For a body welded to ground, use the original gravity vector 
      * rather than gk.
      */

      f = ADD(MUL(VAL1(SysI.mk, b), k < 0 ? VAL(SysI.grav) : VAL1(gk, k)),
              VAL1(ffkb, b));
        
      t = VAL1(ttkb, b);

      /* find all the outboard tree joints and add their reactions in */
      for (j = b + 1; j < SysI.n; j++) {
          if (SysI.Bodies[j].jnt.InbBody != b)
              continue;

          rx = VECTOR_ZERO();
          for (p=SysI.LastDOF[j]; p >= SysI.FirstDOF[j]; p--)
              rx = ADD(MATMUL(rx, TRANSPOSE(VAL1(Cik,p))), 
                       VAL1(rpp, p));

          /* rx now goes from inb hingept to outb, in inb frame,
           * so adding ri gives the desired r.
           */
          rx = ADD(rx, VAL1(SysI.ri, j));

          p = SysI.LastDOF[j]; /* outb body's pseudobody set */

          f = ADD(f, MATMUL(NEG(VAL1(frc, j)), TRANSPOSE(VAL1(Cib, j))));
          t = ADD(t, ADD(MATMUL(NEG(VAL1(trq, j)), TRANSPOSE(VAL1(Cib, j))), 
                         CROSS(rx, MATMUL(NEG(VAL1(frc, j)), 
                                          TRANSPOSE(VAL1(Cib, j))))));
      }

      /* We're almost there.  Find the difference and shift to the weld
       * hinge point.
       */

      *fexpr = INUSE(SUB(fstarx, f));
      *texpr = ADD(SUB(tstarx, t), CROSS(*fexpr, VAL1(SysI.rk, b)));
      *fexpr = UNUSE(*fexpr);
}    
