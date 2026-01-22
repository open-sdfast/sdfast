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

/* COMPUTE mm
 *
 * The next routine computes the sXs mass matrix mm.  
 *
 * Only the diagonal and upper triangle are computed, but all computed
 * elements are assigned so that they are available numerically.  The 
 * global "mm" symbol is not assigned since there are no subsequent
 * symbolic references to the mass matrix anywhere.
 *
 * Separate routines exist below for generating the MM code and for
 * generating the local declarations needed by that code.
 *
 * The name of a variable holding the user-called routine which initiated
 * this computation is passed in in case we need to report an error.
 * (Singular mass matrix.)
 */

/* COMPUTE_kane_mm
 *
 * We use sXs temporary IkWpk[j,k]=Ik[k]*Wpk[j,k] to save redundant multiplies.
 *
 *     mm[i,j] = SUM{mk[k]*Vpk[i,k]*Vpk[j,k] + Wpk[i,k]*IkWpk[j,k]}, k=0..s-1
 *
 * Note: this calculation assumes Vpk and Wpk have been computed.
 * The mm's are written for later *numerical* access; the expressions 
 * themselves are not saved and the mm symbol is not assigned.
 *
 * This is an Order(n**3) calculation.
 */

void kane_mm_decls(FILE *F)
{
    declare_vars(F, 0,
      VT_INTEGER, "i",
      0);
}

#define ELTS_PER_FILE        1000

int
COMPUTE_kane_mm(FILE         *mainF,
                char         *rouvar,
                int        maxaux,
                char        *dynname,
                int *nxtaux)
{
    register Index_t i, j, k;
    expr temp, IkWpk_expr;
    int err_ret = 0, non_const = 0, nelts, naux, eltsper, eltcnt;
    char str_s[10];
    FILE *F;

    esprintf(str_s,  "%d", SysI.s);

    /* Calculate how many auxiliary files to generate and when to switch
     * files.  We'll put only the IkWpk calculation into the
     * current file mainF.  Then an auxiliary will be generated for each
     * ELTS_PER_FILE MM elements, and for the fraction left over.
     * That tells us how many aux files there will be (but don't go over
     * maxaux).  Then compute the actual number of elts per file for all
     * but the last file.
     */
    nelts = ((SysI.s+1)*SysI.s)/2;         /* # of elts in MM */
    naux = nelts/ELTS_PER_FILE;         /* OK if exact multiple */
    if (naux*ELTS_PER_FILE < nelts)        /* fix for the normal case */
        naux++;
    if (naux > maxaux)
        naux = maxaux;
    if (naux > 0) {
        eltsper = nelts/naux;
        if (eltsper*naux < naux)
            eltsper++;
    } else
        eltsper = 0;

    F = mainF;

    efprintf(F, "%{\nCompute mass matrix (MM)\n%}");

    CALL0("%Adovpk");

    IkWpk_expr = INUSE(NEW_2dARRAY(cVectorVal, SysI.s, SysI.s));

    /* `Clean' IkWpk as we go since we're going to use it now.
     * `Clean' mm as we go because it gets so big otherwise.
     */
    /* Compute and clean all IkWpk's first. */
    for (j = 0; j < SysI.s; j++)
        for (k = j; k < SysI.s; k++) 
            SINDX2(IkWpk_expr,j,k,MATMUL(VAL1(SysI.psik,k),VAL2(Wpk,j,k)));
    ASSIGN_CLN(F, IkWpk, UNUSE(IkWpk_expr));

    eltcnt = 0;
    for (i = 0; i < SysI.s; i++) {
        for (j = i; j < SysI.s; j++) {
            if (eltsper && (eltcnt % eltsper) == 0) {
                /* time to switch files */
                CALL2("%Adomm%02d(%s)", *nxtaux, rouvar); 
                if (F != mainF) {
                    domm_tail(F);
                    CLOSE_FILE(F);
                } else 
                    efprintf(mainF, "%<%<"); /* reset indent to left edge */
                if (openaux(&F, dynname, *nxtaux)) {
                    efprintf(mainF, "%>%>"); /* restore indentation level */
                    return 1;
                }
                domm_head(F, *nxtaux);
                (*nxtaux)++;
            }
            temp = SCALAR_ZERO();
            for (k = j; k < SysI.s; k++)
                temp = ADD(temp,
                           ADD(MUL(VAL1(SysI.psmk,k),
                                   DOT(VAL2(Vpk,i,k),VAL2(Vpk,j,k))),
                               DOT(VAL2(Wpk,i,k),VAL2(IkWpk,j,k))));
            if (i == j) {
                if (IS_CONST(temp)) {
                    if (fabs(NUMVAL(temp)) < cEquationNegligible) {
                        PRINTERR(ERR_SingularMassMatrix, 0, NULL, NULL);
                        DISPOSE_EXPR(temp);
                        err_ret = 1;
                        goto cleanup;
                    }
                } else
                    non_const = 1;
            }
            PRINT_ASSN2(F, PRINTNAME(mm), i, j, temp);
            DISPOSE_EXPR(temp);
            eltcnt++;
        }
        if (sdfast_opt.magic_no && i == (SysI.s/2)) {
          IFCOND efprintf(F, "ii%s%d", NE, sdfast_opt.magic_no);
            THEN RETURN;
          ENDIF;
        }
    }

    if (F != mainF) {
        fflush(F);
        domm_tail(F);
        CLOSE_FILE(F);
        efprintf(mainF, "%>%>"); /* restore indentation */
    }

    F = mainF;
    if (non_const) {
        efprintf(F, "%{\nCheck for singular mass matrix\n%}");
        FORCNT("100", "i", str_s);
          IFCOND efprintf(F, "%s%(i%,i%)%s%r",PRINTNAME(mm),LT,
                          cEquationNegligible);
            THEN CALL2("%Aseterr(%s,%d)", rouvar, ERR_SingularMassMatrix);
          ENDIF;
        ENDFOR("100");
    }

    fflush(F);
    if (sdfast_opt.verbose)
        printf("Mass matrix computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    cleanup:

    return err_ret;
}

/* ZAPEXPR
 *
 * Replace all lower-triangle terms in symmetric matrix expression Z
 * with references to upper-triangle terms of V, for any upper triangle
 * term which is not IS_SIMPLE.  
 *
 * Use this ONLY when you are about to print out AT LEAST all non-IS_SIMPLE 
 * elements of Z, because later uses of the lower triangle terms will refer 
 * to the upper triangle by name.
 */
static expr 
ZAPEXPR(expr Z,
        expr V)
{
    register tIndex i, j;

    Z = INUSE(Z);
    V = INUSE(V);
    for (i = 1; i < 3; i++)
        for (j = 0; j < i; j++)
            if (!IS_SIMPLE(INDX2(Z,j,i)))
                 SINDX2(Z, i, j, INDX2(V, j, i));
            else SINDX2(Z, i, j, INDX2(Z, j, i));
    DISPOSE_EXPR(UNUSE(V));
    return UNUSE(Z);
}

/* ZAPEXPR_NC
 *
 * Same as ZAPEXPR but all lower triangle terms are replaced with references
 * to uppers unless they are constants.
 *
 * Use this ONLY when you are about to print out all non-IS_CONST elements
 * of Z, because later uses of the lower triangle terms will refer to the
 * upper triangle by name.
 */
static expr 
ZAPEXPR_NC(expr Z,
           expr V)
{
    register tIndex i, j;

    Z = INUSE(Z);
    V = INUSE(V);
    for (i = 1; i < 3; i++)
        for (j = 0; j < i; j++)
            if (!IS_CONST(INDX2(Z,j,i)))
                 SINDX2(Z, i, j, INDX2(V, j, i));
            else SINDX2(Z, i, j, INDX2(Z, j, i));
    DISPOSE_EXPR(UNUSE(V));
    return UNUSE(Z);
}


/* COMPUTE_ordern_mm
 *
 * This computation for the Order(N) formulation is analogous to the mass
 * matrix computation in Kane's Formulation.  However, we compute different
 * variables here:
 *
 *      DD[k]                            scalar
 *
 *       G[k] = [ G1[k] ]                G1 and G2 are vectors
 *              [ G2[k] ]
 *
 *  Note we don't compute G[b] if inb(b) is ground.
 *
 *  These are computed as follows:
 *
 *      Assume rikt[k] = TILDA(rik2[k])*Cik[k]
 *             mkrk[k] = mk[k]*TILDA(rk[k])
 *
 *      Initialize:
 *
 *        for (k=0; k<SysI.s; k++) {
 *            P11[k] = mk[k]*E            (E is 3x3 identity matrix)
 *            Pd[k]  = mkrk[k]
 *            P22[k] = Iko[k]
 *        }
 *
 *      Compute D,K:
 *
 *        for (k=SysI.s-1; k >= 0; k--) {
 *            Pdt[k] = TRANSPOSE(Pd[k])
 *            PH1[k] = P11*Vkk + Pd*Wkk
 *            PH2[k] = Pdt*Vkk + P22*Wkk
 *
 *            DD[k] = Vkk*PH1 + Wkk*PH2
 *            if (DD[k] near 0)
 *                singular mass matrix -- abort
 *            DD[k] = 1./DD[k]
 *
 *            i = inb(k)
 *            if (i == ground) continue;
 *
 *            G1[k] = PH1*DD
 *            G2[k] = PH2*DD
 *
 *            P11[k] -= PH1 @ G1  (symmetric)
 *            Pd[k]  -= PH1 @ G2
 *            P22[k] -= PH2 @ G2  (symmetric)
 *
 *            L11,L21,L22,D11,D22[k] = LDU(P11,Pd,P22)
 *            N11[k] = Cik*L11
 *            N21[k] = rikt*L11 + Cik*L21
 *            N22[k] = Cik*L22
 *
 *            psiD11[k] = N11*D11
 *            psiD21[k] = N21*D11
 *            psiD22[k] = N22*D22
 *
 *            P11[i] += psiD11*TR(N11)                    (symmetric)
 *            Pd[i]  += psiD11*TR(N21)                   
 *            P22[i] += psiD21*TR(N21) + psiD22*TR(N22)   (symmetric)
 *        }
 *
 * DD and G may reference the temporary symbols shown above, but
 * these are all globals so later symbolic references to DD and G 
 * should work OK.  Every element of DD is assigned so that it may be
 * referenced numerically as well -- but G may be referenced
 * symbolically only.
 * The DD and G symbols are assigned.
 *
 * If we detect a singular mass matrix at generation time, we'll issue
 * an appropriate message to stderr and return 1, otherwise 0.
 *
 * This is an Order(N) calculation.
 */

void ordern_mm_decls(FILE *F)
{
}

#define DOF_PER_FILE        20

int
COMPUTE_ordern_mm(FILE         *mainF,
                  char         *rouvar,
                  int        maxaux, 
                  char        *dynname,
                  int *nxtaux)
{
    expr DDx,G1x,G2x;                                         /* output symbols */
    expr psiD11x,psiD21x,psiD22x,                         /* global temps */
         P11x,Pdx,P22x,PH1x,PH2x,N11x,N21x,N22x,
         L11x,L21x,L22x,D11x,D22x;        
    /* no sym's needed for these locals */
    expr Pdtx,L11xx,L21xx,L22xx,D11xx,D22xx,temp;                
    int err_ret = 0, i, k, naux, dofper, dofcnt;
    char str_s[10];
    FILE *F;

    esprintf(str_s,  "%d", SysI.s);

    /* Calculate how many auxiliary files to generate and when to switch
     * files.  Assume the first DOF_PER_FILE iterations generate code into the
     * current file mainF.  Then an auxiliary will be generated for each
     * DOF_PER_FILE more interations, and for the fraction left over.
     * That tells us how many aux files there will be (but don't go over
     * maxaux).  Then compute the actual number of dofs per file for all
     * but the last file.
     */
    naux = SysI.s/DOF_PER_FILE;                /* OK if SysI.s not exact multiple */
    if (naux*DOF_PER_FILE == SysI.s)        /* correct for the exact case */
        naux--;
    if (naux > maxaux)
        naux = maxaux;
    dofper = SysI.s/(naux+1);
    if (dofper*(naux+1) != SysI.s)
        dofper++;

    DDx    = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));        /* outputs */
    G1x    = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    G2x    = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    psiD11x = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));        /* global temps */
    psiD21x = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    psiD22x = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    P11x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    Pdx    = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    P22x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    PH1x   = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    PH2x   = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    L11x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    L21x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    L22x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    D11x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    D22x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    N11x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    N21x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
    N22x   = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));

    efprintf(mainF, "%{\nCompute gains (DD, G)\n%}");

    /* initialize P */
    for (k=0; k < SysI.s; k++) {
        SINDX(P11x, k, MUL(VAL1(SysI.psmk,k), MATRIX_IDENT()));
        SINDX(Pdx, k, VAL1(mkrk,k));
        SINDX(P22x, k, VAL1(Iko,k));

        /* Initialize these to 0 so we can CLEANVAR anytime we choose.
         */
        SINDX(DDx, k, SCALAR_ZERO());
        SINDX(G1x, k, VECTOR_ZERO());
        SINDX(G2x, k, VECTOR_ZERO());
    }

    ASSIGN(DD, UNUSE(DDx));
    ASSIGN(G1, UNUSE(G1x));
    ASSIGN(G2, UNUSE(G2x));

    /* compute DD, G */

    F = mainF;
    for (dofcnt=0, k=SysI.s-1; k >= 0; dofcnt++, k--) {
        if (dofcnt && ((dofcnt % dofper) == 0)) {
            /* time to switch files */
            CALL2("%Adomm%02d(%s)", *nxtaux, rouvar); /* no semicolon here */
            if (F != mainF) {
                domm_tail(F);
                CLOSE_FILE(F);
            } else 
                efprintf(mainF, "%<%<"); /* reset indentation to left edge */
            if (openaux(&F, dynname, *nxtaux)) {
                efprintf(mainF, "%>%>"); /* restore indentation level */
                return 1;
            }
            domm_head(F, *nxtaux);
            (*nxtaux)++;
        }

        Pdtx = INUSE(TRANSPOSE(INDX(Pdx,k)));

        FLUSH_VEC_NONCONST(F,PH1,k,PH1x,ADD(MATMUL(INDX(P11x,k),VAL1(Vkk,k)),
                                               MATMUL(INDX(Pdx,k), VAL1(Wkk,k))));
        FLUSH_VEC_NONCONST(F,PH2,k,PH2x,ADD(MATMUL(INDX(P22x,k),VAL1(Wkk,k)),
                                               MATMUL(     Pdtx   ,VAL1(Vkk,k))));

        DDx = INUSE(VAL(DD));
        temp = ADD(DOT(VAL1(Vkk,k),INDX(PH1x,k)),
                   DOT(VAL1(Wkk,k),INDX(PH2x,k)));
        if (IS_CONST(temp)) {
            if (NUMVAL(temp) < cEquationNegligible) {
                PRINTERR(ERR_SingularMassMatrix, 0, NULL, NULL);
                DISPOSE_EXPR(UNUSE(DDx));
                ASSIGN(DD, NULLEXPR);
                ASSIGN(G1, NULLEXPR);
                ASSIGN(G1, NULLEXPR);
                DISPOSE_EXPR(temp);
                err_ret = 1;
                goto cleanup;
            }
            temp = DVD(SCALAR_ONE(),temp);
            PRINT_ASSN1(F, PRINTNAME(DD), k, temp);
            SINDX(DDx, k, temp);
            ASSIGN(DD, UNUSE(DDx));
        } else if (IS_VREF(temp)) {
            /* Hang on to values of the form 1/vref in the hope of 
             * encouraging later simplifications.  Note that in this
             * and the next case, the runtime value of DDx will not
             * agree with the symbolic one if the "singular" condition
             * is detected.  But that's OK -- the answers will be 
             * garbage no matter what if the MM is singular.
             */
            IFCOND PRINT_E(F,temp); efprintf(F,"%s%r", LT, cEquationNegligible);
            SINDX(DDx, k, DVD(SCALAR_ONE(),temp));
            ASSIGN(DD, UNUSE(DDx));        /* not clean */
            THEN
              CALL2("%Aseterr(%s,%d)", rouvar, ERR_SingularMassMatrix);
            ELSE
              CLEANVAR(F, DD, 0, 1);        /* always executed now, but not
                                           necessarily at run time */
            ENDIF;
        } else {
            /* Anything more complicated than a lone vref is going to
             * leave us with DD just a vref to itself.  Caution:
             * must ASSIGN to DD here before VREF'ing; otherwise DVD
             * might find a RememberedVal for DD and divide by it as
             * a trial -- if the RememberedVal is zero, disaster.
             */
            SINDX(DDx, k, temp);
            ASSIGN(DD, UNUSE(DDx));
            PRINT_ASSN1(F, PRINTNAME(DD), k, VAL1(DD,k));
            temp = DVD(SCALAR_ONE(),VREF1(DD,k));
            IFCOND efprintf(F,"%s%(%@d%)%s%r", PRINTNAME(DD), k,
                            LT, cEquationNegligible);
            THEN
              SETERR(F, ROU_sdderiv, ERR_SingularMassMatrix);
            ELSE
              PRINT_ASSN1(F, PRINTNAME(DD), k, temp);
            ENDIF;
            DISPOSE_EXPR(temp);
            DDx = INUSE(VAL(DD));
            SINDX(DDx, k, VREF1(DD,k));
            ASSIGN(DD, UNUSE(DDx));                /* already clean */
        }

        i = SysI.PseudoBodies[k].jnt.InbBody;
        if (i == cGroundBody)
            goto skiprest;

        G1x = INUSE(VAL(G1));
        G2x = INUSE(VAL(G2));

        SINDX(G1x,k,MUL(VAL1(DD,k),INDX(PH1x,k)));
        ASSIGN(G1, UNUSE(G1x));
        CLEANVAR(F, G1, 0, 1);
        SINDX(G2x,k,MUL(VAL1(DD,k),INDX(PH2x,k)));
        ASSIGN(G2, UNUSE(G2x));
        CLEANVAR(F, G2, 0, 1);

        /* symmetric */
        FLUSH_MAT(F,P11,k,P11x,ZAPEXPR(SUB(INDX(P11x,k),
                                           OUTER(INDX(PH1x,k),VAL1(G1,k))),
                                       VREF1(P11,k)));
        FLUSH_MAT(F,Pd,k,Pdx,  SUB(INDX(Pdx,k),
                                   OUTER(INDX(PH1x,k),VAL1(G2,k))));
        /* symmetric */
        FLUSH_MAT(F,P22,k,P22x,ZAPEXPR(SUB(INDX(P22x,k),
                                           OUTER(INDX(PH2x,k),VAL1(G2,k))),
                                       VREF1(P22,k)));

        SDLDU(F, 1 /*zero divisor possible*/,
              INDX(P11x,k),INDX(Pdx,k),INDX(P22x,k),
              &L11xx,&L21xx,&L22xx,&D11xx,&D22xx);

        /* Can't leave any dangling references to local temps -- make
           sure these are eventually flushed NONCONST before reuse. */
        FLUSH_MAT(F,L11,k,L11x,L11xx);
        FLUSH_MAT(F,L21,k,L21x,L21xx);
        FLUSH_MAT(F,L22,k,L22x,L22xx);
        FLUSH_MAT(F,D11,k,D11x,D11xx);
        FLUSH_MAT(F,D22,k,D22x,D22xx);

        FLUSH_MAT(F,N11,k,N11x,MATMUL(VAL1(Cik,k),INDX(L11x,k)));
        FLUSH_MAT(F,N21,k,N21x,ADD(MATMUL(VAL1(rikt,k),INDX(L11x,k)),
                                   MATMUL(VAL1(Cik,k),INDX(L21x,k))));
        FLUSH_MAT(F,N22,k,N22x,MATMUL(VAL1(Cik,k),INDX(L22x,k)));

        FLUSH_MAT(F,psiD11,k,psiD11x,MATMUL(INDX(N11x,k),INDX(D11x,k)));
        FLUSH_MAT(F,psiD21,k,psiD21x,MATMUL(INDX(N21x,k),INDX(D11x,k)));
        FLUSH_MAT(F,psiD22,k,psiD22x,MATMUL(INDX(N22x,k),INDX(D22x,k)));

        /* symmetric */
        FLUSH_MAT_NONCONST(F,P11,i,P11x,
                           ZAPEXPR_NC(ADD(INDX(P11x,i),
                                          MATMUL(INDX(psiD11x,k),
                                                 TRANSPOSE(INDX(N11x,k)))),
                                         VREF1(P11,i)));
        FLUSH_MAT_NONCONST(F,Pd,i,Pdx,
                           ADD(INDX(Pdx,i),
                               MATMUL(INDX(psiD11x,k),
                                      TRANSPOSE(INDX(N21x,k)))));
        /* symmetric */
        FLUSH_MAT_NONCONST(F,P22,i,P22x,
                           ZAPEXPR_NC(ADD(INDX(P22x,i),
                                          ADD(MATMUL(INDX(psiD21x,k),
                                                     TRANSPOSE(INDX(N21x,k))),
                                              MATMUL(INDX(psiD22x,k),
                                                     TRANSPOSE(INDX(N22x,k))))),
                                      VREF1(P22,i)));

        if (sdfast_opt.magic_no && k == (SysI.s/2)) {
          IFCOND efprintf(F, "ii%s%d",NE, sdfast_opt.magic_no);
            THEN RETURN;
          ENDIF;
        }

    skiprest:
        DISPOSE_EXPR(UNUSE(Pdtx));
    }

    fflush(F);

    if (F != mainF) {
        domm_tail(F);
        CLOSE_FILE(F);
        efprintf(mainF, "%>%>"); /* restore indentation */
    }

    if (sdfast_opt.verbose)
        printf("Gains computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    cleanup:

    DISPOSE_EXPR(UNUSE(psiD11x));
    DISPOSE_EXPR(UNUSE(psiD21x));
    DISPOSE_EXPR(UNUSE(psiD22x));
    DISPOSE_EXPR(UNUSE(P11x));
    DISPOSE_EXPR(UNUSE(Pdx));
    DISPOSE_EXPR(UNUSE(P22x));
    DISPOSE_EXPR(UNUSE(PH1x));
    DISPOSE_EXPR(UNUSE(PH2x));
    DISPOSE_EXPR(UNUSE(N11x));
    DISPOSE_EXPR(UNUSE(N21x));
    DISPOSE_EXPR(UNUSE(N22x));
    DISPOSE_EXPR(UNUSE(L11x));
    DISPOSE_EXPR(UNUSE(L21x));
    DISPOSE_EXPR(UNUSE(L22x));
    DISPOSE_EXPR(UNUSE(D11x));
    DISPOSE_EXPR(UNUSE(D22x));

    return err_ret;
}

static void 
DO_DIAG(FILE *F,
        expr D, 
        expr Dval,
        sym sDINV,
        tIndex i,
        int ZeroPossible)
{
    expr DINV, Dii, DINVii;

    double limit = 1e-15; /* smallest allowable diagonal */

    DINV = INUSE(VAL(sDINV));
    SINDX2(D, i, i, Dval);
    Dii = INUSE(INDX2(D, i, i));
    if (IS_CONST(Dii) && (fabs(NUMVAL(Dii)) < limit)) {
        if (ZeroPossible)
            SINDX2(DINV, i, i, SCALAR_ZERO());
        else {
            USER_ERR();
            fprintf(stderr,
      "Near-zero divisor (D%d%d) generated - physically unrealizable system.\n",
              i, i);
            abort();
        }
    } else 
        SINDX2(DINV, i, i, DVD(SCALAR_ONE(), Dii));
    
    if (!IS_CONST(Dii)) {
        if (ZeroPossible) {
            PRINT_ASSN2(F, PRINTNAME(sDINV), i, i, SCALAR_ZERO());
            IFCOND PRINT_E(F, ABS(Dii)); efprintf(F,"%s%r", GE, limit);
            THEN
        }
        DINVii = INUSE(INDX2(DINV, i, i));
        PRINT_ASSN2(F, PRINTNAME(sDINV), i, i, DINVii);
        if (ZeroPossible)
            ENDIF;
        DISPOSE_EXPR(UNUSE(DINVii));
        SINDX2(DINV, i, i, VREF2(sDINV, i, i));
    }
    ASSIGN(sDINV, UNUSE(DINV));
    DISPOSE_EXPR(UNUSE(Dii));
}

/* SDLDU
 * Decompose symmetric, positive semi-definite matrix M into a lower 
 * triangular matrix L and a diagonal matrix D, such that M = L*D*Transpose(L).
 * M, L, and D are 2x2 matrices of 3x3 matrices.                        
 * We work with 3x3 submatrices as follows:              
 *                                                                      
 *         | M11     M12 |       | L11       0  |      | D1     0   |  
 *     M = |             |   L = |              |  D = |            |  
 *         | M21     M22 |       | L21      L22 |      |  0     D2  |  
 *                                                                      
 * Note that M21 = TRANSPOSE(M12).  Only M12 is supplied.
 * If any diagonal of M is zero (or near enough to zero) then the 
 * corresponding row and column of M must also be zero, so it is
 * fair to assume that in building the decomposition.
 *
 * We return UNUSEd single matrix expressions L11,L21,L22,D1,D2.  They may
 * contain references to temporary symbols, so make sure you flush
 * them all out (NONCONST) before calling SDLDU again.
 *                                                                      
 * The parameter ZeroPossible tells SDLDU to be careful when generating 
 * divisions, because some of the divisors may be zero. 
 * In cases where we know there ought not to be any zero divisors, we'll
 * generate better (but less conservative) code.                        
 */

void SDLDU(
      FILE *F,
      int ZeroPossible,
      expr M11,
      expr M12,
      expr M22,
      expr *L11xx,
      expr *L21xx,
      expr *L22xx,
      expr *D1xx,
      expr *D2xx)
{
    expr L11,L21,L22,D1,D2,D1INV,D2INV,L11D1,D1L21,L22D2;
    register tIndex i, j;

    L11 = MAKE_ZERO(cMatrixVal);
    L22 = MAKE_ZERO(cMatrixVal);
    for (i = 0; i < 3; i++) {
        SINDX2(L11, i, i, SCALAR_ONE());
        SINDX2(L22, i, i, SCALAR_ONE());
    }
    ASSIGN(sL11, L11);
    ASSIGN(sL22, L22);

    ASSIGN(sL21, MAKE_ZERO(cMatrixVal));
    ASSIGN(sD1, MAKE_ZERO(cMatrixVal));
    ASSIGN(sD2, MAKE_ZERO(cMatrixVal));
    ASSIGN(sD1INV, MAKE_ZERO(cMatrixVal));
    ASSIGN(sD2INV, MAKE_ZERO(cMatrixVal));

    L11D1 = NEW_MATX(cScalarVal);
    L22D2 = NEW_MATX(cScalarVal);
    D1L21 = NEW_MATX(cScalarVal);
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) {
            if (j <= i)
                SINDX2(L11D1, i, j, INDX2(M11, i, j));
            else
                SINDX2(L11D1, i, j, SCALAR_ZERO());
            SINDX2(L22D2, i, j, INDX2(M22, i, j));
            SINDX2(D1L21, i, j, INDX2(M12, i, j));
        }
    ASSIGN(sL11D1, L11D1);
    ASSIGN(sL22D2, L22D2);
    ASSIGN(sD1L21, D1L21);

    L11   = INUSE(VAL(sL11));
    L22   = INUSE(VAL(sL22));
    L21   = INUSE(VAL(sL21));
    D1    = INUSE(VAL(sD1));
    D2    = INUSE(VAL(sD2));
    L11D1 = INUSE(VAL(sL11D1));
    L22D2 = INUSE(VAL(sL22D2));
    D1L21 = INUSE(VAL(sD1L21));

    /* create D1 and L11 using L11D1 */

    /*
     * Whenever we are going to divide by a diagonal element,
     * we have to decide whether that element was trying to be
     * zero.  If so, we just have to wipe out the corresponding
     * row and column of the factorization. 
     */

    DO_DIAG(F, D1, INDX2(L11D1, 0, 0), sD1INV, 0, ZeroPossible);
    ASSIGN_CLN(F, sD1, UNUSE(D1));
    D1 = INUSE(VAL(sD1));
    D1INV = INUSE(VAL(sD1INV));

    SINDX2(L11, 1, 0, MUL(INDX2(L11D1, 1, 0), INDX2(D1INV, 0, 0)));
    SINDX2(L11, 2, 0, MUL(INDX2(L11D1, 2, 0), INDX2(D1INV, 0, 0)));
    ASSIGN_CLN(F, sL11, UNUSE(L11));
    L11 = INUSE(VAL(sL11));

    SINDX2(L11D1, 1, 1, SUB(INDX2(L11D1, 1, 1),
                            MUL(INDX2(L11D1, 1, 0), INDX2(L11, 1, 0))));
    SINDX2(L11D1, 2, 1, SUB(INDX2(L11D1, 2, 1),
                            MUL(INDX2(L11D1, 2, 0), INDX2(L11, 1, 0))));
    SINDX2(L11D1, 2, 2, SUB(INDX2(L11D1, 2, 2),
                            MUL(INDX2(L11D1, 2, 0), INDX2(L11, 2, 0))));
    ASSIGN_CLN(F, sL11D1, UNUSE(L11D1));
    L11D1 = INUSE(VAL(sL11D1));

    DO_DIAG(F, D1, INDX2(L11D1, 1, 1), sD1INV, 1, ZeroPossible);
    ASSIGN_CLN(F, sD1, UNUSE(D1));
    D1 = INUSE(VAL(sD1));
    D1INV = INUSE(VAL(sD1INV));

    SINDX2(L11, 2, 1, MUL(INDX2(L11D1, 2, 1), INDX2(D1INV, 1, 1)));
    ASSIGN_CLN(F, sL11, UNUSE(L11));
    L11 = INUSE(VAL(sL11));

    SINDX2(L11D1, 2, 2, SUB(INDX2(L11D1, 2, 2),
                            MUL(INDX2(L11D1, 2, 1), INDX2(L11, 2, 1))));
    ASSIGN_CLN(F, sL11D1, UNUSE(L11D1));
    L11D1 = INUSE(VAL(sL11D1));

    DO_DIAG(F, D1, INDX2(L11D1, 2, 2), sD1INV, 2, ZeroPossible);
    ASSIGN_CLN(F, sD1, UNUSE(D1));
    D1 = INUSE(VAL(sD1));
    D1INV = INUSE(VAL(sD1INV));

    /* create L21 using D1L21 */

    for (i = 0; i < 3; i++)
        SINDX2(D1L21, 1, i, SUB(INDX2(D1L21, 1, i),
                                MUL(INDX2(L11, 1, 0), INDX2(D1L21, 0, i))));
    ASSIGN_CLN(F, sD1L21, UNUSE(D1L21));
    D1L21 = INUSE(VAL(sD1L21));

    for (i = 0; i < 3; i++)
        SINDX2(D1L21, 2, i, SUB(INDX2(D1L21, 2, i),
                                ADD(MUL(INDX2(L11, 2, 0), INDX2(D1L21, 0, i)),
                                    MUL(INDX2(L11, 2, 1), INDX2(D1L21, 1, i)))));
    ASSIGN_CLN(F, sD1L21, UNUSE(D1L21));
    D1L21 = INUSE(VAL(sD1L21));

    ASSIGN_CLN(F, sL21, TRANSPOSE(MATMUL(D1INV, D1L21)));
    L21 = INUSE(VAL(sL21));

    /* compute D2 and L22 using L22D2 */

    L22D2 = SUB(VAL(sL22D2), MATMUL(L21, D1L21));
    SINDX2(L22D2, 0, 1, SCALAR_ZERO());
    SINDX2(L22D2, 0, 2, SCALAR_ZERO());
    SINDX2(L22D2, 1, 2, SCALAR_ZERO());
    ASSIGN_CLN(F, sL22D2, L22D2);
    L22D2 = INUSE(VAL(sL22D2));

    DO_DIAG(F, D2, INDX2(L22D2, 0, 0), sD2INV, 0, ZeroPossible);
    ASSIGN_CLN(F, sD2, UNUSE(D2));
    D2 = INUSE(VAL(sD2));
    D2INV = INUSE(VAL(sD2INV));

    SINDX2(L22, 1, 0, MUL(INDX2(L22D2, 1, 0), INDX2(D2INV, 0, 0)));
    SINDX2(L22, 2, 0, MUL(INDX2(L22D2, 2, 0), INDX2(D2INV, 0, 0)));
    ASSIGN_CLN(F, sL22, UNUSE(L22));
    L22 = INUSE(VAL(sL22));

    SINDX2(L22D2, 1, 1, SUB(INDX2(L22D2, 1, 1),
                            MUL(INDX2(L22D2, 1, 0), INDX2(L22, 1, 0))));
    SINDX2(L22D2, 2, 1, SUB(INDX2(L22D2, 2, 1),
                            MUL(INDX2(L22D2, 2, 0), INDX2(L22, 1, 0))));
    SINDX2(L22D2, 2, 2, SUB(INDX2(L22D2, 2, 2),
                            MUL(INDX2(L22D2, 2, 0), INDX2(L22, 2, 0))));
    ASSIGN_CLN(F, sL22D2, UNUSE(L22D2));
    L22D2 = INUSE(VAL(sL22D2));

    DO_DIAG(F, D2, INDX2(L22D2, 1, 1), sD2INV, 1, ZeroPossible);
    ASSIGN_CLN(F, sD2, UNUSE(D2));
    D2 = INUSE(VAL(sD2));
    D2INV = INUSE(VAL(sD2INV));

    SINDX2(L22, 2, 1, MUL(INDX2(L22D2, 2, 1), INDX2(D2INV, 1, 1)));
    ASSIGN_CLN(F, sL22, UNUSE(L22));
    L22 = INUSE(VAL(sL22));

    SINDX2(L22D2, 2, 2, SUB(INDX2(L22D2, 2, 2),
                            MUL(INDX2(L22D2, 2, 1), INDX2(L22, 2, 1))));
    ASSIGN_CLN(F, sL22D2, UNUSE(L22D2));
    L22D2 = INUSE(VAL(sL22D2));

    SINDX2(D2, 2, 2, INDX2(L22D2, 2, 2));

    DISPOSE_EXPR(UNUSE(D1INV));
    DISPOSE_EXPR(UNUSE(D2INV));
    DISPOSE_EXPR(UNUSE(L11D1));
    DISPOSE_EXPR(UNUSE(L22D2));
    DISPOSE_EXPR(UNUSE(D1L21));

    *L11xx = UNUSE(L11);
    *L21xx = UNUSE(L21);
    *L22xx = UNUSE(L22);
    *D1xx  = UNUSE(D1);
    *D2xx  = UNUSE(D2);
}
