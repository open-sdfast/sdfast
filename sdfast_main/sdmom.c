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
 * Code to generate the auxiliary routines:
 *   sdmom         sdsys
 * 
 * These routines are generated using data expected already to have been 
 * produced during generation of the SDSTATE routine.  At runtime, they refer
 * to globals (or common) loaded by SDSTATE.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"

/*
 * Compute linear and angular momentum, and kinetic energy.
 * These computations refer to variables computed in SDINIT and SDNSIM.
 * All symbols created here are used only here; hence they are just
 * local variables.
 *
 *    lk(b) = mk(b)*vnb(b)
 *    hnk(k) = ik(b)*wb(b)
 *    lm = sum[lk(b)]
 *    am = sum[(rnb(b)-com) X lk(b) + hnk(b)*cnbt(b)]
 *
 *    ke = .5 * sum[vnb(b)*lk(b) + wb(b)*hnk(b)]
 *
 *    (cnbt is transpose(cnb))
 *
 * WARNING: Don't do angular momentum like this, as was done
 * in SD/FAST through version B.2.8:
 *    am = sum[rnb(b) X lk(b) + hnk(b)*cnbt(b)] - com X lm
 * This causes numerical errors in spacecraft problems in
 * which the ground origin is a long way from the spacecraft.
 * Thanks to Carlos Roithmayr at NASA Johnson (later NASA
 * Goddard) for discovering this and suggesting the above
 * fix.  The fixed version also appears to generate less
 * code, because there can be common terms in com and the
 * various rnb which cancel.  (sherm 980810)
 *
 *
 * Temporary symbols created and used here:
 *   lk, hnk
 *
 * Global (common) symbols referenced here:
 *   sdginput: mk,ik
 *   sdgstate: rnb,cnb,vnb,wb,com
 *
 * This is an Order(N) computation.
 */
void PRINT_SDMOM(FILE *F,
            opstats_t *opcnt)
{
    register Index_t i, b;
    sym lm,am,ke,lk,hnk;
    expr lm_expr, am_expr, ke_expr, lk_expr, hnk_expr;
    expr temp;

    START_COMPSUB(opcnt);

    declare_proc(F, 0, "mom", 
      VT_USER|VT_DSYM, &SysI.type_Vec,  "lm", &lm,
      VT_DUP|VT_DSYM,                         "am", &am,
      VT_REAL|VT_BYREF|VT_DSYM,                "ke", &ke,
      0);
    efprintf(F, "%{\n\
Compute system linear and angular momentum, and kinetic energy.\n\n");
    PRINT_SUBR_STAMP(F);
    efprintf(F, "%}");

    lk_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    hnk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);
    declare_vars(F, 0,
      VT_USER|VT_DSYM, &SysI.type_Vec_n,  "lk", &lk,
      VT_DUP|VT_DSYM,                           "hnk", &hnk,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdmom, ERR_sdstateMustBeCalledFirst);

    /* compute lk */
    for (b = 0; b < SysI.n; b++)
        SINDX(lk_expr, b, MUL(VAL1(SysI.mk,b),VAL1(vnb,b)));
    ASSIGN_CLN(F, lk, lk_expr);

    /* compute hnk */
    for (b = 0; b < SysI.n; b++)
        SINDX(hnk_expr, b, MATMUL(VAL1(SysI.ik,b),VAL1(wb,b)));
    ASSIGN_CLN(F, hnk, hnk_expr);

    /* compute lm */
    lm_expr = VECTOR_ZERO();
    for (b = 0; b < SysI.n; b++)
        lm_expr = ADD(lm_expr, VAL1(lk,b));
    lm_expr = INUSE(lm_expr);
    for (i = 0; i < 3; i++) {
        temp = INDX(lm_expr,i);
        PRINT_ASSN1(F, "lm", i, temp);
        if (!IS_SIMPLE(temp)) 
            SINDX(lm_expr, i, VREF1(lm, i));
    }
    ASSIGN(lm, UNUSE(lm_expr));

    /* compute am */
    am_expr = VECTOR_ZERO();
    for (b = 0; b < SysI.n; b++)
        am_expr = ADD(am_expr, 
                      ADD(CROSS(SUB(VAL1(rnb,b),VAL(com)),VAL1(lk,b)),
                             MATMUL(VAL1(hnk,b),TRANSPOSE(VAL1(cnb,b)))));
    am_expr = INUSE(am_expr);
    for (i = 0; i < 3; i++) {
        temp = INDX(am_expr,i);
        PRINT_ASSN1(F, "am", i, temp);
        if (!IS_SIMPLE(temp)) 
            SINDX(am_expr, i, VREF1(am, i));
    }
    ASSIGN(am, UNUSE(am_expr));

    /* compute ke */
    ke_expr = SCALAR_ZERO();
    for (b = 0; b < SysI.n; b++)
        ke_expr = ADD(ke_expr,
                      ADD(DOT(VAL1(vnb,b),VAL1(lk,b)),
                          DOT(VAL1(wb,b),VAL1(hnk,b))));
    ke_expr = MUL(SC(0.5),ke_expr);
    PRINT_ASSN(F, "ke", ke_expr, 1 /*byref*/);
    if (!IS_SIMPLE(ke_expr)) 
        ke_expr = VREF(ke);
    ASSIGN(ke, ke_expr);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);

    /* free up the space in use by the local symbols */
    ASSIGN(lm, NULLEXPR);
    ASSIGN(am, NULLEXPR);
    ASSIGN(ke, NULLEXPR);
    ASSIGN(lk, NULLEXPR);
    ASSIGN(hnk, NULLEXPR);

    if (sdfast_opt.verbose)
        printf("System momentum and energy computed. (CPU: %g MEM: %ld)\n",
          CPU_SECONDS() - gStartTime, BYTES_USED());
}

/*
 * Compute system total mass, instantaneous center of mass, and
 * instantaneous inertia matrix about the center of mass.  
 *
 * These computations refer to variables computed in SDINIT and SDSTATE.
 * All symbols created here are used only here; hence they are just
 * local variables.
 *
 *    mtoto = mtot
 *    cm    = com
 *
 *    ikcnkt(k) = ik(k)*cnkt(k)
 *    icm = sum[cnk(k)*ikcnkt(k) 
 *                + mk(k)*(rnk(k)*rnk(k)*E - OUTER(rnk(k),rnk(k)))]
 *            - mtot*(com*com*E - OUTER(com,com))
 *
 *    cnkt is transpose(cnk)
 *    E is 3x3 identity matrix
 *
 * Because icm is symmetric and because the expressions as written
 * above produce some redundant output from sd/calc, we implement
 * the above expression at a somewhat lower level, using the fact that
 *
 *    r*r*E - OUTER(r,r) = [ r2**2+r3**2     -r1r2          -r1r3    ]
 *                         [    -r1r2     r1**2+r3**2       -r2r3    ]
 *                         [    -r1r3        -r2r3       r1**2+r2**2 ]
 *
 * Note the symmetry.
 *
 * Temporary symbols created and used here:
 *   cm,icm,ikcnkt
 *
 * Global (common) symbols referenced here:
 *   sdginput: mk,ik,mtot
 *   sdgstate: rnk,cnk,com
 *
 * Notes:
 *   - Although rnk and cnk are indexed by pseudobody number, only those
 *     elements corresponding to actual user bodies have been filled into
 *     sdgstate.  Be careful not to reference any non-real elements of these
 *     variables.
 *
 * This is an Order(N) computation.
 */

/*
 * Routine to compute mat = r*r*E - OUTER(r,r) as described above.
 * `r' should be an INUSE vector-valued expression.
 * `mat' should be an already-allocated, INUSE matrix expression.
 */
void make_mat(expr r,
         expr mat)
{
    register Index_t i,j,k;
    expr     temp;

    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) {
            if (i==j) {
                temp = SCALAR_ZERO();
                for (k = 0; k < 3; k++)
                    if (k != i /*and != j*/) 
                        temp = ADD(temp,MUL(INDX(r,k),INDX(r,k)));
                SINDX2(mat, i, i, temp);
            } else if (i < j) {
                SINDX2(mat, i, j, NEG(MUL(INDX(r,i),INDX(r,j))));
            } else /* i > j, same as (j,i) element */
                SINDX2(mat, i, j, NEG(MUL(INDX(r,j),INDX(r,i))));
        }
}

void PRINT_SDSYS(FILE *F,
            opstats_t *opcnt)
{
    register Index_t i, j, k, p;
    sym icm,ikcnkt;
    expr icm_expr, ikcnkt_expr;
    expr temp,r,mat;

    START_COMPSUB(opcnt);

    declare_proc(F, 0, "sys", 
      VT_REAL|VT_BYREF,                  "mtoto", 
      VT_VECTOR,                     "cm", 
      VT_MATRIX|VT_DSYM,           "icm",  &icm,
      0);
    efprintf(F, "%{\n\
Compute system total mass, and instantaneous center of mass and\n\
inertia matrix.\n\n");
    PRINT_SUBR_STAMP(F);
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);
    declare_vars(F, 0,
      VT_USER|VT_DSYM, &SysI.type_Mat_s,  "ikcnkt", &ikcnkt,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdsys, ERR_sdstateMustBeCalledFirst);

    PRINT_ASSN(F, "mtoto", VAL(mtot), 1 /*byref*/);

    /* print out system COM */
    for (i = 0; i < 3; i++)
        PRINT_ASSN1(F, "cm", i, VAL1(com,i));

    if (SysI.s) {
        /* compute ikcnkt */
        ikcnkt_expr  = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));
        for (p = 0; p < SysI.s; p++) {
            if (SysI.PseudoBodies[p].realbody) 
                SINDX(ikcnkt_expr, p, MATMUL(VAL1(SysI.psik,p),
                                             TRANSPOSE(VAL1(cnk,p))));
            else SINDX(ikcnkt_expr, p, MATRIX_ZERO());
        }
        ASSIGN_CLN(F, ikcnkt, UNUSE(ikcnkt_expr));
    }

    /* Compute icm.  Initialize with the inertia of the ground
     * pseudobody, shifted from the pseudobody com to the ground origin 
     * (it's already in the ground frame).   This will just be zero unless 
     * there are bodies welded to ground.
     */
    mat = INUSE(NEW_MATX(cScalarVal));
    r = INUSE(VAL(SysI.psrcomg));
    make_mat(r,mat);
    DISPOSE_EXPR(UNUSE(r));
    mat = UNUSE(mat);
    icm_expr = ADD(VAL(SysI.psikg), 
                   MUL(VAL(SysI.psmkg),mat));
    for (p = 0; p < SysI.s; p++) {
        if (!SysI.PseudoBodies[p].realbody)
            continue;
        mat = INUSE(NEW_MATX(cScalarVal));
        r = INUSE(VAL1(rnk,p));
        make_mat(r,mat);
        DISPOSE_EXPR(UNUSE(r));
        mat = UNUSE(mat);
        icm_expr = ADD(icm_expr,
                       ADD(MATMUL(VAL1(cnk,p), VAL1(ikcnkt,p)),
                           MUL(VAL1(SysI.psmk,p),mat)));
    }
    mat = INUSE(NEW_MATX(cScalarVal));
    r = INUSE(VAL(com));
    make_mat(r,mat);
    DISPOSE_EXPR(UNUSE(r));
    mat = UNUSE(mat);
    icm_expr = SUB(icm_expr, MUL(VAL(mtot),mat));

    icm_expr = INUSE(icm_expr);
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) 
            if (i <= j)
                PRINT_ASSN2(F, "icm", i, j, INDX2(icm_expr,i,j));
            else /* i > j */ {
                temp = VREF2(icm, j, i);
                PRINT_ASSN2(F, "icm", i, j, temp);
                DISPOSE_EXPR(temp);
            }
    /* no point in assigning icm_expr to the symbol -- just chuck it now */
    DISPOSE_EXPR(UNUSE(icm_expr));

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);

    /* free up the space in use by ikcnkt */
    ASSIGN(ikcnkt, NULLEXPR);

    if (sdfast_opt.verbose)
        printf("System mass properties computed. (CPU: %g MEM: %ld)\n",
          CPU_SECONDS() - gStartTime, BYTES_USED());
}
