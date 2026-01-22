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

/* Get the expression representing the ith Q or U. 
 * Q and U are indexed by degree of freedom number, but
 * Q also has 4th Euler parameters at the end.
 * (I.e., for the first ball joint, use Q[s], for the second, 
 * use Q[s+1], etc.)
 *
 * NOTE: we must use the UNNORMALIZED q's here, not the qn's used
 * in sdstate().  If we used unnormalized q's, the stabilization
 * wouldn't work since there would be no error for it to feed back.
 */
#define Q(i) (VAL1(q, i))
#define U(i) (VAL1(u, i))

/* COMPUTE_qdot
 *
 * Compute QDOT output variables. 
 */
void COMPUTE_qdot(FILE *F)
{
    register Index_t i, b, bno, loc;
    expr qdot_expr,ee_expr,stab_expr,tmpx;
    char str_flt0[10];

    esprintf(str_flt0, "%r", 0.);

    /* QDOT is a final output variable; consequently all its
     * elements must be printed out regardless of their IS_SIMPLE
     * status.
     */

    bno = 0;
    for (b = 0; b < SysI.s; b++) {
        loc = SysI.s+bno;        /* track loc of 4th Euler param for balls */
        switch(SysI.PseudoBodies[b].jnt.JointKind) {
            case cPinJoint:
            case cSlidingJoint:
                PRINT_ASSN1(F, "qdot", b, U(b));
                break;
            case cBallJoint:
                /* handle all three pseudobodies at once */
                tmpx = 
                    MUL(SC(0.5),
                        ADD(MUL(    Q(loc), U(b+0)),
                        ADD(MUL(NEG(Q(b+2)),U(b+1)),
                            MUL(    Q(b+1), U(b+2)))));
                PRINT_ASSN1(F, "qdot", b, tmpx);
                DISPOSE_EXPR(tmpx);

                tmpx = 
                    MUL(SC(0.5),
                        ADD(MUL(    Q(b+2), U(b+0)),
                        ADD(MUL(    Q(loc), U(b+1)),
                            MUL(NEG(Q(b+0)),U(b+2)))));
                PRINT_ASSN1(F, "qdot", b+1, tmpx);
                DISPOSE_EXPR(tmpx);

                tmpx = 
                    MUL(SC(0.5),
                        ADD(MUL(NEG(Q(b+1)),U(b+0)),
                        ADD(MUL(    Q(b+0), U(b+1)),
                            MUL(    Q(loc), U(b+2)))));
                PRINT_ASSN1(F, "qdot", b+2, tmpx);
                DISPOSE_EXPR(tmpx);

                tmpx = 
                    MUL(SC(-0.5),
                        ADD(MUL(    Q(b+0), U(b+0)),
                        ADD(MUL(    Q(b+1), U(b+1)),
                            MUL(    Q(b+2), U(b+2)))));
                PRINT_ASSN1(F, "qdot", loc, tmpx);
                DISPOSE_EXPR(tmpx);

                /* emit stabilization code if stabvel is non-zero */
                if (!IS_ZERO(VAL(SysI.stabvel))) {
                    if (SysI.StabVelFlg & ISQUESFLG) {
                        efprintf(F, Lang->stmt_if2_b);
                        efprintf(F, "%s", PRINTNAME(SysI.stabvel));
                        efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                    }

                    ee_expr = SCALAR_ZERO();
                    for (i=0; i<3; i++)
                        ee_expr = ADD(ee_expr,MUL(Q(b+i),Q(b+i)));
                    ee_expr = ADD(ee_expr,MUL(Q(loc),Q(loc)));
                    PRINT_ASSN(F, "ee", ee_expr, 0);
                    DISPOSE_EXPR(ee_expr);

                    stab_expr = DVD(MUL(VAL(SysI.stabvel), 
                                        SUB(SCALAR_ONE(),VREF(ee))),
                                    VREF(ee));
                    PRINT_ASSN(F, "stab", stab_expr, 0);
                    DISPOSE_EXPR(stab_expr);

                    for (i=0; i<3; i++) {
                        tmpx = 
                            ADD(VREF1(qdot,b+i),
                                MUL(Q(b+i),VREF(stab)));
                        PRINT_ASSN1(F, "qdot", b+i, tmpx);
                        DISPOSE_EXPR(tmpx);
                    }

                    tmpx = 
                        ADD(VREF1(qdot,loc), 
                            MUL(Q(loc),VREF(stab)));
                    PRINT_ASSN1(F, "qdot", loc, tmpx);
                    DISPOSE_EXPR(tmpx);

                    if (SysI.StabVelFlg & ISQUESFLG)
                        efprintf(F, Lang->stmt_if2_e);
                }

                bno++;
                b += 2;
                break;
        }
    }

    if (SysI.nq) {
        qdot_expr = INUSE(NEW_1dARRAY(cScalarVal, SysI.nq));
        for (i = 0; i < SysI.nq; i++)
            SINDX(qdot_expr, i, VREF1(qdot, i));
        ASSIGN(qdot, UNUSE(qdot_expr));
    }
}

/*=================*/
/* COMPUTE_S_AND_C */
/*=================*/

void 
COMPUTE_S_AND_C(FILE *F)
{
    /* This procedure assigns S1=SIN(Q(1)), C4=COS(Q(4)), etc. */
    register Index_t b, i;

    efprintf(F, "%{\nCompute sines and cosines of q\n%}");
    for (b = 0; b < SysI.s; b++) {
        /* compute sin & cos for pin joints */
        if (SysI.PseudoBodies[b].jnt.JointKind == cPinJoint) {
            i = b;
            if (Lang == &ADSIM_language) {        /* sin_cos is faster */
                efprintf(F, "s%@d,c%@d%=sin_cos(q%(%@d%))%;\n", i, i, i);
            } else {
                efprintf(F, "s%@d%=%Dsin(q%(%@d%))%;\n", i, i);
                efprintf(F, "c%@d%=%Dcos(q%(%@d%))%;\n", i, i);
            }
        }
    }
}


/*
 * Print out the SDQDOT() routine.  This just returns QDOT's as
 * last calculated by SDSTATE().  QDOT's are all numerically 
 * accessible so this is really easy!
 */
void 
PRINT_SDQDOT(FILE *F)
{
    char str_0[10], str_nq1[10];

    esprintf(str_0, "%@d", 0);
    esprintf(str_nq1, "%@d", SysI.nq-1);

    declare_proc(F, 0, "qdot",
      VT_USER, &SysI.type_Arr_nq, "oqdot",
      0);

    efprintf(F, "%{\n\
Return position coordinate derivatives for tree joints.\n%}");

    if (SysI.nq == 0) {
        efprintf(F, "%{\nThere are no q's in this system.\n%}");
        efprintf(F, Lang->proc_sbegin);
        efprintf(F, Lang->proc_end);
        return;
    }

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdqdot, ERR_sdstateMustBeCalledFirst);

    FOR("100", "i", str_0, str_nq1);
      SET("oqdot%(i%)","qdot%(i%)");
    ENDFOR("100");

    efprintf(F, Lang->proc_end);
}

/*
 * Print out the SDU2QDOT() routine.  Using the Q's last registered
 * with SDSTATE() and the passed-in uin's, this routine computes
 *       oqdot = W*uin
 * where W is defined by the normal qdot equation
 *       qdot = W*u
 * that is, W is an identity matrix for non-ball joints and would   
 * contain 4x3 blocks for ball joints.  We don't ever actually form
 * W.
 */

#define UIN(i) (VAL1(uin, i))

void 
PRINT_SDU2QDOT(FILE *F)
{
    register Index_t b, bno, loc;
    char str_0[10], str_s1[10];
    sym uin;
    expr tmpx;
    opstats_t opcnt;
    long lasg;

    START_COMPSUB(&opcnt);
    lasg = 0; /* locally printed operation counts */

    esprintf(str_0, "%@d", 0);
    esprintf(str_s1, "%@d", SysI.s-1);

    declare_proc(F, 0, "u2qdot",
      VT_USER|VT_DSYM, &SysI.type_Arr_s, "uin", &uin,
      VT_USER, &SysI.type_Arr_nq, "oqdot",
      0);

    efprintf(F, "%{\nConvert velocities to qdots.\n%}");

    if (SysI.nq == 0) {
        efprintf(F, "%{\nThere are no q's in this system.\n%}");
        efprintf(F, Lang->proc_sbegin);
        efprintf(F, Lang->proc_end);
        return;
    }

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdu2qdot, ERR_sdstateMustBeCalledFirst);

    /* 
     * First assume there aren't any ball joints.
     */
    FOR("100", "i", str_0, str_s1);
      SET("oqdot%(i%)","uin%(i%)");
    ENDFOR("100");
    lasg += SysI.s;

    /* 
     * Now handle each ball joint.
     */

    bno = 0;
    for (b = 0; b < SysI.s; b++) {
        loc = SysI.s+bno;        /* track loc of 4th Euler param for balls */
        if (SysI.PseudoBodies[b].jnt.JointKind == cBallJoint) {
            /* handle all three pseudobodies at once */
            tmpx = 
                MUL(SC(0.5),
                    ADD(MUL(    Q(loc), UIN(b+0)),
                    ADD(MUL(NEG(Q(b+2)),UIN(b+1)),
                        MUL(    Q(b+1), UIN(b+2)))));
            PRINT_ASSN1(F, "oqdot", b, tmpx);
            DISPOSE_EXPR(tmpx);

            tmpx = 
                MUL(SC(0.5),
                    ADD(MUL(    Q(b+2), UIN(b+0)),
                    ADD(MUL(    Q(loc), UIN(b+1)),
                        MUL(NEG(Q(b+0)),UIN(b+2)))));
            PRINT_ASSN1(F, "oqdot", b+1, tmpx);
            DISPOSE_EXPR(tmpx);

            tmpx = 
                MUL(SC(0.5),
                    ADD(MUL(NEG(Q(b+1)),UIN(b+0)),
                    ADD(MUL(    Q(b+0), UIN(b+1)),
                        MUL(    Q(loc), UIN(b+2)))));
            PRINT_ASSN1(F, "oqdot", b+2, tmpx);
            DISPOSE_EXPR(tmpx);

            tmpx = 
                MUL(SC(-0.5),
                    ADD(MUL(    Q(b+0), UIN(b+0)),
                    ADD(MUL(    Q(b+1), UIN(b+1)),
                        MUL(    Q(b+2), UIN(b+2)))));
            PRINT_ASSN1(F, "oqdot", loc, tmpx);
            DISPOSE_EXPR(tmpx);

            bno++;
            b += 2;
        }
    }

    END_COMPSUB(F,&opcnt,0L,0L,0L,lasg);
    efprintf(F, Lang->proc_end);
}
