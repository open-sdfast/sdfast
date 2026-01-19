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

/* COMPUTE_JOINT_CONSTS
 *
 * Compute the symbols which capture as constants the meaning of the joints,
 * so that the remainder of the equations can be couched in joint-independent
 * terms.  These are Wkk, Vkk.  dik and rkWkk are convenient values for
 * later use.
 *
 * These are all evaluated in sdinit().
 */
void COMPUTE_JOINT_CONSTS(FILE *F)
{
    if (SysI.s == 0)
        return;

    COMPUTE_Wkk(F);
    COMPUTE_Vkk(F);
    COMPUTE_rkWkk(F);
    COMPUTE_dik(F);
}

/* COMPUTE_Wkk
 *
 * Compute vectors along which joint rotation occurs into global Wkk.
 * This is zero for sliders, b1, b2, and b3 for balls, and the
 * pin axis vector for pins.
 *
 * This is an Order(N) computation, and futhermore it is a constant
 * so it can be computed in sdinit().
 */
void COMPUTE_Wkk(FILE *F)
{
    expr e, Wkk_expr;
    register Index_t i,w;

    Wkk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (i = 0; i < SysI.s; i++) {
        switch (SysI.PseudoBodies[i].jnt.JointKind) {
            case cPinJoint:
                e = VAL1(SysI.pin,i);
                break;
            case cBallJoint:
                w = SysI.PseudoBodies[i].jnt.whichaxis;
                e = (w == 0 ? B1() : (w == 1 ? B2() : B3()));
                break;
            case cSlidingJoint:
                e = VECTOR_ZERO();
                break;
        }
        SINDX(Wkk_expr, i, e);
    }
    ASSIGN_CLN(F, Wkk, UNUSE(Wkk_expr));
}

/* COMPUTE_Vkk
 *
 * Compute vectors along which joint translation occurs into global Vkk.
 * This is zero for rotational joints, and the sliding axis vector
 * for sliders.
 *
 * This is an Order(N) computation, and futhermore it is a constant
 * so it can be computed in sdinit().
 */
void COMPUTE_Vkk(FILE *F)
{
    expr e, Vkk_expr;
    register Index_t i;

    Vkk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (i = 0; i < SysI.s; i++) {
        switch (SysI.PseudoBodies[i].jnt.JointKind) {
            case cPinJoint:
            case cBallJoint:
                e = VECTOR_ZERO();
                break;
            case cSlidingJoint:
                e = VAL1(SysI.pin,i);
                break;
        }
        SINDX(Vkk_expr, i, e);
    }
    ASSIGN_CLN(F, Vkk, UNUSE(Vkk_expr));
}

/* COMPUTE_rkWkk
 *
 * Compute rkWkk = rk X Wkk.
 *
 * This is an Order(N) computation, and futhermore it is a constant
 * so it can be computed in sdinit().
 */
void COMPUTE_rkWkk(FILE *F)
{
    expr rkWkk_expr;
    register Index_t k;

    rkWkk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rkWkk_expr, k, CROSS(VAL1(SysI.psrk,k), VAL1(Wkk,k)));

    ASSIGN_CLN(F, rkWkk, UNUSE(rkWkk_expr));
}

/* COMPUTE_dik
 *
 * Compute dik[k] = ri[k],             i=inb(k) == ground
 *                = ri[k] - rk[i],     i        != ground
 *
 * This is an Order(N) computation, and futhermore it is a constant
 * so it can be computed in sdinit().
 */
void COMPUTE_dik(FILE *F)
{
    expr dik_expr;
    register Index_t i,k;

    dik_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++) {
        i = SysI.PseudoBodies[k].jnt.InbBody;
        if (i == cGroundBody)
            SINDX(dik_expr, k, VAL1(SysI.psri,k));
        else
            SINDX(dik_expr, k, SUB(VAL1(SysI.psri,k),VAL1(SysI.psrk,i)));
    }

    ASSIGN_CLN(F, dik, UNUSE(dik_expr));
}

/* COMPUTE_Cik
 *                           i k
 * Compute direction cosines  C  giving the orientation of pseudobody
 * k w.r.t. its inboard pseudobody i.  Put these into global var Cik.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_Cik(FILE *F)
{
    expr e, Cik_expr;
    register Index_t i,ballnum;

    if (SysI.s == 0)
        return;

    Cik_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));

    ballnum = 0;
    for (i = 0; i < SysI.s; i++) {
        switch (SysI.PseudoBodies[i].jnt.JointKind) {
            case cPinJoint:
                e = PIN_COS(i, VAL1(SysI.pin, i));
                break;
            case cBallJoint:
                if (SysI.PseudoBodies[i].jnt.whichaxis == 0)
                    e = BALL_COS(i,ballnum++);
                else
                    e = MATRIX_IDENT();
                break;
            case cSlidingJoint:
                e = MATRIX_IDENT();
                break;
        }
        SINDX(Cik_expr, i, e);
    }
    ASSIGN_CLN(F, Cik, UNUSE(Cik_expr));
}

/* COMPUTE_Cib
 *                           i b
 * Compute direction cosines  C  giving the orientation of real body b
 * w.r.t. its inboard real body i.  Put these into global var Cib.
 *
 * This is an Order(N) computation, and depends on previous calculation
 * of Cik.
 */
void COMPUTE_Cib(FILE *F)
{
    int b, k;
    expr Cib_expr, tmpx;

    Cib_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.n));

    for (b = 0; b < SysI.n; b++) {
        tmpx = MATRIX_IDENT();
        for (k=SysI.FirstDOF[b]; k <= SysI.LastDOF[b]; k++)
            tmpx = MATMUL(tmpx, VAL1(Cik, k));
        SINDX(Cib_expr, b, tmpx);
    }

    ASSIGN_CLN(F, Cib, UNUSE(Cib_expr));
}
