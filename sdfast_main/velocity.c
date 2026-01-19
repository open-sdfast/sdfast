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

/* COMPUTE_Wpk
 *
 * Computes values for the global symbol Wpk, the partial angular
 * velocities.  Wpk is an sXs upper triangular matrix.
 *
 * Warning: the lower triangle of Wpk is not computed at all.  Don't
 * reference it!
 *
 * Note: we output all elements which are non-zero
 * to allow use of Wpk in SDREL2CART().  The zeroes were all set in
 * SDINIT().  All IS_SIMPLE elements are retained in Wpk, however,
 * to help simplifications later.
 *
 * This is an Order(N**2) computation.
 */
void COMPUTE_Wpk(FILE *F)
{
    register Index_t p, k, inb, i;
    expr temp, Wpk_expr;

    if (SysI.s == 0)
        return;

    Wpk_expr = INUSE(NEW_2dARRAY(cVectorVal, SysI.s, SysI.s));

    /* We'll `clean' as we go since later Wpk's are expressed in terms
     * of earlier ones.  
     */
    for (p = 0; p < SysI.s; p++) {

        /* process the diagonal element */
        temp = INUSE(VAL1(Wkk,p));
        for (i = 0; i < 3; i++)
            if (!IS_ZERO(INDX(temp,i)))
                PRINT_ASSN3(F, "Wpk", p, p, i, INDX(temp, i));
        temp = UNUSE(temp);
        /* USEXIF will dispose parts of temp that aren't SIMPLE. */
        SINDX2(Wpk_expr, p, p, USEXIF(temp, VREF2(Wpk, p, p)));

        /* process the rest of this row to the right of the diagonal */
        for (k = p+1; k < SysI.s; k++) {
            inb = SysI.PseudoBodies[k].jnt.InbBody;
            if (inb >= p)
                temp = INUSE(MATMUL(INDX2(Wpk_expr, p, inb), VAL1(Cik, k)));
            else temp = VECTOR_ZERO();
            for (i = 0; i < 3; i++)
                if (!IS_ZERO(INDX(temp,i)))
                    PRINT_ASSN3(F, "Wpk", p, k, i, INDX(temp, i));
            temp = UNUSE(temp);
            SINDX2(Wpk_expr, p, k, USEXIF(temp, VREF2(Wpk, p, k)));
        }
    }

    ASSIGN(Wpk, UNUSE(Wpk_expr));
}

/* COMPUTE_Vpk
 *
 * Computes values for the global symbol Vpk, the partial 
 * velocities.  Vpk is an sXs upper triangular matrix.
 *
 * We use an auxiliary variable VWri=Vpk[p,i]+Wpk[p,i]Xri[j] to
 * avoid redundant multiplies.  VWri is also sXs upper triangular, 
 * although there is nothing on the diagonal.
 *
 * Warning: the lower triangle of Vpk is not computed at all.  Don't
 * reference it!
 *
 * Note: we output all elements which are non-zero
 * to allow use of Vpk in SDREL2CART().  The zeroes were all set in
 * SDINIT().  All IS_SIMPLE elements are retained in Vpk, however,
 * to help simplifications later.
 *
 * This is an Order(N**2) computation.
 */
void COMPUTE_Vpk(FILE *F)
{
    register Index_t p, k, inb, i;
    expr temp, Vpk_expr,VWri_expr;

    if (SysI.s == 0)
        return;

    Vpk_expr = INUSE(NEW_2dARRAY(cVectorVal, SysI.s, SysI.s));
    VWri_expr = INUSE(NEW_2dARRAY(cVectorVal, SysI.s, SysI.s));

    /* We'll `clean' as we go since later Vpk's are expressed in terms
     * of earlier ones.
     */
    for (p = 0; p < SysI.s; p++) {
        /* process the diagonal element */
        temp = INUSE(ADD(VAL1(Vkk,p),
                         CROSS(VAL2(Wpk,p,p),VAL1(rpk,p))));
        for (i = 0; i < 3; i++)
            if (!IS_ZERO(INDX(temp,i)))
                PRINT_ASSN3(F, "Vpk", p, p, i, INDX(temp, i));
        temp = UNUSE(temp);
        /* USEXIF will dispose parts of temp that aren't SIMPLE. */
        SINDX2(Vpk_expr, p, p, USEXIF(temp, VREF2(Vpk, p, p)));

        /* process the rest of this row to the right of the diagonal */
        for (k = p+1; k < SysI.s; k++) {
            inb = SysI.PseudoBodies[k].jnt.InbBody;

            /* first the temporary VWri */
            if (inb >= p)
                temp = INUSE(ADD(INDX2(Vpk_expr,p,inb),
                                 CROSS(VAL2(Wpk,p,inb),VAL1(SysI.psri,k))));
            else temp = VECTOR_ZERO();
            for (i = 0; i < 3; i++)
                if (!IS_SIMPLE(INDX(temp,i)))
                    PRINT_ASSN3(F, "VWri", p, k, i, INDX(temp, i));
            temp = UNUSE(temp);
            SINDX2(VWri_expr, p, k, USEXIF(temp, VREF2(VWri, p, k)));

            /* now the Vpk */
            temp = INUSE(ADD(MATMUL(INDX2(VWri_expr,p,k),VAL1(Cik,k)),
                             CROSS(VAL2(Wpk,p,k),VAL1(rpk,k))));
            for (i = 0; i < 3; i++)
                if (!IS_ZERO(INDX(temp,i)))
                    PRINT_ASSN3(F, "Vpk", p, k, i, INDX(temp, i));
            temp = UNUSE(temp);
            SINDX2(Vpk_expr, p, k, USEXIF(temp, VREF2(Vpk, p, k)));
        }
    }

    ASSIGN(VWri, UNUSE(VWri_expr));
    ASSIGN(Vpk, UNUSE(Vpk_expr));
}

/* COMPUTE_ping
 *
 * Expresses each pseudobody's inboard pin in the ground frame.  This
 * is needed for computing the SDREL2CART() function when there are
 * no Vpk's and Wpk's available (i.e., in the Order(N) Formulation).
 *
 * Note: we output all elements which are non-zero
 * to allow use of ping in SDREL2CART().  The zeroes were all set in
 * SDINIT().   The symbol is not assigned since there is no subsequent
 * symbolic use of ping.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_ping(FILE *F)
{
    Index_t k, i;
    expr temp;

    for (k=0; k<SysI.s; k++) {
        switch (SysI.PseudoBodies[k].jnt.JointKind) {
            case cPinJoint:
            case cBallJoint:
                temp = INUSE(MATMUL(VAL1(Wkk,k),TRANSPOSE(VAL1(cnk,k))));
                break;
            case cSlidingJoint:
                temp = INUSE(MATMUL(VAL1(Vkk,k),TRANSPOSE(VAL1(cnk,k))));
                break;
        }
        for (i = 0; i < 3; i++)
            if (!IS_ZERO(INDX(temp,i)))
                PRINT_ASSN2(F, PRINTNAME(ping), k, i, INDX(temp, i));
        DISPOSE_EXPR(UNUSE(temp));
    }
}

/* COMPUTE_hngpt
 *
 * Expresses each pseudobody's inboard hinge point in the ground frame.  This
 * is needed for computing the SDREL2CART() function when there are
 * no Vpk's and Wpk's available (i.e., in the Order(N) Formulation).
 *
 * Note: we output all elements which are non-zero
 * to allow use of ping in SDREL2CART().  The zeroes were all set in
 * SDINIT().   The symbol is not assigned since there is no subsequent
 * symbolic use of hngpt.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_hngpt(FILE *F)
{
    register Index_t k, i;
    expr temp;

    for (k=0; k<SysI.s; k++) {
        temp = INUSE(ADD(VAL1(rnk,k),MATMUL(VAL1(SysI.psrk,k),
                                            TRANSPOSE(VAL1(cnk,k)))));
        for (i = 0; i < 3; i++)
            if (!IS_ZERO(INDX(temp,i)))
                PRINT_ASSN2(F, PRINTNAME(hngpt), k, i, INDX(temp, i));
        DISPOSE_EXPR(UNUSE(temp));
    }
}
