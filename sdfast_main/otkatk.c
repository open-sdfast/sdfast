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

/* COMPUTE_generic_Otk
 *
 * (This is a symbolic subroutine used for generating different kinds of Otk's
 * in different places.)
 *
 * Inertial angular accelerations.  This computation cheats for ball
 * joints by looking at the joint type.  The formula is  Otk(k) = prev + cross,
 * where prev is Otk(inb(k)) expressed in k's frame (0 for the first body), 
 * and cross is a cross product term whose value depends on the type
 * of pseudobody we're dealing with, and the specific axis for ball joints.
 *
 *                        prev                           cross
 *
 * pin or slider     Otk(inb(k))*Cik(k)              wk(k) X Wik(k)
 * ball axis 0,1      or 0 if inb(k)==ground               0
 * ball axis 2                               wk(k) X (Wik(k)+Wik(k-1)+Wik(k-2))
 *                                               {opt: + udot[k]*Wkk[k]}
 *
 * If udots is true, meaning we're computing this Otk for a computation in
 * which the effects of udots should be included,
 * we add the term udot[k]*Wkk[k] to the "cross" term above.  The passed-in
 * udot symbol (udotsym) is not referenced unless udots is true.
 *
 * The final value is ASSIGN'ed to the passed-in Otk symbol, which will
 * contain only constants and self-VREFs.
 *
 * This is an Order(N) calculation.
 */
void COMPUTE_generic_Otk(FILE *F,
                    int udots,
                    sym udotsym,
                    sym  Otksym)
{
    register Index_t k,inb;
    expr prev, cross, Otk_expr;

    if (SysI.s == 0)
        return;

    Otk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;

        /* prev */
        if (inb == cGroundBody) 
              prev = VECTOR_ZERO();
        else  prev = MATMUL(INDX(Otk_expr,inb),VAL1(Cik,k));

        /* cross */
        if (SysI.PseudoBodies[k].jnt.JointKind == cBallJoint) {
            if (SysI.PseudoBodies[k].jnt.whichaxis < 2) 
                cross = VECTOR_ZERO();
            else
                cross = CROSS(VAL1(wk,k),
                              ADD(VAL1(Wik,k),
                                  ADD(VAL1(Wik,k-1),VAL1(Wik,k-2))));
        } else
            cross = CROSS(VAL1(wk,k),VAL1(Wik,k));  

        if (udots)
            cross = ADD(cross, MUL(VAL1(udotsym,k), VAL1(Wkk,k)));
        
        FLUSH_VEC(F,Otksym,k,Otk_expr, ADD(prev,cross));
    }

    ASSIGN(Otksym, UNUSE(Otk_expr)); /* already clean */
}


/* COMPUTE_Atk
 *
 * (This is a symbolic subroutine used for generating different kinds of Atk's
 * in different places.)
 *
 * Inertial linear accelerations.
 *
 * Generate s vectors in Atk.  To avoid redundant compuations, uses three 
 * temporary arrays also of s vectors:
 *
 *   Wirk[k]  = [0,0,0],       inb(k)==ground
 *   Wirk[k]  = wk[i] X ri[k], inb(k)!=ground    (Computed by sdstate())
 *
 *   Wkrpk[k] = wk[k] X rpk[k]                     (Computed by sdstate())
 *
 *   AiOiWi[k] = [0,0,0], inb(k)==ground
 *   AiOiWi[k]= Atk[i] + (Otk[i] X ri[k]) + (wk[i] X Wirk[k])
 *
 *   Atk[k] = AiOiWk[k]*Cik[k] + 2*(wk[k] X Vik[k])
 *            + Otk[k] X rpk[k] + wk[k] X Wkrpk[k]     {opt: + udot[k]*Vkk[k]}
 *
 * If udots is true, meaning we're computing this Atk for a computation in
 * which the effects of udots should be included,
 * we add the term udot[k]*Vkk[k] to the final Atk expression above.  
 * The passed-in udot symbol (udotsym) is not referenced unless udots is true.
 *
 * The final value is ASSIGN'ed to the passed-in Atk symbol, which will
 * contain only constants and self-VREFs.
 *
 * This is an Order(N) calculation.
 */
void COMPUTE_generic_Atk(FILE *F,
                    int udots,
                    sym udotsym,
                    sym  Otksym,
                    sym  AiOiWisym,
                    sym  Atksym)
{
    register Index_t k,inb;
    expr temp, Atk_expr, AiOiWi_expr;

    if (SysI.s == 0)
        return;

    Atk_expr   = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    AiOiWi_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    /* `clean' as we go */
    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;

        /* compute AiOiWi(k) */
        temp = (inb == cGroundBody ? 
                VECTOR_ZERO()
                : ADD(INDX(Atk_expr,inb),
                      ADD(CROSS(VAL1(Otksym,inb),VAL1(SysI.psri,k)),
                          CROSS(VAL1(wk,inb),VAL1(Wirk,k)))));
        FLUSH_VEC(F,AiOiWisym,k,AiOiWi_expr, temp);

        /* compute Atk(k) */
        temp = ADD(inb == cGroundBody ? 
                   VECTOR_ZERO()
                   : MATMUL(INDX(AiOiWi_expr,k),VAL1(Cik,k)),
                   ADD(MUL(SC(2.0),CROSS(VAL1(wk,k),VAL1(Vik,k))),
                       ADD(CROSS(VAL1(Otksym,k),VAL1(rpk,k)),
                           CROSS(VAL1(wk,k),VAL1(Wkrpk,k)))));
            
        if (udots)
            temp = ADD(temp, MUL(VAL1(udotsym,k), VAL1(Vkk,k)));

        FLUSH_VEC(F,Atksym,k,Atk_expr, temp);
    }

    ASSIGN(Atksym, UNUSE(Atk_expr));                /* already clean */
    ASSIGN(AiOiWisym, UNUSE(AiOiWi_expr));
}

void COMPUTE_Otk(FILE *F)
{
    COMPUTE_generic_Otk(F, 0, (sym)0, Otk);
}

void COMPUTE_Atk(FILE *F)
{
    COMPUTE_generic_Atk(F, 0, (sym)0, Otk, AiOiWi, Atk);
}
