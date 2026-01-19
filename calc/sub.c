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

#include "calc.h"
#include "calcprot.h"

/*=====*/
/* SUB */
/*=====*/

pExpr DO_SUB(register pExpr E1,
             register pExpr E2)
{
    /* This is the function that actually does a SUB. */
    register pExpr E;
    register tIndex I;

    C_ASSERT(E1 && E2, 1, "SUB");
    if (!SAME_TYPE(&E1->NodeValueType, &E2->NodeValueType))
        fatal("SUB: expressions incompatible.");
    if (SAME_EXPR(E1, E2)) {
        E = IS_SCALAR(E1) /* and E2 */ ? gScalarZero : MAKE_ZERO_LIKE(E1);
        E1 = INUSE(E1);        /* protect E1 while we dispose E2 */
        DISPOSE_EXPR(E2);
        DISPOSE_EXPR(UNUSE(E1));
        return E;
    } else if (IS_ZERO(E1)) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(NEG(E2));
    } else if (IS_ZERO(E2)) {
        E1 = INUSE(E1);
        DISPOSE_EXPR(E2);
        return UNUSE(E1);
    } else if (IS_CONST(E1) && IS_CONST(E2)) {        /* we'll do a real subtract! */
        switch (E1->NodeKind) {
            case cScalarConstNode:
                /* Subtract the scalars.  If we come out near zero, we'll
                 * pretend we actually got a zero.
                 */
                E = SC(E1->ScalarValue - E2->ScalarValue);
                if (IS_ZERO(E)) {
                    DISPOSE_EXPR(E);
                    E = gScalarZero;
                }
                DISPOSE_EXPR(E1);
                DISPOSE_EXPR(E2);
                return E;
        }        /* case */
    } else if (IS_NEG(E2)) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(ADD(E1, NEG(E2)));
    } else if (IS_NEG(E1)) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(NEG(ADD(NEG(E1), E2)));
    } else if (E1->NodeValueType.ValueType != cScalarVal) {
        E = INUSE(MAKE_EXPR_LIKE(E1));        /* E has same dimensions as E1 */
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        for (I = 0; I < LEN1d(E); I++)
            SINDX(E, I, SUB(INDX(E1, I), INDX(E2, I)));
    } else {        /* can't perform any subtraction */
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        /* try factoring out a constant */
        if (SAME_EXPR(OTHER_PART(E1), OTHER_PART(E2)))
            E = INUSE(MUL(SUB(CONST_PART(E1), CONST_PART(E2)), OTHER_PART(E1)));
        else if (SAME_EXPR(CONST_PART(E1), CONST_PART(E2))
          && !SAME_EXPR(CONST_PART(E1), gScalarOne))
            E = INUSE(MUL(CONST_PART(E1), SUB(OTHER_PART(E1), OTHER_PART(E2))));
        else if (IS_SUB(E1) && IS_CONST(E1->LeftOpnd))
            E = INUSE(SUB(E1->LeftOpnd, ADD(E1->RtOpnd, E2)));
        else if (IS_ADD(E1) && IS_CONST(E1->LeftOpnd))
            E = INUSE(ADD(E1->LeftOpnd, SUB(E1->RtOpnd, E2)));
/****   else if (IS_SUB(E2) && IS_CONST(E2->LeftOpnd))
            E = INUSE(SUB(ADD(E1, E2->RtOpnd), E2->LeftOpnd));
        else if (IS_ADD(E2) && IS_CONST(E2->LeftOpnd))
            E = INUSE(SUB(SUB(E1, E2->RtOpnd), E2->LeftOpnd));
*****/
        else {        /* it's hopeless */
            E = NEWX(cBinaryOperatorNode, 0, 0);
            E->NodeValueType = E1->NodeValueType;
            E->BinOp = cSubtract;
            E->LeftOpnd = E1;
            E->RtOpnd = E2;
            E = INUSE(E);
        }
    }
    DISPOSE_EXPR(UNUSE(E1));
    DISPOSE_EXPR(UNUSE(E2));
    return UNUSE(E);
}

pExpr SUB(pExpr E1,
          pExpr E2)
{
    /* Subtract the two expressions, producing a third which is */
    /* their difference.                                        */

    return APPLYBIN_OP(cSubtract, E1, E2);
}
