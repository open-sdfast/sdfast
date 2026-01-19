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
/* NEG */
/*=====*/

pExpr NEG(register pExpr E)
{
    /* return the negative of E */
    register pExpr X;
    register tIndex I;

    C_ASSERT(E != NULL, 1, "NEG");
    if (IS_ZERO(E))
        return E;

    switch (E->NodeKind) {
        case cScalarConstNode:
            X = SC( - E->ScalarValue);
            DISPOSE_EXPR(E);
            return X;

        case cArray1dNode:
        case cArray2dNode:
            X = INUSE(MAKE_EXPR_LIKE(E));
            E = INUSE(E);        /* keep E around for a while */
            for (I = 0; I < LEN1d(X); I++)
                SINDX(X, I, NEG(INDX(E, I)));
            DISPOSE_EXPR(UNUSE(E));
            return UNUSE(X);

        default:
            if (IS_NEGOP(E)) {
                X = INUSE(E->Opnd);
                DISPOSE_EXPR(E);
                return UNUSE(X);
            } else {
                X = NEWX(cUnaryOperatorNode, 0, 0);
                X->NodeValueType = E->NodeValueType;
                X->UnOp = cNegate;
                X->Opnd = E;
                return X;
            }
    }
}
