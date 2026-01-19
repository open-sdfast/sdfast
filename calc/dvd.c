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
/* DVD */
/*=====*/

pExpr DO_DVD(register pExpr N,
             register pExpr D)
{
    /* This function is what actually does a DVD. */
    register pExpr X;
    register tIndex I;

    C_ASSERT(N && D, 1, "DVD");
    if (D->NodeValueType.ValueType != cScalarVal)
        fatal("DVD: denominator must be a scalar.");
    if (IS_ZERO(D))
        fatal("DVD: tried to divide by zero.");
    if (IS_ONE(D))
        return N;
    else if (IS_MINUSONE(D))
        return NEG(N);
    else if (SAME_EXPR(N, D))
        return gScalarOne;        /* they are both scalars, since D is */
    else if (IS_ZERO(N))
        return N;
    else if (N->NodeValueType.ValueType == cScalarVal) {
        /* both N & D are scalars */
        if (IS_CONST(N) && IS_CONST(D)) {        /* real division] */
            X = SC(N->ScalarValue / D->ScalarValue);
            DISPOSE_EXPR(N);
            DISPOSE_EXPR(D);
        } else {        /* can't divide */
            X = NEWX(cBinaryOperatorNode, 0, 0);
            SCALAR_TYPE(&X->NodeValueType);
            X->BinOp = cDivide;
            X->LeftOpnd = N;
            X->RtOpnd = D;
        }
        return X;
    } else {        /* N is not a scalar -- slice & conquer */
        X = INUSE(MAKE_EXPR_LIKE(N));
        N = INUSE(N);
        D = INUSE(D);
        for (I = 0; I < LEN1d(X); I++)
            SINDX(X, I, DVD(INDX(N, I), D));
        DISPOSE_EXPR(UNUSE(N));
        DISPOSE_EXPR(UNUSE(D));
        return UNUSE(X);
    }
}        /* DO_DVD */

pExpr DVD(pExpr N,
          pExpr D)
{
    /* Divide N (numerator) by D (denominator).  D must be a scalar. */
    return APPLYBIN_OP(cDivide, N, D);
}
