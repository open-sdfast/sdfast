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
/* DOT */
/*=====*/

pExpr DOT(register pExpr E1,
          register pExpr E2)
{
    /* dot product of two vectors -- 0 element ignored*/
    pExpr E;
    register tIndex i;

    C_ASSERT(E1 && E2, 1, "DOT");
    if (!((E1->NodeValueType.ValueType == cVectorVal ||
      E1->NodeValueType.ValueType == cArray1dVal) &&
      (E2->NodeValueType.ValueType == cVectorVal ||
      E2->NodeValueType.ValueType == cArray1dVal) &&
      E1->NodeValueType.ValueType == E2->NodeValueType.ValueType))
        fatal("DOT: can only do vectors and 1d arrays.");
    if (LEN1d(E1) != LEN1d(E2))
        fatal("DOT: lengths weren't the same.");
    E = gScalarZero;
    E1 = INUSE(E1);
    E2 = INUSE(E2);
    if (E1->NodeValueType.BaseType != cScalarVal &&
      E2->NodeValueType.BaseType != cScalarVal) {
        if (E1->NodeValueType.BaseType == E2->NodeValueType.BaseType)
            E = E1->NodeValueType.BaseType == cVectorVal ? gScalarZero :
              MAKE_ZERO(cMatrixVal);
        else
            E = MAKE_ZERO(cVectorVal);

        for (i = 0; i < LEN1d(E1); i++)
            E = ADD(E, MATMUL(INDX(E1, i), INDX(E2, i)));
    } else if (E1->NodeValueType.BaseType != cScalarVal ||
      E2->NodeValueType.BaseType != cScalarVal) {        /* just one is scalar */
        E = MAKE_ZERO_LIKE(INDX(
          E1->NodeValueType.BaseType == cScalarVal ? E2 : E1, 0));
        for (i = 0; i < LEN1d(E1); i++)
            E = ADD(E, MUL(INDX(E1, i), INDX(E2, i)));
    }
    else {        /* both base types are scalars */
        E = gScalarZero;
        for (i = 0; i < LEN1d(E1); i++)
            E = ADD(E, MUL(INDX(E1, i), INDX(E2, i)));
    }
    E = INUSE(E);        /* protect the pieces of E1,E2 used by E */
    DISPOSE_EXPR(UNUSE(E1));
    DISPOSE_EXPR(UNUSE(E2));
    return UNUSE(E);
}        /* DOT */


