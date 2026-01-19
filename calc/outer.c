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

/*=======*/
/* OUTER */
/*=======*/

pExpr OUTER(register pExpr E1,
            register pExpr E2)
{
    /* outer product of two vectors or 1d arrays.  Answer is a matrix or */
    /* 2d array whose 1st dim is same as dim(e1), 2nd is dim(e2)         */
    pExpr E;
    register tIndex i, j;
    NodeValueType_t baseType;

    C_ASSERT(E1 && E2, 1, "OUTER");

    if (!((E1->NodeValueType.ValueType == cVectorVal ||
      E1->NodeValueType.ValueType == cScalarVal ||
      E1->NodeValueType.ValueType == cArray1dVal) &&
      (E2->NodeValueType.ValueType == cVectorVal ||
      E2->NodeValueType.ValueType == cScalarVal ||
      E2->NodeValueType.ValueType == cArray1dVal)))
        fatal("OUTER: can only do scalars, vectors and 1d arrays.");

    if (E1->NodeValueType.ValueType == cScalarVal ||
      E2->NodeValueType.ValueType == cScalarVal)
        return MUL(E1, E2);

    /* here we know that both E1&E2 are vectors or 1d arrays */

    if (E1->NodeValueType.BaseType == cScalarVal)
        baseType = E2->NodeValueType.BaseType;
    else if (E2->NodeValueType.BaseType == cScalarVal)
        baseType = E1->NodeValueType.BaseType;
    else if (E1->NodeValueType.BaseType == cVectorVal &&
      E2->NodeValueType.BaseType == cVectorVal)
        baseType = cMatrixVal;
    else
        fatal("OUTER: currently restricted to scalar or vector base type.");

    if (E1->NodeValueType.ValueType == cVectorVal &&
      E2->NodeValueType.ValueType == cVectorVal)
        E = NEW_MATX(baseType);
    else
        E = NEW_2dARRAY(baseType, LEN1d(E1), LEN1d(E2));
    E = INUSE(E);
    E1 = INUSE(E1);
    E2 = INUSE(E2);
    for (i = 0; i < LEN1d(E1); i++)
        for (j = 0; j < LEN1d(E2); j++)
            SINDX2(E, i, j, OUTER(INDX(E1, i), INDX(E2, j)));

    DISPOSE_EXPR(UNUSE(E1));
    DISPOSE_EXPR(UNUSE(E2));
    return UNUSE(E);
}        /* OUTER */
