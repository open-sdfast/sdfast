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

/*========*/
/* DIMLEN */
/*========*/

static tIndex DIMLEN(pSym V,
                     tIndex Dim)
{
    /* Returns the length of the Dim'th dimension of V. Just */
    /* calls DIMTYPE to get the type of that dimension, and  */
    /* returns the "Dim1" of that type.                      */
    struct tExprType T;

    DIMTYPE(V, Dim, &T);
    return T.Dim1;
}

/*=======*/
/* LEN1d */
/*=======*/

tIndex LEN1d(pExpr E)
{
    /* Returns the length of the first dimension of E.  Scalars */
    /* return 1.  For 2d expressions, this is the number of rows. */

    C_ASSERT(E != NULL, 1, "LEN1d");
    if (E->NodeValueType.ValueType == cScalarVal)
        return 1;
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            return LEN1d(E->Opnd);
        case cBinaryOperatorNode:
            return LEN1d(E->LeftOpnd);
        case cTernaryOperatorNode:
            return LEN1d(E->FirstOpnd);
        case cArray1dNode:
        case cArray2dNode:
            return E->NodeValueType.Dim1;
        case cVarRefNode:
            return DIMLEN(E->VarRef, E->NumIndices);
        default:
            fatal("LEN1d: disaster");
            return -1; /*NOTREACHED*/
    }
}

/*=======*/
/* LEN2d */
/*=======*/

tIndex LEN2d(pExpr E)
{
    /* Returns the length of the second dimension of E. */
    /* Its an error if E is not 2-d.                    */

    C_ASSERT(E != NULL, 1, "LEN2d");
    if (E->NodeValueType.ValueType != cMatrixVal &&
      E->NodeValueType.ValueType != cArray2dVal)
        fatal("LEN2d: must be a 2d expression.");
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            return LEN2d(E->Opnd);
        case cBinaryOperatorNode:
            return LEN2d(E->LeftOpnd);
        case cTernaryOperatorNode:
            return LEN2d(E->FirstOpnd);
        case cArray2dNode:
            return E->NodeValueType.Dim2;
        case cVarRefNode:
            return DIMLEN(E->VarRef, E->NumIndices + 1);
    }
    return -1; /*NOTREACHED*/
}
