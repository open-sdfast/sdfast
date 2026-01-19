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

/*==============*/
/* LOOKFORX_INX */
/*==============*/

int LOOKFORX_INX(pExpr X,
             pExpr E,
             char *id)
{
    /* A debugging routine which announces to `output' if expression */
    /* X is found somewhere in expression E.                         */

    register tIndex i, j;
    if (!X || !E)
        return 0;
    if (X == E) {
        fprintf(stderr, "LOOKFORX_INX(%s): found.\n", id);
        return 1;
    }
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            return LOOKFORX_INX(X, E->Opnd, id);
        case cBinaryOperatorNode:
            return LOOKFORX_INX(X, E->LeftOpnd, id) ||
              LOOKFORX_INX(X, E->RtOpnd, id);
        case cTernaryOperatorNode:
            return LOOKFORX_INX(X, E->FirstOpnd, id) ||
              LOOKFORX_INX(X, E->SecondOpnd, id) ||
              LOOKFORX_INX(X, E->ThirdOpnd, id);
        case cScalarConstNode:
            /*nope*/
            break;
        case cArray1dNode:
            for (i = 0; i < E->NodeValueType.Dim1; i++) {
                if (LOOKFORX_INX(X, E->Array1dValue[i], id))
                    return 1;
            }
            break;
        case cArray2dNode:
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                for (j = 0; j < E->NodeValueType.Dim2; j++) {
                    if (LOOKFORX_INX(X, G2d(E, i, j), id))
                        return 1;
                }
            break;
        case cVarRefNode:
            /*nope*/
            break;
        case cFunctionCallNode:
            return LOOKFORX_INX(X, E->FuncCallParm, id);
        case cFunction2CallNode:
            return LOOKFORX_INX(X, E->Func2CallParm1, id) ||
                   LOOKFORX_INX(X, E->Func2CallParm2, id);
    }
    return 0;
}        /*LOOKFORX_INX*/
