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

void FIXUP_VREFS(register pExpr E,
            pSym S,
            long NewCnt)
{
    /* Look through each element of E to see if its a VREF to symbol S. */
    /* If so we bump the assignment count to match the symbol's current */
    /* level, since we're assigning all elements of S now.              */
    register tIndex i, j;

    if (!E)
        return;

    if (IS_VREF(E))
        if (E->VarRef == S) {
            E->AssignCountE = NewCnt;
            return;
        }

    switch (E->NodeValueType.ValueType) {
        case cScalarVal:
            /* nothing */
            break;

        case cVectorVal:
        case cArray1dVal:
            if (E->NodeKind != cArray1dNode)
                return;
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                FIXUP_VREFS(E->Array1dValue[i], S, NewCnt);
            break;

        case cMatrixVal:
        case cArray2dVal:
            if (E->NodeKind != cArray2dNode)
                return;
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                for (j = 0; j < E->NodeValueType.Dim2; j++)
                    FIXUP_VREFS(G2d(E, i, j), S, NewCnt);
            break;
    }
}
