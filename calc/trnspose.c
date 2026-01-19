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

/*===========*/
/* TRANSPOSE */
/*===========*/

pExpr TRANSPOSE(register pExpr E)
{
    /* Return transpose of E.  If E is scalar or vector we      */
    /* just return it, otherwise make a new 2d thing.           */
    /* The 0th row and column are ignored in the transposition. */
    register pExpr X;
    register tIndex I, J;

    C_ASSERT(E != NULL, 1, "TRANSPOSE");
    if (E->NodeValueType.ValueType != cMatrixVal &&
      E->NodeValueType.ValueType != cArray2dVal)
        return E;
    else {
        if (E->NodeValueType.ValueType == cMatrixVal)
            X = INUSE(NEW_MATX(E->NodeValueType.BaseType));
        else
            X = INUSE(NEW_2dARRAY(E->NodeValueType.BaseType,
              LEN2d(E), LEN1d(E)));
        E = INUSE(E);
        if (E->NodeValueType.BaseType == cScalarVal ||
          E->NodeValueType.BaseType == cVectorVal)
            for (I = 0; I < LEN2d(E); I++)
                for (J = 0; J < LEN1d(E); J++)
                    SINDX2(X, I, J, INDX2(E, J, I));
        else
            for (I = 0; I < LEN2d(E); I++)
                for (J = 0; J < LEN1d(E); J++)
                    SINDX2(X, I, J, TRANSPOSE(INDX2(E, J, I)));
        DISPOSE_EXPR(UNUSE(E));
        return UNUSE(X);
    }
}
