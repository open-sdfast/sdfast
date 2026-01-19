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
/* CROSS */
/*=======*/

pExpr CROSS(pExpr W, 
            pExpr V)
{
    /* Cross two vectors, to give a third. 0 element ignored */
    pExpr X;

    C_ASSERT(W && V, 1, "CROSS");
    if (W->NodeValueType.ValueType != cVectorVal ||
      V->NodeValueType.ValueType != cVectorVal)
        fatal("CROSS: only vectors!!");
    if (W->NodeValueType.BaseType != V->NodeValueType.BaseType)
        fatal("CROSS: base types didn't match.");
    X = INUSE(NEW_VECX(W->NodeValueType.BaseType));
    W = INUSE(W);
    V = INUSE(V);
    SINDX(X, 0,
      SUB(MUL(INDX(W, 1), INDX(V, 2)),
      MUL(INDX(W, 2), INDX(V, 1))));
    SINDX(X, 1,
      SUB(MUL(INDX(W, 2), INDX(V, 0)),
      MUL(INDX(W, 0), INDX(V, 2))));
    SINDX(X, 2,
      SUB(MUL(INDX(W, 0), INDX(V, 1)),
      MUL(INDX(W, 1), INDX(V, 0))));
    DISPOSE_EXPR(UNUSE(W));
    DISPOSE_EXPR(UNUSE(V));
    return UNUSE(X);
}        /* CROSS */
