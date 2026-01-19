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
/* TILDA */
/*=======*/

pExpr TILDA(register pExpr V)
{
    /* Given a vector V, return a matrix M: [0,v3,-v2],[-v3,0,v1],[v2,-v1,0] */
    register pExpr M;
    register tIndex i;

    CHECKX(V);
    if (V->NodeValueType.ValueType != cVectorVal)
        fatal("TILDA: argument must be a vector.");
    V = INUSE(V);
    M = INUSE(NEW_MATX(V->NodeValueType.BaseType));
    for (i = 0; i < 3; i++)
        SINDX2(M, i, i, MAKE_ZERO(V->NodeValueType.BaseType));
    SINDX2(M, 0, 1, NEG(INDX(V, 2)));
    SINDX2(M, 0, 2, INDX(V, 1));
    SINDX2(M, 1, 0, INDX(V, 2));
    SINDX2(M, 1, 2, NEG(INDX(V, 0)));
    SINDX2(M, 2, 0, NEG(INDX(V, 1)));
    SINDX2(M, 2, 1, INDX(V, 0));
    DISPOSE_EXPR(UNUSE(V));
    return UNUSE(M);
}
