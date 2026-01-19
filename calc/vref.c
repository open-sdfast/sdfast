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

/*======*/
/* VREF */
/*======*/

pExpr VREF(register pSym V)
{
    /* Produces an expression which is a reference to the */
    /* supplied variable.                                 */
    register pExpr E;

    C_ASSERT(V != NULL, 1, "VREF");
    E = NEWX(cVarRefNode, 0, 0);
    E->VarRef = V;
    E->NumIndices = 0;
    E->NodeValueType = V->SymValueType;
    E->AssignCountE = V->AssignCountS;
    return E;
}

/*======*/
/* VREF1*/
/*======*/

pExpr VREF1(register pSym V,
            tIndex Index)
{
    /* Produces an expression which is a reference to the */
    /* supplied variable, indexed as indicated.           */
    register pExpr E;

    C_ASSERT(V != NULL, 1, "VREF1");
    if (Index >= V->SymValueType.Dim1) {
        fprintf(stderr, "VREF1: Index (%d) out of range 1..%d\n", Index,
          V->SymValueType.Dim1);
        abort();
    }
    E = NEWX(cVarRefNode, 0, 0);
    E->VarRef = V;
    E->NumIndices = 1;
    E->Indices[0] = Index;
    DIMTYPE(V, 1, &E->NodeValueType);
    E->AssignCountE = V->AssignCountS;
    return E;
}

/*=======*/
/* VREF2 */
/*=======*/

pExpr VREF2(register pSym V,
            tIndex Row,
            tIndex Col)
{
    /* Produces an expression which is a reference to */
    /* a particular element of the 2d array V.        */
    register pExpr E;

    C_ASSERT(V != NULL, 1, "VREF2");
    if (Row >= V->SymValueType.Dim1) {
        fprintf(stderr, "VREF2: Row (%d) out of range 1..%d\n", Row,
          V->SymValueType.Dim1);
        abort();
    }
    if (Col >= V->SymValueType.Dim2) {
        fprintf(stderr, "VREF2: Col (%d) out of range 1..%d\n", Col,
          V->SymValueType.Dim2);
        abort();
    }
    E = NEWX(cVarRefNode, 0, 0);
    E->VarRef = V;
    E->NumIndices = 2;
    E->Indices[0] = Row;
    E->Indices[1] = Col;
    E->AssignCountE = V->AssignCountS;
    DIMTYPE(V, 2, &E->NodeValueType);
    return E;
}
