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

static void RETURN_BASE_TYPE(pSym V,
                        struct tExprType *T)
{
    /* set T to base type of V */

    T->ValueType = V->SymValueType.BaseType;
    T->Dim1 = T->ValueType == cScalarVal ? 1 : 3;
    T->Dim2 = T->ValueType == cMatrixVal ? 3 : 1;
    T->BaseType = cScalarVal;
}

static void RETURN_VECTOR_TYPE(struct tExprType *T)
{
    T->ValueType = cVectorVal;
    T->Dim1 = 3;
    T->Dim2 = 1;
    T->BaseType = cScalarVal;
}

static void RETURN_SCALAR_TYPE(struct tExprType *T)
{
    T->ValueType = cScalarVal;
    T->Dim1 = 1;
    T->Dim2 = 1;
    T->BaseType = cScalarVal;
}

static void RETURN_ROW_TYPE(pSym V,
                       struct tExprType *T)
{
    /* Make T look like the type of a row of 2d V. */
    T->ValueType = V->SymValueType.ValueType == cMatrixVal ?
      cVectorVal : cArray1dVal;
    T->Dim1 = V->SymValueType.Dim2;
    T->Dim2 = 1;
    T->BaseType = V->SymValueType.BaseType;
}

static void DIM_TOO_BIG(int Where,
                   pSym V,
                   tIndex Dim)
{
    fprintf(stderr, "DIMTYPE: Dim (%d) too big for var \"%s\". (Loc %d)\n", Dim,
      V->PrintName, Where);
    abort();
}

/*=========*/
/* DIMTYPE */
/*=========*/

void DIMTYPE(pSym V,
        tIndex Dim,
        struct tExprType *T)
{
    /* This routine returns the type of the Dim'th dimension of */
    /* variable V.  If V is a scalar, the only legal value for  */
    /* Dim is 0.  For other types Dim can go up to the number   */
    /* of dimensions for the variable plus the number in its    */
    /* base type.  For example, if V is an Array1d of matrices, */
    /* dim can go up to 3.  The type of Dim=0 is Array1d of     */
    /* matrices.  Dim=1 returns "matrix".  Dim=2 returns vector.*/
    /* Dim=3 returns scalar. Dim=4 bombs out.                   */

    if (Dim == 0) {
        *T = V->SymValueType;
        return;
    }
    if (V->SymValueType.ValueType == cScalarVal)
        fatal("DIMTYPE: var was scalar, but Dim>0.");
    switch (V->SymValueType.ValueType) {
        case cVectorVal:
        case cArray1dVal:
            switch (Dim) {
                case 1:
                    RETURN_BASE_TYPE(V, T);
                    break;

                case 2:
                    switch (V->SymValueType.BaseType) {
                        case cVectorVal:
                            RETURN_SCALAR_TYPE(T);
                            break;
                        case cMatrixVal:
                            RETURN_VECTOR_TYPE(T);
                            break;
                        default:
                            DIM_TOO_BIG(1, V, Dim);
                    }
                    break;

                case 3:
                    switch (V->SymValueType.BaseType) {
                        case cMatrixVal:
                            RETURN_SCALAR_TYPE(T);
                            break;
                        default:
                            DIM_TOO_BIG(2, V, Dim);
                    }
                    break;

                default:
                    DIM_TOO_BIG(3, V, Dim);
            }
            break;

        case cMatrixVal:
        case cArray2dVal:
            switch (Dim) {
                case 1:
                    RETURN_ROW_TYPE(V, T);
                    break;

                case 2:
                    RETURN_BASE_TYPE(V, T);
                    break;

                case 3:
                    switch (V->SymValueType.BaseType) {
                        case cVectorVal:
                            RETURN_SCALAR_TYPE(T);
                            break;
                        case cMatrixVal:
                            RETURN_VECTOR_TYPE(T);
                            break;
                        default:
                            DIM_TOO_BIG(4, V, Dim);
                    }
                    break;

                case 4:
                    switch (V->SymValueType.BaseType) {
                        case cMatrixVal:
                            RETURN_SCALAR_TYPE(T);
                            break;
                        default:
                            DIM_TOO_BIG(5, V, Dim);
                    }
                    break;

                default:
                    DIM_TOO_BIG(6, V, Dim);
            }
            break;
    }
}        /* DIMTYPE */
