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

/*=============*/
/* SCALAR_TYPE */
/*=============*/

void SCALAR_TYPE(register struct tExprType *T)
{
    /* returns a scalar type in T */

    T->ValueType = cScalarVal;
    T->Dim1 = 1;
    T->Dim2 = 1;
    T->BaseType = cScalarVal;
}

void VECTOR_TYPE(NodeValueType_t baseType,
            register struct tExprType *T)
{
    T->ValueType = cVectorVal;
    T->Dim1 = 3;
    T->Dim2 = 1;
    T->BaseType = baseType;
}

void MATRIX_TYPE(NodeValueType_t baseType,
            register struct tExprType *T)
{
    T->ValueType = cMatrixVal;
    T->Dim1 = 3;
    T->Dim2 = 3;
    T->BaseType = baseType;
}

void ARRAY1d_TYPE(NodeValueType_t baseType,
             tIndex dim1,
             register struct tExprType *T)
{
    T->ValueType = cArray1dVal;
    T->Dim1 = dim1;
    T->Dim2 = 1;
    T->BaseType = baseType;
}

void ARRAY2d_TYPE(NodeValueType_t baseType,
             tIndex dim1,
             tIndex dim2,
             register struct tExprType *T)
{
    T->ValueType = cArray2dVal;
    T->Dim1 = dim1;
    T->Dim2 = dim2;
    T->BaseType = baseType;
}
