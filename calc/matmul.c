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
/* MULBASETYPE */
/*=============*/

static NodeValueType_t MULBASETYPE(register NodeValueType_t t1, 
                                   register NodeValueType_t t2)
{
    /* Returns the NodeValueType which would result if expressions           */
    /* of types t1 & t2 were multiplied by MUL, DOT or MATUL as appropriate. */

    if (t1 == cScalarVal)
        return t2;
    else if (t2 == cScalarVal)
        return t1;
    else if (t1 == cVectorVal) {
        if (t2 == cVectorVal)
            return cScalarVal;
        else if (t2 == cMatrixVal)
            return cVectorVal;
        else
            fatal("MULBASETYPE: second argument wasn't scalar, vector, or matrix");
    } else if (t1 == cMatrixVal) {
        if (t2 == cVectorVal)
            return cVectorVal;
        else if (t2 == cMatrixVal)
            return cMatrixVal;
        else
            fatal("MULBASETYPE: second argument wasn't scalar, vector, or matrix");
    } else
        fatal("MULBASETYPE: first argument wasn't scalar, vector, or matrix");
    return cScalarVal; /*NOTREACHED*/
}

/*========*/
/* MATMUL */
/*========*/

pExpr MATMUL(register pExpr M1,
             register pExpr M2)
{
    /* Matrix multiplication.  Uses DOT if M1&M2 both 1d.     */
    /* If M1 is nxm, then M2                                  */
    /* must be mxp for some p.  Arrays are 1xm on the left,   */
    /* mx1 on the right.                                      */
    /* 0th row and column is ignored.                         */
    pExpr X, T;
    register tIndex i, j;

    C_ASSERT(M1 && M2, 1, "MATMUL");
    if (M1->NodeValueType.ValueType == cScalarVal ||
      M2->NodeValueType.ValueType == cScalarVal) {
        return MUL(M1, M2);
    }
    if (((M1->NodeValueType.ValueType == cVectorVal) &&
      (M2->NodeValueType.ValueType == cVectorVal)) ||
      ((M1->NodeValueType.ValueType == cArray1dVal) &&
      (M2->NodeValueType.ValueType == cArray1dVal))) {
        return DOT(M1, M2);
    }
    M1 = INUSE(M1);
    M2 = INUSE(M2);

    /* If M2 is 2d, we will need to take column "slices" to dot with row */
    /* slices of M1.  Since INDX only gives row slices, we'll make a     */
    /* 2d array T which has the elements of M2 in transposed positions.  */
    /* This is NOT equivalent to TRANSPOSE(M2), however, since the       */
    /* elements themselves are not transposed in T.                      */
    if (M2->NodeValueType.ValueType == cMatrixVal ||
      M2->NodeValueType.ValueType == cArray2dVal) {
        if (M2->NodeValueType.ValueType == cMatrixVal)
            T = NEW_MATX(M2->NodeValueType.BaseType);
        else
            T = NEW_2dARRAY(M2->NodeValueType.BaseType, LEN2d(M2), LEN1d(M2));
        for (i = 0; i < M2->NodeValueType.Dim1; i++)
            for (j = 0; j < M2->NodeValueType.Dim2; j++)
                SINDX2(T, j, i, INDX2(M2, i, j));
    } else
        T = M2;
    T = INUSE(T);
    /* Now we'll build the answer in X. */
    if (M1->NodeValueType.ValueType == cVectorVal) {
        if (M2->NodeValueType.ValueType != cMatrixVal)
            fatal("MATMUL: m1 vec, so m2 should be matrix.");
        X = INUSE(NEW_VECX(cScalarVal));
        for (i = 0; i < 3; i++)
            SINDX(X, i, DOT(M1, INDX(T, i)));
    } else if (M1->NodeValueType.ValueType == cMatrixVal) {
        if (M2->NodeValueType.ValueType == cVectorVal) {
            X = INUSE(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++)
                SINDX(X, i, DOT(INDX(M1, i), M2));
        } else if (M2->NodeValueType.ValueType == cMatrixVal) {
            X = INUSE(NEW_MATX(cScalarVal));
            for (i = 0; i < 3; i++)
                for (j = 0; j < 3; j++)
                    SINDX2(X, i, j, DOT(INDX(M1, i), INDX(T, j)));
        } else
            fatal("MATMUL: m1 matrix, m2 must be vec or mat");
    } else if (M1->NodeValueType.ValueType == cArray2dVal) {
        if (M2->NodeValueType.ValueType == cArray1dVal) {
            X = INUSE(NEW_1dARRAY(MULBASETYPE(M1->NodeValueType.BaseType,
              M2->NodeValueType.BaseType),
              M1->NodeValueType.Dim1));
            for (i = 0; i < M1->NodeValueType.Dim1; i++)
                SINDX(X, i, DOT(INDX(M1, i), M2));
        } else if (M2->NodeValueType.ValueType == cArray2dVal) {
            X = INUSE(NEW_2dARRAY(MULBASETYPE(M1->NodeValueType.BaseType,
              M2->NodeValueType.BaseType),
              M1->NodeValueType.Dim1, M2->NodeValueType.Dim2));
            for (i = 0; i < M1->NodeValueType.Dim1; i++)
                for (j = 0; j < M2->NodeValueType.Dim2; j++)
                    SINDX2(X, i, j, DOT(INDX(M1, i), INDX(T, j)));
        } else
            fatal("MATMUL: m1 array2d, m2 must be array1d or 2d");
    } else {
        fprintf(stderr, "MATMUL: illegal argument type: %d\n\
MATMUL: hint -- can't handle M1 a 1d array.\n",
          M1->NodeValueType.ValueType);
        abort();
    }
    DISPOSE_EXPR(UNUSE(T));
    DISPOSE_EXPR(UNUSE(M1));
    DISPOSE_EXPR(UNUSE(M2));
    return UNUSE(X);
}        /* MATMUL */
