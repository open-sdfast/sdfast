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

/*=========*/
/*  USEXIF */
/*=========*/

pExpr USEXIF(register pExpr X,
             pExpr VX)
{
    /* This routine is similar to CLEANX.  It examines the     */
    /* supplied expression X, and if it is sufficiently simple */
    /* it returns it.  Otherwise, it returns the supplied      */
    /* variable reference VX.  X is only allowed to be a       */
    /* scalar, vector or matrix.  If X is a vector or matrix,  */
    /* each element is examined and left alone or replaced     */
    /* with the appropriately indexed VREF.                    */
    /* Both X and VX are disposed.                            */
    pExpr E;
    register tIndex i, j;

    C_ASSERT(VX != NULL, 1, "USEXIF");
    C_ASSERT(IS_VREF(VX), 2, "USEXIF");
    switch (VX->NodeValueType.ValueType) {
        case cScalarVal:
            if (!X)
                return VX;
            else if (!IS_SIMPLE(X)) {
                DISPOSE_EXPR(X);
                return VX;
            } else {
                DISPOSE_EXPR(VX);
                return X;
            }

        case cVectorVal:
            E = INUSE(NEW_VECX(cScalarVal));
            X = INUSE(X);
            VX = INUSE(VX);
            for (i = 0; i < 3; i++)
                if (IS_SIMPLE(INDX(X, i)))
                    SINDX(E, i, INDX(X, i));
                else
                    SINDX(E, i, INDX(VX, i));
            DISPOSE_EXPR(UNUSE(X));
            DISPOSE_EXPR(UNUSE(VX));
            return UNUSE(E);

        case cMatrixVal:
            E = INUSE(NEW_MATX(cScalarVal));
            X = INUSE(X);
            VX = INUSE(VX);
            for (i = 0; i < 3; i++)
                for (j = 0; j < 3; j++)
                    if (IS_SIMPLE(INDX2(X, i, j)))
                        SINDX2(E, i, j, INDX2(X, i, j));
                    else
                        SINDX2(E, i, j, INDX2(VX, i, j));
            DISPOSE_EXPR(UNUSE(X));
            DISPOSE_EXPR(UNUSE(VX));
            return UNUSE(E);
    }
    /*NOTREACHED*/
}
