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

/* DERIV
 *
 * Take derivative of E with respect to V. E must be vec or
 * scalar expression.  V must be a 0,1, or 2 index VREF. 
 * The type of V must be scalar after indices apply. 
 */
pExpr 
DERIV(register pExpr E,
      register pExpr V)
{
    pExpr D, X;
    register tIndex i;

    CHECKX(E);
    CHECKX(V);
    E = INUSE(E);
    V = INUSE(V);
    if (E->NodeValueType.ValueType == cVectorVal) {
        D = INUSE(NEW_VECX(E->NodeValueType.BaseType));
        for (i = 0; i < 3; i++)
            SINDX(D, i, DERIV(INDX(E, i), V));
        goto L2;
    }

    if (E->NodeValueType.ValueType != cScalarVal)
        fatal("DERIV: expression must be scalar type.");
    if (!IS_VREF(V))
        fatal("DERIV: V must be a variable reference.");
    if (V->NodeValueType.ValueType != cScalarVal)
        fatal("DERIV: var must be scalar.");
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            if (E->UnOp == cNegate)
                D = INUSE(NEG(DERIV(E->Opnd, V)));
            else
                fatal("DERIV: only negate allowed.");
            break;

        case cBinaryOperatorNode:
            switch (E->BinOp) {
                case cAdd:
                    D = INUSE(ADD(DERIV(E->LeftOpnd, V), DERIV(E->RtOpnd, V)));
                    break;
                case cSubtract:
                    D = INUSE(SUB(DERIV(E->LeftOpnd, V), DERIV(E->RtOpnd, V)));
                    break;
                case cMultiply:
                    D = INUSE(ADD(MUL(DERIV(E->LeftOpnd, V), E->RtOpnd),
                      MUL(E->LeftOpnd, DERIV(E->RtOpnd, V))));
                    break;
                case cDivide:        /* d(a/b)=d(a)/b-(a*d(b)/b*b) */
                    D = INUSE(SUB(DVD(DERIV(E->LeftOpnd, V), E->RtOpnd),
                      DVD(MUL(E->LeftOpnd, DERIV(E->RtOpnd, V)), 
                      MUL(E->RtOpnd, E->RtOpnd))));
                    break;
                default:
                    fatal("DERIV: only add, sub, mul or dvd allowed.");
            }
            break;

        case cTernaryOperatorNode:
            if (E->TerOp == cIfThenElse)
                D = INUSE(QUES(E->FirstOpnd, 
                               DERIV(E->SecondOpnd, V),
                               DERIV(E->ThirdOpnd, V)));
            else
                fatal("DERIV: only ques operator allowed");
            break;

        case cScalarConstNode:
            D = gScalarZero;
            break;

        case cVarRefNode:
            if (SAME_VREF(E, V)) {
                D = gScalarOne;
                goto L2;        /* done */
            }
            X = E->VarRef->SymValue;
            if (!X) {
                D = gScalarZero;
                goto L2;        /* done */
            }
            /* select the pertinent element */
            for (i = 0; i < E->NumIndices; i++)
                X = INDX(X, E->Indices[i]);
            if (SAME_VREF(E, X)) {
              /* cur val is just a vref to itself - how 'bout remembered val? */
                X = E->VarRef->RememberedVal;
                if (!X) {
                    D = gScalarZero;
                    goto L2;        /* done */
                }
                for (i = 0; i < E->NumIndices; i++)
                    X = INDX(X, E->Indices[i]);
                if (SAME_VREF(E, X)) {        /* nothing useful remembered */
                    D = gScalarZero;
                    goto L2;
                }
            }
            D = INUSE(DERIV(X, V));
            break;

        case cFunctionCallNode:
            switch (E->FuncVarRef->WhichFunction) {
                case cSine:
                    D = INUSE(MUL(COSINE(E->FuncCallParm),
                      DERIV(E->FuncCallParm, V)));
                    break;
                case cCosine:
                    D = INUSE(MUL(NEG(SINE(E->FuncCallParm)),
                      DERIV(E->FuncCallParm, V)));
                    break;
                case cSqrt:
                    /*  d sqrt(x)     0.5     dx
                     *  --------- = ------- * --
                     *    d v        sqrt(x)  dv
                     */
                    D = INUSE(MUL(DVD(SC(0.5),E), DERIV(E->FuncCallParm, V)));
                    break;
                default:
                    fatal("DERIV: only sin, cos, and sqrt allowed");
            }
            break;

        case cFunction2CallNode:
            fatal("DERIV: no two-arg functions allowed");
            break;
    }

L2:        /* D is the derivative of E wrt V */
    DISPOSE_EXPR(UNUSE(E));
    DISPOSE_EXPR(UNUSE(V));
    return UNUSE(D);
}
