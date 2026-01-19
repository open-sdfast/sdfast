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
/* CALL_FUNC */
/*===========*/

pExpr CALL_FUNC(enum tKnownFunction Func,
                pExpr Parm)
{
    /* Applies the supplied one-arg function to the expression. */

    switch (Func) {
        case cSine:
            return SINE(Parm);
        case cCosine:
            return COSINE(Parm);
        case cAsin:
            return ASINE(Parm);
        case cAcos:
            return ACOSN(Parm);
        case cAbs:
            return ABS(Parm);
        case cSqrt:
            return SQRTT(Parm);
        default:
            fatal("CALL_FUNC: unrecognized one-arg function");
    }
    /*NOTREACHED*/
}

/*============*/
/* CALL_FUNC2 */
/*============*/

pExpr CALL_FUNC2(enum tKnownFunction Func,
                 pExpr Parm1, pExpr Parm2)
{
    /* Applies the supplied two-arg function to the expressions. */

    switch (Func) {
        case cAtan2:
            return ATANG2(Parm1,Parm2);
        default:
            fatal("CALL_FUNC: unrecognized two-arg function");
    }
    /*NOTREACHED*/
}

/*======*/
/* EVAL */
/*======*/

pExpr EVAL(register pExpr E)
{
    /* Attempts to evaluate the expression.                                   */
    /* Disposes E, so watch out.                                              */
    /* EVAL is guaranteed to return an expression which contains no nodes from*/
    /* the original expression, except for nodes whose protection=cPermanent. */
    /* This avoids potential disasters which could occur if variables are     */
    /* disposed later, and EVAL has used their values in some expression.     */
    pExpr X, V;
    register tIndex I;

    E = INUSE(E);        /* we don't want E to get hurt yet */

    /* We compute a value for EVAL in this case statement, and put it in X. */
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            X = UN_OP(E->UnOp, EVAL(E->Opnd));
            break;
        case cBinaryOperatorNode:
            X = BIN_OP(E->BinOp, EVAL(E->LeftOpnd), EVAL(E->RtOpnd));
            break;
        case cTernaryOperatorNode:
            X = TER_OP(E->TerOp, EVAL(E->FirstOpnd), EVAL(E->SecondOpnd),
                                 EVAL(E->ThirdOpnd));
            break;
        case cScalarConstNode:
            X = E->Protection == cPermanent ? E : SC(E->ScalarValue);
            break;
        case cArray1dNode:
            X = E->NodeValueType.ValueType == cVectorVal ?
              NEW_VECX(E->NodeValueType.BaseType) :
              NEW_1dARRAY(E->NodeValueType.BaseType, E->NodeValueType.Dim1);
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                SINDX(X, I, EVAL(INDX(E, I)));
            break;
        case cArray2dNode:
            X = E->NodeValueType.ValueType == cMatrixVal ?
              NEW_MATX(E->NodeValueType.BaseType) :
              NEW_2dARRAY(E->NodeValueType.BaseType,
              E->NodeValueType.Dim1, E->NodeValueType.Dim2);
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                SINDX(X, I, EVAL(INDX(E, I)));
            break;
        case cVarRefNode:
            if (!E->VarRef->SymValue)
                X = E->Protection == cPermanent ? E : COPY_EXPR(E);
            else {        /* there is a value -- index it if necessary. */
                V = E->VarRef->SymValue;
                for (I = 0; I < E->NumIndices; I++)
                    V = INDX(V, E->Indices[I]);        /* select the element */
                if (V->NodeKind == cVarRefNode && SAME_VREF(E, V))
                        X = E->Protection == cPermanent ? E :
                          COPY_EXPR(E);        /* avoid infinite recursion */
                else
                    X = EVAL(V);
            }
            break;
        case cFunctionCallNode:
            X = CALL_FUNC(E->FuncVarRef->WhichFunction,
              EVAL(E->FuncCallParm));
            break;
        case cFunction2CallNode:
            X = CALL_FUNC2(E->Func2VarRef->WhichFunction,
                           EVAL(E->Func2CallParm1), EVAL(E->Func2CallParm2));
            break;
    }        /* case */

    DISPOSE_EXPR(UNUSE(E));
    return X;
}
