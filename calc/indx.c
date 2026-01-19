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
/* CHECKX  */
/*=========*/

void CHECKX(pExpr E)
{
    /* Looks at E and dies if it doesn't like it.  Notably, */
    /* if E is free we croak.                               */
    if (!E)
        fatal("CHECKX: nil expression.");
}

/*=======*/
/* SINDX */
/*=======*/

void SINDX(register pExpr E,
      register tIndex I,
      pExpr Val)
{
    /* Sets the indexed element of E to value Val.  Val must */
    /* have type baseType(E).                                */
    /* The protection level of Val is raised by the protection */
    /* level of E to ensure that it won't be disposed of before*/
    /* E is.                                                   */
    register tIndex J;
    pExpr X;

    CHECKX(E);
    CHECKX(Val);
    if (E->NodeValueType.ValueType == cScalarVal)
        fatal("SINDX: can't index a scalar.");
    if (E->NodeValueType.ValueType == cMatrixVal &&
      Val->NodeValueType.ValueType != cVectorVal ||
      E->NodeValueType.ValueType == cArray2dVal &&
      Val->NodeValueType.ValueType != cArray1dVal ||
      E->NodeValueType.ValueType != cMatrixVal &&
      E->NodeValueType.ValueType != cArray2dVal &&
      E->NodeValueType.BaseType != Val->NodeValueType.ValueType)
        fatal("SINDX: incompatible types.");
    switch (E->NodeKind) {
        case cArray1dNode:
          /* We're going to dispose of the original value in E, but we have to
             protect Val since it may include terms from E.
             When we nuke the old term we reduce its protection by the
             amount attributable  to its association with E, i.e. E^.Protection.
             We'll protect Val by one more than that to keep any part of it from
             disappearing out from under.        */

            if (E->Protection != cPermanent) {
                PROTECT(Val, E->Protection + 1);
                UNPROTECT(E->Array1dValue[I], E->Protection);
                DISPOSE_EXPR(E->Array1dValue[I]);
                E->Array1dValue[I] = UNUSE(Val);
            } else {
                PROTECT(Val, cPermanent);
                E->Array1dValue[I] = Val;
            }
            break;
        case cArray2dNode:
            if (LEN1d(Val) != E->NodeValueType.Dim2)
                fatal("SINDX: Val not right len for row.");

            /*==================================================*/
            /* INDX would love to dispose of Val before we want */
            /* it to.  We must make sure it realizes that we're */
            /* using it.                                        */
            /*==================================================*/

            Val = INUSE(Val);
            for (J = 0; J < E->NodeValueType.Dim2; J++) {
                X = INDX(Val, J);
                PROTECT(X, E->Protection);
                if (E->Protection != cPermanent) {
                    UNPROTECT(G2d(E, I, J), E->Protection);
                    DISPOSE_EXPR(G2d(E, I, J));
                }
                S2d(E, I, J, X);
            }
            DISPOSE_EXPR(UNUSE(Val));
            break;
        default:
            fatal("SINDX: can't assign to this kind of expr.");
    }        /* case */
}        /* SINDX */

/*========*/
/* SINDX2 */
/*========*/

void SINDX2(register pExpr E,
       tIndex I,
       tIndex J,
       pExpr Val)
{
    /* Sets E[I,J] to Val.  Val must be of the base type */
    /* of E, and E must be a matrix or array2d.          */
    /* Protection considerations are the same as for SINDX. */

    CHECKX(E);
    CHECKX(Val);
    if (E->NodeKind != cArray2dNode)
        fatal("SINDX2: must be array2d node.");
    if (E->NodeValueType.BaseType != Val->NodeValueType.ValueType)
        fatal("SINDX2: val not base type of expr.");
    if (E->Protection != cPermanent) {
        PROTECT(Val, E->Protection + 1);
        UNPROTECT(G2d(E, I, J), E->Protection);
        DISPOSE_EXPR(G2d(E, I, J));
        S2d(E, I, J, UNUSE(Val));
    } else {
        PROTECT(Val, cPermanent);
        S2d(E, I, J, Val);
    }
}

/*=======*/
/* INDX  */
/*=======*/

pExpr INDX(
           register pExpr E,
           tIndex I)
{
    /* Returns the expression E[I] of type baseType(E). */
    /* Disposes E, so WATCH OUT!!!                      */
    register tIndex J;
    register pExpr X,temp;

    CHECKX(E);
    if (E->NodeValueType.ValueType == cScalarVal)
        fatal("INDX: can't index a scalar.");
    E = INUSE(E);        /* save 'til later */
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            X = INUSE(UN_OP(E->UnOp, INDX(E->Opnd, I)));
            break;
        case cBinaryOperatorNode:
            X = INUSE(BIN_OP(E->BinOp, INDX(E->LeftOpnd, I),
              INDX(E->RtOpnd, I)));
            break;
        case cTernaryOperatorNode:
            /* For if-then-else, don't index the condition. */
            if (E->TerOp == cIfThenElse)
                temp = E->FirstOpnd;
            else temp = INDX(E->FirstOpnd, I);
            X = INUSE(TER_OP(E->TerOp, temp,
                      INDX(E->SecondOpnd, I), INDX(E->ThirdOpnd, I)));
            break;
        case cArray1dNode:
            X = INUSE(E->Array1dValue[I]);
            break;
        case cArray2dNode:
            X = E->NodeValueType.ValueType == cMatrixVal ?
              INUSE(NEW_VECX(E->NodeValueType.BaseType)) :
              INUSE(NEW_1dARRAY(E->NodeValueType.BaseType,
              E->NodeValueType.Dim2));
            for (J = 0; J < E->NodeValueType.Dim2; J++)
                SINDX(X, J, G2d(E, I, J));
            break;
        case cVarRefNode:
            X = INUSE(COPY_EXPR(E));        /* Copy the variable reference */
            X->Indices[X->NumIndices++] = I;
            DIMTYPE(X->VarRef, X->NumIndices, &X->NodeValueType);
            break;
    }        /* case */
    DISPOSE_EXPR(UNUSE(E));
    return UNUSE(X);
}        /* INDX */

/*=======*/
/* INDX2 */
/*=======*/

pExpr INDX2(register pExpr E,
            tIndex Row,
            tIndex Col)
{
    /* Doubly index a 2-d array valued expression. */
    register pExpr X, temp;

    CHECKX(E);
    if (E->NodeValueType.ValueType != cMatrixVal &&
      E->NodeValueType.ValueType != cArray2dVal)
        fatal("INDX2: expression not 2d");
    if (E->NodeKind == cArray2dNode) {        /* special case for speed */
        return G2d(E, Row, Col);
    }
    E = INUSE(E);
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            X = INUSE(UN_OP(E->UnOp, INDX2(E->Opnd, Row, Col)));
            break;
        case cBinaryOperatorNode:
            X = INUSE(BIN_OP(E->BinOp, INDX2(E->LeftOpnd, Row, Col),
              INDX2(E->RtOpnd, Row, Col)));
            break;
        case cTernaryOperatorNode:
            /* For if-then-else, don't index the condition. */
            if (E->TerOp == cIfThenElse)
                temp = E->FirstOpnd;
            else temp = INDX2(E->FirstOpnd, Row, Col);
            X = INUSE(TER_OP(E->TerOp, temp,
                      INDX2(E->SecondOpnd, Row, Col), 
                      INDX2(E->ThirdOpnd, Row, Col)));
            break;
        /**** case cArray2dNode:           */
        /****   X = INUSE(G2d(E,Row,Col)); */
        /****   break;                           */
        case cVarRefNode:
            X = INUSE(COPY_EXPR(E));        /* Copy the variable reference */
            X->Indices[X->NumIndices++] = Row;
            X->Indices[X->NumIndices++] = Col;
            DIMTYPE(X->VarRef, X->NumIndices, &X->NodeValueType);
            break;
    }
    DISPOSE_EXPR(UNUSE(E));
    return UNUSE(X);
}        /* INDX2 */

/*====================*/
/* INDX11, 12, 21, 22 */
/*====================*/

pExpr INDX11(pExpr E,
             tIndex I,
             tIndex J)
{
    /* This indexed E by I, and then again by J. */

    return INDX(INDX(E, I), J);
}

pExpr INDX12(pExpr E,
             tIndex I,
             tIndex J,
             tIndex K)
{
    /* Indexes E by I, then INDX2s the result using J,K */

    return INDX2(INDX(E, I), J, K);
}

pExpr INDX21(pExpr E,
             tIndex I,
             tIndex J,
             tIndex K)
{
    /* INDX2s E with I,J, then indexes result by K. */

    return INDX(INDX2(E, I, J), K);
}

pExpr INDX22(
             pExpr E,
             tIndex I,
             tIndex J,
             tIndex K,
             tIndex L)
{
    /* INDX2s E with I,J, then INDX2s the result with K,L. This */
    /* will only work with a 2-d array (or mat) of matrices.    */

    return INDX2(INDX2(E, I, J), K, L);
}
