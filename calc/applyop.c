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

static pExpr DO_BIN_OP(tBinaryOperator Op,
                       pExpr E1,
                       pExpr E2)
{
    switch (Op) {
        case cAdd:
            return DO_ADD(E1, E2);
        case cSubtract:
            return DO_SUB(E1, E2);
        case cMultiply:
            return DO_MUL(E1, E2);
        case cDivide:
            return DO_DVD(E1, E2);
        default:
            fprintf(stderr, "DO_BIN_OP: unknown op %d\n", Op);
            abort();
            /*NOTREACHED*/
    }
}

/*=======*/
/* UN_OP */
/*=======*/

pExpr UN_OP(tUnaryOperator Op,
            pExpr E)
{
    /* Applies the supplied unary op to the supplied expression. */

    switch (Op) {
        case cNegate:
            return NEG(E);
        default:
            fprintf(stderr, "UN_OP: unknown op %d\n", Op);
            abort();
            /*NOTREACHED*/
    }
} /*UN_OP*/

/*========*/
/* BIN_OP */
/*========*/

pExpr BIN_OP(
             tBinaryOperator Op,
             pExpr E1,
             pExpr E2)
{
    /* This applies the supplied binary operator to the supplied expressions. */

    switch (Op) {
        case cAdd:
            return ADD(E1, E2);
        case cSubtract:
            return SUB(E1, E2);
        case cMultiply:
            return MUL(E1, E2);
        case cDivide:
            return DVD(E1, E2);
        case cDot:
            return DOT(E1, E2);
        default:
            fprintf(stderr, "BIN_OP: unknown op %d\n", Op);
            abort();
            /*NOTREACHED*/
    }
}

/* TER_OP
 *
 * Applies the supplied unary op to the supplied expressions.
 */
pExpr TER_OP(tTernaryOperator Op,
             pExpr E1,
             pExpr E2,
             pExpr E3)
{
    switch (Op) {
        case cIfThenElse:
            return QUES(E1,E2,E3);
        default:
            fprintf(stderr, "TER_OP: unknown op %d\n", Op);
            abort();
            /*NOTREACHED*/
    }
}

/*===========*/
/* COPY_EXPR */
/*===========*/

pExpr COPY_EXPR(pExpr E)
{
    /* Returns a copy of the expression E.  The entire subtree */
    /* under E is copied if necessary (i.e. not Permanent).    */
    /* E is not disposed here.  The protection of the newly    */
    /* created expression returned is cTemporary.              */
    register pExpr X;
    register tIndex i, j;

    if (!E)
        return NULL;
    if (E->Protection == cPermanent)
        return E;

    switch (E->NodeKind) {
        case cScalarConstNode:
            return SC(E->ScalarValue);
        case cVarRefNode:
            X = NEWX(cVarRefNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->VarRef = E->VarRef;
            X->NumIndices = E->NumIndices;
            for (i = 0; i < X->NumIndices; i++)
                X->Indices[i] = E->Indices[i];
            X->AssignCountE = E->AssignCountE;
            break;
        case cFunctionCallNode:
            X = NEWX(cFunctionCallNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->FuncVarRef = E->FuncVarRef;
            X->FuncCallParm = COPY_EXPR(E->FuncCallParm);
            break;
        case cFunction2CallNode:
            X = NEWX(cFunction2CallNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->Func2VarRef = E->Func2VarRef;
            X->Func2CallParm1 = COPY_EXPR(E->Func2CallParm1);
            X->Func2CallParm2 = COPY_EXPR(E->Func2CallParm2);
            break;
        case cArray1dNode:
            X = MAKE_EXPR_LIKE(E);
            for (i = 0; i < LEN1d(X); i++)
                X->Array1dValue[i] = COPY_EXPR(E->Array1dValue[i]);
            break;
        case cArray2dNode:
            X = MAKE_EXPR_LIKE(E);
            for (i = 0; i < LEN1d(X); i++)
                for (j = 0; j < LEN2d(X); j++)
                    S2d(X, i, j, COPY_EXPR(G2d(E, i, j)));
            break;
        case cUnaryOperatorNode:
            X = NEWX(cUnaryOperatorNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->UnOp = E->UnOp;
            X->Opnd = COPY_EXPR(E->Opnd);
            break;
        case cBinaryOperatorNode:
            X = NEWX(cBinaryOperatorNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->BinOp = E->BinOp;
            X->LeftOpnd = COPY_EXPR(E->LeftOpnd);
            X->RtOpnd = COPY_EXPR(E->RtOpnd);
            break;
        case cTernaryOperatorNode:
            X = NEWX(cTernaryOperatorNode, 0, 0);
            X->NodeValueType = E->NodeValueType;
            X->TerOp = E->TerOp;
            X->FirstOpnd  = COPY_EXPR(E->FirstOpnd);
            X->SecondOpnd = COPY_EXPR(E->SecondOpnd);
            X->ThirdOpnd  = COPY_EXPR(E->ThirdOpnd);
            break;
        default:
            fatal("COPY_EXPR: bad node kind.");
            /*NOTREACHED*/
    } /*case*/

    return X;
}

/* EXPR_COST 
 *
 * Returns a number proportional to the cost of computing the given expr.
 * XXX Sine and cosine calls just count one since they are currently all
 * abbreviated when printed.  Other functions cost 10.
 */

long EXPR_COST(pExpr E)
{
    register tIndex Cnt, i, j;

    Cnt = 0;
    if (E)
        switch (E->NodeKind) {
            case cUnaryOperatorNode:
                Cnt = 2 + EXPR_COST(E->Opnd);
                break;
            case cBinaryOperatorNode:
                Cnt = 2 + EXPR_COST(E->LeftOpnd) + EXPR_COST(E->RtOpnd);
                if (E->BinOp == cDivide)
                    Cnt += 2;  /* says divide costs 2 times as much as + or * */
                break;
            case cTernaryOperatorNode:
                Cnt = 2 + EXPR_COST(E->FirstOpnd) + EXPR_COST(E->SecondOpnd)
                        + EXPR_COST(E->ThirdOpnd);
                break;
            case cScalarConstNode:
                Cnt = 0;
                break;
            case cArray1dNode:
                for (i = 0; i < E->NodeValueType.Dim1; i++)
                    Cnt += EXPR_COST(E->Array1dValue[i]);
                break;
            case cArray2dNode:
                for (i = 0; i < E->NodeValueType.Dim1; i++)
                    for (j = 0; j < E->NodeValueType.Dim2; j++)
                        Cnt += EXPR_COST(G2d(E, i, j));
                break;
            case cVarRefNode:
                Cnt = 1;
                break;
            case cFunctionCallNode:
                if (E->FuncVarRef->WhichFunction == cSine
                    || E->FuncVarRef->WhichFunction == cCosine)
                    Cnt = 1;
                else
                    Cnt = 10;
                break;
            case cFunction2CallNode:
                Cnt = 10;
                break;
        }

    return Cnt;
}

/*=============*/
/* APPLYBIN_OP */
/*=============*/

pExpr APPLYBIN_OP(tBinaryOperator Op,
                  register pExpr E1,
                  register pExpr E2)
{
    /* Applies the given binary operator Op to the expressions E1 and E2.  */
    /* May make several trial runs if E1 and/or E2 is a variable reference */
    /* to a variable that has a remembered value.                          */
    /* This is a generic routine used by ADD,SUB,MUL and DVD to allow them */
    /* all to make similar trials and pick the best result.                */
    pExpr E1R, E2R, best, trial, temp;
    long bcost, trcost;
    register tIndex i;

    E1R = NULL;
    E2R = NULL;
    if (IS_VREF(E1)) {
        temp = E1->VarRef->RememberedVal;
        if (temp) {
            for (i = 0; i < E1->NumIndices; i++)
                temp = INDX(temp, E1->Indices[i]);
            if (!SAME_EXPR(E1,temp))
                E1R = INUSE(COPY_EXPR(temp));
            DISPOSE_EXPR(temp);
        }
    }
    if (IS_VREF(E2)) {
        temp = E2->VarRef->RememberedVal;
        if (temp) {
            for (i = 0; i < E2->NumIndices; i++)
                temp = INDX(temp, E2->Indices[i]);
            if (!SAME_EXPR(E2,temp))
                E2R = INUSE(COPY_EXPR(temp));
            DISPOSE_EXPR(temp);
        }
    }

    if (E1R || E2R) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        /* best is least cost expr seen so far */
        best = INUSE(DO_BIN_OP(Op, E1, E2));
        bcost = EXPR_COST(best);        /* bcost is cost of best */

        if (E1R) {
            trial = BIN_OP(Op, E1R, E2);
            trcost = EXPR_COST(trial);
            if (trcost < bcost) {
                DISPOSE_EXPR(UNUSE(best));
                best = INUSE(trial);
                bcost = trcost;
            } else
                DISPOSE_EXPR(trial);

            if (E2R) {
                /* both E1 and E2 are vars with remembered values */
                trial = BIN_OP(Op, E1R, E2R);
                trcost = EXPR_COST(trial);
                if (trcost < bcost) {
                    DISPOSE_EXPR(UNUSE(best));
                    best = INUSE(trial);
                    bcost = trcost;
                } else
                    DISPOSE_EXPR(trial);
            }
        }

        if (E2R) {
            /* Try E1 <Op> remembered(E2) */
            trial = BIN_OP(Op, E1, E2R);
            trcost = EXPR_COST(trial);
            if (trcost < bcost) {
                DISPOSE_EXPR(UNUSE(best));
                best = INUSE(trial);
                bcost = trcost;
            } else
                DISPOSE_EXPR(trial);
        }

        DISPOSE_EXPR(UNUSE(E1));
        DISPOSE_EXPR(UNUSE(E2));
        DISPOSE_EXPR(UNUSE(E1R));
        DISPOSE_EXPR(UNUSE(E2R));
        best = UNUSE(best);
    } else        /* neither E1 nor E2 is a vref with a remembered val */
        best = DO_BIN_OP(Op, E1, E2);

    return best;
}
