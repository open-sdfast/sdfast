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

/*===================================*/
/* Boolean functions of expressions. */
/*===================================*/

/*==========*/
/* IS_CONST */
/*==========*/

MMBOOL IS_CONST(pExpr E)
{
    /* Returns true if the supplied expression node is a */
    /* constant.                                         */

    C_ASSERT(E != NULL, 1, "IS_CONST");
    return E->NodeKind == cScalarConstNode;
}

/*===========*/
/* IS_SCALAR */
/*===========*/

MMBOOL IS_SCALAR(pExpr E)
{
    /* true iff E is a scalar expression */

    C_ASSERT(E != NULL, 1, "IS_SCALAR");
    return E->NodeValueType.ValueType == cScalarVal;
}

/*==========*/
/* IS_NEGOP */
/*==========*/

MMBOOL IS_NEGOP(pExpr E)
{
    /* true iff E is a NEGation operator node */

    return E->NodeKind == cUnaryOperatorNode && E->UnOp == cNegate;
}

/*========*/
/* IS_NEG */
/*========*/

MMBOOL IS_NEG(pExpr E)
{
    /* true if E is a NEGOP, or a negative scalar constant */

    /* first part is same as IS_NEGOP */
    return E->NodeKind == cUnaryOperatorNode && E->UnOp == cNegate ||
      E->NodeKind == cScalarConstNode && E->ScalarValue < 0;
}

/*========*/
/* IS_MUL */
/*========*/

MMBOOL IS_MUL(pExpr E)
{
    /* true if E is a product (multiplication node) */

    return E->NodeKind == cBinaryOperatorNode && E->BinOp == cMultiply;
}

/*========*/
/* IS_DVD */
/*========*/

MMBOOL IS_DVD(pExpr E)
{
    /* true if E is a quotient (division node) */

    return E->NodeKind == cBinaryOperatorNode && E->BinOp == cDivide;
}

/*========*/
/* IS_ADD */
/*========*/

MMBOOL IS_ADD(pExpr E)
{
    /* true if E is a sum (addition node) */

    return E->NodeKind == cBinaryOperatorNode && E->BinOp == cAdd;
}

/*========*/
/* IS_SUB */
/*========*/

MMBOOL IS_SUB(pExpr E)
{
    /* true if E is a difference (subtraction) node */

    return E->NodeKind == cBinaryOperatorNode && E->BinOp == cSubtract;
}

/*=========*/
/* IS_VREF */
/*=========*/

MMBOOL IS_VREF(pExpr E)
{
    /* Returns true iff E is a variable reference.  An         */
    /*         indexed variable reference counts.              */

    C_ASSERT(E != NULL, 1, "IS_VREF");
    return E->NodeKind == cVarRefNode;
}

/*===========*/
/* SAME_TYPE */
/*===========*/

MMBOOL SAME_TYPE(
          struct tExprType *T1,
          struct tExprType *T2)
{
    /* Compare the types.  Value & Base type and dimensions */
    /* must match.                                          */

    return T1->ValueType == T2->ValueType &&
      T1->BaseType == T2->BaseType &&
      T1->Dim1 == T2->Dim1 &&
      T1->Dim2 == T2->Dim2;
}

/*===========*/
/* SAME_VREF */
/*===========*/

MMBOOL SAME_VREF(register pExpr V1,
          register pExpr V2)
{
    /* Returns true if both V1 and V2 are references to the */
    /* same element of the same variable.                   */
    /* It's an error if neither V1 nor V2 is a VREF.        */
    register tIndex I;

    if (IS_VREF(V1)) {
        if (!IS_VREF(V2))
            return 0;
    } else if (IS_VREF(V2))
        return FALSE;
    else
        fatal("SAME_VREF: expected at least one VREF.");
    if (V1->VarRef == V2->VarRef &&
      V1->NumIndices == V2->NumIndices) {
        for (I = 0; I < V1->NumIndices; I++)
            if (V1->Indices[I] != V2->Indices[I])
                return FALSE;
        return TRUE;
    } else
        return FALSE;
}        /* SAME_VREF */

/*===========*/
/* SAME_FUNC */
/*===========*/

MMBOOL SAME_FUNC(pSym F,
          pSym G)
{
    /* true if the supplied symbols represent the same functions */

    C_ASSERT(F->SymbolKind == cFunctionSym && G->SymbolKind == cFunctionSym,
      1, "SAME_FUNC");
    if (F->WhichFunction != G->WhichFunction)
        return FALSE;
    else
        return SAME_TYPE(&F->SymValueType, &G->SymValueType);
}

/*===========*/
/* SAME_EXPR */
/*===========*/

MMBOOL SAME_EXPR(pExpr E, 
          pExpr F )        /* probably slower if register due to recursion */
{
    /* The only thing this function is guaranteed to do is return false if */
    /* E and F represent different expressions.  Sometimes it will even    */
    /* return true if E and F represent the same expression.               */
    /*   It will always tell you if two constant expressions have equal    */
    /* values.                                                             */
    tIndex i, j;

    if (E->NodeKind != F->NodeKind ||
      !SAME_TYPE(&E->NodeValueType, &F->NodeValueType))
        return FALSE;
    switch (E->NodeKind) {        /* also F->NodeKind */
        case cUnaryOperatorNode:
            return E->UnOp == F->UnOp && SAME_EXPR(E->Opnd, F->Opnd);
        case cBinaryOperatorNode:
            return E->BinOp == F->BinOp &&
              SAME_EXPR(E->LeftOpnd, F->LeftOpnd) &&
              SAME_EXPR(E->RtOpnd, F->RtOpnd);
        case cTernaryOperatorNode:
            return E->TerOp == F->TerOp &&
              SAME_EXPR(E->FirstOpnd, F->FirstOpnd) &&
              SAME_EXPR(E->SecondOpnd, F->SecondOpnd) &&
              SAME_EXPR(E->ThirdOpnd, F->ThirdOpnd);
        case cScalarConstNode:
            return E->ScalarValue == F->ScalarValue;
        case cArray1dNode:
            for (i = 0; i < E->NodeValueType.Dim1; i++)        /* also F->..Dim1 */
                if (!SAME_EXPR(E->Array1dValue[i], F->Array1dValue[i]))
                    return FALSE;
            break;
        case cArray2dNode:
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                for (j = 0; j < E->NodeValueType.Dim2; j++)
                    if (!SAME_EXPR(G2d(E, i, j), G2d(F, i, j)))
                        return FALSE;
            break;
        case cVarRefNode:
            return SAME_VREF(E, F);
        case cFunctionCallNode:
            return SAME_FUNC(E->FuncVarRef, F->FuncVarRef) &&
              SAME_EXPR(E->FuncCallParm, F->FuncCallParm);
        case cFunction2CallNode:
            return SAME_FUNC(E->Func2VarRef, F->Func2VarRef) &&
                   SAME_EXPR(E->Func2CallParm1, F->Func2CallParm1) &&
                   SAME_EXPR(E->Func2CallParm2, F->Func2CallParm2);
        default:
            fatal("SAME_EXPR: unrecognized expression type.");
    }
    return TRUE;
}        /* SAME_EXPR */

/*=========*/
/* IS_ZERO */
/*=========*/

MMBOOL IS_ZERO(pExpr E)        /* probably slower if register due to recursion */
{
    /* Returns true if the supplied expression node is a */
    /* constant, and is all zero.                        */
    tIndex I, J;

    switch (E->NodeKind) {
        case cScalarConstNode:
            return E->ScalarValue == 0.; /* must check for exact 0 or all
                                            hell will break loose */
        case cArray1dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                if (!IS_ZERO(E->Array1dValue[I]))
                    return FALSE;
            break;
        case cArray2dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                for (J = 0; J < E->NodeValueType.Dim2; J++)
                    if (!IS_ZERO(G2d(E, I, J)))
                        return FALSE;
            break;
        default:
            return FALSE;
    }        /* switch */
    return TRUE;
}        /* IS_ZERO */

/* IS_NRZERO
 * Check for an expression all of whose values are very NEAR zero.
 */
MMBOOL IS_NRZERO(pExpr E)        /* probably slower if register due to recursion */
{
    /* Returns true if the supplied expression node is a */
    /* constant, and all values are near zero.           */
    tIndex I, J;

    switch (E->NodeKind) {
        case cScalarConstNode:
            return fabs(E->ScalarValue) <= cVeryClose;
        case cArray1dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                if (!IS_NRZERO(E->Array1dValue[I]))
                    return FALSE;
            break;
        case cArray2dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                for (J = 0; J < E->NodeValueType.Dim2; J++)
                    if (!IS_NRZERO(G2d(E, I, J)))
                        return FALSE;
            break;
        default:
            return FALSE;
    }        /* switch */
    return TRUE;
}

/*========*/
/* IS_ONE */
/*========*/

MMBOOL IS_ONE(pExpr E)
{
    /* Returns true only if E is the scalar constant expression */
    /* whose value is one.  Doesn't currently look for identity */
    /* matrices.                                                */

    C_ASSERT(E != NULL, 1, "IS_ONE");
    return E->NodeKind == cScalarConstNode && 
           fabs(E->ScalarValue-1.) <= cVeryClose;
}

/*=============*/
/* IS_MINUSONE */
/*=============*/

MMBOOL IS_MINUSONE(pExpr E)
{
    /* Returns true only if E is the scalar constant expression */
    /* whose value is minus one.                                */

    C_ASSERT(E != NULL, 1, "IS_MINUSONE");
    return E->NodeKind == cScalarConstNode && 
           fabs(E->ScalarValue+1.) <= cVeryClose;
}

/*===========*/
/* IS_SIMPLE */
/*===========*/

MMBOOL IS_SIMPLE(register pExpr E)
{
    /* Returns true if the expression is simple enough that it */
    /* would be better to substitute it into other expressions */
    /* than to assign it to a variable and use the variable.   */
    /*   Simple expressions are defined here as scalar         */
    /* constants, any variable reference, or a function call   */
    /* which will become a variable reference, such as         */
    /* sin(Q(4)) which will become S4.  Also, the negation of  */
    /* a simple expression is a simple expression, and a       */
    /* constant times a simple expression is a simple expr.    */
    /*   This routine is used by CLEANX and USEXIF to figure   */
    /* out what they should do with their expressions.         */

    if (IS_NEGOP(E))
        return IS_SIMPLE(E->Opnd);
    if (IS_MUL(E))
        return IS_CONST(E->LeftOpnd) && IS_SIMPLE(E->RtOpnd);
    if (IS_CONST(E) || IS_VREF(E))
        return TRUE;
    if (E->NodeKind == cFunctionCallNode &&
      E->FuncCallParm->NodeKind == cVarRefNode &&
      E->FuncCallParm->NumIndices == 1 &&
      (!strcmp(E->FuncCallParm->VarRef->PrintName, "q") ||
      !strcmp(E->FuncCallParm->VarRef->PrintName, "qbar")) &&
      (E->FuncVarRef->WhichFunction == cSine ||
      E->FuncVarRef->WhichFunction == cCosine))
        return TRUE;
    return FALSE;
}
