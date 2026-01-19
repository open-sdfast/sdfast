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

#include <ctype.h>
#include "calc.h"
#include "calcprot.h"

/* case-insensitive strcmp() */
int strcmp_ci(register char *s1,
              register char *s2)
{
    register int diff;

    while (*s1 || *s2) {
        if (diff = (isupper(*s1) ? tolower(*s1) : *s1) -
          (isupper(*s2) ? tolower(*s2) : *s2))
            return diff;
        s1++; s2++;
    }
    return 0;
}


/*=============*/
/* ORDER_VREF */
/*=============*/

static enum tExprOrder ORDER_VREF(pExpr E,
                                  pExpr F)
{
    /* determine the order in which the two variables should appear in */
    /* a commutative term, as defined in ORDER below.                  */
    long i;

    if ((i = strcmp_ci(E->VarRef->PrintName, F->VarRef->PrintName)) < 0)
        return cBeforeOrder;
    if (i > 0)
        return cAfterOrder;
    /* names are the same -- use ordering of indices */
    if (E->NumIndices < F->NumIndices)
        return cBeforeOrder;
    else if (E->NumIndices > F->NumIndices)
        return cAfterOrder;
    else {
        for (i = 0; i < E->NumIndices; i++)
            if (E->Indices[i] != F->Indices[i]) {
                if (E->Indices[i] < F->Indices[i])
                    return cBeforeOrder;
                else return cAfterOrder;
            }
        return cSameOrder;
    }
}

/*=============*/
/* ORDER_FUNC  */
/*=============*/

static enum tExprOrder ORDER_FUNC(pExpr F,
                                  pExpr G)
{
    /* true if one-arg function F should come before one-arg function G, see below */

    if ((int)F->FuncVarRef->WhichFunction < (int)G->FuncVarRef->WhichFunction)
        return cBeforeOrder;
    else
      if ((int)F->FuncVarRef->WhichFunction > (int)G->FuncVarRef->WhichFunction)
        return cAfterOrder;
    else        /* same function */
        return ORDER(F->FuncCallParm, G->FuncCallParm);
}

/*==============*/
/* ORDER_FUNC2  */
/*==============*/

static enum tExprOrder ORDER_FUNC2(pExpr F,
                                   pExpr G)
{
    /* true if two-arg function F should come before two-arg function G, see below */
    enum tExprOrder p1Order;

    if ((int)F->Func2VarRef->WhichFunction < (int)G->Func2VarRef->WhichFunction)
        return cBeforeOrder;
    else
      if ((int)F->Func2VarRef->WhichFunction > (int)G->Func2VarRef->WhichFunction)
        return cAfterOrder;
    else {        /* same function */
        if ((p1Order = ORDER(F->Func2CallParm1, G->Func2CallParm1)) != cSameOrder)
            return p1Order;
        return ORDER(F->Func2CallParm2, G->Func2CallParm2);
    }
}

/*==========*/
/* ORDER_1d */
/*==========*/

static enum tExprOrder ORDER_1d(register pExpr E,
                                register pExpr F)
{
    register tIndex i;
    enum tExprOrder Xorder;

    if (E->NodeValueType.Dim1 < F->NodeValueType.Dim1)
        return cBeforeOrder;
    else if (E->NodeValueType.Dim1 > F->NodeValueType.Dim1)
        return cAfterOrder;
    else {
        for (i = 0; i < E->NodeValueType.Dim1; i++) {
            if ((Xorder = ORDER(E->Array1dValue[i], F->Array1dValue[i])) !=
              cSameOrder)
                return Xorder;
        }
        return cSameOrder;
    }
}

/*==========*/
/* ORDER_2d */
/*==========*/

static enum tExprOrder ORDER_2d(pExpr E,
                                pExpr F)
{
    long i, j;
    enum tExprOrder Xorder;
    if (E->NodeValueType.Dim1 < F->NodeValueType.Dim1)
        return cBeforeOrder;
    else if (E->NodeValueType.Dim1 > F->NodeValueType.Dim1)
        return cAfterOrder;
    else {
        if (E->NodeValueType.Dim2 < F->NodeValueType.Dim2)
            return cBeforeOrder;
        else if (E->NodeValueType.Dim2 > F->NodeValueType.Dim2)
            return cAfterOrder;
        else {
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                for (j = 0; j < E->NodeValueType.Dim2; j++) {
                    if ((Xorder = ORDER(G2d(E, i, j), G2d(F, i, j))) !=
                      cSameOrder)
                        return Xorder;
                }
            return cSameOrder;
        }
    }
}

/*========*/
/* ORDER  */
/*========*/

enum tExprOrder ORDER(register pExpr E,register pExpr F)
{
   /* To facilitate detection of identical expressions, we define an ordering
      on expressions so that we always have commutative terms expressed the
      same way.  (e.g. we don't want both A+B and B+A to exist.)  The ordering
      between E and F is determined, and we return cBeforeOrder if E should
      come before F, cAfterOrder if E should come after F and cSameOrder if
      E and F are the same.  The ordering is determined by
      applying the following rules in order:
        1 A constant always comes BEFORE anything else.
          If both expressions are constant, use numeric ordering.
        2 A variable reference always comes BEFORE anything else.
          If both are vars, use lexicographic ordering of print names.  If
          same print names, use ordering of indices.
       3a A one-arg function call is BEFORE anything else.
          If both are one-arg function calls, ordering is by ord(WhichFunction).  If
          both are the same function ordering is by ORDER of the parameters.
       3b A two-arg function call is BEFORE anything else.
          If both are two-arg function calls, ordering is by ord(WhichFunction).  If
          both are the same function ordering is by ORDER of the first parameters.
          If those are the same then use ordering of the second parameters.
        4 A non-operator node comes BEFORE operators.
        5 If both are operators use ordering of only, Left, or First operand, 
          followed by ordering of Right or Second operand, followed 
          (for Ternary operators) by the ordering of the Third operand.
        6 A 1d array is "less than" a 2d array. If both are arrays, use      
          ordering of the elements.
    */

    enum tExprOrder Xorder;
    pExpr Eopnd,Fopnd;

    if (IS_CONST(E)) {
        if (IS_CONST(F)) {
            if (E->ScalarValue < F->ScalarValue)
                return cBeforeOrder;
            else if (E->ScalarValue > F->ScalarValue)
                return cAfterOrder;
            else
                return cSameOrder;
        } else
            return cBeforeOrder;
    }
    if (IS_CONST(F))
        return cAfterOrder;
    if (IS_VREF(E))
        return IS_VREF(F) ? ORDER_VREF(E, F) : cBeforeOrder;
    if (IS_VREF(F))
        return cAfterOrder;
    if (E->NodeKind == cFunctionCallNode)
        return F->NodeKind == cFunctionCallNode ? ORDER_FUNC(E, F) :
          cBeforeOrder;
    if (F->NodeKind == cFunctionCallNode)
        return cAfterOrder;
    if (E->NodeKind == cFunction2CallNode)
        return F->NodeKind == cFunction2CallNode ? ORDER_FUNC2(E, F) :
          cBeforeOrder;
    if (F->NodeKind == cFunction2CallNode)
        return cAfterOrder;
    if (E->NodeKind == cArray1dNode)
        return F->NodeKind != cArray1dNode ? cBeforeOrder : ORDER_1d(E, F);
    if (F->NodeKind == cArray1dNode)
        return cAfterOrder;
    if (E->NodeKind == cArray2dNode)
        return F->NodeKind != cArray2dNode ? cBeforeOrder : ORDER_2d(E, F);
    if (F->NodeKind == cArray2dNode)
        return cAfterOrder;

    /* E and F are operators: try to order by first operand */
    Eopnd = E->NodeKind == cUnaryOperatorNode 
            ? E->Opnd 
            : (E->NodeKind == cBinaryOperatorNode
               ? E->LeftOpnd : E->FirstOpnd);
    Fopnd = F->NodeKind == cUnaryOperatorNode 
            ? F->Opnd 
            : (F->NodeKind == cBinaryOperatorNode
               ? F->LeftOpnd : F->FirstOpnd);
    Xorder = ORDER(Eopnd, Fopnd);
    if (Xorder == cSameOrder) {
        if (E->NodeKind == cUnaryOperatorNode)
            return F->NodeKind == cUnaryOperatorNode ?
              cSameOrder : cBeforeOrder; /* unary before binary or ternary */
        else if (F->NodeKind == cUnaryOperatorNode)
            return cAfterOrder;

        /* E and F ar binary or ternary: try second operand */
        Eopnd = E->NodeKind == cBinaryOperatorNode
                ? E->RtOpnd : E->SecondOpnd;
        Fopnd = F->NodeKind == cBinaryOperatorNode
                ? F->RtOpnd : F->SecondOpnd;
        Xorder = ORDER(Eopnd, Fopnd);
        if (Xorder == cSameOrder) {
            if (E->NodeKind == cBinaryOperatorNode)
                return F->NodeKind == cBinaryOperatorNode ?
                  cSameOrder : cBeforeOrder; /* binary before ternary */
            else if (F->NodeKind == cBinaryOperatorNode)
                return cAfterOrder;

            /* E and F are ternary: try third operand */
            Xorder = ORDER(E->ThirdOpnd, F->ThirdOpnd);
        }
    }

    return Xorder;
}
