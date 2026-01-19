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

/* This module contains function related to logical calculations.
 */

#include "calc.h"
#include "calcprot.h"
#include "language.h"

/* IFTHEN
 * IFELSE
 * IFEND
 *
 * Generate if-then-else statements, if required.  Each of these routines
 * is passed the same logical expression `cond'.  If `cond' is a constant
 * then these routines do no output.  Otherwise, output is as follows 
 * (adjusted for language, of course):
 *
 *      IFTHEN => if ( cond ) {
 *      IFELSE => } else {
 *      IFEND  => }
 *
 * In addition, IFTHEN and IFELSE return 1 or 0 as their
 * functional values.  If cond is a constant, IFTHEN returns cond
 * and IFELSE returns !cond.  Otherwise, both routines return 1.
 * (Actually, for constants they return 0 if cond==0, 1 otherwise.)
 * In addition, each routine returns an indication of what the other
 * routine will return, to allow the enclosed code to be sensitive
 * to the case where both pieces of code are generated.  In that case,
 * ASSIGN'ed variables will have meaningless values, since at run time
 * only one of the sections of code will be executed.
 *
 * The odd definitions of these routines allow one to write the following
 * amazing code:
 *
 *      cond = <some boolean expression>
 *      if (IFTHEN(F, cond, &elsetoo)) {
 *          <1: code to be generated when cond is true>
 *      }
 *      if (IFELSE(F, cond, &thentoo)) {
 *          <2: code to be generated when cond is false>
 *      }
 *      IFEND(F, cond);
 *
 * If cond can be determined at generation time, this will simply generate
 * either the <1:...> clause or the <2:...> clause, without any of the
 * `if' stuff.  Otherwise, both the <1:...> and <2:...> clause will come
 * out, with the appropriate run-time tests.
 */
int
IFTHEN(FILE *F,
       pExpr cond,
       int *elsetoo)
{
    if (IS_CONST(cond)) {
        *elsetoo = IS_ZERO(cond);
        return !IS_ZERO(cond);
    }

    *elsetoo = 1;

    efprintf(F, Lang->stmt_if2_b);
    PRINT_E(F, cond);
    efprintf(F, Lang->stmt_if2_then, "", "");
    return 1;
}

int
IFELSE(FILE *F,
       pExpr cond,
       int  *thentoo)
{
    if (IS_CONST(cond)) {
        *thentoo = !IS_ZERO(cond);
        return IS_ZERO(cond);
    }

    *thentoo = 1;

    efprintf(F, Lang->stmt_if2_else);
    return 1;
}

void
IFEND(FILE *F,
      pExpr cond)
{
    if (!IS_CONST(cond))
        efprintf(F, Lang->stmt_if2_e);
}

/* AND
 * Boolean `and' function.
 * We'll try to short-circuit if possible, but there can be no
 * guarantee.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
AND(register pExpr E1,
    register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "AND");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("AND: both expressions must be scalars");

    if (IS_CONST(E1)) {
        if (IS_ZERO(E1))
            E = SCALAR_ZERO();
        else 
            E = E2;
    } else if (IS_CONST(E2)) {
        if (IS_ZERO(E2))
            E = SCALAR_ZERO();
        else
            E = E1;
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cLogicalAnd;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* OR
 * Boolean `or' function.
 * We'll try to short-circuit if possible, but there can be no
 * guarantee.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
OR(register pExpr E1,
   register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "OR");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("OR: both expressions must be scalars");

    if (IS_CONST(E1)) {
        if (!IS_ZERO(E1))
            E = SCALAR_ONE();
        else 
            E = E2;
    } else if (IS_CONST(E2)) {
        if (!IS_ZERO(E2))
            E = SCALAR_ONE();
        else
            E = E1;
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cLogicalOr;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* NOT
 * Boolean `not' function.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expression.
 */
pExpr
NOT(register pExpr E1)
{
    register pExpr E;

    C_ASSERT(E1 != NULL, 1, "NOT");
    if (!(IS_SCALAR(E1)))
        fatal("NOT: expression must be scalar");

    if (IS_CONST(E1)) {
        if (IS_ZERO(E1))
            E = SCALAR_ONE();
        else 
            E = SCALAR_ZERO();
    } else {
        E = NEWX(cUnaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->UnOp = cLogicalNot;
        E->Opnd = E1;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    return UNUSE(E);
}

/* EQUAL
 * Boolean `equal' function.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
EQUAL(register pExpr E1,
      register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "EQUAL");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("EQUAL: both expressions must be scalars");

    /* When SAME_EXPR returns false, that does not always guarantee that
       the two expressions are not equal.  For constants it does, though. */
    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ONE(); /* they're equal */
    else if (IS_CONST(E1) && IS_CONST(E2))
            E =  SCALAR_ZERO();
    else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cIsEqual;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* NOTEQUAL
 * Boolean `not equal' function.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
NOTEQUAL(register pExpr E1,
         register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "NOTEQUAL");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("NOTEQUAL: both expressions must be scalars");

    /* When SAME_EXPR returns false, that does not always guarantee that
       the two expressions are not equal.  For constants it does, though. */
    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ZERO(); /* they're not notequal */
    else if (IS_CONST(E1) && IS_CONST(E2))
            E =  SCALAR_ONE();  /* they're definitely not equal */
    else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cIsEqual;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* LESSTHAN
 * Boolean `less than' function -- returns true if E1 < E2.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
LESSTHAN(
         register pExpr E1, 
         register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "LESSTHAN");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("LESSTHAN: both expressions must be scalars");

    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ZERO(); /* they're equal (not less than) */
    else if (IS_CONST(E1) && IS_CONST(E2)) {
        if (NUMVAL(E1) < NUMVAL(E2))
            E = SCALAR_ONE();
        else E = SCALAR_ZERO();
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cLessThan;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* GREATERTHAN
 * Boolean `greater than' function -- returns true if E1 > E2.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
GREATERTHAN(register pExpr E1,
            register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "GREATERTHAN");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("GREATERTHAN: both expressions must be scalars");

    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ZERO(); /* they're equal (not greater than) */
    else if (IS_CONST(E1) && IS_CONST(E2)) {
        if (NUMVAL(E1) > NUMVAL(E2))
            E = SCALAR_ONE();
        else E = SCALAR_ZERO();
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cGreaterThan;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* LESSOREQ
 * Boolean `less than or equal to' function -- returns true if E1 <= E2.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
LESSOREQ(register pExpr E1, 
         register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "LESSOREQ");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("LESSOREQ: both expressions must be scalars");

    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ONE(); /* they're equal */
    else if (IS_CONST(E1) && IS_CONST(E2)) {
        if (NUMVAL(E1) <= NUMVAL(E2))
            E = SCALAR_ONE();
        else E = SCALAR_ZERO();
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cLessOrEq;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* GREATEROREQ
 * Boolean `greater than or equal to' function -- returns true if E1 >= E2.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
GREATEROREQ(register pExpr E1,
            register pExpr E2)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "GREATEROREQ");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("GREATEROREQ: both expressions must be scalars");

    if (SAME_EXPR(E1,E2))
        E =  SCALAR_ONE(); /* they're equal */
    else if (IS_CONST(E1) && IS_CONST(E2)) {
        if (NUMVAL(E1) >= NUMVAL(E2))
            E = SCALAR_ONE();
        else E = SCALAR_ZERO();
    } else {
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cGreaterOrEq;
        E->LeftOpnd = E1;
        E->RtOpnd = E2;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* NEARTO
 * Boolean `nearly equal to' function -- returns true if 
 * ABS(E1-E2) <= howClose.
 * This only works on scalars.
 * An attempt will be made to DISPOSE the passed-in expressions.
 */
pExpr
NEARTO(register pExpr E1, 
       register pExpr E2,
       double howClose)
{
    register pExpr E;

    C_ASSERT(E1 && E2, 1, "NEARTO");
    if (!(IS_SCALAR(E1) && IS_SCALAR(E2)))
        fatal("NEARTO: both expressions must be scalars");

    if (SAME_EXPR(E1,E2))
        E = SCALAR_ONE(); /* they're equal */
    else {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = LESSOREQ(ABS(SUB(E1,E2)),SC(howClose));
        E1 = UNUSE(E1);
        E2 = UNUSE(E2);
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    return UNUSE(E);
}

/* QUES
 * `Question mark' expression, like C's E1 ? E2 : E3.
 * E1 is treated as a logical expression.  The types of E2 and E3
 * must be equal.
 * An attempt will be made to DISPOSE all the passed-in expressions.
 */
pExpr
QUES(register pExpr E1,
     register pExpr E2,
     register pExpr E3)
{
    register pExpr E;

    C_ASSERT(E1 && E2 && E3, 1, "QUES");
    if (!SAME_TYPE(&E2->NodeValueType,&E3->NodeValueType))
        fatal("QUES: selected expressions must be of the same type");

    if (IS_ONE(E1))
        E = E2;
    else if (IS_ZERO(E1))
        E = E3;
    else if (SAME_EXPR(E2, E3))
        E = E2;
    else {
        E = NEWX(cTernaryOperatorNode, 0, 0);
        E->NodeValueType = E2->NodeValueType;
        E->TerOp = cIfThenElse;
        E->FirstOpnd = E1;
        E->SecondOpnd = E2;
        E->ThirdOpnd = E3;
    }

    E = INUSE(E);
    DISPOSE_EXPR(E1);
    DISPOSE_EXPR(E2);
    DISPOSE_EXPR(E3);
    return UNUSE(E);
}

/* QUESDVD
 * `Question mark' expression, like C's E1 ? E2 : (E3/E4).
 * This is the same as QUES except that we avoid
 * performing the divide if E1 is a constant and true.
 * This is to avoid generation-time divides by zero.
 * See QUES above for more info.
 * An attempt will be made to DISPOSE all the passed-in expressions.
 */
pExpr
QUESDVD(register pExpr E1,
        register pExpr E2,
        register pExpr E3,
        register pExpr E4)
{
    register pExpr E;

    C_ASSERT(E1 && E2 && E3 && E4, 1, "QUESDVD");
    if (!SAME_TYPE(&E2->NodeValueType,&E3->NodeValueType))
        fatal("QUES: selected expressions must be of the same type");

    if (IS_ONE(E1)) {
        E = INUSE(E2);
        DISPOSE_EXPR(E1);
        DISPOSE_EXPR(E3);
        DISPOSE_EXPR(E4);
        E = UNUSE(E);
    } else
        E = QUES(E1,E2,DVD(E3,E4));

    return E;
}

/* REMOVE_QUES
 *
 * This routine processes the input expression to remove all "QUES"
 * expressions, that is, all embedded if statements.  An equivalent
 * expression is returned in which these if statements are replaced
 * by a reference to a variable which holds the outcome of the if.
 * This may cause output of one or more statements of the form:
 *
 *           if ( cond )
 *               temp[i] = <true expression>
 *           else
 *               temp[i] = <false expression>
 *           end if
 *
 * Then the returned expression will contain references to temp[i].  
 * i is chosen starting with NextTemp and incrementing.  Highest temp used 
 * is returned in HighestTemp.
 *
 * An attempt is made to dispose the original expression.
 */
pExpr 
REMOVE_QUES(FILE *F,
            pExpr E,
            int NextTemp,
            int *HighestTemp)
{
    register tIndex i, tempno;
    register pExpr NoIfExpr;
    int      elsetoo,thentoo;
    pExpr    e1,e2,e3;

    *HighestTemp = NextTemp - 1;        /* none used yet */
    E = INUSE(E);

    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            NoIfExpr = NEWX(cUnaryOperatorNode, 0, 0);
            NoIfExpr->NodeValueType = E->NodeValueType;
            NoIfExpr->UnOp = E->UnOp;
            NoIfExpr->Opnd = 
              REMOVE_QUES(F, E->Opnd, *HighestTemp+1, HighestTemp);
            break;

        case cBinaryOperatorNode:
            NoIfExpr = NEWX(cBinaryOperatorNode, 0, 0);
            NoIfExpr->NodeValueType = E->NodeValueType;
            NoIfExpr->BinOp = E->BinOp;
            NoIfExpr->LeftOpnd = 
                REMOVE_QUES(F, E->LeftOpnd, *HighestTemp+1, HighestTemp);
            NoIfExpr->RtOpnd = 
                REMOVE_QUES(F, E->RtOpnd, *HighestTemp+1, HighestTemp);
            break;
        
        case cTernaryOperatorNode:
            switch(E->TerOp) {
                case cIfThenElse:
                    tempno = ++(*HighestTemp);
                    if (*HighestTemp >= gMaxTemps-1)
                        fatal("REMOVE_QUES: ran out of temps");

                    e1 = REMOVE_QUES(F, E->FirstOpnd, 
                                     *HighestTemp+1, HighestTemp);
                    if (IFTHEN(F, e1, &elsetoo)) {
                        e2 = REMOVE_QUES(F, E->SecondOpnd, 
                                         *HighestTemp+1, HighestTemp);
                        PRINT_ASSN1(F, PRINTNAME(gTempSym), tempno, e2);
                        DISPOSE_EXPR(e2);
                    }
                    if (IFELSE(F, e1, &thentoo)) {
                        e3 = REMOVE_QUES(F, E->ThirdOpnd, 
                                     *HighestTemp+1, HighestTemp);
                        PRINT_ASSN1(F, PRINTNAME(gTempSym), tempno, e3);
                        DISPOSE_EXPR(e3);
                    }
                    IFEND(F, e1);
                    DISPOSE_EXPR(e1);
                    NoIfExpr = VREF1(gTempSym, tempno);
                    break;

                default:
                    NoIfExpr = NEWX(cTernaryOperatorNode, 0, 0);
                    NoIfExpr->NodeValueType = E->NodeValueType;
                    NoIfExpr->TerOp = E->TerOp;
                    NoIfExpr->FirstOpnd = 
                        REMOVE_QUES(F, E->FirstOpnd, 
                                    *HighestTemp+1, HighestTemp);
                    NoIfExpr->SecondOpnd = 
                        REMOVE_QUES(F, E->SecondOpnd, 
                                    *HighestTemp+1, HighestTemp);
                    NoIfExpr->ThirdOpnd = 
                        REMOVE_QUES(F, E->ThirdOpnd, 
                                    *HighestTemp+1, HighestTemp);
                    break;
            }
            break;
           
        case cScalarConstNode:
            NoIfExpr = SC(NUMVAL(E));
            break;

        case cArray1dNode:
            NoIfExpr = E->NodeValueType.ValueType == cVectorVal
              ? NEW_VECX(E->NodeValueType.BaseType)
              : NEW_1dARRAY(E->NodeValueType.BaseType, E->NodeValueType.Dim1);
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                SINDX(NoIfExpr, i, REMOVE_QUES(F, INDX(E,i), 
                                        *HighestTemp+1, HighestTemp));
            break;

        case cArray2dNode:
            NoIfExpr = E->NodeValueType.ValueType == cMatrixVal
              ? NEW_MATX(E->NodeValueType.BaseType)
              : NEW_2dARRAY(E->NodeValueType.BaseType,
                            E->NodeValueType.Dim1, E->NodeValueType.Dim2);
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                SINDX(NoIfExpr, i, REMOVE_QUES(F, INDX(E,i), 
                                        *HighestTemp+1, HighestTemp));
            break;

        case cVarRefNode:
            NoIfExpr = COPY_EXPR(E);
            break;

        case cFunctionCallNode:
            NoIfExpr = CALL_FUNC(E->FuncVarRef->WhichFunction,
                                 REMOVE_QUES(F, E->FuncCallParm, 
                                                   *HighestTemp+1, HighestTemp));
            break;

        case cFunction2CallNode:
            e1 = REMOVE_QUES(F, E->Func2CallParm1, *HighestTemp+1, HighestTemp);
            e2 = REMOVE_QUES(F, E->Func2CallParm2, *HighestTemp+1, HighestTemp);
            NoIfExpr = CALL_FUNC2(E->Func2VarRef->WhichFunction, e1, e2);
            break;
    }

    NoIfExpr = INUSE(NoIfExpr);
    DISPOSE_EXPR(UNUSE(E));

    return UNUSE(NoIfExpr);
}
