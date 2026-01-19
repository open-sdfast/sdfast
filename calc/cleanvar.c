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

/*============*/
/* SHARE_EXPR */
/*============*/

static pExpr SHARE_EXPR(pExpr E)
{
    /* Returns a new expression with the same value as E.  The leaf */
    /* expressions are physically the same, while array and matrix  */
    /* descriptors are new.  The INUSE level is bumped for the      */
    /* shared expressions, but the new stuff is temporary.          */
    /* E is not disposed.                                           */
    /*   If E is nil or a scalar, it is just returned.              */
    pExpr X;
    tIndex i, j;

    if (!E)
        return NULL;

    switch (E->NodeValueType.ValueType) {
        case cScalarVal:
            return INUSE(E);        /* here's the only real sharing */
        case cArray1dVal:
        case cVectorVal:
            X = MAKE_EXPR_LIKE(E);
            for (i = 0; i < LEN1d(X); i++)
                X->Array1dValue[i] = SHARE_EXPR(E->Array1dValue[i]);
            break;
        case cArray2dVal:
        case cMatrixVal:
            X = MAKE_EXPR_LIKE(E);
            for (i = 0; i < LEN1d(X); i++)
                for (j = 0; j < LEN2d(X); j++)
                    S2d(X, i, j, SHARE_EXPR(G2d(E, i, j)));
            break;
        default:
            fatal("SHARE_EXPR: bad node type.");
    }        /*case*/

    return X;
}

/*=========*/
/*  CLEANX */
/*=========*/

static int
printout(char PrintAll,
         pExpr X,
         pExpr VX,
         int   *repl)
{
    /* return 1 if we should print out the assignment VX = X */
    /* Set repl to 1 if we should replace X by VX */
    *repl = 0;
    if (!IS_SIMPLE(X)) {
        *repl = 1;
        return 1;
    }
    if (PrintAll) {
       if (SAME_EXPR(X,VX))
           return 0;
       *repl = !IS_CONST(X);
       if (PrintAll == 1 && IS_CONST(X))
           return 0;
       return 1;
    }
    return 0;
}



static pExpr 
CLEANX(
FILE *F,
pExpr X, 
pExpr VX,
char PrintAll)
{
    /* X is a doomed expression which needs "cleaning".  That 
     * means that we replace values of X with VREFs to VX(i),
     * for each element of X.  We don't do this if X(i) is  
     * a const or already a VREF to VX(i).      
     * VX must be a variable reference whose type is the same as X. 
     * X can be nil, in which case we create a new expr.    
     *
     * When we replace an expression by a reference, we output 
     * a line like V= X to be included in the program output. 
     *   If "PrintAll" is 1, we'll output V= X unless X is
     * constant.  If PrintAll is 2, we'll output V= X regardless.
     * In either case, we'll replace the expression with a VREF 
     * unless the expression is a constant or nil.           
     */
    pExpr E, LimitedExpr, NoIfExpr;
    tIndex i, j;
    int HighestTemp, repl;
    pExpr tempXi, tempVXi;

    C_ASSERT(VX != NULL, 1, "CLEANX");
    C_ASSERT(IS_VREF(VX), 2, "CLEANX");
    if (!X)
        return VX;

    switch (VX->NodeValueType.ValueType) {
        case cScalarVal:
            if (printout(PrintAll, X, VX, &repl)) {
                X = INUSE(X);
                    NoIfExpr = INUSE(REMOVE_QUES(F, X, 0, &HighestTemp));
                LimitedExpr = LIMIT_EXPR(F, NoIfExpr, 
                                         HighestTemp+1, &HighestTemp);
                PRINT_VREF(F, VX);
                efprintf(F, "%=");
                PRINT_EXPR(F, LimitedExpr);
                DISPOSE_EXPR(LimitedExpr);
                DISPOSE_EXPR(UNUSE(NoIfExpr));
                X = UNUSE(X);
                if (repl) {
                    DISPOSE_EXPR(X);
                    return VX;
                }
            } 
            DISPOSE_EXPR(VX);
            return X;
            break;

        case cVectorVal:
            E = INUSE(NEW_VECX(cScalarVal));
            X = INUSE(X);
            VX = INUSE(VX);
            for (i = 0; i < 3; i++) {
                tempXi = INDX(X, i);
                tempVXi = INDX(VX, i);
                if (printout(PrintAll, tempXi, tempVXi, &repl)) {
                    tempXi = INUSE(tempXi);
                    NoIfExpr = INUSE(REMOVE_QUES(F, tempXi, 0, &HighestTemp));
                    LimitedExpr = LIMIT_EXPR(F, NoIfExpr, 
                                             HighestTemp+1, &HighestTemp);
                    PRINT_VREF(F, tempVXi);
                    efprintf(F, "%=");
                    PRINT_EXPR(F, LimitedExpr);
                    DISPOSE_EXPR(LimitedExpr);
                    DISPOSE_EXPR(UNUSE(NoIfExpr));
                    tempXi = UNUSE(tempXi);
                    if (repl)
                         SINDX(E, i, INDX(VX, i));
                    else SINDX(E, i, tempXi);
                } else
                    SINDX(E, i, tempXi);
                DISPOSE_EXPR(tempXi);
                DISPOSE_EXPR(tempVXi);
            }
            break;

        case cMatrixVal:
            E = INUSE(NEW_MATX(cScalarVal));
            X = INUSE(X);
            VX = INUSE(VX);
            for (i = 0; i < 3; i++)
                for (j = 0; j < 3; j++) {
                    tempXi = INDX2(X, i, j);
                    tempVXi = INDX2(VX, i, j);
                    if (printout(PrintAll, tempXi, tempVXi, &repl)) {
                        tempXi = INUSE(tempXi);
                        NoIfExpr = 
                            INUSE(REMOVE_QUES(F, tempXi, 0, &HighestTemp));
                        LimitedExpr = LIMIT_EXPR(F, NoIfExpr, 
                                                 HighestTemp+1, &HighestTemp);
                        PRINT_VREF(F, tempVXi);
                        efprintf(F, "%=");
                        PRINT_EXPR(F, LimitedExpr);
                        DISPOSE_EXPR(LimitedExpr);
                        DISPOSE_EXPR(UNUSE(NoIfExpr));
                        tempXi = UNUSE(tempXi);
                        if (repl) 
                             SINDX2(E, i, j, INDX2(VX, i, j));
                        else SINDX2(E, i, j, tempXi);
                    } else
                        SINDX2(E, i, j, tempXi);
                    DISPOSE_EXPR(tempXi);        
                    DISPOSE_EXPR(tempVXi);
                }
            break;
    }
    DISPOSE_EXPR(UNUSE(X));
    DISPOSE_EXPR(UNUSE(VX));
    return UNUSE(E);
}

/*===========*/
/* CLEANSC   */
/*===========*/

static void CLEANSC(
               FILE *F,
               pSym V,
               char PrintAll)
{
    /* If V has no value, or V is not a constant, we set */
    /* its value to VREF(V).                             */

    C_ASSERT(V != NULL, 1, "CLEANSC");
    C_ASSERT(V->SymbolKind == cVariableSym, 2, "CLEANSC");
    C_ASSERT(V->SymValueType.ValueType == cScalarVal, 3, "CLEANSC");
    V->SymValue = INUSE(CLEANX(F, UNUSE(V->SymValue), VREF(V), PrintAll));
}

static void ALL_VREF1d(register pSym V)
{
    /* Make a new value, set to all VREFs */
    register tIndex i;

    V->SymValue = INUSE(NEW_1dARRAY(V->SymValueType.BaseType,
      V->SymValueType.Dim1));
    for (i = 0; i < V->SymValueType.Dim1; i++)
        SINDX(V->SymValue, i, VREF1(V, i));
}

/*===========*/
/* CLEAN1d   */
/*===========*/

static void CLEAN1d(FILE *F,
               register pSym V,
               char PrintAll)
{
    /* For each element of V, if their is no value or the */
    /* value isn't constant, we change it to VREF1(V,i).  */
    register tIndex i;
    pExpr X;

    C_ASSERT(V != NULL, 1, "CLEAN1d");
    C_ASSERT(V->SymbolKind == cVariableSym, 2, "CLEAN1d");
    C_ASSERT(V->SymValueType.ValueType == cVectorVal ||
      V->SymValueType.ValueType == cArray1dVal, 3, "CLEAN1d");

    if (!V->SymValue) {
        ALL_VREF1d(V);
        return;
    }

    /* X will be an 1d Array */
    X = MAKE_EXPR_LIKE(V->SymValue);

    if (V->SymValue->NodeKind != cArray1dNode)
        for (i = 0; i < V->SymValueType.Dim1; i++)
            X->Array1dValue[i] = 
                CLEANX(F, INDX(V->SymValue, i), VREF1(V, i), PrintAll);
    else /* more efficient if we can avoid INDX'ing to get the elements */
        for (i = 0; i < V->SymValueType.Dim1; i++) {
            X->Array1dValue[i] = 
                CLEANX(F, V->SymValue->Array1dValue[i], VREF1(V, i), PrintAll);
        }

    X = INUSE(X);    
    DISPOSE_EXPR(UNUSE(V->SymValue));
    V->SymValue = X;
}

static void ALL_VREF2d(register pSym V)
{
    /* Make a new value, set to all VREFs */
    register tIndex i, j;

    V->SymValue = INUSE(NEW_2dARRAY(V->SymValueType.BaseType,
      V->SymValueType.Dim1,
      V->SymValueType.Dim2));
    for (i = 0; i < V->SymValueType.Dim1; i++)
        for (j = 0; j < V->SymValueType.Dim2; j++)
            SINDX2(V->SymValue, i, j, VREF2(V, i, j));
}

/*===========*/
/* CLEAN2d   */
/*===========*/

static void CLEAN2d(FILE *F,
               register pSym V,
               char PrintAll)
{
    /* For each element of V, if their is no value or the */
    /* value isn't constant, we change it to VREF2(V,i,j). */
    register tIndex i, j;
    pExpr X;

    C_ASSERT(V != NULL, 1, "CLEAN2d");
    C_ASSERT(V->SymbolKind == cVariableSym, 2, "CLEAN2d");
    C_ASSERT(V->SymValueType.ValueType == cMatrixVal ||
      V->SymValueType.ValueType == cArray2dVal, 3, "CLEAN2d");

    if (!V->SymValue) {
        ALL_VREF2d(V);
        return;
    }

    /* X will be a 2d Array */
    X = MAKE_EXPR_LIKE(V->SymValue);

    if (V->SymValue->NodeKind != cArray2dNode)
        for (i = 0; i < V->SymValueType.Dim1; i++)
            for (j = 0; j < V->SymValueType.Dim2; j++) 
                S2d(X, i, j,
                    CLEANX(F, INDX2(V->SymValue,i,j), VREF2(V,i,j), PrintAll));
    else /* more efficient if we can avoid INDX2'ing to get the elements */
        for (i = 0; i < V->SymValueType.Dim1; i++)
            for (j = 0; j < V->SymValueType.Dim2; j++) 
                S2d(X, i, j,
                    CLEANX(F, G2d(V->SymValue,i,j), VREF2(V,i,j), PrintAll));

    X = INUSE(X);    
    DISPOSE_EXPR(UNUSE(V->SymValue));
    V->SymValue = X;
}

/*==============*/
/* CLEANVAR     */
/*==============*/

void CLEANVAR(FILE *F,
         pSym V,
         char PrintAll,
         char Remember)
{
    /* This routine is normally called to output variables.  It examines */
    /* each element of the value of V to see whether it is complicated   */
    /* enough that it would be worthwhile to print it out instead of     */
    /* carrying it around in expressions.  If it IS_SIMPLE, it won't be  */
    /* printed out unless PrintAll is true, in which case it will be     */
    /* printed unless the value is IS_CONST.  If it is printed out, the  */
    /* value of that element of V will be replaced with a VREF to that   */
    /* element unless the value is a constant. Normally the complex      */
    /* expression is then disposed, but if Remember is true it will be   */
    /* remembered for later use, e.g. in DERIV.                          */

    C_ASSERT(V != NULL, 1, "CLEANVAR");
    if (V->RememberedVal) {
        DISPOSE_EXPR(UNUSE(V->RememberedVal));
        V->RememberedVal = NULL;
    }
    if (Remember)
        V->RememberedVal = INUSE(SHARE_EXPR(V->SymValue));
    switch (V->SymValueType.ValueType) {
        case cScalarVal:
            CLEANSC(F, V, PrintAll);
            break;
        case cVectorVal:
        case cArray1dVal:
            CLEAN1d(F, V, PrintAll);
            break;
        case cMatrixVal:
        case cArray2dVal:
            CLEAN2d(F, V, PrintAll);
            break;
    }
}
