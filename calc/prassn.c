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
#include "language.h"

/*==========*/
/* EXPR_LEN */
/*==========*/

static long EXPR_LEN(pExpr E)
{
    /* Returns a number proportional to the length of the given expression   */
    /* as it would appear in print.  Anything which could use up much        */
    /* space is counted, e.g. variable names, indexes, constants, etc.       */
    /* A function call is just counts one since they are currently all       */
    /* abbreviated.  Operators aren't counted at all.                        */
    register long Cnt = 0;
    register tIndex i, j;

    if (E)
        switch (E->NodeKind) {
            case cUnaryOperatorNode:
                return EXPR_LEN(E->Opnd);
            case cBinaryOperatorNode:
                return EXPR_LEN(E->LeftOpnd) + EXPR_LEN(E->RtOpnd);
            case cTernaryOperatorNode:
                return 1 + EXPR_LEN(E->FirstOpnd) + 
                       EXPR_LEN(E->SecondOpnd) + EXPR_LEN(E->ThirdOpnd);
            case cScalarConstNode:
                /* some of these can be very long -- should really measure */
                return 2;
            case cArray1dNode:
                for (i = 0; i < E->NodeValueType.Dim1; i++)
                    Cnt += EXPR_LEN(E->Array1dValue[i]);
                break;
            case cArray2dNode:
                for (i = 0; i < E->NodeValueType.Dim1; i++)
                    for (j = 0; j < E->NodeValueType.Dim2; j++)
                        Cnt += EXPR_LEN(G2d(E, i, j));
                break;
            case cVarRefNode:
                return 1 + E->NumIndices;
            case cFunctionCallNode:
                return 1;
            case cFunction2CallNode:
                return 1 + EXPR_LEN(E->Func2CallParm1) + EXPR_LEN(E->Func2CallParm2);
        }
    return Cnt;
}

/*============*/
/* LIMIT_EXPR */
/*============*/

pExpr LIMIT_EXPR(FILE *F,
                 pExpr E,
                 int NextTemp,
                 int *HighestTemp)
{
    /* Returns an expression equivalent to E, but with no more than gMaxExprLen
       terms.  May cause some assignment statements of the form
                 TEMP(i)= ...
       to be printed out.  Then the returned expression will contain
       references to TEMP(i).  i is chosen starting with NextTemp and
       incrementing.  Highest temp used is returned in HighestTemp.        */

    register pExpr LimitedExpr;
    long LeftCount, RtCount, FirstCnt, SecondCnt, ThirdCnt;

    *HighestTemp = NextTemp - 1;        /* none used yet */

    /* It the expression is small, or there aren't any more TEMPS available, */
    /* we just return the passed-in expression.                              */
    if (EXPR_LEN(E) <= gMaxExprLen || *HighestTemp >= gMaxTemps-1)
        return E;

    /* E is too big to print out as is. */
    /* *** We only handle operator nodes properly. *** */

    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            LimitedExpr = NEWX(cUnaryOperatorNode, 0, 0);
            LimitedExpr->UnOp = E->UnOp;
            LimitedExpr->Opnd = 
              LIMIT_EXPR(F, E->Opnd, NextTemp, HighestTemp);
            break;

        case cBinaryOperatorNode:
            LeftCount = EXPR_LEN(E->LeftOpnd);
            RtCount = EXPR_LEN(E->RtOpnd);
            LimitedExpr = NEWX(cBinaryOperatorNode, 0, 0);
            LimitedExpr->BinOp = E->BinOp;
            LimitedExpr->LeftOpnd = E->LeftOpnd;
            LimitedExpr->RtOpnd = E->RtOpnd;

            if (LeftCount > gMaxExprLen) {
                /* Trim down the left side. */
                LimitedExpr->LeftOpnd = 
                  LIMIT_EXPR(F, E->LeftOpnd, NextTemp, HighestTemp);
                LeftCount = EXPR_LEN(LimitedExpr->LeftOpnd);
            }

            if (*HighestTemp < gMaxTemps-1 && RtCount > gMaxExprLen) {
                /* Trim down the right side. */
                LimitedExpr->RtOpnd = 
                  LIMIT_EXPR(F, E->RtOpnd, *HighestTemp + 1, HighestTemp);
                RtCount = EXPR_LEN(LimitedExpr->RtOpnd);
            }

            if (*HighestTemp < gMaxTemps-1 &&
              LeftCount + RtCount > gMaxExprLen) {
                (*HighestTemp)++;
                if (LeftCount > RtCount) {
                    PRINT_TEMP_EXPR(F, (tIndex)*HighestTemp,
                      LimitedExpr->LeftOpnd);
                    DISPOSE_EXPR(LimitedExpr->LeftOpnd);
                    LimitedExpr->LeftOpnd =
                      VREF1(gTempSym, (tIndex)*HighestTemp);
                } else {
                    PRINT_TEMP_EXPR(F, (tIndex)*HighestTemp,
                      LimitedExpr->RtOpnd);
                    DISPOSE_EXPR(LimitedExpr->RtOpnd);
                    LimitedExpr->RtOpnd =
                      VREF1(gTempSym, (tIndex)*HighestTemp);
                }
            }
            break;

        case cTernaryOperatorNode:
            FirstCnt  = EXPR_LEN(E->FirstOpnd);
            SecondCnt = EXPR_LEN(E->SecondOpnd);
            ThirdCnt  = EXPR_LEN(E->ThirdOpnd);
            LimitedExpr = NEWX(cTernaryOperatorNode, 0, 0);
            LimitedExpr->TerOp = E->TerOp;
            LimitedExpr->FirstOpnd = E->FirstOpnd;
            LimitedExpr->SecondOpnd = E->SecondOpnd;
            LimitedExpr->ThirdOpnd = E->ThirdOpnd;

            if (FirstCnt > gMaxExprLen) {
                /* Trim down the first argument. */
                LimitedExpr->FirstOpnd = 
                  LIMIT_EXPR(F, E->FirstOpnd, NextTemp, HighestTemp);
            }

            if (*HighestTemp < gMaxTemps-1 && SecondCnt > gMaxExprLen) {
                /* Trim down the second argument. */
                LimitedExpr->SecondOpnd = 
                  LIMIT_EXPR(F, E->SecondOpnd, *HighestTemp + 1, HighestTemp);
            }

            if (*HighestTemp < gMaxTemps-1 && ThirdCnt > gMaxExprLen) {
                /* Trim down the second argument. */
                LimitedExpr->ThirdOpnd = 
                  LIMIT_EXPR(F, E->ThirdOpnd, *HighestTemp + 1, HighestTemp);
            }
            break;

        default:
            /* not really correct, but won't cause trouble in current use *** */
            return E;
    }

    return LimitedExpr;
}

/*============*/
/* PRINT_ASSN */
/*============*/

void PRINT_ASSN(FILE *F,
           char *Vname,
           pExpr E,
           int byref)
{
/* Prints an assignment statement Vname = E, using temporaries if necessary.
 * If Vname is a parameter passed by reference, set `byref' non-zero and
 * we'll spit out the appropriate dereferencer first.
 */
    pExpr LimitedExpr, NoIfExpr;
    int HighestTemp;

    E = INUSE(E);
    NoIfExpr = 
        INUSE(REMOVE_QUES(F, E, 0 /*next available temporary*/, &HighestTemp));
    LimitedExpr = LIMIT_EXPR(F, NoIfExpr, HighestTemp+1, &HighestTemp);
    efprintf(F, "%s%s%=", byref ? Lang->deref : "", Vname);
    PRINT_EXPR(F, LimitedExpr);
    DISPOSE_EXPR(LimitedExpr);
    DISPOSE_EXPR(UNUSE(NoIfExpr));
    E = UNUSE(E);
}

/*=============*/
/* PRINT_ASSN1 */
/*=============*/

void PRINT_ASSN1(
            FILE *F,
            char *Vname,
            tIndex i,
            pExpr E)
{
/* Prints an assignment stmt Vname(i) = E, using temporaries if necessary. */
    pExpr LimitedExpr, NoIfExpr;
    int HighestTemp;

    E = INUSE(E);
    NoIfExpr = 
        INUSE(REMOVE_QUES(F, E, 0 /*next available temporary*/, &HighestTemp));
    LimitedExpr = LIMIT_EXPR(F, NoIfExpr, HighestTemp+1, &HighestTemp);
    efprintf(F, "%s%(%@d%)%=", Vname, i);
    PRINT_EXPR(F, LimitedExpr);
    DISPOSE_EXPR(LimitedExpr);
    DISPOSE_EXPR(UNUSE(NoIfExpr));
    E = UNUSE(E);
}

/*=============*/
/* PRINT_ASSN2 */
/*=============*/

void PRINT_ASSN2(FILE *F,
            char *Vname,
            tIndex i,
            tIndex j,
            pExpr E)
{
/* Prints an assignment stmt Vname(i,j) = E, using temporaries if necessary. */
    pExpr LimitedExpr, NoIfExpr;
    int HighestTemp;

    E = INUSE(E);
    NoIfExpr = 
        INUSE(REMOVE_QUES(F, E, 0 /*next available temporary*/, &HighestTemp));
    LimitedExpr = LIMIT_EXPR(F, NoIfExpr, HighestTemp+1, &HighestTemp);
    efprintf(F, "%s%(%@d%,%@d%)%=", Vname, i, j);
    PRINT_EXPR(F, LimitedExpr);
    DISPOSE_EXPR(LimitedExpr);
    DISPOSE_EXPR(UNUSE(NoIfExpr));
    E = UNUSE(E);
}

/*=============*/
/* PRINT_ASSN3 */
/*=============*/

void PRINT_ASSN3(FILE *F,
            char *Vname,
            tIndex i,
            tIndex j,
            tIndex k,
            pExpr E)
{
/* Prints an assignment stmt Vname(i,j,k) = E, using temporaries if necessary*/
    pExpr LimitedExpr, NoIfExpr;
    int HighestTemp;

    E = INUSE(E);
    NoIfExpr = 
        INUSE(REMOVE_QUES(F, E, 0 /*next available temporary*/, &HighestTemp));
    LimitedExpr = LIMIT_EXPR(F, NoIfExpr, HighestTemp+1, &HighestTemp);
    efprintf(F, "%s%(%@d%,%@d%,%@d%)%=", Vname, i, j, k);
    PRINT_EXPR(F, LimitedExpr);
    DISPOSE_EXPR(LimitedExpr);
    DISPOSE_EXPR(UNUSE(NoIfExpr));
    E = UNUSE(E);
}
