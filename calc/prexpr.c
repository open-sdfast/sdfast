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

long gAsgCount;                /* counts # assignment statements printed */
char BinOpNames[][6] = { "+", "-", "*", "/", "dot", "x", "deriv" };
long gBinOpCount[7];
char UnOpNames[][2] = { "-" };
long gUnOpCount[1];

static void PRINT_EP(FILE *F,
                register pExpr E,
                enum tParentOp ParentOp);

/*==============*/
/*  PRINT_VREF  */
/*==============*/

void PRINT_VREF(FILE *F,
           register pExpr V)
{
    /* Prints the variable reference, no newline.  Won't split the var */
    /* across lines.                                                   */
    register tIndex i;
    char vout[100], *vp;

    C_ASSERT(IS_VREF(V), 1, "PRINT_VREF");

    /* We'll get very upset if this VREF thought it was for one incarnation */
    /* of a variable but the symbol says it's another.  This can happen     */
    /* if a symbol is reused without flushing out someone who depends on it.*/

    /* *** doesn't work right yet  
      if (V^.AssignCountE <> V^.VarRef^.AssignCountS) then
      begin
        writeln('PRINT_VREF: found out-of-date VREF');
        writeln('VREF expected assign ',V^.AssignCountE,
                ' but symbol was at ',V^.VarRef^.AssignCountS);
        writeln('This is an internal error - contact Symbolic Dynamics.');
        SHOW_TYPE(V);
        TRACE(output); halt;
        end;
      *** */

    esprintf(vout, "%s", V->VarRef->PrintName);
    if (V->NumIndices) {
        vp = vout + strlen(vout);
        esprintf(vp, "%(");
        vp += strlen(vp);
        for (i = 0; i < V->NumIndices; i++) {
            if (i) {
                esprintf(vp, "%,");
                vp += strlen(vp);
            }
            esprintf(vp, "%@d", V->Indices[i]);
            vp += strlen(vp);
        }
        esprintf(vp, "%)");
    }
    efprintf(F, "%s", vout);
}

/*===========*/
/* BINOPTYPE */
/*===========*/

static enum tParentOp BINOPTYPE(tBinaryOperator Op)
{
    /* Classify binary operator for use in precedence in PRINT_E. */

    switch (Op) {
        case cAdd:
            return cPlusOp;
        case cSubtract:
            return cMinusOp;
        case cMultiply:
            return cMulOp;
        case cDivide:
            return cDivOp;
        default:
            return cSpecialOp;
    }
}

/*==========*/
/* UNOPTYPE */
/*==========*/

static enum tParentOp UNOPTYPE(tUnaryOperator Op)
{
    /* same as BINOPTYPE, but for unary operators */

    switch (Op) {
        case cNegate:
            return cNegateOp;
        default:
            return cSpecialOp;
    }
    /*NOTREACHED*/
}

static void PRINT_EP(FILE *F,
                register pExpr E,
                enum tParentOp ParentOp)
{
    /* The parent node's operator is passed in to allow precedence checking   */
    /* to avoid unnecessary parentheses.                                      */
    /* If the operator in the current expr E is lower precedence than the     */
    /* passed in precedence, we parenthesize it.  Also, if it is the same     */
    /* precedence but at least one of the two ops is non-commutative (i.e. is */
    /* divide or subtract) we parenthesize.                                   */

    /* **** ELIMINATION OF PARENTHESES HAS BEEN DISABLED, SINCE NUMERICALLY   */
    /* BETTER RESULTS ARE PRODUCED WITH THEM IN. THIS IS DUE TO CANCELLATIONS */
    /* WHICH CAN OCCUR WHEN SIMILAR TERMS ARE COMPUTED IN SIMILAR ORDER.      */
    register tIndex I, J;
    char ParensNeeded;
    enum tParentOp NodeOpType;

    if (!E) {
        efprintf(F, "?");
        return;
    }
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            /* We have to parenthesize unary operators to avoid making
             * expressions which are illegal in Fortran, like
             *          a/-b
             * Officially, Fortran doesn't allow consecutive operators
             * like the above.  In practice, almost every Fortran compiler
             * quietly does the right thing.  IBM RS6000 Fortran, however,
             * calls that a syntax error.  With parentheses, we generate
             *          a/(-b)
             * and avoid the problem.
             */
            ParensNeeded = (ParentOp != cTopLevelOp);
            if (ParensNeeded)
                efprintf(F, "(");
            switch (E->UnOp) {

                case cNegate:           efprintf(F, "%s", UnOpNames[E->UnOp]);
                                   gUnOpCount[E->UnOp]++;
                                   break;
                case cLogicalNot:  efprintf(F, "%s", Lang->not_op);
                                   break;
                default: 
                    fatal("PRINT_EP: unrecognized unary operator");
            }
            PRINT_EP(F, E->Opnd, UNOPTYPE(E->UnOp));
            if (ParensNeeded)
                efprintf(F, ")");
            break;

        case cBinaryOperatorNode:
            NodeOpType = BINOPTYPE(E->BinOp);
            switch (ParentOp) {
                case cTopLevelOp:
                    ParensNeeded = 0;
                    break;
                case cPlusOp:
                    ParensNeeded = NodeOpType == cSpecialOp;
                    break;
                case cMinusOp:
                    ParensNeeded = NodeOpType == cSpecialOp ||
                      NodeOpType == cPlusOp || NodeOpType == cMinusOp;
                    break;
                case cMulOp:
                    ParensNeeded = NodeOpType != cMulOp &&
                      NodeOpType != cNegateOp;
                    break;
                case cDivOp:
                    ParensNeeded = NodeOpType != cNegateOp;
                    break;
                case cNegateOp:
                case cSpecialOp:
                    ParensNeeded = 1;
                    break;
                default: 
                    fatal("PRINT_EP: unrecognized operator classification");
            }
            ParensNeeded = 1;        /* **** TEMP */
            if (ParensNeeded)
                efprintf(F, "(");
            PRINT_EP(F, E->LeftOpnd, NodeOpType);
            switch (E->BinOp) {
                case cAdd:
                case cSubtract:
                case cMultiply:
                case cDivide:
                case cDot:
                case cCross:
                case cDeriv:           efprintf(F, "%s", BinOpNames[E->BinOp]);
                                   gBinOpCount[E->BinOp]++;
                                   break;
                case cIsEqual:     efprintf(F, "%s", Lang->eq_op);
                                   break;
                case cNotEqual:    efprintf(F, "%s", Lang->ne_op);
                                   break;
                case cLessThan:    efprintf(F, "%s", Lang->lt_op);
                                   break;
                case cGreaterThan: efprintf(F, "%s", Lang->gt_op);
                                   break;
                case cLessOrEq:    efprintf(F, "%s", Lang->le_op);
                                   break;
                case cGreaterOrEq: efprintf(F, "%s", Lang->ge_op);
                                   break;
                case cLogicalAnd:  efprintf(F, "%s", Lang->and_op);
                                   break;
                case cLogicalOr:   efprintf(F, "%s", Lang->or_op);
                                   break;
                default: 
                    fatal("PRINT_EP: unrecognized binary operator");
            }
            PRINT_EP(F, E->RtOpnd, NodeOpType);
            if (ParensNeeded)
                efprintf(F, ")");
            break;

        case cTernaryOperatorNode:
            switch (E->TerOp) {
                case cIfThenElse:
                    efprintf(F, "(");
                    PRINT_EP(F, E->FirstOpnd, cSpecialOp);
                    efprintf(F, "?");
                    PRINT_EP(F, E->SecondOpnd, cSpecialOp);
                    efprintf(F, ":");
                    PRINT_EP(F, E->ThirdOpnd, cSpecialOp);
                    efprintf(F, ")");
                    break;
                default: 
                    fatal("PRINT_EP: unrecognized ternary operator");
            }
            break;

        case cScalarConstNode:
            efprintf(F, "%r", E->ScalarValue);
            break;

        case cArray1dNode:
            efprintf(F, "[");
            for (I = 0; I < E->NodeValueType.Dim1; I++) {
                if (I)
                    efprintf(F, ",");
                PRINT_E(F, E->Array1dValue[I]);
            }
            efprintf(F, "]");
            break;

        case cArray2dNode:
            /* old code always had [n,n] -- never used? */
            efprintf(F, "[");
            for (I = 0; I < E->NodeValueType.Dim1; I++) {
                if (I)
                    efprintf(F, ",");
                efprintf(F, "[");
                for (J = 0; J < E->NodeValueType.Dim2; J++) {
                    if (J)
                        efprintf(F, ",");
                    PRINT_E(F, G2d(E, I, J));
                }
                efprintf(F, "]");
            }
            efprintf(F, "]");
            break;

        case cVarRefNode:
            PRINT_VREF(F, E);
            break;

        case cFunctionCallNode:
            if (E->FuncCallParm->NodeKind == cVarRefNode &&
              E->FuncCallParm->NumIndices == 1 &&
              (!strcmp(E->FuncCallParm->VarRef->PrintName, "q") ||
              !strcmp(E->FuncCallParm->VarRef->PrintName, "qbar"))) {
                /* a function of q[i] or qbar[i]*/
                switch (E->FuncVarRef->WhichFunction) {
                    char str[10];

                    case cSine:
                        /* do it this way to line breakup between name & # */
                        esprintf(str, "%s%@d",
                          !strcmp(E->FuncCallParm->VarRef->PrintName, "qbar") ?
                          "sb" : "s", E->FuncCallParm->Indices[0]);
                        efprintf(F, "%s", str);
                        return;

                    case cCosine:
                        /* do it this way to line breakup between name & # */
                        esprintf(str, "%s%@d",
                          !strcmp(E->FuncCallParm->VarRef->PrintName, "qbar") ?
                          "cb" : "c", E->FuncCallParm->Indices[0]);
                        efprintf(F, "%s", str);
                        return;
                }        /* switch */
            }
            switch (E->FuncVarRef->WhichFunction) {
                case cSine:           efprintf(F, "%@D%s", Lang->func_sin);  break;
                case cCosine:         efprintf(F, "%@D%s", Lang->func_cos);  break;
                case cAsin:         efprintf(F, "%@D%s", Lang->func_asin); break;
                case cAcos:         efprintf(F, "%@D%s", Lang->func_acos); break;
                case cAbs:         efprintf(F, "%@D%s", Lang->func_abs);  break;
                case cSqrt:         efprintf(F, "%@D%s", Lang->func_sqrt); break;
                default:
                    fatal("PRINT_EP: unrecognized one-arg function");
                    /*NOTREACHED*/
            }
            efprintf(F, "(");
            PRINT_E(F, E->FuncCallParm);
            efprintf(F, ")");
            break;

        case cFunction2CallNode:
            switch (E->Func2VarRef->WhichFunction) {
                case cAtan2:           efprintf(F, "%@D%s", Lang->func_atan2);  break;
                default:
                    fatal("PRINT_EP: unrecognized two-arg function");
                    /*NOTREACHED*/
            }
            efprintf(F, "(");
            PRINT_E(F, E->Func2CallParm1);
            efprintf(F, ",");
            PRINT_E(F, E->Func2CallParm2);
            efprintf(F, ")");
            break;
    }        
}

/*=========*/
/* PRINT_E */
/*=========*/

void PRINT_E(FILE *F,
        pExpr E)
{
    /* prints an expression, no newline afterwards */

    PRINT_EP(F, E, cTopLevelOp);
}

/*============*/
/* PRINT_EXPR */
/*============*/

void PRINT_EXPR(FILE *F,
           pExpr E)
{
    /* print expression, follow with end comment if in comment mode and newline.
       No attempt to limit size. */

    PRINT_E(F, E);
    efprintf(F, "%;");
    if (!CMT_MODE()) {
        efprintf(F, "\n");
        gAsgCount++;
    } else
        efprintf(F, "%}");
}

/*=================*/
/* PRINT_TEMP_EXPR */
/*=================*/

void PRINT_TEMP_EXPR(FILE *F,
                tIndex i,
                pExpr E)
{
    /* Prints an assignment stmt TEMP(i) = E */

    efprintf(F, "%s%(%@d%)%=", gTempSym->PrintName, i);
    PRINT_EXPR(F, E);
}
