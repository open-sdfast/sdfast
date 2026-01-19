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

pSym gSineFunction;
pSym gCosineFunction;
pSym gAsinFunction;
pSym gAcosFunction;
pSym gAbsFunction;
pSym gSqrtFunction;
pSym gAtan2Function;
pExpr gScalarZero;        /* scalar constant node = 0.0 */
pExpr gScalarOne;        /* scalar constant node = 1.0 */
pExpr gVectorZero;
pExpr gB1;        /* three basis vectors 100, 010, 001 */
pExpr gB2;
pExpr gB3;
pExpr gMatrixZero;
pExpr gMatrixIdent;

char  gPrefix[33];

/* User-supplied global values (set in INIT_CALC) */
long gMaxExprLen;        /* Max length of printed expr. */
int gMaxTemps;        /* Max no. temporary variables. */

int gSinglePrecision;        /* use single precision if true, else double */

/* The symbol used for temporary variables TEMP(i) */
pSym gTempSym;

/*================*/
/* INIT_CALC      */
/*================*/

void INIT_CALC(int MaxExprLen,
          int MaxTemps)
{
    /* Call before using the calculator. */
    register tIndex i, j;

    SET_LANGUAGE(&Text_language);

    RESET_OPS();

    RESET_COUNTS();
    DECL_FUNC(&gSineFunction, "sin", cSine);
    DECL_FUNC(&gCosineFunction, "cos", cCosine);
    DECL_FUNC(&gAsinFunction, "asin", cAsin);
    DECL_FUNC(&gAcosFunction, "acos", cAcos);
    DECL_FUNC(&gAbsFunction, "abs", cAbs);
    DECL_FUNC(&gSqrtFunction, "sqrt", cSqrt);
    DECL_FUNC(&gAtan2Function, "atan2", cAtan2);

    gScalarZero = NEWX(cScalarConstNode, 0, 0);
    SCALAR_TYPE(&gScalarZero->NodeValueType);
    gScalarZero->ScalarValue = 0.0;
    gScalarZero = PERM(gScalarZero);

    gScalarOne = NEWX(cScalarConstNode, 0, 0);
    SCALAR_TYPE(&gScalarOne->NodeValueType);
    gScalarOne->ScalarValue = 1.0;
    gScalarOne = PERM(gScalarOne);

    gVectorZero = NEWX(cArray1dNode, 3, 0);
    VECTOR_TYPE(cScalarVal, &gVectorZero->NodeValueType);
    for (i = 0; i < 3; i++)
        gVectorZero->Array1dValue[i] = gScalarZero;
    gVectorZero = PERM(gVectorZero);

    /* Initialize basis vectors b1,b2,b3=100,010,001 */

    gB1 = NEWX(cArray1dNode, 3, 0);
    VECTOR_TYPE(cScalarVal, &gB1->NodeValueType);
    for (i = 0; i < 3; i++)
        gB1->Array1dValue[i] = !i ? gScalarOne : gScalarZero;
    gB1 = PERM(gB1);

    gB2 = NEWX(cArray1dNode, 3, 0);
    VECTOR_TYPE(cScalarVal, &gB2->NodeValueType);
    for (i = 0; i < 3; i++)
        gB2->Array1dValue[i] = i == 1 ? gScalarOne : gScalarZero;
    gB2 = PERM(gB2);

    gB3 = NEWX(cArray1dNode, 3, 0);
    VECTOR_TYPE(cScalarVal, &gB3->NodeValueType);
    for (i = 0; i < 3; i++)
        gB3->Array1dValue[i] = i == 2 ? gScalarOne : gScalarZero;
    gB3 = PERM(gB3);

    gMatrixZero = NEWX(cArray2dNode, 3, 3);
    MATRIX_TYPE(cScalarVal, &gMatrixZero->NodeValueType);
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            S2d(gMatrixZero, i, j, gScalarZero);
    gMatrixZero = PERM(gMatrixZero);

    gMatrixIdent = NEWX(cArray2dNode, 3, 3);
    MATRIX_TYPE(cScalarVal, &gMatrixIdent->NodeValueType);
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            S2d(gMatrixIdent, i, j, i == j ? gScalarOne : gScalarZero);
    gMatrixIdent = PERM(gMatrixIdent);

    gMaxExprLen = MaxExprLen;
    gMaxTemps = MaxTemps;
    gSinglePrecision = 0;        /*default is double*/

    /* Define the gTempSym symbol TEMP(gMaxTemps).  This is like */
    /* DECL_1dARRAY without the printing.                        */
    gTempSym = newsym(cVariableSym);
    gTempSym->SymbolKind = cVariableSym;
    strcpy(gTempSym->PrintName, "temp");
    ARRAY1d_TYPE(cScalarVal, gMaxTemps, &gTempSym->SymValueType);
}

/*===============*/
/* SET_PRECISION */
/*===============*/

void SET_PRECISION(int SinglePrecision)
{
    gSinglePrecision = SinglePrecision;
}

/*==============*/
/* SET_LANGUAGE */
/*==============*/

void SET_LANGUAGE(struct language *language)
{
    extern int lexlev;

    Lang = language;
    lexlev = Lang->stmt_lexlev;
}

/*============*/
/* SET_PREFIX */
/*============*/

void SET_PREFIX(char *Prefix)
{
    strncpy(gPrefix,Prefix,33);
}

/* fatal(s)
   Print the string + newline to stderr and abort */
void fatal(char *s)
{
    fputs(s, stderr);
    putc('\n', stderr);
    abort();
}
