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

/*=====*/
/* SC  */
/*=====*/

pExpr SC(scalar S)
{
    /* Produce a scalar constant expression from a scalar. */
    pExpr E;

    if (!S)
        return gScalarZero;
    else if (S == 1)
        return gScalarOne;
    else {
        E = NEWX(cScalarConstNode, 0, 0);
        SCALAR_TYPE(&E->NodeValueType);
        E->ScalarValue = S;
        return E;
    }
}

/*=====*/
/* VEC */
/*=====*/

pExpr VEC(vector V)
{
    /* Produces a vector constant expression from a vector const. */
    pExpr E;
    register tIndex I;

    E = NEWX(cArray1dNode, 3, 0);
    VECTOR_TYPE(cScalarVal, &E->NodeValueType);
    for (I = 0; I < 3; I++)
        E->Array1dValue[I] = SC(V[I]);
    return E;
}

/*=============*/
/* SCALAR_ZERO */
/*=============*/

pExpr SCALAR_ZERO(void)
{
    return gScalarZero;
}

/*=============*/
/* SCALAR_ONE  */
/*=============*/

pExpr SCALAR_ONE(void)
{
    return gScalarOne;
}

/*=============*/
/* VECTOR_ZERO */
/*=============*/

pExpr VECTOR_ZERO(void)
{
    return gVectorZero;
}

/*============*/
/* B1, B2, B3 */
/*============*/

pExpr B1(void)
{
    extern pExpr gB1;        /* basis vector 100 */

    return gB1;
}

pExpr B2(void)
{
    extern pExpr gB2;        /* basis vector 010 */

    return gB2;
}

pExpr B3(void)
{
    extern pExpr gB3;        /* basis vector 001 */

    return gB3;
}

/*=============*/
/* MATRIX_ZERO */
/*=============*/

pExpr MATRIX_ZERO(void)
{
    extern pExpr gMatrixZero;

    return gMatrixZero;
}

/*==============*/
/* MATRIX_IDENT */
/*==============*/

pExpr MATRIX_IDENT(void)
{
    extern pExpr gMatrixIdent;

    return gMatrixIdent;
}

char *PRINTNAME(pSym S)
{
    /* Returns the print name of the supplied symbol. */
    /* in case S is a sym in the caller */
    /* This routine should go away when sym's are really pSym's [TRE] */

    return S->PrintName;
}

/*
 * Returns the numeric value of a scalar constant expression.
 * Drops dead if you pass in the wrong kind of expression.
 */
double
NUMVAL(pExpr cx)
{
    C_ASSERT(IS_CONST(cx), 1, "NUMVAL");
    return cx->ScalarValue;
}
