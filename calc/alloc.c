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

/* These declarations are for the fussy SGI compiler which
 * can't find these static routines otherwise.
 */
static void count_temp_nodes();
static void dispose_temp_nodes();

int64_t gNewCount;  /* counts # expression nodes allocated */
int64_t gNewSize;   /* # bytes allocated for expr nodes */
int64_t gDispCount; /*counts # expresson nodes disposed. */
int64_t gDispSize;  /* # bytes disposed for expr nodes */

/*=============*/
/* NEW_1dARRAY */
/*=============*/

pExpr NEW_1dARRAY(NodeValueType_t baseType,
                  tIndex Len)
{
    /* Make a new array1d expression node with the indicated */
    /* base type.                                           */
    register pExpr E;
    register tIndex i;

    if (baseType != cScalarVal && baseType != cVectorVal &&
        baseType != cMatrixVal)
        fatal("NEW_1dARRAY: illegal base type.");
    E = NEWX(cArray1dNode, Len, 0);
    ARRAY1d_TYPE(baseType, Len, &E->NodeValueType);
    for (i = 0; i < Len; i++)
        E->Array1dValue[i] = NULL;
    return E;
}

/*=============*/
/* NEW_2dARRAY */
/*=============*/

pExpr NEW_2dARRAY(NodeValueType_t baseType,
                  tIndex dim1,
                  tIndex dim2)
{
    /* Make a new array2d expression node with the indicated */
    /* base type.                                           */
    register pExpr E;
    register tIndex i, j;

    if (baseType != cScalarVal && baseType != cVectorVal &&
      baseType != cMatrixVal) {
        fprintf(stderr, "NEW_2dARRAY: illegal base type (%d)\n", baseType);
        abort();
    }
    E = NEWX(cArray2dNode, dim1, dim2);
    ARRAY2d_TYPE(baseType, dim1, dim2, &E->NodeValueType);
    for (i = 0; i < dim1; i++)
        for (j = 0; j < dim2; j++)
            S2d(E, i, j, NULL);
    return E;
}

/*==========*/
/* NEW_VECX */
/*==========*/

pExpr NEW_VECX(NodeValueType_t baseType)
{
    /* Create a vector expression node of the indicated */
    /* base type.                                       */
    pExpr E;

    E = NEW_1dARRAY(baseType, 3);
    VECTOR_TYPE(baseType, &E->NodeValueType);
    return E;
}

/*==========*/
/* NEW_MATX */
/*==========*/

pExpr NEW_MATX(NodeValueType_t baseType)
{
    /* Create a matrix expression node with indicated base type. */
    pExpr E;

    E = NEW_2dARRAY(baseType, 3, 3);
    MATRIX_TYPE(baseType, &E->NodeValueType);
    return E;
}

/*=====*/
/* MAT */
/*=====*/

pExpr MAT(matrix M)
{
    /* Produces a matrix constant expression from a matrix const. */
    register pExpr E;
    register tIndex I, J;

    E = NEWX(cArray2dNode, 3, 3);
    MATRIX_TYPE(cScalarVal, &E->NodeValueType);
    for (I = 0; I < 3; I++)
        for (J = 0; J < 3; J++)
            S2d(E, I, J, SC(M[I][J]));
    return E;
}


/* DISPOSE_EXPR 
 *
 * Throws E away.  You MUST NOT use E again, but note that
 * we can't null the pointer here.                          
 * This routine does nothing if E is null or if its protection
 * level is not cTemporary (0).
 *
 * This is a two-step procedure:
 *   (1) Count the number of occurrences of each node whose protection
 *       is cTemporary.  There can be multiple references to the 
 *       same expression produced by assignments like E = ADD(X,MUL(X,Y)).
 *   (2) Go through all the cTemporary nodes again, decrementing the 
 *       occurrence count.  If the occurrence count goes to 0, free  
 *       the node.
 *
 * We overload the "Protection" field in the expression as the counter.
 * On entry, if Protection=0 the node is temporary and can be freed.  In step 
 * (1) above, we'll decrement any Protection field which is <= 0, so for
 * example a node which appears twice will end with Protection=-2.  In
 * step (2), we'll increment any Protection which is < 0, and free the
 * node when Protection reaches 0.
 *
 */

void DISPOSE_EXPR(pExpr E)
{
    if (!E || E->Protection != cTemporary)
        return;

    count_temp_nodes(E);
    dispose_temp_nodes(E);
}

/* count_temp_nodes
 *
 * This is the pass of DISPOSE_EXPR in which the number of occurrences
 * of each temporary node is counted.  The passed-in expression is 
 * recursively examined and each occurrence of a temporary node is 
 * counted as described in DISPOSE_EXPR above.  
 *
 * If E is not temporary, there cannot be any temporary nodes below it either.
 */
static
void count_temp_nodes(pExpr E)
{
    register tIndex I, J;

    if (!E || E->Protection > 0) 
        return;

    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            count_temp_nodes(E->Opnd);
            break;
        case cBinaryOperatorNode:
            count_temp_nodes(E->LeftOpnd);
            count_temp_nodes(E->RtOpnd);
            break;
        case cTernaryOperatorNode:
            count_temp_nodes(E->FirstOpnd);
            count_temp_nodes(E->SecondOpnd);
            count_temp_nodes(E->ThirdOpnd);
            break;
        case cScalarConstNode:
            /* nothing */
            break;
        case cArray1dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                count_temp_nodes(E->Array1dValue[I]);
            break;
        case cArray2dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                for (J = 0; J < E->NodeValueType.Dim2; J++)
                    count_temp_nodes(G2d(E, I, J));
            break;
        case cVarRefNode:
            /* nothing */
            break;
        case cFunctionCallNode:
            count_temp_nodes(E->FuncCallParm);
            break;
        case cFunction2CallNode:
            count_temp_nodes(E->Func2CallParm1);
            count_temp_nodes(E->Func2CallParm2);
            break;
    }

    E->Protection--;
}

/* dispose_temp_nodes
 *
 * This is the pass of DISPOSE_EXPR in which the previously-counted temporary
 * nodes are actually freed.  No matter how many times a particular node
 * appears in the expression, it will be freed only once.
 *
 * As described above, any node passed in here with a negative Protection
 * value has that value incremented, and the node is freed when its 
 * Protection reaches 0.
 *
 * On entry, we don't expect to see an expression with Protection of 0,
 * so we'll get upset if we do.  Also, if we see an expression with 
 * Protection > 0, we can return immediately since there can be no
 * temporary nodes below it.
 */
static
void dispose_temp_nodes(pExpr E)
{
    register tIndex I, J;

    if (!E || E->Protection > 0) 
        return;

    C_ASSERT(E->Protection < 0, 1, "dispose_temp_nodes");

    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            dispose_temp_nodes(E->Opnd);
            break;
        case cBinaryOperatorNode:
            dispose_temp_nodes(E->LeftOpnd);
            dispose_temp_nodes(E->RtOpnd);
            break;
        case cTernaryOperatorNode:
            dispose_temp_nodes(E->FirstOpnd);
            dispose_temp_nodes(E->SecondOpnd);
            dispose_temp_nodes(E->ThirdOpnd);
            break;
        case cScalarConstNode:
            /* nothing */
            break;
        case cArray1dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                dispose_temp_nodes(E->Array1dValue[I]);
            break;
        case cArray2dNode:
            for (I = 0; I < E->NodeValueType.Dim1; I++)
                for (J = 0; J < E->NodeValueType.Dim2; J++)
                    dispose_temp_nodes(G2d(E, I, J));
            break;
        case cVarRefNode:
            /* nothing */
            break;
        case cFunctionCallNode:
            dispose_temp_nodes(E->FuncCallParm);
            break;
        case cFunction2CallNode:
            dispose_temp_nodes(E->Func2CallParm1);
            dispose_temp_nodes(E->Func2CallParm2);
            break;
    }

    if (++E->Protection == 0)
        DISPX(&E);
}

/*================*/
/* MAKE_EXPR_LIKE */
/*================*/

pExpr MAKE_EXPR_LIKE(pExpr E)
{
    /* Makes a new expression node which has the same type */
    /* as the supplied expression.  1- and 2-d arrays will */
    /* have the same length as in E.                       */

    C_ASSERT(E != NULL, 1, "MAKE_EXPR_");
    switch (E->NodeValueType.ValueType) {
        case cScalarVal:
            fatal("MAKE_EXPR_LIKE: can't make scalar.");
        case cVectorVal:
            return NEW_VECX(E->NodeValueType.BaseType);
        case cMatrixVal:
            return NEW_MATX(E->NodeValueType.BaseType);
        case cArray1dVal:
            return NEW_1dARRAY(E->NodeValueType.BaseType, LEN1d(E));
        case cArray2dVal:
            return NEW_2dARRAY(E->NodeValueType.BaseType, LEN1d(E), LEN2d(E));
    }
    return NULL; /*NOTREACHED*/
}

/*===========*/
/* MAKE_ZERO */
/*===========*/

pExpr MAKE_ZERO(NodeValueType_t T)
{
    /* Makes an expression node of the given type (base type will */
    /* be scalar) and sets it to zero.                            */
    /* Only works for scalar, vector or matrix.                   */
    register pExpr X;
    register tIndex I, J;

    switch (T) {
        case cScalarVal:
            return gScalarZero;
        case cVectorVal:
            return gVectorZero;
        case cMatrixVal:
            X = NEW_MATX(cScalarVal);
            for (I = 0; I < 3; I++)
                for (J = 0; J < 3; J++)
                    S2d(X, I, J, gScalarZero);
            return X;
        default:
            fatal("MAKE_ZERO: only works for scalar,vec,mat");
            return NULL; /*NOTREACHED*/
    }
} /* MAKE_ZERO */

/*================*/
/* MAKE_ZERO_LIKE */
/*================*/

pExpr MAKE_ZERO_LIKE(pExpr E)
{
    /* Calls MAKE_EXPR_LIKE to get an expression of the same */
    /* type as E, and then sets it to zero.                  */
    /* Remember that E's base type might not be scalar.      */
    register pExpr X;
    register tIndex I, J;

    X = MAKE_EXPR_LIKE(E);
    switch (X->NodeKind) {
        case cScalarConstNode:
            X->ScalarValue = 0.0;
            break;
        case cArray1dNode:
            for (I = 0; I < X->NodeValueType.Dim1; I++)
                X->Array1dValue[I] = MAKE_ZERO(X->NodeValueType.BaseType);
            break;
        case cArray2dNode:
            for (I = 0; I < X->NodeValueType.Dim1; I++)
                for (J = 0; J < X->NodeValueType.Dim2; J++)
                    S2d(X, I, J, MAKE_ZERO(X->NodeValueType.BaseType));
            break;
        default:
            fatal("MAKE_ZERO_LIKE: bad node type.");
    }        /* case */
    return X;
} /* MAKE_ZERO_LIKE */

/*==================================================*/
/* STORAGE ALLOCATION/DEALLOCATION FOR EXPRESSIONS  */
/*==================================================*/

/*======*/
/* NEWX */
/*======*/

pExpr NEWX(enum tNodeKind Kind,
           tIndex dim1,
           tIndex dim2)
{
    /* Allocates a new temporary expression of the indicated kind.        */

    uintptr_t size;
    register pExpr X;

    C_ASSERT(dim1 >= 0, 1, "NEWX");
    C_ASSERT(dim2 >= 0, 2, "NEWX");

    switch (Kind) {
        case cUnaryOperatorNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.UnaryOperatorNode;
            break;
        case cBinaryOperatorNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.BinaryOperatorNode;
            break;
        case cTernaryOperatorNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.TernaryOperatorNode;
            break;
        case cScalarConstNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.ScalarConstNode;
            break;
        case cArray1dNode:
            if (dim1 == 0)
                fatal("NEWX: Array1d can't have zero dimension.");
            size = (uintptr_t)&((pExpr)0)->un + dim1 * sizeof(pExpr);
            break;
        case cArray2dNode:
            if (dim1 == 0 || dim2 == 0)
                fatal("NEWX: Array2d can't have zero dimension.");
            size = (uintptr_t)&((pExpr)0)->un + dim1 * dim2 * sizeof(pExpr);
            break;
        case cVarRefNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.VarRefNode;
            break;
        case cFunctionCallNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.FunctionCallNode;
            break;
        case cFunction2CallNode:
            size = (uintptr_t)&((pExpr)0)->un + sizeof X->un.Function2CallNode;
            break;
    }
    if (!(X = (pExpr)malloc(size))) {
        perror("fatal");
        exit(2);
        /*NOTREACHED*/
    }
    X->NodeKind = Kind;
    X->Protection = cTemporary;
    gNewCount++;
    gNewSize += size;
    return X;
}

/*=======*/
/* DISPX */
/*=======*/

void DISPX(pExpr *X)
{
    /* Disposes the given expression node.  Does not trudge */
    /* down the tree, and hates disposing null nodes or     */
    /* nodes with protection other than cTemporary.         */
    /* X is returned null.                                  */

    C_ASSERT(*X != NULL, 1, "DISPX");
    C_ASSERT((*X)->Protection == cTemporary, 2, "DISPX");

    free((char *)*X);
    gDispCount++;
/*    gDispSize = gDispSize + NODE_SIZE(*X);        silly to re-calc... */
    *X = NULL;
}
