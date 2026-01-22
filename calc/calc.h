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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
 
#define cVeryClose 1e-15        /* used to see if a constant is "equal" to
                                   a particular numerical value */

#define cMaxDim 1000000
#define cMaxNumIndices 4

/* these are values for ProtectionLevels */
#define cTemporary 0
#define cPermanent 32765
#define cMaxProtection 10000     /* bomb out if we need more protection */
                                 /* than this (something's wrong!)      */

// N.B. pExpr is temporarily defined as a macro here, then later
// undef'ed and replaced with an actual typedef.
#define pExpr struct tExprNode * /* until tExprNode is defined */

#define MAX_SYMBOL_NAME_LEN 15

typedef double scalar;
typedef scalar vector[3];
typedef vector matrix[3];

typedef int Index_t;
/*XXX*/ typedef Index_t tIndex;

typedef enum {cVariableSym, cFunctionSym} SymbolKind_t;
typedef enum {cScalarVal, cVectorVal, cMatrixVal, cArray1dVal, cArray2dVal} 
             NodeValueType_t;

/*=========================================================*/
/* This type is used to describe the type of a particular  */
/* expression node or variable                             */
/*=========================================================*/

struct tExprType {
    NodeValueType_t ValueType;
    Index_t Dim1;        /* for arrays */
    Index_t Dim2;
    NodeValueType_t BaseType;
};

/*================================================*/
/* This type is used for usage counts in expr.    */
/* nodes to indicate whether they may be disposed.*/
/*================================================*/

typedef int tProtectionLevel;
enum tKnownFunction {cSine, cCosine, cAsin, cAcos, cAbs, cSqrt, cAtan2};


/*             tSymTabEntry
 * There are four kinds of symbols: scalar symbols, one-  
 * dimensional array symbols, two-dimensional array        
 * symbols, and functions.  There are five value types.    
 * These are scalar, one-dimensional array, two-dimensional
 * array, vector (a 3-element one-dimensional array) and   
 * matrix (a 3x3 two-dimensional array).  All types except 
 * scalar require that the element type also be described. 
 * Elements may be scalars, vectors or matrices.  The      
 * elements of structured elements must be scalars.  That  
 * is, it is okay to have a matrix of vectors as long as   
 * the vectors consist only of 3 scalar elements.  It is   
 * not allowed to have, e.g., a matrix of vectors of       
 * matrices.  The element type of a structured symbol or   
 * expression is referred to sometimes as its "base" type. 
 * A scalar's base type is always scalar.                  
 * 
 * When a variable is printed out, the value for each element that gets 
 * printed is replaced by a VREF to that element.  The expr corr. to   
 * the value that was printed is normally disposed.  For some variables,
 * however, it is important to remember their actual values even after 
 * the variable is printed.  This may be the case, for example, if    
 * expressions involving this variable have DERIV applied to them.   
 *
 * NOT IMPLEMENTED YET: (May never be!)
 * We bump a counter each time an ASSIGN is executed for this symbol. 
 * This enables us to detect VREF's to out-of-date symbols.  VREF's  
 * count should always match the symbol's count.                    
 */
struct tSymTabEntry {
    char PrintName[MAX_SYMBOL_NAME_LEN+1];
    struct tExprType SymValueType;
    SymbolKind_t SymbolKind;
    union {
        struct {
            pExpr uSymValue;
            pExpr uRememberedVal;
            long uAssignCountS;
        } VariableSym;
        struct {
            enum tKnownFunction uWhichFunction;
        } FunctionSym;
    } un;
};
#define SymValue un.VariableSym.uSymValue
#define RememberedVal un.VariableSym.uRememberedVal
#define AssignCountS un.VariableSym.uAssignCountS
#define WhichFunction un.FunctionSym.uWhichFunction

typedef struct tSymTabEntry *pSym;

/* can't use enums for these since some compilers whine too much about type */
typedef char tUnaryOperator;
#define cNegate          0
#define cLogicalNot      1

typedef char tBinaryOperator;
#define cAdd             0        /* numeric */
#define cSubtract        1
#define cMultiply        2
#define cDivide          3
#define cDot             4
#define cCross           5
#define cDeriv           6
#define cIsEqual         7        /* logical */
#define cNotEqual        8
#define cLessThan        9
#define cGreaterThan     10
#define cLessOrEq        11
#define cGreaterOrEq     12
#define cLogicalAnd      13
#define cLogicalOr       14

typedef char tTernaryOperator;
#define cIfThenElse      0

enum tParentOp
  {cTopLevelOp, cSpecialOp, cPlusOp, cMinusOp, cMulOp, cDivOp, cNegateOp};

enum tNodeKind {
  cUnaryOperatorNode, cBinaryOperatorNode, cTernaryOperatorNode,
  cScalarConstNode, cArray1dNode, cArray2dNode, cFunctionCallNode, 
  cFunction2CallNode, cVarRefNode};

/*                 tExprNode                               
 *   All expressions are composed of one or more expression 
 * nodes.  Nodes can be either operators or operands.      
 * Operators can be unary (negate, not) binary (like +,-,*, dot, etc) or 
 * ternary (if-then-else). Operands can be scalar constants, or    
 * references to variables, or collections of expressions  
 * forming one- or two-dimensional arrays, or calls to     
 * functions.  Each expression node has a type, and all    
 * structured types (arrays) have a "base" type as for     
 * tSymTabEntries described above.  The type for an        
 * operator node is derived from the types of its          
 * operands.  As for symbols, the types vector and matrix  
 * are considered distinct from array1d and array2d, but   
 * they are implemented as arrays.                         
 *   Variable references can be indexed, to allow expres-  
 * sions in terms of particular elements of structured     
 * symbols, e.g. q[3].  Either one or two indices can be   
 * supplied.  A single index applied to a 2-dimensional    
 * variable selects a row of that variable, and has type   
 * 1-dimensional array of the same base type.              
 *   If the base type is a structured type, ITS base type  
 * must be a scalar or all hell will break loose.          
 */

struct tExprNode {
    tProtectionLevel Protection;
    struct tExprType NodeValueType;
    enum tNodeKind NodeKind;
    union {
        struct {
            tUnaryOperator uUnOp;
            pExpr uOpnd;
        } UnaryOperatorNode;
        struct {
            tBinaryOperator uBinOp;
            pExpr uLeftOpnd;
            pExpr uRtOpnd;
        } BinaryOperatorNode;
        struct {
            tTernaryOperator uTerOp;
            pExpr uFirstOpnd;
            pExpr uSecondOpnd;
            pExpr uThirdOpnd;
        } TernaryOperatorNode;
        struct {
            scalar uScalarValue;
        } ScalarConstNode;
        struct {
            struct tExprNode *uArray1dValue[1];
        } Array1dNode;
        struct {
            struct tExprNode *uArray2dValue[1];
        } Array2dNode;
        struct {
            pSym uVarRef;
            long uAssignCountE;  /* should match in symbol */
            Index_t uNumIndices;
            Index_t uIndices[cMaxNumIndices];
        } VarRefNode;
        struct {
            pSym uFuncVarRef;
            pExpr uFuncCallParm;
        } FunctionCallNode;
        struct {
            pSym uFunc2VarRef;
            pExpr uFunc2CallParm1;
            pExpr uFunc2CallParm2;
        } Function2CallNode;
    } un;
};
#define UnOp un.UnaryOperatorNode.uUnOp
#define Opnd un.UnaryOperatorNode.uOpnd
#define BinOp un.BinaryOperatorNode.uBinOp
#define LeftOpnd un.BinaryOperatorNode.uLeftOpnd
#define RtOpnd un.BinaryOperatorNode.uRtOpnd
#define TerOp un.TernaryOperatorNode.uTerOp
#define FirstOpnd un.TernaryOperatorNode.uFirstOpnd
#define SecondOpnd un.TernaryOperatorNode.uSecondOpnd
#define ThirdOpnd un.TernaryOperatorNode.uThirdOpnd
#define ScalarValue un.ScalarConstNode.uScalarValue
#define Array1dValue un.Array1dNode.uArray1dValue
#define Array2dValue un.Array2dNode.uArray2dValue
#define VarRef un.VarRefNode.uVarRef
#define AssignCountE un.VarRefNode.uAssignCountE
#define NumIndices un.VarRefNode.uNumIndices
#define Indices un.VarRefNode.uIndices
#define FuncVarRef un.FunctionCallNode.uFuncVarRef
#define FuncCallParm un.FunctionCallNode.uFuncCallParm
#define Func2VarRef un.Function2CallNode.uFunc2VarRef
#define Func2CallParm1 un.Function2CallNode.uFunc2CallParm1
#define Func2CallParm2 un.Function2CallNode.uFunc2CallParm2

#undef pExpr
typedef struct tExprNode *pExpr; /* now do it properly */

enum tExprOrder {cBeforeOrder = -1, cSameOrder = 0, cAfterOrder = 1};

/* These macros are used to access and set elements of 2d expressions.
 * This has to be faked since we don't know the dimensions of the 2d 
 * arrays until runtime.
 */
#define G2d(E,I,J)   ((E)->Array2dValue[(I)*(E)->NodeValueType.Dim2+(J)])
#define S2d(E,I,J,X) ((E)->Array2dValue[(I)*(E)->NodeValueType.Dim2+(J)] = (X))

typedef union {
    uintptr_t ut_dim[10];
    char *ut_sdim[10];
} dim_t;
struct user_type {
    char *name;
    unsigned type;
    dim_t dimu;
};

/* Holds a "packed" variable declaration. */
typedef struct packedvar_s {
    unsigned            pv_flags;     /* e.g. VT_INTEGER|VT_ARRAY */
    char               *pv_name;      /* name of variable */
    char               *pv_typename;  /* if VT_TYPENAME */
    unsigned            pv_cond;      /* if VT_COND */
    pSym               *pv_sym;       /* if VT_DSYM */
    struct user_type   *pv_usertype;  /* if VT_USER */
    dim_t               pv_dimu;      /* if VT_ARRAY|VECTOR|MATRIX */
    struct packedvar_s *pv_next;      /* next allocated packedvar */
} packedvar_t;


#if 0
/* external function declarations */

pExpr ADD(), APPLYBIN_OP(), B1(), B2(), B3(), BIN_OP(), CONST_PART(),
  COPY_EXPR(), CROSS(), DERIV(), DOT(), DO_ADD(), DO_DVD(),
  DO_MUL(), DO_SUB(), DVD(), EVAL(), INDX(), INDX11(), INDX12(), INDX2(),
  INDX21(), INDX22(), INUSE(), LIMIT_EXPR(), MAKE_EXPR_LIKE(), MAKE_ZERO(),
  MAKE_ZERO_LIKE(), MAT(), MATMUL(), MATRIX_IDENT(), MATRIX_ZERO(), MUL(),
  NEG(), NEWX(), NEW_1dARRAY(), NEW_2dARRAY(), NEW_MATX(), NEW_VECX(),
  OTHER_PART(), OUTER(), PERM(), SC(), SCALAR_ONE(), SCALAR_ZERO(), TER_OP(),
  SUB(), TILDA(), TRANSPOSE(), UNUSE(), UN_OP(), USEXIF(), VAL(), VAL1(),
  VAL2(), VEC(), VECTOR_ZERO(), VREF(), VREF1(), VREF2(), CALL_FUNC(),
  REMOVE_QUES();
#if defined(RALPHA)
#undef ABS
#endif
pExpr SINE(),COSINE(),ASINE(),ACOSN(),ABS(),SQRTT(),ATANG2();
pExpr AND(),OR(),NOT(),EQUAL(),NOTEQUAL(),LESSTHAN(),GREATERTHAN(),
      LESSOREQ(),GREATEROREQ(),NEARTO(),QUES(),QUESDVD();
pSym newsym();
long ADDOPS_USED(), ASGOPS_USED(), BYTES_USED(), DIVOPS_USED(), EXPR_COST(),
  MULOPS_USED();
Index_t LEN1d(), LEN2d();
enum tExprOrder ORDER();
char *PRINTNAME();

int fprintfcnt(FILE *stream, char *fmt, ...);
void eprintf(char *fmt, ...), efprintf(FILE *F, char *fmt, ...);
char *esprintf(char *ostr, char *fmt, ...);

double NUMVAL();
int IFTHEN(),IFELSE();
void IFEND();

#endif

/* Handy defines for use with CLEANVAR */
#define CL_FLUSHCOMPLEX  0        /* flush out complicated stuff only */
#define CL_FLUSHNONCONST 1        /* flush out all but constant stuff */
#define CL_FLUSHALL      2        /* flush out everything */

#define CL_FORGET        0        /* forget the value after flushing */
#define CL_REMEMBER      1        /* remember the value after flushing */

#ifdef RDEBUG
#define C_ASSERT(cond, num, pname)        c_assert(cond, num, pname)
#else
#define C_ASSERT(cond, num, pname)
#endif


/*==================*/
/* GLOBAL VARIABLES */
/*==================*/

/* User-supplied global values (set in INIT_CALC) */
extern long    gMaxExprLen;       /* Max length of printed expr. */
extern Index_t gMaxTemps;         /* Max no. temporary variables. */
extern int     gSinglePrecision;  /* use single precision if true, else double */
extern char    gPrefix[];         /* prefix for generated external symbol names */

extern pSym  gTempSym;     /* The symbol used for temporary variables TEMP(i) */
extern pExpr gScalarZero;  /* scalar constant node = 0.0 */
extern pExpr gScalarOne;   /* scalar constant node = 1.0 */
extern pExpr gVectorZero;

