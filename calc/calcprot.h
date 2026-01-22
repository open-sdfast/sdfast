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

#define TRUE 1

// Sherm 20260117
// #include "common/src/include/cpp_optional.h"
#define CC_C
// Sherm

#define FALSE 0
#define MMBOOL int

extern CC_C pExpr ADD(pExpr E1, pExpr E2);
extern CC_C pExpr DO_ADD(register pExpr E1, register pExpr E2);
extern CC_C pExpr MAKE_EXPR_LIKE(pExpr E);
extern CC_C pExpr MAKE_ZERO(NodeValueType_t T);
extern CC_C pExpr MAKE_ZERO_LIKE(pExpr E);
extern CC_C pExpr MAT(matrix M);
extern CC_C pExpr NEWX(enum tNodeKind Kind, tIndex dim1, tIndex dim2);
extern CC_C pExpr NEW_1dARRAY(NodeValueType_t baseType, tIndex Len);
extern CC_C pExpr NEW_2dARRAY(NodeValueType_t baseType, tIndex dim1, tIndex dim2);
extern CC_C pExpr NEW_MATX(NodeValueType_t baseType);
extern CC_C pExpr NEW_VECX(NodeValueType_t baseType);
extern CC_C void DISPOSE_EXPR(pExpr E);
extern CC_C void DISPX(pExpr* X);
extern CC_C void ASSIGN(register pSym S, pExpr Val);
extern CC_C void ASSIGN_CLN(FILE* F, pSym S, pExpr Val);
extern CC_C MMBOOL IS_CONST(pExpr E);
extern CC_C MMBOOL IS_SCALAR(pExpr E);
extern CC_C MMBOOL IS_NEGOP(pExpr E);
extern CC_C MMBOOL IS_NEG(pExpr E);
extern CC_C MMBOOL IS_MUL(pExpr E);
extern CC_C MMBOOL IS_DVD(pExpr E);
extern CC_C MMBOOL IS_ADD(pExpr E);
extern CC_C MMBOOL IS_SUB(pExpr E);
extern CC_C MMBOOL IS_VREF(pExpr E);
extern CC_C MMBOOL SAME_TYPE(struct tExprType* T1, struct tExprType* T2);
extern CC_C MMBOOL SAME_VREF(register pExpr V1, register pExpr V2);
extern CC_C MMBOOL SAME_FUNC(pSym F, pSym G);
extern CC_C MMBOOL SAME_EXPR(pExpr E, pExpr F);
extern CC_C MMBOOL  IS_ZERO(pExpr E);
extern CC_C MMBOOL  IS_NRZERO(pExpr E);
extern CC_C MMBOOL  IS_ONE(pExpr E);
extern CC_C MMBOOL  IS_MINUSONE(pExpr E);
extern CC_C MMBOOL  IS_SIMPLE(register pExpr E);
extern CC_C void CLEANVAR(FILE* F, pSym V, char PrintAll, char Remember);
extern CC_C pExpr B1(void );
extern CC_C pExpr B2(void );
extern CC_C pExpr B3(void );
extern CC_C pExpr MATRIX_IDENT(void );
extern CC_C pExpr MATRIX_ZERO(void );
extern CC_C pExpr SC(scalar S);
extern CC_C pExpr SCALAR_ONE(void );
extern CC_C pExpr SCALAR_ZERO(void );
extern CC_C pExpr VEC(vector V);
extern CC_C pExpr VECTOR_ZERO(void );
extern CC_C char* PRINTNAME(pSym S);
extern CC_C double NUMVAL(pExpr cx);
extern CC_C pExpr CROSS(pExpr W, pExpr V);
extern CC_C int LOOKFORX_INX(pExpr X, pExpr E, char* id);
extern CC_C pSym newsym(SymbolKind_t kind);
extern CC_C void declare_vars(FILE* F, unsigned int decl_flags, ...);
extern CC_C void declare_proc(FILE* F, unsigned int decl_flags, ...);
extern CC_C void declare_type(FILE* F, unsigned int decl_flags, ...);
extern CC_C packedvar_t *packvar(unsigned int vtype, ...);
extern CC_C void DECL_FUNC(pSym* F, char* Name, enum tKnownFunction Which);
extern CC_C void DECL_TEMP(FILE* F);
extern CC_C pExpr DERIV(register pExpr E, register pExpr V);
extern CC_C tIndex LEN1d(pExpr E);
extern CC_C tIndex LEN2d(pExpr E);
extern CC_C void DIMTYPE(pSym V, tIndex Dim, struct tExprType* T);
extern CC_C pExpr DOT(register pExpr E1, register pExpr E2);
extern CC_C int NiceDTOC(scalar r, char* Out);
extern CC_C pExpr DO_DVD(register pExpr N, register pExpr D);
extern CC_C pExpr DVD(pExpr N, pExpr D);
extern CC_C int fprintfcnt(FILE* stream, char* fmt, ...);
extern CC_C void eprintf(char* fmt, ...);
extern CC_C void efprintf(FILE* F, char* fmt, ...);
extern CC_C char* esprintf(char* ostr, char* fmt, ...);
extern CC_C int CMT_MODE(void );
extern CC_C pExpr EVAL(register pExpr E);
extern CC_C pExpr CALL_FUNC(enum tKnownFunction Func, pExpr Parm);
extern CC_C pExpr CALL_FUNC2(enum tKnownFunction Func, pExpr Parm1, pExpr Parm2);
extern CC_C void FIXUP_VREFS(register pExpr E, pSym S, long NewCnt);
extern CC_C pExpr INDX(register pExpr E, tIndex I);
extern CC_C pExpr INDX11(pExpr E, tIndex I, tIndex J);
extern CC_C pExpr INDX12(pExpr E, tIndex I, tIndex J, tIndex K);
extern CC_C pExpr INDX2(register pExpr E, tIndex Row, tIndex Col);
extern CC_C pExpr INDX21(pExpr E, tIndex I, tIndex J, tIndex K);
extern CC_C pExpr INDX22(pExpr E, tIndex I, tIndex J, tIndex K, tIndex L);
extern CC_C void CHECKX(pExpr E);
extern CC_C void SINDX(register pExpr E, register tIndex I, pExpr Val);
extern CC_C void SINDX2(register pExpr E, tIndex I, tIndex J, pExpr Val);
extern CC_C void INIT_CALC(int MaxExprLen, int MaxTemps);
extern CC_C void SET_PRECISION(int SinglePrecision);
extern CC_C void SET_PREFIX(char* Prefix);
extern CC_C void fatal(char* s);
extern CC_C pExpr REMOVE_QUES(FILE* F, pExpr E, int NextTemp, int* HighestTemp);
extern CC_C pExpr AND(register pExpr E1, register pExpr E2);
extern CC_C pExpr OR(register pExpr E1, register pExpr E2);
extern CC_C pExpr NOT(register pExpr E1);
extern CC_C pExpr EQUAL(register pExpr E1, register pExpr E2);
extern CC_C pExpr NOTEQUAL(register pExpr E1, register pExpr E2);
extern CC_C pExpr LESSTHAN(register pExpr E1, register pExpr E2);
extern CC_C pExpr GREATERTHAN(register pExpr E1, register pExpr E2);
extern CC_C pExpr LESSOREQ(register pExpr E1, register pExpr E2);
extern CC_C pExpr GREATEROREQ(register pExpr E1, register pExpr E2);
extern CC_C pExpr NEARTO(register pExpr E1, register pExpr E2, double howClose);
extern CC_C pExpr QUES(register pExpr E1, register pExpr E2, register pExpr E3);
extern CC_C pExpr QUESDVD(register pExpr E1, register pExpr E2, register pExpr E3, register pExpr E4);
extern CC_C int IFTHEN(FILE* F, pExpr cond, int* elsetoo);
extern CC_C int IFELSE(FILE* F, pExpr cond, int* thentoo);
extern CC_C void IFEND(FILE* F, pExpr cond);
extern CC_C pExpr MATMUL(register pExpr M1, register pExpr M2);
extern CC_C pExpr DO_MUL(pExpr E1, pExpr E2);
extern CC_C pExpr MUL(pExpr E1, pExpr E2);
extern CC_C pExpr NEG(register pExpr E);
extern CC_C enum tExprOrder ORDER(register pExpr E, register pExpr F);
extern CC_C int strcmp_ci(register char* s1, register char* s2);
extern CC_C pExpr OUTER(register pExpr E1, register pExpr E2);
extern CC_C pExpr CONST_PART(pExpr F);
extern CC_C pExpr OTHER_PART(pExpr F);
extern CC_C pExpr LIMIT_EXPR(FILE* F, pExpr E, int NextTemp, int* HighestTemp);
extern CC_C void  PRINT_ASSN(FILE* F, char* Vname, pExpr E, int byref);
extern CC_C void  PRINT_ASSN1(FILE* F, char* Vname, tIndex i, pExpr E);
extern CC_C void  PRINT_ASSN2(FILE* F, char* Vname, tIndex i, tIndex j, pExpr E);
extern CC_C void  PRINT_ASSN3(FILE* F, char* Vname, tIndex i, tIndex j, tIndex k, pExpr E);
extern CC_C void  PRINT_VREF(FILE* F, register pExpr V);
extern CC_C void  PRINT_E(FILE* F, pExpr E);
extern CC_C void  PRINT_EXPR(FILE* F, pExpr E);
extern CC_C void  PRINT_TEMP_EXPR(FILE* F, tIndex i, pExpr E);
extern CC_C pExpr INUSE(pExpr E);
extern CC_C pExpr PERM(pExpr E);
extern CC_C pExpr UNUSE(pExpr E);
extern CC_C void  PROTECT(pExpr E, tProtectionLevel Lev);
extern CC_C void  UNPROTECT(pExpr E, tProtectionLevel Lev);
extern CC_C void  SHOW_TYPE(register pExpr E);
extern CC_C pExpr SINE(pExpr E);
extern CC_C pExpr COSINE(pExpr E);
extern CC_C pExpr ASINE(pExpr E);
extern CC_C pExpr ACOSN(pExpr E);
extern CC_C pExpr ABS(pExpr E);
extern CC_C pExpr SQRTT(pExpr E);
extern CC_C pExpr ATANG2(pExpr E1, pExpr E2);
extern CC_C pExpr CALL_KNOWN_FUNC(enum tKnownFunction func, pExpr arg);
extern CC_C pExpr CALL_KNOWN_FUNC2(enum tKnownFunction func, pExpr arg1, pExpr arg2);
extern CC_C long ADDOPS_USED(void );
extern CC_C long ASGOPS_USED(void );
extern CC_C int64_t BYTES_USED(void );
extern CC_C long DIVOPS_USED(void );
extern CC_C long MULOPS_USED(void );
extern CC_C void  RESET_OPS(void );
extern CC_C void  SHOW_COUNTS(void );
extern CC_C void  RESET_COUNTS(void );
extern CC_C pExpr DO_SUB(register pExpr E1, register pExpr E2);
extern CC_C pExpr SUB(pExpr E1, pExpr E2);
extern CC_C pExpr TILDA(register pExpr V);
extern CC_C pExpr TRANSPOSE(register pExpr E);
extern CC_C void  SCALAR_TYPE(register struct tExprType* T);
extern CC_C void  VECTOR_TYPE(NodeValueType_t baseType, register struct tExprType* T);
extern CC_C void  MATRIX_TYPE(NodeValueType_t baseType, register struct tExprType* T);
extern CC_C void  ARRAY1d_TYPE(NodeValueType_t baseType, tIndex dim1, register struct tExprType* T);
extern CC_C void  ARRAY2d_TYPE(NodeValueType_t baseType, tIndex dim1, tIndex dim2, register struct tExprType* T);
extern CC_C pExpr USEXIF(register pExpr X, pExpr VX);
extern CC_C pExpr VAL(pSym V);
extern CC_C pExpr VAL1(pSym V, tIndex I);
extern CC_C pExpr VAL2(pSym V, tIndex I, tIndex J);
extern CC_C pExpr VREF(register pSym V);
extern CC_C pExpr VREF1(register pSym V, tIndex Index);
extern CC_C pExpr VREF2(register pSym V, tIndex Row, tIndex Col);
extern CC_C pExpr UN_OP(tUnaryOperator Op, pExpr E);
extern CC_C pExpr BIN_OP(tBinaryOperator Op, pExpr E1, pExpr E2);
extern CC_C pExpr TER_OP(tTernaryOperator Op, pExpr E1, pExpr E2, pExpr E3);
extern CC_C pExpr COPY_EXPR(pExpr E);
extern CC_C long EXPR_COST(pExpr E);
extern CC_C pExpr APPLYBIN_OP(tBinaryOperator Op, register pExpr E1, register pExpr E2);
extern CC_C void c_assert(int cond,int num, char *pname);
