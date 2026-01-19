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

#ifdef THINK_C
#  include "::calc:calcprot.h"
#else
#  include "../calc/calcprot.h"
#endif

// Sherm 20260117
// #include "common/src/include/cpp_optional.h"
#define C_C
// Sherm

extern CC_C void UPSTR(char* S);
extern CC_C void TIME_STAMP(string32 DateAndTime);
extern CC_C int ADD_EXT_TO_FILENAME(char* S, int maxlen, char* Ext);
extern CC_C void GET_BASE_NAME(char* instr, char* outstr);
extern CC_C int READSTR(FILE* F, int len, string32 s);
extern CC_C int READSTR32(FILE* F, string32 s);
extern CC_C char DUMP_ERR_MSG(string10 kw, FILE* outp, int optnum, int y, int m, int d, string20 machID);
extern CC_C char DUMP_WARN_MSG(string10 kw, FILE* inp, int* optnum, long* expdate, string20 machID);
extern CC_C int CH2INT(char ch);
extern CC_C int FINDCONSTRAINT(SystemInfo_t* SystemInfo, char* Constraint);
extern CC_C int FINDBODY(SystemInfo_t* SystemInfo, char* Body);
extern CC_C int FINDJOINT(SystemInfo_t* SystemInfo, char* Jname);
extern CC_C void INIT_BODY(SystemInfo_t* SystemInfo, Index_t BodyNum, char* Name);
extern CC_C void INIT_LOOP(SystemInfo_t* SystemInfo, Index_t LoopNum, Index_t BodyNum, char* Name);
extern CC_C void INIT_CONSTRAINT(SystemInfo_t* SystemInfo, Index_t ConstraintNum, char* Name, ConstraintKind_t ConstraintKind);
extern CC_C long STRCMP32(void);
extern CC_C long GETNUMTIME(void);
extern CC_C double CPU_SECONDS(void);
extern CC_C void GETDATE(register string11 s);
extern CC_C void GETTIME(string11 s);
extern CC_C int openr(FILE** f, char* fn);
extern CC_C int openw(FILE** f, char* fn);
extern CC_C int CLOSE_FILE(FILE* f);
extern CC_C void GETMACHINEID(string20 machID);
extern CC_C char FIND_EQUAL(FILE* System);
extern CC_C char GET_INT(FILE* System, long* IntVal);
extern CC_C char GET_REAL(FILE* System, double* RealVal);
extern CC_C char GET_REAL_EXPR(FILE* System, expr* Rexpr, flags_t* QuesFlg);
extern CC_C char EQUAL_NEXT(FILE* System, int* IsEqual);
extern CC_C char NUMERIC_NEXT(FILE* System, int* IsNumeric);
extern CC_C char READ_ID(FILE* System, string32 Ident);
extern CC_C char WHATS_NEXT(FILE* System, ItemType_t* Next);
extern CC_C void ASSERT(int cond, int num, char* pname);
extern CC_C void USER_ERR(void);
extern CC_C void USER_WARN(void);
extern CC_C int LOOKUP(char* ReservedWords[], char* Id);
extern CC_C void parse_cmdline(int argc, char* argv[]);
extern CC_C int GET_INPUTS(SystemInfo_t* sys);
extern CC_C void genname(int prompting, char* prompt, char* prefix, char* basename, char* ext, char* langsuf, char* name);
extern CC_C void PROCESS_INPUTS(SystemInfo_t* SystemInfo);
extern CC_C char DO_INERTIA(FILE* System, SystemInfo_t* SystemInfo);
extern CC_C char PROCESS_VAR(FILE* System, register SystemInfo_t* SystemInfo, char* ReservedWords[], int IdNum, int* loopjoint);
extern CC_C char READ_SYSTEM(FILE* System, SystemInfo_t* SystemInfo);
extern CC_C void declare_sys_types(FILE* F, int decl_flags, register SystemInfo_t* sys);
extern CC_C void declare_input_parms(FILE* F, int decl_flags, register SystemInfo_t* sys);
extern CC_C void FREE_SYSTEM(SystemInfo_t* SystemInfo);
