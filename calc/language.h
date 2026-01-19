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

// Sherm 20260117
// #include "common/src/include/cpp_optional.h"
#define CC_C
// Sherm

/* structure used to describe most language-dependent data */
struct language {
        struct language *next;        /* links all languages */
        char name[8];                /* name of the language */
        char file_suffix[5];        /* filename suffix for source files */
        char subs_offset;        /* subscript offset (0 or 1) */
        short max_line;                /* max line length (minus len of cont_stmt) */
        char indent;                /* # of spaces per indent level */
        short stmt_lexlev;        /* base indent level of statement mode */
        char begin_subs[2];        /* start of subscript */
        char between_subs[3];        /* between subscripts */
        char end_subs[2];        /* end of subscript */
        char unknown_len[3];        /* symbol for decl of unknown-len 1d array */
        char *begin_stmt;        /* beginning of first statement line */
        char cont_stmt[3];        /* end of statement line that is continued */
        char *next_stmt;        /* start of subsequent statement line */
        char end_stmt[2];        /* end of last stment line (not including \n) */
        char *begin_cmt;        /* beginning of first block comment line,
                                   or all line comment lines */
        short cmt_indent;        /* offset between begin_cmt size and
                                   statement mode column of lexlev 1 */
        char end_cmt[5];        /* end of last comment line (including \n) */
        char spacer[2];                /* text of "spacer" line */
        char assign_op[5];        /* assignment operator */
        char lt_op[7];                /* `less than' operator */
        char le_op[7];                /* `less than or equal to' operator */
        char eq_op[7];                /* `equal to' operator */
        char ne_op[7];                /* `not equal to' operator */
        char ge_op[7];                /* `greater than or equal to' operator */
        char gt_op[7];                /* `greater than' operator */
        char and_op[15];        /* `logical and' operator */
        char or_op[15];                /* `logical or' operator */
        char not_op[15];        /* `logical not' operator */
        char dbl_prefix[2];        /* double math function prefix (D in FORTRAN) */
        char *int_decl;                /* integer declaration */
        char *single_decl;        /* single precision floating declaration */
        char *double_decl;        /* double precision floating declaration */
        char *procparm_decl;        /* procedure-as-parameter declaration */
        char ref[2];                /* create ref to var for pass by ref (C: &)*/
        char deref[2];                /* dereference vars and decl. pointers (C: *)*/
        char *proc_decl;        /* procedure decl before name */
        char *func_decl;        /* function decl before name */
        char *proc_decl_end;        /* procedure def'n after parameter decls */
        char *proc_decl_fwd;        /* procedure forward decl after param decls */
        char *proc_dbegin;        /* procedure local var decl begin */
        char *proc_dend;        /* procedure local var decl end */
        char *proc_sbegin;        /* procedure begin statements */
        char *proc_end;                /* procedure return and closing */
        char *proc_end_noret;        /* procedure close without explicit return */
        char *proc_call;        /* procedure call statement (if one) */
        char *proc_noargs_decl;        /* declare an empty arg list: (void) in ANSI C */
        char *proc_noargs_call;        /* call a proc with no args: () in C */
        char *stmt_if1;                /* "if (fabs(%s) > %r) %s" type statement */
        char stmt_if_b[5];        /* "if ("         start of multi-line if */
#define stmt_if2_b stmt_if_b
        char *stmt_if_then;        /* ") then {"               */
        char *stmt_if2_then;        /* " <relop> <val>) then {"               */
        char *stmt_if_else;        /* "} else {"                               */
#define stmt_if2_else stmt_if_else
        char *stmt_if_e;        /* "}"          end of multi-line if   */
#define stmt_if2_e stmt_if_e
        char *stmt_for_b;        /* beginning of for-type loop */
        char *stmt_forcnt_b;        /* beg of counting for loop 1..n or 0..n-1 */ 
        char *stmt_for_e;        /* end of for-type loop */
        char *stmt_for_e_brk;        /* end of for loop (with allowance for break) */
        char *stmt_until_b;        /* beginning of do ... while-type loop */
        char *stmt_until1;        /* end "} while (%s >= 1);" while type loop */
        char *stmt_while_b;        /* "while ("             */
        char *stmt_while_do;        /* " <relop> <val) do {" */
        char *stmt_while_e;        /* "}"                   */
        char *stmt_loop_b;        /* beginning of infinite loop */
        char *stmt_loop_e;        /* end infinite loop */
        char *stmt_loop_e_brk;        /* end infinite loop (w/allowance for break) */
        char *stmt_break;        /* break out of loop */
        char *stmt_label;        /* target for goto */
        char *stmt_goto;        /* the dread goto statement */
        char *stmt_freturn;        /* return from a function */
        char *stmt_return;        /* return from a (non-value returning) proc */
        char *struct_b;                /* "typedef struct {" or "type %s = record" */
        char *struct_e;                /* "} %s;" or "end;" */
        char *struct_ref;        /* %s.%s */
        char *initblk_b;        /* "= {" or ""             (per initialization) */
        char *initblk_e;        /* "}"   or ""                 */
        char *init_b;                /* "(* " or "data " (per line)        */
        char *init_m;                /* " *)" or "/"         */
        char *init_e;                /* ",\n"   or "/\n"         */
        char *func_sin;                /* names of various functions.  Note that */
        char *func_cos;                /* these do not include an initial type   */
        char *func_abs;                /* designator like `D' in FORTRAN.           */
        char *func_asin;
        char *func_acos;
        char *func_sqrt;
        char *func_atan2;
        int flags;
#define LANG_STMT_UPPER         0x001        /* convert statements to upper case */
#define LANG_CMT_UPPER          0x002        /* convert comments to upper case */
#define LANG_DECL2              0x004        /* use "type 2" decls, e.g. i,j: int */
#define LANG_COMMON_DECL        0x008        /* use FORTRAN common, not globals */
#define LANG_LINE_COMMENTS      0x010        /* line-oriented comments, as opposed
                                           to block-oriented */
#define LANG_SEPARATE_INIT      0x020   /* initialization of a variable
                                           is separate from its declaration */
#define LANG_VARDIM             0x040        /* language supports formal parameters
                                           with declared variable dimensions
                                           (C does not) */
#define LANG_COLMAJOR           0x080        /* 2-d arrays stored transposed 
                                           (only FORTRAN could be that dumb) */
#define LANG_FUNCVAL_IN_NAME    0x100        /* function return values are assigned
                                           to the function name rather than
                                           set with the return statement */
#define LANG_C_FAMILY           0x200        /* language in C family: K&R, ANSI, C++ */
#define LANG_DECL_IN_ARGLIST    0x400        /* type decls in function arglist */
};

extern struct language *Lang, FORTRAN_language, ADSIM_language,
  ADSIM_language_decl, KRC_language, ANSIC_language, Cpp_language,
  Pascal_language, Ada_language, Text_language;

#define first_language &Ada_language        /* head of linked list */
/* this prototype should be in calcprot.h, but that would require 
   including language.h before it in all .c files, which isn't so hot. 
   leave it here for now */
extern CC_C void SET_LANGUAGE(struct language* language);
