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

/* Convenient defines for use in language-independent code generation.
 * This file is not included by any of the `calc' code.  It is just
 * for users of calc to include.
 */

/* Logical operators. */

#define LT Lang->lt_op
#define LE Lang->le_op
#define EQ Lang->eq_op
#define NE Lang->ne_op
#define GE Lang->ge_op
#define GT Lang->gt_op
#define AND_OP Lang->and_op      /* avoid confusion with AND,OR,NOT */
#define OR_OP Lang->or_op        /*   calculator functions          */
#define NOT_OP Lang->not_op

/* Lots of different kinds of looping constructs. */

#define LOOP(lab) efprintf(F, Lang->stmt_loop_b, lab)
#define ENDLOOP(lab) efprintf(F, Lang->stmt_loop_e, lab)
#define ENDLOOPBRK(lab,brklab) efprintf(F, Lang->stmt_loop_e_brk, lab, brklab)
#define BREAK(lab) efprintf(F, Lang->stmt_break, lab)
#define GOTO(namlab,numlab) efprintf(F, Lang->stmt_goto, namlab, numlab)
#define LABEL(namlab,numlab) efprintf(F, Lang->stmt_label, namlab, numlab)
#define FOR(lab,var,low,high) efprintf(F, Lang->stmt_for_b, lab, var, low, high)
#define FORCNT(lab,var,high) efprintf(F, Lang->stmt_forcnt_b, lab, var, high)
#define ENDFOR(lab) efprintf(F, Lang->stmt_for_e, lab);
#define ENDFORBRK(lab,brklab) efprintf(F, Lang->stmt_for_e_brk, lab, brklab)
#define WHILE(lab, exp1, op, exp2) {\
              efprintf(F, Lang->stmt_while_b, lab);\
              efprintf(F, exp1);\
              efprintf(F, Lang->stmt_while_do, op, exp2);}
#define ENDWHILE(lab) efprintf(F, Lang->stmt_while_e, lab);

/* Conditional execution constructs. */

#define IF(exp1, op, exp2) {\
           efprintf(F, Lang->stmt_if_b);\
           efprintf(F, exp1);\
           efprintf(F, "%s%s", op, exp2);}
#define IFCOND efprintf(F, Lang->stmt_if_b);
#define THEN efprintf(F, Lang->stmt_if_then);
#define ELSE efprintf(F, Lang->stmt_if_else);
#define ENDIF efprintf(F, Lang->stmt_if_e)

/* Variable assignments. */

#define SET(var,val) {efprintf(F, var); efprintf(F,"%="); efprintf(F,val);\
                      efprintf(F, "%;\n");}
#define ISET(var,val) {efprintf(F, var); efprintf(F,"%=%d%;\n", val);}
#define RSET(var,val) {efprintf(F, var); efprintf(F,"%=%r%;\n", val);}

/* Call a non-value returning subroutine.  This will generate an initial
 * "call" in FORTRAN.  In C, we conveniently define %R to expand either
 * to the null string (in most languages) or "&" in C, as an aid to those
 * attempting to pass arguments by reference.  Simply precede these parameters
 * by %R (or %@R, if you're worried about sccs).  The right thing will happen.
 *
 * CALL0 calls a routine that has no arguments.
 *
 * CALL1 calls a routine with one argument which must be substituted into
 * the call string with efprintf().  The routine may have an arbitrary
 * number of arguments; the 1 refers to the number which have substitution
 * slots in the passsed-in fcall string.
 *
 * CALL2 is the same as CALL1 but expects two substitutions.  I bet you
 * can guess what CALL3 does.
 */
#define CALL(fcall) {\
             efprintf(F, "%:Rs%s", Lang->ref, Lang->proc_call);\
             efprintf(F, fcall);\
             efprintf(F, "%;\n");}
#define CALL0(fcall) {\
              efprintf(F, "%s", Lang->proc_call);\
              efprintf(F, fcall);\
              efprintf(F, "%s", Lang->proc_noargs_call);\
              efprintf(F, "%;\n");}
#define CALL1(fcall,arg) {\
              efprintf(F, "%:Rs%s", Lang->ref, Lang->proc_call);\
              efprintf(F, fcall, arg);\
              efprintf(F, "%;\n");}
#define CALL2(fcall,arg1,arg2) {\
              efprintf(F, "%:Rs%s", Lang->ref, Lang->proc_call);\
              efprintf(F, fcall, arg1, arg2);\
              efprintf(F, "%;\n");}
#define CALL3(fcall,arg1,arg2,arg3) {\
              efprintf(F, "%:Rs%s", Lang->ref, Lang->proc_call);\
              efprintf(F, fcall, arg1, arg2, arg3);\
              efprintf(F, "%;\n");}

/* Return from function `fname' with function value set to val.  Works
 * right even if fname begins with %A.   If you use this at the end of
 * a function, you should close the function with Lang->proc_end_noret
 * to avoid a duplicate `return' statement in Fortran.
 */
#define FRETURN(fname,val) {\
            if (Lang->flags & LANG_FUNCVAL_IN_NAME) {\
                efprintf(F, fname);\
                efprintf(F, "%=%s%;\n", val);\
            }\
            efprintf(F, Lang->stmt_freturn, val);}

/* Return from a (non-value returning) procedure. */
#define RETURN efprintf(F, Lang->stmt_return)

/* Put out an `end of statement' */
#define ENDSTMT efprintf(F, "%;\n");

/* Reference, set or increment a variable which is a passed-by-reference 
 * parameter.  In C, which is quite fussy about these things you get code
 * like this:
 *
 *  REF:    *var
 *  SETREF: *var = val
 *  INCREF: *var = *var + val
 */
#define REF(var) efprintf(F, "%s%s", Lang->deref, var)
#define SETREF(var,val) efprintf(F, "%s%s%=%s%;\n", Lang->deref, var, val)
#define INCREF(var,val)\
               efprintf(F, "%s%s%=%s%s+%s%;\n", \
               Lang->deref,var,Lang->deref,var,val)

