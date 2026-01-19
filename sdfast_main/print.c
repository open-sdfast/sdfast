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

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#include "../calc/gencode.h"

/* PRINT_SUBR_STAMP 
 * 
 * Print time, date and copyright stamps into a subroutine or at the
 * beginning of a generated file.  We assume that efprintf is in comment mode.
 */
void PRINT_SUBR_STAMP(FILE *F)
{
    string32 DateTime;
    string20 machineID;
    extern char VersionNumber[]; 
    extern int gProgramSerialNo;

    TIME_STAMP(DateTime);
    GETMACHINEID(machineID);
    efprintf(F, "Generated %s by SD/FAST, %s formulation\n", DateTime,
       sdfast_opt.formulation == OPT_KANE 
       || sdfast_opt.formulation == OPT_DEFAULT  ?         "Kane's" :
       sdfast_opt.formulation == OPT_ORDERN ?                 "Order(N)" :
       sdfast_opt.formulation == OPT_EXP ?                 "experimental" :
       sdfast_opt.formulation == OPT_EXP2 ?                 "experimental2" :
                                                "???");
    efprintf(F, "(sdfast %s #%05d) on machine ID %s\n", VersionNumber,
      gProgramSerialNo, machineID);
    efprintf(F,
      "Copyright (c) 1990-1997 Symbolic Dynamics, Inc.\n");
    efprintf(F, "Copyright (c) 1990-1997 Parametric Technology Corp.\n");
    efprintf(F, "\
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.\n\
Government is subject to restrictions as set forth in subparagraph\n\
(c)(1)(ii) of the Rights in Technical Data and Computer Software\n\
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA\n\
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041\n");

}

/* PRINT_STUB
 *
 * In case you have to put out a subroutine stub, print its declaration,
 * call this routine, and return.
 */
void PRINT_STUB(FILE *F)
{
    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    efprintf(F, Lang->proc_end);
}

/* ZERO_OPCNTS
 * START_COMPSUB
 * END_COMPSUB
 * 
 * For generated routines containing a substantial amount of computation,
 * we want to track the number of operations generated, and the amount of
 * memory and CPU time used in generating the routine.  Any operations
 * which are generated through the calculator's PRINT_EXPR routines are
 * counted automatically.  In addition, explicit code is often used to
 * output some arithmetical operations.  These must be counted explicitly
 * in the calling routine, and the counts are passed to END_COMPSUB which
 * will combine them with the automatically-generated counts.
 *
 * Opcounts are reported back to the main program in an `opstats' structure.
 * This structure is passed in to START_COMPSUB for initialization and to
 * END_COMPSUB to be filled in.  All fields of opstats are initialized to
 * zero by START_COMPSUB, but only the op count fields are filled in by
 * END_COMPSUB.  Subroutine call counts must be filled in explicitly by
 * the caller.
 *
 * We use globals here to remember the state of things at the start of
 * the routine so that END_COMPSUB can calculate the additions made just
 * for the new routine.  Make sure that no two routines are every being
 * generated at once!
 */

float Ctime;
long Addops, Mulops, Divops, Asgops, BytesUsed;

void ZERO_OPCNTS(opstats_t *opcnt)
{
    opcnt->nadd  = 0;
    opcnt->nmul  = 0;
    opcnt->ndiv  = 0;
    opcnt->nassign  = 0;
    opcnt->nldudcomp = 0;
    opcnt->nldubsl   = 0;
    opcnt->nldubsd   = 0;
    opcnt->nmfrc = 0;
    opcnt->nlhs  = 0;
    opcnt->nrhs  = 0;
    opcnt->nfs0  = 0;
    opcnt->nfsmult = 0;
    opcnt->nfsfull = 0;
    opcnt->nqrdcomp = 0;
    opcnt->nqrbslv  = 0;
    opcnt->ndoww   = 0;
    opcnt->ndoltau = 0;
    opcnt->ndoiner = 0;
    opcnt->ndofs0  = 0;
    opcnt->ndomm   = 0;
    opcnt->ndommldu = 0;
    opcnt->ndovpk  = 0;
    opcnt->nfulltrq  = 0;
    opcnt->nudot0  = 0;
    opcnt->nudotm  = 0;
}

void START_COMPSUB(opstats_t *opcnt)
{
    Ctime = CPU_SECONDS();
    Addops = ADDOPS_USED();
    Mulops = MULOPS_USED();
    Divops = DIVOPS_USED();
    Asgops = ASGOPS_USED();
    BytesUsed = BYTES_USED();
    ZERO_OPCNTS(opcnt);
}

void END_COMPSUB(FILE *F,
            opstats_t *opcnt,
            long ladd,
            long lmul,
            long ldiv,
            long lasg)
{
    opcnt->nadd = ADDOPS_USED()-Addops+ladd;
    opcnt->nmul = MULOPS_USED()-Mulops+lmul;
    opcnt->ndiv = DIVOPS_USED()-Divops+ldiv;
    opcnt->nassign = ASGOPS_USED()-Asgops+lasg;

    /* Print the subroutine footing (e.g., RETURN and END) and statistics. */
    efprintf(F, "%{\n\
 Used %.2f seconds CPU time,\n\
 %ld additional bytes of memory.\n\
 Equations contain %4ld adds/subtracts/negates\n\
                   %4ld multiplies\n\
                   %4ld divides\n\
                   %4ld assignments\n%}",
      CPU_SECONDS() - Ctime, BYTES_USED()-BytesUsed,
      opcnt->nadd,opcnt->nmul,opcnt->ndiv,opcnt->nassign);
    fflush(F);
}

/*
 * This handy function is used when generating arrays of vectors.
 * One of the vector elements has been computed in
 * expression `value', which is a temporary (i.e., disposable) expression.
 * We look at the three elements of value, and output assignments according
 * to whether they meet the conditions of the cl_flag, which is used in the
 * same manner as CLEANVAR.
 *
 * On output, var_expr[which] is set either to the 
 * value itself, or to an appropriately-indexed VREF to symbol `var'.
 * If remember=CL_REMEMBER we'll output the assignment but keep the
 * value around if it is IS_SIMPLE, even if it is non-constant.  Remember
 * is really only useful when cl_flag == CL_FLUSHALL.
 *
 * On return, `value' will have been disposed if necessary.
 */
void FLUSH_VEC_GEN(FILE *F,
              int cl_flag, 
              int remember, 
              sym var,
              int which,
              expr var_expr, 
              expr value)
{
    int i;
    expr temp,outx,vref;

    value = INUSE(value);
    outx = INUSE(NEW_VECX(cScalarVal));
    for (i=0; i<3; i++) {
        temp = INDX(value,i);
        vref = INDX(VREF1(var,which),i);
        if (!SAME_EXPR(temp,vref))
            if (cl_flag == CL_FLUSHALL
                    || !IS_SIMPLE(temp)
                    || (cl_flag == CL_FLUSHNONCONST && !IS_CONST(temp)))
            {
                PRINT_ASSN2(F, PRINTNAME(var), which, i, temp);
            }
        if (!IS_SIMPLE(temp)
            || (remember != CL_REMEMBER 
                && ((cl_flag == CL_FLUSHNONCONST || cl_flag == CL_FLUSHALL) 
                    && !IS_CONST(temp)))
           )
        {
            /* replace element's value with a self-reference */
            SINDX(outx, i, vref);
        } else
            SINDX(outx, i, temp);
        DISPOSE_EXPR(temp);
        DISPOSE_EXPR(vref);
    }
    DISPOSE_EXPR(UNUSE(value));
    SINDX(var_expr, which, UNUSE(outx));
}

/*
 * This function is just like used when generating arrays of vectors, 
 * except its for arrays of matrices.
 * One of the matrix elements has been computed in
 * expression `value', which is a temporary (i.e., disposable) expression.
 * We look at the nine elements of value, and output assignments according
 * to whether they meet the conditions of the cl_flag, which is used in the
 * same manner as CLEANVAR.
 *
 * On output, var_expr[which] is set either to the 
 * value itself, or to an appropriately-indexed VREF to symbol `var'.
 *
 * On return, `value' will have been disposed if necessary.
 */
void FLUSH_MAT_GEN(FILE *F,
              int cl_flag, 
              int remember, 
              sym var,
              int which,
              expr var_expr,
              expr  value)
{
    int i,j;
    expr temp,outx,vref;

    value = INUSE(value);
    outx = INUSE(NEW_MATX(cScalarVal));
    for (i=0; i<3; i++)
        for (j=0; j<3; j++) {
            temp = INDX2(value,i,j);
            vref = INDX2(VREF1(var,which),i,j);
            if (!SAME_EXPR(temp,vref))
                if (cl_flag == CL_FLUSHALL
                        || !IS_SIMPLE(temp)
                        || (cl_flag == CL_FLUSHNONCONST && !IS_CONST(temp)))
                {
                    PRINT_ASSN3(F, PRINTNAME(var), which, i, j, temp);
                }
            if (!IS_SIMPLE(temp)
                || (remember != CL_REMEMBER 
                    && ((cl_flag == CL_FLUSHNONCONST || cl_flag == CL_FLUSHALL) 
                        && !IS_CONST(temp)))
               )
            {
                /* replace element's value with a self-reference */
                SINDX2(outx, i, j, vref);
            } else
                SINDX2(outx, i, j, temp);
            DISPOSE_EXPR(temp);
            DISPOSE_EXPR(vref);
        }
    DISPOSE_EXPR(UNUSE(value));
    SINDX(var_expr, which, UNUSE(outx));
}

void FLUSH_VEC(FILE *F,
          sym var,
          int which,
          expr var_expr, 
          expr value)
{
    FLUSH_VEC_GEN(F, CL_FLUSHCOMPLEX, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_VEC_NONCONST(FILE *F,
                   sym var,
                   int which,
                   expr var_expr,
                   expr  value)
{
    FLUSH_VEC_GEN(F, CL_FLUSHNONCONST, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_VEC_ALL(FILE *F,
              sym var,
              int which,
              expr var_expr,
              expr  value)
{
    FLUSH_VEC_GEN(F, CL_FLUSHALL, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_VEC_ALL_SAVE(FILE *F,
                   sym var,
                   int which,
                   expr var_expr,
                   expr  value)
{
    FLUSH_VEC_GEN(F, CL_FLUSHALL, CL_REMEMBER, var, which, var_expr, value);
}

void FLUSH_MAT(FILE *F,
          sym var,
          int which,
          expr var_expr,
          expr value)
{
    FLUSH_MAT_GEN(F, CL_FLUSHCOMPLEX, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_MAT_NONCONST(FILE *F,
                   sym var,
                   int which,
                   expr var_expr,
                   expr value)
{
    FLUSH_MAT_GEN(F, CL_FLUSHNONCONST, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_MAT_ALL(FILE *F,
              sym var,
              int which,
              expr var_expr,
              expr value)
{
    FLUSH_MAT_GEN(F, CL_FLUSHALL, CL_FORGET, var, which, var_expr, value);
}

void FLUSH_MAT_ALL_SAVE(FILE *F,
                   sym var,
                   int which,
                   expr var_expr,
                   expr value)
{
    FLUSH_MAT_GEN(F, CL_FLUSHALL, CL_REMEMBER, var, which, var_expr, value);
}

/* TWOINDX
 * 
 * Generates a reference to an element of a two-dimensional array
 * of indeterminate size; i.e., one which was passed in as an argument
 * to the routine being generated.  In some cases, the actual dimensions
 * of the array are known and were also passed to the subroutine.  If that
 * is the case (as indicated by known!=0) and the language has 
 * attribute LANG_VARDIM, we'll generate
 * a nice reference like V[r,c].  Otherwise, we'll treat the symbol as
 * a one-dimensional array and index like V[r*cdim+c] (or V[r+c*rdim]
 * if LANG_COLMAJOR).  In the one-dimensional case we'll adjust one of r 
 * and c by subs_offset if necessary.
 * 
 * You are expected to have declared the array appropriately for the 
 * language.
 *
 * The strings `pre' and `post' are put at the beginning and end (resp)
 * of the efprintf format string surrounding the array reference.
 *
 * `Indx' is the index into vname at which the desired variable begins.
 * This is used when packing several variables into a single array.
 * A non-null indx forces one-dimensional array indexing.  So typical
 * indexing looks like V[indx+r*cdim+c] in count-from-zero languages
 * or V[indx-1+r*cdim+c] in count-from-one languages.
 *
 * If `vname' is null, we'll suppress printing of the brackets around
 * the index expression, so you can get things like indx+r*cdim+c which
 * you can then assign to some variable.
 *
 * If any of indx, rdim, cdim, r, or c is an expression rather than just a
 * constant or variable name , make sure it is parenthesized.
 */

void TWOINDX(FILE *F,
        char *pre,
        char *vname,
        char  *indx,
        int known,
        char  *rdim,
        char  *cdim,
        char  *r,
        char  *c,
        char  *post)
{
    char fmt[100];

    sprintf(fmt, "%s", pre);
    efprintf(F, fmt);
    if (vname[0]) 
        efprintf(F, "%s%(", vname);
    if (indx[0] == '\0' && known && (Lang->flags & LANG_VARDIM)) {
        /* can just use a simple 2-d reference */
        efprintf(F, "%s%,%s", r, c);
    } else {
        /* must access as 1-d */
        if (indx[0])
            efprintf(F, "%s%s+", indx, Lang->subs_offset?"-1":"");
        if (Lang->flags & LANG_COLMAJOR) 
            efprintf(F, "%s+%s%s%s*%s", r,
                Lang->subs_offset?"(":"", c, Lang->subs_offset?"-1)":"",
                rdim);
        else
            efprintf(F, "%s%s%s*%s+%s", 
                Lang->subs_offset?"(":"", r, Lang->subs_offset?"-1)":"",
                cdim, c);
    }
    if (vname[0])
        efprintf(F, "%)");
    sprintf(fmt, "%s", post);
    efprintf(F, fmt);
}

/* ONEINDX
 * 
 * Stripped-down version of TWOINDX for use with 1-d arrays.
 * 
 * The strings `pre' and `post' are put at the beginning and end (resp)
 * of the efprintf format string surrounding the array reference.
 *
 * `Indx' is the index into vname at which the desired variable begins.
 * This is used when packing several variables into a single array.
 *
 * If `vname' is null, we'll suppress printing of the brackets around
 * the index expression, so you can get things like indx-1+r which
 * you can then assign to some variable.
 *
 * If indx or r is an expression rather than just a
 * constant or variable name , make sure it is parenthesized.
 */

void ONEINDX(FILE *F,
        char *pre,
        char *vname,
        char *indx,
        char *r,
        char *post)
{
    char fmt[100];

    sprintf(fmt, "%s", pre);
    efprintf(F, fmt);
    if (vname[0]) 
        efprintf(F, "%s%(", vname);
    if (indx[0])
        efprintf(F, "%s%s+", indx, Lang->subs_offset?"-1":"");
    efprintf(F, "%s", r);
    if (vname[0])
        efprintf(F, "%)");
    sprintf(fmt, "%s", post);
    efprintf(F, fmt);
}

/* SETERR
 *
 * Outputs code to post an error number and routine name.  Nothing
 * happens if there was already an error posted.
 */
void SETERR(FILE *F,
       int routine, 
       int errnum)
{
    char callstr[50];

    esprintf(callstr, "%Aseterr(%d,%d)", routine, errnum);
    CALL(callstr);
}

/* CHECK_STATE
 * 
 * Generate code to make sure the current state is one of the allowed
 * states for the current routine.  CHECK_STATE can handle one or
 * two states.  If you only have one legal one, use ST_NOSTATE as
 * the second one.
 */
void CHECK_STATE(FILE *F,
            int st1,
            int st2,
            int routine,
            int errnum)
{
    IFCOND 
      if (st2 == ST_NOSTATE)
          efprintf(F, "roustate%s%d", NE, st1);
      else 
          efprintf(F, "(roustate%s%d)%s(roustate%s%d)",NE,st1, AND_OP,NE,st2);
    THEN 
      SETERR(F, routine, errnum);
      RETURN;
    ENDIF;
}

/* DECL_CHKBNUM, JNUM, JAXIS, JPIN, UCNUM
 *
 * If the current language requires it, we'll declare the integer functions
 * sdchkbnum, sdchkjnum, etc. when these routines are called.
 */
static void
decl_intfunc(FILE *F,
             char *rouname)
{
    char callstr[50];

    if (Lang == &FORTRAN_language) {
        esprintf(callstr, "%@A%s", rouname);
        declare_vars(F, 0,
          VT_INTEGER, callstr,
          0);
    }
}

void DECL_CHKBNUM(FILE *F)
{
    decl_intfunc(F, "chkbnum");
}

void DECL_CHKJNUM(FILE *F)
{
    decl_intfunc(F, "chkjnum");
}

void DECL_CHKJAXIS(FILE *F)
{
    decl_intfunc(F, "chkjaxis");
}

void DECL_CHKJPIN(FILE *F)
{
    decl_intfunc(F, "chkjpin");
}

void DECL_CHKUCNUM(FILE *F)
{
    decl_intfunc(F, "chkucnum");
}


/* CHECK_BNUM, JNUM, JAXIS, JPIN, UCNUM
 * 
 * Generate code to make sure the body number passed in to the current
 * routine is in range.  If not, an error is posted and the routine
 * will return.  Before calling this, make sure that DECL_CHKBNUM
 * has been called so that the sdchkbnum() function is declared if necessary.
 *
 * Similar comments apply to JNUM, JAXIS, and UCNUM.
 */
void CHECK_BNUM(FILE *F,
           char *bnumname, /* name of the parameter to be checked */
           int routine)
{
    IFCOND efprintf(F, "%Achkbnum(%d,%s)%s0", routine, bnumname, NE);
      THEN RETURN;
    ENDIF;
}
void CHECK_JNUM(FILE *F,
           char *jnumname,
           int routine)
{
    IFCOND efprintf(F, "%Achkjnum(%d,%s)%s0", routine, jnumname, NE);
      THEN RETURN;
    ENDIF;
}
void CHECK_JAXIS(FILE *F,
            char *jnumname,
            char  *axnumname,
            int routine)
{
    IFCOND efprintf(F, "%Achkjaxis(%d,%s,%s)%s0", 
                    routine, jnumname, axnumname, NE);
      THEN RETURN;
    ENDIF;
}
void CHECK_JPIN(FILE *F,
           char *jnumname,
           char  *pinnoname,
           int routine)
{
    IFCOND efprintf(F, "%Achkjpin(%d,%s,%s)%s0", 
                    routine, jnumname, pinnoname, NE);
      THEN RETURN;
    ENDIF;
}
void CHECK_UCNUM(FILE *F,
            char *ucnumname,
            int routine)
{
    IFCOND efprintf(F, "%Achkucnum(%d,%s)%s0", routine, ucnumname, NE);
      THEN RETURN;
    ENDIF;
}

/* PRINTERR
 *
 * If an error is discovered during equation generation, this routine
 * can print an appropriate message.  The error
 * occurred at joint (or body) jointno.  `what' is either "body" or
 * "loop joint".  `name' is the name of the (outboard) body associated
 * with this joint.  You should terminate SD/FAST after calling this
 * routine.
 */
void PRINTERR(int errnum,
         int jointno,
         char *what, 
         char *name)
{
    char *errmsg;

    /* Prepare to issue user error. */
    fflush(stdout);
    fprintf(stderr, "\n\n*** ERROR: ");
    switch(errnum) {
        case ERR_ZeroTreePin:
            errmsg = "Pin was too close to zero to be normalized";
            break;
        case ERR_ZeroLoopInbPin1:
            errmsg = 
            "1st loop-joint pin was too close to zero to be normalized";
            break;
        case ERR_ZeroLoopInbPin2:
            errmsg = 
            "2nd loop-joint pin was too close to zero to be normalized";
            break;
        case ERR_ZeroLoopInbPin3:
            errmsg = 
            "3rd loop-joint pin was too close to zero to be normalized";
            break;
        case ERR_ZeroLoopInbRef:
            errmsg = 
            "Inbref was too close to zero to be normalized";
            break;
        case ERR_NotRtHandSet:
            errmsg = 
            "The pins for a planar or sixdof loop joint didn't form\n\
a right-handed set";
            break;
        case ERR_ZeroLoopBodyPin:
            errmsg = 
            "Bodypin was too close to zero to be normalized";
            break;
        case ERR_ZeroLoopBodyRef:
            errmsg = 
            "Bodyref was too close to zero to be normalized";
            break;
        case ERR_Inbpin12notPerp:
            errmsg = 
            "1st and 2nd pins were not perpendicular";
            break;
        case ERR_Inbpin23notPerp:
            errmsg = 
            "2nd and 3rd pins were not perpendicular";
            break;
        case ERR_Inbpin13notPerp:
            errmsg = 
            "1st and 3rd pins were not perpendicular";
            break;
        case ERR_Inbpin1refNotPerp:
            errmsg = 
            "Inboard pin and ref were not perpendicular";
            break;
        case ERR_BodypinRefNotPerp:
            errmsg = 
            "Body pin and ref were not perpendicular";
            break;
        case ERR_SingularMassMatrix:
            errmsg = 
            "Singular mass matrix - illegal massless or inertialess body";
            break;
        default:
            errmsg = "Unrecognized error number -- SD/FAST internal error";
            break;
    }
    if (!what && !name)
        fprintf(stderr, "%s.", errmsg);
    else {
        fprintf(stderr, "%s ", errmsg);
        fprintf(stderr, "(%s `%s').\n",what, name);
    }
}

/* Output code to file F which prints 
 *       <prefix>rouname:\n
 * to file `fnum' if runtime variable `routine' is equal to the 
 * passed-in rounumber.
 */
static void
prt_routine(FILE *F,
            int rounumber,
            char *rouname)
{
    char name[50];

    esprintf(name, "%@A%s:", rouname);
    
    IFCOND efprintf(F, "routine%s%d", EQ, rounumber);
    THEN 
      if (Lang->flags & LANG_C_FAMILY)
        efprintf(F, "fprintf(fnum,\"%s\\n\")%;\n", name);
      else if (Lang == &FORTRAN_language)
        efprintf(F, "write(fnum,*)'%s'%;\n", name);
      else if (Lang == &Ada_language)
        efprintf(F, "PUT_LINE(fnum,\"%s\")%;\n", name);
    ENDIF;
}

/* Output code to file F which prints 
 *       errmsg\n
 * to file `fnum' if runtime variable `errnum' is equal to the 
 * passed-in errnumber.  The error message must be <= 72 chars.
 */
static void
prt_errmsg(FILE *F,
           int errnumber,
           char *errmsg)
{
    IFCOND efprintf(F, "errnum%s%d", EQ, errnumber);
    THEN 
      if (Lang->flags & LANG_C_FAMILY)
        efprintf(F, "fprintf(fnum,%&\"%@s\\n\")%;\n", errmsg);
      else if (Lang == &FORTRAN_language)
        efprintf(F, "write(fnum,*)%&'%@s'%;\n", errmsg);
      else if (Lang == &Ada_language)
        efprintf(F, "PUT_LINE(fnum,%&\"%@s\")%;\n", errmsg);
    ENDIF;
}

/* Output code to file F which prints a blank line to file `fnum'.
 */
static void
prt_blankline(FILE *F)
{
    if (Lang->flags & LANG_C_FAMILY)
        efprintf(F, "fprintf(fnum,\"\\n\")%;\n");
    else if (Lang == &FORTRAN_language)
        efprintf(F, "write(fnum,*)' '%;\n");
    else if (Lang == &Ada_language)
        efprintf(F, "PUT_LINE(fnum,\"\")%;\n");
}

void PRINT_SDPRERRMSG(FILE *F)
{
    char msg[100];

    /* Print the sdprerrmsg(fnum,routine,errnum) routine. */
    if (Lang->flags & LANG_C_FAMILY) {
        declare_proc(F, 0, "prerrmsg", 
          VT_TYPENAME|VT_BYREF, "FILE", "fnum",
          VT_INTEGER,     "routine",
          VT_DUP,           "errnum",
          0);
    } else if (Lang == &FORTRAN_language) {
        declare_proc(F, 0, "prerrmsg", 
          VT_INTEGER,     "fnum",
          VT_DUP,         "routine",
          VT_DUP,           "errnum",
          0);
    } else if (Lang == &Ada_language) {
        declare_proc(F, 0, "prerrmsg", 
          VT_TYPENAME, "FILE_TYPE", "fnum",
          VT_INTEGER,     "routine",
          VT_DUP,           "errnum",
          0);
    }

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IF ("errnum", EQ, "0");
      THEN RETURN;
    ENDIF;

    prt_blankline(F);

    prt_routine(F, ROU_sdgrav, "grav");
    prt_routine(F, ROU_sdmass, "mass");
    prt_routine(F, ROU_sdiner, "iner");
    prt_routine(F, ROU_sdbtj, "btj");
    prt_routine(F, ROU_sditj, "itj");
    prt_routine(F, ROU_sdpin, "pin");
    prt_routine(F, ROU_sdinit, "init");
    prt_routine(F, ROU_sdstate, "state");
    prt_routine(F, ROU_sdpsstate, "psstate");
    prt_routine(F, ROU_sdpresacc, "presacc");
    prt_routine(F, ROU_sdpresvel, "presvel");
    prt_routine(F, ROU_sdprespos, "prespos");
    prt_routine(F, ROU_sdhinget, "hinget");
    prt_routine(F, ROU_sdpointf, "pointf");
    prt_routine(F, ROU_sdbodyt, "bodyt");
    prt_routine(F, ROU_sdderiv, "deriv");
    prt_routine(F, ROU_sdresid, "resid");
    prt_routine(F, ROU_sdpseudo, "pseudo");
    prt_routine(F, ROU_sdmom, "mom");
    prt_routine(F, ROU_sdsys, "sys");
    prt_routine(F, ROU_sdpos, "pos");
    prt_routine(F, ROU_sdvel, "vel");
    prt_routine(F, ROU_sdorient, "orient");
    prt_routine(F, ROU_sdangvel, "angvel");
    prt_routine(F, ROU_sdtrans, "trans");
    prt_routine(F, ROU_sdperr, "perr");
    prt_routine(F, ROU_sdverr, "verr");
    prt_routine(F, ROU_sdpsqdot, "psqdot");
    prt_routine(F, ROU_sdpsudot, "psudot");
    prt_routine(F, ROU_sdgetht, "getht");
    prt_routine(F, ROU_sdreac, "reac");
    prt_routine(F, ROU_sdacc, "acc");
    prt_routine(F, ROU_sdangacc, "angacc");
    prt_routine(F, ROU_sdmult, "mult");
    prt_routine(F, ROU_sdaerr, "aerr");
    prt_routine(F, ROU_sdindx, "indx");
    prt_routine(F, ROU_sdpres, "pres");
    prt_routine(F, ROU_sdstab, "stab");
    prt_routine(F, ROU_sdgetgrav, "getgrav");
    prt_routine(F, ROU_sdgetmass, "getmass");
    prt_routine(F, ROU_sdgetiner, "getiner");
    prt_routine(F, ROU_sdgetbtj, "getbtj");
    prt_routine(F, ROU_sdgetitj, "getitj");
    prt_routine(F, ROU_sdgetpin, "getpin");
    prt_routine(F, ROU_sdgetpres, "getpres");
    prt_routine(F, ROU_sdgetstab, "getstab");
    prt_routine(F, ROU_sdinfo, "info");
    prt_routine(F, ROU_sdjnt, "jnt");
    prt_routine(F, ROU_sdcons, "cons");
    prt_routine(F, ROU_sdassemble, "assemble");
    prt_routine(F, ROU_sdinitvel, "initvel");
    prt_routine(F, ROU_sdstatic, "static");
    prt_routine(F, ROU_sdsteady, "steady");
    prt_routine(F, ROU_sdmotion, "motion");
    prt_routine(F, ROU_sdfmotion, "fmotion");
    prt_routine(F, ROU_sdequivht, "equivht");
    prt_routine(F, ROU_sdmassmat, "massmat");
    prt_routine(F, ROU_sdfrcmat, "frcmat");
    prt_routine(F, ROU_sdrel2cart, "rel2cart");
    prt_routine(F, ROU_sdcomptrq, "comptrq");
    prt_routine(F, ROU_sdfulltrq, "fulltrq");
    prt_routine(F, ROU_sdvrot, "vrot");

    prt_errmsg(F, ERR_ZeroTreePin, 
      "a tree joint pin axis was zero");
    prt_errmsg(F, ERR_ZeroLoopInbPin1, 
      "the 1st inboard pin for a loop joint was zero");
    prt_errmsg(F, ERR_ZeroLoopInbPin2, 
      "the 2nd inboard pin for a loop joint was zero");
    prt_errmsg(F, ERR_ZeroLoopInbPin3, 
      "the 3rd inboard pin for a loop joint was zero");
    prt_errmsg(F, ERR_ZeroLoopInbRef,
      "an inboard reference line was zero");
    prt_errmsg(F, ERR_NotRtHandSet,
      "a set of loop joint axes was not right handed");
    prt_errmsg(F, ERR_ZeroLoopBodyPin,
      "a loop joint bodypin was zero");
    prt_errmsg(F, ERR_ZeroLoopBodyRef,
      "a loop joint body reference line was zero");
    prt_errmsg(F, ERR_Inbpin12notPerp,
      "1st/2nd pins in a loop joint not perpendicular");
    prt_errmsg(F, ERR_Inbpin23notPerp,
      "2nd/3rd pins in a loop joint not perpendicular");
    prt_errmsg(F, ERR_Inbpin13notPerp,
      "1st/3rd pins in a loop joint not perpendicular");
    prt_errmsg(F, ERR_Inbpin1refNotPerp,
      "a loop jt pin and inbref were not perpendicular");
    prt_errmsg(F, ERR_BodypinRefNotPerp,
      "a bodypin and bodyref were not perpendicular");
    prt_errmsg(F, ERR_BadlyUnnormEulerParms,
      "Euler parameters were far from normalized");
    prt_errmsg(F, ERR_BadBodyNum,
      "illegal body number");
    prt_errmsg(F, ERR_BadJointNum,
      "illegal joint number");
    prt_errmsg(F, ERR_BadAxisNum,
      "illegal axis number");
    prt_errmsg(F, ERR_BadAxisNumForThisJoint,
      "illegal axis number for this joint");
    prt_errmsg(F, ERR_TriedToSetNonQues,
      "tried to set non-variable (i.e., non-?) parameter");
    prt_errmsg(F, ERR_BadValueForPres,
      "prescribed motion was neither 0 (off) or 1 (on)");
    prt_errmsg(F, ERR_BadUserConstraintNum,
      "illegal user constraint number");

    esprintf(msg, "%@Ainit %@s", "must be called first");
    prt_errmsg(F, ERR_sdinitMustBeCalledFirst, msg);
      
    esprintf(msg, "%@Astate %@s", "must be called first");
    prt_errmsg(F, ERR_sdstateMustBeCalledFirst, msg);

    esprintf(msg, "%@Aderiv %@s", "must be called first");
    prt_errmsg(F, ERR_sdderivMustBeCalledFirst, msg);

    prt_errmsg(F, ERR_GravityMustBeSpecified,
      "a gravity ? parameter is unspecified");
    prt_errmsg(F, ERR_MassMustBeSpecified,
      "a ? mass is unspecified");
    prt_errmsg(F, ERR_InertiaMustBeSpecified,
      "a ? inertia is unspecified");
    prt_errmsg(F, ERR_TreePinMustBeSpecified,
      "a ? tree jt pin is unspecified");
    prt_errmsg(F, ERR_TreeBtjMustBeSpecified,
      "a ? tree bodyToJoint vector is unspecified");
    prt_errmsg(F, ERR_TreeItjMustBeSpecified,
      "a ? tree inbToJoint vector is unspecified");
    prt_errmsg(F, ERR_TreePresMustBeSpecified,
      "a ? prescribed tree jt axis is unspecified");
    prt_errmsg(F, ERR_StabvelMustBeSpecified,
      "the stabvel ? parameter is unspecified");
    prt_errmsg(F, ERR_StabposMustBeSpecified,
      "the stabpos ? parameter is unspecified");
    prt_errmsg(F, ERR_LoopIpinMustBeSpecified,
      "a ? loop jt inboard pin is unspecified");
    prt_errmsg(F, ERR_LoopIrefMustBeSpecified,
      "a ? loop jt inbref is unspecified");
    prt_errmsg(F, ERR_LoopBpinMustBeSpecified,
      "a ? loop jt bodypin is unspecified");
    prt_errmsg(F, ERR_LoopBrefMustBeSpecified,
      "a ? loop jt bodyref is unspecified");
    prt_errmsg(F, ERR_LoopBtjMustBeSpecified,
      "a ? loop jt bodyToJoint vector is unspecified");
    prt_errmsg(F, ERR_LoopItjMustBeSpecified,
      "a ? loop jt inbToJoint vector is unspecified");
    prt_errmsg(F, ERR_LoopPresMustBeSpecified,
      "a ? prescribed loop jt axis is unspecified");
    prt_errmsg(F, ERR_LibraryMismatch,
      "Dynamics & Library File serial nos. differ");
    prt_errmsg(F, ERR_DynamicsAnalysisMismatch,
      "Dynamics & Analysis File gen. times differ");
    prt_errmsg(F, ERR_TreeGimbalLocked,
      "A tree gimbal joint is in gimbal lock");
    prt_errmsg(F, ERR_LoopGimbalLocked,
      "A loop gimbal joint is in gimbal lock");
    prt_errmsg(F, ERR_BadTreeCoordNum,
      "Bad relative coordinate number");
    prt_errmsg(F, ERR_CantRotAboutZeroVector,
      "The vector about which to rotate was zero");
    prt_errmsg(F, ERR_SingularMassMatrix,
      "Singular mass matrix - bad inertialess body?");

    efprintf(F, Lang->proc_end); 
}

/* Generates the sdserialno() routine, which returns (from the library)
 * the serial number of the sdfast which generated the routine.
 */
void PRINT_SDSERIALNO(FILE *F)
{
    extern int gProgramSerialNo;
    char sernostr[10];

    esprintf(sernostr, "%d", gProgramSerialNo);

    /* Declare the SDSERIALNO routine heading. */
    declare_proc(F, 0, "serialno",
      VT_INTEGER|VT_BYREF, "serno",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SETREF("serno", sernostr);

    efprintf(F, Lang->proc_end);
}

/* Generates the sdgentime() routine, which returns (from the dynamics file)
 * the time at which the generating sdfast run began, as an integer hhmmss.
 */
void PRINT_SDGENTIME(FILE *F)
{
    char gentimestr[10];

    esprintf(gentimestr, "%d", gGenerateTime);

    /* Declare the SDGENTIME routine heading. */
    declare_proc(F, 0, "gentime",
      VT_INTEGER|VT_BYREF, "gentm",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SETREF("gentm", gentimestr);

    efprintf(F, Lang->proc_end);
}
