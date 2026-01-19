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

/*
 * This file contains code associated with the generation of the
 * nonlinear root finding routine `sdroot'.  This is a purely numerical
 * routine, generated identically every time.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "../calc/gencode.h"

/* PRINT_SDROOT
 *
 * Generate routine for finding a root of a set of nonlinear equations.
 *
 *     sdroot(f,vars,param,nf,nv,nd,lock,rtol,dtol,maxeval, 
 *                  jw,dw,rw,iw,fret,fcnt,err)
 *
 *          void   f(vars,param,fret);
 *          double vars[nv],rtol,dtol,fret[nf],param[];
 *          int    nf,nv,nd,lock[nv],maxeval,*fcnt,*err;
 *          double jw[nf*nk], dw[2*(nf+nk)**2], rw[7*nf+9*nk)];
 *          int    iw[4*(nf+nk)];
 *
 * Adjust nv vars to simultaneously zero nf=nr+nd functions, using the Newton
 * Raphson method in multiple dimensions and a QR least-squares linear solver.
 * Nr is the number of `required' functions, e.g. constraints, and nd is the
 * number of `desired' functions, e.g. objectives.  The routine quits when
 * the largest required function is at or below rtol and the largest
 * desired function is at or below dtol.  If that can't be done, the routine
 * quits when it's improving too slowly.
 *
 * If lock[i] != 0, vars[i] will be left alone.  Param is the address of
 * some arbitrary stuff which is passed through to f unchanged.  Do with it
 * what you will.
 *
 * Maxeval limits the number of function evaluations made.  Sdroot will stop
 * when it discovers it has made maxeval or more function evaluations.  Fcnt
 * is returned with the actual count, which may be greater than maxeval. Sdroot
 * was still making progress if, on return, fcnt >= maxeval.
 *
 * On return, knobs will contain the best setting seen so far, fret will 
 * contain the corresponding values of the functions at that knob setting, 
 * and fcnt will give the number of calls to f which were made.  If the first 
 * nr elements of fret are <= rtol, and the last nd elements are <= dtol, 
 * the err return will be 0.  If only the first nr are <= rtol, err will be
 * returned 1.  If one of the first nr is > rtol, err will be 2.
 *
 * Kludge for ApM:
 * When the requireds are not all met, the acceptance or rejection of a step
 * is determined by the maximum absolute value of any single required 
 * function.  A step is accepted only if the maximum value is reduced by
 * taking the step.  When the requireds are met, acceptance/rejection of a 
 * step is controlled by the desireds.  Normally, the step is accepted if
 * the SUM of the absolute values of the errors decreases (i.e. the average
 * error value decreases).  This is not always acceptable because it
 * may result in the MAXIMUM error in a desired function increasing, as
 * long as the AVERAGE decreases.  If you want the "desired" steps to
 * be accepted only if the maximum decreases (i.e., the same method as is
 * used for requireds), then pass in the negative of the number of desireds
 * in the "nd" parameter above.
 */

/* These parameters control the `crudeness' with which we compute the
 * downhill direction using finite differences.  We start with a delta
 * large enough to avoid roundoff problems.  If we get stuck, we'll try
 * progressively smaller deltas.
 */
#define INITCRUDE 1e-6                /* start with +/- INITCRUDE as delta */
#define CRUDEFAC  1e-3          /* reduce by this factor each restart */
#define NUMTRYS   2                /* try this many different deltas */

/* After choosing a direction, we must decide how big a step to take in
 * that direction.  We start by stepping fraction INITSTEP of the distance 
 * that the NR says we should go.  Each time we start a new NR iteration, 
 * we grow the step by ADVANCE.  If the step proved too big (the error got
 * worse), we shrink it by RETREAT.  If step goes below MINSTEP we 
 * have either found a local minimum or the Jacobian (matrix of derivatives) 
 * is no good.  At that point we restart at a reduced crudeness level.
 * The last time around, we will in desperation allow the step to go as
 * small as LASTMINSTEP.
 */
#define INITSTEP 1.
#define RETREAT  0.5
#define ADVANCE  2.
#define MINSTEP  0.01
#define LASTMINSTEP 1e-5
#define MAXSTEP  1.0

/* This is the smallest function value change which we think is significant. 
 * Ignoring smaller changes prevents sdnr() from turning knobs which have
 * no (or negligible) effect. 
 */
#define MINCHANGE         1e-13

/* Limit single step knob change to MAXKREL*fabs(k)+MAXKABS.  But if we
 * take less than fraction BIGENOUGH of the full step,
 * we'll try ever-bigger steps in the
 * same direction until we either take a BIGENOUGH step or find one that
 * makes the solution worse.  At first we're cautious and take only 
 * slightly bigger steps, using SMALLMAG.  But if we succeed for FIRSTCUT 
 * steps, we switch to MIDMAG, which is bigger.  If that works up
 * to step SECONDCUT, we switch to MAXMAG which should be large enough
 * to provide acceptable performance for linear functions (these will
 * always be solved by taking the full step).
 */
#define MAXKREL         0.1
#define MAXKABS         1.0
#define BIGENOUGH        0.5
#define SMALLMAG        1.25
#define MIDMAG                2.0
#define MAXMAG                10.0
#define FIRSTCUT        5
#define SECONDCUT        10

/* This is the maximum error in the requireds that we'll attempt to 
   fix up after a desired-improving step that breaks them, and the
   maximum number of NR iterations we'll spend trying to fix them up. */
#define MAXFIXUPERR         0.05
#define MAXFIXITS        5

/* These constants determine what constitutes `too slow' improvement
 * of the objective.  There are both absolute and relative factors for
 * both the requireds and the desireds.  The absolute factors are
 * multiplied by rtol and dtol respectively.
 *
 * TOOSLOWSLACK says how many consecutive `too slow' improvements we
 * have to see before we're convinced that nothing's happening.
 */
#define TOOSLOWREQABS        0.01
#define TOOSLOWREQREL        0.01
#define TOOSLOWDESABS        0.1
#define TOOSLOWDESREL        0.01
#define TOOSLOWSLACK        3

/* Tolerance to be used in the QR solver -- see Linpack manual for an
   explanation. */
#define QRTOL                 1e-13

/* Error returns. */
#define ERR_OK                 "0"
#define ERR_DESNOTMET         "1"
#define ERR_REQNOTMET         "2"

void PRINT_SDROOT(FILE *F)
{
    char lowlmt[10],str_flt0[10];
    char uplmtntrys[15];
    char numtrys[15],maxfixits[15];

    esprintf(lowlmt, "%@d", 0);
    esprintf(str_flt0, "%r", 0.);
    esprintf(numtrys, "%d", NUMTRYS);
    esprintf(maxfixits, "%d", MAXFIXITS);

    if (Lang->subs_offset == 1) 
        esprintf(uplmtntrys, "%d", NUMTRYS);
    else 
        esprintf(uplmtntrys, "%d", NUMTRYS-1);

    efprintf(F, "\n%{Utility routines for use with %Aroot.%}\n");

/* sdcalcerrs 
 *
 * Evaluate the errors in the current values of the functions.  We return
 *       maxderr  -- the largest abs value in the last ndes fval entries
 *       maxrerr  -- the largest abs value in the first nreq fval entries
 *       derrnorm -- the sum of the abs values of the last ndes fval entries,
 *                   or maxderr if dnorm==1.
 */

    declare_proc(F, DECL_PACKED, "calcerrs",
      packvar(VT_SARRAY,           "fval", "nfunc", NULL),
      packvar(VT_INTEGER,          "nfunc"),
      packvar(VT_DUP,              "ndes"),
      packvar(VT_DUP,              "dnorm"),
      packvar(VT_REAL|VT_BYREF,        "maxderr"),
      packvar(VT_DUP|VT_BYREF,  "maxrerr"),
      packvar(VT_DUP|VT_BYREF,  "derrnorm"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "nreq",
      VT_REAL,    "tmp",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SET("nreq", "nfunc-ndes");
    SETREF("maxderr", str_flt0);
    SETREF("maxrerr", str_flt0);
    SETREF("derrnorm", str_flt0);

    FORCNT("100", "i", "nreq");
      efprintf(F, "tmp%=%@D%s(fval%(i%))%;\n", Lang->func_abs);
      IFCOND efprintf(F, "tmp%s", GT); REF("maxrerr");
          THEN SETREF("maxrerr", "tmp");
      ENDIF;
    ENDFOR("100");
    FORCNT("200", "i", "ndes");
      efprintf(F, "tmp%=%@D%s(fval%(nreq+i%))%;\n", Lang->func_abs);
      IFCOND efprintf(F, "tmp%s", GT); REF("maxderr");
          THEN SETREF("maxderr", "tmp");
      ENDIF;
      INCREF("derrnorm", "tmp");
    ENDFOR("200");

    IF("dnorm", EQ, "1")
    THEN REF("derrnorm"); efprintf(F,"%="); 
         REF("maxderr"); efprintf(F,"%;\n");
    ENDIF;

    efprintf(F, Lang->proc_end);

/* sdadjvars 
 *
 * Adjust all the variables according to the passed-in deltas and the step.
 * Limit changes to MAXKREL*fabs(v)+MAXKABS where v is the current value
 * of a variable.  This keeps cyclical vars from being bumped out of their
 * cycles and generally limits the damage that can be done by an overzealous
 * and lucky change.  However, if that works we'll keep trying bigger
 * changes here so that we don't take forever to solve a problem in which
 * big changes are necessary.
 *
 * Variable limiting is done by multiplying the entire delta by a scalar so that
 * the direction of movement is preserved.
 */
    declare_proc(F, DECL_PACKED, "adjvars",
      packvar(VT_PROCNAME,                 "func"),
      packvar(VT_SARRAY,                   "vars", "nvar", NULL),
      packvar(VT_SARRAY,                   "param", Lang->unknown_len, NULL),
      packvar(VT_INTEGER,                  "nfunc"),
      packvar(VT_DUP,                      "ndes"),
      packvar(VT_DUP,                      "dnorm"),
      packvar(VT_DUP,                      "nvar"),
      packvar(VT_SARRAY,                   "deltas", "nvar", NULL),
      packvar(VT_REAL,                     "step"),
      packvar(VT_DUP,                      "rerr"),
      packvar(VT_DUP,                      "tderr"),
      packvar(VT_DUP,                      "rtol"),
      packvar(VT_INTEGER|VT_BYREF,        "fcnt"),
      packvar(VT_SARRAY,                   "newvars", "nvar", NULL),
      packvar(VT_SARRAY,                   "newerrs", "nfunc", NULL),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "cnt",
      VT_DUP,     "alldone",
      VT_REAL,    "impr",
      VT_DUP,     "maxchg",
      VT_DUP,     "pmaxfact",
      VT_DUP,     "maxfact",
      VT_DUP,     "pmaxrerr",
      VT_DUP,     "pderrnorm",
      VT_DUP,     "maxderr",
      VT_DUP,     "maxrerr",
      VT_DUP,     "derrnorm",
      VT_DUP,     "mag",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    /* Compute the limit factor to scale down the proposed change. */

    RSET("maxfact", 1.);

    FORCNT("100", "i", "nvar");
      efprintf(F, "impr%=%@D%s(deltas%(i%)*step)%;\n", Lang->func_abs);
      efprintf(F, "maxchg%=%r*%@D%s(vars%(i%))+%r%;\n", 
          MAXKREL, Lang->func_abs, MAXKABS);
      IF("impr/maxchg", GT, "maxfact")
          THEN SET("maxfact", "impr/maxchg");
      ENDIF;
    ENDFOR("100");
    efprintf(F, "maxfact%=%r/maxfact%;\n", 1.);

    /* Now start with the limited change, followed by larger and larger
     * changes up to the full proposed change or a failure.  Failure means
     * that a larger change produced worse errors than a smaller one.
     */

    SET("pmaxrerr", "rerr");
    SET("pderrnorm", "tderr");
    SET("pmaxfact", "maxfact");
    ISET("cnt", 0);
    ISET("alldone", 0);

    LABEL("retry", "150");

    SET("cnt", "cnt+1");

    FORCNT("200", "i", "nvar");
      SET("impr", "deltas%(i%)*step");
      SET("newvars%(i%)", "vars%(i%) - impr*maxfact");
    ENDFOR("200");
    CALL("func(newvars,param,newerrs)");
    INCREF("fcnt", "1");

    IF("alldone", NE, "0")
      THEN RETURN;
    ENDIF;

    CALL("%Acalcerrs(newerrs,nfunc,ndes,dnorm,%Rmaxderr,%Rmaxrerr,%Rderrnorm)");

    /* Accept this partial step if
     *   1. prev req err too big and current err improved; or
     *   2. prev req OK, current OK, and des improved.
     */
    IFCOND efprintf(F,"(pmaxrerr%srtol)%s(maxrerr%srtol)",LE,AND_OP,LE);
      THEN SET("impr", "pderrnorm-derrnorm");
      ELSE SET("impr", "pmaxrerr-maxrerr");
    ENDIF;

    SET("pmaxrerr", "maxrerr");
    SET("pderrnorm", "derrnorm");

    IFCOND efprintf(F,"impr%s%r",LE,0.);
    THEN /* got worse -- back up if we can */
      IF("maxfact", NE, "pmaxfact")
      THEN
        SET("maxfact", "pmaxfact");
        ISET("alldone", 1);
        GOTO("retry", "150");
      ENDIF;
    ELSE /* got better -- should we try for more? */
      IFCOND efprintf(F,"maxfact%s%r",LT,BIGENOUGH);
      THEN /* we'll go for more */
        /* choose magnification */
        IFCOND efprintf(F, "cnt%s%d", LT, FIRSTCUT);
          THEN RSET("mag", SMALLMAG);
          ELSE IFCOND efprintf(F, "cnt%s%d", LT, SECONDCUT);
            THEN RSET("mag", MIDMAG);
            ELSE RSET("mag", MAXMAG);
          ENDIF;
        ENDIF;
        SET("pmaxfact", "maxfact");
        SET("maxfact", "mag*maxfact");
        IFCOND efprintf(F, "maxfact%s%r", GT, 1.);
          THEN RSET("maxfact", 1.);
        ENDIF;
        GOTO("retry", "150");
      ENDIF;
    ENDIF;

    efprintf(F, Lang->proc_end);

/* sdcalcjac 
 *   
 * Calculate the Jacobian of f by forward differences using differencing
 * step `delta'.  (Actually, if the current value of a knob is k, we use
 * delta*(|k|+1) as the differencing step size.
 * Fval (input) is the vector value of f(vars).  Ftmp
 * is workspace large enough to hold a return value of f (may be more than
 * nf if we're just working on requireds).  Fcnt (input/output) is incremented
 * by the number of evaluations of f() performed here.
 */
    declare_proc(F, DECL_PACKED, "calcjac",
      packvar(VT_PROCNAME,                 "func"),
      packvar(VT_SARRAY,                   "vars",  "nvar",   NULL),
      packvar(VT_SARRAY,                   "param",  Lang->unknown_len, NULL),
      packvar(VT_INTEGER,                  "nfunc"),
      packvar(VT_DUP,                      "nvar"),
      packvar(VT_SARRAY|VT_INTEGER,        "lock",  "nvar",   NULL),
      packvar(VT_REAL,                           "delta"), 
      packvar(VT_SARRAY,                  "fval",   Lang->unknown_len, NULL),
      packvar(VT_DUP,                     "ftmp"), 
      packvar(VT_SARRAY,                  "jw",    "nfunc", "nvar",    NULL),
      packvar(VT_INTEGER|VT_BYREF,        "fcnt"),
      packvar(VT_SARRAY,                  "scale", "nfunc",  NULL),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "j",
      VT_REAL,    "save",
      VT_DUP,     "chg",
      VT_DUP,     "vchg",
      VT_DUP,     "maxelt",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    FORCNT("120", "j", "nvar");
      IF("lock%(j%)", NE, "0")
      THEN
        FORCNT("100", "i", "nfunc");
          TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", "%=");
          efprintf(F, "%r%;\n", 0.);
        ENDFOR("100");
      ELSE
        SET("save", "vars%(j%)");
        efprintf(F, "vchg%=delta*(%@D%s(save)+%r)%;\n", Lang->func_abs, 1.);
        SET("vars%(j%)", "vars%(j%)+vchg");
        CALL("func(vars,param,ftmp)");
        INCREF("fcnt", "1");
        SET("vars%(j%)", "save");
        FORCNT("110", "i", "nfunc");
          SET("chg", "ftmp%(i%)-fval%(i%)");
          IFCOND efprintf(F, "%@D%s(chg)%s%r", Lang->func_abs, LT, MINCHANGE);
          THEN
            TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", "%=");
            efprintf(F, "%r%;\n", 0.);
          ELSE
            TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", "%=");
            efprintf(F, "chg/vchg%;\n");
          ENDIF;
        ENDFOR("110");
      ENDIF;
    ENDFOR("120");


    /* Perform row scaling on the Jacobian so that the largest element in
     * each row is 1 or -1. See Matrix Computations, Golub & VanLoan, pp. 73-74.
     */
    FORCNT("220", "i", "nfunc");
      RSET("maxelt", 0.);
      FORCNT("200", "j", "nvar");
        IFCOND 
          efprintf(F, "%@D%s(", Lang->func_abs);
          TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", ")");
          efprintf(F, "%smaxelt", GT);
        THEN 
          efprintf(F, "maxelt%=");
          efprintf(F, "%@D%s(", Lang->func_abs);
          TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", ")%;\n");
        ENDIF;
      ENDFOR("200");
      IF("maxelt", GT, str_flt0);
      THEN
        efprintf(F, "scale%(i%)%=%r/maxelt%;\n", 1.);
        FORCNT("210", "j", "nvar");
          TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", "%=");
          TWOINDX(F, "", "jw", "", 1, "nfunc", "nvar", "i", "j", 
                  "*scale%(i%)%;\n");
        ENDFOR("210");
      ELSE
        RSET("scale%(i%)", 1.);
      ENDIF;
    ENDFOR("220");

    efprintf(F, Lang->proc_end);

/* sdroot
 */
    efprintf(F, "\n%{\n\
====================================================================\n\
Root-finding scheme for solving a set of nfunc=nreq+ndes nonlinear\n\
equations in nvar unknowns:\n\
  r (v) = r (v) = ... = r (v) = 0    (actually |r | <= rtol)\n\
   1       2             nreq                    i\n\
\n\
  d (v) = d (v) = ... = d (v) = 0    (actually |d | <= dtol)\n\
   1       2             ndes                    i\n");
    efprintf(F, "\
The r's are \"required\" functions while the d's are only \"desired\".\n\
That is, we adjust the nvar variables v to find a solution which keeps\n\
each of the r's below rtol, and keeps the d's as small as possible\n\
without violating any of the r's.  Least squares residuals are attempted\n\
if the equations cannot be solved.  No guarantees can be made that\n\
this routine will find a solution even if there is one.  The better\n\
the initial values for the v's, the more likely it is to succeed and\n");
    efprintf(F, "\
the more quickly it will converge.\n\
\n\
A single function func is provided which calculates all of the\n\
r's and d's for the current v and returns the residuals.  A parameter\n\
param is passed through unchanged to the function. \n\
\n\
The array lock has an element corresponding to each variable.  Any\n\
variable which may be modified should have its corresponding lock\n\
element set to 0.  If non-zero, the variable will not be altered here.\n");
    efprintf(F, "\
\n\
Maxeval sets an upper limit on the number of calls to func which may\n\
be made.  The root finder only returns after complete iterations, so\n\
it may make somewhat more than maxeval calls.  On return, the actual\n\
number of calls made is returned in fcnt.  Err is returned 0 if we\n\
successfully reduce all the r's to below rtol and all the d's to below\n");
    efprintf(F, "\
dtol.  If the r's are met but not the d's, we return with err=1, and if\n\
the r's are not met we return err=2.\n\
\n\
Work arrays should be dimensioned as follows:\n\
  jw is nf*nv\n\
  dw is 2*(nf+nv)**2\n\
  rw is 7*nf+9*nv\n\
  iw is 4*(nf+nv)\n\
====================================================================\n%}");

    declare_proc(F, DECL_PACKED, "root",
      packvar(VT_PROCNAME,                 "func"),
      packvar(VT_SARRAY,                   "vars", "nvar",  NULL),
      packvar(VT_SARRAY,                   "param", Lang->unknown_len, NULL),
      packvar(VT_INTEGER,                  "nfunc"),
      packvar(VT_DUP,                      "nvar"),
      packvar(VT_DUP,                      "ndesin"),
      packvar(VT_SARRAY|VT_INTEGER,        "lock", "nvar",  NULL),
      packvar(VT_REAL,                           "rtol"), 
      packvar(VT_DUP,                           "dtol"), 
      packvar(VT_INTEGER,                  "maxeval"),
      packvar(VT_SARRAY,                  "jw",    Lang->unknown_len, NULL),
      packvar(VT_DUP,                     "dw"),
      packvar(VT_DUP,                     "rw"),
      packvar(VT_SARRAY|VT_INTEGER,          "iw",    Lang->unknown_len, NULL),
      packvar(VT_SARRAY,                  "fret", "nfunc", NULL),
      packvar(VT_INTEGER|VT_BYREF,         "fcnt"),
      packvar(VT_DUP|VT_BYREF,             "err"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "slowcnt",
      VT_DUP, "tooslow",
      VT_DUP, "ntrys",
      VT_DUP, "nreq",
      VT_DUP, "fixits",
      VT_DUP, "ndes",
      VT_DUP, "dnorm",
      0);

    declare_vars(F, 0, 
      VT_INTEGER, "f1", 
      VT_DUP, "f2",
      VT_DUP, "scale",
      VT_DUP, "deltav",
      VT_DUP, "guess",
      VT_DUP, "rvars",
      VT_DUP, "rdeltav",
      VT_DUP, "rguess",
      VT_DUP, "morerw",
      VT_DUP, "mapf",
      VT_DUP, "mapv",
      VT_DUP, "moreiw",
      0);

    declare_vars(F, 0, 
      VT_REAL, "qrtol",
      VT_DUP,  "maxderr",
      VT_DUP,  "maxrerr",
      VT_DUP,  "derrnorm",
      VT_DUP,  "pmaxderr",
      VT_DUP,  "pmaxrerr",
      VT_DUP,  "pderrnorm",
      VT_DUP,  "step",
      VT_DUP,  "crude",
      VT_DUP,  "impr",
      VT_DUP,  "rstep",
      VT_DUP,  "preverr",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IF ("ndesin", LT, "0")
    THEN
        SET("ndes", "-ndesin");
        ISET("dnorm", 1);
    ELSE
        SET("ndes", "ndesin");
        ISET("dnorm", 0);
    ENDIF;

    SET("nreq", "nfunc-ndes");
    RSET("qrtol", QRTOL);
    SETREF("fcnt", "0");
    SETREF("err", ERR_OK);

    /* Locations of variables in work arrays */
    /* RW */
    SET("f1",                 lowlmt);
    SET("f2",                 "f1+nfunc");
    SET("scale",         "f2+nfunc");
    SET("deltav",         "scale+nfunc");
    SET("guess",         "deltav+nvar");
    SET("rvars",         "guess+nvar");
    SET("rdeltav",         "rvars+nvar");
    SET("rguess",         "rdeltav+nvar");
    SET("morerw",         "rguess+nvar");

    /* IW */
    SET("mapf",         lowlmt);
    SET("mapv",         "mapf+nfunc");
    SET("moreiw",         "mapv+nvar");

    FORCNT("100", "i", "nfunc");
      ONEINDX(F, "", "iw", "mapf", "i", "%=i%;\n");
    ENDFOR("100");
    FORCNT("110", "i", "nvar");
      ONEINDX(F, "", "iw", "mapv", "i", "%=i%;\n");
    ENDFOR("110");
    CALL("func(vars,param,fret)");
    INCREF("fcnt", "1");
    CALL("%Acalcerrs(fret,nfunc,ndes,dnorm,%Rpmaxderr,%Rpmaxrerr,%Rpderrnorm)");
    RSET("crude", INITCRUDE);
    FORCNT("590", "ntrys", numtrys);
      ISET("slowcnt", 0);
      RSET("step", INITSTEP);

      /* This loop performs one Newton iteration. */

      LOOP("490");

        /* We win if all the functions are below their tolerance. */
        IFCOND efprintf(F,"(pmaxrerr%srtol)%s(pmaxderr%sdtol)", LE,AND_OP,LE);
          THEN RETURN;
        ENDIF;

        /* If we're out of time, we'll return here. */
        IFCOND REF("fcnt"); efprintf(F, "%smaxeval", GE);
          THEN GOTO("givingUp", "600"); 
        ENDIF;

        /* Compute the scaled Jacobian and use it to solve for the 
         * deltav's which would drive all the functions to 0 if they
         * were linear. 
         */

        CALL("%Acalcjac(func,vars,param,nfunc,nvar,lock,crude,fret,%&\
%Rrw%(f1%),jw,fcnt,%Rrw%(scale%))");
        FORCNT("210", "i", "nfunc");
          ONEINDX(F, "", "rw", "f1", "i", "%=fret%(i%)*");
          ONEINDX(F, "", "rw", "scale", "i", "%;\n");
        ENDFOR("210");
        CALL("%Alsslv(nfunc,nvar,nfunc,nvar,ndes,%Riw%(mapf%),%&\
%Riw%(mapv%),qrtol,dw,%Rrw%(morerw%),%Riw%(moreiw%),jw,%Rrw%(f1%),%&\
%Rrw%(deltav%))");

        /* Take a step in the direction indicated by the deltav's.  We
         * start with a bold step and retreat if we get hurt.
         */

        LOOP("390");

          /* Move `step' of the way in the deltav direction and see what
           * that does to the functions. 
           */
          CALL("%Aadjvars(func,vars,param,nfunc,ndes,dnorm,nvar,%&\
%Rrw%(deltav%),step,pmaxrerr,pderrnorm,rtol,fcnt,%&\
%Rrw%(guess%),%Rrw%(f1%))");
          CALL(
            "%Acalcerrs(%Rrw%(f1%),nfunc,ndes,dnorm,%Rmaxderr,%Rmaxrerr,%Rderrnorm)");

          /* If there are desireds, and the requireds were formerly met
           * but this step would break them, see if we can tweak the 
           * requireds from here to get them back without ruining the
           * desireds.  This is actually a repeat of the whole root finding
           * apparatus in miniature, applied only to the requireds.
           */
          IFCOND
            efprintf(F,"(pderrnorm%sderrnorm)%s(pmaxrerr%srtol)\
%s(maxrerr%srtol)%s(maxrerr%s%r)",GT,AND_OP,LE,AND_OP,GT,AND_OP,LE,MAXFIXUPERR);
          THEN
            FORCNT("220", "i", "nvar");
              ONEINDX(F, "", "rw", "rvars", "i", "%=");
              ONEINDX(F, "", "rw", "guess", "i", "%;\n");
            ENDFOR("220");
            RSET("rstep", INITSTEP);
            SET("preverr", "maxrerr");

            /* This loop performs one Newton iteration.  But this inner
               root finder is limited to MAXFIXITS iterations. */
            FORCNT("290", "fixits", maxfixits);

              /* Compute the scaled Jacobian (for the requireds only) and use 
               * it to solve for the rdeltav's which would drive all the 
               * required functions to 0 if they were linear. 
               */

              CALL("%Acalcjac(func,%Rrw%(rvars%),param,nreq,nvar,lock,crude,%&\
%Rrw%(f1%),%Rrw%(f2%),jw,fcnt,%Rrw%(scale%))");
              FORCNT("230", "i", "nreq");
                ONEINDX(F, "", "rw", "f2", "i", "%=");
                ONEINDX(F, "", "rw", "f1", "i", "*");
                ONEINDX(F, "", "rw", "scale", "i", "%;\n");
              ENDFOR("230");
              CALL("%Alsslv(nreq,nvar,nreq,nvar,0,%Riw%(mapf%),%&\
%Riw%(mapv%),qrtol,dw,%Rrw%(morerw%),%Riw%(moreiw%),jw,%Rrw%(f2%),%&\
%Rrw%(rdeltav%))");

              /* Take a step in the direction indicated by the rdeltav's.  We
               * start with a bold step and retreat if we get hurt.
               */
              LOOP("240");
                /* Move `step' of the way in the rdeltav direction and see what
                 * that does to the required functions. 
                 */
                CALL(
                "%Aadjvars(func,%Rrw%(rvars%),param,nfunc,ndes,dnorm,nvar,%&\
%Rrw%(rdeltav%),rstep,preverr,pderrnorm,rtol,fcnt,%&\
%Rrw%(rguess%),%Rrw%(f2%))");
                CALL(
                "%Acalcerrs(%Rrw%(f2%),nfunc,ndes,dnorm,%Rmaxderr,%Rmaxrerr,%&\
%Rderrnorm)");

                /* In the inner root finder, we're not interested in 
                   steps which make no improvement. */
                IF("preverr-maxrerr", GE, "rtol");
                THEN BREAK("245"); /* an improvement of at least rtol */
                ELSE 
                  efprintf(F, "rstep%=rstep*%r%;\n", RETREAT);
                  IFCOND efprintf(F, "rstep%s%r", LT, MINSTEP);
                    THEN GOTO("couldntFixReqs", "380"); 
                  ENDIF;
                ENDIF;
              ENDLOOPBRK("240", "245");

              /* Improved the requireds, but no point continuing if we
                 spoiled the desireds. */
              IF("derrnorm", GT, "pderrnorm")
                THEN GOTO("couldntFixReqs", "380"); 
              ENDIF;

              /* Accept this fixup step. */
              FORCNT("250", "i", "nfunc");
                ONEINDX(F, "", "rw", "f1", "i", "%=");
                ONEINDX(F, "", "rw", "f2", "i", "%;\n");
              ENDFOR("250");
              FORCNT("260", "i", "nvar");
                ONEINDX(F, "", "rw", "rvars", "i", "%=");
                ONEINDX(F, "", "rw", "rguess", "i", "%;\n");
              ENDFOR("260");
              SET("preverr", "maxrerr");

              /* Check for a win. */
              IF("maxrerr", LE, "rtol")
                THEN GOTO("fixUpSucceeded", "300"); 
              ENDIF;

              /* Increase step size and try another fixup Newton iteration. */
              efprintf(F, "rstep%=rstep*%r%;\n", ADVANCE);
              IFCOND efprintf(F, "rstep%s%r", GT, MAXSTEP);
                THEN RSET("rstep", MAXSTEP);
              ENDIF;

            ENDFOR("290");

            GOTO("couldntFixReqs", "380"); /* ran out of time */

            LABEL("fixUpSucceeded", "300"); 
            FORCNT("310", "i", "nvar");
              ONEINDX(F, "", "rw", "guess", "i", "%=");
              ONEINDX(F, "", "rw", "rvars", "i", "%;\n");
            ENDFOR("310");
          ENDIF;

          /* Accept the new solution if
           *   1. prev req err too big and current err improved or unchanged; or
           *   2. prev req OK, current OK, and des improved or unchanged.
           */

          IFCOND efprintf(F,"(pmaxrerr%srtol)%s(maxrerr%srtol)",LE,AND_OP,LE);
          THEN
            SET("impr", "pderrnorm-derrnorm");
            IFCOND efprintf(F,"impr%s%r*dtol+%r*derrnorm",LT,
                            TOOSLOWDESABS,TOOSLOWDESREL);
              THEN ISET("tooslow", 1);
              ELSE ISET("tooslow", 0);
            ENDIF;
          ELSE
            SET("impr", "pmaxrerr-maxrerr");
            IFCOND efprintf(F,"impr%s%r*rtol+%r*maxrerr",LT,
                            TOOSLOWREQABS,TOOSLOWREQREL);
              THEN ISET("tooslow", 1);
              ELSE ISET("tooslow", 0);
            ENDIF;
          ENDIF;
          IF("impr", GE, str_flt0)
            THEN BREAK("395"); /* function got better or stayed the same */
          ENDIF;
            
          LABEL("couldntFixReqs", "380"); /* couldn't fix requireds */

          /* function got worse -- cut back step and try again */

          efprintf(F, "step%=step*%r%;\n", RETREAT);
          IF("ntrys", EQ, uplmtntrys)
          THEN
            IFCOND efprintf(F, "step%s%r", LT, LASTMINSTEP);
              THEN GOTO("nextcrude", "580");
            ENDIF;
          ELSE
            IFCOND efprintf(F, "step%s%r", LT, MINSTEP);
              THEN GOTO("nextcrude", "580");
            ENDIF;
          ENDIF;
        ENDLOOPBRK("390", "395");

        /* function got better or stayed the same */

        /* Accept this step. */
        FORCNT("400", "i", "nfunc");         /* fret <- f1 */
          ONEINDX(F, "fret%(i%)%=", "rw", "f1", "i", "%;\n");
        ENDFOR("400");
        FORCNT("410", "i", "nvar");         /* vars <- guess */
          ONEINDX(F, "vars%(i%)%=", "rw", "guess", "i", "%;\n");
        ENDFOR("410");
        SET("pmaxderr","maxderr");
        SET("pmaxrerr","maxrerr");
        SET("pderrnorm","derrnorm");

        /* See if this is fast enough improvement. */
        IF("tooslow",NE,"0")
        THEN
          /* The function is improving too slowly, so we are probably
           * at a minimum or at least at a very flat spot.  But let's
           * not be hasty -- we have to see slow improvement several times
           * in a row before we're convinced to abandon the search.
           */
          SET("slowcnt","slowcnt+1");
          IFCOND efprintf(F, "slowcnt%s%d", GE, TOOSLOWSLACK);
            THEN GOTO("nextcrude", "580");
          ENDIF;
        ELSE /* got a reasonable improvement this time */
          ISET("slowcnt", 0);
        ENDIF;

        /* Advance the step size for the next Newton iteration. */
        efprintf(F, "step%=step*%r%;\n", ADVANCE);
        IFCOND efprintf(F, "step%s%r", GT, MAXSTEP);
          THEN RSET("step", MAXSTEP);
        ENDIF;
          
      ENDLOOP("490");

      LABEL("nextcrude", "580");

      /* Maybe our Jacobian was no good due to too large a finite
         differencing step.  Try a more refined approach. */

      efprintf(F, "crude%=crude*%r%;\n", CRUDEFAC);
    ENDFOR("590");

    /* We give up! */
    
    LABEL("givingUp", "600");

    IF("pmaxrerr", GT, "rtol")
    THEN SETREF("err", ERR_REQNOTMET);
    ELSE 
      IF("pmaxderr", GT, "dtol")
        THEN SETREF("err", ERR_DESNOTMET);
      ENDIF;
    ENDIF;

    efprintf(F, Lang->proc_end);
}
