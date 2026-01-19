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
 * integration routines `sdfinteg' and `sdvinteg'.  These are purely 
 * numerical routines, generated identically every time.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "../calc/gencode.h"

/* PRINT_SDINTEG
 *
 * Generate routines for integrating sets of ODE's.
 *
 *     sdvinteg(f, time, y, dy, param, dt, step, neq, tol, work, err, which)
 *         void (*f)();
 *         double *time, y[], dy[], param[], dt, *step, tol, work[6*neq];
 *         int neq,*err,*which;
 *
 *     f is f(time,y,dy,param,int *status)
 *
 * A variable timestep Runga-Kutta-Merson integrator.  The function
 * f is called to compute the derivatives of the state variables y.  (The
 * initial derivative is passed in, however, in dy.) These
 * are integrated between time and time+dt, with relative error normally kept
 * to below reltol.  When a function value is near zero,
 * the relative error can become very large even though the absolute error
 * is still small.  In case an absolute error is <= .1*tol, no further
 * attempt is made to reduce relative accuracy.
 *
 * Note: tolerance applies only within a single integration interval.
 * In the worst case, EVERY interval could introduce an error the size
 * of reltol.  If these were all in the same direction, the global error
 * could be much larger than tol.  But it should be bounded by tol*n
 * where n is the number of timesteps made.  Normally the errors are
 * not systematic so the global error will be pretty good.
 *
 * `step' is used as an initial guess at the stepsize, and is returned
 * as a suggested initial step size for the next call to vinteg.  This 
 * reflects a "locality" assumption that the function's behavior in
 * the next interval is likely to be similar to its behavior in the
 * current interval.
 *
 * On return y, and time will have been updated.  dy will contain 
 * f(time+dt, y), i.e., the derivative of y at time+dt.
 *
 * If neq is passed in as a negative number, sdvinteg will return after
 * taking a single successful step.  In that case, time will be updated
 * by AT MOST dt, but more likely it will come back earlier.  This lets
 * the caller (e.g. Applied Motion) grab control after every successful
 * integration step.
 *
 * Err is returned 0 if we're successful.  Other returns are:
 *   err=1   Error estimates forced the step size too small, but then after
 *           a single `bad' minimum-width step, the error estimates got
 *           reasonable again.  This suggests that we 
 *           integrated over a step.  In that case, `which' is the index of 
 *           the function exhibiting the first alleged discontinuity during 
 *           the interval.  The entire interval is integrated, however.
 *
 *   err=2   Can't continue integrating.  This means that the error estimate
 *           became large and didn't go back to normal after taking a 
 *           minimum-width step.  Time, y, and dy are returned just after
 *           the last successful step, just before the bad spot.  This is
 *           most likely a lockup point, beyond which constraints would be
 *           inconsistent.  `Which' says which function had the problem.
 *
 *   err=3   The user-supplied function f returned with a non-zero status.
 *           The actual status is reported in `which'.  The returned time,
 *           y, and dy are those from just before (within a step of width tol)
 *           the point at which the function would have returned non-zero.
 *
 *     sdfinteg(f, time, y, dy, param, step, neq, work, errest, status)
 *         void (*f)();
 *         double *time, y[], dy[], param[], step, work[4*neq], *errest;
 *         int neq, *status;
 *
 * A fixed timestep Runge-Kutta 4th order integrator.  The function
 * f is called to compute the derivatives of the state variables y.  These
 * are integrated between time and time+step.  An estimate of the 
 * integration error introduced in the step is returned in errest.  (This
 * is the same value used to measure against the user-supplied `tol'
 * value in sdvinteg(); i.e., it is the `tol' actually achieved by the
 * step.
 *
 * dy should initially contain f(time,y).
 *
 * On return y and time will have been updated.
 * dy will contain f(time+step, y), i.e., the derivative of y at time+step.
 * The status returned by f() on this final call is returned in status.
 */

/* A small value which we can add to a divisor to prevent division by
 * zero, without affecting the result significantly when the divisor is
 * meaningfully non-zero.
 */
#define TINY 1.e-20

void PRINT_SDINTEG(FILE *F)
{
    char str_flt0[10], lowlmt[10];

    esprintf(str_flt0, "%r", 0.);
    esprintf(lowlmt, "%@d", 0);

    efprintf(F, 
"\n%{Utility routine for use with %Afinteg and %Avinteg.  Work is 2*neq.%}\n");

/* sdrk4m
 *
 * Integrate dst = func(time,st) between time and time+step without adjusting
 * the step size.  St is supplied as the value of st at time, nst is returned
 * with the estimate of st's value at time+step.  `maxerr' is returned
 * as the worst error believed to be in nst.  This is the relative error
 * for any element of nst with magnitude >= .1.  Otherwise, the error
 * is defined to be 10 times the absolute error.  `which' says which
 * function showed the greatest error.
 *
 * This is a Runge-Kutta-Merson integrator which is a fourth-order
 * RK method with a fifth evaluation of the derivative used for
 * error estimation.  (One evaluation is done before the call to sdrk4m.)
 *
 * NOTE: st and nst MUST NOT be the same array.  Initially, dst0 (passed in)
 * is func(time,st).  On return time, st, and dst0 are unchanged.
 *
 * `work' should be dimensioned 2*neq doubles.
 */

    declare_proc(F, DECL_PACKED, "rk4m",
      packvar(VT_PROCNAME,                 "func"),
      packvar(VT_REAL,                  "time"),
      packvar(VT_SARRAY,                   "st",   "neq",              NULL),
      packvar(VT_DUP,                      "dst0"), 
      packvar(VT_SARRAY,                   "param", Lang->unknown_len, NULL),
      packvar(VT_REAL,                  "step"),
      packvar(VT_SARRAY,                   "nst",  "neq",              NULL),
      packvar(VT_INTEGER,               "neq"),
      packvar(VT_SARRAY,                   "work",  Lang->unknown_len, NULL),
      packvar(VT_SARRAY,                   "errs", "neq",              NULL),
      packvar(VT_REAL|VT_BYREF,                "maxerr"),
      packvar(VT_INTEGER|VT_BYREF,        "which"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,          "dst1",        /* locations in work */
      VT_DUP,          "dst2",
      VT_DUP,          "errf",
      VT_REAL,    "step2",
      VT_DUP,     "step3",
      VT_DUP,     "step6",
      VT_DUP,     "step8",
      VT_DUP,     "err",
      VT_DUP,     "old",
      VT_DUP,     "ast",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SET("dst1", lowlmt);
    SET("dst2", "dst1+neq");

    efprintf(F, "step2%=step/%r%;\n",2.);
    efprintf(F, "step3%=step/%r%;\n",3.);
    efprintf(F, "step6%=step/%r%;\n",6.);
    efprintf(F, "step8%=step/%r%;\n",8.);

    FORCNT("100", "i", "neq");
      SET("nst%(i%)", "st%(i%)+dst0%(i%)*step3");
    ENDFOR("100");

    CALL("func(time+step3,nst,%Rwork%(dst1%),param,%Rerrf)");
    FORCNT("110", "i", "neq");
      ONEINDX(F, "nst%(i%)%=st%(i%)+(dst0%(i%)+", 
                 "work", "dst1", "i", ")*step6%;\n");
    ENDFOR("110");

    CALL("func(time+step3,nst,%Rwork%(dst1%),param,%Rerrf)");
    FORCNT("120", "i", "neq");
      efprintf(F, "nst%(i%)%=st%(i%)+(dst0%(i%)+%r*", 3.);
      ONEINDX(F, "", "work", "dst1", "i", ")*step8%;\n");
    ENDFOR("120");

    CALL("func(time+step2,nst,%Rwork%(dst2%),param,%Rerrf)");
    FORCNT("130", "i", "neq");
      efprintf(F, "nst%(i%)%=st%(i%)+(dst0%(i%)-%r*", 3.);
      ONEINDX(F, "", "work", "dst1", "i", "");
      efprintf(F, "+%r*", 4.);
      ONEINDX(F, "", "work", "dst2", "i", ")*step2%;\n");
    ENDFOR("130");

    CALL("func(time+step,nst,%Rwork%(dst1%),param,%Rerrf)");

    /* Now update nst to the final estimate, while searching for the 
       function (in nst) with the worst error. */

    SETREF("maxerr", str_flt0);
    efprintf(F, "%swhich%=%@d%;\n", Lang->deref, 0);
    FORCNT("140", "i", "neq");
      SET("old", "nst%(i%)");
      efprintf(F, "nst%(i%)%=st%(i%)+(dst0%(i%)+%r*", 4.);
      ONEINDX(F, "", "work", "dst2", "i", "+");
      ONEINDX(F, "", "work", "dst1", "i", ")*step6%;\n");
      efprintf(F, "err%=%@D%s(%r*(old-nst%(i%)))%;\n", Lang->func_abs, .2);
      efprintf(F, "ast%=%@D%s(nst%(i%))%;\n", Lang->func_abs);
      IFCOND efprintf(F, "ast%s%r", GT, .1);
        THEN efprintf(F, "errs%(i%)%=err/(ast+%r)%;\n", TINY);
        ELSE efprintf(F, "errs%(i%)%=%r*err%;\n", 10.);
      ENDIF;
        
      IFCOND efprintf(F, "errs%(i%)%s", GT); REF("maxerr");
      THEN 
        REF("maxerr"); efprintf(F, "%=errs%(i%)%;\n");
        SETREF("which", "i");
      ENDIF;
    ENDFOR("140");
    efprintf(F, Lang->proc_end);

/* sdfinteg */

    efprintf(F, "\n%{\
A fixed-step integrator.  Work should be dimensioned 4*neq.%}\n");

    declare_proc(F, 0,  "finteg",
      VT_PROCNAME,      "func",
      VT_REAL|VT_BYREF,        "time",
      VT_SARRAY,           "st", "neq", NULL,
      VT_DUP,                "dst",
      VT_SARRAY,           "param", Lang->unknown_len, NULL,
      VT_REAL,                   "step", 
      VT_INTEGER,          "neq",
      VT_SARRAY,          "work", Lang->unknown_len, NULL,
      VT_REAL|VT_BYREF,        "errest", 
      VT_INTEGER|VT_BYREF, "status",
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "which",
      VT_DUP,     "nst",        /* locations in work array */
      VT_DUP,     "errs",        
      VT_DUP,     "morework",        
      VT_REAL,    "ttime",        
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SET("nst", lowlmt);
    SET("errs", "nst+neq");
    SET("morework", "errs+neq");

    /* ttime is for convenience in the call, to avoid the dereference */
    efprintf(F, "ttime%="); REF("time"); efprintf(F, "%;\n");
    IF("step", GT, str_flt0);
    THEN
      CALL("%Ark4m(func,ttime,st,dst,param,step,%Rwork%(nst%),neq,\
%Rwork%(morework%),%Rwork%(errs%),errest,%Rwhich)");
      FORCNT("100", "i", "neq");
        ONEINDX(F, "st%(i%)%=", "work", "nst", "i", "%;\n");
      ENDFOR("100");
      SET("ttime", "ttime+step");
    ELSE
      SETREF("errest", str_flt0);
    ENDIF;
    SETREF("status", "0");
    CALL("func(ttime,st,dst,param,status)");
    SETREF("time", "ttime");

    efprintf(F, Lang->proc_end);

/* sdvinteg */

#define ERR_OK                         "0"
#define ERR_POSSIBLE_DISCONT         "1"
#define ERR_CANT_CONTINUE         "2"
#define ERR_FUNC_BAD_STATUS        "3"

/* The smallest allowed integrator step size. */
#define MINSTEP                1e-10

    efprintf(F, "\n%{\
A variable-step integrator.  Work should be dimensioned 6*neq.%}");

    declare_proc(F, DECL_PACKED,  "vinteg",
      packvar(VT_PROCNAME,              "func"),
      packvar(VT_REAL|VT_BYREF,                "time"),
      packvar(VT_SARRAY,                   "st",   "neqin",            NULL),
      packvar(VT_DUP,                        "dst"),
      packvar(VT_SARRAY,                   "param", Lang->unknown_len, NULL),
      packvar(VT_REAL,                           "dt"), 
      packvar(VT_REAL|VT_BYREF,                "step"),
      packvar(VT_INTEGER,                  "neqin"),
      packvar(VT_REAL,                        "tol"),
      packvar(VT_SARRAY,                  "work",  Lang->unknown_len, NULL),
      packvar(VT_INTEGER|VT_BYREF,        "err"),
      packvar(VT_DUP|VT_BYREF,             "which"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "wh",
      VT_DUP,     "wh2",
      VT_DUP,     "xst",        /* locations in work array */
      VT_DUP,     "xst2",
      VT_DUP,     "xdst",
      VT_DUP,     "errs",
      VT_DUP,     "morework",        
      VT_DUP,     "errf",        
      VT_DUP,     "neq",        
      VT_DUP,     "earlyret",        
      0);
    declare_vars(F, 0, 
      VT_REAL,          "ttime",
      VT_DUP,     "ntime",        
      VT_DUP,     "xtime",        
      VT_DUP,     "tfin",        
      VT_DUP,     "stp",        
      VT_DUP,     "stp2",        
      VT_DUP,     "nstp",        
      VT_DUP,     "maxerr",        
      VT_DUP,     "minstep",        
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IF("neqin", LT, "0")
    THEN
      SET("neq", "-neqin");
      SET("earlyret", "1");
    ELSE
      SET("neq", "neqin");
      SET("earlyret", "0");
    ENDIF;

    SET("xst", lowlmt);
    SET("xst2", "xst+neq");
    SET("xdst", "xst2+neq");
    SET("errs", "xdst+neq");
    SET("morework", "errs+neq");

    RSET("minstep", MINSTEP);

    efprintf(F, "ttime%="); REF("time"); efprintf(F, "%;\n");
    SET("tfin", "ttime+dt");
    efprintf(F, "stp%="); REF("step"); efprintf(F, "%;\n");
    IF("stp", LT, "minstep")
      THEN SET("stp", "minstep");
    ENDIF;
    SETREF("err", ERR_OK);

    IF("dt", LE, str_flt0)
    THEN
      SET("errf", "0");
      CALL("func(ttime,st,dst,param,%Rerrf)");
      IF("errf", NE, "0");
      THEN
        SETREF("err", ERR_FUNC_BAD_STATUS);
        SETREF("which", "errf");
      ENDIF;
      RETURN;
    ENDIF;

    WHILE("200", "ttime", LT, "tfin");
      /* At this point func has been successfully integrated to time,
         with the integral's value at time in st, and func(time,st) in 
         dst. */

      /* If this next step would get us to within a quarter-step
       * of the end of the interval, adjust it so that we get exactly
       * to the end.  This is to prevent numerical errors caused
       * by an interval's steps not quite adding up to the interval's
       * length, leading to a final step 1e-17 or so wide.  This way
       * we stretch out the last step to cover the whole interval. 
       */
     
      IFCOND efprintf(F, "ttime+%r*stp%stfin",1.25,GE);
        THEN SET("stp", "tfin-ttime");
      ENDIF;

      /* Integrate as far as we can get with a good answer. */
      SET("nstp", "stp");
      LOOP("100");
        CALL("%Ark4m(func,ttime,st,dst,param,nstp,%Rwork%(xst%),neq,\
%Rwork%(morework%),%Rwork%(errs%),%Rmaxerr,%Rwh)");

        /* Set xtime to the time at the end of the experimental step. */
        SET("xtime", "ttime+nstp");

        IF("maxerr", LE, "tol");
        THEN
          SET("errf", "0");
          CALL("func(xtime,%Rwork%(xst%),%Rwork%(xdst%),param,%Rerrf)");
          IF("errf", EQ, "0");
          THEN
            /* We're within range so we'll accept this step.  Can we
               expand the step for next time? */
              efprintf(F, "stp2%=%r*nstp%;\n",2.);
              IFCOND efprintf(F, "(maxerr*%r%stol)%s(stp2%sdt)",64.,LE,AND_OP,LE);
                THEN SET("stp", "stp2");
                ELSE SET("stp", "nstp");
              ENDIF;
              BREAK("110");
          ENDIF;
        ENDIF;

        /* Step failed. Error is out of range or func returned bad status.  
           Can we cut step size some more? */

        efprintf(F, "stp2%=nstp/%r%;\n",2.);
        IF("stp2", LT, "minstep");
        THEN 
          /* Can't cut step any more.  As long as func returns 0,
           * we tentatively accept this step (call it S1) and try
           * one more (S2).  If S2 succeeds, accept S1 (with an 
           * err=POSSIBLE_DISCONTINUITY warning); otherwise, reject both 
           * and return with err=CANT_CONTINUE error.
           */
          SET("errf", "0");
          CALL("func(xtime,%Rwork%(xst%),%Rwork%(xdst%),param,%Rerrf)");
          IF("errf", NE, "0");
          THEN
            SETREF("err", ERR_FUNC_BAD_STATUS);
            SETREF("which", "errf");
            SETREF("time", "ttime");
            SETREF("step", "nstp");
            /* Last call must be made at t, st */
            CALL("func(ttime,st,dst,param,%Rerrf)");
            RETURN;
          ENDIF;

          /* Assume that we're just looking at a step function here. 
           * (We don't do anything if we've already seen a discontinuity,
           * since we're supposed to report only the first one.
           */
          IFCOND REF("err"); efprintf(F, "%s0", EQ);
          THEN
            SETREF("err", ERR_POSSIBLE_DISCONT);
            SETREF("which", "wh");
          ENDIF;

          /* Use the same stepsize unless it takes us beyond the end of 
           * the interval.  Put the step to take in stp2. 
           */
          IF("xtime+nstp", GT, "tfin");
            THEN SET("stp2", "tfin-xtime");
            ELSE SET("stp2", "nstp");
          ENDIF;

          SET("ntime", "xtime+stp2");
          IF("ntime", EQ, "xtime");
          THEN /* too close to end to do lookahead -- just accept */
            SET("stp", "nstp");
            BREAK("110");
          ENDIF;

          /* Attempt lookahead step S2. */
          CALL("%Ark4m(func,xtime,%Rwork%(xst%),%Rwork%(xdst%),param,stp2,%&\
%Rwork%(xst2%),neq,%Rwork%(morework%),%Rwork%(errs%),%Rmaxerr,%Rwh2)");

          /* If the error in the previously bad function went below tol,
             we can acept S1. */
          IFCOND  
            ONEINDX(F, "", "work", "errs", "wh", "");
            efprintf(F, "%stol", LE);
          THEN
            CALL("func(xtime,%Rwork%(xst%),%Rwork%(xdst%),param,%Rerrf)");
            SET("stp", "nstp");
            BREAK("110");
          ENDIF;

          /* Otherwise, reject both steps, backup and barf. */
          SETREF("err", ERR_CANT_CONTINUE);
          SETREF("which", "wh");
          SETREF("time", "ttime");
          SETREF("step", "nstp");
          /* Last call must be made at t, st */
          CALL("func(ttime,st,dst,param,%Rerrf)");
          RETURN;
        ELSE
          /* Try again with the smaller step. */
          SET("nstp", "stp2");
        ENDIF;

      ENDLOOPBRK("100","110");

      /* Step accepted: xtime is end of step taken; stp is the size of the
       * next step to take; work[xst], work[xdst] contain the state and
       * derivative at the end; func was last called at the end of the 
       * interval (xtime) using work[xst].
       */

      /* Update time, st, and dst. */
      SET("ttime", "xtime");
      FORCNT("120", "i", "neq");
        ONEINDX(F, "st%(i%)%=", "work", "xst", "i", "%;\n");
        ONEINDX(F, "dst%(i%)%=", "work", "xdst", "i", "%;\n");
      ENDFOR("120");

      /* Remember next step size to use.  It is conceivable that this 
       * last step was a roundoff-width leftover tfin-ttime, in which case we
       * shouldn't return it as the step size to use next time.  Also,
       * if the leftover step were so short that it has no effect when
       * added to time, we're done -- we certainly shouldn't get into
       * an infinite loop!
       */
      IF("stp", GE, "minstep")
        THEN SETREF("step", "stp");
      ENDIF;
      SET("ntime", "ttime+stp");
      IF("ntime", EQ, "ttime")
        THEN SET("ttime", "tfin");  /* this will cause `while' termination */
      ENDIF;

      /* Drop out early if we're supposed to. */
      IF("earlyret", NE, "0")
      THEN
        SETREF("time", "ttime");
        RETURN;
      ENDIF;

    ENDWHILE("200");

    SETREF("time", "tfin");

    efprintf(F, Lang->proc_end);
}
