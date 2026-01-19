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
 * This file contains code associated with the generation of the problem-
 * specific analysis routines sdassemble(), sdinitvel(), sdstatic(), and
 * sdmotion().  Because the sdroot() routine has trouble working with four
 * Euler parameters (used to represent ball joint orientations), we also 
 * generate routines sdst2ang() and sdang2st() to convert the system state 
 * to/from a form using three Euler angles instead.  Sdnormst() is present
 * to normalize a state vector containing possible unnormalized Euler
 * parameters.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#include "../calc/gencode.h"

/* This routine generates code to file F which raises an error if
 * the time returned by sdgentime() is different from the gGenerateTime
 * at which the current routine was built.
 *
 * Assumes that an integer variable i is available.
 */
static void 
chkgentime(FILE *F,
           int routine)
{
    CALL("%Agentime(%Ri)");
    IFCOND efprintf(F, "i%s%d", NE, gGenerateTime);
    THEN
      SETERR(F, routine, ERR_DynamicsAnalysisMismatch);
    ENDIF;
}

/* PRINT_SDST2ANG
 *
 * Generate sdst2ang(), sdang2st(), and sdnormst();
 *
 *    sdst2ang(st, stang)
 *    sdang2st(stang, st)
 *    sdnormst(st, normst)
 *      real st[nq], stang[nu], normst[nq]
 *
 * The following routine is generated, but only for sdfast internal use:
 *    sdnrmsterr(st, normst, routine)
 *      real st[nq], stang[nu], normst[nq]
 *      int routine;
 *    
 * These routines help to handle the always-annoying Euler parameters 
 * present in the state vector if there are any tree ball joints.
 *
 * Sdst2ang() and sdang2st() convert a hinge position state array to/from 
 * a form which substitutes three Euler angles for four Euler parameters for 
 * ball joints.  This can be done in place since the 4th Euler parameters 
 * are always at the end of the q portion of the state array.  (That is, it 
 * is OK for st and stang to be the same variable.)
 *
 * Sdnrmsterr() normalizes all the Euler parameters in the passed-in state,
 * leaving the result in the output normst.  It is OK if st and normst are
 * the same array.  If `routine' is non-zero, and any of the Euler parameters 
 * are found to have norms < .9 or > 1.1, an error is noted for sderror() as
 * though it had come from the passed-in routine, and the normalization
 * is done anyway.  The special case of (0,0,0,0) produces the error and
 * is normalized to (0,0,0,1).  Sdnormst() is simply a call to sdnrmsterr()
 * with routine=0 so that no error is posted in the case of badly non-
 * normalized Euler parameters.
 */
void
PRINT_SDST2ANG(FILE *F)
{
    char ndof[10], numq[10], str_flt0[10], callstr[200];
    int frst,fourth,i,s1,nq1;
    JointDesc_t *jntp;

    s1 = SysI.s ? SysI.s : 1;
    nq1 = SysI.nq ? SysI.nq : 1;

    esprintf(ndof, "%d", SysI.s);
    esprintf(numq, "%d", SysI.nq);
    esprintf(str_flt0, "%r", 0.);

    efprintf(F, "\n\
%{Convert state to form using 1-2-3 Euler angles for ball joints.%}");

    declare_proc(F, 0, "st2ang",
      VT_ARRAY,        "st",    nq1, 0,
      VT_ARRAY,               "stang", s1,  0,
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_MATRIX|VT_COND, SysI.nb, "dc",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    FORCNT("100", "i", ndof);
      SET("stang%(i%)", "st%(i%)");
    ENDFOR("100");

    if (SysI.nb) {
        for (i=0; i < SysI.n; i++) {
            jntp = &SysI.Bodies[i].jnt;
            if (JointInfo[(int)jntp->JointKind].hasball) {
                if (jntp->JointKind==cBallJoint || jntp->JointKind==cRev6dJoint)
                    frst = SysI.FirstDOF[i];
                else if (jntp->JointKind == c6dJoint)
                    frst = SysI.FirstDOF[i]+3;
                else fatal("expected ball or 6dof");
                fourth = SysI.BallQ[i];
                esprintf(callstr, 
                  "%Aquat2dc(st%(%@d%),st%(%@d%),st%(%@d%),st%(%@d%),dc)",
                  frst,frst+1,frst+2,fourth);
                CALL(callstr);
                esprintf(callstr, 
                 "%Adc2ang(dc,%%Rstang%(%@d%),%%Rstang%(%@d%),%%Rstang%(%@d%))",
                  frst,frst+1,frst+2);
                CALL(callstr);
            }
        }
    }

    efprintf(F, Lang->proc_end);

    efprintf(F, "\n\
%{Convert 1-2-3 form of state back to Euler parameters for ball joints.%}");

    declare_proc(F, 0, "ang2st",
      VT_ARRAY,               "stang", s1,  0,
      VT_ARRAY,        "st",    nq1, 0,
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_MATRIX|VT_COND, SysI.nb, "dc",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    FORCNT("100", "i", ndof);
      SET("st%(i%)", "stang%(i%)");
    ENDFOR("100");

    if (SysI.nb) {
        for (i=0; i < SysI.n; i++) {
            jntp = &SysI.Bodies[i].jnt;
            if (JointInfo[(int)jntp->JointKind].hasball) {
                if (jntp->JointKind==cBallJoint || jntp->JointKind==cRev6dJoint)
                    frst = SysI.FirstDOF[i];
                else if (jntp->JointKind == c6dJoint)
                    frst = SysI.FirstDOF[i]+3;
                else fatal("expected ball or 6dof");
                fourth = SysI.BallQ[i];
                esprintf(callstr, 
                  "%Aang2dc(stang%(%@d%),stang%(%@d%),stang%(%@d%),dc)",
                  frst,frst+1,frst+2);
                CALL(callstr);
                esprintf(callstr, 
      "%Adc2quat(dc,%%Rst%(%@d%),%%Rst%(%@d%),%%Rst%(%@d%),%%Rst%(%@d%))",
                  frst,frst+1,frst+2,fourth);
                CALL(callstr);
            }
        }
    }

    efprintf(F, Lang->proc_end);

    efprintf(F, "\n\
%{Normalize Euler parameters in state.%}");

    declare_proc(F, 0, "nrmsterr",
      VT_ARRAY,        "st",     nq1, 0,
      VT_ARRAY,               "normst", nq1, 0,
      VT_INTEGER,      "routine", 
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM); /* lasterr */
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_REAL|VT_COND, SysI.nb, "norm",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    
    FORCNT("100", "i", numq);
      SET("normst%(i%)", "st%(i%)");
    ENDFOR("100");

    if (SysI.nb) {
        for (i=0; i < SysI.n; i++) {
            jntp = &SysI.Bodies[i].jnt;
            if (JointInfo[(int)jntp->JointKind].hasball) {
                if (jntp->JointKind==cBallJoint || jntp->JointKind==cRev6dJoint)
                    frst = SysI.FirstDOF[i];
                else if (jntp->JointKind == c6dJoint)
                    frst = SysI.FirstDOF[i]+3;
                else fatal("expected ball or 6dof");
                fourth = SysI.BallQ[i];
                efprintf(F, "norm%=%@D%s(st%(%@d%)*st%(%@d%)+\
st%(%@d%)*st%(%@d%)+st%(%@d%)*st%(%@d%)+st%(%@d%)*st%(%@d%))%;\n",
                         Lang->func_sqrt,
                         frst,frst,frst+1,frst+1,frst+2,frst+2,fourth,fourth);
                IF("routine", NE, "0");
                THEN
                  IFCOND efprintf(F, "(norm%s%r)%s(norm%s%r)",
                                  LT, .9, OR_OP, GT, 1.1);
                  THEN 
                    esprintf(callstr, "%Aseterr(routine,%d)", 
                             ERR_BadlyUnnormEulerParms);
                    CALL(callstr);
                  ENDIF;
                ENDIF;

                IF("norm", EQ, str_flt0);
                THEN
                  efprintf(F, "normst%(%@d%)%=%r%;\n", fourth, 1.);
                  RSET("norm", 1.);
                ENDIF;
                efprintf(F, "norm%=%r/norm%;\n", 1.);
                efprintf(F, "normst%(%@d%)%=normst%(%@d%)*norm%;\n",
                         frst,frst);
                efprintf(F, "normst%(%@d%)%=normst%(%@d%)*norm%;\n",
                         frst+1,frst+1);
                efprintf(F, "normst%(%@d%)%=normst%(%@d%)*norm%;\n",
                         frst+2,frst+2);
                efprintf(F, "normst%(%@d%)%=normst%(%@d%)*norm%;\n",
                         fourth,fourth);
            }
        }
    }

    efprintf(F, Lang->proc_end);

    /* sdnormst */

    declare_proc(F, 0, "normst",
      VT_ARRAY,        "st",     nq1, 0,
      VT_ARRAY,               "normst", nq1, 0,
      0);
    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    CALL("%Anrmsterr(st,normst,0)");
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDPOSFUNC
 *
 * Generate sdposfunc(), sdvelfunc(), sdstatfunc(), sdstdyfunc(), sdmotfunc().
 * These are small routines which are in the format required by the
 * root finder or the integrators.  Sdmotfunc() is used by sdmotion()
 * to pass to the sdvinteg() integrator.  The other routines are used
 * by sdassemble(), sdinitvel() and sdstatic() as fodder for sdroot().
 *
 *    sdposfunc(vars,param,resid)             -- param[0] is the current time
 *      real vars[nu], param[1], resid[nc]    
 *
 *    sdvelfunc(vars,param,resid)             -- param[0..nq-1] are the current
 *      real vars[nu], param[nq+1], resid[nc]    q's, param[nq] is the time
 *
 *    sdstatfunc(vars,param,resid)            -- param[0..nu-1] are the current
 *      real vars[nu],param[nu+1],resid[nc+nu]   u's, param[nu] is the time
 *
 *    sdstdyfunc(vars,param,resid)            -- param[0] is the current time
 *      real vars[2*nu], param[1], resid[2*nc+nu]
 *
 *    sdmotfunc(t,state,dstate,param,status)  -- param[0] is required ctol
 *      real t,state[nq+nu],dstate[nq+nu],param[1]
 *      int *status;
 *    
 */
void
PRINT_SDPOSFUNC(FILE *F)
{
    char ndof[10], ncons[10], callstr[200];
    int s1, nq1;

    s1 = SysI.s ? SysI.s : 1;
    nq1 = SysI.nq ? SysI.nq : 1;

    esprintf(ndof, "%d", SysI.s);
    esprintf(ncons, "%d", SysI.nc);

    efprintf(F, "\n\
%{These routines are passed to %Aroot.%}");

/* sdposfunc
 *
 * Returns current perrs as a function of time and hinge positions.
 * Time is hidden in the first element of param[].  Hinge positions
 * are passed in using Euler angles for ball joints, so they must be
 * converted to Euler parameters before we can calculate the perrs.
 */
    declare_proc(F, 0, "posfunc",
      VT_ARRAY,               "vars",  s1, 0,
      VT_ARRAY,               "param", 1, 0,
      VT_USER, &SysI.type_Arr_nc, "resid", 
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_ARRAY,   "pos", nq1, 0,
      VT_ARRAY,   "vel", s1, 0,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    FORCNT("100", "i", ndof);
      RSET("vel%(i%)", 0.);
    ENDFOR("100");

    CALL("%Aang2st(vars,pos)");
    esprintf(callstr, "%Astate(param%(%@d%),pos,vel)", 0);
    CALL(callstr);
    if (SysI.np) {
        esprintf(callstr, "%Aumotion(param%(%@d%),pos,vel)", 0);
        CALL(callstr);
    }
    CALL("%Aperr(resid)");

    efprintf(F, Lang->proc_end);

/* sdvelfunc
 *
 * Returns current verrs as a function of time, hinge positions, and 
 * hinge velocities.  The vars are the hinge velocities.  The hinge
 * positions are in the first nq elements of param, with time following
 * in the nq+1st position.  The hinge positions are already in Euler
 * parameter form so no conversion is necessary.
 */
    declare_proc(F, 0, "velfunc",
      VT_ARRAY,               "vars",  s1, 0,
      VT_ARRAY,               "param", SysI.nq+1, 0,
      VT_USER, &SysI.type_Arr_nc, "resid", 
      0);
    efprintf(F, Lang->proc_dbegin);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    esprintf(callstr, "%Astate(param%(%@d%),param,vars)", SysI.nq);
    CALL(callstr);
    if (SysI.np) {
        esprintf(callstr, "%Aumotion(param%(%@d%),param,vars)", SysI.nq);
        CALL(callstr);
    }
    CALL("%Averr(resid)");

    efprintf(F, Lang->proc_end);

/* sdstatfunc
 *
 * Returns as a function of time and hinge positions, the current perrs
 * followed by the current hinge accelerations.  Hinge velocities are 
 * passed in param[0..nu-1].  Time is passed in param[nu].
 * Hinge positions
 * are passed in using Euler angles for ball joints, so they must be
 * converted to Euler parameters before we can calculate the perrs.
 */
    declare_proc(F, 0, "statfunc",
      VT_ARRAY,               "vars",  s1, 0,
      VT_ARRAY,               "param", SysI.s+1, 0,
      VT_ARRAY,        "resid", SysI.nc+s1, 0,
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_ARRAY,   "pos",     nq1, 0,
      VT_ARRAY,   "qdotdum", nq1, 0, 
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CALL("%Aang2st(vars,pos)");
    esprintf(callstr, "%Astate(param%(%@d%),pos,param)", SysI.s);
    CALL(callstr);
    if (SysI.np) {
        esprintf(callstr, "%Aumotion(param%(%@d%),pos,param)", SysI.s);
        CALL(callstr);
    }
    esprintf(callstr, "%Auforce(param%(%@d%),pos,param)", SysI.s);
    CALL(callstr);
    CALL("%Aperr(resid)");
    esprintf(callstr, "%Aderiv(qdotdum,%%Rresid%(%@d%))", SysI.nc);
    CALL(callstr);

    efprintf(F, Lang->proc_end);

/* sdstdyfunc
 *
 * Returns as a function of time, hinge positions, and hinge velocities,
 * the current perrs and verrs followed by the current hinge accelerations.  
 * Time is passed in param[0].  Hinge positions
 * are passed in using Euler angles for ball joints, so they must be
 * converted to Euler parameters before we can calculate the perrs.
 */
    declare_proc(F, 0, "stdyfunc",
      VT_ARRAY,               "vars",  2*s1, 0,
      VT_ARRAY,               "param", 1, 0,
      VT_ARRAY,        "resid", 2*SysI.nc+s1, 0,
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_ARRAY,   "pos",     nq1, 0,
      VT_ARRAY,   "qdotdum", nq1, 0, 
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CALL("%Aang2st(vars,pos)");
    esprintf(callstr, "%Astate(param%(%@d%),pos,%%Rvars%(%@d%))", 0, SysI.s);
    CALL(callstr);
    if (SysI.np) {
        esprintf(callstr, 
                 "%Aumotion(param%(%@d%),pos,%%Rvars%(%@d%))", 0,SysI.s);
        CALL(callstr);
    }
    esprintf(callstr, "%Auforce(param%(%@d%),pos,%%Rvars%(%@d%))", 0, SysI.s);
    CALL(callstr);
    CALL("%Aperr(resid)");
    esprintf(callstr, "%Averr(%%Rresid%(%@d%))", SysI.nc);
    CALL(callstr);
    esprintf(callstr, "%Aderiv(qdotdum,%%Rresid%(%@d%))", 2*SysI.nc);
    CALL(callstr);

    efprintf(F, Lang->proc_end);

/* sdmotfunc
 *
 * Returns as a function of time, hinge positions and hinge velocities,
 * the derivatives of the hinge positions and velocities.
 */

    efprintf(F, "\n\
%{This routine is passed to the integrator.%}");

    declare_proc(F, 0, "motfunc",
      VT_REAL,               "time",  
      VT_ARRAY,               "state",  SysI.nq+s1, 0,
      VT_ARRAY,        "dstate", SysI.nq+s1, 0,
      VT_ARRAY,               "param", 1, 0,
      VT_INTEGER|VT_BYREF, "status", 
      0);

    efprintf(F, Lang->proc_dbegin);
    if (SysI.nc) {
        declare_vars(F, 0, 
          VT_USER, &SysI.type_Arr_nc, "err",
          VT_INTEGER,                      "i",
          0);
    }
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    esprintf(callstr, "%Astate(time,state,%%Rstate%(%@d%))", SysI.nq);
    CALL(callstr);
    if (SysI.np) {
        esprintf(callstr, "%Aumotion(time,state,%%Rstate%(%@d%))", SysI.nq);
        CALL(callstr);
    }
    esprintf(callstr, "%Auforce(time,state,%%Rstate%(%@d%))", SysI.nq);
    CALL(callstr);
    esprintf(callstr, "%Aderiv(dstate,%%Rdstate%(%@d%))", SysI.nq);
    CALL(callstr);

    if (SysI.nc) {
        SETREF("status", "1"); /* assume constraints violated */
        CALL("%Averr(err)");
        FORCNT("100", "i", ncons);
          IFCOND efprintf(F, "%@D%s(err%(i%))%sparam%(%@d%)",
                   Lang->func_abs,GT,0);
            THEN RETURN;
          ENDIF;
        ENDFOR("100");
        CALL("%Aperr(err)");
        FORCNT("200", "i", ncons);
          IFCOND efprintf(F, "%@D%s(err%(i%))%sparam%(%@d%)",
                   Lang->func_abs,GT,0);
            THEN RETURN;
          ENDIF;
        ENDFOR("200");
    }

    SETREF("status", "0");
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDANAL
 *
 * Generate the analysis routines sdassemble(), sdinitvel(), sdstatic(),
 * sdsteady(), and sdmotion().
 *
 *    sdassemble(time,state,lock,tol,maxevals,fcnt,err)
 *      real time,state[nq+nu],tol
 *      int  lock[nu], maxevals, *fcnt, *err
 *
 * This routine performs an assembly analysis, attempting to meet all
 * position constraints to within tol.  err=0, success; 1, failed; 2,
 * exceeded maxevals calls to sdstate().  If there is any prescribed
 * motion, this routine expects to be able to call a user written
 * sdumotion() routine.
 *
 *    sdinitvel(time,state,lock,tol,maxevals,fcnt,err)
 *      real time,state[nq+nu],tol
 *      int  lock[nu], maxevals, *fcnt, *err, 
 *
 * This routine performs an initial velocity analysis, attempting to meet 
 * all velocity constraints to within tol.  err=0, success; 1, failed; 2,
 * exceeded maxevals calls to sdstate().  If there is any prescribed motion, 
 * this routine expects to be able to call a user written sdumotion() 
 * routine.  A successful sdassemble() should have been done first.  
 * Sdinitvel() does not alter the q's at all; it just fiddles with the u's.
 *
 *    sdstatic(time,state,lock,ctol,tol,maxevals,fcnt,err)
 *      real ctol
 *
 * Performs a static analysis of the system starting with the passed-in
 * q's in state, using the u's as they are supplied in state.  Tries to find 
 * a set of q's which yield all-zero udot's with the given u's.  Maintains 
 * position constraints to ctol, IGNORES velocity constraints, attempts
 * to get all the udot's below tol.  err=0, success; 1, failed; 2,
 * exceeded maxevals calls to sdstate().  The user-written sduforce()
 * routine will be called.  If there is any prescribed motion, sdumotion()
 * will be called as well.
 *
 *    sdsteady(time,state,lock,ctol,tol,maxevals,fcnt,err)
 *      real ctol
 *
 * Performs a steady motion analysis of the system starting with the passed-in
 * q's and u's in state.  Tries to find a set of q's and u's which yield 
 * all-zero udot's.  Maintains position constraints and velocity constraints
 * to ctol, attempts to get all the udot's below tol.  err=0, success; 
 * 1, failed; 2, exceeded maxevals calls to sdstate().  The user-written 
 * sduforce() routine will be called.  If there is any prescribed motion, 
 * sdumotion() will be called as well.
 *
 *    sdmotion(time,state,dstate,dt,ctol,tol,err)
 *      real dt
 *
 * Integrates the equations of motion from time to time+dt, updating
 * time, state, and dstate and achieving a local integration error below tol.  
 * This routine should be called once with dt=0
 * to initialize dstate and internal data structures (the step size).  After 
 * that, pass dstate in unchanged from one call to the next.  err=0, 
 * success; err=1, warning: possible discontinuity; err=2, a position or
 * velocity constraint error exceeded ctol.
 *    
 */

#define ERR_OK "0"
#define ERR_FAILED "1"
#define ERR_RANOUTOFTIME "2"

void
PRINT_SDANAL(FILE *F)
{
    int nfunc = SysI.nc,    /* problem size for assemble and initvel */
        nvar  = SysI.s?SysI.s : 1,
        nfnv  = nfunc+nvar,
        ndes;

    int s1, nq1;

    char numq[10],numu[10],numc[10],callstr[200],str_flt0[10];

    s1 = SysI.s ? SysI.s : 1;
    nq1 = SysI.nq ? SysI.nq : 1;

    esprintf(numq, "%d", SysI.nq);
    esprintf(numu, "%d", SysI.s);
    esprintf(numc, "%d", SysI.nc);
    esprintf(str_flt0, "%r", 0.);

/* sdassemble
 *
 * This routine performs assembly analysis using sdroot().
 */
    efprintf(F, "\n%{This routine performs assembly analysis.%}");

    declare_proc(F, 0,                 "assemble",
      VT_REAL,                               "time",  
      VT_ARRAY,                               "state", SysI.nq+s1, 0,
      VT_ARRAY|VT_INTEGER,         "lock", s1, 0,
      VT_REAL,                               "tol", 
      VT_INTEGER,                      "maxevals",
      VT_DUP|VT_BYREF,                 "fcnt",
      VT_DUP|VT_BYREF,                 "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, 0, 
      VT_USER, &SysI.type_Arr_nc,         "perrs", /* nfunc long */
      VT_ARRAY,                         "param", 1, 0,
      VT_INTEGER,                        "i", 
      0);
    if (Lang==&FORTRAN_language) /* sorry */
        efprintf(F,"external %Aposfunc%;\n");

    if (SysI.nc > 0) {
        declare_vars(F, 0, 
          VT_ARRAY,                        "jw", nfunc*nvar, 0,
          VT_ARRAY,                         "dw", 2*nfnv*nfnv, 0,
          VT_ARRAY,                        "rw", 7*nfunc + 9*nvar, 0,
          VT_ARRAY|VT_INTEGER,                 "iw", 4*nfnv, 0,
          VT_INTEGER,                        "rooterr",
          0);
    }

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdassemble);

    efprintf(F, "param%(%@d%)%=time%;\n", 0);
    if (SysI.nb)
        CALL("%Ast2ang(state,state)");
    if (SysI.nc) {
        esprintf(callstr, 
        "%Aroot(%Aposfunc,state,param,%d,%d,0,lock,tol,tol,maxevals,%%&\
jw,dw,rw,iw,perrs,fcnt,%%Rrooterr)",nfunc,nvar);
        CALL(callstr);
    } else {
        SETREF("err", ERR_OK);
        SETREF("fcnt", "0");
    }

    /* make sure the last call to sdstate() was made with the correct
       state */
    CALL("%Aposfunc(state,param,perrs)");
    INCREF("fcnt", "1");
    if (SysI.nb)
        CALL("%Aang2st(state,state)");
        
    if (SysI.nc) {
        IF ("rooterr", EQ, ERR_OK)
        THEN SETREF("err", ERR_OK);
        ELSE 
          IFCOND REF("fcnt"); efprintf(F, "%smaxevals", GE);
            THEN SETREF("err", ERR_RANOUTOFTIME);
            ELSE SETREF("err", ERR_FAILED);
          ENDIF;
        ENDIF;
    }

    efprintf(F, Lang->proc_end);

/* sdinitvel
 *
 * This routine performs initial velocity analysis using sdroot().
 */
    efprintf(F, "\n%{This routine performs initial velocity analysis.%}");

    declare_proc(F, 0,                 "initvel",
      VT_REAL,                               "time",  
      VT_ARRAY,                               "state", SysI.nq+s1, 0,
      VT_ARRAY|VT_INTEGER,         "lock", s1, 0,
      VT_REAL,                               "tol", 
      VT_INTEGER,                      "maxevals",
      VT_DUP|VT_BYREF,                 "fcnt",
      VT_INTEGER|VT_BYREF,         "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, 0, 
      VT_USER, &SysI.type_Arr_nc,         "verrs", /* nfunc long */
      VT_ARRAY,                         "param", SysI.nq+1, 0,
      VT_INTEGER,                        "i",
      0);
    if (Lang==&FORTRAN_language) /* sorry */
        efprintf(F,"external %Avelfunc%;\n");

    if (SysI.nc > 0) {
        declare_vars(F, 0, 
          VT_ARRAY,                        "jw", nfunc*nvar, 0,
          VT_ARRAY,                         "dw", 2*nfnv*nfnv, 0,
          VT_ARRAY,                        "rw", 7*nfunc + 9*nvar, 0,
          VT_ARRAY|VT_INTEGER,                 "iw", 4*nfnv, 0,
          VT_INTEGER,                        "rooterr",
          0);
    }

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdinitvel);

    FORCNT("100", "i", numq);
      SET("param%(i%)", "state%(i%)");
    ENDFOR("100");
    efprintf(F, "param%(%@d%)%=time%;\n",SysI.nq);
                                   
    if (SysI.nc) {
        esprintf(callstr, 
"%Aroot(%Avelfunc,%%Rstate%(%@d%),param,%d,%d,0,lock,tol,tol,maxevals,%%&\
jw,dw,rw,iw,verrs,fcnt,%%Rrooterr)",SysI.nq,nfunc,nvar);
        CALL(callstr);
    } else {
        SETREF("err", ERR_OK);
        SETREF("fcnt", "0");
    }

    /* make sure the last call to sdstate() was made with the correct
       state */
    esprintf(callstr,"%Avelfunc(%%Rstate%(%@d%),param,verrs)", SysI.nq);
    CALL(callstr);
    INCREF("fcnt", "1");
        
    if (SysI.nc) {
        IF ("rooterr", EQ, ERR_OK)
        THEN SETREF("err", ERR_OK);
        ELSE 
          IFCOND REF("fcnt"); efprintf(F, "%smaxevals", GE);
            THEN SETREF("err", ERR_RANOUTOFTIME);
            ELSE SETREF("err", ERR_FAILED);
          ENDIF;
        ENDIF;
    }

    efprintf(F, Lang->proc_end);

/* sdstatic
 *
 * This routine performs static analysis using sdroot().
 */
    nfunc = SysI.nc+s1;        /* problem size for static analysis */
    nvar  = s1;
    nfnv  = nfunc+nvar;
    ndes  = s1;

    efprintf(F, "\n%{This routine performs static analysis.%}");

    declare_proc(F, 0, "static",
      VT_REAL,               "time",  
      VT_ARRAY,               "state", SysI.nq+s1, 0,
      VT_ARRAY|VT_INTEGER, "lock", nvar, 0,
      VT_REAL,               "ctol", 
      VT_DUP,               "tol", 
      VT_INTEGER,      "maxevals",
      VT_DUP|VT_BYREF, "fcnt",
      VT_DUP|VT_BYREF, "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, 0, 
      VT_ARRAY,     "resid", nfunc, 0,
      VT_ARRAY,     "param", SysI.s+1, 0,
      VT_ARRAY,            "jw", nfunc*nvar, 0,
      VT_ARRAY,     "dw", 2*nfnv*nfnv, 0,
      VT_ARRAY,            "rw", 7*nfunc + 9*nvar, 0,
      VT_ARRAY|VT_INTEGER, "iw", 4*nfnv, 0,
      VT_INTEGER,   "rooterr",
      VT_DUP,       "i",
      0);
    if (Lang==&FORTRAN_language) /* sorry */
        efprintf(F,"external %Astatfunc%;\n");

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdstatic);

    FORCNT("100", "i", numu);
      efprintf(F, "param%(i%)%=state%(%d+i%)%;\n", SysI.nq);
    ENDFOR("100");
    efprintf(F, "param%(%@d%)%=time%;\n", SysI.s);
    if (SysI.nb)
        CALL("%Ast2ang(state,state)");
    esprintf(callstr, 
    "%Aroot(%Astatfunc,state,param,%d,%d,%d,lock,%%&ctol,tol,maxevals,\
jw,dw,rw,iw,resid,fcnt,%%Rrooterr)",nfunc,nvar,ndes);
    CALL(callstr);
    /* make sure last calls to sdstate() and sdderiv() were done on the
       correct state */
    CALL("%Astatfunc(state,param,resid)");
    INCREF("fcnt","1");
    if (SysI.nb)
        CALL("%Aang2st(state,state)");
    
    IF ("rooterr", EQ, ERR_OK)
    THEN SETREF("err", ERR_OK);
    ELSE 
      IFCOND REF("fcnt"); efprintf(F, "%smaxevals", GE);
        THEN SETREF("err", ERR_RANOUTOFTIME);
        ELSE SETREF("err", ERR_FAILED);
      ENDIF;
    ENDIF;

    efprintf(F, Lang->proc_end);

/* sdsteady
 *
 * This routine performs `steady motion' analysis using sdroot().
 */
    nfunc = 2*SysI.nc+s1;        /* problem size for steady analysis */
    nvar  = 2*s1;
    nfnv  = nfunc+nvar;
    ndes  = s1;

    efprintf(F, "\n%{This routine performs steady motion analysis.%}");

    declare_proc(F, 0, "steady",
      VT_REAL,               "time",  
      VT_ARRAY,               "state", SysI.nq+s1, 0,
      VT_ARRAY|VT_INTEGER, "lock", nvar, 0,
      VT_REAL,               "ctol", 
      VT_DUP,               "tol", 
      VT_INTEGER,      "maxevals",
      VT_DUP|VT_BYREF, "fcnt",
      VT_DUP|VT_BYREF, "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, 0, 
      VT_ARRAY,     "resid", nfunc, 0,
      VT_ARRAY,     "param", 1, 0,
      VT_ARRAY|VT_COND, SysI.nb, "vars", nvar, 0,
      0);
    declare_vars(F, 0, 
      VT_ARRAY,            "jw", nfunc*nvar, 0,
      VT_ARRAY,     "dw", 2*nfnv*nfnv, 0,        /* that's a lot! */
      VT_ARRAY,            "rw", 7*nfunc + 9*nvar, 0,
      VT_ARRAY|VT_INTEGER, "iw", 4*nfnv, 0,
      VT_INTEGER,   "rooterr",
      VT_DUP,       "i",
      0);
    if (Lang==&FORTRAN_language) /* sorry */
        efprintf(F,"external %Astdyfunc%;\n");

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdsteady);

    efprintf(F, "param%(%@d%)%=time%;\n", 0);

    /* If there are balls, we'll have to use our own array for the variables.
     * Just converting the state using sdst2ang() won't work because there
     * would be a gap of `nb' elements between the q's and the u's.
     */
    if (SysI.nb) {
        CALL("%Ast2ang(state,vars)");
        FORCNT("100", "i", numu);
          efprintf(F, "vars%(%d+i%)%=state%(%d+i%)%;\n", SysI.s,SysI.nq);
        ENDFOR("100");
    }
    esprintf(callstr, 
    "%Aroot(%Astdyfunc,%s,param,%d,%d,%d,lock,%%&ctol,tol,maxevals,\
jw,dw,rw,iw,resid,fcnt,%%Rrooterr)",SysI.nb ? "vars":"state", nfunc,nvar,ndes);
    CALL(callstr);
    /* make sure last calls to sdstate() and sdderiv() were done on the
       correct state */
    esprintf(callstr,"%Astdyfunc(%s,param,resid)",SysI.nb?"vars":"state");
    CALL(callstr);
    INCREF("fcnt","1");
    if (SysI.nb) {
        CALL("%Aang2st(vars,state)");
        FORCNT("200", "i", numu);
          efprintf(F, "state%(%d+i%)%=vars%(%d+i%)%;\n", SysI.nq,SysI.s);
        ENDFOR("200");
    }
    
    IF ("rooterr", EQ, ERR_OK)
    THEN SETREF("err", ERR_OK);
    ELSE 
      IFCOND REF("fcnt"); efprintf(F, "%smaxevals", GE);
        THEN SETREF("err", ERR_RANOUTOFTIME);
        ELSE SETREF("err", ERR_FAILED);
      ENDIF;
    ENDIF;

    efprintf(F, Lang->proc_end);

/* sdmotion
 *
 * This routine performs system integration using sdvinteg().
 */

    nfunc = SysI.nq+s1;  /* length of state array */

    efprintf(F, "\n%{This routine performs state integration.%}");

    declare_proc(F, 0, "motion",
      VT_REAL|VT_BYREF, "time",  
      VT_ARRAY,                "state", nfunc, 0,
      VT_DUP,                 "dstate",
      VT_REAL,                "dt", 
      VT_DUP,                "ctol", 
      VT_DUP,                "tol", 
      VT_INTEGER|VT_BYREF, "flag",
      VT_INTEGER|VT_BYREF, "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, DECL_STATIC, 
      VT_REAL, "step", /* also see below for Fortran hack */
      0);
    declare_vars(F, 0, 
      VT_ARRAY,                 "work", 6*nfunc, 0,
      VT_REAL,                 "ttime",
      VT_ARRAY,                 "param", 1, 0,
      VT_INTEGER,         "vintgerr",
      VT_DUP,                 "which",
      VT_DUP,                 "ferr",
      VT_DUP,                 "i",
      0);
    if (Lang==&FORTRAN_language) { /* sorry for the hack */
        efprintf(F,"external %Amotfunc%;\n");
        /* this forces the Fortran compiler to make "step" static */
        efprintf(F,"data step/%r/%;\n", 0.);
    }

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdmotion);

    efprintf(F, "param%(%@d%)%=ctol%;\n", 0);
    efprintf(F, "ttime%="); REF("time"); efprintf(F, "%;\n");

    IFCOND REF("flag"); efprintf(F, "%s0", NE);
    THEN
      CALL("%Amotfunc(ttime,state,dstate,param,%Rferr)");
      SET("step", "dt");
      SETREF("flag", "0");
    ENDIF;

    IF("step", LE, str_flt0);
    THEN
      SET("step", "dt");
    ENDIF;

    esprintf(callstr, 
       "%Avinteg(%Amotfunc,%%Rttime,state,dstate,param,dt,%%Rstep,%d,tol,%%&\
work,%%Rvintgerr,%%Rwhich)",nfunc);
    CALL(callstr);
    SETREF("time", "ttime");

    /* This works because there is only one thing which causes sdmotfunc()
     * to return bad status -- constraint violation.  That makes all the
     * error numbers the same between sdvinteg and sdmotion. 
     */
    SETREF("err", "vintgerr");

    efprintf(F, Lang->proc_end);

/* sdfmotion
 *
 * This routine performs system integration using sdfinteg().
 */

#define ERR_FMOTION_VIOLCONST         "1"

    nfunc = SysI.nq+s1;  /* length of state array */

    efprintf(F, "\n\
%{This routine performs state integration with a fixed-step integrator.%}");

    declare_proc(F, 0, "fmotion",
      VT_REAL|VT_BYREF, "time",  
      VT_ARRAY,                "state", nfunc, 0,
      VT_DUP,                 "dstate",
      VT_REAL,                "dt", 
      VT_DUP,                "ctol", 
      VT_INTEGER|VT_BYREF, "flag",
      VT_REAL|VT_BYREF,           "errest", 
      VT_INTEGER|VT_BYREF, "err",
      0);
    efprintf(F, Lang->proc_dbegin);
 
    declare_vars(F, 0, 
      VT_ARRAY,                 "work", 4*nfunc, 0,
      VT_REAL,                 "ttime",
      VT_ARRAY,          "param", 1, 0,
      VT_INTEGER,         "ferr",
      VT_DUP,                 "i",
      0);
    if (Lang==&FORTRAN_language) /* sorry */
        efprintf(F,"external %Amotfunc%;\n");

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    chkgentime(F, ROU_sdfmotion);

    efprintf(F, "param%(%@d%)%=ctol%;\n", 0);
    SETREF("err", ERR_OK);
    efprintf(F, "ttime%="); REF("time"); efprintf(F, "%;\n");

    IFCOND REF("flag"); efprintf(F, "%s0", NE);
    THEN 
      CALL("%Amotfunc(ttime,state,dstate,param,%Rferr)");
      SETREF("flag", "0");
    ENDIF;

    esprintf(callstr, 
        "%Afinteg(%Amotfunc,%%Rttime,state,dstate,param,dt,%d,%%&\
work,errest,%%Rferr)", nfunc);
    CALL(callstr);
    IF("ferr", NE, ERR_OK);
      THEN SETREF("err", ERR_FMOTION_VIOLCONST);
    ENDIF;
    SETREF("time", "ttime");
    
    efprintf(F, Lang->proc_end);
}
