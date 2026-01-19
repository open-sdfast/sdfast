/*
Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[6],
    double param[1],
    double resid[3])
{
    int i;
    double pos[7],vel[6];

    for (i = 0; i < 6; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdumotion(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[6],
    double param[8],
    double resid[3])
{

    sdstate(param[7],param,vars);
    sdumotion(param[7],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[6],
    double param[7],
    double resid[9])
{
    double pos[7],qdotdum[7];

    sdang2st(vars,pos);
    sdstate(param[6],pos,param);
    sdumotion(param[6],pos,param);
    sduforce(param[6],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[3]);
}

void sdstdyfunc(double vars[12],
    double param[1],
    double resid[12])
{
    double pos[7],qdotdum[7];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[6]);
    sdumotion(param[0],pos,&vars[6]);
    sduforce(param[0],pos,&vars[6]);
    sdperr(resid);
    sdverr(&resid[3]);
    sdderiv(qdotdum,&resid[6]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[13],
    double dstate[13],
    double param[1],
    int *status)
{
    double err[3];
    int i;

    sdstate(time,state,&state[7]);
    sdumotion(time,state,&state[7]);
    sduforce(time,state,&state[7]);
    sdderiv(dstate,&dstate[7]);
    *status = 1;
    sdverr(err);
    for (i = 0; i < 3; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    sdperr(err);
    for (i = 0; i < 3; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    *status = 0;
}

/* This routine performs assembly analysis. */

void sdassemble(double time,
    double state[13],
    int lock[6],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[3],param[1];
    int i;
    double jw[18],dw[162],rw[75];
    int iw[36],rooterr;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdst2ang(state,state);
    sdroot(sdposfunc,state,param,3,6,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,perrs,fcnt,&rooterr);
    sdposfunc(state,param,perrs);
    *fcnt = *fcnt+1;
    sdang2st(state,state);
    if (rooterr == 0) {
        *err = 0;
    } else {
        if (*fcnt >= maxevals) {
            *err = 2;
        } else {
            *err = 1;
        }
    }
}

/* This routine performs initial velocity analysis. */

void sdinitvel(double time,
    double state[13],
    int lock[6],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[3],param[8];
    int i;
    double jw[18],dw[162],rw[75];
    int iw[36],rooterr;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(51,42);
    }
    for (i = 0; i < 7; i++) {
        param[i] = state[i];
    }
    param[7] = time;
    sdroot(sdvelfunc,&state[7],param,3,6,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[7],param,verrs);
    *fcnt = *fcnt+1;
    if (rooterr == 0) {
        *err = 0;
    } else {
        if (*fcnt >= maxevals) {
            *err = 2;
        } else {
            *err = 1;
        }
    }
}

/* This routine performs static analysis. */

void sdstatic(double time,
    double state[13],
    int lock[6],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[9],param[7],jw[54],dw[450],rw[117];
    int iw[60],rooterr,i;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(52,42);
    }
    for (i = 0; i < 6; i++) {
        param[i] = state[7+i];
    }
    param[6] = time;
    sdst2ang(state,state);
    sdroot(sdstatfunc,state,param,9,6,6,lock,
      ctol,tol,maxevals,jw,dw,rw,iw,resid,fcnt,&rooterr);
    sdstatfunc(state,param,resid);
    *fcnt = *fcnt+1;
    sdang2st(state,state);
    if (rooterr == 0) {
        *err = 0;
    } else {
        if (*fcnt >= maxevals) {
            *err = 2;
        } else {
            *err = 1;
        }
    }
}

/* This routine performs steady motion analysis. */

void sdsteady(double time,
    double state[13],
    int lock[12],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[12],param[1],vars[12];
    double jw[144],dw[1152],rw[192];
    int iw[96],rooterr,i;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdst2ang(state,vars);
    for (i = 0; i < 6; i++) {
        vars[6+i] = state[7+i];
    }
    sdroot(sdstdyfunc,vars,param,12,12,6,lock,
      ctol,tol,maxevals,jw,dw,rw,iw,resid,fcnt,&rooterr);
    sdstdyfunc(vars,param,resid);
    *fcnt = *fcnt+1;
    sdang2st(vars,state);
    for (i = 0; i < 6; i++) {
        state[7+i] = vars[6+i];
    }
    if (rooterr == 0) {
        *err = 0;
    } else {
        if (*fcnt >= maxevals) {
            *err = 2;
        } else {
            *err = 1;
        }
    }
}

/* This routine performs state integration. */

void sdmotion(double *time,
    double state[13],
    double dstate[13],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[78],ttime,param[1];
    int vintgerr,which,ferr,i;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(54,42);
    }
    param[0] = ctol;
    ttime = *time;
    if (*flag != 0) {
        sdmotfunc(ttime,state,dstate,param,&ferr);
        step = dt;
        *flag = 0;
    }
    if (step <= 0.) {
        step = dt;
    }
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,13,tol,work,&vintgerr,
      &which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[13],
    double dstate[13],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[52],ttime,param[1];
    int ferr,i;

    sdgentime(&i);
    if (i != 131045) {
        sdseterr(55,42);
    }
    param[0] = ctol;
    *err = 0;
    ttime = *time;
    if (*flag != 0) {
        sdmotfunc(ttime,state,dstate,param,&ferr);
        *flag = 0;
    }
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,13,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
