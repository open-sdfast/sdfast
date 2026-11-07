/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[8],
    double param[1],
    double resid[1])
{
    int i;
    double pos[8],vel[8];

    for (i = 0; i < 8; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdumotion(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[8],
    double param[9],
    double resid[1])
{

    sdstate(param[8],param,vars);
    sdumotion(param[8],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[8],
    double param[9],
    double resid[9])
{
    double pos[8],qdotdum[8];

    sdang2st(vars,pos);
    sdstate(param[8],pos,param);
    sdumotion(param[8],pos,param);
    sduforce(param[8],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[1]);
}

void sdstdyfunc(double vars[16],
    double param[1],
    double resid[10])
{
    double pos[8],qdotdum[8];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[8]);
    sdumotion(param[0],pos,&vars[8]);
    sduforce(param[0],pos,&vars[8]);
    sdperr(resid);
    sdverr(&resid[1]);
    sdderiv(qdotdum,&resid[2]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[16],
    double dstate[16],
    double param[1],
    int *status)
{
    double err[1];
    int i;

    sdstate(time,state,&state[8]);
    sdumotion(time,state,&state[8]);
    sduforce(time,state,&state[8]);
    sdderiv(dstate,&dstate[8]);
    *status = 1;
    sdverr(err);
    for (i = 0; i < 1; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    sdperr(err);
    for (i = 0; i < 1; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    *status = 0;
}

/* This routine performs assembly analysis. */

void sdassemble(double time,
    double state[16],
    int lock[8],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[1],param[1];
    int i;
    double jw[8],dw[162],rw[79];
    int iw[36],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdroot(sdposfunc,state,param,1,8,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,perrs,fcnt,&rooterr);
    sdposfunc(state,param,perrs);
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

/* This routine performs initial velocity analysis. */

void sdinitvel(double time,
    double state[16],
    int lock[8],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[1],param[9];
    int i;
    double jw[8],dw[162],rw[79];
    int iw[36],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 8; i++) {
        param[i] = state[i];
    }
    param[8] = time;
    sdroot(sdvelfunc,&state[8],param,1,8,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[8],param,verrs);
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
    double state[16],
    int lock[8],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[9],param[9],jw[72],dw[578],rw[135];
    int iw[68],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 8; i++) {
        param[i] = state[8+i];
    }
    param[8] = time;
    sdroot(sdstatfunc,state,param,9,8,8,lock,
      ctol,tol,maxevals,jw,dw,rw,iw,resid,fcnt,&rooterr);
    sdstatfunc(state,param,resid);
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

/* This routine performs steady motion analysis. */

void sdsteady(double time,
    double state[16],
    int lock[16],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[10],param[1];
    double jw[160],dw[1352],rw[214];
    int iw[104],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdroot(sdstdyfunc,state,param,10,16,8,lock,
      ctol,tol,maxevals,jw,dw,rw,iw,resid,fcnt,&rooterr);
    sdstdyfunc(state,param,resid);
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

/* This routine performs state integration. */

void sdmotion(double *time,
    double state[16],
    double dstate[16],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[96],ttime,param[1];
    int vintgerr,which,ferr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,16,tol,work,&vintgerr,
      &which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[16],
    double dstate[16],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[64],ttime,param[1];
    int ferr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(55,42);
    }
    param[0] = ctol;
    *err = 0;
    ttime = *time;
    if (*flag != 0) {
        sdmotfunc(ttime,state,dstate,param,&ferr);
        *flag = 0;
    }
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,16,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
