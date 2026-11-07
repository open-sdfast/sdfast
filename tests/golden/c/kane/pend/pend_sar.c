/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[1],
    double param[1],
    double resid[1])
{
    int i;
    double pos[1],vel[1];

    for (i = 0; i < 1; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[1],
    double param[2],
    double resid[1])
{

    sdstate(param[1],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[1],
    double param[2],
    double resid[1])
{
    double pos[1],qdotdum[1];

    sdang2st(vars,pos);
    sdstate(param[1],pos,param);
    sduforce(param[1],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[0]);
}

void sdstdyfunc(double vars[2],
    double param[1],
    double resid[1])
{
    double pos[1],qdotdum[1];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[1]);
    sduforce(param[0],pos,&vars[1]);
    sdperr(resid);
    sdverr(&resid[0]);
    sdderiv(qdotdum,&resid[0]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[2],
    double dstate[2],
    double param[1],
    int *status)
{

    sdstate(time,state,&state[1]);
    sduforce(time,state,&state[1]);
    sdderiv(dstate,&dstate[1]);
    *status = 0;
}

/* This routine performs assembly analysis. */

void sdassemble(double time,
    double state[2],
    int lock[1],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[1],param[1];
    int i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    *err = 0;
    *fcnt = 0;
    sdposfunc(state,param,perrs);
    *fcnt = *fcnt+1;
}

/* This routine performs initial velocity analysis. */

void sdinitvel(double time,
    double state[2],
    int lock[1],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[1],param[2];
    int i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 1; i++) {
        param[i] = state[i];
    }
    param[1] = time;
    *err = 0;
    *fcnt = 0;
    sdvelfunc(&state[1],param,verrs);
    *fcnt = *fcnt+1;
}

/* This routine performs static analysis. */

void sdstatic(double time,
    double state[2],
    int lock[1],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[1],param[2],jw[1],dw[8],rw[16];
    int iw[8],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 1; i++) {
        param[i] = state[1+i];
    }
    param[1] = time;
    sdroot(sdstatfunc,state,param,1,1,1,lock,
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
    double state[2],
    int lock[2],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[1],param[1];
    double jw[2],dw[18],rw[25];
    int iw[12],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdroot(sdstdyfunc,state,param,1,2,1,lock,
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
    double state[2],
    double dstate[2],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[12],ttime,param[1];
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,2,tol,work,&vintgerr,&
      which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[2],
    double dstate[2],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[8],ttime,param[1];
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
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,2,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
