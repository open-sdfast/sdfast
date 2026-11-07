/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[3],
    double param[1],
    double resid[5])
{
    int i;
    double pos[3],vel[3];

    for (i = 0; i < 3; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[3],
    double param[4],
    double resid[5])
{

    sdstate(param[3],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[3],
    double param[4],
    double resid[8])
{
    double pos[3],qdotdum[3];

    sdang2st(vars,pos);
    sdstate(param[3],pos,param);
    sduforce(param[3],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[5]);
}

void sdstdyfunc(double vars[6],
    double param[1],
    double resid[13])
{
    double pos[3],qdotdum[3];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[3]);
    sduforce(param[0],pos,&vars[3]);
    sdperr(resid);
    sdverr(&resid[5]);
    sdderiv(qdotdum,&resid[10]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[6],
    double dstate[6],
    double param[1],
    int *status)
{
    double err[5];
    int i;

    sdstate(time,state,&state[3]);
    sduforce(time,state,&state[3]);
    sdderiv(dstate,&dstate[3]);
    *status = 1;
    sdverr(err);
    for (i = 0; i < 5; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    sdperr(err);
    for (i = 0; i < 5; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    *status = 0;
}

/* This routine performs assembly analysis. */

void sdassemble(double time,
    double state[6],
    int lock[3],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[5],param[1];
    int i;
    double jw[15],dw[128],rw[62];
    int iw[32],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdroot(sdposfunc,state,param,5,3,0,lock,tol,tol,maxevals,
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
    double state[6],
    int lock[3],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[5],param[4];
    int i;
    double jw[15],dw[128],rw[62];
    int iw[32],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 3; i++) {
        param[i] = state[i];
    }
    param[3] = time;
    sdroot(sdvelfunc,&state[3],param,5,3,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[3],param,verrs);
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
    double state[6],
    int lock[3],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[8],param[4],jw[24],dw[242],rw[83];
    int iw[44],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 3; i++) {
        param[i] = state[3+i];
    }
    param[3] = time;
    sdroot(sdstatfunc,state,param,8,3,3,lock,
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
    double state[6],
    int lock[6],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[13],param[1];
    double jw[78],dw[722],rw[145];
    int iw[76],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdroot(sdstdyfunc,state,param,13,6,3,lock,
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
    double state[6],
    double dstate[6],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[36],ttime,param[1];
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,6,tol,work,&vintgerr,&
      which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[6],
    double dstate[6],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[24],ttime,param[1];
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
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,6,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
