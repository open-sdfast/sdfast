/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[4],
    double param[1],
    double resid[3])
{
    int i;
    double pos[4],vel[4];

    for (i = 0; i < 4; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[4],
    double param[5],
    double resid[3])
{

    sdstate(param[4],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[4],
    double param[5],
    double resid[7])
{
    double pos[4],qdotdum[4];

    sdang2st(vars,pos);
    sdstate(param[4],pos,param);
    sduforce(param[4],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[3]);
}

void sdstdyfunc(double vars[8],
    double param[1],
    double resid[10])
{
    double pos[4],qdotdum[4];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[4]);
    sduforce(param[0],pos,&vars[4]);
    sdperr(resid);
    sdverr(&resid[3]);
    sdderiv(qdotdum,&resid[6]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[8],
    double dstate[8],
    double param[1],
    int *status)
{
    double err[3];
    int i;

    sdstate(time,state,&state[4]);
    sduforce(time,state,&state[4]);
    sdderiv(dstate,&dstate[4]);
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
    double state[8],
    int lock[4],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[3],param[1];
    int i;
    double jw[12],dw[98],rw[57];
    int iw[28],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdroot(sdposfunc,state,param,3,4,0,lock,tol,tol,maxevals,
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
    double state[8],
    int lock[4],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[3],param[5];
    int i;
    double jw[12],dw[98],rw[57];
    int iw[28],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 4; i++) {
        param[i] = state[i];
    }
    param[4] = time;
    sdroot(sdvelfunc,&state[4],param,3,4,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[4],param,verrs);
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
    double state[8],
    int lock[4],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[7],param[5],jw[28],dw[242],rw[85];
    int iw[44],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 4; i++) {
        param[i] = state[4+i];
    }
    param[4] = time;
    sdroot(sdstatfunc,state,param,7,4,4,lock,
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
    double state[8],
    int lock[8],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[10],param[1];
    double jw[80],dw[648],rw[142];
    int iw[72],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdroot(sdstdyfunc,state,param,10,8,4,lock,
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
    double state[8],
    double dstate[8],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[48],ttime,param[1];
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,8,tol,work,&vintgerr,&
      which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[8],
    double dstate[8],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[32],ttime,param[1];
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
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,8,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
