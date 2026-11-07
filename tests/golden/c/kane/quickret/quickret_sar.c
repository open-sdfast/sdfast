/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[5],
    double param[1],
    double resid[11])
{
    int i;
    double pos[5],vel[5];

    for (i = 0; i < 5; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdumotion(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[5],
    double param[6],
    double resid[11])
{

    sdstate(param[5],param,vars);
    sdumotion(param[5],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[5],
    double param[6],
    double resid[16])
{
    double pos[5],qdotdum[5];

    sdang2st(vars,pos);
    sdstate(param[5],pos,param);
    sdumotion(param[5],pos,param);
    sduforce(param[5],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[11]);
}

void sdstdyfunc(double vars[10],
    double param[1],
    double resid[27])
{
    double pos[5],qdotdum[5];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[5]);
    sdumotion(param[0],pos,&vars[5]);
    sduforce(param[0],pos,&vars[5]);
    sdperr(resid);
    sdverr(&resid[11]);
    sdderiv(qdotdum,&resid[22]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[10],
    double dstate[10],
    double param[1],
    int *status)
{
    double err[11];
    int i;

    sdstate(time,state,&state[5]);
    sdumotion(time,state,&state[5]);
    sduforce(time,state,&state[5]);
    sdderiv(dstate,&dstate[5]);
    *status = 1;
    sdverr(err);
    for (i = 0; i < 11; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    sdperr(err);
    for (i = 0; i < 11; i++) {
        if (fabs(err[i]) > param[0]) {
            return;
        }
    }
    *status = 0;
}

/* This routine performs assembly analysis. */

void sdassemble(double time,
    double state[10],
    int lock[5],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[11],param[1];
    int i;
    double jw[55],dw[512],rw[122];
    int iw[64],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdroot(sdposfunc,state,param,11,5,0,lock,tol,tol,maxevals,
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
    double state[10],
    int lock[5],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[11],param[6];
    int i;
    double jw[55],dw[512],rw[122];
    int iw[64],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 5; i++) {
        param[i] = state[i];
    }
    param[5] = time;
    sdroot(sdvelfunc,&state[5],param,11,5,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[5],param,verrs);
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
    double state[10],
    int lock[5],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[16],param[6],jw[80],dw[882],rw[157];
    int iw[84],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 5; i++) {
        param[i] = state[5+i];
    }
    param[5] = time;
    sdroot(sdstatfunc,state,param,16,5,5,lock,
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
    double state[10],
    int lock[10],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[27],param[1];
    double jw[270],dw[2738],rw[279];
    int iw[148],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdroot(sdstdyfunc,state,param,27,10,5,lock,
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
    double state[10],
    double dstate[10],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[60],ttime,param[1];
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,10,tol,work,&vintgerr,
      &which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[10],
    double dstate[10],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[40],ttime,param[1];
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
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,10,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
