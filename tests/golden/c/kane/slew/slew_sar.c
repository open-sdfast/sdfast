/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
#include <math.h>

/* These routines are passed to sdroot. */

void sdposfunc(double vars[11],
    double param[1],
    double resid[1])
{
    int i;
    double pos[12],vel[11];

    for (i = 0; i < 11; i++) {
        vel[i] = 0.;
    }
    sdang2st(vars,pos);
    sdstate(param[0],pos,vel);
    sdumotion(param[0],pos,vel);
    sdperr(resid);
}

void sdvelfunc(double vars[11],
    double param[13],
    double resid[1])
{

    sdstate(param[12],param,vars);
    sdumotion(param[12],param,vars);
    sdverr(resid);
}

void sdstatfunc(double vars[11],
    double param[12],
    double resid[12])
{
    double pos[12],qdotdum[12];

    sdang2st(vars,pos);
    sdstate(param[11],pos,param);
    sdumotion(param[11],pos,param);
    sduforce(param[11],pos,param);
    sdperr(resid);
    sdderiv(qdotdum,&resid[1]);
}

void sdstdyfunc(double vars[22],
    double param[1],
    double resid[13])
{
    double pos[12],qdotdum[12];

    sdang2st(vars,pos);
    sdstate(param[0],pos,&vars[11]);
    sdumotion(param[0],pos,&vars[11]);
    sduforce(param[0],pos,&vars[11]);
    sdperr(resid);
    sdverr(&resid[1]);
    sdderiv(qdotdum,&resid[2]);
}

/* This routine is passed to the integrator. */

void sdmotfunc(double time,
    double state[23],
    double dstate[23],
    double param[1],
    int *status)
{
    double err[1];
    int i;

    sdstate(time,state,&state[12]);
    sdumotion(time,state,&state[12]);
    sduforce(time,state,&state[12]);
    sdderiv(dstate,&dstate[12]);
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
    double state[23],
    int lock[11],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double perrs[1],param[1];
    int i;
    double jw[11],dw[288],rw[106];
    int iw[48],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(50,42);
    }
    param[0] = time;
    sdst2ang(state,state);
    sdroot(sdposfunc,state,param,1,11,0,lock,tol,tol,maxevals,
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
    double state[23],
    int lock[11],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double verrs[1],param[13];
    int i;
    double jw[11],dw[288],rw[106];
    int iw[48],rooterr;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(51,42);
    }
    for (i = 0; i < 12; i++) {
        param[i] = state[i];
    }
    param[12] = time;
    sdroot(sdvelfunc,&state[12],param,1,11,0,lock,tol,tol,maxevals,
      jw,dw,rw,iw,verrs,fcnt,&rooterr);
    sdvelfunc(&state[12],param,verrs);
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
    double state[23],
    int lock[11],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[12],param[12],jw[132],dw[1058],rw[183];
    int iw[92],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(52,42);
    }
    for (i = 0; i < 11; i++) {
        param[i] = state[12+i];
    }
    param[11] = time;
    sdst2ang(state,state);
    sdroot(sdstatfunc,state,param,12,11,11,lock,
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
    double state[23],
    int lock[22],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
{
    double resid[13],param[1],vars[22];
    double jw[286],dw[2450],rw[289];
    int iw[140],rooterr,i;

    sdgentime(&i);
    if (i != TIMESTAMP) {
        sdseterr(53,42);
    }
    param[0] = time;
    sdst2ang(state,vars);
    for (i = 0; i < 11; i++) {
        vars[11+i] = state[12+i];
    }
    sdroot(sdstdyfunc,vars,param,13,22,11,lock,
      ctol,tol,maxevals,jw,dw,rw,iw,resid,fcnt,&rooterr);
    sdstdyfunc(vars,param,resid);
    *fcnt = *fcnt+1;
    sdang2st(vars,state);
    for (i = 0; i < 11; i++) {
        state[12+i] = vars[11+i];
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
    double state[23],
    double dstate[23],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
{
    static double step;
    double work[138],ttime,param[1];
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
    sdvinteg(sdmotfunc,&ttime,state,dstate,param,dt,&step,23,tol,work,&vintgerr,
      &which);
    *time = ttime;
    *err = vintgerr;
}

/* This routine performs state integration with a fixed-step integrator. */

void sdfmotion(double *time,
    double state[23],
    double dstate[23],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
{
    double work[92],ttime,param[1];
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
    sdfinteg(sdmotfunc,&ttime,state,dstate,param,dt,23,work,errest,&ferr);
    if (ferr != 0) {
        *err = 1;
    }
    *time = ttime;
}
