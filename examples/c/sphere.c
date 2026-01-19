/* The nested spheres problem using SD/FAST.
 *
 * @(#)sphere.c 1.1 91/08/25 - Copyright 1991 Symbolic Dynamics, Inc.
 * All rights reserved.
 *
 * Authors: Michael Sherman
 *          Dan Rosenthal
 *
 * In this example, the hollow outer sphere is fixed to ground.  The inner 
 * sphere moves inside, and may be
 *         1) free in the interior,
 *         2) rolling along the inside surface, or
 *         3) slipping along the inside surface.
 *
 * Most of the code below deals with recognizing and implementing the 
 * transitions among these three conditions.  Collisions between the
 * spheres are modeled as impacts, with a coefficient of restitution.
 * A static and dynamic coefficient of friction are used to model
 * the behavior of the spheres when slipping, whether along the 
 * surface or at a point during an impact.  No attempt is made to
 * model rolling resistance, although that could easily be added.  It would
 * also be straightforward to allow the outer sphere to move relative to
 * ground, but that has not been done here.
 *
 * The simplest way to model this system with SD/FAST would be to "attach" the 
 * inner sphere to ground with a six degree-of-freedom joint.  However, this
 * would require us to write some equations to convert the resulting
 * coordinates into more appropriate spherical coordinates.  Instead, we
 * compose several joints (using two intermediate massless frames) to 
 * produce spherical coordinates directly, and let SD/FAST write the equations.
 *
 *                  .---.              y
 *                / INNER \            |               |
 *                |   O   |            |____x          | gravity -9.8y
 *                \       /           /                V
 *          OUTER   `---'            z
 *                                  
 * The first massless frame, called LATLONG, is attached to the 
 * point at the center of the outer sphere by a U-joint (two perpendicular
 * rotational degrees of freedom).  The first coordinate is a rotation 
 * about y (corresponding to longitude), the second is a rotation about x
 * (corresponding to latitude).  
 *
 * The other massless frame, called RADIUS, is attached to LATLONG by
 * a sliding joint in the RADIUS local y direction.  This coordinate
 * represents the radial distance from the outer sphere center to the
 * inner sphere center.  The inner sphere is attached to the RADIUS
 * frame by a ball joint at its center.
 * 
 * In the chosen reference configuration, where all the coordinates
 * are zero, the inner sphere is in the center of the outer sphere.
 * If the RADIUS coordinate is set to (R_OUTER-R_INNER) while all
 * other coordinates are left zero, the inner sphere touches the
 * outer sphere at the top.  Note that this means the latitude is
 * considered zero at the top, 90 degrees at the equator, contrary to
 * common practice on maps where the equator is at zero latitude.
 *
 * Three constraints are used in the model to (1) prevent the inner
 * sphere from penetrating the outer sphere, and (2) while the inner
 * sphere should be rolling, to prevent relative slipping between
 * the surfaces (in two directions).  These constraints are turned on
 * and off as appropriate.  The first is enabled only when the the
 * sphere is rolling or sliding along the inner surface, and the
 * other two constraints are enabled only while rolling.
 *
 * The "non-penetration" constraint is implemented using SD/FAST's built-in
 * prescribed motion facility on the RADIUS sliding joint.  Whenever
 * the spheres are supposed to maintain contact, the inner
 * sphere is simply presribed to be at the radius which just causes it
 * to contact the outer sphere.  The sign of the resulting constraint
 * multiplier is monitored, and the prescribed motion constraint is turned
 * off if it would prevent the inner sphere from moving into the interior.
 *
 * The "no-slip" constraints are implemented using SD/FAST's "user constraint"
 * facility.  These constraints are enabled only when the spheres are 
 * supposed to be in rolling contact.  When on, the resulting force (found
 * in the multipliers) is monitored and if its magnitude exceeds the
 * static coefficient of friction times the normal force, the constraints
 * are turned off and the sphere begins to slip.
 *
 * The variable-step integrator provided with SD/FAST is used to advance
 * time.  This integrator is capable of precisely locating events in
 * time, and we use this feature to isolate in time the transitions
 * among the free, rolling and slipping conditions.  This way errors are
 * avoided which would occur if a single integration step contained transitions.
 *
 * The SD/FAST root finder is used to solve the impact problem.  It is also
 * used in the transition from slipping to rolling to eliminate any
 * residual slipping velocity when the decision is made to roll.
 *
 * Fixed-point iteration is used to solve for the friction forces during
 * sliding.  
 *
 * Here are the rules for changing from one condition to another:
 *
 *   From        To          When
 *
 *   UNKNOWN     FREE        Inner sphere is not touching the outer sphere.
 *   UNKNOWN     ROLLING     Inner sphere touching and required tangential
 *                             force does not exceed mu_static*normal_force.
 *   UNKNOWN     SLIPPING    Inner sphere touching and tangential force 
 *                             required for rolling > mu_static*normal_force.
 *
 *   FREE        ROLLING     Impact produces rebound velocity less than
 *                             EPS_MINREBOUNDVEL, and required tangential
 *                             force <= mu_static*normal_force.
 *   FREE        SLIPPING    Impact produces rebound velocity less than
 *                             EPS_MINREBOUNDVEL, and tangential force required
 *                             to produce rolling > mu_static*normal_force.
 *
 *   ROLLING     FREE        Normal force preventing inner sphere from
 *                             penetrating outer changes sign.
 *   ROLLING     SLIPPING    Tangential force required to continue rolling
 *                             would exceed mu_static*normal_force.
 *
 *   SLIPPING    FREE        Normal force preventing inner sphere from
 *                             penetrating outer changes sign.
 *   SLIPPING    ROLLING     Relative slip velocity drops below EPS_MINSLIPVEL,
 *                             and is decreasing.
 */

#include <stdio.h>
#include <math.h>

/* Sphere radii. */
#define R_INNER   1.
#define R_OUTER   2.

/* frames (bodies) */
#define GROUND    -1
#define LATLONG    0  /* frame giving latitude and longitude of inner */
#  define LONGITUDE 0 /*    these are the meanings of the two axes    */
#  define LATITUDE  1
#define RADIUS      1 /* frame giving radial distance of inner com 
                         from outer com */
#define INNER    2    /* the body (frame) number of the inner sphere */
#define OUTER    GROUND    /* body number of outer sphere, here same as ground */

/* state variables */
#define NQ    7
#define NU    6
#define NEQ    (NQ+NU)

/* constraints (includes both prescribed motion and user constraints) */
#define NC            3
#define MULT_NORMAL   0    /* meaning of multipliers */
#define MULT_SLIPX    1    
#define MULT_SLIPZ    2    

/* these are the user constraint numbers */
#define NUSERC    2 
#define CSLIPX    0    
#define CSLIPZ    1

/* The system is in one of 3 "conditions", or unknown at start of program. */
#define COND_UNKNOWN     0
#define COND_FREE     1    /* free in the interior of the outer sphere */
#define COND_ROLLING     2    /* rolling along the inner surface */
#define COND_SLIPPING     3    /* slipping along the inner surface */

char *condnames[] = {
    "UNKNOWN",
    "FREE",
    "ROLLING",
    "SLIPPING",
};

/* Condition change threshholds. */
#define EPS_MINSLIPVEL    1e-3    /* start rolling when relative velocity
                                     is this low if previously slipping */
#define EPS_MINREBOUNDVEL 1e-3    /* a normal velocity smaller than this
                                     is considered stopped when near wall */

/* Don't use a velocity whose magnitude is smaller than this as a slip
 * direction while in the ROLLING->SLIPPING transition.  Use the remembered
 * direction instead.  THIS MUST BE LESS THAN EPS_MINSLIPVEL!!!
 */
#define EPS_MINMEANINGFULVEL    TOL

/* Friction force convergence threshhold -- continue iterating until
   forces are consistent to this tolerance. */
#define EPS_FORCE    1e-10

/* Integration parameters. */
#define DT     .02
#define NSTEP  500
#define TOL    1e-7    /* allowable integration error (relative) */
#define CTOL   1e-5    /* maximum allowable constraint violation */

/* Error returns from integrator. */
#define OVERBUMP        1
#define CANTCONTINUE    2
#define USERSTAT        3
#  define WANTCONDCHG   1    /* returns from our deriv routine */
#  define BADSTATE      2
#  define CONSVIOL      3

/* Ignorable (in this case) complaint from SD/FAST */
#define ERR_SINGULAR_MASS_MATRIX 47

#define SIGN(x) ((x)<0.?-1.:1.)
#define SQR(x)    ((x)*(x))

/* Coefficients of friction and restitution. */
double mu_static, mu_dynamic, coef_rest;

/* The current condition (free, slipping, or rolling) is maintained
   as a global to be accessible throughout the program. */
int syscond = COND_UNKNOWN;

/* Just at the point of transition from ROLLING to SLIPPING, the apparent
 * slip direction is recorded here.  This can be used to orient the 
 * friction force until sufficient slipping velocity has been built up. 
 */
double slipdir[3];

main()
{
    int       i,status,err,which,wantcond,condchg;
    double    t,y[NEQ],dy[NEQ],param[1],step,tout,tfinal;
    double    imp[3],com[3],work[6*NEQ],dtr,a;
    double    rad,radv,lat,latv,lng,lngv,angvel[3];

    dtr = acos(-1.)/180.;
    com[0] = com[1] = com[2] = 0.;

    fprintf(stderr,"static friction coefficient : "); scanf("%lf", &mu_static);
    fprintf(stderr,"dynamic friction coefficient: "); scanf("%lf", &mu_dynamic);
    fprintf(stderr,"coefficient of restitution  : "); scanf("%lf", &coef_rest);
    fprintf(stderr,"initial radial pos (m) and vel (m/s): ");
    scanf("%lf %lf", &rad, &radv);
    fprintf(stderr,"latitude pos (deg) and rate (rad/s): ");
    scanf("%lf %lf", &lat, &latv);
    fprintf(stderr,"longitude pos (deg) and rate (rad/s): ");
    scanf("%lf %lf", &lng, &lngv);
    fprintf(stderr,"ball ang. vel. vector in INNER frame (rad/s): ");
    scanf("%lf %lf %lf", &angvel[0], &angvel[1], &angvel[2]);

    printf("radii: inner=%g outer=%g\n", R_INNER, R_OUTER);
    printf("coef: static=%g dynamic=%g restitution=%g\n",
           mu_static,mu_dynamic,coef_rest);
    printf("ic's: radial=%g radv=%g\n",rad,radv);
    printf("    : latitude=%g latv=%g\n",lat,latv);
    printf("    : longitude=%g longv=%g\n",lng,lngv);
    printf("    : sphere angvel=%g %g %g\n\n", angvel[0],angvel[1],angvel[2]);

    sdinit();

    /* enable constraint stabilization */
    a = 10.;
    sdstab(2.*a,a*a);

    /* Set up the initial conditions. */
    t = 0.;
    y[sdindx(RADIUS,0)] = rad;                /* positions */
    y[sdindx(LATLONG,LATITUDE)] = lat*dtr;
    y[sdindx(LATLONG,LONGITUDE)] = lng*dtr;
    y[NQ+sdindx(RADIUS,0)] = radv;            /* velocities */
    y[NQ+sdindx(LATLONG,LATITUDE)] = latv;
    y[NQ+sdindx(LATLONG,LONGITUDE)] = lngv;

    /* Initialize ball coordinates using 1-2-3 Euler angles, then convert to 
       Euler parameters. */
    for (i=0; i<3; i++) {
        y[sdindx(INNER,i)] = 0.;
        y[NQ+sdindx(INNER,i)] = angvel[i];
    }
    sdang2st(y,y);

    /* Determine the initial system condition (free, slipping, rolling) */
    deriv(t,y,dy,param,&status);
    syscond = (int)param[0];

    /* Turn on the prescribed motion constraint if necessary. */
    sdpres(RADIUS,0, syscond != COND_FREE);

    /* Recompute the derivatives now that the condition is known. */
    deriv(t,y,dy,param,&status);

    printf(" cond     time%6s%8s%6s%6s%6s%8s%8s%8s%9s\n", "rad", "radv", 
           "com x", "com y", "com z", "angv 1", "angv 2", "angv 3", "energy");

    tfinal = t + NSTEP*DT;
    step = DT;
    for (i=0; i<NSTEP+1; i++) {
    condchg = 0;
    tout = t + DT;
    while (t < tout) {
        double comg[3], junk[3], ke, pe, mass, grav[3];

        /* calculate quantities to display */
        sdpos(INNER,com,comg);
        sdangvel(INNER,angvel);
        sdtrans(INNER,angvel,GROUND,angvel);
        sdmom(junk,junk,&ke);
        sdgetgrav(grav);
        sdgetmass(INNER,&mass);
        pe = -(R_OUTER+comg[1])*grav[1]*mass;

        printf(
          "%c%.4s%9.5f%6.2f%8.2f%6.2f%6.2f%6.2f%8.2f%8.2f%8.2f%9.3f\n",
        condchg ? '*' : ' ', condnames[syscond], t, 
        y[sdindx(RADIUS,0)], y[NQ+sdindx(RADIUS,0)],
        comg[0], comg[1], comg[2],
        angvel[0], angvel[1], angvel[2], ke+pe);

        if (t >= tfinal)
        break;
        condchg = 0;

        sdvinteg(deriv,&t,y,dy,param,tout-t,&step,NEQ,TOL,work,&err,&which);
        if (!err) continue;

        if (err == OVERBUMP) 
        printf("sdvinteg: went over a bump in function %d\n", which);
        else if (err == USERSTAT && which == WANTCONDCHG) {
        wantcond = param[0];

        if (syscond == COND_FREE)
            impact(t,y,imp);
        else {
            if (wantcond == COND_ROLLING)
            impact(t,y,imp);
            else if (wantcond == COND_SLIPPING)
            getslipdir(slipdir);
            syscond = wantcond;
        }
        condchg = 1;

        /* enable/disable prescribed motion as appropriate */
        sdpres(RADIUS,0, syscond != COND_FREE);
        deriv(t,y,dy,param,&status);
        } else {
        printf("sdvinteg: err=%d which=%d\n", err, which);
        exit(1);
        }
    }
    }

    sdprinterr(stdout);
}

/* This routine computes system derivatives, given the time and state
 * and current conditions (unknown,free,slipping,rolling).  Condition
 * transitions are detected here, and if seen cause a WANTCONDCHG
 * status to be returned, with the desired condition in param[0].
 *
 * This routine is normally called by the integrator.
 */
deriv(t,y,dy,param,status)
double t,y[NEQ],dy[NEQ],param[1];
int    *status;
{
    static double lastnormal = 0.;
    double prevnorm, frc[3], dir[3], radial, radialv;
    double cpt[3], cptvr[3], vmag;    /* contact point */
    double mults[NC], fmag, perr[NC], verr[NC];
    int multmap[NC], st, rank, routine, err, i, j;

    /* Set time and state.  If the integrator's guess produced an
     * illegal state, reject it immediately.
     */
    sdstate(t,y,&y[NQ]);
    sderror(&routine,&err);
    if (err)
    goto allDone;

    /* Apply prescribed motions, if any. */
    sdumotion(t,y,&y[NQ]);

    /* Check for constraint violation and reject this state if they 
     * exceed the allowable tolerance. 
     */
    sdperr(perr);
    sdverr(verr);
    for (i=0; i<NC; i++) {
      if (fabs(verr[i]) > CTOL) {
        for (j=0; j<NEQ; j++) dy[j] = 0.;
        /*printf("verr[%d]=%g at t=%lg\n",i,verr[i],t);*/
        *status = CONSVIOL;
            goto allDone;
      }
      if (fabs(perr[i]) > CTOL) {
        for (j=0; j<NEQ; j++) dy[j] = 0.;
        printf("perr[%d]=%g at t=%lg\n",i,perr[i],t);
        *status = CONSVIOL;
            goto allDone;
      }
    }

    /* If we make it this far, the integrator's trial state is 
     * legitimate.  Behavior after this depends on the current system
     * condition (unknown, free, slipping, or rolling).
     */
    switch (syscond) {

    case COND_UNKNOWN:
    case COND_FREE:
    sduforce(t,y,&y[NQ]);
    sdderiv(dy,&dy[NQ]);

    /* radial  is length of the radius from the outer sphere center to
     *         the furthest-out point of the inner sphere.
     * radialv is the rate at which radial is changing (negative means
     *         moving towards the center).
     */
    radial = fabs(y[sdindx(RADIUS,0)]) + R_INNER;
    radialv = SIGN(y[sdindx(RADIUS,0)]) * y[NQ+sdindx(RADIUS,0)];

    /* stay COND_FREE until we touch the wall while approaching */
    if (radial < R_OUTER || radialv < EPS_MINMEANINGFULVEL) {
        /* we're free */
        if (syscond == COND_UNKNOWN) {
        *status = WANTCONDCHG;
        param[0] = COND_FREE;
        }
    } else {
        /* we're on the wall -- first guess that we're rolling */
        *status = WANTCONDCHG;
        param[0] = COND_ROLLING;
    }
    goto allDone;

    case COND_ROLLING:
    sduforce(t,y,&y[NQ]);
    sdderiv(dy,&dy[NQ]);
    sdmult(mults,&rank,multmap);
    if (mults[MULT_NORMAL] != 0. &&
           SIGN(mults[MULT_NORMAL]) == SIGN(y[sdindx(RADIUS,0)])) 
    {
        *status = WANTCONDCHG;
        param[0] = COND_FREE;
    } else {
        fmag = sqrt(SQR(mults[MULT_SLIPX]) + SQR(mults[MULT_SLIPZ]));
        if (fmag > mu_static*fabs(mults[MULT_NORMAL])) {
            *status = WANTCONDCHG;
            param[0] = COND_SLIPPING;
        }
    }
    goto allDone;

    case COND_SLIPPING:
    /* if slipping, use the last known normal force as an estimate
       and correct if necessary. */

    contact(y, cpt, cptvr);
    vmag = sqrt(SQR(cptvr[0]) + SQR(cptvr[2]));

    /* in transition we'll use last known rolling constraint force 
       direction if slip velocity is too small to be a reliable direction */

    if (vmag < EPS_MINMEANINGFULVEL)
        for (i=0; i<3; i++) dir[i] = slipdir[i];
    else {
        dir[0] = cptvr[0]/vmag;
        dir[1] = 0.;
        dir[2] = cptvr[2]/vmag;
    }

    /* Use fixed-point iteration until consistent forces are achieved. 
     * In this problem, this never requires more than two iterations
     * since the normal and tangential forces are independent.
     */
    do {
        sdstate(t,y,&y[NQ]);
        sdumotion(t,y,&y[NQ]);
        sduforce(t,y,&y[NQ]);
        for (i=0; i<3; i++) frc[i] = -dir[i]*fabs(lastnormal)*mu_dynamic;
        sdtrans(RADIUS,frc,INNER,frc);
        sdpointf(INNER,cpt,frc);

        sdderiv(dy,&dy[NQ]);
        sdmult(mults,&rank,multmap);
        prevnorm = lastnormal;
        lastnormal = mults[MULT_NORMAL];
    } while (fabs(lastnormal-prevnorm) > EPS_FORCE);

    /* go from SLIPPING to FREE? */
    if (lastnormal != 0. && SIGN(lastnormal) == SIGN(y[sdindx(RADIUS,0)])) {
        *status = WANTCONDCHG;
        param[0] = COND_FREE;
        goto allDone;
    }

    /* go from SLIPPING to ROLLING? */
    if (vmag >= EPS_MINMEANINGFULVEL && vmag < EPS_MINSLIPVEL) {    
        double cptar[3], sgrowth;

        contactacc(cpt, cptar);
        sgrowth = cptar[0]*cptvr[0] + cptar[2]*cptvr[2];

        if (sgrowth < 0.) {
        *status = WANTCONDCHG;
        param[0] = COND_ROLLING;
        goto allDone;
        }
    }

    /* keep SLIPPING */
    }

  allDone:

    /* We're done with deriv().  If an error occurred, we'll assume that
     * means the integrator picked a bad time/state for this guess, and
     * we'll reject the guess.  This will make the integrator cut its
     * step size, hopefully producing a better guess next time.
     *
     * If the error is "singular mass matrix" we'll ignore it, however,
     * since in this case that's just an artifact of our use of massless
     * frames when in certain configurations and does not actually cause
     * problems.
     *
     * If we reject this guess, we'll clear out dy[] which may contain
     * garbage (e.g. NaNs) resulting from the bad state.
     */
    sderror(&routine,&err);
    if (err) {
        if (err != ERR_SINGULAR_MASS_MATRIX) {
        printf("deriv: bad state err=%d rou=%d\n",err,routine);
        for (j=0; j<NEQ; j++) dy[j] = 0.;
        *status = BADSTATE;
    }
    sdclearerr();
    }
}

/*
 * Compute velocities resulting from an impact.  This is used both on 
 * impact when inner sphere first contacts the outer sphere, and on
 * transition from slipping to rolling where the residual slipping velocity
 * must be converted to rolling.
 */

/* Where things go in the param[] array. */
#define TIME    0    
#define COEF    1    /* coefficient of restitution */
#define QS    2    
#define US    (QS+NQ)
#define NEWUS    (US+NU)
#define PARAMLEN (2+NQ+NU+NU)

/* root finder stuff */
#define NV    3        /* number of variables */
#define NF    3        /* number of functions */
#define SOLNTOL    EPS_FORCE     /* required solution tol */
#define MAXEVAL    500        /* maximum allowable number of resid calls */

impact(t,y,imp)
double t,y[NEQ],imp[3];
{
    int i,err,fcnt,lock[NV],iw[4*(NF+NV)];
    int secondtime = 0, newstate;
    double param[PARAMLEN], fret[NF], tmag, cpt[3], cptvr[3];
    double jw[NF*NV], dw[2*(NF+NV)*(NF+NV)], rw[9*(NF+NV)];
    extern resid();

    param[COEF] = (syscond == COND_FREE ? coef_rest : 0.);
    param[TIME] = t;
    for (i=0; i<NEQ; i++)
    param[QS+i] = y[i];

    /* initial guess for the impulse is 0 */
    imp[0] = imp[1] = imp[2] = 0.;
    lock[0] = lock[1] = lock[2] = 0;

  tryagain:

    sdroot(resid,imp,param,NV,NF,0,lock,SOLNTOL,0.,MAXEVAL,
       jw,dw,rw,iw,fret,&fcnt,&err);

    if (err) {
    printf("!!! root finder failed err=%d fcnt=%d\n", err, fcnt);
    printf("!!! best imp: %g %g %g\n", imp[0],imp[1],imp[2]);
    printf("!!! residual: %g %g %g\n", fret[0],fret[1],fret[2]);
    }

    /* Check for slipping by looking at magnitude of tangential impulse.
       If it's too big, adjust it down. */
    tmag = sqrt(SQR(imp[0]) + SQR(imp[2]));
    if (syscond == COND_FREE && tmag > mu_static*fabs(imp[1])) {
    /* a slipping impact */
    imp[0] = (imp[0]/tmag)*mu_dynamic*fabs(imp[1]);
    imp[2] = (imp[2]/tmag)*mu_dynamic*fabs(imp[1]);
    newstate = COND_SLIPPING;
    } else {
    /* a rolling impact */
    newstate = COND_ROLLING;
    }

    /* Compute final u's. */
    resid(imp,param,fret);
    for (i=0; i<NU; i++)
    y[NQ+i] = param[NEWUS+i];

    if (syscond != COND_FREE) {
    syscond = newstate;
    return;
    }

    sdstate(t,y,&y[NQ]);
    contact(y, cpt, cptvr);

    if (!secondtime) {
    /* See if we're rebounding too slowly. */
    if (fabs(cptvr[1]) < EPS_MINREBOUNDVEL) {
        /* solve the problem again with 0 coefficient of restitution */
        secondtime = 1;
        param[COEF] = 0.;
        goto tryagain;
    }
    } else {
    /* We're now either rolling or sliding along the surface. */
    syscond = newstate;
    }
}

/* This routine takes a proposed impulse, and returns the residual error
 * in the resulting velocities.  
 *
 * This routine is normally called by the root finder, which responds by 
 * attempting to adjust the impulse until the error goes to zero.
 */
resid(impr,param,fret) 
double impr[3],param[PARAMLEN],fret[3];
{
    int i;
    double qdot[NQ], udot1[NU], udot2[NU], imp[3];
    double cpt[3], cptvr[3], nvel;

    sdstate(param[TIME],&param[QS],&param[US]);
    sduforce(param[TIME],&param[QS],&param[US]);
    sdderiv(qdot,udot1);

    sdstate(param[TIME],&param[QS],&param[US]);
    sduforce(param[TIME],&param[QS],&param[US]);
    contact(&param[QS],cpt,cptvr);
    sdtrans(RADIUS,impr,INNER,imp);
    sdpointf(INNER,cpt,imp);
    sdderiv(qdot,udot2);

    nvel = cptvr[1];

    for (i=0; i<NU; i++)
    param[NEWUS+i] = param[US+i] + (udot2[i] - udot1[i]);

    sdstate(param[TIME],&param[QS],&param[NEWUS]);
    contact(&param[QS],cpt,cptvr);

    fret[0] = cptvr[0];
    fret[1] = cptvr[1] + param[COEF]*nvel;
    fret[2] = cptvr[2];
}

/* These routines define the two user constraints. */

/* There are no position errors since a "no-slip" constraint is nonholonomic. */
sduperr(t,q,errs)
double t, q[NQ], errs[NC];
{
    errs[CSLIPX] = 0.;
    errs[CSLIPZ] = 0.;
}

/* The relative velocity at the contact point should be zero. */
sduverr(t,q,u,errs)
double t, q[NQ], u[NU], errs[NC];
{
    double cpt[3],cptvr[3];

    if (syscond == COND_ROLLING) {
    contact(q,cpt,cptvr);
    errs[CSLIPX] = cptvr[0];
    errs[CSLIPZ] = cptvr[2];
    } else {
    errs[CSLIPX] = 0.;
    errs[CSLIPZ] = 0.;
    }
}

/* This is the derivative of the velocity constraint above. */
sduaerr(t,q,u,udot,errs)
double t, q[NQ], u[NU], udot[NU], errs[NC];
{
    double cpt[3],cptvr[3],cptar[3],angvr[3],angvi[3];

    if (syscond == COND_ROLLING) {
    contact(q, cpt, cptvr);
    contactacc(cpt, cptar);

    sdangvel(RADIUS,angvr);
    sdangvel(INNER, angvi);
    sdtrans (INNER, angvi, RADIUS,angvi);

    errs[CSLIPX] = cptar[0] + cptvr[1]*angvr[2] - cptvr[2]*angvr[1];
    errs[CSLIPZ] = cptar[2] + cptvr[0]*angvr[1] - cptvr[1]*angvr[0];
    } else {
    errs[CSLIPX] = 0.;
    errs[CSLIPZ] = 0.;
    }
}

/* Interpret the multipliers as forces and apply them. */
sduconsfrc(t,q,u,mults)
double t, q[NQ], u[NU], mults[NUSERC];
{
    double cpt[3], cptvr[3], frc[3];

    contact(q,cpt,cptvr);
    frc[0] = syscond == COND_ROLLING ? mults[CSLIPX]  : 0.;
    frc[1] = 0.;
    frc[2] = syscond == COND_ROLLING ? mults[CSLIPZ]  : 0.;
    sdtrans(RADIUS,frc,INNER,frc);
    sdpointf(INNER,cpt,frc);
}

/* If there were any applied forces besides gravity, they would be
 * applied here. 
 */
sduforce(t,q,u)
double t,q[NQ], u[NU];
{
}

/* Calculate prescribed motion constraint.  This is ignored if the
 * constraint is not enabled.  We prescribe the inner sphere to
 * be just in contact with the outer sphere, with no radial velocity
 * or acceleration.  (Caution: with our choice of coordinates, the
 * "radius" may be either positive or negative.)
 */
sdumotion(t,q,u)
double t,q[NQ], u[NU];
{
    sdprespos(RADIUS,0,SIGN(q[sdindx(RADIUS,0)])*(R_OUTER-R_INNER));
    sdpresvel(RADIUS,0,0.);
    sdpresacc(RADIUS,0,0.);
}

/* Utility routines */

/* 
 * Calculate the direction of slip by looking at the "no-slip" multipliers 
 * just before we transit from ROLLING to SLIPPING.  The initial slip 
 * direction must be opposite to the direction of the "no-slip" force.
 */
getslipdir(dir)
double dir[3];
{
    double mults[NC], fmag;
    int    rank, multmap[NC];

    sdmult(mults,&rank,multmap);
    fmag = sqrt(SQR(mults[MULT_SLIPX])+ SQR(mults[MULT_SLIPZ]));

    dir[0] = -mults[MULT_SLIPX]/fmag;
    dir[1] = 0.;
    dir[2] = -mults[MULT_SLIPZ]/fmag;
}

/*
 * Compute location and velocity of the contact point.  Location is
 * given relative to inner sphere COM in INNER frame.  Velocity
 * is given relative to ground frame and expressed in RADIUS frame.
 */
contact(q, pt, velr)
double q[NQ], pt[3], velr[3];
{ 
    double locr[3];

    locr[0] = locr[2] = 0.;
    locr[1] = SIGN(q[sdindx(RADIUS,0)])*R_INNER;

    sdtrans(RADIUS,locr,INNER,pt);
    sdvel(INNER,pt,velr);
    sdtrans(GROUND,velr,RADIUS,velr);
}

/*
 * Compute the acceleration of the contact point.  Note: this is not
 * the same as the acceleration of the physical point of the inner
 * sphere which is currently in contact with the outer sphere.
 * The contact point is expected as input in the INNER frame (see
 * routine contact() above).  Output is the acceleration in the
 * RADIUS frame.
 */
contactacc(pt, accr)
double pt[3], accr[3];
{
    double angvr[3], angvi[3];

    sdacc   (INNER, pt,   accr);
    sdtrans (GROUND,accr, RADIUS,accr);
    sdangvel(RADIUS,angvr);
    sdangvel(INNER, angvi);
    sdtrans (INNER, angvi, RADIUS,angvi);
    accr[0] += angvi[1]*(angvr[0] - angvi[0]);
    accr[2] += angvi[1]*(angvr[2] - angvi[2]);
}
