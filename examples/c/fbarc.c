#include <stdio.h>
#include <math.h>

/* Body numbers (from the fbar_info file). */
#define BODY_GROUND	(-1)
#define BODY_CRANK	0
#define BODY_ROCKER	1
#define BODY_CONNECT	2

/* Joint numbers.  Note that tree joints have the
 * same number as their outboard body.
 */
#define JOINT_GND_CRANK		BODY_CRANK
#define JOINT_GND_ROCKER	BODY_ROCKER
#define JOINT_ROCKER_CONNECT	BODY_CONNECT
#define JOINT_CRANK_CONNECT	3		/* loop joint */
#define NJOINTS	4

/* State variables.  The state variable corresponding to
 * a particular joint axis can be obtained from the 
 * fbar_info file or by using the sdindx() routine.
 */
#define NQ 3
#define NU 3
#define NSTATE (NQ+NU)
#define ST_CRANK	0
#define ST_ROCKER	1
#define ST_CONNECT	2

#define CTOL		1e-5	/* constraint tolerance */
#define TOL		1e-6	/* integration tolerance */
#define STEP_SIZE	0.05	/* seconds */
#define NSTEPS		200


int
main()
{
    double	t, state[NSTATE], dstate[NSTATE],
		frc[NJOINTS][3], trq[NJOINTS][3];
    int		lock[NU], flag, err, fcnt, i;

    sdinit(); /* initialize sd/fast model */

    /* Set initial conditions to a starting guess. */
    t = 0.;
    for (i=0; i<NQ; i++) state[i] = 0.;	/* positions */
    state[NQ+ST_CRANK]   = 10.;		/* crank velocity */
    state[NQ+ST_ROCKER]  = 1.;		/* rocker velocity */
    state[NQ+ST_CONNECT] = -1.;		/* rocker-connect joint velocity */

    /* Do assembly and velocity analysis.  Only q[0] and u[0] are locked. */
    lock[ST_CRANK] = 1; lock[ST_ROCKER]=lock[ST_CONNECT] = 0;
    sdassemble(t,state,lock,0.1*CTOL,1000,&fcnt,&err);
    sdinitvel (t,state,lock,0.1*CTOL,1000,&fcnt,&err);
    printf("Initial state:");
    for (i=0; i<NSTATE; i++) printf(" %g", state[i]);
    printf("\n");

    /* Do motion analysis.  Constraints maintained to CTOL, integration to TOL. */

    sdstab(2.,1.);	/* turn on constraint stabilization */
    flag = 1;

    printf("%6s %14s %14s %14s %14s\n", 
	"time", "crank pos", "crank vel", "reaction frc x", "reaction frc y");

    for (i=0; i<NSTEPS; i++) {
	sdmotion(&t,state,dstate,STEP_SIZE,CTOL,TOL,&flag,&err);
	if (err) printf("*** at t=%g got err=%d\n", t, err);
	sdreac(frc,trq);
	printf("%6g %14g %14g %14g %14g\n",
	    t, state[ST_CRANK], state[NQ+ST_CRANK], 
	    frc[JOINT_GND_CRANK][0], frc[JOINT_GND_CRANK][1]);
    }

    sdprinterr(stderr);
	
    return 0;
}

/* 
 * Apply viscous damping torque to the crank joint.
 */
void
sduforce(double t, double *q, double *u)
{
    sdhinget(JOINT_GND_CRANK, 0, -3.*u[ST_CRANK]);
}
