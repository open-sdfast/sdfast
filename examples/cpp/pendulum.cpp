//
//  pendulum.cpp  --  C++ version of SD/FAST PEND.FOR
//
//	John Nagle	May, 1996
//
//  This is a set of analyses performed on the pendulum example in
//  Tutorial 2 of the SD/FAST User's Manual, version B.1.0.
//  See pend.sd for a description of the pendulum.
//
//
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "sdfastdefs.h"
#define SQR(x) ((x)*(x))
static const int maxevals = 1000;
static const double tol = 1.0e-4;
static const double ctol = 0.0;
//
//	Body and joint numbers from SD/FAST information file.
//	These can all be obtained from the input file, without running SD/FAST.
//
const groundbodynum = -1;					// body number of ground
const pendbodynum = 0;						// body number of pendulum
const pendpinjoint = 0;						// joint number of pin
const pendpinaxis = 0;						// axis number
//
//	Communication with our own routines
static double bias;
static sdvector3 fvec;						// 3D vector
//
//	printvector  --  print a single vector of 3 elts.
//
static void printvector(FILE* outfile, sdvector3 tab)
{
	for (int j=0; j<3; j++)
	{	fprintf(outfile," %18.10lf",tab[j]);	}
	fprintf(outfile,"\n");
}
//
//	pause -- like FORTRAN pause
//
static void pause(char msg[])
{	printf("%s\n",msg);
	printf("Press any key to continue..");
	fgetc(stdin);
}
//	
//	getsdinfo   --  get sizing info from SD/FAST model
//
//	Ref. section R17 of SD/FAST manual
//
static void getsdinfo(int &nq, int &nu, int &njnt)
{	int info[50];						// 50 per SD/FAST manual
	sdinfo(info);						// retrieve info
	int nbod = info[kInfoNBOD];			// number of bodies
	int ndof = info[kInfoNDOF];			// number of degress of freedom
	int nc = info[kInfoNC];				// number of constraints
	int nloop = info[kInfoNLOOP];		// number of loop joints
	int nb = info[kInfoNB];				// number of 6DOF/ball joints in tree 
	nq = ndof+nb;						//  
	nu = ndof;
	njnt = nbod+nloop;					// number of joint
}
//
//	main
//
int main()
{
	//	Allocate arrays based on model-dependent info
	int nq,nu,njnt;								// sizing info
	getsdinfo(nq,nu,njnt);						// get sizing info
	double* y = new double[nq+nu];				// sim output
	double* dy = new double[nq+nu];				// derivs of sim outputs
	assert(y != NULL);
	assert(dy != NULL);
	int* lock = new int[nu];					// lock-joints flag
	assert(lock != NULL);
	y[sdindx(pendpinjoint,pendpinaxis)] = 0.0;	// joint position to 0
	y[nq+sdindx(pendpinjoint,pendpinaxis)] = 0.0;// joint speed 0
	lock[pendpinjoint] = 0;						// this joint is not locked
	const pendpinindx = sdindx(pendpinjoint,pendpinaxis);  // look up index
	//	Other declarations
	sdvector3 acc;								// acceleration vector
	sdvector3 comass = {0.0,0.0,0.0};			// center of mass point
	int		flag,nstep;
	int		fcnt;
    int 	err;								// error code, if any
    double	t;									// simulation time

//  Initialize all SD subroutines before making any calls to analysis routines.
	sdinit();

//	EXERCISE #1: STATIC ANALYSIS FOR 10kg PENDULUM
//  Perform static analysis for mass = 10 kg (default) and spring bias as shown.
	bias = 0.3;
	sdstatic(t,y,lock,ctol,tol,maxevals,&fcnt,&err);
	printf("Exercise 1: Static equilibrium for mass = 10 kg\n\n");
	printf("Y = %lf  FCNT = %d   ERR = %d\n",y[0],fcnt,err);
	printf("DONE WITH EXERCISE #1\n");

//	EXERCISE #2: PERTURBATION ANALYSIS TO VERIFY STABLE EQUILIBRIUM
//	Perturb solution slightly and run a motion analysis at static configuration 
//	to verify no unstable motion occurs.
//	Note that FLAG = 1 must be set each time SDMOTION is first called or
//	the state is changed discontinuously, such as after an impulsive load.
	flag = 1;
	nstep = 10;
	y[pendpinindx] += 0.0001;	// peturb
	printf("\nExercise 2: Perturbation Analysis\n");
	printf("\nCheck subsequent motion to verify static condition:\n");
	printf(
     "      Time (sec)             Angle(rad)           Rate(rad/sec)\n");
	double dt = 0.05;
	for (int iloop = 1; iloop<= nstep; iloop++)
	{
		sdmotion(&t,y,dy,dt,ctol,tol,&flag,&err);
		printf(" %18.10lf     %18.10lf     %18.10lf\n",t,
			y[pendpinindx],
			y[nq+pendpinindx]);
	}
	pause("DONE WITH EXERCISE #2.");

//  EXERCISE #3: STATIC ANALYSIS FOR 5kg PENDULUM
//  Redo static analysis for mass set to 5 kg, bias unchanged.
//  Note that all velocities, Y(2), must be set to zero for SDSTATIC() to work
	sdmass(pendbodynum,5.0);			// set mass of body 0 to 0.5Kg
	sdinit();							// re-init SD/FAST
	y[pendpinindx] = 0.0;	// joint to neutral
	sdstatic(t,y,lock,ctol,tol,maxevals,&fcnt,&err);
	printf("Exercise 3: Static equilibrium for mass = 5 kg\n\n");
	printf("Y = %lf  FCNT = %d   ERR = %d\n",y[pendpinindx],fcnt,err);
	pause("DONE WITH EXERCISE #3.");

//  EXERCISE #4: CHECK F=MA
//  Set spring bias to zero to allow the pendulum to fall.
//  At the instant of release, check that F = ma for all three axes.
//  Note that setting DT = 0 causes the derivatives to be evaluated, but 
//  not propagated.
	bias = 0.0;
	dt = 0.0;
	flag = 1;							// ***TEMP***
	sdmotion(&t,y,dy,dt,ctol,tol,&flag,&err);
//  Get reaction forces and torques acting on the pendulum
	sdvector3* force = new sdvector3[njnt];			// allocate force array, 
	assert(force != NULL);
	sdvector3* torque = new sdvector3[njnt];
	assert(torque != NULL);
	sdreac(force,torque);
	printf("\nExercise 4: Check F=ma on Released Pendulum\n");
	printf("   Force = ");	
	printvector(stdout,force[pendpinindx]);
	printf("  Torque = ");
	printvector(stdout,torque[pendpinindx]);		
	printf("\n");
//  Transform the force vector from the pendulum frame to the inertial frame
	sdtrans(pendbodynum,force[pendpinindx],groundbodynum,force[pendpinindx]);
//	Find the acceleration of the center of mass of the pendulum
	sdacc(pendbodynum,comass,acc);
//  Transform the external force vector from SDUFORCE to inertial frame
	sdtrans(pendbodynum,fvec,groundbodynum,fvec);
//	Get the values of the pendulum mass and gravity for F = ma eqn.
	double mass0;							// mass of body 0
	sdvector3 grav;							// gravity vector
	sdgetmass(pendbodynum,&mass0);			// get mass from model
	sdgetgrav(grav);						// get grevity vector from model
	printf("Set torsion spring bias to zero\n");
	printf("Compute external forces, accelerations, and position\n");
	printf("at the time of release:\n");
	printf("       Force = ");
	printvector(stdout,force[pendpinindx]);
	printf("Acceleration = ");
	printvector(stdout,acc);
	printf("\nCheck if sum of external forces equals mass*acceleration:\n");
	printf("(along three inertial unit vectors, n1, n2, and n3)\n");
	printf("               F        =     m       a       ?\n");
	printf(" n1: %18.10lf = %18.10lf\n",force[pendpinindx][0]+mass0*grav[0]+fvec[0],mass0*acc[0]);
	printf(" n2: %18.10lf = %18.10lf\n",force[pendpinindx][1]+mass0*grav[1]+fvec[1],mass0*acc[1]);
	printf(" n3: %18.10lf = %18.10lf\n",force[pendpinindx][2]+mass0*grav[2]+fvec[2],mass0*acc[2]);
	pause("DONE WITH EXERCISE #4");

//  EXERCISE #5: MOTION ANALYSIS OF RELEASED PENDULUM
//  Set DT = 0.05 to allow motion to propagate for a while
	printf("\nExercise 5: Motion Analysis After Release\n");
	printf("      Time (sec)             Angle(rad)           Rate(rad/sec)\n");
	dt = 0.05;								// motion interval
	nstep = 200;							// number of steps
	////flag = 0;								// don't reset integrators
	//	Do the simulation
	for (iloop = 0; iloop<nstep; iloop++)
	{	sdmotion(&t,y,dy,dt,ctol,tol,&flag,&err);	// does all the work
		printf("%6.3lf ",t);
		for (int i=0; i<nq+nu; i++)					// show all sim outputs
		{	printf("  %18.10lf",y[i]); }
		printf("\n");
	}
	printf("DONE WITH EXERCISE #5\n");

//  EXERCISE #6: STATIC ANALYSIS OF FINAL RESTING POSITION
//  Redo static analysis for spring bias set to 0.
//  Long term motion of damped pendulum should lead to hanging vertically.
//	Note that all velocities, Y(2), must be set to zero.
	y[1] = 0.0;
	sdstatic(t,y,lock,ctol,tol,maxevals,&fcnt,&err);
	printf("Exercise 6: Static equilibrium for bias = 0\n");
	printf(" Y = %lg   FCNT = %d   ERR = %d\n",y[0],fcnt,err);
	printf("DONE WITH EXERCISE #6\n");
//  END OF EXERCISES

//  ALWAYS include a call to SDPRINTERR() at the end of your program
//  to report any problems found by SD/FAST.
	sdprinterr(stdout);
	return(0);
}
//
//	sduforce  --  user code applies forces and torques here
//
//	C linkage
//
void sduforce(double t, double q[], double u[])
{
//  Set up variables, common block to PEND, and all known parameters.
	//	Get index for the state variable for the one axis of the pendulum pin joint
	const pendpinindx = sdindx(pendpinjoint,pendpinaxis);  // look up index
	assert(pendpinindx == 0);					// ***TEMP***
	sdvector3 pntp = {0.0,0.5,0.0};				// spring/pendulum attach point
	sdvector3 gndpnt = {-1.0,-1.0,0.0};			// spring/ground attach point
	const double FFRICT = 10.0;					// rotational friction
	const double KROTATE = 100.0;				// rotational spring constant
	const double KLINEAR = 10.0;				// spring constant
	const double FREELENGTH = 1.0;				// zero length of spring
//  Compute spring and damper hinge torque TAU and apply to axis 1 of joint 1.
	double tau = -FFRICT*u[pendpinindx] - KROTATE*(q[pendpinindx]-bias);
	////printf("sduforce:  t= %lg tau = %  lg  q[0]=%lg bias=%lg\n",t,tau,q[0],bias);	// ***TEMP***
	sdhinget(pendpinjoint,pendpinaxis,tau);
	sdvector3 posp;
//  Compute inertial location of linear spring attach point pntp, put into posp
	sdpos(pendbodynum,pntp,posp);
//  Find a unit vector in the direction of the spring force
	sdvector3 vvec;
	vvec[0] = gndpnt[0] - posp[0];
	vvec[1] = gndpnt[1] - posp[1];
	vvec[2] = gndpnt[2] - posp[2];
	double mag = sqrt( SQR(vvec[0]) + SQR(vvec[1]) + SQR(vvec[2]));
	if ( mag > 1.0e-15 ) 
	{
		vvec[0] = vvec[0] / mag;
		vvec[1] = vvec[1] / mag;
		vvec[2] = vvec[2] / mag;
	} else {
		printf(" Error in SDUFORCE: Zero length unit vector\n");
		abort();
	}
//  Find the stretch of the spring beyond its free length
	double stretch = mag - FREELENGTH;
//  Find the resultant components of the linear spring force
	fvec[0] = KLINEAR * stretch * vvec[0];
	fvec[1] = KLINEAR * stretch * vvec[1];
	fvec[2] = KLINEAR * stretch * vvec[2];
//  Transform the spring force back to the pendulum frame
	sdtrans(groundbodynum,fvec,pendbodynum,fvec);		// note C body numbers
//  Apply the spring force as a point force acting at point P, PNTP, on pendulum.
	sdpointf(pendbodynum,pntp,fvec);

}
