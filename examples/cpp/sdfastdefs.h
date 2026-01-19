//
//	SD/Fast headers
//
//	John Nagle		May, 1996
//
#ifndef SDFASTDEFS					// "Pragma once" would be better
#define SDFASTDEFS
extern "C" {
typedef double sdvector3[3];			// 3D vector
//	SD/Fast routines
extern int sdinit();
extern int sdprinterr(FILE*);
extern int sdstatic(double time,double state[],int lock[],double ctol,double tol,
	int maxevals,int* fcnt,int *err);
extern int sdpointf(int body,sdvector3 point,sdvector3 force);
extern int sdtrans(int frbod,sdvector3 ivec,int tobod,sdvector3 ovec);
extern int sdhinget(int joint,int axis,double torque);
extern int sdpos(int body,sdvector3 pt,sdvector3 loc);
extern int sdmotion(double* time,double state[],double dstate[],
	double dt,double ctol,double tol,int* flag,int* err);
extern int sdmass(int body,double massin);
extern int sdreac(sdvector3 force[],sdvector3 torque[]);
extern int sdacc(int body,sdvector3 pt, sdvector3 accel);
extern int sdgetgrav(sdvector3 gravout);
extern int sdgetmass(int body, double *massout);
extern int sdinfo(int info[50]);						// get info
extern int sdindx(int joint, int axis);
typedef enum {						// indices into sdinfo reply array
	kInfoGROUNDED = 0,				// 1 if system is grounded, 0 if free flying
	kInfoNBOD = 1,					// number of bodies (and tree joints)
	kInfoNDOF = 2,					// number of tree hinge DOFs
	kInfoNC = 3,					// number of constraints
	kInfoNLOOP = 4,					// number of loop joints
	kInfoNLDOF = 5,					// number of loop hinge DOFs
	kInfoNLC = 6,					// number of loop constraints (ign. pres)
	kInfoNB = 7,					// number of 6DOF and ball joints in tree
	kInfoNLB = 8,					// number of 6DOF and ball joints in loop
	kInfoNPRESC = 9,				// number of prescribed motion constraints
	kInfoNUSERC = 10,				// number of user constraints
	kInfoRANK = 11 }				// number of constraints not obv. redundant.
	kSDInfo;
extern void sduforce(double t, double q[], double u[]);	// user provides this
}
#endif // SDFASTDEFS
