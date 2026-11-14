/* Interface for the nested-spheres example.
 *
 * Prototypes match the generated model and sdlib. The user routines at
 * the bottom are called by that generated code.
 */

#ifndef SPHERE_H
#define SPHERE_H

#include <stdio.h>

void sdinit(void);
void sdstab(double velin, double posin);
int sdindx(int joint, int axis);
void sdang2st(double stang[], double st[]);
void sdstate(double timein, double qin[], double uin[]);
void sderror(int* routine, int* errnum);
void sdclearerr(void);
void sdpres(int joint, int axis, int presin);
void sdprespos(int joint, int axis, double prval);
void sdpresvel(int joint, int axis, double prval);
void sdpresacc(int joint, int axis, double prval);
void sdpos(int body, double pt[], double loc[]);
void sdvel(int body, double pt[], double velo[]);
void sdacc(int body, double pt[], double accel[]);
void sdangvel(int body, double avel[]);
void sdtrans(int frbod, double ivec[], int tobod, double ovec[]);
void sdmom(double lm[], double am[], double* ke);
void sdgetgrav(double gravout[]);
void sdgetmass(int body, double* massout);
void sdprinterr(FILE* fnum);
void sdperr(double errs[]);
void sdverr(double errs[]);
void sdderiv(double oqdot[], double oudot[]);
void sdmult(double omults[], int* owrnk, int omultmap[]);
void sdpointf(int body, double point[], double force[]);
void sdvinteg(int (*func)(double, double[], double[], double[], int*),
    double* time, double st[], double dst[], double param[],
    double dt, double* step, int neqin, double tol, double work[],
    int* err, int* which);
void sdroot(int (*func)(double[], double[], double[]),
    double vars[], double param[], int nfunc, int nvar, int ndesin,
    int lock[], double rtol, double dtol, int maxeval,
    double jw[], double dw[], double rw[], int iw[],
    double fret[], int* fcnt, int* err);

void sduperr(double t, double q[], double errs[]);
void sduverr(double t, double q[], double u[], double errs[]);
void sduaerr(double t, double q[], double u[], double udot[], double errs[]);
void sduconsfrc(double t, double q[], double u[], double mults[]);
void sduforce(double t, double q[], double u[]);
void sdumotion(double t, double q[], double u[]);

#endif
