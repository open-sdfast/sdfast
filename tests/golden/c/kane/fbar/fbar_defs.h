#ifndef SDFAST_SD_FBAR_DEFS_H
#define SDFAST_SD_FBAR_DEFS_H
/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
Public declarations for this model. This is the header to include
from user code. Single-precision (-s) prototypes use float. A K&R
definition with a float parameter is not ABI-compatible with that
prototype, because a K&R float argument is promoted to double.
*/
#include <stdio.h>
#ifdef __cplusplus
extern "C" {
#endif

/* Model sizes. These are integer constant expressions, so user
 * code can use them as array bounds. Add further counts here.
 *
 * The macro prefix is the uppercased -p prefix (SD_NQ, or XX_NQ
 * with -p xx), not the input basename. Two models in one program
 * already need distinct prefixes: the generated routines would
 * otherwise clash, and so would these macros.
 *
 * NU is the number of rate coordinates (the sdinfo "nu" line),
 * not the number of user constraints (NUSERC, sdinfo "nuserc").
 */
#define SD_NBOD 3
#define SD_NJNT 4
#define SD_NDOF 3
#define SD_NLOOP 1
#define SD_NLDOF 1
#define SD_NBALL 0
#define SD_NLBALL 0
#define SD_NQ 3
#define SD_NU 3
#define SD_NLQ 1
#define SD_NLU 1
#define SD_NC 5
#define SD_NLC 5
#define SD_NXC 0
#define SD_NPRESC 0
#define SD_NUSERC 0

void sdprerrmsg(FILE *fnum,
    int routine,
    int errnum)
;

void sderror(int *routine,
    int *errnum)
;

void sdprinterr(FILE *fnum)
;

void sdclearerr(void)
;

void sdseterr(int routine,
    int errnum)
;

/*
===========================================================
LDU decomposition scheme for solving Mx=b
  M: na X na symmetric, pos. definite, upper rt triangle filled
  b: na X 1
  x: na X 1 (returned)
Actual dimensions (n) may be larger, map says where to find
the na interesting elements.
===========================================================
*/

void sdldudcomp(int n,
    int na,
    int map[],
    double tol,
    double ld[],
    double sum[],
    double m[],
    double l[],
    double d[])
;

void sdldubsl(int n,
    int na,
    int map[],
    double l[],
    double b[],
    double x[])
;

void sdldubsd(int n,
    int na,
    int map[],
    double d[],
    double b[],
    double x[])
;

void sdldubsu(int n,
    int na,
    int map[],
    double l[],
    double b[],
    double x[])
;

void sdldubslv(int n,
    int na,
    int map[],
    double work[],
    double l[],
    double d[],
    double b[],
    double x[])
;

void sdlduslv(int n,
    int na,
    int map[],
    double tol,
    double work1[],
    double work2[],
    double m[],
    double b[],
    double l[],
    double d[],
    double x[])
;

/*
===========================================================
QR decomposition scheme for solving Wx=b
  W is nra by nca
  b is nra by 1
  x (returned) is nca by 1
Actual dimensions (nr,nc) may be larger, mapr (mapc) says where
to find the nra (nca) interesting rows (columns).  On return, W is
overwritten by the Q and R matrices in compact form.  Solution yields
least squares residual for overdetermined systems.  Underdetermined
systems give a solution in which elements corresponding to redundant
or missing equations are set to 0. (Not necessarily the LS solution.)
===========================================================
*/

void sdqrdcomp(int nr,
    int nc,
    int nra,
    int nca,
    int mapr[],
    int mapc[],
    double w[],
    double qraux[],
    int jpvt[])
;

void sdqrsl(int nr,
    int nc,
    int nra,
    int nca,
    int mapr[],
    int mapc[],
    int k,
    double work[],
    double w[],
    double qraux[],
    double b[],
    double x[])
;

void sdqrbslv(int nr,
    int nc,
    int nra,
    int nca,
    int mapr[],
    int mapc[],
    double tol,
    double work[],
    int iwork[],
    double w[],
    double qraux[],
    int jpvt[],
    double b[],
    double x[],
    int *rank)
;

void sdqrslv(int nr,
    int nc,
    int nra,
    int nca,
    int mapr[],
    int mapc[],
    double tol,
    int jpvt[],
    double qraux[],
    double work[],
    int iwork[],
    double w[],
    double b[],
    double x[],
    int *rank)
;

/*
===========================================================
Linear equation solver for Wx=b
  W is nra by nca
  b is nra by 1
  x (returned) is nca by 1
Actual dimensions (nr,nc) may be larger, mapr (mapc) says where
to find the nra (nca) interesting rows (columns).  On return, W and
b are unchanged.  Solution yields least squares residual for
overdetermined systems and a least squares solution vector for
underdetermined systems.  If ndes > 0, the first nra-ndes rows
are `required' while the remaining ndes rows are `desired'.  The
returned solution minimizes the residual for the required rows,
then does the best it can on the desired rows.

The performance and numerical properties of this routine are
considerably worse than those of the qrslv routine.  However, it
works very well as a guide to the root finding routine.

Work arrays should be dimensioned as follows:
  dw is 2*(nra+nca)**2
  rw is 4*(nra+nca)
  iw is 3*(nra+nca)
===========================================================
*/

void sdlsslv(int nr,
    int nc,
    int nra,
    int nca,
    int ndes,
    int mapr[],
    int mapc[],
    double tol,
    double dw[],
    double rw[],
    int iw[],
    double w[],
    double b[],
    double x[])
;

/* Utility routines for use with sdroot. */


void sdcalcerrs(double fval[],
    int nfunc,
    int ndes,
    int dnorm,
    double *maxderr,
    double *maxrerr,
    double *derrnorm)
;

void sdadjvars(void (*func)(double [], double [], double []),
    double vars[],
    double param[],
    int nfunc,
    int ndes,
    int dnorm,
    int nvar,
    double deltas[],
    double step,
    double rerr,
    double tderr,
    double rtol,
    int *fcnt,
    double newvars[],
    double newerrs[])
;

void sdcalcjac(void (*func)(double [], double [], double []),
    double vars[],
    double param[],
    int nfunc,
    int nvar,
    int lock[],
    double delta,
    double fval[],
    double ftmp[],
    double jw[],
    int *fcnt,
    double scale[])
;

/*
====================================================================
Root-finding scheme for solving a set of nfunc=nreq+ndes nonlinear
equations in nvar unknowns:
  r (v) = r (v) = ... = r (v) = 0    (actually |r | <= rtol)
   1       2             nreq                    i

  d (v) = d (v) = ... = d (v) = 0    (actually |d | <= dtol)
   1       2             ndes                    i
The r's are "required" functions while the d's are only "desired".
That is, we adjust the nvar variables v to find a solution which keeps
each of the r's below rtol, and keeps the d's as small as possible
without violating any of the r's.  Least squares residuals are attempted
if the equations cannot be solved.  No guarantees can be made that
this routine will find a solution even if there is one.  The better
the initial values for the v's, the more likely it is to succeed and
the more quickly it will converge.

A single function func is provided which calculates all of the
r's and d's for the current v and returns the residuals.  A parameter
param is passed through unchanged to the function. 

The array lock has an element corresponding to each variable.  Any
variable which may be modified should have its corresponding lock
element set to 0.  If non-zero, the variable will not be altered here.

Maxeval sets an upper limit on the number of calls to func which may
be made.  The root finder only returns after complete iterations, so
it may make somewhat more than maxeval calls.  On return, the actual
number of calls made is returned in fcnt.  Err is returned 0 if we
successfully reduce all the r's to below rtol and all the d's to below
dtol.  If the r's are met but not the d's, we return with err=1, and if
the r's are not met we return err=2.

Work arrays should be dimensioned as follows:
  jw is nf*nv
  dw is 2*(nf+nv)**2
  rw is 7*nf+9*nv
  iw is 4*(nf+nv)
====================================================================
*/

void sdroot(void (*func)(double [], double [], double []),
    double vars[],
    double param[],
    int nfunc,
    int nvar,
    int ndesin,
    int lock[],
    double rtol,
    double dtol,
    int maxeval,
    double jw[],
    double dw[],
    double rw[],
    int iw[],
    double fret[],
    int *fcnt,
    int *err)
;

/* Utility routine for use with sdfinteg and sdvinteg.  Work is 2*neq. */


void sdrk4m(void (*func)(double , double [], double [], double [], int*),
    double time,
    double st[],
    double dst0[],
    double param[],
    double step,
    double nst[],
    int neq,
    double work[],
    double errs[],
    double *maxerr,
    int *which)
;

/* A fixed-step integrator.  Work should be dimensioned 4*neq. */


void sdfinteg(void (*func)(double , double [], double [], double [], int*),
    double *time,
    double st[],
    double dst[],
    double param[],
    double step,
    int neq,
    double work[],
    double *errest,
    int *status)
;

/* A variable-step integrator.  Work should be dimensioned 6*neq. */

void sdvinteg(void (*func)(double , double [], double [], double [], int*),
    double *time,
    double st[],
    double dst[],
    double param[],
    double dt,
    double *step,
    int neqin,
    double tol,
    double work[],
    int *err,
    int *which)
;

void sddc2ang(double dircos[3][3],
    double *a1,
    double *a2,
    double *a3)
;

void sddc2quat(double dircos[3][3],
    double *e1,
    double *e2,
    double *e3,
    double *e4)
;

void sdang2dc(double a1,
    double a2,
    double a3,
    double dircos[3][3])
;

void sdquat2dc(double ie1,
    double ie2,
    double ie3,
    double ie4,
    double dircos[3][3])
;
double 
sdvdot(double ivec1[3],
    double ivec2[3])
;
double 
sdvnorm(double ivec[3])
;

void sdvcopy(double ivec[3],
    double ovec[3])
;

void sdvset(double sclr1,
    double sclr2,
    double sclr3,
    double ovec[3])
;

void sdvadd(double ivec1[3],
    double ivec2[3],
    double ovec[3])
;

void sdvsub(double ivec1[3],
    double ivec2[3],
    double ovec[3])
;

void sdvmul(double sclr,
    double ivec[3],
    double ovec[3])
;

void sdvaxpy(double sclr,
    double ivec1[3],
    double ivec2[3],
    double ovec[3])
;

void sdvcross(double ivec1[3],
    double ivec2[3],
    double ovec[3])
;

void sdvrot(double ivec[3],
    double rvec[3],
    double theta,
    double ovec[3])
;

void sdserialno(int *serno)
;

void sduforce(double t,
    double q[],
    double u[])
;

void sdinit(void)
;

void sdst2ang(double st[3],
    double stang[3])
;

void sdang2st(double stang[3],
    double st[3])
;

void sdnrmsterr(double st[3],
    double normst[3],
    int routine)
;

void sdnormst(double st[3],
    double normst[3])
;

void sdstate(double timein,
    double qin[3],
    double uin[3])
;

void sdqdot(double oqdot[3])
;

void sdu2qdot(double uin[3],
    double oqdot[3])
;

void sdpsstate(double lqin[1])
;

void sddovpk(void)
;

void sddoltau(void)
;

void sddoiner(void)
;

void sddofs0(void)
;

void sddomm(int routine)
;

void sddommldu(int routine)
;

void sdlhs(int routine)
;

void sdmfrc(double imult[5])
;

void sdequivht(double tau[3])
;

void sdfs0(void)
;

void sdfsmult(void)
;

void sdfsfull(void)
;

void sdfsgenmult(void)
;

void sdfsgenfull(void)
;

void sdfulltrq(double udotin[3],
    double multin[5],
    double trqout[3])
;

void sdcomptrq(double udotin[3],
    double trqout[3])
;

void sdmulttrq(double multin[5],
    double trqout[3])
;

void sdrhs(void)
;

void sdmassmat(double mmat[3][3])
;

void sdfrcmat(double fmat[3])
;

void sdpseudo(double lqout[1],
    double luout[1])
;

void sdpsqdot(double lqdout[1])
;

void sdpsudot(double ludout[1])
;

void sdperr(double errs[5])
;

void sdverr(double errs[5])
;

void sdaerr(double errs[5])
;
int 
sdchkbnum(int routine,
    int bnum)
;
int 
sdchkjnum(int routine,
    int jnum)
;
int 
sdchkucnum(int routine,
    int ucnum)
;
int 
sdchkjaxis(int routine,
    int jnum,
    int axnum)
;
int 
sdchkjpin(int routine,
    int jnum,
    int pinno)
;
int 
sdindx(int joint,
    int axis)
;

void sdpresacc(int joint,
    int axis,
    double prval)
;

void sdpresvel(int joint,
    int axis,
    double prval)
;

void sdprespos(int joint,
    int axis,
    double prval)
;

void sdgetht(int joint,
    int axis,
    double *torque)
;

void sdhinget(int joint,
    int axis,
    double torque)
;

void sdpointf(int body,
    double point[3],
    double force[3])
;

void sdbodyt(int body,
    double torque[3])
;

void sddoww(int routine)
;

void sdxudot0(int routine,
    double oudot0[3])
;

void sdudot0(double oudot0[3])
;

void sdsetudot(double iudot[3])
;

void sdxudotm(int routine,
    double imult[5],
    double oudotm[3])
;

void sdudotm(double imult[5],
    double oudotm[3])
;

void sdderiv(double oqdot[3],
    double oudot[3])
;

void sdresid(double eqdot[3],
    double eudot[3],
    double emults[5],
    double resid[11])
;

void sdmult(double omults[5],
    int *owrank,
    int omultmap[5])
;

void sdreac(double force[4][3],
    double torque[4][3])
;

void sdmom(double lm[3],
    double am[3],
    double *ke)
;

void sdsys(double *mtoto,
    double cm[3],
    double icm[3][3])
;

void sdpos(int body,
    double pt[3],
    double loc[3])
;

void sdvel(int body,
    double pt[3],
    double velo[3])
;

void sdorient(int body,
    double dircos[3][3])
;

void sdangvel(int body,
    double avel[3])
;

void sdtrans(int frbod,
    double ivec[3],
    int tobod,
    double ovec[3])
;

void sdrel2cart(int coord,
    int body,
    double point[3],
    double linchg[3],
    double rotchg[3])
;

void sdacc(int body,
    double pt[3],
    double accel[3])
;

void sdangacc(int body,
    double aacc[3])
;

void sdgrav(double gravin[3])
;

void sdmass(int body,
    double massin)
;

void sdiner(int body,
    double inerin[3][3])
;

void sdbtj(int joint,
    double btjin[3])
;

void sditj(int joint,
    double itjin[3])
;

void sdpin(int joint,
    int pinno,
    double pinin[3])
;

void sdpres(int joint,
    int axis,
    int presin)
;

void sdconschg(void)
;

void sdstab(double velin,
    double posin)
;

void sdgetgrav(double gravout[3])
;

void sdgetmass(int body,
    double *massout)
;

void sdgetiner(int body,
    double inerout[3][3])
;

void sdgetbtj(int joint,
    double btjout[3])
;

void sdgetitj(int joint,
    double itjout[3])
;

void sdgetpin(int joint,
    int pinno,
    double pinout[3])
;

void sdgetpres(int joint,
    int axis,
    int *presout)
;

void sdgetstab(double *velout,
    double *posout)
;

void sdinfo(int info[50])
;

void sdjnt(int joint,
    int info[50],
    int tran[6])
;

void sdcons(int consno,
    int info[50])
;

void sdgentime(int *gentm)
;

void sdposfunc(double vars[3],
    double param[1],
    double resid[5])
;

void sdvelfunc(double vars[3],
    double param[4],
    double resid[5])
;

void sdstatfunc(double vars[3],
    double param[4],
    double resid[8])
;

void sdstdyfunc(double vars[6],
    double param[1],
    double resid[13])
;

void sdmotfunc(double time,
    double state[6],
    double dstate[6],
    double param[1],
    int *status)
;

void sdassemble(double time,
    double state[6],
    int lock[3],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
;

void sdinitvel(double time,
    double state[6],
    int lock[3],
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
;

void sdstatic(double time,
    double state[6],
    int lock[3],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
;

void sdsteady(double time,
    double state[6],
    int lock[6],
    double ctol,
    double tol,
    int maxevals,
    int *fcnt,
    int *err)
;

void sdmotion(double *time,
    double state[6],
    double dstate[6],
    double dt,
    double ctol,
    double tol,
    int *flag,
    int *err)
;

void sdfmotion(double *time,
    double state[6],
    double dstate[6],
    double dt,
    double ctol,
    int *flag,
    double *errest,
    int *err)
;
#ifdef __cplusplus
}
#endif
#endif /* SDFAST_SD_FBAR_DEFS_H */
