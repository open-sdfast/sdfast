/* Copyright 2026 Michael Sherman
 * Copyright 1989-2025 PTC Inc.; 1984-1988 Symbolic Dynamics, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "../calc/gencode.h"

/* COMPUTE_LDUCOST
 *
 * Return the number of operations in the LDU decomposition and related
 * back solve routines for a matrix of size w X w.
 *
 *   LDU complexity:  (w^3)/6 + (w^2)/2  +   w/3     multiplies
 *                    (w^3)/6 +          +  5w/6     adds/subtracts
 *                                           w       divides
 *                    (w^3)/6 + 5(w^2)/2 + 19w/3     assigns
 *
 *   back solve lower           (w^2)/2  -  w/2      multiplies
 *   (or upper) complexity:     (w^2)/2  -  w/2      adds/subtracts
 *                                          0        divides
 *                              (w^2)/2  + 3w/2 - 1  assigns
 *
 *   back solve diagonal                    w        multiplies
 *   complexity:                            0        adds/subtracts
 *                                          0        divides
 *                                          w        assigns
 *
 *   BSL and BSU have the same complexity.
 *
 * If you change anything in sdlduslv(), make sure you update the above
 * formulas here and in MAIN. 
 */
void COMPUTE_LDUCOST(int w,
                opstats_t *opcntldu,
                opstats_t *opcntbsl,
                opstats_t *opcntbsd)
{
    double s;

    s = (double)w;

    /* These formulas come from painstaking analysis of the SDLDUSLV code. */
    opcntldu->nmul = (long)(s*s*s/6. + s*s/2. + s/3. + 0.1);
    opcntldu->nadd = (long)(s*s*s/6. + 5.*s/6. + 0.1);
    opcntldu->ndiv = (long)(s + 0.1);
    opcntldu->nassign = (long)(s*s*s/6. + 5.*s*s/2. + 19.*s/3. + 0.1);

    opcntbsl->nmul = (long)(s*s/2. - s/2. + 0.1);
    opcntbsl->nadd = (long)(s*s/2. - s/2. + 0.1);
    opcntbsl->ndiv = 0;
    opcntbsl->nassign = (long)(s*s/2. + 3.*s/2. - 1. + 0.1);

    opcntbsd->nmul = (long)(s + 0.1);
    opcntbsd->nadd = 0;
    opcntbsd->ndiv = 0;
    opcntbsd->nassign = (long)(s + 0.1);
}

/* COMPUTE_QRCOST
 *
 * Return the number of operations in the QR decomposition for a matrix
 * of size r X c, and the number of operations for the related back solve
 * using a matrix of size r X k where k (<=c) is the number of independent
 * columns of the original matrix.  First, define
 * m = min(r-1,c).  All cubic-like terms below look like this
 *            z = m^3 - 3(m^2)r/2 - 3(m^2)c/2 + 3mrc
 * (Note that this is exactly m^3 if m=r=c which is roughly true for
 *  square matrices.)
 *
 * Then 
 *
 *   QR complexity:
 *   z -    m^2    +  mr/2 + 3mc/2 + rc                   multiplies
 *   z -    m^2    -  mr/2 + 5mc/2 + rc +  4m             adds/subtracts
 *     -   (m^2)/2 +          mc        +   m/2           divides
 *     -   (m^2)/2 +          mc        +   m/2     +  c  square roots
 *   z - 13(m^2)/4 + 7mr/2 + 6mc   + rc + 29m/4     + 2c  assigns
 *
 * XXX BUG: the above should be modified to include the fact that if
 *          there were redundant columns (as indicated by k < c) then
 *          most of the code in the QR is skipped for each redundancy.
 *          Some of those m's above should really be (m - (c-k)).
 *
 * CAVEAT:  The cost of column pivoting is data dependent so the above
 *          is some kind of `typical' cost which may not be right.
 *
 * CAVEAT:  Since we don't have a reporting category for square roots, we'll
 *          just count each square root as though it were one multiply and one
 *          divide.  The timing for this is about right on the (IEEE) Sun 3.
 *
 * For the backsolve, define m=min(r-1,k).  The square-like terms are all
 *            s = 2mr - m^2 + (k^2)/2
 * (Note that this is 3(m^2)/2 when m=r=k which is roughly true for
 *  square matrices of full rank.)
 *
 *   back solve complexity:            s +  m     -   k/2  multiplies
 *                                     s + 2m     +   k/2  adds/subtracts
 *                                          m     +   k    divides
 *                                     s + 6m + r +  5k/2  assigns
 *
 * If you change anything in sdqrslv(), make sure you update the above
 * formulas here and in MAIN.  
 */
void COMPUTE_QRCOST(int r,
               int c,
               int k,
               opstats_t *opcntqr,
               opstats_t *opcntqrbslv)
{
    double rd,cd,kd,m,z,s,sq;

    rd = (double)r;
    cd = (double)c;
    kd = (double)k;

    m = rd-1.;
    if (cd < m) 
        m = cd;

    /* These formulas come from painstaking analysis of the SDQRSLV code. */

    /* number of square roots */
    sq = m*cd - m*m/2. + m/2. + cd;

    /* cubic stuff */
    z = m*m*m - 3.*m*m*rd/2. - 3.*m*m*cd/2. + 3*m*rd*cd;

    opcntqr->nmul = (long)(z - m*m + m*rd/2. + 3.*m*cd/2. + rd*cd + sq + 0.1);
    opcntqr->nadd = (long)(z - m*m - m*rd/2. + 5.*m*cd/2. + rd*cd +4*m + 0.1);
    opcntqr->ndiv = (long)(m*cd - m*m/2. + m/2. + sq + 0.1);
    opcntqr->nassign = 
  (long)(z - 13.*m*m/4. + 7.*m*rd/2. + 6.*m*cd + rd*cd + 29.*m/4. + 2*cd + 0.1);

    m = rd-1;
    if (kd < m)
        m = kd;

    /* square stuff for backsolve */
    s = 2.*m*rd - m*m + kd*kd/2.;

    opcntqrbslv->nmul = (long)(s + m - kd/2. + 0.1);
    opcntqrbslv->nadd = (long)(s + 2.*m + kd/2. + 0.1);
    opcntqrbslv->ndiv = (long)(m+kd + 0.1);
    opcntqrbslv->nassign = (long)(s + 6.*m + rd + 5.*kd/2. + 0.1);
}

/* PRINT_SDLDUSLV 
 *
 * Generate routines for 1) producing an LDU decomposition of an arbitrary
 * size square, symmetric, positive definite matrix; and 2) back solving
 * to use this decomposition to solve simultaneous equations.
 */
void PRINT_SDLDUSLV(FILE *F)
{
    char lowlmt[10],lowlmtp1[10],uplmt[10],uplmtm1[10],tmp1[20];

    esprintf(lowlmt, "%@d", 0);
    esprintf(lowlmtp1, "%@d", 1);
    if (Lang->subs_offset) {
        esprintf(uplmt, "na");
        esprintf(uplmtm1, "na-1");
    } else {
        esprintf(uplmt, "na-1");
        esprintf(uplmtm1, "na-2");
    }

    efprintf(F, "\n%{\n\
===========================================================\n\
LDU decomposition scheme for solving Mx=b\n\
  M: na X na symmetric, pos. definite, upper rt triangle filled\n\
  b: na X 1\n\
  x: na X 1 (returned)\n\
Actual dimensions (n) may be larger, map says where to find\n\
the na interesting elements.\n");
    efprintf(F, "\
===========================================================\n%}");

    declare_proc(F, DECL_PACKED, "ldudcomp", 
      packvar(VT_INTEGER,                 "n"),
      packvar(VT_DUP,                     "na"),
      packvar(VT_SARRAY|VT_INTEGER,        "map", "na",       NULL),
      packvar(VT_REAL,                    "tol"),
      packvar(VT_SARRAY,                  "ld",  "na", "na", NULL),
      packvar(VT_SARRAY,                  "sum", "na",       NULL),
      packvar(VT_SARRAY,                  "m",   "n",  "n",  NULL),
      packvar(VT_DUP,                     "l"),
      packvar(VT_SARRAY,                  "d",   "n",        NULL),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "r", 
      VT_DUP,     "c", 
      VT_DUP,     "row", 
      VT_DUP,     "col", 
      VT_DUP,     "cm1",
      VT_DUP,     "j", 
      VT_DUP,     "mj", 
      VT_REAL,    "dtemp",
      VT_DUP,     "dtempi", 
      VT_DUP,     "ldtemp", 
      VT_DUP,     "tsum",
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, Lang->stmt_for_b, "400", "c", lowlmt, uplmt);
    efprintf(F, "col%=map%(c%)%;\n");
    efprintf(F, "cm1%=c-1%;\n\n");
    efprintf(F, Lang->stmt_for_b, "300", "r", "c", uplmt);
    efprintf(F, "row%=map%(r%)%;\n");

    /* note that we swap row,col here to access only the upper-right
     * triangle -- the bottom left is not filled in.
     */
    TWOINDX(F, "tsum%=", "m", "", 1, "n", "n", "col", "row", "%;\n");

    efprintf(F, Lang->stmt_for_b, "200", "j", lowlmt, "cm1");
    efprintf(F, "mj%=map%(j%)%;\n");
    TWOINDX(F, "tsum%=tsum-", "ld", "", 1, "na", "na", "r", "j", "*");
    TWOINDX(F, "", "l", "", 1, "n", "n", "col", "mj", "%;\n");
    efprintf(F, Lang->stmt_for_e, "200");
    efprintf(F, "sum%(r%)%=tsum%;\n");
    efprintf(F, Lang->stmt_for_e, "300");
    efprintf(F, "dtemp%=sum%(c%)%;\n");
    efprintf(F, "dtempi%=%r%;\n", 0.);
    esprintf(tmp1, "dtempi%=%r/dtemp%;", 1.);
    efprintf(F, Lang->stmt_if1, "dtemp", "tol", tmp1);
    efprintf(F, "d%(col%)%=dtempi%;\n");
    efprintf(F, Lang->stmt_for_b, "350", "r", "c", uplmt);
    efprintf(F, "row%=map%(r%)%;\n");
    efprintf(F, "ldtemp%=sum%(r%)%;\n");
    TWOINDX(F, "", "ld", "", 1, "na", "na", "r", "c", "%=ldtemp%;\n");
    TWOINDX(F, "", "l", "", 1, "n", "n", "row", "col", "%=ldtemp*dtempi%;\n");
    efprintf(F, Lang->stmt_for_e, "350");
    efprintf(F, Lang->stmt_for_e, "400");
    efprintf(F, Lang->proc_end);

    /* sdbsl
     */
    declare_proc(F, 0, "ldubsl", 
      VT_INTEGER, "n",
      VT_DUP,     "na",
      VT_SARRAY|VT_INTEGER, "map", "na", NULL,
      VT_SARRAY,  "l", "n", "n", NULL,
      VT_SARRAY,  "b", "n", NULL,
      VT_DUP,  "x",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "j", 
      VT_DUP, "im1",
      VT_DUP, "mi",
      VT_DUP, "mj",
      VT_REAL, "sum", 
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "mi%=map%(%s%)%;\n",lowlmt);
    efprintf(F, "x%(mi%)%=b%(mi%)%;\n");
    efprintf(F, Lang->stmt_for_b, "200", "i", lowlmtp1, uplmt);
    efprintf(F, "mi%=map%(i%)%;\n");
    efprintf(F, "im1%=i-1%;\nsum%=b%(mi%)%;\n");
    efprintf(F, Lang->stmt_for_b, "100", "j", lowlmt, "im1");
    efprintf(F, "mj%=map%(j%)%;\n");
    TWOINDX(F, "sum%=sum-", "l", "", 1, "n", "n", "mi", "mj", "*x%(mj%)%;\n");
    efprintf(F, Lang->stmt_for_e, "100");
    efprintf(F, "x%(mi%)%=sum%;\n");
    efprintf(F, Lang->stmt_for_e, "200");
    efprintf(F, Lang->proc_end);

    /* sdbsd
     */
    declare_proc(F, 0, "ldubsd", 
      VT_INTEGER, "n",
      VT_DUP,     "na",
      VT_SARRAY|VT_INTEGER, "map", "na", NULL,
      VT_SARRAY,  "d", "n", NULL,
      VT_DUP,  "b",
      VT_DUP,  "x",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "mi",
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, Lang->stmt_for_b, "100", "i", lowlmt, uplmt);
    efprintf(F, "mi%=map%(i%)%;\n");
    efprintf(F, "x%(mi%)%=b%(mi%)*d%(mi%)%;\n");
    efprintf(F, Lang->stmt_for_e, "100");
    efprintf(F, Lang->proc_end);

    /* sdbsu
     */
    declare_proc(F, 0, "ldubsu", 
      VT_INTEGER, "n",
      VT_DUP,     "na",
      VT_SARRAY|VT_INTEGER, "map", "na", NULL,
      VT_SARRAY,  "l", "n", "n", NULL,
      VT_SARRAY,  "b", "n", NULL,
      VT_DUP,  "x",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "j", 
      VT_DUP, "ip1",
      VT_DUP, "mi",
      VT_DUP, "mj",
      VT_REAL, "sum", 
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "mi%=map%(%s%)%;\n",uplmt);
    efprintf(F, "x%(mi%)%=b%(mi%)%;\ni%=%s%;\n", uplmtm1);
    efprintf(F, Lang->stmt_while_b, "100");
    efprintf(F, "i");
    efprintf(F, Lang->stmt_while_do, Lang->ge_op, lowlmt);
    efprintf(F, "\
mi%=map%(i%)%;\n\
ip1%=i+1%;\n\
sum%=b%(mi%)%;\n");
    efprintf(F, Lang->stmt_for_b, "200", "j", "ip1", uplmt);
    efprintf(F, "mj%=map%(j%)%;\n");
    TWOINDX(F, "sum%=sum-", "l", "", 1, "n", "n", "mj", "mi", "*x%(mj%)%;\n");
    efprintf(F, Lang->stmt_for_e, "200");
    efprintf(F, "x%(mi%)%=sum%;\ni%=i-1%;\n");
    efprintf(F, Lang->stmt_while_e, "100");
    efprintf(F, Lang->proc_end);

    /* sdldubslv
     */
    declare_proc(F, DECL_PACKED, "ldubslv", 
      packvar(VT_INTEGER,                 "n"),
      packvar(VT_DUP,                     "na"),
      packvar(VT_SARRAY|VT_INTEGER,        "map",  "na",     NULL),
      packvar(VT_SARRAY,                  "work", "n",      NULL),
      packvar(VT_SARRAY,                  "l",    "n", "n", NULL),
      packvar(VT_SARRAY,                  "d",    "n",      NULL),
      packvar(VT_DUP,                          "b"),
      packvar(VT_DUP,                          "x"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    efprintf(F, "%:Ps\
%@P%Aldubsl(n,na,map,l,b,work)%;\n\
%@P%Aldubsd(n,na,map,d,work,work)%;\n\
%@P%Aldubsu(n,na,map,l,work,x)%;\n",
             Lang->proc_call);
    efprintf(F, Lang->proc_end);

    /* sdlduslv
     */
    declare_proc(F, DECL_PACKED, "lduslv", 
      packvar(VT_INTEGER,                 "n"),
      packvar(VT_DUP,                     "na"),
      packvar(VT_SARRAY|VT_INTEGER,         "map",   "na",       NULL),
      packvar(VT_REAL,                    "tol"),
      packvar(VT_SARRAY,                  "work1", "na", "na", NULL),
      packvar(VT_SARRAY,                  "work2", "n",        NULL),
      packvar(VT_SARRAY,                  "m",     "n",  "n",  NULL),
      packvar(VT_SARRAY,                  "b",     "n",        NULL),
      packvar(VT_SARRAY,                  "l",     "n",  "n",  NULL),
      packvar(VT_SARRAY,                  "d",     "n",        NULL),
      packvar(VT_DUP,                          "x"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    efprintf(F, "%:Ps\
%@P%Aldudcomp(n,na,map,tol,work1,work2,m,l,d)%;\n\
%@P%Aldubslv(n,na,map,work2,l,d,b,x)%;\n",
             Lang->proc_call);
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDQRSLV 
 *
 * Generate routines for 1) producing a QR decomposition of an arbitrary
 * size rectangular, over- or under-determined, ill-conditioned matrix;
 * and 2) backsolving to use this decomposition to produce a 
 * solution to simultaneous equations.  In the case of an over-determined
 * system (with no solution) the returned solution is one which minimizes
 * the sum of squares of the residuals.  WARNING: you do not get the least 
 * squares solution to an underdetermined system.  Instead, you get a 
 * solution in which the elements corresponding to redundant or non-existent 
 * equations are set to 0.  The method is basically LINPACK's
 * QR with pivoting.  
 */
void PRINT_SDQRSLV(FILE *F)
{
    char lowlmt[10],lowlmtp1[10],str_flt0[10];
    char uplmtr[15],uplmtc[15],lmtminrca[15],uplmtk[15],lmtminrk[15];
    int i;

    esprintf(lowlmt, "%@d", 0);
    esprintf(lowlmtp1, "%@d", 1);
    esprintf(str_flt0, "%r", 0.);
    if (Lang->subs_offset == 1) {
        esprintf(uplmtr, "nra");
        esprintf(uplmtc, "nca");
        esprintf(uplmtk, "k");
        esprintf(lmtminrca, "minrca");
        esprintf(lmtminrk, "minrk");
    } else {
        esprintf(uplmtr, "nra-%d", 1-Lang->subs_offset);
        esprintf(uplmtc, "nca-%d", 1-Lang->subs_offset);
        esprintf(uplmtk, "k-%d", 1-Lang->subs_offset);
        esprintf(lmtminrca, "minrca-%d",1-Lang->subs_offset);
        esprintf(lmtminrk, "minrk-%d",1-Lang->subs_offset);
    }

    efprintf(F, "\n%{\n\
===========================================================\n\
QR decomposition scheme for solving Wx=b\n\
  W is nra by nca\n\
  b is nra by 1\n\
  x (returned) is nca by 1\n\
Actual dimensions (nr,nc) may be larger, mapr (mapc) says where\n\
to find the nra (nca) interesting rows (columns).  On return, W is\n");
    efprintf(F, "\
overwritten by the Q and R matrices in compact form.  Solution yields\n\
least squares residual for overdetermined systems.  Underdetermined\n\
systems give a solution in which elements corresponding to redundant\n\
or missing equations are set to 0. (Not necessarily the LS solution.)\n");
    efprintf(F, "\
===========================================================\n%}");

    /* sdqrdcomp 
     */
    declare_proc(F, DECL_PACKED, "qrdcomp", 
      packvar(VT_INTEGER,                 "nr"),
      packvar(VT_DUP,                     "nc"),
      packvar(VT_DUP,                     "nra"),
      packvar(VT_DUP,                     "nca"),
      packvar(VT_SARRAY|VT_INTEGER,        "mapr",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "mapc",  "nca",      NULL),
      packvar(VT_SARRAY,                  "w",     "nr", "nc", NULL),
      packvar(VT_SARRAY,                  "qraux", "nca",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "jpvt",  "nca",      NULL),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP,     "j", 
      VT_DUP,     "l", 
      VT_DUP,     "lp1", 
      VT_DUP,     "maxj",
      VT_DUP,     "mi", 
      VT_DUP,     "mj", 
      VT_DUP,     "mlr", 
      VT_DUP,     "mlc", 
      VT_DUP,     "mmaxj", 
      VT_DUP,     "minrca", 
      VT_REAL,    "maxnrm",
      VT_DUP,     "nrmxl", 
      VT_DUP,     "t", 
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, Lang->stmt_for_b, "70", "j", lowlmt, uplmtc);
    efprintf(F, "mj%=mapc%(j%)%;\n");
    efprintf(F, "jpvt%(j%)%=j%;\n");
    efprintf(F, "t%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_b, "60", "l", lowlmt, uplmtr);
    efprintf(F, "mlr%=mapr%(l%)%;\n");
    TWOINDX(F, "t%=t+", "w", "", 1, "nr", "nc", "mlr", "mj", "*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mlr", "mj", "%;\n");
    efprintf(F, Lang->stmt_for_e, "60");
    efprintf(F, "qraux%(j%)%=%@D%s(t)%;\n",Lang->func_sqrt);
    efprintf(F, Lang->stmt_for_e, "70");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "nca");
    efprintf(F, Lang->stmt_if2_then, Lang->lt_op, "nra");
    efprintf(F, "minrca%=nca%;\n");
    efprintf(F, Lang->stmt_if2_else);
    efprintf(F, "minrca%=nra%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_b, "200", "l", lowlmt, lmtminrca);
    efprintf(F, "mlr%=mapr%(l%)%;\n");
    efprintf(F, "mlc%=mapc%(l%)%;\n");
    efprintf(F, "maxnrm%=%r%;\n",0.);
    efprintf(F, "maxj%=l%;\n");
    efprintf(F, Lang->stmt_for_b, "100", "j", "l", uplmtc);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "qraux%(j%)");
    efprintf(F, Lang->stmt_if2_then, Lang->gt_op, "maxnrm");
    efprintf(F, "maxnrm%=qraux%(j%)%;\n");
    efprintf(F, "maxj%=j%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_e, "100");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "maxj");
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "l");
    efprintf(F, "mmaxj%=mapc%(maxj%)%;\n");
    efprintf(F, Lang->stmt_for_b, "105", "i", lowlmt, uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "t%=", "w", "", 1, "nr", "nc", "mi", "mlc", "%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mlc", "%=");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mmaxj", "%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mmaxj", "%=t%;\n");
    efprintf(F, Lang->stmt_for_e, "105");
    efprintf(F, "qraux%(maxj%)%=qraux%(l%)%;\n");
    efprintf(F, "i%=jpvt%(maxj%)%;\n");
    efprintf(F, "jpvt%(maxj%)%=jpvt%(l%)%;\n");
    efprintf(F, "jpvt%(l%)%=i%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, "qraux%(l%)%=%r%;\n",0.);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "l");
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, uplmtr);
    efprintf(F, "t%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_b, "110", "i", "l", uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "t%=t+", "w", "", 1, "nr", "nc", "mi", "mlc", "*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mlc", "%;\n");
    efprintf(F, Lang->stmt_for_e, "110");
    efprintf(F, "nrmxl%=%@D%s(t)%;\n",Lang->func_sqrt);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "nrmxl");
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
    efprintf(F, Lang->stmt_if2_b);
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mlr", "mlc", "");
    efprintf(F, Lang->stmt_if2_then, Lang->lt_op, str_flt0);
    efprintf(F, "nrmxl%= -nrmxl%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, "t%=%r/nrmxl%;\n",1.);
    efprintf(F, Lang->stmt_for_b, "120", "i", "l", uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mlc", "%=t*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mlc", "%;\n");
    efprintf(F, Lang->stmt_for_e, "120");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mlr", "mlc", "%=");
    efprintf(F, "%r+",1.);
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mlr", "mlc", "%;\n");
    efprintf(F, "lp1%=l+1%;\n");
    efprintf(F, Lang->stmt_for_b, "160", "j", "lp1", uplmtc);
    efprintf(F, "mj%=mapc%(j%)%;\n");
    efprintf(F, "t%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_b, "130", "i", "l", uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "t%=t-", "w", "", 1, "nr", "nc", "mi", "mlc", "*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mj", "%;\n");
    efprintf(F, Lang->stmt_for_e, "130");
    TWOINDX(F, "t%=t/", "w", "", 1, "nr", "nc", "mlr", "mlc", "%;\n");
    efprintf(F, Lang->stmt_for_b, "140", "i", "l", uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mj", "%=");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mj", "+t*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mlc", "%;\n");
    efprintf(F, Lang->stmt_for_e, "140");
    efprintf(F, "t%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_b, "150", "i", "lp1", uplmtr);
    efprintf(F, "mi%=mapr%(i%)%;\n");
    TWOINDX(F, "t%=t+", "w", "", 1, "nr", "nc", "mi", "mj", "*");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mi", "mj", "%;\n");
    efprintf(F, Lang->stmt_for_e, "150");
    efprintf(F, "qraux%(j%)%=%@D%s(t)%;\n",Lang->func_sqrt);
    efprintf(F, Lang->stmt_for_e, "160");
    TWOINDX(F, "qraux%(l%)%=", "w", "", 1, "nr", "nc", "mlr", "mlc", "%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mlr", "mlc", "%= -nrmxl%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_e, "200");
    efprintf(F, Lang->proc_end);

    /* sdqrsl
     */
    declare_proc(F, DECL_PACKED, "qrsl", 
      packvar(VT_INTEGER,                 "nr"),
      packvar(VT_DUP,                     "nc"),
      packvar(VT_DUP,                     "nra"),
      packvar(VT_DUP,                     "nca"),
      packvar(VT_SARRAY|VT_INTEGER,        "mapr",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,        "mapc",  "nca",      NULL),
      packvar(VT_INTEGER,                 "k"),
      packvar(VT_SARRAY,                  "work",  "nra",      NULL),
      packvar(VT_SARRAY,                  "w",     "nr", "nc", NULL),
      packvar(VT_SARRAY,                  "qraux", "nca",      NULL),
      packvar(VT_SARRAY,                  "b",     "nr",       NULL),
      packvar(VT_SARRAY,                  "x",     "nc",       NULL),
      packvar(0));

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "j", 
      VT_DUP, "jj",
      VT_DUP, "jm1",
      VT_DUP, "mir",
      VT_DUP, "mic",
      VT_DUP, "mjr",
      VT_DUP, "mjc",
      VT_DUP, "minrk",
      VT_REAL, "t", 
      VT_DUP, "tt", 
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, Lang->stmt_for_b, "50", "i", lowlmt, uplmtr);
    efprintf(F, "mir%=mapr%(i%)%;\n");
    efprintf(F, "work%(i%)%=b%(mir%)%;\n");
    efprintf(F, Lang->stmt_for_e, "50");

    efprintf(F, "minrk%=nra-1%;\n");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "k");
    efprintf(F, Lang->stmt_if2_then, Lang->lt_op, "minrk");
    efprintf(F, "minrk%=k%;\n");
    efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->stmt_for_b, "90", "j", lowlmt, lmtminrk);
    efprintf(F, "mjr%=mapr%(j%)%;\n");
    efprintf(F, "mjc%=mapc%(j%)%;\n");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "qraux%(j%)");
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
    TWOINDX(F, "tt%=", "w", "", 1, "nr", "nc", "mjr", "mjc", "%;\n");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mjr", "mjc", "%=qraux%(j%)%;\n");
    efprintf(F, "t%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_b, "60", "i", "j", uplmtr);
    efprintf(F, "mir%=mapr%(i%)%;\n");
    TWOINDX(F, "t%=t-", "w", "", 1, "nr", "nc", "mir", "mjc", "*work%(i%)%;\n");
    efprintf(F, Lang->stmt_for_e, "60");
    TWOINDX(F, "t%=t/", "w", "", 1, "nr", "nc", "mjr", "mjc", "%;\n");
    efprintf(F, Lang->stmt_for_b, "70", "i", "j", uplmtr);
    efprintf(F, "mir%=mapr%(i%)%;\n");
    TWOINDX(F, "work%(i%)%=work%(i%)+t*", 
                "w", "", 1, "nr", "nc", "mir", "mjc", "%;\n");
    efprintf(F, Lang->stmt_for_e, "70");
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mjr", "mjc", "%=tt%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_e, "90");
    efprintf(F, Lang->stmt_for_b, "100", "j", lowlmt, uplmtk);
    efprintf(F, "mjc%=mapc%(j%)%;\n");
    efprintf(F, "x%(mjc%)%=work%(j%)%;\n");
    efprintf(F, Lang->stmt_for_e, "100");
    efprintf(F, Lang->stmt_for_b, "170", "jj", lowlmt, uplmtk);
    /* We want j to count down from uplmtk to lowlmt.  If you work
     * it out, that means j=k-jj+2*Lang->subs_offset-1.
     */
    i = 2*Lang->subs_offset-1;
    if (i < 0)
        efprintf(F, "j%=k-jj-%d%;\n",-i);
    else
        efprintf(F, "j%=k-jj+%d%;\n",i);
    efprintf(F, "jm1%=j-1%;\n");
    efprintf(F, "mjr%=mapr%(j%)%;\n");
    efprintf(F, "mjc%=mapc%(j%)%;\n");
    TWOINDX(F, "x%(mjc%)%=x%(mjc%)/", 
                "w", "", 1,"nr", "nc", "mjr", "mjc", "%;\n");
    efprintf(F, "t%= -x%(mjc%)%;\n");
    efprintf(F, Lang->stmt_for_b, "160", "i", lowlmt, "jm1");
    efprintf(F, "mir%=mapr%(i%)%;\n");
    efprintf(F, "mic%=mapc%(i%)%;\n");
    TWOINDX(F, "x%(mic%)%=x%(mic%)+t*", 
                "w", "", 1, "nr", "nc", "mir", "mjc", "%;\n");
    efprintf(F, Lang->stmt_for_e, "160");
    efprintf(F, Lang->stmt_for_e, "170");
    efprintf(F, Lang->proc_end);

    /* sdqrbslv
     */
    declare_proc(F, DECL_PACKED, "qrbslv", 
      packvar(VT_INTEGER,                 "nr"),
      packvar(VT_DUP,                     "nc"),
      packvar(VT_DUP,                     "nra"),
      packvar(VT_DUP,                     "nca"),
      packvar(VT_SARRAY|VT_INTEGER,         "mapr",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "mapc",  "nca",      NULL),
      packvar(VT_REAL,                    "tol"),
      packvar(VT_SARRAY,                  "work",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "iwork", "nca",      NULL),
      packvar(VT_SARRAY,                  "w",     "nr", "nc", NULL),
      packvar(VT_SARRAY,                  "qraux", "nca",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "jpvt",  "nca",      NULL),
      packvar(VT_SARRAY,                  "b",     "nr",       NULL),
      packvar(VT_SARRAY,                  "x",     "nc",       NULL),
      packvar(VT_INTEGER|VT_BYREF,         "rank"),
      packvar(0));

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "j", 
      VT_DUP, "k",
      VT_DUP, "kk",
      VT_DUP, "mjc",
      VT_DUP, "mkkr",
      VT_DUP, "mkkc",
      VT_DUP, "minrca",
      VT_REAL, "t", 
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    /* Be sure not to trash jpvt! */
    FORCNT("10", "j", "nca");
      SET("iwork%(j%)", "jpvt%(j%)");
    ENDFOR("10");

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "nca");
    efprintf(F, Lang->stmt_if2_then, Lang->lt_op, "nra");
    efprintf(F, "minrca%=nca%;\n");
    efprintf(F, Lang->stmt_if2_else);
    efprintf(F, "minrca%=nra%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, "k%=0%;\n");
    efprintf(F, "mkkr%=mapr%(%@d%)%;\n", 0);
    efprintf(F, "mkkc%=mapc%(%@d%)%;\n", 0);

    /* This sets a lower bound on the *relative* size of an accepted
     * diagonal element to tol*w(1,1).  We don't limit the minimum
     * *absolute* value since a small one may just represent bad
     * scaling.
     */
    efprintf(F, "t%=tol*%@D%s(", Lang->func_abs);
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mkkr", "mkkc", ")%;\n");

    /* Now decide on the rank. */
    efprintf(F, Lang->stmt_for_b, "20", "kk", lowlmt, lmtminrca);
    efprintf(F, "mkkr%=mapr%(kk%)%;\n");
    efprintf(F, "mkkc%=mapc%(kk%)%;\n");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "%@D%s(", Lang->func_abs);
    TWOINDX(F, "", "w", "", 1, "nr", "nc", "mkkr", "mkkc", ")");
    efprintf(F, Lang->stmt_if2_then, Lang->le_op, "t");
    efprintf(F, Lang->stmt_break, "30");
    efprintf(F, Lang->stmt_if2_else);
    efprintf(F, "k%=k+1%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_e_brk, "20", "30");

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "k");
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
    efprintf(F, "%s%Aqrsl(nr,nc,nra,nca,mapr,mapc,%&k,work,w,qraux,b,x)%;\n",
                Lang->proc_call);
    efprintf(F, Lang->stmt_if2_e);
    if (Lang->subs_offset != 0) {
        efprintf(F, "kk%=k+%d%;\n",Lang->subs_offset);
        efprintf(F, Lang->stmt_for_b, "35", "j", "kk", uplmtc);
    } else {
        efprintf(F, Lang->stmt_for_b, "35", "j", "k", uplmtc);
    }
    efprintf(F, "x%(mapc%(j%)%)%=%r%;\n",0.);
    efprintf(F, Lang->stmt_for_e, "35");
    efprintf(F, Lang->stmt_for_b, "50", "j", lowlmt, uplmtc);
    efprintf(F, "kk%=iwork%(j%)%;\n");
    efprintf(F, Lang->stmt_while_b, "40");
    efprintf(F, "kk");
    efprintf(F, Lang->stmt_while_do, Lang->ne_op, "j");
    efprintf(F, "\
mjc%=mapc%(j%)%;\n\
mkkc%=mapc%(kk%)%;\n\
t%=x%(mjc%)%;\n\
x%(mjc%)%=x%(mkkc%)%;\n\
x%(mkkc%)%=t%;\n\
iwork%(j%)%=iwork%(kk%)%;\n\
iwork%(kk%)%=kk%;\n\
kk%=iwork%(j%)%;\n");
    efprintf(F, Lang->stmt_while_e, "40");
    efprintf(F, Lang->stmt_for_e, "50");
    efprintf(F, "%srank%=k%;\n",Lang->deref);
    efprintf(F, Lang->proc_end);

    /* sdqrslv
     */
    declare_proc(F, DECL_PACKED, "qrslv", 
      packvar(VT_INTEGER,                 "nr"),
      packvar(VT_DUP,                     "nc"),
      packvar(VT_DUP,                     "nra"),
      packvar(VT_DUP,                     "nca"),
      packvar(VT_SARRAY|VT_INTEGER,         "mapr",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "mapc",  "nca",      NULL),
      packvar(VT_REAL,                    "tol"),
      packvar(VT_SARRAY|VT_INTEGER,         "jpvt",  "nca",      NULL),
      packvar(VT_SARRAY,                  "qraux", "nca",      NULL),
      packvar(VT_SARRAY,                  "work",  "nra",      NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "iwork", "nca",      NULL),
      packvar(VT_SARRAY,                  "w",     "nr", "nc", NULL),
      packvar(VT_SARRAY,                  "b",     "nr",       NULL),
      packvar(VT_SARRAY,                  "x",     "nc",       NULL),
      packvar(VT_INTEGER|VT_BYREF,         "rank"),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
    efprintf(F, "%:Ps\
%@P%Aqrdcomp(nr,nc,nra,nca,mapr,mapc,w,qraux,jpvt)%;\n\
%@P%Aqrbslv(nr,nc,nra,nca,mapr,mapc,tol,work,iwork,w,qraux,%&\
jpvt,b,x,rank)%;\n",
             Lang->proc_call);
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDLSSLV 
 *
 * Generates the sdlsslv routine which flexibly solves a set of linear 
 * equations Ax=b for the `best' answer x.
 * For an overdetermined system, x is the solution which minimizes the
 * 2-norm (sqrt of sum of the squares) of the residuals.  For an 
 * underdetermined system, x is the solution whose 2-norm is minimum.
 *
 * The equations are divided into those which *must* be satisfied
 * (required equations), and those that the caller would *like* to have 
 * satisfied (desired equations).  That is, if we can't satisfy all the 
 * equations, we'll try to let the slop fall on the desireds rather
 * than the requireds.
 *
 * A is neq X nv, b is neq X 1.  Neq=nr+nd is the number of equations,
 * with nr the number of requireds and nd the number of desireds.  Nv is
 * the number of independent variables.
 *
 * Tol says how close to redundant a pair of equations must be in order
 * for one of them to be tossed out.
 *
 * A and b are returned unchanged.
 *
 * Method:
 *
 * If we solve this problem directly with the QR, and the problem turns out 
 * to be rank deficient, the solution we get won't be the least squares 
 * solution (which is the one we want).  And of course if there are desireds,
 * the straight QR won't properly dump all the errors on them.  So we use
 * the QR to solve related problems whose solutions lead to the desired
 * solution of the original problem.  The related problems will probably
 * be less well conditioned than the originals, so the answers won't be as
 * good, but they should still be fine for use in sdnr since they'll still
 * probably point us in the right direction.
 *
 * 1) In the case that there are no desireds, we can just solve the original
 * problem unless the system is underdetermined.  Unfortunately, we have to
 * perform the QR decomposition of A just to find out its rank.  If we then
 * have to solve a different problem, we'll end up running the QR twice.
 * So we always solve the modified problem, since it continues to yield the
 * correct solution in square or overdetermined problems, and gives the least
 * squares solution for underdetermined problems:
 * 
 *          Solve    AA'y=b           (where A' is TRANSPOSE(A))
 *          then set x = A'y
 *
 * 2) In the case where there are `desired' equations, we construct
 * new matrices D and r as follows:
 * 
 * Think of A and b partitioned as follows, with dimensions as shown:
 *
 *             nv                       1
 *       A = [ A1 ] nr           b = [ b1 ] nr
 *           [ A2 ] nd               [ b2 ] nd
 *
 *     Then build D and r as follows:
 *
 *              nv   nr                 1
 *       D = [ D11  D12 ] nv     r = [ r1 ] nv
 *           [ D21  D22 ] nr         [ r2 ] nr
 *
 *       D11 = A2'*A2      r1 = A2'*b2
 *       D12 = A1'         r2 = b1
 *       D21 = A1
 *       D22 = 0
 *
 * Finally solve Dy = r.  The first nv elements of y are the solution x
 * to the original problem.  We don't attempt to produce a least squares
 * solution if D is rank deficient (underdetermined) since that doesn't
 * appear to affect performance at all.  That may, however, be because
 * the structure of D causes the QR to return the least squares solution
 * anyway but we don't know.
 *
 * The above method for dealing with desireds was taken from "Matrix 
 * Computations", Golub & Van Loan, Section 12.1.
 *
 * Note: this would be somewhat nicer if we were using an SVD rather than a QR
 * since we would then always get the least squares solution without having
 * to screw around.  The answers would also be more accurate since we wouldn't 
 * have to deal with AA' (no desireds) or DD' (desireds).
 */
void PRINT_SDLSSLV(FILE *F)
{
    char lowlmt[10],str_flt0[10];
    char uplmtr[15],uplmtc[15],uplmtndes[15],uplmtnreq[15],uplmtdsiz[15];

    esprintf(lowlmt, "%@d", 0);
    esprintf(str_flt0, "%r", 0.);
    if (Lang->subs_offset == 1) {
        esprintf(uplmtr, "nra");
        esprintf(uplmtc, "nca");
        esprintf(uplmtndes, "ndes");
        esprintf(uplmtnreq, "nreq");
        esprintf(uplmtdsiz, "dsiz");
    } else {
        esprintf(uplmtr, "nra-1");
        esprintf(uplmtc, "nca-1");
        esprintf(uplmtndes, "ndes-1");
        esprintf(uplmtnreq, "nreq-1");
        esprintf(uplmtdsiz, "dsiz-1");
    }

    efprintf(F, "\n%{\n\
===========================================================\n\
Linear equation solver for Wx=b\n\
  W is nra by nca\n\
  b is nra by 1\n\
  x (returned) is nca by 1\n\
Actual dimensions (nr,nc) may be larger, mapr (mapc) says where\n\
to find the nra (nca) interesting rows (columns).  On return, W and\n\
b are unchanged.  Solution yields least squares residual for\n");
    efprintf(F, "\
overdetermined systems and a least squares solution vector for\n\
underdetermined systems.  If ndes > 0, the first nra-ndes rows\n\
are `required' while the remaining ndes rows are `desired'.  The\n\
returned solution minimizes the residual for the required rows,\n\
then does the best it can on the desired rows.\n");
    efprintf(F, "\
\n\
The performance and numerical properties of this routine are\n\
considerably worse than those of the qrslv routine.  However, it\n\
works very well as a guide to the root finding routine.\n\
\n\
Work arrays should be dimensioned as follows:\n\
  dw is 2*(nra+nca)**2\n\
  rw is 4*(nra+nca)\n\
  iw is 3*(nra+nca)\n\
===========================================================\n%}");

    /* sdlsslv
     */
    declare_proc(F, DECL_PACKED, "lsslv", 
      packvar(VT_INTEGER,                 "nr"),
      packvar(VT_DUP,                     "nc"),
      packvar(VT_DUP,                     "nra"),
      packvar(VT_DUP,                     "nca"),
      packvar(VT_DUP,                     "ndes"),
      packvar(VT_SARRAY|VT_INTEGER,        "mapr", "nra",              NULL),
      packvar(VT_SARRAY|VT_INTEGER,         "mapc", "nca",              NULL),
      packvar(VT_REAL,                    "tol"),
      packvar(VT_SARRAY,                  "dw",    Lang->unknown_len, NULL),
      packvar(VT_DUP,                     "rw"),
      packvar(VT_SARRAY|VT_INTEGER,          "iw",    Lang->unknown_len, NULL),
      packvar(VT_SARRAY,                  "w",    "nr", "nc",         NULL),
      packvar(VT_SARRAY,                  "b",    "nr",               NULL),
      packvar(VT_SARRAY,                  "x",    "nc",               NULL),
      packvar(0));
    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0, 
      VT_INTEGER, "i", 
      VT_DUP, "j", 
      VT_DUP, "k",
      VT_DUP, "nreq",
      VT_DUP, "dsiz",
      VT_DUP, "rank",
      VT_DUP, "mapi",
      VT_DUP, "mapj",
      VT_DUP, "mapk",
      0);
    declare_vars(F, 0, 
      VT_INTEGER, "ix",
      VT_DUP, "wwt",
      VT_DUP, "dloc",
      VT_DUP, "ddt",
      VT_DUP, "qraux",
      VT_DUP, "work",
      VT_DUP, "rhs",
      VT_DUP, "soln",
      VT_DUP, "map",
      VT_DUP, "jpvt",
      VT_DUP, "iwork",
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "nreq%=nra-ndes%;\n");
    efprintf(F, "dsiz%=nca+nreq%;\n");

    /* Locations of variables in work arrays */
    /* DW */
    efprintf(F, "wwt%=%@d%;\n",0);
    efprintf(F, "dloc%=%@d%;\n",0);
    efprintf(F, "ddt%=dloc+dsiz*dsiz%;\n");
    /* RW */
    efprintf(F, "qraux%=%@d%;\n",0);
    efprintf(F, "work%=qraux+nca+nra%;\n");
    efprintf(F, "rhs%=work+nca+nra%;\n");
    efprintf(F, "soln%=rhs+nca+nra%;\n");
    /* IW */
    efprintf(F, "map%=%@d%;\n",0);
    efprintf(F, "jpvt%=map+nca+nra%;\n");
    efprintf(F, "iwork%=jpvt+nca+nra%;\n");

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "ndes");
    efprintf(F, Lang->stmt_if2_then, Lang->eq_op, "0");

        /* no desireds -- build AA' (symmetric) */

        efprintf(F, Lang->stmt_for_b, "130", "i", lowlmt, uplmtr);
        efprintf(F, "mapi%=mapr%(i%)%;\n");
        ONEINDX(F, "", "rw", "rhs", "i", "%=");
        efprintf(F, "b%(mapi%)%;\n");
        efprintf(F, Lang->stmt_for_b, "120", "j", "i", uplmtr);
        efprintf(F, "mapj%=mapr%(j%)%;\n");
        TWOINDX(F, "ix%=", "", "wwt", 0, "nra", "nra", "i", "j", "%;\n");
        efprintf(F, "dw%(ix%)%=%r%;\n", 0.0);
        efprintf(F, Lang->stmt_for_b, "110", "k", lowlmt, uplmtc);
        efprintf(F, "mapk%=mapc%(k%)%;\n");
        TWOINDX(F, "dw%(ix%)%=dw%(ix%)+", 
                   "w", "", 1, "nr", "nc", "mapi", "mapk", "*");
        TWOINDX(F, "", "w", "", 1, "nr", "nc", "mapj", "mapk", "%;\n");
        efprintf(F, Lang->stmt_for_e, "110");
        TWOINDX(F, "", "dw", "wwt", 0, "nra", "nra", "j", "i", 
                "%=dw%(ix%)%;\n");
        efprintf(F, Lang->stmt_for_e, "120");
        ONEINDX(F, "", "iw", "map", "i", "%=i%;\n");
        efprintf(F, Lang->stmt_for_e, "130");

        /* solve for y */
        efprintf(F, "%:Ps\
%s%Aqrslv(nra,nra,nra,nra,%Piw%(map%),%Piw%(map%),%&\
tol,%Piw%(jpvt%),%Prw%(qraux%),%Prw%(work%),%Piw%(iwork%),%&\
%Pdw%(wwt%),%Prw%(rhs%),%Prw%(soln%),%Prank)%;\n",
                 Lang->ref, Lang->proc_call);

        /* compute x = A'y */
        efprintf(F, Lang->stmt_for_b, "220", "i", lowlmt, uplmtc);
        efprintf(F, "mapi%=mapc%(i%)%;\n");
        efprintf(F, "x%(mapi%)%=%r%;\n",0.0);
        efprintf(F, Lang->stmt_for_b, "210", "j", lowlmt, uplmtr);
        efprintf(F, "mapj%=mapr%(j%)%;\n");
        TWOINDX(F, "x%(mapi%)%=x%(mapi%)+", 
                   "w", "", 1, "nr", "nc", "mapj", "mapi", "*");
        ONEINDX(F, "", "rw", "soln", "j", "%;\n");
        efprintf(F, Lang->stmt_for_e, "210");
        efprintf(F, Lang->stmt_for_e, "220");

    efprintf(F, Lang->stmt_if2_else);

        /* the problem has desireds */

        efprintf(F, Lang->stmt_for_b, "350", "i", lowlmt, uplmtc);
        efprintf(F, "mapi%=mapc%(i%)%;\n");

        /* build D11 (symmetric) */
        efprintf(F, Lang->stmt_for_b, "320", "j", "i", uplmtc);
        efprintf(F, "mapj%=mapc%(j%)%;\n");
        TWOINDX(F, "ix%=", "", "dloc", 0, "dsiz", "dsiz", "i", "j", "%;\n");
        efprintf(F, "dw%(ix%)%=%r%;\n", 0.0);
        efprintf(F, Lang->stmt_for_b, "310", "k", lowlmt, uplmtndes);
        efprintf(F, "mapk%=mapr%(nreq+k%)%;\n");
        TWOINDX(F, "dw%(ix%)%=dw%(ix%)+", 
                   "w", "", 1, "nr", "nc", "mapk", "mapi", "*");
        TWOINDX(F, "", "w", "", 1, "nr", "nc", "mapk", "mapj", "%;\n");
        efprintf(F, Lang->stmt_for_e, "310");
        TWOINDX(F, "", "dw", "dloc", 0, "dsiz", "dsiz", "j", "i", 
                "%=dw%(ix%)%;\n");
        efprintf(F, Lang->stmt_for_e, "320");

        /* build D12 and D21 (D21=D12') */
        efprintf(F, Lang->stmt_for_b, "330", "j", lowlmt, uplmtnreq);
        efprintf(F, "mapj%=mapr%(j%)%;\n");
        TWOINDX(F, "ix%=", "", "dloc", 0, "dsiz", "dsiz", "i", "(j+nca)", 
                "%;\n");
        TWOINDX(F, "dw%(ix%)%=", 
                "w", "", 1, "nr", "nc", "mapj", "mapi", "%;\n");
        TWOINDX(F, "", "dw", "dloc", 0, "dsiz", "dsiz", "(j+nca)", "i", 
                "%=dw%(ix%)%;\n");
        efprintf(F, Lang->stmt_for_e, "330");
        ONEINDX(F, "", "rw", "rhs", "i", "%=");
        efprintf(F, "%r%;\n",0.0);

        /* build RHS1 */
        efprintf(F, Lang->stmt_for_b, "340", "j", lowlmt, uplmtndes);
        efprintf(F, "mapj%=mapr%(nreq+j%)%;\n");
        ONEINDX(F, "", "rw", "rhs", "i", "%=");
        ONEINDX(F, "", "rw", "rhs", "i", "+");
        TWOINDX(F, "", "w", "", 1, "nr", "nc", "mapj", "mapi", 
                "*b%(mapj%)%;\n");
        efprintf(F, Lang->stmt_for_e, "340");
        efprintf(F, Lang->stmt_for_e, "350");

        /* build D22 (0) and RHS2 */
        efprintf(F, Lang->stmt_for_b, "420", "i", lowlmt, uplmtnreq);
        efprintf(F, Lang->stmt_for_b, "410", "j", lowlmt, uplmtnreq);
        TWOINDX(F, "", "dw", "dloc", 0, "dsiz", "dsiz", "(i+nca)", "(j+nca)", 
                "%=");
        efprintf(F, "%r%;\n",0.0);
        efprintf(F, Lang->stmt_for_e, "410");
        ONEINDX(F, "", "rw", "rhs", "(i+nca)", "%=b%(mapr%(i%)%)%;\n");
        efprintf(F, Lang->stmt_for_e, "420");

        /* Now build DD' to ensure a least squares solution. */
        efprintf(F, Lang->stmt_for_b, "530", "i", lowlmt, uplmtdsiz);
        ONEINDX(F, "", "iw", "map", "i", "%=i%;\n");
        efprintf(F, Lang->stmt_for_b, "520", "j", "i", uplmtdsiz);
        TWOINDX(F, "ix%=", "", "ddt", 0, "dsiz", "dsiz", "i", "j", "%;\n");
        efprintf(F, "dw%(ix%)%=%r%;\n", 0.0);
        efprintf(F, Lang->stmt_for_b, "510", "k", lowlmt, uplmtdsiz);
        TWOINDX(F, "dw%(ix%)%=dw%(ix%)+", 
                   "dw", "dloc", 0, "dsiz", "dsiz", "i", "k", "*");
        TWOINDX(F, "", "dw", "dloc", 0, "dsiz", "dsiz", "j", "k", "%;\n");
        efprintf(F, Lang->stmt_for_e, "510");
        TWOINDX(F, "", "dw", "ddt", 0, "dsiz", "dsiz", "j", "i", 
                "%=dw%(ix%)%;\n");
        efprintf(F, Lang->stmt_for_e, "520");
        efprintf(F, Lang->stmt_for_e, "530");

        /* solve for y */
        efprintf(F, "%:Ps\
%s%Aqrslv(dsiz,dsiz,dsiz,dsiz,%Piw%(map%),%Piw%(map%),%&\
tol,%Piw%(jpvt%),%Prw%(qraux%),%Prw%(work%),%Piw%(iwork%),%&\
%Pdw%(ddt%),%Prw%(rhs%),%Prw%(soln%),%Prank)%;\n",
                 Lang->ref, Lang->proc_call);

        /* compute x = A'y */
        efprintf(F, Lang->stmt_for_b, "620", "i", lowlmt, uplmtc);
        efprintf(F, "mapi%=mapc%(i%)%;\n");
        efprintf(F, "x%(mapi%)%=%r%;\n",0.0);
        efprintf(F, Lang->stmt_for_b, "610", "j", lowlmt, uplmtdsiz);
        TWOINDX(F, "x%(mapi%)%=x%(mapi%)+", 
                   "dw", "dloc", 0, "dsiz", "dsiz", "j", "i", "*");
        ONEINDX(F, "", "rw", "soln", "j", "%;\n");
        efprintf(F, Lang->stmt_for_e, "610");
        efprintf(F, Lang->stmt_for_e, "620");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->proc_end);
}
