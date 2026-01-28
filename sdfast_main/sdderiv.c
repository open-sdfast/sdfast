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
#include "sderror.h"
#include "../calc/gencode.h"

static void ordern_inward_pass(FILE *F, int flags, struct tSymTabEntry *tau, 
                               struct tSymTabEntry *z1, struct tSymTabEntry *z2, 
                               struct tExprNode *z1x, struct tExprNode *z2x, 
                               struct tSymTabEntry *eps);

static void ordern_outward_pass(FILE *F, struct tSymTabEntry *eps, 
                                struct tSymTabEntry *a1, struct tSymTabEntry *a2, 
                                struct tSymTabEntry *k1, struct tSymTabEntry *k2);

static void print_comments(FILE *F);

static void doww_head(FILE *F, int nxtaux, mfrcsym_t *syms, 
               struct tSymTabEntry **dfs, struct tSymTabEntry **deps, 
               struct tSymTabEntry **dZ1, struct tSymTabEntry **dZ2);

static void doww_tail(FILE *F);

#define CONST_PER_FILE        20

/* PRINT_SDDOWW
 *
 * Computes the constraint matrices pp and dpp and the constraint
 * coefficient matrix ww = pp*dpp.  This is then decomposed using
 * a QR decomposition IN PLACE so that ww, qraux, and jpvt together
 * described the QR decomposition and pivoting of the original
 * ww matrix.
 *
 * The number of apparently non-redundant constraints encountered is
 * returned so that operation counts 
 * can be correctly computed for the appropriate sized QR decomposition.
 *
 * The matrix WW that we're building here is 
 *                -1 T
 *        WW = A M  A
 *
 * This matrix will be used later (in sdderiv) to solve for the 
 * multipliers m in the equation
 *
 *                  -1
 *        WW m = A M  f  + b + <Baumgarte stabilization stuff>
 *
 * Note that just because A and M appear in the above equations does
 * not necessarily mean they get formed explicitly!  But unfortunately
 * we really do have to form and factor WW.
 */
void PRINT_SDDOWW(FILE      *mainF,
                  int        maxaux,
                  char      *dynname,
                  int       *nxtaux,
                  opstats_t *opcnt,
                  int       *nonred)
{
    int c,k;
    long ladd, lmul, ldiv, lasg;
    sym  dfs,deps,dZ1,dZ2;
    mfrcsym_t syms;
#        define DFK        syms.fk_    
#        define DTK        syms.tk_    
#        define DTAU        syms.tau_   
    expr multx,dfkx,dtkx,dtaux,dfsx;
    char str_0[10], str_2[10], str_n1[10], str_s1[10], str_nq1[10], str_nc1[10];
    char str_lab[10], str_lab2[10], str_lab3[10], str_nindepc1[10];
    int nasg, nxtlab = 100; /* next label number to use for FORTRAN */
    int nindepc=0;             /* # of independent constraints seen so far */
    int naux, constper;
    FILE *F;

    START_COMPSUB(opcnt);
    ladd = lmul = ldiv = lasg = 0; /* locally printed operation counts */

    esprintf(str_0, "%@d", 0);
    esprintf(str_2, "%@d", 2);
    esprintf(str_n1, "%@d", SysI.n-1);
    esprintf(str_s1, "%@d", SysI.s-1);
    esprintf(str_nq1, "%@d", SysI.nq-1);
    esprintf(str_nc1, "%@d", SysI.nc-1);
    esprintf(str_lab, "%d", nxtlab);

    /* Calculate how many auxiliary files to generate and when to switch
     * files.  Assume the first CONST_PER_FILE iterations generate code into the
     * current file mainF.  Then an auxiliary will be generated for each
     * CONST_PER_FILE more iterations, and for the fraction left over.
     * That tells us how many aux files there will be (but don't go over
     * maxaux).  Then compute the actual number of constraints per file for all
     * but the last file.
     */
    
    naux = SysI.nc/CONST_PER_FILE;        /* OK if SysI.nc not exact multiple */
    if (naux && naux*CONST_PER_FILE == SysI.nc)        /* correct for the exact case */
        naux--;
    if (naux > maxaux)
        naux = maxaux;
    constper = SysI.nc/(naux+1);
    if (constper*(naux+1) != SysI.nc)
        constper++;

    F = mainF;

    /* print heading of main subroutine */
    doww_head(F, -1, &syms, &dfs, &deps, &dZ1, &dZ2);

    /* Now calculate the influence of the multipliers, one at a time. 
     * This puts us back in ST_STATEREADY state since the accelerations
     * calculated by sdrhs() earlier are no longer valid.
     */
    ISET("roustate", ST_STATEREADY);

    IF("wwflg", EQ, "0")
    THEN

    if (SysI.nc == 0)
        goto alldone;

    /* Now create PP and DPP matrices using one of two methods:
     *
     * Using Vpk's and mass matrix (Kane's formulation)
     *    This is an O(nc*(n**2)) calculation.
     *
     *    Assume mm = mlo * mdi * TR(mlo)  (LDU decomposition of mm available)
     *           Vpk, Wpk available
     *
     *    for (c=0; c<SysI.nc; c++) {
     *        mult[c] = 1, other mults = 0
     *        sdmfrc(mult)  -- calculate mfk,mtk,mtau for constraint c
     *
     *        fs[p] = mtau[p] + SUM(Vpk[p,k]*mfk[k] + Wpk[p,k]*mtk[k]), all p
     *        if (all fs[p] == 0)
     *            continue           -- redundant constraint
     *        row     = INV(mlo)*fs  -- back solve lower for column of PP
     *        dinvrow = INV(mdi)*row -- back solve diagonal for row of DPP
     *        PP[k][c]  = row[k], all k
     *        DPP[c][k] = dinvrow[k], all k
     *    }
     *
     * Order(N) formulation inward pass 
     *    This is an O(nc*n) calculation.
     *
     *    Assume rikt[k] = TILDA(rik[k])*Cik[k]
     *           DD,G1,G2 see COMPUTE_ordern_mm
     *
     *    for (c=0; c<SysI.nc; c++) {
     *        mult[c] = 1, other mults = 0
     *        sdmfrc(mult)  -- calculate mfk,mtk,mtau for constraint c
     *
     *        Z1[k] = mfk[k], Z2[k] = mtk[k], all k, shifted to inb hinge pt
     *                                               of pseudobody set k
     *        
     *        for (k=s-1; k>=0; k--) {
     *            eps[k] = mtau[k] + (Vkk[k]*Z1[k] + Wkk[k]*Z2[k])
     *            i = inb(k)
     *            if (i == ground)
     *                continue
     *            Z1[k] += G1[k]*eps[k]
     *            Z2[k] += G2[k]*eps[k]
     *            Z1[i] += Cik[k]*Z1[k]
     *            Z2[i] += rikt[k]*Z1[k] + Cik[k]*Z2[k] 
     *        }
     *        if (all eps[k] == 0)
     *            -- redundant constraint
     *        else
     *            PP[k][c] = eps[k]
     *            DPP[c][k] = DD[k]*eps[k]
     *    }
     */

    efprintf(F, "%{\nCompute constraint effects\n%}");
    if (sdfast_opt.formulation == OPT_KANE) 
        CALL0("%Adovpk");

    if (sdfast_opt.formulation == OPT_ORDERN)
        CALL("%Adomm(routine)") /* no semicolon here */
    else
        CALL("%Adommldu(routine)");

    multx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nc));
    for (k=0; k<SysI.nc; k++)
        SINDX(multx, k, SCALAR_ZERO());

    for (c = 0; c < SysI.nc; c++) {
        int redundant;

        if (c && ((c % constper) == 0)) {
            /* time to switch files */
            CALL1("%Adoww%02d(routine,pp,dpp)", *nxtaux);
            if (F != mainF) {
                doww_tail(F);
                CLOSE_FILE(F);
            } else 
                efprintf(mainF, "%<%<"); /* reset indentation to left edge */
            if (openaux(&F, dynname, *nxtaux)) {
                efprintf(mainF, "%>%>"); /* restore indentation level */
                return;
            }
            doww_head(F, *nxtaux, &syms, &dfs, &deps, &dZ1, &dZ2);
            (*nxtaux)++;
        }

        efprintf(F, "%{\nConstraint %@d (%s)\n%}", c,
            c < SysI.np ? "prescribed motion" : 
            c < SysI.np+SysI.nlc ? "loop constraint" : 
                                   "user constraint");
        
        dfkx  = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
        dtkx  = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
        dtaux = INUSE(NEW_1dARRAY(cScalarVal, SysI.s?SysI.s:1));
        SINDX(multx, c, SCALAR_ONE());
        COMPUTE_mfrc(F, multx, &syms, dfkx, dtkx, dtaux, &nasg, &nxtlab);
        ASSIGN_CLN(F, DFK, UNUSE(dfkx));   
        ASSIGN_CLN(F, DTK, UNUSE(dtkx));  
        if (SysI.s)
            ASSIGN_CLN(F, DTAU, UNUSE(dtaux));
        else DISPOSE_EXPR(UNUSE(dtaux));
        esprintf(str_lab, "%d", nxtlab);
        SINDX(multx, c, SCALAR_ZERO());
        lasg += nasg;

        if (sdfast_opt.formulation != OPT_ORDERN) {
            if (c < SysI.np+SysI.nlc) {
                dfsx  = INUSE(NEW_1dARRAY(cScalarVal, SysI.s?SysI.s:1));
                COMPUTE_fsmult(F, DFK, DTK, DTAU, dfs, dfsx);
                for (redundant=1, k = 0; redundant && k < SysI.s; k++)
                    redundant = IS_ZERO(INDX(dfsx, k));
                if (!redundant) {
                    /* constant elements of dfs haven't been printed yet */
                    for (k = 0; k < SysI.s; k++)
                        if (IS_CONST(INDX(dfsx,k)))
                            PRINT_ASSN1(F, PRINTNAME(dfs), k, INDX(dfsx, k));
                    efprintf(F, "%s%Aldubsl(%d,%d,mmap,mlo,%s,row)%;\n", 
                             Lang->proc_call, SysI.s, SysI.s, PRINTNAME(dfs));
                    opcnt->nldubsl++;
                }
                DISPOSE_EXPR(UNUSE(dfsx));
            } else {
                /* user constraint
                 * Set global mfk, mtk, mtau to the just-computed dfk, etc. and
                 * print out all the non-constant elements of the forces so that
                 * the symbols mfk, mtk and mtau are up to date for the
                 * upcoming sdfsmult() numerical reference.
                 */
                ASSIGN(mfk, VAL(DFK));
                ASSIGN(mtk, VAL(DTK));
                ASSIGN(mtau, VAL(DTAU));
                CLEANVAR(F, mfk, CL_FLUSHNONCONST, CL_FORGET);
                CLEANVAR(F, mtk, CL_FLUSHNONCONST, CL_FORGET);
                CLEANVAR(F, mtau, CL_FLUSHNONCONST, CL_FORGET);
                redundant = 0; /* we can't know at generate time ... */
                CALL0("%Afsmult");
                opcnt->nfsmult++;
                efprintf(F, "%s%Aldubsl(%d,%d,mmap,mlo,%s,row)%;\n", 
                            Lang->proc_call, SysI.s, SysI.s, PRINTNAME(fs));
                opcnt->nldubsl++;
            }

            if (!redundant) {
                efprintf(F, "%s%Aldubsd(%d,%d,mmap,mdi,row,dinvrow)%;\n", 
                            Lang->proc_call, SysI.s, SysI.s);
                opcnt->nldubsd++;
                FOR(str_lab, "i", str_0, str_s1);
                    efprintf(F, "pp%(%@d%,i%)%=row%(i%)%;\n", c);
                    efprintf(F, "dpp%(i%,%@d%)%=dinvrow%(i%)%;\n", c);
                ENDFOR(str_lab);
                lasg += 2*SysI.s;
                esprintf(str_lab, "%d", ++nxtlab);
                efprintf(F, "wmap%(%@d%)%=%@d%;\n",nindepc,c);
                nindepc++;
            } else {
                efprintf(F, "%{... redundant%}");
            }

        } else { /* Order(N) */
            expr dZ1x, dZ2x;
            int b;

            /* initialize Z1 and Z2 */
            dZ1x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
            dZ2x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
            for (k = 0; k < SysI.s; k++) {
                SINDX(dZ1x, k, VECTOR_ZERO());
                SINDX(dZ2x, k, VECTOR_ZERO());
            }
            for (b = 0; b < SysI.n; b++) {
                if ((k = SysI.LastDOF[b]) < 0)        /* ignore body welded to gnd */
                    continue;

                /* Note that rcom[b]-psrk[k] (body b is in pseudobody
                 * set k) is the vector from the head pseudobody's inboard
                 * hinge point to body b's COM, where mfk[b] is applied.
                 */
                SINDX(dZ1x, k, SUB(INDX(dZ1x, k), VAL1(DFK, b)));
                SINDX(dZ2x, k, SUB(INDX(dZ2x, k),
                                   ADD(VAL1(DTK, b),
                                       CROSS(SUB(VAL1(SysI.rcom,b),
                                                 VAL1(SysI.psrk,k)),
                                             VAL1(DFK, b)))));
            }
            /* calculate deps */
            ordern_inward_pass(F,1,DTAU,dZ1,dZ2,dZ1x,dZ2x,deps);        
            DISPOSE_EXPR(UNUSE(dZ1x));
            DISPOSE_EXPR(UNUSE(dZ2x));

            for (redundant=1, k = 0; redundant && k < SysI.s; k++)
                redundant = IS_ZERO(VAL1(deps, k));

            if (!redundant) {
                FOR(str_lab, "i", str_0, str_s1);
                    efprintf(F, "pp%(%@d%,i%)%=%s%(i%)%;\n", c,
                             PRINTNAME(deps));
                    efprintf(F, "dpp%(i%,%@d%)%=%s%(i%)*%s%(i%)%;\n", c,
                             PRINTNAME(DD),PRINTNAME(deps));
                ENDFOR(str_lab);
                lasg += 2*SysI.s;
                lmul += SysI.s;
                esprintf(str_lab, "%d", ++nxtlab);
                efprintf(F, "wmap%(%@d%)%=%@d%;\n",nindepc,c);
                nindepc++;
            } else {
                efprintf(F, "%{... redundant%}");
            }

        }
    }

    fflush(F);

    if (F != mainF) {
        doww_tail(F);
        CLOSE_FILE(F);
        efprintf(mainF, "%>%>"); /* restore indentation */
    }

    F = mainF;

    if (nindepc) {
        efprintf(F, "%{\nProduce constraint coefficient matrix WW\n%}");

        esprintf(str_nindepc1, "%@d", nindepc-1);
        esprintf(str_lab2, "%d", ++nxtlab);
        esprintf(str_lab3, "%d", ++nxtlab);
        FOR(str_lab3, "c", str_0, str_nindepc1);
            FOR(str_lab2, "i", "c", str_nindepc1);
                RSET("sum", 0.);
                FOR(str_lab, "j", str_0, str_s1);
                    SET("sum","sum+pp%(wmap%(c%)%,j%)*dpp%(j%,wmap%(i%)%)");
                ENDFOR(str_lab);
                SET("ww%(wmap%(c%)%,wmap%(i%)%)","sum");
                SET("ww%(wmap%(i%)%,wmap%(c%)%)","sum");
            ENDFOR(str_lab2);
        ENDFOR(str_lab3);
        esprintf(str_lab, "%d", ++nxtlab);
        lasg += (SysI.s+3)*(nindepc*(nindepc+1)/2);
        ladd += SysI.s*(nindepc*(nindepc+1)/2);
        lmul += SysI.s*(nindepc*(nindepc+1)/2);

        efprintf(F, "%{\nForm QR decomposition of WW\n%}");

        efprintf(F, "%s%Aqrdcomp(%d,%d,%d,%d,wmap,wmap,ww,qraux,jpvt)%;\n",
              Lang->proc_call, SysI.nc, SysI.nc, nindepc, nindepc);
        opcnt->nqrdcomp++;
    } else 
        efprintf(F, "%{\nNo independent constraints were present\n%}");

    alldone: 

    ISET("wwflg", 1);
    ENDIF;
    fflush(F);

    *nonred = nindepc;

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,ladd,lmul,ldiv,lasg);
    efprintf(F, Lang->proc_end);
}

/* doww_head
 *
 * Print out the subroutine declaration and appropriate local declarations
 * for one of the subroutines used by SDDOWW().  The subroutine name is
 * SDDOWWnn() where nn is the auxiliary file number.  If the auxno is passed
 * in less than 0, we just generate SDDOWW(), the "main" routine.
 *
 * The file F should be the one named to match auxno.
 */
static void
doww_head(FILE      *F,
          int       auxno,
          mfrcsym_t *syms,
          sym       *dfs,
          sym       *deps,
          sym       *dZ1,
          sym       *dZ2)
{
    sym umult,dfk,dtk,dtau;
    sym dlfci,dlfc,dltc,dltci,dTinb,dToutb;
    sym dltaufi, dltaufo, dltauti, dltauto;
    char rouname[100];

    /* Declare the SDDOWW routine heading. */
    if (auxno >= 0) {
        auxnoname(auxno, "doww", 100, rouname);
        declare_proc(F, 0, rouname,
          VT_INTEGER, "routine",
          VT_USER, &SysI.type_Arr_nc_s,                "pp", 
          VT_USER, &SysI.type_Arr_s_nc,                "dpp", 
          0);
    } else
        declare_proc(F, 0, "doww",
          VT_INTEGER, "routine",
          0);

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    if (SysI.nc) {
        if (auxno < 0)
            declare_vars(F, 0, 
              VT_USER, &SysI.type_Arr_nc_s,                "pp", 
              VT_USER, &SysI.type_Arr_s_nc,                "dpp", 
              0);
        declare_vars(F, 0, 
          VT_INTEGER,                                 "i",
          VT_DUP,                                     "j",
          VT_DUP,                                     "c",
          VT_REAL,                                       "sum", 
          0);
        declare_vars(F, 0,
          VT_USER|VT_DSYM, &SysI.type_Vec_n,  "dfk",    &dfk,
          VT_DUP|VT_DSYM,                     "dtk",    &dtk,
          VT_USER|VT_DSYM, &SysI.type_Arr_s,  "dtau",   &dtau,
          VT_USER|VT_DSYM, &SysI.type_Vec_nl, "dltci",  &dltci,
          VT_DUP|VT_DSYM,                     "dltc",   &dltc,
          VT_DUP|VT_DSYM,                     "dlfci",  &dlfci,
          VT_DUP|VT_DSYM,                     "dlfc",   &dlfc,
          0);  
        declare_vars(F, 0,
          VT_USER|VT_DSYM, &SysI.type_Vec_nl, "dTinb",  &dTinb,
          VT_DUP|VT_DSYM,                     "dToutb", &dToutb,
          VT_DUP|VT_DSYM,                     "dltaufi", &dltaufi,
          VT_DUP|VT_DSYM,                     "dltaufo", &dltaufo,
          VT_DUP|VT_DSYM,                     "dltauti", &dltauti,
          VT_DUP|VT_DSYM,                     "dltauto", &dltauto,
          0);  
        if (SysI.nu)
          declare_vars(F, 0,
            VT_ARRAY|VT_DSYM, "umult", SysI.nu, 0, &umult,
            0);

        if (sdfast_opt.formulation == OPT_ORDERN) 
            declare_vars(F, 0, 
              VT_USER|VT_DSYM, &SysI.type_Arr_s,          "deps", deps,
              VT_USER|VT_DSYM, &SysI.type_Vec_s,          "dZ1",         dZ1,
              VT_DUP|VT_DSYM,                                      "dZ2",  dZ2,
              0);
        else 
            declare_vars(F, 0, 
              VT_USER|VT_DSYM, &SysI.type_Arr_s,          "dfs",  dfs,
              VT_DUP,                                         "row", 
              VT_DUP,                                              "dinvrow",
              0);

        syms->umult_ = umult;
        syms->lfci_  = dlfci;
        syms->lfc_   = dlfc;
        syms->ltc_   = dltc;
        syms->ltci_  = dltci;
        syms->Tinb_  = dTinb;
        syms->Toutb_ = dToutb;
        syms->ltaufi_ = dltaufi;
        syms->ltaufo_ = dltaufo;
        syms->ltauti_ = dltauti;
        syms->ltauto_ = dltauto;
        syms->tmpv1_ = tvec1;
        syms->tmpv2_ = tvec2;
        syms->tmpv3_ = tvec3;
        syms->fk_    = dfk;
        syms->tk_    = dtk;
        syms->tau_   = dtau;
    }

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }
}

/* domm_tail
 *
 * Terminate the SDDOWWnn() subroutine. 
 */
static void
doww_tail(FILE *F)
{
    efprintf(F, Lang->proc_end);
    fflush(F);
}

/* PRINT_SDUDOT0
 *
 * Generate the sdudot0() subroutine (and internal utility routine sdxudot0()).
 * The number of explicit calls made to various routines is returned in opcnt.
 * The opcounts for these must be added in by the caller.
 *
 * We also output the routine sdsetudot() here which allows a user to
 * compute his own udot and then get us to compute all the downstream
 * stuff, like aerr's, etc. via the internal sdrhs().
 *
 * sdudot0() solves the dynamics problem ignoring constraints:
 *                -1 
 *       udot0 = M  f
 *
 * Here f is the hinge-torque equivalent of all the applied and inertial
 * forces (but excluding constraint forces). 
 *
 * So here is the complete algorithm:
 *                -1
 *    1. compute M   or the O(N) equivalent
 *    2. compute f  (we call this fs0 below)
 *    3. solve
 *                    -1
 *           udot0 = M  f
 *        
 * This routine leaves us in stage ST_STATEREADY, with the global "udot"
 * symbol set to udot0.  If you want to get to ST_DERIVREADY with udot0 as 
 * the accelerations, just call sdsetudot(udot0) after sdudot0().
 */
void PRINT_SDUDOT0(FILE *F, opstats_t *opcnt)
{
    int k;
    char str_0[10], str_s1[10];
    long ladd,lmul,ldiv,lasg;
    sym eps,Z1,Z2,A1,A2,K1,K2;

    esprintf(str_0, "%@d", 0);
    esprintf(str_s1, "%@d", SysI.s-1);

    START_COMPSUB(opcnt);
    ladd = lmul = ldiv = lasg = 0; /* locally printed operation counts */

    /* Declare the SDXUDOT0 routine heading. */
    declare_proc(F, 0, "xudot0",
      VT_INTEGER,                      "routine",
      VT_USER,    &SysI.type_Arr_s,  "oudot0",
      0);

    efprintf(F, "%{\nCompute unconstrained equations\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      0);

    if (sdfast_opt.formulation == OPT_ORDERN)
        declare_vars(F, 0, 
          VT_USER|VT_DSYM, &SysI.type_Arr_s, "eps", &eps,
          VT_USER|VT_DSYM, &SysI.type_Vec_s, "Z1",  &Z1,
          VT_DUP|VT_DSYM,                    "Z2",  &Z2,
          VT_DUP|VT_DSYM,                    "A1",  &A1,
          VT_DUP|VT_DSYM,                    "A2",  &A2,
          VT_DUP|VT_DSYM,                    "K1",  &K1,
          VT_DUP|VT_DSYM,                    "K2",  &K2,
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    CALL("%Alhs(routine)");
    opcnt->nlhs++;

    if (SysI.nc)
        efprintf(F, "%{\nSolve equations ignoring constraints\n%}");
    else 
        efprintf(F, "%{\nSolve equations for udots\n%}");

    if (sdfast_opt.formulation != OPT_ORDERN) {
        CALL0("%Afs0");
        opcnt->nfs0++;

        efprintf(F, "%s%Aldubslv(%d,%d,mmap,works,mlo,mdi,fs,udot)%;\n", 
                    Lang->proc_call,SysI.s,SysI.s);
        opcnt->nldubsl += 2;
        opcnt->nldubsd++;
    } else {
        expr Z1x,Z2x;

        Z1x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
        Z2x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
        for (k=0; k < SysI.s; k++) {
            SINDX(Z1x, k, VECTOR_ZERO());
            SINDX(Z2x, k, VECTOR_ZERO());
        }
        ordern_inward_pass(F,0,fs0,Z1,Z2,Z1x,Z2x,eps);        /* calculate eps */
        DISPOSE_EXPR(UNUSE(Z1x));
        DISPOSE_EXPR(UNUSE(Z2x));

        ordern_outward_pass(F,eps,A1,A2,K1,K2);         /* calculate udot */
    }

    FOR("5", "i", str_0, str_s1);
      SET("oudot0%(i%)","udot%(i%)");
    ENDFOR("5");
    lasg += SysI.s;

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,ladd,lmul,ldiv,lasg);
    efprintf(F, Lang->proc_end);

    /* Declare the SDUDOT0 routine heading. */
    declare_proc(F, 0, "udot0",
      VT_USER,    &SysI.type_Arr_s,  "oudot0",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdudot0, ERR_sdstateMustBeCalledFirst);

    CALL1("%Axudot0(%d,oudot0)", ROU_sdudot0);
    
    efprintf(F, Lang->proc_end);

    /* Declare the SDSETUDOT routine heading. */
    declare_proc(F, 0, "setudot",
      VT_USER,    &SysI.type_Arr_s,  "iudot",
      0);

    efprintf(F, "%{\nAssign udots and advance to stage Dynamics Ready\n%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      0);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdsetudot, ERR_sdstateMustBeCalledFirst);

    FOR("5", "i", str_0, str_s1);
      SET("udot%(i%)","iudot%(i%)");
    ENDFOR("5");

    CALL0("%Arhs");                        /* SDRHS()        */
    
    efprintf(F, Lang->proc_end);

    fflush(F);
}

/* PRINT_SDUDOTM
 *
 * Generate the sdudotm() subroutine (and the internal utility
 * routine sdxudotm()).  The number of explicit calls made to various 
 * routines is returned in opcnt.  The opcounts for these must be 
 * added in by the caller.
 *
 * sdudotm() returns udots which would be produced by the passed-in
 * multipliers, ignoring applied, inertial and gravity forces.
 *
 * The equation we're solving is:
 *
 *                -1  T
 *       udotm = M  (A m)
 *
 * Here is the complete algorithm:
 *                -1
 *    1. compute M   or the O(N) equivalent
 *                T
 *    2. compute A m (via sdmfrc() and sdfsmult() or equivalent)
 *    3. solve for udotm
 *
 * If there are no constraints, this routine just returns 0 for the
 * udots.
 *        
 * This routine leaves us in stage ST_STATEREADY, with the global "udot"
 * symbol set to udotm.  If you want to get to ST_DERIVREADY with udotm as 
 * the accelerations, just call sdrhs() after sdudotm().
 */
void PRINT_SDUDOTM(FILE *F, opstats_t *opcnt)
{
    int k;
    char str_0[10], str_s1[10];
    long ladd,lmul,ldiv,lasg;
    sym eps,Z1,Z2,A1,A2,K1,K2;

    esprintf(str_0, "%@d", 0);
    esprintf(str_s1, "%@d", SysI.s-1);

    START_COMPSUB(opcnt);
    ladd = lmul = ldiv = lasg = 0; /* locally printed operation counts */

    /* Declare the SDXUDOTM routine heading. */
    declare_proc(F, 0, "xudotm",
      VT_INTEGER,                      "routine",
      VT_USER,    &SysI.type_Arr_nc, "imult",
      VT_USER,    &SysI.type_Arr_s,  "oudotm",
      0);

    efprintf(F, "%{\nCompute udots due only to multipliers\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      0);

    if (sdfast_opt.formulation == OPT_ORDERN)
        declare_vars(F, 0, 
          VT_USER|VT_DSYM, &SysI.type_Arr_s, "eps", &eps,
          VT_USER|VT_DSYM, &SysI.type_Vec_s, "Z1",  &Z1,
          VT_DUP|VT_DSYM,                    "Z2",  &Z2,
          VT_DUP|VT_DSYM,                    "A1",  &A1,
          VT_DUP|VT_DSYM,                    "A2",  &A2,
          VT_DUP|VT_DSYM,                    "K1",  &K1,
          VT_DUP|VT_DSYM,                    "K2",  &K2,
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    /* We'll call SDLHS() here just to make sure we have a mass
     * matrix to work with, but we won't count the call in the
     * opcnt array because we don't want this to get counted 
     * twice for SDDERIV().  SDLHS() would have just been called
     * by SDUDOT0(), so it won't do anything when called again
     * here.
     */
    CALL("%Alhs(routine)");

    if (SysI.nc == 0) {
        FOR("5", "i", str_0, str_s1);
          RSET("udot%(i%)", 0.);
        ENDFOR("5");
        lasg += SysI.s;

        goto alldone;
    }

    CALL("%Amfrc(imult)");                /* SDMFRC(IMULT) */
    opcnt->nmfrc++;

    if (sdfast_opt.formulation != OPT_ORDERN) {
        CALL0("%Afsmult");                        /* SDFSMULT()   */
        opcnt->nfsmult++;
        efprintf(F, "%s%Aldubslv(%d,%d,mmap,works,mlo,mdi,fs,udot)%;\n", 
                    Lang->proc_call,SysI.s,SysI.s);
        opcnt->nldubsl += 2;
        opcnt->nldubsd++;
    } else {
        expr Z1x, Z2x;
        int b;

        /* initialize Z1 and Z2 */
        Z1x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
        Z2x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s?SysI.s:1));
        for (k = 0; k < SysI.s; k++) {
            SINDX(Z1x, k, VECTOR_ZERO());
            SINDX(Z2x, k, VECTOR_ZERO());
        }
        for (b = 0; b < SysI.n; b++) {
            if ((k = SysI.LastDOF[b]) < 0)        /* ignore body welded to gnd */
                continue;

            /* Note that rcom[b]-psrk[k] (body b is in pseudobody
             * set k) is the vector from the head pseudobody's inboard
             * hinge point to body b's COM, where mfk[b] is applied.
             */
            SINDX(Z1x, k, SUB(INDX(Z1x, k), VAL1(mfk, b)));
            SINDX(Z2x, k, SUB(INDX(Z2x, k),
                              ADD(VAL1(mtk, b),
                                  CROSS(SUB(VAL1(SysI.rcom,b),
                                            VAL1(SysI.psrk,k)),
                                        VAL1(mfk, b)))));
        }
        /* calculate eps */
        ordern_inward_pass(F,0,mtau,Z1,Z2,Z1x,Z2x,eps);        
        DISPOSE_EXPR(UNUSE(Z1x));
        DISPOSE_EXPR(UNUSE(Z2x));

        ordern_outward_pass(F,eps,A1,A2,K1,K2);         /* calculate udot */
    }

  alldone:

    FOR("10", "i", str_0, str_s1);
      SET("oudotm%(i%)","udot%(i%)");
    ENDFOR("10");
    lasg += SysI.s;

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,ladd,lmul,ldiv,lasg);
    efprintf(F, Lang->proc_end);

    /* Declare the SDUDOTM routine heading. */
    declare_proc(F, 0, "udotm",
      VT_USER,    &SysI.type_Arr_nc, "imult",
      VT_USER,    &SysI.type_Arr_s,  "oudotm",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdudotm, ERR_sdstateMustBeCalledFirst);

    CALL1("%Axudotm(%d,imult,oudotm)", ROU_sdudotm);
    
    efprintf(F, Lang->proc_end);

    fflush(F);
}

/* PRINT_SDDERIV
 *
 * Generate the sdderiv() subroutine.
 * The number of explicit calls made to various routines is returned in opcnt.
 * The opcounts for these must be added in by the caller.
 *
 * nindepc says how many apparently independent constraints there are.
 *
 * sdderiv() returns qdot and udot and also calculates the multipliers.
 * Multipliers can be accessed with routine sdmult(), but they are 
 * calculate here.
 *
 * qdot is actually calculated by sdstate().  It is only returned here
 * as a convenience.  For reference, 
 *
 *      qdot = B u + <Baumgarte stabilization stuff>
 *
 * where B is a matrix which is diagonal for non-ball joints, and has
 * 4x3 blocks where the ball joints are.
 *
 * The multiplers m and accelerations udot are calculated here using the
 * following equations:
 *                 T
 *       M udot - A m = f
 *                   -1
 *       WW m = -(A M  f  + b + <Baumgarte stabilization stuff>)
 *
 * Here f is the hinge-torque equivalent of all the applied and inertial
 * forces (but excluding constraint forces).  We don't explicitly form
 * b; instead we note that the constraint accelerations errors are
 *
 *       aerr = A udot + b
 *
 * So if we first compute 
 *                -1 
 *       udot0 = M  f
 * then
 *          -1
 *       A M  f + b = aerr, if we set udot=udot0
 *
 * So here is the complete algorithm:
 *                -1
 *    1. compute M   or the O(N) equivalent
 *    2. compute f  (we call this fs0 below)
 *    3. solve
 *                    -1
 *           udot0 = M  f
 *    4. form right hand side
 *           r = -(aerr + <Baumgarte stabilization stuff>)
 *                 -1
 *    5. compute WW
 *    6. solve
 *                 -1
 *           m = WW  r
 *    7. solve
 *                    -1  T
 *           udot1 = M  (A m)
 *
 *    8. return
 *           udot = udot0 + udot1
 *        
 */
void PRINT_SDDERIV(FILE *F,
              opstats_t *opcnt,
              int nindepc)
{
    char str_0[10], str_flt0[10], str_s1[10], str_nq1[10], str_nc1[10];
    char str_nindepc1[10];
    long ladd,lmul,ldiv,lasg;
    sym eps,Z1,Z2,A1,A2,K1,K2;

    esprintf(str_0, "%@d", 0);
    esprintf(str_flt0, "%r", 0.0);
    esprintf(str_s1, "%@d", SysI.s-1);
    esprintf(str_nq1, "%@d", SysI.nq-1);
    esprintf(str_nc1, "%@d", SysI.nc-1);
    esprintf(str_nindepc1, "%@d", nindepc-1);

    START_COMPSUB(opcnt);
    ladd = lmul = ldiv = lasg = 0; /* locally printed operation counts */

    /* Declare the SDDERIV routine heading. */
    declare_proc(F, 0, "deriv",
      VT_USER, &SysI.type_Arr_nq, "oqdot",
      VT_USER, &SysI.type_Arr_s,  "oudot",
      0);
    print_comments(F);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);

    if (SysI.nc)
        declare_vars(F, 0, 
          VT_USER, &SysI.type_Arr_nc,    "workr",
          VT_DUP,                        "bb",
          VT_DUP,                            "b0",
          VT_DUP,                              "v0",
          VT_DUP,                               "p0",
          VT_USER, &SysI.type_IntArr_nc, "iwork",
          0);

    declare_vars(F, 0, 
      VT_INTEGER,                 "i",
      VT_DUP|VT_COND, SysI.nc,        "j",
      VT_USER, &SysI.type_Arr_s, "udot0",
      VT_DUP,                         "udot1", 
      0);

    if (sdfast_opt.formulation == OPT_ORDERN)
        declare_vars(F, 0, 
          VT_USER|VT_DSYM, &SysI.type_Arr_s, "eps", &eps,
          VT_USER|VT_DSYM, &SysI.type_Vec_s, "Z1",  &Z1,
          VT_DUP|VT_DSYM,                    "Z2",  &Z2,
          VT_DUP|VT_DSYM,                    "A1",  &A1,
          VT_DUP|VT_DSYM,                    "A2",  &A2,
          VT_DUP|VT_DSYM,                    "K1",  &K1,
          VT_DUP|VT_DSYM,                    "K2",  &K2,
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdderiv, ERR_sdstateMustBeCalledFirst);

    /* sdderiv() leaves us in state ST_DERIVREADY.  However, this is
     * accomplished by a series of state changes between ST_STATEREADY
     * and ST_DERIVREADY.  A call to sdlhs() or application of new forces
     * directly (see sdppbb()) or through sdmfrc() puts us back in 
     * ST_STATEREADY since the application of new forces trashes the
     * accelerations.  Whenever we call sdrhs() we are put into
     * ST_DERIVREADY.  
     */
    
    /* Make sure stabilization parameters have been supplied if they
     * were entered as stabvel=? or stabpos=? in the input file.
     */
    IFCOND efprintf(F, "stabvelq%s%d", EQ, ISQUESFLG);
      THEN SETERR(F, ROU_sdderiv, ERR_StabvelMustBeSpecified);
    ENDIF;
    IFCOND efprintf(F, "stabposq%s%d", EQ, ISQUESFLG);
      THEN SETERR(F, ROU_sdderiv, ERR_StabposMustBeSpecified);
    ENDIF;

    /* Make sure number of apparently independent constraints is 
     * known numerically.
     */
    ISET("wsiz", nindepc);

    efprintf(F, "%{\nCompute unconstrained equations\n%}");
    CALL1("%Axudot0(%d,udot0)", ROU_sdderiv);
    opcnt->nudot0++;

    CALL0("%Arhs");                /* SDRHS() */
    opcnt->nrhs++;

    if (SysI.nc == 0) {
        ISET("wrank", 0);
        goto alldone;
    }

    /* Now we're going to compute the constraint equation rhs "bb".  This
     * is the acceleration constraint errors plus velocity and position
     * errors fed in via Baumgarte constants.
     */

    CALL("%Aaerr(b0)");                /* SDAERR(B0) */

    if (!IS_ZERO(VAL(SysI.stabvel))) {
        if (SysI.StabVelFlg & ISQUESFLG) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "%s", PRINTNAME(SysI.stabvel));
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
        }
        CALL("%Averr(v0)");        /* SDVERR(V0) */
        if (SysI.StabVelFlg & ISQUESFLG)
            efprintf(F, Lang->stmt_if2_e);
    }
    if (!IS_ZERO(VAL(SysI.stabpos))) {
        if (SysI.StabPosFlg & ISQUESFLG) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "%s", PRINTNAME(SysI.stabpos));
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
        }
        CALL("%Aperr(p0)");        /* SDPERR(P0) */
        if (SysI.StabPosFlg & ISQUESFLG)
            efprintf(F, Lang->stmt_if2_e);
    }

    if (!IS_ZERO(VAL(SysI.stabvel)) || !IS_ZERO(VAL(SysI.stabpos)))
        efprintf(F, "%{\nStabilize constraints using Baumgarte's method\n%}");

    FOR("10", "i", str_0, str_nc1);
      SET("bb%(i%)","-b0%(i%)");
    ENDFOR("10");
    ladd += SysI.nc;
    lasg += SysI.nc;

    if (!IS_ZERO(VAL(SysI.stabvel))) {
        if (SysI.StabVelFlg & ISQUESFLG) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "%s", PRINTNAME(SysI.stabvel));
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
        }
        FOR("20", "i", str_0, str_nc1);
            efprintf(F, "bb%(i%)%=bb%(i%)-");
            if (IS_ONE(VAL(SysI.stabvel)))
                efprintf(F, "v0%(i%)%;\n");
            else {
                PRINT_E(F, VAL(SysI.stabvel));
                efprintf(F, "*v0%(i%)%;\n");
            }
        ENDFOR("20");
        if (SysI.StabVelFlg & ISQUESFLG)
            efprintf(F, Lang->stmt_if2_e);
    }
    ladd += SysI.nc;
    lmul += SysI.nc;
    lasg += SysI.nc;

    if (!IS_ZERO(VAL(SysI.stabpos))) {
        if (SysI.StabPosFlg & ISQUESFLG) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "%s", PRINTNAME(SysI.stabpos));
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
        }
        FOR("30", "i", str_0, str_nc1);
            efprintf(F, "bb%(i%)%=bb%(i%)-");
            if (IS_ONE(VAL(SysI.stabpos)))
                efprintf(F, "p0%(i%)%;\n");
            else {
                PRINT_E(F, VAL(SysI.stabpos));
                efprintf(F, "*p0%(i%)%;\n");
            }
        ENDFOR("30");
        if (SysI.StabPosFlg & ISQUESFLG)
            efprintf(F, Lang->stmt_if2_e);
    }
    ladd += SysI.nc;
    lmul += SysI.nc;
    lasg += SysI.nc;

    efprintf(F, "%{\nCompute and decompose constraint matrix WW\n%}");
    CALL1("%Adoww(%d)", ROU_sdderiv);
    opcnt->ndoww++;

    efprintf(F, "%{\nNumerically solve for constraint multipliers\n%}");

    /* Make sure symbolically-redundant ones get set to zero. */
    if (nindepc != SysI.nc) {
        FOR("75", "i", str_0, str_nc1);
          RSET("mult%(i%)", 0.);
        ENDFOR("75");
        lasg += SysI.nc;
    }

    if (nindepc > 0) {
        efprintf(F, "%s%Aqrbslv(%d,%d,%d,%d,wmap,wmap,%r,workr,iwork,\
%&ww,qraux,jpvt,bb,mult,%swrank)%;\n", 
              Lang->proc_call, SysI.nc, SysI.nc, nindepc, nindepc, 
              cEquationNegligible, Lang->ref);
        opcnt->nqrbslv++;
        FOR("80", "i", str_0, str_nindepc1);
          ISET("multmap%(i%)", 0);
        ENDFOR("80");
        FORCNT("85", "i", "wrank");
          ISET("multmap%(jpvt%(i%)%)", 1);
        ENDFOR("85");
        SET("j", str_0);
        FOR("90", "i", str_0, str_nindepc1);
          IF("multmap%(i%)", NE, "0");
          THEN  
            SET("multmap%(j%)", "wmap%(i%)");
            SET("j", "j+1");
          ENDIF;
        ENDFOR("90");
    } else {
        ISET("wrank", 0);
    }
        
    efprintf(F, "%{\nCompute final udots\n%}");

    CALL1("%Axudotm(%d,mult,udot1)", ROU_sdderiv);
    opcnt->nudotm++;
    
    FOR("95", "i", str_0, str_s1);
      SET("udot%(i%)","udot0%(i%)+udot1%(i%)");
    ENDFOR("95");
    lasg += SysI.s;
    ladd += SysI.s;

    CALL0("%Arhs");                        /* SDRHS()        */
    opcnt->nrhs++;

  alldone:
    /* Return qdots, which are actually calculated elsewhere. */
    FOR("100", "i", str_0, str_nq1);
      SET("oqdot%(i%)","qdot%(i%)");
    ENDFOR("100");
    lasg += SysI.nq;

    /* copy out the udots */
    FOR("110", "i", str_0, str_s1);
      SET("oudot%(i%)","udot%(i%)");
    ENDFOR("110");
    lasg += SysI.s;

    fflush(F);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,ladd,lmul,ldiv,lasg);
    efprintf(F, Lang->proc_end);
}

/* print_comments()
 *
 * Print the sdderiv() initial comments (subroutine heading should
 * already have been printed).
 */
static void
print_comments(FILE *F)
{
    efprintf(F, "%{\n");
    if (SysI.Grounded) {
        efprintf(F, "\
This is the derivative section for a %d-body ground-based\n\
system with %d hinge degree(s) of freedom.\n", SysI.n, SysI.s);
    } else {
        efprintf(F, "\
This is the derivative section for a %d-body free-flying\n\
system with %d degree(s) of freedom, including 6 degrees\n\
of freedom for attitude and translation of the base body.\n",
          SysI.n, SysI.s);
    }
    if (SysI.s_pres) {
        efprintf(F, "\
%d of the degrees of freedom follow(s) prescribed motion.\n", 
                 SysI.s_pres);
        if (SysI.s_runtime)
            efprintf(F, "\
%d additional degrees of freedom may follow prescribed motion.\n", 
                     SysI.s_runtime);
    } else
        if (SysI.s_runtime)
            efprintf(F, "\
%d of the degrees of freedom may follow prescribed motion.\n", 
                     SysI.s_runtime);
    if (SysI.nc == 1)
        efprintf(F, "There is 1 constraint.\n");
    else if (SysI.nc > 1)
        efprintf(F, "There are %d constraints.\n", SysI.nc);
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
}

/* ordern_inward_pass
 *
 * This is the generic symbolic subroutine for the Order(N) inward pass.
 * The goal here is to calculate the "eps" symbol.
 *
 *    Assume rikt[k] = TILDA(rik[k])*Cik[k]
 *           G1,G2,DD as calculated by COMPUTE_ordern_mm.
 *
 *    Z1[k] = Z2[k] = already initialized to appropriate values
 *    
 *    for (k=s-1; k>=0; k--) {
 *        eps[k] = trq[k] - (Vkk[k]*Z1[k] + Wkk[k]*Z2[k])
 *        i = inb(k)
 *        if (i == ground)
 *            continue
 *        Z1[k] += G1[k]*eps[k]
 *        Z2[k] += G2[k]*eps[k]
 *        Z1[i] += Cik[k]*Z1[k]
 *        Z2[i] += rikt[k]*Z1[k] + Cik[k]*Z2[k] 
 *    }
 *
 * On return, the "eps" symbol will have been flushed and ASSIGNed.  
 * Depending on flag setting, it may refer to the temporary symbols
 * Z1 or Z2 (flushall==0) or it will consist only of VREF's and constants
 * (flushall==1).  In the latter case, every element of eps will in addition
 * have been printed so that it may be accessed numerically if desired.
 */

static void
ordern_inward_pass(FILE *F,
                   int flushall,
                   sym trq,
                   sym Z1,
                   sym Z2,
                   expr Z1x,
                   expr Z2x,
                   sym eps)
{
    expr epsx,temp;
    int i,k;

    if (SysI.s == 0)
        return;

    /* computation of eps */
    epsx = INUSE(NEW_1dARRAY(cScalarVal,SysI.s));
    for (k=SysI.s-1; k >= 0; k--) {
        temp = SUB(VAL1(trq,k),
                   ADD(DOT(VAL1(Vkk,k),INDX(Z1x,k)),
                       DOT(VAL1(Wkk,k),INDX(Z2x,k))));
        if (IS_CONST(temp)) {
            SINDX(epsx, k, temp);
            if (flushall)
                PRINT_ASSN1(F, PRINTNAME(eps), k, temp);
        } else {
            PRINT_ASSN1(F, PRINTNAME(eps), k, temp);
            DISPOSE_EXPR(temp);
            SINDX(epsx, k, VREF1(eps, k));
        }

        /* Don't let eps get to this point without having been
         * flushed, unless it is all constant.  Otherwise the
         * reference to eps below in Z2 could contain an out-of-date
         * reference to Z1.
         */

        i = SysI.PseudoBodies[k].jnt.InbBody;
        if (i == cGroundBody)
            continue;
        
        FLUSH_VEC(F,Z1,k,Z1x,ADD(INDX(Z1x,k),MUL(INDX(epsx,k),VAL1(G1,k))));
        FLUSH_VEC(F,Z2,k,Z2x,ADD(INDX(Z2x,k),MUL(INDX(epsx,k),VAL1(G2,k))));

        FLUSH_VEC_NONCONST(F,Z1,i,Z1x,
            ADD(INDX(Z1x,i), MATMUL(VAL1(Cik,k),INDX(Z1x,k))));
        
        FLUSH_VEC_NONCONST(F,Z2,i,Z2x,
            ADD(INDX(Z2x,i), ADD(MATMUL(VAL1(rikt,k),INDX(Z1x,k)),
                                 MATMUL(VAL1(Cik,k),INDX(Z2x,k)))));
    }

    ASSIGN(eps, UNUSE(epsx));        /* already clean */
}

/* ordern_outward_pass
 *
 * This is the generic symbolic subroutine for the Order(N) outward pass.
 * The goal here is to calculate udots.
 *
 *    Assume rikt[k] = TILDA(rik[k])*Cik[k]
 *           G1,G2,DD as calculated by COMPUTE_ordern_mm.
 *           eps as calculate by previous inward pass.
 *
 *    for (k=0; k < s; k++) {
 *        i = inb(k)
 *        if (i == ground) 
 *           udot[k] = DD[k]*eps[k]
 *           A1[k] = Vkk[k]*udot[k]
 *           A2[k] = Wkk[k]*udot[k]
 *        else             
 *           K1[k] = Cikt[k]*A1[i] + TR(rikt[k])*A2[i]
 *           K2[k] = Cikt[k]*A2[i]
 *           udot[k] = DD[k]*eps[k] - (G1[k]*K1[k] + G2[k]*K2[k])
 *           A1[k] = K1[k]+Vkk[k]*udot[k]
 *           A2[k] = K2[k]+Wkk[k]*udot[k]
 *    }
 *
 * The global udot is set for numerical access; the symbol is not assigned.
 * The temporary symbols A1,A2,K1,K2 are not assigned.
 */

static void
ordern_outward_pass(FILE *F,
                    sym eps,
                    sym A1,
                    sym A2,
                    sym K1,
                    sym K2)
{
    expr A1x,A2x,K1x,K2x,temp;
    int i,k;

    if (SysI.s == 0)
        return;

    A1x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s));
    A2x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s));
    K1x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s));
    K2x = INUSE(NEW_1dARRAY(cVectorVal,SysI.s));
    for (k=0; k < SysI.s; k++) {
        i = SysI.PseudoBodies[k].jnt.InbBody;
        if (i == cGroundBody) {
            temp = MUL(VAL1(DD,k),VAL1(eps,k));
            PRINT_ASSN1(F, PRINTNAME(udot), k, temp);
            temp = INUSE(USEXIF(temp, VREF1(udot,k)));
            FLUSH_VEC(F,A1,k,A1x,MUL(temp,VAL1(Vkk,k)));
            FLUSH_VEC(F,A2,k,A2x,MUL(temp,VAL1(Wkk,k)));
        } else {
            FLUSH_VEC(F,K1,k,K1x,
                      ADD(MATMUL(TRANSPOSE(VAL1(Cik,k)),INDX(A1x,i)),
                           MATMUL(TRANSPOSE(VAL1(rikt,k)),INDX(A2x,i))));
            FLUSH_VEC(F,K2,k,K2x,
                      MATMUL(TRANSPOSE(VAL1(Cik,k)),INDX(A2x,i)));
            temp = SUB(MUL(VAL1(DD,k),VAL1(eps,k)),
                       ADD(DOT(VAL1(G1,k),INDX(K1x,k)),
                           DOT(VAL1(G2,k),INDX(K2x,k))));
            PRINT_ASSN1(F, PRINTNAME(udot), k, temp);
            temp = INUSE(USEXIF(temp, VREF1(udot,k)));
            FLUSH_VEC(F,A1,k,A1x,ADD(INDX(K1x,k),
                                 MUL(temp,VAL1(Vkk,k))));
            FLUSH_VEC(F,A2,k,A2x,ADD(INDX(K2x,k),
                                 MUL(temp,VAL1(Wkk,k))));
        }
        DISPOSE_EXPR(UNUSE(temp));
    }
    DISPOSE_EXPR(UNUSE(A1x));
    DISPOSE_EXPR(UNUSE(A2x));
    DISPOSE_EXPR(UNUSE(K1x));
    DISPOSE_EXPR(UNUSE(K2x));
}

/* PRINT_SDMULT
 *
 * Generate the sdmult() subroutine.  This routine simply copies the 
 * global `mult' and `wrank' (w's rank, i.e. the number of
 * independent constraints) variables into its parameters.
 */
void PRINT_SDMULT(FILE *F)
{
    char str_nc[10],str_m1[10],lastwrank[10];

    esprintf(str_nc, "%d", SysI.nc);
    esprintf(str_m1, "%@d", -1);

    /* lastwrank is the index of the last valid element of multmap */
    if (Lang->subs_offset)
        esprintf(lastwrank, "wrank");                /* e.g. FORTRAN */
    else
        esprintf(lastwrank, "wrank-1");                /* e.g. C */

    /* Declare the SDMULT routine heading. */
    declare_proc(F, 0, "mult",
      VT_USER, &SysI.type_Arr_nc, "omults",
      VT_INTEGER|VT_BYREF,          "owrank",
      VT_USER, &SysI.type_IntArr_nc, "omultmap",
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    if (SysI.nc)
        declare_vars(F, 0, 
          VT_INTEGER, "i",
          0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Agstate,%Aglhs do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE, 
                ROU_sdmult, ERR_sdderivMustBeCalledFirst);

    if (SysI.nc) {
        FORCNT("100", "i", str_nc);
          SET("omults%(i%)", "mult%(i%)");
          IF("i", LE, lastwrank);
            THEN SET("omultmap%(i%)", "multmap%(i%)");
            ELSE SET("omultmap%(i%)", str_m1);        /* let's be neat */
          ENDIF;
        ENDFOR("100");
    }

    SETREF("owrank", "wrank");

    fflush(F);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    efprintf(F, Lang->proc_end);
}
