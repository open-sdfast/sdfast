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

#define SDMAIN

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#ifdef apollo
#include <strings.h>
extern char *strchr();
extern char *strrchr();
#else
#include <string.h>
#endif

#ifdef ardent
extern fprintf();
#endif

/* These declarations are for the fussy SGI compiler which
 * can't find these static routines otherwise.
 */
static void adj_stats(opstats_t opstats[],  int which);

/* Location of opcount stats for each routine. */
#define SDSTATE      0
#define SDMOM        1
#define SDSYS        2
#define SDDERIV      3
#define SDRESID      4
#define SDREAC       5
#define SDMFRC       6
#define SDLHS        7
#define SDRHS        8
#define SDLDUDCOMP   9
#define SDLDUBSL    10
#define SDLDUBSD    11
#define SDQRDCOMP   12
#define SDQRBSLV    13
#define SDFS0       14
#define SDFSMULT    15        
#define SDFSFULL    16
#define SDDOWW      17
#define SDDOLTAU    18
#define SDDOINER    19
#define SDDOFS0     20
#define SDDOMM      21
#define SDDOMMLDU   22
#define SDDOVPK     23
#define SDFULLTRQ   24
#define SDUDOT0     25
#define SDUDOTM     26
#define NUM_OP_COUNTS 27

/*======*/
/* main */
/*======*/

int main(int argc, char *argv[])
{
    string32 DateTime;
    opstats_t opstats[NUM_OP_COUNTS];
    FILE *dynF, *infoF, *libF, *sarF, *F;
    extern char VersionNumber[];
    extern int gProgramSerialNo;
    struct language *Lang_save;
    int i,nonred,exitstat,nxtaux;
    expr lqx,lux,lqdotx,ludotx;
    void (*prfunc)(FILE *, char *, ...);

    nxtaux = 0;

    /* Initialize CPU timer. */
    gStartTime = CPU_SECONDS();

    /* Remember start time of day to use as a stamp in _sar and _dyn to
       verify that both were generated at the same time. */
    gGenerateTime = GETNUMTIME();

    /* We use this to quietly determine that a user is trying to get around */
    /* the expiration date.                                                 */
    /* If s/he tries to trick us, this will still be nil after calling      */
    /* PRODUCE_EQUATIONS.                                                   */
    UtoQexpr = NULL;

    /* Zero out all op counts, just to be sure. */
    for (i=0; i<NUM_OP_COUNTS; i++)
        ZERO_OPCNTS(&opstats[i]);

    parse_cmdline(argc, argv);

    TIME_STAMP(DateTime);

    if (sdfast_opt.verbose) {
        printf("SD/FAST (sdfast %s #%05d)\n", VersionNumber, gProgramSerialNo);
        printf("\nThis run: %s.\n\n", DateTime);
    }

    INIT_CALC(cMaxNumTerms, cMaxNumDOF);

    if (sdfast_opt.verbose) 
        printf("Using %s formulation.\n", 
           sdfast_opt.formulation == OPT_KANE 
           || sdfast_opt.formulation == OPT_DEFAULT
                                                ?         "Kane's" :
           sdfast_opt.formulation == OPT_ORDERN ?         "Order(N)" :
           sdfast_opt.formulation == OPT_EXP ?            "experimental" :
           sdfast_opt.formulation == OPT_EXP2 ?           "experimental2" :
                                                           "???");

    if (GET_INPUTS(&SysI)) {
        fprintf(stderr, "\nUnable to continue due to error during input.\n");
        exit(EXIT_INFILEERR);
    }

    /* At this point space has been allocated for tables in SysI.  Any
     * error returns must first free up the space in SysI.
     */
    if (Lang == &ADSIM_language) {
        fputs("\nADSIM language not supported.\n", stderr);
        exitstat = EXIT_BADCMDLINE;
        goto error_return;
    }

    /* 
     * Note: ordering matters here.  First, SDINIT must be computed
     * so that variables like mtot, iperp, etc. have valid values.
     *
     * Then, SDSTATE must be generated so that variables like rnk, cnk,
     * and com have valid values.
     *
     * Then, the auxiliary routines SDPOS, SDVEL, SDMOM, SDSYS, etc. can
     * be generated.
     *
     * Then, SDLHS and RHS must be generated so that accelerations are 
     * valid.
     *
     * Then, auxiliary routines SDACC, SDANGACC, SDREAC, etc. can be 
     * generated.
     *
     * Want a simple rule?  Generate the routines in the same order that
     * users are supposed to call them.
     */

    if (sdfast_opt.verbose && sdfast_opt.infile) {
        Lang_save = Lang;
        Text_language.subs_offset = Lang->subs_offset;
        Lang = &Text_language;
        eprintf("%{\n");
        PRINT_ROADMAP(stdout);
        eprintf("%}\n");
        Lang = Lang_save;
    }

    declare_sys_types((FILE *)0, DECL_NOPRINT, &SysI);

    /* The Information File */
    if (sdfast_opt.geninfo) {
        if (openw(&infoF, sdfast_opt.infoname)) {
            exitstat = EXIT_FILEERR;
            goto error_return;
        }
        Lang_save = Lang;
        Text_language.subs_offset = Lang->subs_offset;
        Lang = &Text_language;
        efprintf(infoF, "SD/FAST Information File: %s\n", sdfast_opt.infile);
        efprintf(infoF, "Generated %s by SD/FAST, %s formulation\n", DateTime,
           sdfast_opt.formulation == OPT_KANE 
           || sdfast_opt.formulation == OPT_DEFAULT 
                                                ? "Kane's" :
           sdfast_opt.formulation == OPT_ORDERN ? "Order(N)" :
           sdfast_opt.formulation == OPT_EXP ?    "experimental" :
           sdfast_opt.formulation == OPT_EXP2 ?   "experimental2" :
                                                  "???");
        efprintf(infoF, "(sdfast %s #%05d)\n", VersionNumber, gProgramSerialNo);

        PRINT_ROADMAP(infoF);
        PRINT_JTAXIS_DOC(infoF);
        PRINT_SYSTEM_DOC(infoF);

        Lang = Lang_save;
        CLOSE_FILE(infoF);

        if (sdfast_opt.verbose) 
            printf("Information File %s generated.\n", sdfast_opt.infoname);
    }

    /* The Dynamics File (SDINIT, SDSTATE, SDLHS, SDRHS, SDDERIV, ...) */

    if (!sdfast_opt.gendyn) {
        UtoQexpr = SCALAR_ONE();        /* don't do additional security check */
        goto dynDone;
    }

    if (openw(&dynF, sdfast_opt.dynname)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }
    F = dynF;

    efprintf(dynF, "%{\n");
    PRINT_SUBR_STAMP(dynF);
    efprintf(dynF, "\n");
    PRINT_ROADMAP(dynF);
    efprintf(dynF, "%}");

    declare_sys_types(dynF, 0, &SysI);

    if (Lang->flags & LANG_C_FAMILY) {
        efprintf(dynF,"#include <math.h>\n");
        efprintf(dynF,"#include <stdio.h>\n");
    }

    DECLARE_GLOBALS(dynF, 1/*generate initializations*/);

    if (PRINT_SDINIT(dynF)) {
        fprintf(stderr, "\nUnable to continue due to input model error.\n");
        exitstat = EXIT_INFILEERR;
        goto error_return;
    }
    PRINT_SDST2ANG(F);

    if (SysI.sl > 0) {
        lqx    = INUSE(NEW_1dARRAY(cScalarVal, SysI.nlq));
        lux    = INUSE(NEW_1dARRAY(cScalarVal, SysI.sl));
        lqdotx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nlq));
        ludotx = INUSE(NEW_1dARRAY(cScalarVal, SysI.sl));
    }

    PRINT_SDSTATE(dynF, lqx, lux, lqdotx, &opstats[SDSTATE]);
    PRINT_SDQDOT(dynF);
    PRINT_SDU2QDOT(dynF);
    PRINT_SDPSSTATE(dynF);

    if (sdfast_opt.breakup && openaux(&F, sdfast_opt.dynname, nxtaux++)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }

    /* these three routines call no subroutines */
    PRINT_SDDOVPK(F,&opstats[SDDOVPK]); /* sddovpk() or sddoping() */
    PRINT_SDDOLTAU(F,&opstats[SDDOLTAU]);
    PRINT_SDDOINER(F,&opstats[SDDOINER]);

    /* this calls several of the above routines */
    PRINT_SDDOFS0(F,&opstats[SDDOFS0]);
    adj_stats(opstats, SDDOFS0);

    if (sdfast_opt.breakup)
        CLOSE_FILE(F);

    if (sdfast_opt.breakup && openaux(&F, sdfast_opt.dynname, nxtaux++)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }

    /* Failure here means mass matrix is singular.  This routine may
     * call sddovpk(), but we won't count it here because we'll assume
     * sddovpk() was called and counted in sddofs0() above.
     *
     * If we're breaking up the Dyn file, we'll tell PRINT_SDDOMM how
     * many aux files it can use, what to use for a base name, and
     * what aux number to start with.  On return nxtaux will contain
     * the number of the next unused aux file.
     */
    if (PRINT_SDDOMM(F, sdfast_opt.breakup ? 40 : 0, sdfast_opt.dynname,
                     &nxtaux, &opstats[SDDOMM])) 
    {
        fprintf(stderr, "\nUnable to continue due to input model error.\n");
        exitstat = EXIT_SINGMASSMAT;
        goto error_return;
    }

    if (sdfast_opt.breakup)
        CLOSE_FILE(F);

    /* Compute the number of ops which will be consumed by the various
     * LDU routines.
     */
    COMPUTE_LDUCOST(SysI.s,
        &opstats[SDLDUDCOMP], &opstats[SDLDUBSL], &opstats[SDLDUBSD]);

    if (sdfast_opt.breakup && openaux(&F, sdfast_opt.dynname, nxtaux++)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }

    /* This calls sddomm() and sdldudcomp() */
    PRINT_SDDOMMLDU(F,&opstats[SDDOMMLDU]);
    adj_stats(opstats, SDDOMMLDU);

    /* This calls sddomm() or sddommldu() and calls sddofs0(). */
    PRINT_SDLHS(F, &opstats[SDLHS]);
    adj_stats(opstats, SDLHS);

    PRINT_SDMFRC(F,&opstats[SDMFRC]);

    PRINT_SDEQUIVHT(F);

    PRINT_SDFS0(F,&opstats[SDFS0]);
    PRINT_SDFSMULT(F,&opstats[SDFSMULT]);
    PRINT_SDFSFULL(F,&opstats[SDFSFULL]);
    /* sdfsfull() calls sdfsmult() */
    opstats[SDFSFULL].nadd += opstats[SDFSMULT].nadd;
    opstats[SDFSFULL].nmul += opstats[SDFSMULT].nmul;
    opstats[SDFSFULL].ndiv += opstats[SDFSMULT].ndiv;
    opstats[SDFSFULL].nassign += opstats[SDFSMULT].nassign;
    PRINT_SDFSGENMULT(F);
    PRINT_SDFSGENFULL(F);

    /* Calls sddoltau(), sdmfrc(), sddovpk() */
    PRINT_SDFULLTRQ(F,&opstats[SDFULLTRQ]);
    adj_stats(opstats, SDFULLTRQ);

    PRINT_SDRHS(F, ludotx, &opstats[SDRHS]);

    PRINT_MAT(F);

    PRINT_SDPSEUDO(F,lqx,lux);
    PRINT_SDPSDERIV(F,lqdotx,ludotx);
    PRINT_SDPERR(F);
    PRINT_SDVERR(F,lux);
    PRINT_SDAERR(F,ludotx);
    if (SysI.sl > 0) {
        DISPOSE_EXPR(UNUSE(lqx));
        DISPOSE_EXPR(UNUSE(lux));
        DISPOSE_EXPR(UNUSE(lqdotx));
        DISPOSE_EXPR(UNUSE(ludotx));
    }

    PRINT_CHECKSTUFF(F);
    PRINT_SDINDX(F);
    PRINT_FORCE_MOTION(F);

    if (sdfast_opt.breakup)
        CLOSE_FILE(F);

    if (sdfast_opt.breakup && openaux(&F, sdfast_opt.dynname, nxtaux++)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }

    /* This computes the number of symbolically nonredundant constraints
     * as a side effect.  This is the info we need to compute the number
     * of ops (roughly) which will be used by the QR routines.
     *
     * If we're breaking up the Dyn file, we'll tell PRINT_SDDOWW how
     * many aux files it can use, what to use for a base name, and
     * what aux number to start with.  On return nxtaux will contain
     * the number of the next unused aux file.
     */
    PRINT_SDDOWW(F, sdfast_opt.breakup ? 40 : 0, sdfast_opt.dynname,
                &nxtaux, &opstats[SDDOWW],&nonred);
    COMPUTE_QRCOST(nonred, nonred, nonred,
        &opstats[SDQRDCOMP], &opstats[SDQRBSLV]);
    adj_stats(opstats, SDDOWW);

    PRINT_SDUDOT0(F, &opstats[SDUDOT0]);
    adj_stats(opstats, SDUDOT0);

    PRINT_SDUDOTM(F, &opstats[SDUDOTM]);
    adj_stats(opstats, SDUDOTM);

    PRINT_SDDERIV(F,&opstats[SDDERIV],nonred);
    adj_stats(opstats, SDDERIV);

    PRINT_SDRESID(F,&opstats[SDRESID]);
    adj_stats(opstats, SDRESID);

    if (sdfast_opt.breakup)
        CLOSE_FILE(F);

    if (sdfast_opt.breakup && openaux(&F, sdfast_opt.dynname, nxtaux++)) {
        exitstat = EXIT_FILEERR;
        goto error_return;
    }

    PRINT_SDMULT(F);
    PRINT_SDREAC(F,&opstats[SDREAC]);

    PRINT_SDMOM(F,&opstats[SDMOM]);
    PRINT_SDSYS(F,&opstats[SDSYS]);

    PRINT_SDPOS(F);
    PRINT_SDVEL(F);
    PRINT_SDORIENT(F);
    PRINT_SDANGVEL(F);
    PRINT_SDTRANS(F);
    PRINT_SDREL2CART(F);
    PRINT_SDACC(F);
    PRINT_SDANGACC(F);
    PRINT_SET_ROUTINES(F);
    PRINT_GET_ROUTINES(F,nonred);

    if (sdfast_opt.breakup)
        CLOSE_FILE(F);

    PRINT_SDGENTIME(dynF);

    F = dynF;
    prfunc = efprintf;
    (*prfunc)(F, "%{\n");
    for (i=0; i<2; i++) {
        (*prfunc)(F, "Done. CPU seconds used: %.2f  Memory used: %ld bytes.\n",
               CPU_SECONDS() - gStartTime, BYTES_USED());
        (*prfunc)(F, "\
Equation complexity:\n\
  sdstate: %5ld adds %5ld multiplies %5ld divides %5ld assignments\n\
  sdderiv: %5ld adds %5ld multiplies %5ld divides %5ld assignments\n\
  sdresid: %5ld adds %5ld multiplies %5ld divides %5ld assignments\n\
  sdreac:  %5ld adds %5ld multiplies %5ld divides %5ld assignments\n\
  sdmom:   %5ld adds %5ld multiplies %5ld divides %5ld assignments\n\
  sdsys:   %5ld adds %5ld multiplies %5ld divides %5ld assignments\n",
        opstats[SDSTATE].nadd,opstats[SDSTATE].nmul,opstats[SDSTATE].ndiv,
          opstats[SDSTATE].nassign,
        opstats[SDDERIV].nadd,opstats[SDDERIV].nmul,opstats[SDDERIV].ndiv,
          opstats[SDDERIV].nassign,
        opstats[SDRESID].nadd,opstats[SDRESID].nmul,opstats[SDRESID].ndiv,
          opstats[SDRESID].nassign,
        opstats[SDREAC].nadd,opstats[SDREAC].nmul,opstats[SDREAC].ndiv,
          opstats[SDREAC].nassign,
        opstats[SDMOM].nadd,opstats[SDMOM].nmul,opstats[SDMOM].ndiv,
          opstats[SDMOM].nassign,
        opstats[SDSYS].nadd,opstats[SDSYS].nmul,opstats[SDSYS].ndiv,
          opstats[SDSYS].nassign
        );
        if (i==0)
            (*prfunc)(F, "%}");
        if (!sdfast_opt.verbose)
            break;
        F = stdout;
        prfunc = (void(*)(FILE *, char *, ...))fprintf;
    }

    CLOSE_FILE(dynF);
    if (sdfast_opt.verbose) 
        printf("Dynamics File %s generated.\n", sdfast_opt.dynname);

 dynDone:

    /* The Simplified Analysis File (sdassemble, sdinitvel, sdmotion, ...) */
    if (sdfast_opt.gensar) {
        if (openw(&sarF, sdfast_opt.sarname)) {
            exitstat = EXIT_FILEERR;
            goto error_return;
        }
        efprintf(sarF, "%{\n");
        PRINT_SUBR_STAMP(sarF);
        efprintf(sarF, "%}");
        if (Lang->flags & LANG_C_FAMILY)
            efprintf(sarF,"#include <math.h>\n");

        PRINT_SDPOSFUNC(sarF);
        PRINT_SDANAL(sarF);

        if (sdfast_opt.verbose) 
            printf("Simplified Analysis File %s generated.\n", 
                   sdfast_opt.sarname);
    }

    /* The Library File (sdlduslv, sdroot, ...) */
    if (sdfast_opt.genlib) {
        if (openw(&libF, sdfast_opt.libname)) {
            exitstat = EXIT_FILEERR;
            goto error_return;
        }

        efprintf(libF, "%{\n");
        PRINT_SUBR_STAMP(libF);
        efprintf(libF, "%}");
        if (Lang->flags & LANG_C_FAMILY) {
            efprintf(libF,"#include <math.h>\n");
            efprintf(libF,"#include <stdio.h>\n");
        }

        DECLARE_LIB_GLOBALS(libF);

        PRINT_SDPRERRMSG(libF);
        PRINT_ERRSTUFF(libF);

        PRINT_SDLDUSLV(libF);
        PRINT_SDQRSLV(libF);
        PRINT_SDLSSLV(libF);
        PRINT_SDROOT(libF);
        PRINT_SDINTEG(libF);

        PRINT_SDDC2ANG(libF);
        PRINT_SDDC2QUAT(libF);
        PRINT_SDANG2DC(libF);
        PRINT_SDQUAT2DC(libF);
        PRINT_VECSUBS(libF);

        PRINT_SDSERIALNO(libF);
        CLOSE_FILE(libF);

        if (sdfast_opt.verbose) 
            printf("Library File %s generated.\n", sdfast_opt.libname);
    }

#ifdef APPLIED_MOTION
    FREE_SYSTEM(&SysI);
    return EXIT_SUCCESS;
#else
    if (UtoQexpr) {
        FREE_SYSTEM(&SysI);
        exit(EXIT_SUCCESS);
    }

    /* Failed second security check. */

    fprintf(stderr, "\
                *** YOU HAVE BEEN CAUGHT ***\n\
It is a felony to attempt to subvert the expiration date on this\n\
program.  If you continue, Symbolic Dynamics will prosecute\n\
you to the fullest extent possible.  To obtain a runnable version\n\
of SD/FAST, contact Symbolic Dynamics Customer Support by email\n\
at support@symdyn.com, telephone +1 650 960-1532, fax +1 650 960-0338\n\
or send mail to Symbolic Dynamics, Inc., 561 Bush Street,\n\
Mountain View, CA 94041.  In the long run, the cost will\n\
be much lower for you if you purchase or lease a new version than\n\
if you try to steal one.\n");
    exitstat = EXIT_KEYPROBLEM;
    goto error_return;
#endif

    /* If we branch to here, we have an exit or return status in
     * exitstat, and we have space allocated in SysI that must be
     * freed.
     */
  error_return:
    FREE_SYSTEM(&SysI);
#ifdef APPLIED_MOTION
    return exitstat;
#else
    exit(exitstat);
    return 0;
#endif
}

/* If we're building the standalone version of SD/FAST, use a
 * stub for this routine rather than the real thing, which carries
 * along a lot of baggage.  Also, the heartbeat routine will look
 * at am_debug to decide what to say; that won't be present in the
 * standalone setup either.
 */
#if defined(_WIN32) && !defined(APPLIED_MOTION)
int am_debug;

EX2UI_heartbeat(void)
{
}
#endif

/* auxnoname
 *
 * Take a name like                  blahblah.x
 * and make it                       blahblah03.x
 * or in general                     blahblahnn.x
 * where nn is the passed-in auxno.
 * If there is no suffix, the digits are just stuck on the end.
 *
 * The size of the output array is passed in to prevent against overflows.
 *
 * auxno must be in the range 0 <= auxno <= 99.
 */
void auxnoname(int auxno, 
          char *origname,
          int sz,
          char  *auxname)
{
    char num[10], *dotloc, *p;
    int  oldlen;

    ASSERT(auxno >= 0 && auxno <= 99, 1, "auxfilename");

    strncpy(auxname, origname, sz-3);
    auxname[sz-3] = '\0';

    oldlen = (int)strlen(auxname);
    dotloc = strrchr(auxname, '.');
    if (dotloc) {
        for (p = &auxname[oldlen-1]; p >= dotloc; p--)
            *(p+2) = *p;
        p = dotloc;
    } else
        p = &auxname[oldlen];
    sprintf(num, "%02d", auxno);
    strncpy(p, num, 2);
    auxname[oldlen+2] = '\0';
}

/* openaux
 *
 * Open an auxiliary Dynamics file and start it off with appropriate
 * header material, such as #define's, #include's, typedefs, and extern's
 * in C.   Returns non-zero in case of a file error.  The file name is
 * generated by taking the passed-in dynname and adding a two-digit number
 * before the last suffix.  E.g., myfile_dyn.c becomes myfile_dyn04.c.
 * The number to add is passed in auxno; don't let it be > 99.
 */
int
openaux(FILE **dynbF,
        char *dynname,
        int auxno)
{
    char numnm[200];
    int  st;

    auxnoname(auxno, dynname, 200, numnm);

    if (st = openw(dynbF, numnm))
        return st;

    if (Lang->flags & LANG_C_FAMILY) {
        efprintf(*dynbF,"#include <math.h>\n");
        efprintf(*dynbF,"#include <stdio.h>\n");
    }

    DECLARE_GLOBALS(*dynbF, 0/*no initializations*/);

    return 0;
}

/* 
 * For routines which completely fill in the opstats struct, this
 * routine will adjust their opcounts to reflect all the calls they
 * make.
 */
static void
adj_stats(opstats_t opstats[],
          int which)
{
    opstats[which].nadd += 
       opstats[which].nldudcomp * opstats[SDLDUDCOMP].nadd +
       opstats[which].nldubsl   * opstats[SDLDUBSL].nadd +
       opstats[which].nldubsd   * opstats[SDLDUBSD].nadd +
       opstats[which].nmfrc     * opstats[SDMFRC].nadd +
       opstats[which].nlhs      * opstats[SDLHS].nadd +
       opstats[which].nrhs      * opstats[SDRHS].nadd +
       opstats[which].nfs0      * opstats[SDFS0].nadd +
       opstats[which].nfsmult   * opstats[SDFSMULT].nadd +
       opstats[which].nfsfull   * opstats[SDFSFULL].nadd +
       opstats[which].nqrdcomp  * opstats[SDQRDCOMP].nadd +
       opstats[which].nqrbslv   * opstats[SDQRBSLV].nadd +
       opstats[which].ndoww     * opstats[SDDOWW].nadd +
       opstats[which].nudot0    * opstats[SDUDOT0].nadd +
       opstats[which].nudotm    * opstats[SDUDOTM].nadd +
       opstats[which].ndoltau   * opstats[SDDOLTAU].nadd +
       opstats[which].ndoiner   * opstats[SDDOINER].nadd +
       opstats[which].ndofs0    * opstats[SDDOFS0].nadd +
       opstats[which].ndomm     * opstats[SDDOMM].nadd +
       opstats[which].ndommldu  * opstats[SDDOMMLDU].nadd +
       opstats[which].ndovpk    * opstats[SDDOVPK].nadd +
       opstats[which].nfulltrq  * opstats[SDFULLTRQ].nadd;
    opstats[which].nmul += 
       opstats[which].nldudcomp * opstats[SDLDUDCOMP].nmul +
       opstats[which].nldubsl   * opstats[SDLDUBSL].nmul +
       opstats[which].nldubsd   * opstats[SDLDUBSD].nmul +
       opstats[which].nmfrc     * opstats[SDMFRC].nmul +
       opstats[which].nlhs      * opstats[SDLHS].nmul +
       opstats[which].nrhs      * opstats[SDRHS].nmul +
       opstats[which].nfs0      * opstats[SDFS0].nmul +
       opstats[which].nfsmult   * opstats[SDFSMULT].nmul +
       opstats[which].nfsfull   * opstats[SDFSFULL].nmul +
       opstats[which].nqrdcomp  * opstats[SDQRDCOMP].nmul +
       opstats[which].nqrbslv   * opstats[SDQRBSLV].nmul +
       opstats[which].ndoww     * opstats[SDDOWW].nmul +
       opstats[which].nudot0    * opstats[SDUDOT0].nmul +
       opstats[which].nudotm    * opstats[SDUDOTM].nmul +
       opstats[which].ndoltau   * opstats[SDDOLTAU].nmul +
       opstats[which].ndoiner   * opstats[SDDOINER].nmul +
       opstats[which].ndofs0    * opstats[SDDOFS0].nmul +
       opstats[which].ndomm     * opstats[SDDOMM].nmul +
       opstats[which].ndommldu  * opstats[SDDOMMLDU].nmul +
       opstats[which].ndovpk    * opstats[SDDOVPK].nmul +
       opstats[which].nfulltrq  * opstats[SDFULLTRQ].nmul;
    opstats[which].ndiv += 
       opstats[which].nldudcomp * opstats[SDLDUDCOMP].ndiv +
       opstats[which].nldubsl   * opstats[SDLDUBSL].ndiv +
       opstats[which].nldubsd   * opstats[SDLDUBSD].ndiv +
       opstats[which].nmfrc     * opstats[SDMFRC].ndiv +
       opstats[which].nlhs      * opstats[SDLHS].ndiv +
       opstats[which].nrhs      * opstats[SDRHS].ndiv +
       opstats[which].nfs0      * opstats[SDFS0].ndiv +
       opstats[which].nfsmult   * opstats[SDFSMULT].ndiv +
       opstats[which].nfsfull   * opstats[SDFSFULL].ndiv +
       opstats[which].nqrdcomp  * opstats[SDQRDCOMP].ndiv +
       opstats[which].nqrbslv   * opstats[SDQRBSLV].ndiv +
       opstats[which].ndoww     * opstats[SDDOWW].ndiv +
       opstats[which].nudot0    * opstats[SDUDOT0].ndiv +
       opstats[which].nudotm    * opstats[SDUDOTM].ndiv +
       opstats[which].ndoltau   * opstats[SDDOLTAU].ndiv +
       opstats[which].ndoiner   * opstats[SDDOINER].ndiv +
       opstats[which].ndofs0    * opstats[SDDOFS0].ndiv +
       opstats[which].ndomm     * opstats[SDDOMM].ndiv +
       opstats[which].ndommldu  * opstats[SDDOMMLDU].ndiv +
       opstats[which].ndovpk    * opstats[SDDOVPK].ndiv +
       opstats[which].nfulltrq  * opstats[SDFULLTRQ].ndiv;
    opstats[which].nassign += 
       opstats[which].nldudcomp * opstats[SDLDUDCOMP].nassign +
       opstats[which].nldubsl   * opstats[SDLDUBSL].nassign +
       opstats[which].nldubsd   * opstats[SDLDUBSD].nassign +
       opstats[which].nmfrc     * opstats[SDMFRC].nassign +
       opstats[which].nlhs      * opstats[SDLHS].nassign +
       opstats[which].nrhs      * opstats[SDRHS].nassign +
       opstats[which].nfs0      * opstats[SDFS0].nassign +
       opstats[which].nfsmult   * opstats[SDFSMULT].nassign +
       opstats[which].nfsfull   * opstats[SDFSFULL].nassign +
       opstats[which].nqrdcomp  * opstats[SDQRDCOMP].nassign +
       opstats[which].nqrbslv   * opstats[SDQRBSLV].nassign +
       opstats[which].ndoww     * opstats[SDDOWW].nassign +
       opstats[which].nudot0    * opstats[SDUDOT0].nassign +
       opstats[which].nudotm    * opstats[SDUDOTM].nassign +
       opstats[which].ndoltau   * opstats[SDDOLTAU].nassign +
       opstats[which].ndoiner   * opstats[SDDOINER].nassign +
       opstats[which].ndofs0    * opstats[SDDOFS0].nassign +
       opstats[which].ndomm     * opstats[SDDOMM].nassign +
       opstats[which].ndommldu  * opstats[SDDOMMLDU].nassign +
       opstats[which].ndovpk    * opstats[SDDOVPK].nassign +
       opstats[which].nfulltrq  * opstats[SDFULLTRQ].nassign;
}

#ifdef APPLIED_MOTION
extern time_t time();
static time_t last_intrchk;

void
sd_init_intrchk(void)
{
    last_intrchk = (time_t)0;
}

void
sd_check_for_interrupt(void)
{
    time_t        now;

    (void)time(&now);

    if (now < last_intrchk + SD_INTRCHK_RATE) 
        return;

    if (mm_checkintr())
        longjmp(sdfast_intr, 1);

    (void)time(&last_intrchk);
}
#else
/* This stub will be called even when we're not in APPLIED_MOTION,
 * but we won't ever interrupt in that case.
 */
void
sd_check_for_interrupt(void)
{
}
#endif

#ifdef NOTDEF
void
dump_opstats(opstats_t opstats[],
             int which)
{
    opstats_t *op = &opstats[which];

    printf("nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    printf("nldubsl=%d nldubsd=%d nmfrc=%d nlhs=%d nrhs=%d\n",
        op->nldubsl,op->nldubsd,op->nmfrc,op->nlhs,op->nrhs);
    printf("nfs0=%d nfsmult=%d nfsfull=%d nqrdcomp=%d nqrbslv=%d ndoww=%d\n",
        op->nfs0,op->nfsmult,op->nfsfull,op->nqrdcomp,op->nqrbslv,op->ndoww);

    
    op = &opstats[SDLDUBSL];
    printf("ldubsl: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDLDUBSD];
    printf("ldubsd: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDMFRC];
    printf("mfrc: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDLHS];
    printf("lhs: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDRHS];
    printf("rhs: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDFS0];
    printf("fs0: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDFSMULT];
    printf("fsmult: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDFSFULL];
    printf("fsfull: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDQRDCOMP];
    printf("qrdcomp: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDQRBSLV];
    printf("qrbslv: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDDOWW];
    printf("doww: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDUDOT0];
    printf("udot0: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
    op = &opstats[SDUDOTM];
    printf("udotm: nadd=%d nmul=%d ndiv=%d nassign=%d\n",
        op->nadd,op->nmul,op->ndiv,op->nassign);
}
#endif
