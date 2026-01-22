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

#define        MM_PI                3.14159265358979323846

/* These declarations are for the fussy SGI compiler which
 * can't find these static routines otherwise.
 */
static void compute_aux_q(FILE *F);
static void compute_aux_u(FILE *F);
static void compute_aux_wk(FILE *F);

/* PRINT_SDSTATE
 *
 * Generate the sdstate() subroutine:
 *
 *            sdstate(t,q,u)
 *
 * t,q,u, are inputs.
 *
 * Sneaky: if a magic_no has been supplied on the command line, the
 * routine declaration changes to
 *
 *            sdstate(t,u,q,magic)
 *
 * Note that q and u are reversed, and the magic number should be
 * passed in.  Quiet, but terrible, things will happen if the wrong
 * number is supplied.  We give the magic number the discrete name "n"
 * rather than "magic" in the generated code.
 *
 * Note that sdstate() has few local variables.  This is
 * no accident!  Everything that sdstate() generates is put in the global
 * struct or common block `sdgstate'.  This includes all intermediate
 * variables, so that a subsequent user does not have to worry that a
 * reference to an IS_SIMPLE kinematic variable might produce a reference
 * to some symbol that the user does not have access to.
 *
 * To comfortably reference the variables defined here, a routine should
 * include the sdginput and sdgstate common blocks, using the same field
 * names as used here.
 *
 * The lqx and lux expressions are filled in with calculated loop q and u
 * values for later use.  They should already have been allocated.
 * The lqdotx expression is filled in with calculated loop 
 * qdot values for later use.  It should already have been 
 * allocated.
 */
void PRINT_SDSTATE(FILE *F,
              expr lqx, 
              expr lux, 
              expr lqdotx,
              opstats_t *opcnt)
{
    long lasg;
    char str_n[10], str_s[10], str_nq[10], str_sl[10];
    sym e1,e2,e3,e4;
    expr perrx,verrx,tempx;
    int i,first;
    double fiveaway;

    /* Make sure q, qn, and u consist of permanent VREFs to
     * themselves since they may get referenced a lot and we want
     * to create the VREF expressions only once.
     */
    if (SysI.s) {
        tempx = NEW_1dARRAY(cScalarVal, SysI.nq);
        for (i=0; i<SysI.nq; i++)
            SINDX(tempx, i, VREF1(q,i));
        ASSIGN(q, PERM(tempx));
        tempx = NEW_1dARRAY(cScalarVal, SysI.nq);
        for (i=0; i<SysI.nq; i++)
            SINDX(tempx, i, VREF1(qn,i));
        ASSIGN(qn, PERM(tempx));
        tempx = NEW_1dARRAY(cScalarVal, SysI.s);
        for (i=0; i<SysI.s; i++)
            SINDX(tempx, i, VREF1(u,i));
        ASSIGN(u, PERM(tempx));
    }

    /* Don't allow the gimbal joint to operate
     * closer than 5 degrees from gimbal lock.  Compute the
     * closest to 1 we can stand to have the dot product of
     * pin1 and pin3 be.
     */
    fiveaway = 1. - cos(5.*MM_PI/180.);

    START_COMPSUB(opcnt);
    lasg = 0;

    esprintf(str_n, "%d", SysI.n);
    esprintf(str_s, "%d", SysI.s);
    esprintf(str_sl, "%d", SysI.sl);
    esprintf(str_nq, "%d", SysI.nq);

    if (sdfast_opt.magic_no) {
        declare_proc(F, 0, "state", 
          VT_REAL,                        "timein",
          VT_USER, &SysI.type_Arr_s,    "uin",
          VT_USER, &SysI.type_Arr_nq,   "qin", 
          VT_INTEGER,                         "n",
          0);
    } else {
        declare_proc(F, 0, "state", 
          VT_REAL,                        "timein",
          VT_USER, &SysI.type_Arr_nq,   "qin", 
          VT_USER, &SysI.type_Arr_s,    "uin",
          0);
    }

    efprintf(F, "%{\n\
Compute kinematic information and store it in sdgstate.\n\n");
    PRINT_SUBR_STAMP(F);
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER, "i",
      VT_DUP,     "j",
      VT_DUP,     "qchg",
      VT_DUP,     "uchg",
      0);

    /* these symbols are local to SDSTATE */

    if (SysI.nl) {
        declare_vars(F, 0, 
            VT_REAL|VT_DSYM,  "e1", &e1,
            VT_DUP|VT_DSYM,  "e2", &e2,
            VT_DUP|VT_DSYM,  "e3", &e3,
            VT_DUP|VT_DSYM,  "e4", &e4,
            0);
    }

    /* local vars used in stabilization of ball joint Euler params (scalars) */
    if (SysI.nb)
        declare_vars(F, 0,
          VT_REAL|VT_DSYM, "ee", &ee,
          VT_DUP|VT_DSYM, "stab", &stab,
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND 
      efprintf(F, "(roustate%s%d)%s(roustate%s%d)%s(roustate%s%d)", 
               NE, ST_INIT, AND_OP,NE, ST_STATEREADY, AND_OP,NE, ST_DERIVREADY);
    THEN 
      SETERR(F, ROU_sdstate, ERR_sdinitMustBeCalledFirst);
      RETURN;
    ENDIF;

    IFCOND efprintf(F, "roustate%s%d", EQ, ST_INIT);
    THEN
        FORCNT("50", "i", str_s);
          IFCOND efprintf(F, "presq%(i%)%s%d", EQ, ISQUESFLG);
            THEN SETERR(F, ROU_sdstate, ERR_TreePresMustBeSpecified);
          ENDIF;
        ENDFOR("50");
        if (SysI.sl) {
            FORCNT("60", "i", str_sl);
              IFCOND efprintf(F, "lpresq%(i%)%s%d", EQ, ISQUESFLG);
                THEN SETERR(F, ROU_sdstate, ERR_LoopPresMustBeSpecified);
              ENDIF;
            ENDFOR("60");
        }
    ENDIF;

    efprintf(F, "%{\nSee if time or any qs have changed since last call\n%}");
    IFCOND efprintf(F, "(roustate%s%d)%s(timein%scurtim)", 
           NE, ST_INIT, AND_OP, EQ);
    THEN
      ISET("qchg", 0);
      FORCNT("65", "i", str_nq);
        IFCOND efprintf(F, "qin%(i%)%sq%(i%)", NE);
        THEN
          ISET("qchg", 1);
          BREAK("70");
        ENDIF;
      ENDFORBRK("65","70");
    ELSE
      ISET("qchg", 1);
    ENDIF;

    efprintf(F, "%{\nIf time and qs are unchanged, check us\n%}");
    IF("qchg", EQ, "0")
    THEN
      ISET("uchg", 0);
      FORCNT("75", "i", str_s);
        IFCOND efprintf(F, "uin%(i%)%su%(i%)", NE);
        THEN
          ISET("uchg", 1);
          BREAK("80");
        ENDIF;
      ENDFORBRK("75","80");
    ELSE
      ISET("uchg", 1);
    ENDIF;

    /* this path is always executed */

    efprintf(F, "curtim%=timein%;\n");

    /* SDSTATE() leaves us in state ST_STATEREADY.  We set it now
     * since we may have to call user constraint routines below which
     * will call routines like sdpos() which will barf if they don't
     * think sdstate has already been called.  By the time we get there,
     * we will have already calculated everything
     * those routines will need.  (Although they MUST NOT reference
     * sdperr() or sdverr() in the sduperr() and sduverr() routines.)
     */
    ISET("roustate", ST_STATEREADY);

    IF("qchg", EQ, "0")
    THEN
      GOTO("skipqs", "190");
    ENDIF;

    efprintf(F, "%{\nPosition-related variables need to be computed\n%}");

    /* Q-dependent variables are now invalid. */
    ISET("vpkflg",   0);
    ISET("mmflg",    0);
    ISET("mmlduflg", 0);
    ISET("wwflg", 0);

    FORCNT("100", "i", str_nq);
      SET("q%(i%)","qin%(i%)");
    ENDFOR("100");
    lasg += SysI.nq;

    /* Save the magic number. */
    if (sdfast_opt.magic_no)
        SET("ii", "n");

    /* This may set lasterr if q is severely unnormalized. */
    if (SysI.nb) {
        efprintf(F, "%{\nNormalize Euler parameters in state\n%}");
        CALL1("%Anrmsterr(q,qn,%d)", ROU_sdstate);
    }

    COMPUTE_S_AND_C(F);        /* S1=SIN(Q(1)), etc. */

    efprintf(F, "%{\nCompute across-axis direction cosines Cik\n%}");
    COMPUTE_Cik(F);
    fflush(F);

    efprintf(F, "%{\nCompute across-joint direction cosines Cib\n%}");
    COMPUTE_Cib(F);
    fflush(F);

    /* gk will print out a comment if user specified gravity in the input */
    COMPUTE_gk(F);
    fflush(F);

    efprintf(F, "%{\nCompute cnk & cnb (direction cosines in N)\n%}");
    COMPUTE_cnk(F);
    COMPUTE_cnb(F);
    fflush(F);

    if (SysI.nl) {
        efprintf(F, 
            "%{\nCompute Cio (dircos btw loop jt connected bodies)\n%}");
        COMPUTE_Cio(F);
        fflush(F);
    }

    if (sdfast_opt.verbose)
        printf("Direction cosines computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    /* This says that we passed all security checks. */
    UtoQexpr = SCALAR_ONE();

    efprintf(F, "%{\nCompute q-related auxiliary variables\n%}");
    compute_aux_q(F);
    fflush(F);

    efprintf(F, "%{\nCompute rnk & rnb (mass center locations in N)\n%}");
    COMPUTE_rnk(F);
    COMPUTE_rnb(F);
    fflush(F);

    efprintf(F, "%{\nCompute com (system mass center location in N)\n%}");
    COMPUTE_com(F);
    fflush(F);
    if (sdfast_opt.verbose)
        printf("Mass center locations computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    if (SysI.nl) {
        efprintf(F, "%{\nCompute loop joint q-related temps\n%}");
        COMPUTE_lptemps(F);

        efprintf(F, "%{\nCompute dircos inside loop joints\n%}");
        COMPUTE_Cibob(F);
        fflush(F);
        efprintf(F, 
    "%{\nDecompose loop joint dircos into Euler angles or parameters\n%}");
        COMPUTE_euler(F, tsc1, tsc2, tsc3, tvec1, tvec2, tvec3,
                      e1, e2, e3, e4, ipin2x);
        fflush(F);
        if (SysI.sl)
            COMPUTE_lq(lqx);
    }

    /* Position constraint errors.  Loop and user constraints
     * are handled here; prescribed motion errors are calculated directly
     * in SDPERR() since the user doesn't call sdprespos()
     * until *after* sdstate() returns.
     */

    /* Compute position constraint errors for loop joints. */
    if (SysI.nc) {
        efprintf(F, "%{\nCompute constraint position errors\n%}");

        perrx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nc));

        /* set prescribed motion pos errors to 0 for now */
        for (i = 0; i < SysI.np; i++)
            SINDX(perrx, i, SCALAR_ZERO());

        if (SysI.nl)
            COMPUTE_lperr(perrx);
    }

    /* User constraints are computed numerically by a user-written
     * routine (sduperr()), so we just reference the (as yet to be
     * filled in) numerical values in perrx here.
     */
    if (SysI.nu) {
        Index_t m = SysI.Const[0].Mindex;
        for (i=m; i<m+SysI.nu; i++)
            SINDX(perrx, i, VREF1(perr,i));
    }

    /* We're assigning the perr symbol here, so prior to
     * this the sdperr() routine was not valid. 
     */
    if (SysI.nc)
        ASSIGN_CLN(F, perr, UNUSE(perrx));

    /* That's all the q-related stuff we'll do in sdstate(), except
     * for a final call to the user's sduperr() routine. 
     */

    LABEL("skipqs", "190");

    /* Check for gimbal lock. */
    first = 1;

    /* tree gimbals */
    for (i=0; i<SysI.n; i++) {
        JointKind_t jk = SysI.Bodies[i].jnt.JointKind;
        if (JointInfo[(int)jk].hasgimbal) {
            int pin1, pin3, elsetoo;
            expr x, cond;

            if (first) {
                efprintf(F, "%{\nCheck for locked gimbal tree joint\n%}");
                first = 0;
            }
            if (jk==cBearingJoint)
                pin1 = SysI.FirstDOF[i]+1;
            else if (jk==cBushingJoint)
                pin1 = SysI.FirstDOF[i]+3;
            else
                pin1 = SysI.FirstDOF[i];
            pin3 = pin1 + 2;
            x = DOT(MATMUL(VAL1(SysI.pin,pin1),
                           TRANSPOSE(VAL1(cnk,pin1))),
                    MATMUL(VAL1(SysI.pin,pin3),
                           TRANSPOSE(VAL1(cnk,pin3))));
            cond = NEARTO(ABS(x), SCALAR_ONE(), fiveaway);
            if (IFTHEN(F, cond, &elsetoo)) {
                SETERR(F, ROU_sdstate, ERR_TreeGimbalLocked);
            }
            IFEND(F, cond);
            DISPOSE_EXPR(cond);
        }
    }

    /* loop gimbals */
    first = 1;

    for (i=0; i<SysI.nl; i++) {
        JointKind_t jk = SysI.LoopConst[i].jnt.JointKind;
        if (JointInfo[(int)jk].hasgimbal) {
            int elsetoo;
            expr x, cond;

            if (first) {
                efprintf(F, "%{\nCheck for locked gimbal loop joint\n%}");
                first = 0;
            }
            x = DOT(MATMUL(VAL1(ipin,i), VAL1(Cio,i)), 
                    VAL1(opin,i));
            cond = NEARTO(ABS(x), SCALAR_ONE(), fiveaway);
            if (IFTHEN(F, cond, &elsetoo)) {
                SETERR(F, ROU_sdstate, ERR_LoopGimbalLocked);
            }
            IFEND(F, cond);
            DISPOSE_EXPR(cond);
        }
    }

    /* Evaluate user constraint perr's. 
     * We delay user constraint perr evaluations until the
     * end of sdstate()'s q stuff to make sure everything needed by 
     * routines like sdpos() is already calculated by the time the 
     * user's routine gets called.  Also, we must call sduperr()
     * even if the q's haven't changed, since the user may have
     * changed something that affects his user-written routine (e.g.,
     * he turned on a new constraint).
     */
    if (SysI.nu) {
        Index_t m = SysI.Const[0].Mindex;
        efprintf(F, "%s%Auperr(curtim,q,%sperr%(%@d%))%;\n", 
                 Lang->proc_call, Lang->ref, m);
    }

    IF("uchg", EQ, "0")
    THEN
      GOTO("skipus", "290");
    ENDIF;

    efprintf(F, "%{\nVelocity-related variables need to be computed\n%}");

    /* U-dependent variables are now invalid. */
    ISET("inerflg", 0);

    FORCNT("200", "i", str_s);
      SET("u%(i%)","uin%(i%)");
    ENDFOR("200");

    lasg += SysI.s;

    efprintf(F, "%{\nCompute u-related auxiliary variables\n%}");
    compute_aux_u(F);
    fflush(F);

    efprintf(F, "%{\nCompute wk & wb (angular velocities)\n%}");
    COMPUTE_wk(F);
    COMPUTE_wb(F);
    fflush(F);
    if (sdfast_opt.verbose)
        printf("Angular velocities computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    efprintf(F, "%{\nCompute auxiliary variables involving wk\n%}");
    compute_aux_wk(F);
    fflush(F);

    efprintf(F, "%{\nCompute temporaries for use in SDRHS\n%}");
    COMPUTE_wbtemps(F);
    fflush(F);

    efprintf(F, 
        "%{\nCompute vnk & vnb (mass center linear velocities in N)\n%}");
    COMPUTE_vnk(F);
    COMPUTE_vnb(F);
    fflush(F);
    if (sdfast_opt.verbose)
        printf("Linear velocities computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    if (SysI.nl) {
        efprintf(F, "%{\nCompute loop joint u-related temps\n%}");
        COMPUTE_lvtemps(F);

        efprintf(F, "%{\nCompute angvel inside loop joints\n%}");
        COMPUTE_Woiob(F);
        fflush(F);
        efprintf(F, 
    "%{\nCompute derivatives of loop joint pseudo-coordinates\n%}");
        COMPUTE_euldot(F, ipin2x, tsc1, tsc2, tsc3, tvec1, tvec2, tvec3);
        fflush(F);
        if (SysI.sl)
            COMPUTE_lu(lux);
        /* No need to assign the "lu" symbol. */
    }

    efprintf(F, "%{\nCompute qdot (kinematical equations)\n%}");
    COMPUTE_qdot(F);
    fflush(F);
    if (sdfast_opt.verbose)
        printf("Qdot equations computed. (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    COMPUTE_lqdot(lqdotx);

    /* Velocity constraint errors.  Loop and user constraints
     * are handled here; prescribed motion errors are calculated directly
     * in SDVERR() since the user doesn't call sdpresvel()
     * until *after* sdstate() returns.
     */

    /* Compute velocity constraint errors for loop joints. */
    if (SysI.nc) {
        efprintf(F, "%{\nCompute constraint velocity errors\n%}");

        verrx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nc));

        /* set prescribed motion vel errors to 0 for now */
        for (i = 0; i < SysI.np; i++)
            SINDX(verrx, i, SCALAR_ZERO());

        if (SysI.nl)
            COMPUTE_lverr(verrx);
    }

    /* User constraints will be filled in numerically at run time. */
    if (SysI.nu) {
        Index_t m = SysI.Const[0].Mindex;
        for (i=m; i<m+SysI.nu; i++)
            SINDX(verrx, i, VREF1(verr,i));
    }

    /* We're assigning the verr symbol here, so prior to
     * this the sdverr() routine was not valid. 
     */
    if (SysI.nc)
        ASSIGN_CLN(F, verr, UNUSE(verrx));

    LABEL("skipus", "290");

    /* Evaluate user constraint verr's. 
     * We delayed user constraint verr evaluations until the
     * end of sdstate()'s u-related calculations to make sure everything 
     * needed by routines like sdvel() is already calculated by the 
     * time the user's routine gets called.  Also, we call this routine
     * even if no u's have changed, since the user may have changed
     * something which will affect the behavior of his sduverr() routine
     * (e.g. turned on a constraint that was off).
     */
    if (SysI.nu) {
        Index_t m = SysI.Const[0].Mindex;
        efprintf(F, "%s%Auverr(curtim,q,u,%sverr%(%@d%))%;\n", 
                 Lang->proc_call, Lang->ref, m);
    }

    efprintf(F, "%{\nInitialize applied forces and torques to zero\n%}");
    FORCNT("310", "i", str_n);
        FORCNT("300", "j", "3");
            RSET("ufk%(i%,j%)",0.);
            RSET("utk%(i%,j%)",0.);
        ENDFOR("300");
    ENDFOR("310");

    FORCNT("320", "i", str_s);
        RSET("utau%(i%)",0.);
    ENDFOR("320");

    if (SysI.sl) {
        FORCNT("330", "i", str_sl);
            RSET("ltau%(i%)",0.0);
        ENDFOR("330");
    }
    lasg += 6*SysI.n + SysI.s + SysI.sl;

    /* Effect of loop tau's has not yet been computed. */
    ISET("ltauflg", 0);

    /* Active force contributions not yet computed. */
    ISET("fs0flg", 0);

    if (SysI.np) {
        efprintf(F, "%{\nInitialize prescribed motions\n%}");
        for (i=0; i<SysI.s; i++) 
            if (!IS_ZERO(VAL1(SysI.pres,i))) {
                PRINT_ASSN1(F, "uacc", i, SCALAR_ZERO());
                PRINT_ASSN1(F, "uvel", i, VAL1(u,i));
                PRINT_ASSN1(F, "upos", i, VAL1(q,i));
            }
        for (i=0; i<SysI.sl; i++) 
            if (!IS_ZERO(VAL1(SysI.lpres,i))) {
                PRINT_ASSN1(F, "lacc", i, SCALAR_ZERO());
                PRINT_ASSN1(F, "lvel", i, INDX(lux,i));
                PRINT_ASSN1(F, "lpos", i, INDX(lqx,i));
            }
    }

    if (SysI.sl) {
        efprintf(F, "%{\nSet lqs -- %Apsstate may override some\n%}");
        for (i=0; i<SysI.sl; i++)
            PRINT_ASSN1(F, "lq", i, INDX(lqx,i));
    }

    if (sdfast_opt.verbose)
        printf("Done with kinematic equations.  (CPU: %g MEM: %lu)\n",
          CPU_SECONDS() - gStartTime, (unsigned long)BYTES_USED());

    END_COMPSUB(F,opcnt,0L,0L,0L,lasg);
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDPSSTATE
 *
 * Generate the sdpsstate() subroutine.  This routine simply copies some of the
 * elements of its lone parameter into the global `lq' variable, overwriting 
 * the default values stuffed there by sdstate().  The elements copied are
 * those which correspond to rotational (non-slider, non-ball) loop joint 
 * hinges which have active (turned on) prescribed motion.
 */
void PRINT_SDPSSTATE(FILE *F)
{
    int  i,j;
    char   str_flt0[20];
    JointDesc_t *jntp;

    esprintf(str_flt0, "%r", 0.0); /* for use in `if' statement */

    /* Declare the SDPSSTATE routine heading. */
    declare_proc(F, 0, "psstate",
      VT_USER, &SysI.type_Arr_sl, "lqin",
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Agstate do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_STATEREADY, ST_NOSTATE, 
                ROU_sdpsstate, ERR_sdstateMustBeCalledFirst);

    if (SysI.sl) {
        for (i = SysI.n; i < SysI.nj; i++) {
            jntp = &SysI.LoopConst[i-SysI.n].jnt;
            for (j = SysI.FirstDOF[i]; j <= SysI.LastDOF[i]; j++) {
                if (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[i]]
                    == AX_ROT)
                {
                    if (IS_ONE(VAL1(SysI.lpres,j))) {
                        /* definitely prescribed */
                        efprintf(F, "lq%(%@d%)%=lqin%(%@d%)%;\n", j, j);
                        efprintf(F, "lpos%(%@d%)%=lq%(%@d%)%;\n", j, j);
                    } else if (!IS_ZERO(VAL1(SysI.lpres,j))) {
                        /* runtime prescribed */
                        efprintf(F, Lang->stmt_if2_b);
                        PRINT_E(F, VAL1(SysI.lpres,j));
                        efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
                          efprintf(F, "lq%(%@d%)%=lqin%(%@d%)%;\n", j, j);
                          efprintf(F, "lpos%(%@d%)%=lq%(%@d%)%;\n", j, j);
                        efprintf(F, Lang->stmt_if2_e);
                    }
                }
            }
        }
    }

    fflush(F);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    efprintf(F, Lang->proc_end);
}

/*
 * compute_aux_q
 *
 * Compute auxiliary variables rpp, rpri, rpk, rik.
 * If we're doing Order(N), we'll compute rikt here too.
 */
static void 
compute_aux_q(FILE *F)
{
    if (SysI.s == 0)
        return;

    COMPUTE_rpp(F);
    COMPUTE_rpri(F);
    COMPUTE_rpk(F);
    COMPUTE_rik(F);
    if (sdfast_opt.formulation == OPT_ORDERN) {
        COMPUTE_rik2(F);
        COMPUTE_rikt(F);
    }
}

/*
 * compute_aux_u
 *
 * Compute auxiliary variables Wik, Vik.
 */
static void
compute_aux_u(FILE *F)
{
    if (SysI.s == 0)
        return;

    COMPUTE_Wik(F);
    COMPUTE_Vik(F);
}

/*
 * compute_aux_wk
 *
 * Compute auxiliary variables Wirk, Wkrpk, VikWkr, IkWk, WkIkWk, WkIkbWk.
 */
static void 
compute_aux_wk(FILE *F)
{
    if (SysI.s == 0)
        return;

    COMPUTE_Wirk(F);
    COMPUTE_Wkrpk(F);          /* must compute Wkrpk before VikWkr */
    COMPUTE_VikWkr(F);
    COMPUTE_WkIkWk(F);
    COMPUTE_WkIkbWk(F);
}

/* COMPUTE_rpp
 *
 * Compute joint translation vectors into global var rpp.
 * This is zero except for sliding joints, where it is
 * a vector along the joint axis whose length is the amount
 * of sliding that has taken place.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rpp(FILE *F)
{
    expr rpp_expr;
    register Index_t i;

    rpp_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (i = 0; i < SysI.s; i++)
        SINDX(rpp_expr, i, MUL(VAL1(q,i), VAL1(Vkk,i)));

    ASSIGN_CLN(F, rpp, UNUSE(rpp_expr));
}

/* COMPUTE_rpri
 *
 * Compute rpri, the vector from the center of mass of the inboard
 * body to the hinge point on the outboard body k.  For
 * non-sliders, this is just ri[k].  Otherwise, use rpp to
 * account for the amount of sliding that has taken place.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rpri(FILE *F)
{
    expr rpri_expr;
    register Index_t k;

    rpri_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rpri_expr, k, ADD(VAL1(rpp,k), VAL1(SysI.psri,k)));

    ASSIGN_CLN(F, rpri, UNUSE(rpri_expr));
}

/* COMPUTE_rpk
 *
 * Compute rpk, the vector from the hinge point on the inboard
 * body to the center of mass on the outboard body k.  For
 * non-sliders, this is just -rk[k].  Otherwise, use rpp to
 * account for the amount of sliding that has taken place.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rpk(FILE *F)
{
    expr rpk_expr;
    register Index_t k;

    rpk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rpk_expr, k, SUB(VAL1(rpp,k), VAL1(SysI.psrk,k)));

    ASSIGN_CLN(F, rpk, UNUSE(rpk_expr));
}

/* COMPUTE_rik
 *
 * Compute rik, the vector from the center of mass of the inboard
 * body to the center of mass on the outboard body k.  This is
 * just ri[k]*Cik+rpk[k].  (It's in the k frame.)
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rik(FILE *F)
{
    expr rik_expr;
    register Index_t k;

    rik_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rik_expr, k, 
           ADD(MATMUL(VAL1(SysI.psri,k),VAL1(Cik,k)), VAL1(rpk,k)));

    ASSIGN_CLN(F, rik, UNUSE(rik_expr));
}

/* COMPUTE_rik2
 *
 * Compute rik2, the vector from the inboard body's inboard hinge
 * point to the current body's inboard hinge point.  (It's in the
 * i frame.) This is for use with the Order(N) formulation.
 *
 *       rik2[k] = dik[k] + rpp[k]   
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rik2(FILE *F)
{
    expr rik2_expr;
    register Index_t k;

    rik2_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rik2_expr, k, ADD(VAL1(dik,k),VAL1(rpp,k)));

    ASSIGN_CLN(F, rik2, UNUSE(rik2_expr));
}

/* COMPUTE_rikt
 *
 * Compute rikt, an operator used in the Order(N) formulation.
 * This is a matrix:
 *             rikt[k] = TILDA(rik2[k])*Cik[k]
 *
 * This is an Order(N) computation.
 */
void COMPUTE_rikt(FILE *F)
{
    expr rikt_expr;
    register Index_t k;

    rikt_expr = INUSE(NEW_1dARRAY(cMatrixVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(rikt_expr, k, MATMUL(TILDA(VAL1(rik2,k)),VAL1(Cik,k)));

    ASSIGN_CLN(F, rikt, UNUSE(rikt_expr));
}

/* COMPUTE_Wik
 *
 * Compute joint relative angular velocity vectors into global var Wik.
 * This is zero for sliding joints, otherwise just the velocity around
 * the rotating hinge axis.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_Wik(FILE *F)
{
    expr Wik_expr;
    register Index_t i;

    Wik_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (i = 0; i < SysI.s; i++)
        SINDX(Wik_expr, i, MUL(VAL1(u,i), VAL1(Wkk,i)));

    ASSIGN_CLN(F, Wik, UNUSE(Wik_expr));
}

/* COMPUTE_Vik
 *
 * Compute joint relative displacement velocity vectors into global var Vik.
 * This is zero for rotational joints, otherwise just the velocity along
 * the sliding hinge axis.
 *
 * This is an Order(N) computation.
 */
void COMPUTE_Vik(FILE *F)
{
    expr Vik_expr;
    register Index_t i;

    Vik_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (i = 0; i < SysI.s; i++)
        SINDX(Vik_expr, i, MUL(VAL1(u,i), VAL1(Vkk,i)));

    ASSIGN_CLN(F, Vik, UNUSE(Vik_expr));
}

/* COMPUTE_Wirk
 *
 * Compute auxiliary variable Wirk:
 *   Wirk[k]  = [0,0,0],             inb(k) == ground
 *            = wk[i] X ri[k],       inb(k) != ground
 *
 * This is an Order(N) computation.
 */
void COMPUTE_Wirk(FILE *F)
{
    register Index_t k, inb;
    expr Wirk_expr;

    Wirk_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++) {
        inb = SysI.PseudoBodies[k].jnt.InbBody;
        if (inb == cGroundBody)
            SINDX(Wirk_expr, k, VECTOR_ZERO());
        else
            SINDX(Wirk_expr, k, CROSS(VAL1(wk,inb),VAL1(SysI.psri,k)));
    }

    ASSIGN_CLN(F, Wirk, UNUSE(Wirk_expr));
}

/* COMPUTE_Wkrpk
 *
 * Compute auxiliary variable Wkrpk:
 *   Wkrpk[k] = wk[k] X rpk[k]
 *
 * This is an Order(N) computation.
 */
void COMPUTE_Wkrpk(FILE *F)
{
    register Index_t k;
    expr Wkrpk_expr;

    Wkrpk_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(Wkrpk_expr, k, CROSS(VAL1(wk,k),VAL1(rpk,k)));

    ASSIGN_CLN(F, Wkrpk, UNUSE(Wkrpk_expr));
}

/* COMPUTE_VikWkr
 *
 * Compute auxiliary variable Wkrpk:
 *   VikWkr[k] = Vik[k] + wk[k] X rpk[k]
 *             = Vik[k] + Wkrpk[k]
 *
 * Be sure that Wkrpk has already been computed.
 * This is an Order(N) computation.
 */
void COMPUTE_VikWkr(FILE *F)
{
    register Index_t k;
    expr VikWkr_expr;

    VikWkr_expr  = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (k = 0; k < SysI.s; k++)
        SINDX(VikWkr_expr, k, ADD(VAL1(Vik,k), VAL1(Wkrpk,k)));

    ASSIGN_CLN(F, VikWkr, UNUSE(VikWkr_expr));
}

/* COMPUTE_WkIkWk
 *
 * Compute temporary variables 
 *            IkWk[k] = ik[k]*wk[k] 
 *          WkIkWk[k] = wk[k] X IkWk[k]
 * for use in Tstar calculation.
 *
 * This is an Order(n) calculation.
 */
void COMPUTE_WkIkWk(FILE *F)
{
    register Index_t b,k;
    expr IkWk_expr,WkIkWk_expr;

    IkWk_expr   = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
    WkIkWk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));

    for (b = 0; b < SysI.n; b++)
        for (k = SysI.FirstDOF[b]; k <= SysI.LastDOF[b]; k++) {
            /* compute IkWk[k] */
            FLUSH_VEC(F, IkWk, k, IkWk_expr,
                      MATMUL(VAL1(SysI.psik,k),VAL1(wk,k)));

            /* compute WkIkWk[k] */
            FLUSH_VEC(F, WkIkWk, k, WkIkWk_expr, 
                      CROSS(VAL1(wk,k),INDX(IkWk_expr,k)));
        }

    /* these are already clean */
    ASSIGN(IkWk, UNUSE(IkWk_expr));
    ASSIGN(WkIkWk, UNUSE(WkIkWk_expr));
}

/* COMPUTE_WkIkbWk
 *
 * Compute temporary variables 
 *            IkbWk[b] = ik[b]*wk[k]        
 *          WkIkbWk[b] = wk[k] X IkWk[b]
 * where b is a body whose inboard body is a weld joint and k is that
 * body's corresponding composite pseudobody.  Note that we use the inertia
 * of the BODY not the composite pseudobody.
 *
 * This is used in computation of tree weld joint reaction torques.
 *
 * This is an Order(n) calculation.
 */
void COMPUTE_WkIkbWk(FILE *F)
{
    register Index_t b,k;
    expr IkbWk_expr,WkIkbWk_expr;

    IkbWk_expr   = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    WkIkbWk_expr = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));

    for (b = 0; b < SysI.n; b++) {
        if (SysI.Bodies[b].jnt.JointKind != cWeldJoint)
            continue;

        if ((k = SysI.LastDOF[b]) < 0) {
            FLUSH_VEC(F, IkbWk, b, IkbWk_expr, VECTOR_ZERO());
            FLUSH_VEC(F, WkIkbWk, b, WkIkbWk_expr, VECTOR_ZERO());
        } else {
            /* compute IkbWk[b] */
            FLUSH_VEC(F, IkbWk, b, IkbWk_expr,
                      MATMUL(VAL1(SysI.ik,b),VAL1(wk,k)));

            /* compute WkIkbWk[k] */
            FLUSH_VEC(F, WkIkbWk, b, WkIkbWk_expr, 
                      CROSS(VAL1(wk,k),INDX(IkbWk_expr,b)));
        }
    }

    /* these are already clean */
    ASSIGN(IkbWk, UNUSE(IkbWk_expr));
    ASSIGN(WkIkbWk, UNUSE(WkIkbWk_expr));
}
