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


/* PRINT_SDLHS 
 *
 * Generate the sdlhs() subroutine.  This routine depends on PRINT_SDSTATE
 * having already been called.  The routine number of the user-called
 * routine which called sdlhs is passed in so error messages come out
 * right.
 *
 * In case we already happen to be in the ST_DERIVREADY state, the
 * call to sdlhs() puts us back into the ST_STATEREADY state since
 * its recalculation of the mass matrix and application of forces
 * invalidates and earlier-computed derivatives.
 */
void PRINT_SDLHS(FILE *F,
            opstats_t *opcnt)
{
    START_COMPSUB(opcnt);

    declare_proc(F, 0, "lhs",
      VT_INTEGER,      "routine", 
      0);

    efprintf(F, 
      "%{Compute all remaining state- and force-dependent quantities\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    ISET("roustate", ST_STATEREADY);

    if (sdfast_opt.formulation != OPT_ORDERN) {
        /* Compute mass matrix LDU decomposition */
        CALL("%Adommldu(routine)");
        opcnt->ndommldu++;
    } else {
        CALL("%Adomm(routine)");/* Just do the Order(N) gain computation */
        opcnt->ndomm++;
    }

    CALL0("%Adofs0");        /* Complete RHS ignoring constraints. */
    opcnt->ndofs0++;

    /* Note since no ops above, no END_COMPSUB subroutine footing here. */
    efprintf(F, Lang->proc_end);
    fflush(F);
}

/* Generate SDDOVPK() (Kane's Formulation) or SDDOPING() (otherwise).
 */
void PRINT_SDDOVPK(FILE *F,
              opstats_t *opcnt)
{
    START_COMPSUB(opcnt);

    /* SDDOVPK()
     *
     * These quantities (Wpk, Vpk) depend only on q's.  They do
     * not require recomputation if u's change or if new forces are
     * applied.  These are required only for Kane's Formulation.
     */
    if (sdfast_opt.formulation == OPT_KANE) {

        declare_proc(F, 0, "dovpk",
          0);
        efprintf(F, Lang->proc_dbegin);

        declare_sdginput_vars(F, DECL_NODSYM);
        declare_sdgstate_vars(F, DECL_NODSYM);
        declare_sdglhs_vars(F, DECL_NODSYM);
        declare_sdgtemp_vars(F, DECL_NODSYM);

        efprintf(F, Lang->proc_dend);
        efprintf(F, Lang->proc_sbegin);

        IF("vpkflg", EQ, "0")
          THEN

          efprintf(F, "%{\nCompute Wpk (partial angular velocities)\n%}");
          COMPUTE_Wpk(F);

          efprintf(F, "%{\nCompute Vpk (partial velocities)\n%}");
          COMPUTE_Vpk(F);

          ISET("vpkflg", 1);
        ENDIF;

        if (sdfast_opt.verbose)
            printf("Partial velocities computed. (CPU: %g MEM: %ld)\n",
              CPU_SECONDS() - gStartTime, BYTES_USED());
    } else {
        /* SDDOPING()
         *
         * These quantities (ping, hngpt) depend only on q's.  They do
         * not require recomputation if u's change or if new forces are
         * applied.  These are required only for Order(N) Formulation.
         */
        declare_proc(F, 0, "doping",
          0);
        efprintf(F, Lang->proc_dbegin);

        declare_sdginput_vars(F, DECL_NODSYM);
        declare_sdgstate_vars(F, DECL_NODSYM);
        declare_sdglhs_vars(F, DECL_NODSYM);

        efprintf(F, Lang->proc_dend);
        efprintf(F, Lang->proc_sbegin);

        IF("vpkflg", EQ, "0")
          THEN

          efprintf(F, "%{\nCompute ping (jt pins in ground frame)\n%}");
          COMPUTE_ping(F);

          efprintf(F, "%{\nCompute hngpt (hinge pts in ground frame)\n%}");
          COMPUTE_hngpt(F);

          ISET("vpkflg", 1);
        ENDIF;
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);
    fflush(F);
}

void PRINT_SDDOLTAU(FILE *F,
               opstats_t *opcnt)
{
    Index_t i;
    expr ltaufkx,ltautkx;

    START_COMPSUB(opcnt);

    /* SDDOLTAU() 
     * 
     * The quantities calculated here (ltaufk, ltautk) depend on q's, u's
     * and applied loop taus.  If any of these change, these computations
     * must be repeated.
     */
    declare_proc(F, 0, "doltau",
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{\nCompute effect of loop hinge torques\n%}");

    if (SysI.sl) {
        IF("ltauflg", EQ, "0")
        THEN
            ltaufkx = INUSE(NEW_1dARRAY(cVectorVal,SysI.n));
            ltautkx = INUSE(NEW_1dARRAY(cVectorVal,SysI.n));

            for (i=0; i < SysI.n; i++) {
                SINDX(ltaufkx, i, VECTOR_ZERO());
                SINDX(ltautkx, i, VECTOR_ZERO());
            }

            efprintf(F, "%{\nCompute forces due to loop taus\n%}");
            COMPUTE_ltauforces(F, VAL(ltau), 
                               tvec1, tvec2, tvec3,
                               ltaufi, ltaufo, ltauti, ltauto, 
                               ltaufkx, ltautkx);
            ASSIGN_CLN(F, ltaufk, ltaufkx);
            ASSIGN_CLN(F, ltautk, ltautkx);
            fflush(F);

            ISET("ltauflg", 1);
        ENDIF;
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);
    fflush(F);
}

void PRINT_SDDOINER(FILE *F,
               opstats_t *opcnt)
{
    START_COMPSUB(opcnt);

    /* SDDOINER() 
     * 
     * The quantities calculated here (Otk, Atk) depend
     * on q's and u's but do not have to be recalculated if new forces
     * are applied to the same set of q's and u's.
     */
    declare_proc(F, 0, "doiner",
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{\nCompute inertial accelerations and related temps\n%}");

    IF("inerflg", EQ, "0")
    THEN
      efprintf(F, "%{\nCompute Otk (inertial angular acceleration)\n%}");
      COMPUTE_Otk(F);
  
      efprintf(F, "%{\nCompute Atk (inertial linear acceleration)\n%}");
      COMPUTE_Atk(F);
      ISET("inerflg", 1);
    ENDIF;

    if (sdfast_opt.verbose)
          printf("Inertial accelerations computed. (CPU: %g MEM: %ld)\n",
            CPU_SECONDS() - gStartTime, BYTES_USED());

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);
    fflush(F);
}

void PRINT_SDDOFS0(FILE *F,
              opstats_t *opcnt)
{
    START_COMPSUB(opcnt);

    /* SDDOFS0() 
     * 
     * The quantities calculated here (Fstar, Tstar, fs0) depend on q's, u's
     * and applied loads.  If any of these change, these computations
     * must be repeated.
     */
    declare_proc(F, 0, "dofs0",
      0);
    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{\nCompute effect of all applied loads\n%}");

    IF("fs0flg", EQ, "0")
    THEN
      CALL0("%Adoltau");        /* Fk's and Tk's produced by loop taus. */
      opcnt->ndoltau++;
      CALL0("%Adoiner");        /* Inertial-force related variables */
      opcnt->ndoiner++;

      efprintf(F, "%{\nCompute Fstar (forces)\n%}");
      COMPUTE_Fstar(F); 

      efprintf(F, "%{\nCompute Tstar (torques)\n%}");
      COMPUTE_Tstar(F); 

      efprintf(F, "%{\nCompute fs0 (RHS ignoring constraints)\n%}");
      if (sdfast_opt.formulation == OPT_KANE) {
          CALL0("%Adovpk");         /* Wpk, Vpk */
          opcnt->ndovpk++;
      }
      COMPUTE_fs0(F);

      ISET("fs0flg", 1);
    ENDIF;

    if (sdfast_opt.verbose)
        printf("Inertial and applied forces computed. (CPU: %g MEM: %ld)\n",
               CPU_SECONDS() - gStartTime, BYTES_USED());

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);
    fflush(F);
}


/* Generate SDDOMM()
 *
 * Can sometimes fail during equation generation by detecting a singular
 * mass matrix.  In that case an appropriate message goes to stderr and
 * we return 1.  Otherwise we return 0.
 *
 * Maxaux says how many auxiliary Dyn files we can consume here in addition
 * to file F on which we are currently writing (this may be 0).  If we do
 * generate some auxiliary files we start numbering them from the passed-in
 * value of nxtaux, and we update nxtaux appropriately.
 */
int
PRINT_SDDOMM(FILE           *F,
             int          maxaux,
             char          *dynname,
             int       *nxtaux,
             opstats_t *opcnt)
{
    /* All the code in the routines generated in PRINT_LHS is counted
     * towards the opcounts in SDLHS().  Make sure all these routines
     * are actually called by SDLHS()!
     */

    START_COMPSUB(opcnt);

    /* SDDOMM() 
     *
     * Mass matrix computation depends only on q's.  It does not have
     * to be recomputed if u's or applied forces change.
     */

    /* We'll use different guts for this routine depending on the formulation.*/

    domm_head(F, -1);        /* print heading of main subroutine */

    IF("mmflg", EQ, "0")
    THEN

        if (SysI.s) {
            /* Calculate mass matrix or Order(N) equivalent, and check for
             * singularities.  Return 1 if a singularity is found symbolically. 
             */
            switch(sdfast_opt.formulation) {
                case OPT_KANE:        if (COMPUTE_kane_mm(F,"routine",
                                        maxaux,dynname,nxtaux))    return 1;
                                    break;
                case OPT_ORDERN:         if (COMPUTE_ordern_mm(F,"routine",
                                        maxaux,dynname,nxtaux))    return 1;
                                    break;
                case OPT_EXP:          if (COMPUTE_ordern_mm(F,"routine",
                                        maxaux,dynname,nxtaux))    return 1;
                                    break;
                case OPT_EXP2:         if (COMPUTE_ordern_mm(F,"routine",
                                        maxaux,dynname,nxtaux))    return 1;
                                    break;
            }
        }

        /*
         * Only set mass-matrix-ready-flag if no error is present
         * (being conservative). dumroutine and errnum declared in
         * domm_head()
         */

        CALL("%Aerror(%Rdumroutine,%Rerrnum)");

        IF("errnum", EQ, "0")
           THEN
               ISET("mmflg", 1);
        ENDIF;

    ENDIF;

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);

    domm_tail(F);
    return 0;
}

/* domm_head
 *
 * Print out the subroutine declaration and appropriate local declarations
 * for one of the subroutines used by SDDOMM().  The subroutine name is
 * SDDOMMnn() where nn is the auxiliary file number.  If the auxno is passed
 * in less than 0, we just generate SDDOMM(), the "main" routine.
 *
 * The file F should be the one named to match auxno.
 */
void domm_head(FILE *F,
          int auxno)
{
    char rouname[100];

    if (auxno >= 0)
        auxnoname(auxno, "domm", 100, rouname);
    else
        strcpy(rouname, "domm");

    declare_proc(F, 0, rouname,
      VT_INTEGER,        "routine",
      0);
    efprintf(F, Lang->proc_dbegin);

    /* Declare variables for checking the error state */
    declare_vars(F, 0,
      VT_INTEGER, "dumroutine", 
      VT_DUP,     "errnum", 
      0);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    if (SysI.s) {
        /* Declare appropriate local variables */
        switch(sdfast_opt.formulation) {
            case OPT_KANE:             kane_mm_decls(F);
                                break;
            case OPT_ORDERN:    ordern_mm_decls(F);
                                break;
            case OPT_EXP:              ordern_mm_decls(F);
                                break;
            case OPT_EXP2:             ordern_mm_decls(F);
                                break;
            default:
                fatal("unrecognized formulation -- SD/FAST bug");
        }
    }

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);
}

/* domm_tail
 *
 * Terminate the SDDOMMnn() subroutine. 
 */
void domm_tail(FILE *F)
{
    efprintf(F, Lang->proc_end);
    fflush(F);
}

void PRINT_SDDOMMLDU(FILE *F,
                opstats_t *opcnt)
{
    char str_s[10];
    esprintf(str_s,  "%d", SysI.s);

    START_COMPSUB(opcnt);

    /* SDDOMMLDU() 
     *
     * Decompose the mass matrix.  Makes sure a new mass matrix
     * has been calculated if necessary.  No need to generate this
     * with Order(N) since there is no actual mass matrix.
     */

    if (sdfast_opt.formulation != OPT_ORDERN) {

        declare_proc(F, 0, "dommldu",
          VT_INTEGER,        "routine",
          0);
        efprintf(F, Lang->proc_dbegin);

        declare_vars(F, 0,
          VT_INTEGER, "i",
          0);

        /* Declare variables for checking the error state */
        declare_vars(F, 0,
                     VT_INTEGER, "dumroutine", 
                     VT_DUP,     "errnum", 
                     0);

        declare_sdginput_vars(F, DECL_NODSYM);
        declare_sdglhs_vars(F, DECL_NODSYM);

        efprintf(F, Lang->proc_dend);
        efprintf(F, Lang->proc_sbegin);

        IF("mmlduflg", EQ, "0")
        THEN
          CALL("%Adomm(routine)");        /* Compute new mass matrix if needed. */
          efprintf(F, "%{\nNumerically decompose the mass matrix\n%}");
          CALL3("%Aldudcomp(%d,%d,mmap,%r,workss,works,%&mm,mlo,mdi)", 
              SysI.s, SysI.s, cEquationNegligible);
          efprintf(F, "%{\nCheck for singular mass matrix\n%}");
          FORCNT("100", "i", str_s);
            IFCOND efprintf(F, "mdi%(i%)%s%r", EQ, 0.0);
              THEN CALL1("%Aseterr(routine,%d)", ERR_SingularMassMatrix);
            ENDIF;
          ENDFOR("100");

          /*
           * Only set mass-matrix-ldu-ready-flag if no error is present
           * (being conservative)
           */
  
          CALL("%Aerror(%Rdumroutine,%Rerrnum)");

          IF("errnum", EQ, "0")
          THEN
            ISET("mmlduflg", 1);
          ENDIF;

        ENDIF;

        opcnt->ndomm++;
        opcnt->nldudcomp++;

        /* No ops above, so no END_COMPSUB footer here. */
        efprintf(F, Lang->proc_end);
        fflush(F);
    }
}

/* PRINT_MAT
 *
 * Generate the sdmassmat() and sdfrcmat() subroutine.  Sdmassmat()
 * computes (if necessary) and returns the mass matrix (LHS).  Sdfrcmat()
 * computes (if necessary) and returns the force matrix (RHS) excluding
 * effects of constraints (i.e., fs0).
 * PRINT_SDLHS must already have been called.
 *
 * Either routine can be called in state 2 (Kinematics Ready) or
 * state 3 (Dynamics Ready).  If new forces are added or u's change, 
 * sdfrcmat() will recalculate.  Sdmassmat() recalculates only if q changes.
 */
void PRINT_MAT(FILE *F)
{
    char str_s[10], str_s1[10];
    esprintf(str_s,  "%d", SysI.s);
    esprintf(str_s1, "%@d", SysI.s-1);

    declare_proc(F, 0, "massmat",
      VT_USER, &SysI.type_Arr_s_s,    "mmat",
      0);

    if (sdfast_opt.formulation != OPT_ORDERN)
        efprintf(F, "%{Return the system mass matrix (LHS)\n%}");
    else
        efprintf(F, "%{Calculate the system mass matrix\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0,
      VT_INTEGER, "i", 
      VT_DUP,     "j", 
      0);

    if (sdfast_opt.formulation == OPT_ORDERN) 
        declare_vars(F, 0,
          VT_USER, &SysI.type_Arr_s,         "udotin",
          VT_DUP,                        "mmrow",
          VT_DUP,                        "biastrq",
          0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdmassmat, ERR_sdstateMustBeCalledFirst);

    if (sdfast_opt.formulation != OPT_ORDERN) {
        CALL1("%Adomm(%d)", ROU_sdmassmat);

        FORCNT("110", "i", str_s);
          FOR("100", "j", "i", str_s1);
            SET("mmat%(i%,j%)", "mm%(i%,j%)");
            SET("mmat%(j%,i%)", "mm%(i%,j%)");
          ENDFOR("100");
        ENDFOR("110");
    } else {
        /* Order(N) */
        FORCNT("50", "i", str_s);
          RSET("udotin%(i%)", 0.);
        ENDFOR("50");
        CALL("%Acomptrq(udotin,biastrq)");
        FORCNT("110", "i", str_s);
          RSET("udotin%(i%)", 1.);
          CALL("%Acomptrq(udotin,mmrow)");
          RSET("udotin%(i%)", 0.);

          FOR("100", "j", "i", str_s1);
            SET("mmat%(i%,j%)", "mmrow%(j%)-biastrq%(j%)");
            SET("mmat%(j%,i%)", "mmat%(i%,j%)");
          ENDFOR("100");
        ENDFOR("110");

        efprintf(F, "%{\nCheck for singular mass matrix\n%}");
        FORCNT("200", "i", str_s);
          IFCOND efprintf(F, "mmat%(i%,i%)%s%r",LE,cEquationNegligible);
            THEN SETERR(F, ROU_sdmassmat, ERR_SingularMassMatrix);
          ENDIF;
        ENDFOR("200");
    }

    efprintf(F, Lang->proc_end);

    declare_proc(F, 0, "frcmat",
      VT_USER, &SysI.type_Arr_s,    "fmat",
      0);

    efprintf(F, 
      "%{Return the system force matrix (RHS), excluding constraints\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0,
      VT_INTEGER, "i", 
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdfrcmat, ERR_sdstateMustBeCalledFirst);

    CALL0("%Adofs0");

    FORCNT("100", "i", str_s);
       SET("fmat%(i%)", "fs0%(i%)");
    ENDFOR("100");

    efprintf(F, Lang->proc_end);
}

/* PRINT_SDREL2CART
 *
 * Generate the sdrel2cart() routine.  This converts a change in relative 
 * coordinate (a "u") to the consequent change in the cartesian coordinates
 * of a point.  Two vectors are returned, one for the change in translational
 * coordinates of a point on a body, and one for the change in rotational
 * coordinates (angular velocity) of the body itself.  This is a row in
 * the system Jacobian.
 *
 * This routine can be called in state 2 (Kinematics Ready) or
 * state 3 (Dynamics Ready).  
 * 
 * With Kane's Formulation, Vpk's and Wpk's are required and will be computed
 * here if they haven't already.  Otherwise, "ping" (pins in ground frame)
 * is required and will be computed here if necessary.
 */
void PRINT_SDREL2CART(FILE *F)
{
    declare_proc(F, 0, "rel2cart",
      VT_INTEGER,         "coord", 
      VT_DUP,            "body",
      VT_VECTOR,             "point", 
      VT_DUP,            "linchg",
      VT_DUP,            "rotchg",
      0);

    efprintf(F, 
     "%{Return derivative of pt loc and body orient w.r.t. hinge rate\n%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);

    declare_vars(F, 0,
      VT_INTEGER,  "x",
      VT_DUP,      "i",
      VT_DUP,      "gnd",
      VT_VECTOR,   "lin",
      VT_DUP,           "pv",
      0);
    if (sdfast_opt.formulation != OPT_KANE)
        declare_vars(F, 0,
          VT_VECTOR,   "pink",
          VT_DUP,      "ptvec",
          0);
    DECL_CHKBNUM(F);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "(coord%s%@d)%s(coord%s%@d)", 
                    LT, 0, OR_OP, GT, SysI.s-1);
    THEN
      SETERR(F,ROU_sdrel2cart,ERR_BadTreeCoordNum);
      RETURN;
    ENDIF;

    CHECK_BNUM(F, "body", ROU_sdrel2cart);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdrel2cart, ERR_sdstateMustBeCalledFirst);

    efprintf(F,"gnd%=%@d%;\n", cGroundBody);

    if (sdfast_opt.formulation == OPT_KANE) {
        int i;

        /* We can use Vpk's and Wpk's in this case -- much easier. */

        /* Find the last pseudobody associated with this body.  If it's
         * ground, we give it a pseudobody number one less than the
         * lowest coord allowed.  This also happens if the body is welded 
         * to ground.
         */
        IF("body",EQ,"gnd")
        THEN
          efprintf(F, "x%=%@d%;\n", -1);
        ELSE
          efprintf(F, "x%=firstq%(body%)+njntdof%(body%)-1%;\n");
        ENDIF;

        IF("x", LT, "coord")
        THEN
          CALL3("%Avset(%r,%r,%r,linchg)",0.,0.,0.);
          CALL3("%Avset(%r,%r,%r,rotchg)",0.,0.,0.);
          RETURN;
        ENDIF;

        CALL0("%Adovpk");

        FORCNT("100", "i", "3");
            efprintf(F, "rotchg%(i%)%=%s%(coord%,x%,i%)%;\n", PRINTNAME(Wpk));
            efprintf(F, "lin%(i%)%=%s%(coord%,x%,i%)%;\n", PRINTNAME(Vpk));
        ENDFOR("100");

        /* Shift pt loc from body COM relative to composite pseudobody COM. */
        IF("body",EQ,"gnd")
        THEN
          CALL("%Avcopy(point,pv)");
        ELSE
          for (i=0; i<3; i++) 
              efprintf(F,"pv%(%@d%)%=rcom%(body%,%@d%)+point%(%@d%)%;\n",i,i,i);
        ENDIF;

        CALL("%Avcross(rotchg,pv,linchg)");
        CALL("%Avadd(linchg,lin,linchg)");

    } else {
        /* We'll have to use ping and hngpt for Order(N) */

        /* assume 0 return */
        CALL3("%Avset(%r,%r,%r,linchg)",0.,0.,0.);
        CALL3("%Avset(%r,%r,%r,rotchg)",0.,0.,0.);

        /* Does coord lie on the path back from body to ground? 
         * This is complicated by the fact that the "inb" array is
         * indexed by real body number.
         */
        SET("i", "body");
        LOOP("100");
          IF("i", EQ, "gnd") THEN
            RETURN;
          ENDIF;
          SET("x", "firstq%(i%)");
          IF("x", LE, "coord")
          THEN
            IFCOND efprintf(F, "coord%sx+njntdof%(i%)", GE);
            THEN
              RETURN;
            ENDIF;
            BREAK("110");
          ENDIF;
          SET("i", "inb%(i%)");
        ENDLOOPBRK("100","110");

        CALL0("%Adoping");

        FORCNT("120", "i", "3");
            efprintf(F, "pink%(i%)%=%s%(coord%,i%)%;\n", PRINTNAME(ping));
            efprintf(F, "lin%(i%)%=%s%(coord%,i%)%;\n", PRINTNAME(hngpt));
        ENDFOR("120");

        CALL("%Atrans(gnd,pink,body,pink)");
        IF("trans%(coord%)", NE, "0")
        THEN
          CALL("%Avcopy(pink,linchg)");
        ELSE
          CALL("%Avcopy(pink,rotchg)");
          CALL("%Apos(body,point,ptvec)");
          CALL("%Avsub(ptvec,lin,ptvec)");
          CALL("%Atrans(gnd,ptvec,body,ptvec)");
          CALL("%Avcross(pink,ptvec,linchg)");
        ENDIF;
    }

    efprintf(F, Lang->proc_end);
}

/* PRINT_SDRHS 
 *
 * Generate the sdrhs() subroutine.  This routine depends on PRINT_SDLHS
 * having already been called.  It is expected that the mult and udot globals
 * contain correct values.
 *
 * The ludotx expression is filled in with calculated loop 
 * udot values for later use.  It should already have been 
 * allocated.
 *
 * At the end of this routine, we will be put into state ST_DERIVREADY
 * since everything needed to call routines like sdacc() will have been
 * calculated.
 */
void PRINT_SDRHS(FILE *F,
            expr ludotx,
            opstats_t *opcnt)
{
    long ldiv, lasg;
    expr aerrx;
    int i;

    START_COMPSUB(opcnt);
    ldiv = lasg = 0; /* # locally printed divides and assignments */

    /* Declare the SDRHS routine heading. */
    declare_proc(F, 0, "rhs",
      0);

    efprintf(F, "%{\n");
    PRINT_SUBR_STAMP(F);
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    fflush(F);

    efprintf(F, "%{\nCompute hinge torques for tree hinges\n%}");
    COMPUTE_tau(F);
    fflush(F);

    if (SysI.sl) {
        efprintf(F, "%{\nCompute hinge torques for loop joints\n%}");
        COMPUTE_ltau(F);
    }

    CALL0("%Adoiner");         /* Otk, Atk */

    efprintf(F, "%{\nCompute onk & onb (angular accels in N)\n%}");
    COMPUTE_onk(F);
    COMPUTE_onb(F);
    fflush(F);

    efprintf(F, "%{\nCompute acceleration dyadics\n%}");
    COMPUTE_dyad(F);
    fflush(F);

    efprintf(F, "%{\nCompute ank & anb (mass center linear accels in N)\n%}");
    COMPUTE_ank(F);
    COMPUTE_anb(F);
    fflush(F);

    /* Compute handy vector temporaries useful in subsequent loop-joint
     * related calculations here.
     */
    if (SysI.nl) {
        efprintf(F, "%{\nCompute loop joint temps\n%}");
        COMPUTE_latemps(F);

        efprintf(F, 
    "%{\nCompute loop joint accelerations\n%}");
        COMPUTE_Ooiob(F);
        COMPUTE_eulacc(F,ipin2x,tsc1,tsc2,tvec1,tvec2,tvec3,tvec4,tvec5);
        fflush(F);
        if (SysI.sl)
            COMPUTE_ludot(ludotx);
    }
    fflush(F);

    /* Acceleration constraint errors.  
     * Loop and user constraints are done here; prescribed motion is 
     * calculated directly in SDAERR().
     */

    /* Loop joint acceleration errors */
    if (SysI.nc) {
        efprintf(F, "%{\nCompute constraint acceleration errors\n%}");
        aerrx = INUSE(NEW_1dARRAY(cScalarVal, SysI.nc));

        /* set prescribed motion accel errors to 0 for now */
        for (i = 0; i < SysI.np; i++) 
            SINDX(aerrx, i, SCALAR_ZERO());

        if (SysI.nl)
            COMPUTE_laerr(aerrx); /* loop joints */
    }
    fflush(F);

    /* User constraint acceleration errors.
     * We put this at the end of SDRHS() so that everything that sduaerr()
     * needs to call sdacc(), for example, is already calculated. 
     */

    /* Note that sduaerr() may call routines like sdacc() which insist
       on being in state ST_DERIVREADY, so we go there now.  */
    ISET("roustate", ST_DERIVREADY);

    if (SysI.nu) {
        Index_t m;
        m = SysI.Const[0].Mindex;
        CALL1("%Auaerr(curtim,q,u,udot,%Raerr%(%@d%))", m);
        for (i=m; i<m+SysI.nu; i++)
            SINDX(aerrx, i, VREF1(aerr,i));
    }

    /* We're assigning the aerr symbol here.  That means that sdaerr()
     * MUST NOT be called from inside sduaerr().  (Of course that would
     * be meaningless anyway, but ...)
     */
    if (SysI.nc) 
        ASSIGN_CLN(F, aerr, UNUSE(aerrx));
    
    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,ldiv,lasg);
    efprintf(F, Lang->proc_end);

    if (sdfast_opt.verbose)
        printf("Accelerations computed. (CPU: %g MEM: %ld)\n",
          CPU_SECONDS() - gStartTime, BYTES_USED());
}

/* We use one routine to generate the three flavors of FS-computing 
   routines. */
typedef enum {cFS0, cFSmult, cFSfull, cFSgenMult, cFSgenFull} whichfs_t;

extern void PRINT_SDFSROUTINE(FILE* F, whichfs_t which, opstats_t* opcnt);


/* PRINT_SDFS0
 *
 * Generate the sdfs0() subroutine which sets the global FS equal to
 * the SDLHS-computed FS0 value.  
 *
 * Every element of FS is assigned here -- and subsequent access should be
 * numerical only, i.e., by name.
 *
 * This routine depends on PRINT_SDLHS having already been called.  
 */
void PRINT_SDFS0(FILE *F,
            opstats_t *opcnt)
{
    PRINT_SDFSROUTINE(F, cFS0, opcnt);
}

/* PRINT_SDFSMULT
 *
 * Generate the sdfsmult() subroutine.  Here we set the global FS to 
 * the force contribution from all the multiplier-generated forces.  We
 * only explicitly reference forces which are not known a priori.
 *
 * Every element of FS is assigned here -- and subsequent access should be
 * numerical only, i.e., by name.
 *
 * This routine depends on PRINT_SDLHS having already been called.  
 * SDFSMULT is not generated if there are no constraints.
 */
void PRINT_SDFSMULT(FILE *F,
               opstats_t *opcnt)
{
    PRINT_SDFSROUTINE(F, cFSmult, opcnt);
}

/* PRINT_SDFSFULL
 *
 * Generate the sdfsfull() subroutine.  Here we set the global FS to 
 * the force contribution from all the multiplier-generated forces PLUS
 * the SDLHS-computed FS0 value.
 *
 * Every element of FS is assigned here -- and subsequent access should be
 * numerical only, i.e., by name.
 *
 * This routine depends on PRINT_SDLHS having already been called.  
 * SDFSFULL is not generated if there are no constraints.
 */
void PRINT_SDFSFULL(FILE *F,
               opstats_t *opcnt)
{
    PRINT_SDFSROUTINE(F, cFSfull, opcnt);
}

/* PRINT_SDFSGENMULT
 *
 * Generate the sdfsgenmult() subroutine.  Here we set the global FS to 
 * the force contribution from all the multiplier-generated forces.  All
 * multiplier forces are referenced symbolically since we can't know in
 * advance how they will be set during assembly analysis.  Otherwise this
 * is the same as SDFSMULT.
 *
 * Every element of FS is assigned here -- and subsequent access should be
 * numerical only, i.e., by name.
 *
 * This routine depends on PRINT_SDLHS having already been called.  
 * SDFSGENMULT is always generated since constraints may be added by
 * the user at run time for assembly analysis.
 */
void PRINT_SDFSGENMULT(FILE *F)
{
    opstats_t opcnt;
    PRINT_SDFSROUTINE(F, cFSgenMult, &opcnt);
}

/* PRINT_SDFSGENFULL
 *
 * Generate the sdfsgenfull() subroutine.  Here we set the global FS to 
 * the force contribution from all the multiplier-generated forces PLUS
 * the SDLHS-computed FS0 value.  This is the same as SDFSFULL() above
 * except that SDFSGENMULT() is used instead of SDFSMULT().
 *
 * Every element of FS is assigned here -- and subsequent access should be
 * numerical only, i.e., by name.
 *
 * This routine depends on PRINT_SDLHS having already been called.  
 * SDFSGENFULL is always generated.
 */
void PRINT_SDFSGENFULL(FILE *F)
{
    opstats_t opcnt;
    PRINT_SDFSROUTINE(F, cFSgenFull, &opcnt);
}

void PRINT_SDFSROUTINE(FILE *F,
                  whichfs_t which,
                  opstats_t *opcnt)
{
    Index_t i;
    expr fs_expr;
    sym lmfk,lmtk,lmtau;
    long lasg;
    char str_s[10];

    esprintf(str_s,  "%d", SysI.s);

    START_COMPSUB(opcnt);
    lasg = 0;

    /* Declare the routine heading. */
    switch (which) {
        case cFS0: declare_proc(F, 0, "fs0", 0);
                   break;
        case cFSmult: declare_proc(F, 0, "fsmult", 0);
                      break;
        case cFSfull: declare_proc(F, 0, "fsfull", 0);
                      break;
        case cFSgenMult: declare_proc(F, 0, "fsgenmult", 0);
                      break;
        case cFSgenFull: declare_proc(F, 0, "fsgenfull", 0);
                      break;
    }

    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    if (SysI.nc && which == cFSgenMult)
        declare_vars(F, DECL_NOPRINT,
           VT_USER|VT_DSYM, &SysI.type_Vec_n, PRINTNAME(mfk),  &lmfk,
           VT_DUP|VT_DSYM,                    PRINTNAME(mtk),  &lmtk,
           VT_USER|VT_DSYM, &SysI.type_Arr_s, PRINTNAME(mtau), &lmtau,
           0);

    if (!SysI.nc && (which == cFSmult || which == cFSgenMult))
        declare_vars(F, 0,
           VT_INTEGER, "i",
           0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    fflush(F);

    switch (which) {
        case cFS0:
            efprintf(F, 
                "%{\nCompute Fs (ignoring multiplier forces)\n%}");
            for (i=0; i<SysI.s; i++)
                PRINT_ASSN1(F, PRINTNAME(fs), i, VAL1(fs0, i));
            break;
        case cFSmult:
            efprintf(F, 
                "%{\nCompute Fs (multiplier-generated forces only)\n%}");
            if (SysI.nc) {
                fs_expr = INUSE(NEW_1dARRAY(cScalarVal, SysI.s?SysI.s:1));
                if (sdfast_opt.formulation == OPT_KANE)
                    CALL0("%Adovpk");         /* Wpk, Vpk */

                COMPUTE_fsmult(F, mfk, mtk, mtau, fs, fs_expr);
                /* constant fs elements haven't been output yet */
                for (i=0; i<SysI.s; i++)
                    if (IS_CONST(INDX(fs_expr,i)))
                        PRINT_ASSN1(F, PRINTNAME(fs), i, INDX(fs_expr, i));
                DISPOSE_EXPR(UNUSE(fs_expr));
            } else {
                FORCNT("100", "i", str_s);
                  efprintf(F, "%s%(i%)%=%r%;\n", PRINTNAME(fs), 0.);
                ENDFOR("100");
                lasg += SysI.s;
            }
            break;
        case cFSfull:
            efprintf(F,
                "%{\nCompute Fs (including all forces)\n%}");
            CALL0("%@Afsmult");
            for (i=0; i<SysI.s; i++)
                PRINT_ASSN1(F, PRINTNAME(fs), i,
                    ADD(VREF1(fs,i), VAL1(fs0,i)));
            break;
        case cFSgenMult:
            efprintf(F, 
                "%{\nCompute Fs (generic multiplier-generated forces)\n%}");
            if (SysI.nc) {
                fs_expr = INUSE(NEW_1dARRAY(cScalarVal, SysI.s?SysI.s:1));
                if (sdfast_opt.formulation == OPT_KANE)
                    CALL0("%Adovpk");         /* Wpk, Vpk */
                COMPUTE_fsmult(F, lmfk,lmtk,lmtau, fs, fs_expr);
                /* constant fs elements haven't been output yet */
                for (i=0; i<SysI.s; i++)
                    if (IS_CONST(INDX(fs_expr,i)))
                        PRINT_ASSN1(F, PRINTNAME(fs), i, INDX(fs_expr, i));
                DISPOSE_EXPR(UNUSE(fs_expr));
            } else {
                FORCNT("100", "i", str_s);
                  efprintf(F, "%s%(i%)%=%r%;\n", PRINTNAME(fs), 0.);
                ENDFOR("100");
                lasg += SysI.s;
            }
            break;
        case cFSgenFull:
            efprintf(F,
                "%{\nCompute Fs (incl generic mult & other forces)\n%}");
            CALL0("%@Afsgenmult");
            for (i=0; i<SysI.s; i++)
                PRINT_ASSN1(F, PRINTNAME(fs), i,
                    ADD(VREF1(fs,i), VAL1(fs0,i)));
            break;
    }
    fflush(F);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,lasg);
    efprintf(F, Lang->proc_end);
}

/* PRINT_SDREAC
 *
 * Generate the sdreac() subroutine.  This routine depends on 
 * PRINT_SDMFRC, PRINT_SDLHS and PRINT_SDRHS having already been called.
 */
void PRINT_SDREAC(FILE *F,
             opstats_t *opcnt)
{
    Index_t i,j,rbod;
    sym            frc, trq;
    expr    fexpr, texpr;

    START_COMPSUB(opcnt);

    /* Declare the SDFORCES routine heading. */
    declare_proc(F, 0, "reac",
      VT_USER|VT_DSYM, &SysI.type_Vec_nj,  "force",  &frc,
      VT_DUP|VT_DSYM,                      "torque", &trq,
      0);

    efprintf(F, "%{\n");
    PRINT_SUBR_STAMP(F);
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate,%Aglhs,%Agrhs do\n");
        efprintf(F,"%>");
    }

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE, 
                ROU_sdreac, ERR_sdderivMustBeCalledFirst);

    efprintf(F, "%{\nCompute reaction forces for non-weld tree joints\n%}");
    COMPUTE_reaction(F);
    fflush(F);

    for (i=0; i < SysI.n; i++) {
        if (SysI.Bodies[i].jnt.JointKind == cWeldJoint)
            continue;
        rbod = SysI.LastDOF[i];
        for (j=0; j<3; j++) {
            PRINT_ASSN2(F, "force", i, j, INDX(VAL1(fc,rbod),j));
            PRINT_ASSN2(F, "torque", i, j, INDX(VAL1(tc,rbod),j));
        }
    }

    if (SysI.nl) {
        efprintf(F, "%{\nCompute reaction forces for loop joints\n%}");

        for (i=0; i < SysI.nl; i++) {
            JointKind_t jk;

            jk = SysI.LoopConst[i].jnt.JointKind;

            /* get the loop tau contributions to the outboard reaction
               loads into fexpr and texpr */
            switch (jk) {
            case cPinJoint:
            case cUjoint:
            case c3dJoint:
            case cBallJoint:
                fexpr = VECTOR_ZERO();
                texpr = ADD(VAL1(ltauto, i),VAL1(mltauto, i));
                break;
            
            case cSlidingJoint:
                fexpr = ADD(VAL1(ltaufo, i),VAL1(mltaufo, i));
                texpr = VECTOR_ZERO();
                break;

            case c6dJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cBearingJoint:
            case cBushingJoint:
                fexpr = INUSE(ADD(VAL1(ltaufo, i),VAL1(mltaufo, i)));
                texpr = SUB(ADD(VAL1(ltauto, i),VAL1(mltauto, i)),
                            CROSS(VAL1(SysI.lbtj,i),fexpr));
                fexpr = UNUSE(fexpr);
                break;

            case cWeldJoint:
                fexpr = VECTOR_ZERO();
                texpr = VECTOR_ZERO();
                break;
            }

            fexpr = INUSE(ADD(fexpr, VAL1(lfc,i)));
            texpr = INUSE(ADD(texpr, VAL1(ltc,i)));

            for (j=0; j<3; j++) {
                PRINT_ASSN2(F, "force", SysI.n+i, j, INDX(fexpr, j));
                PRINT_ASSN2(F, "torque", SysI.n+i, j, INDX(texpr, j));
            }

            DISPOSE_EXPR(UNUSE(fexpr));
            DISPOSE_EXPR(UNUSE(texpr));
        }
    }

    efprintf(F, "%{\nCompute reaction forces for tree weld joints\n%}");

    /* Make sure outboard bodies are done first so their reactions can
     * be used in computing tne more inboard ones.
     */
    for (i=SysI.n-1; i >= 0; i--) {
        if (SysI.Bodies[i].jnt.JointKind != cWeldJoint)
            continue;

        COMPUTE_weld_reaction(F, i, frc, trq, &fexpr, &texpr);

        fexpr = INUSE(fexpr);
        texpr = INUSE(texpr);

        for (j=0; j<3; j++) {
            PRINT_ASSN2(F, "force",  i, j, INDX(fexpr,j));
            PRINT_ASSN2(F, "torque", i, j, INDX(texpr,j));
        }

        DISPOSE_EXPR(UNUSE(fexpr));
        DISPOSE_EXPR(UNUSE(texpr));
    }

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    END_COMPSUB(F,opcnt,0L,0L,0L,0L);
    efprintf(F, Lang->proc_end);

    fflush(F);

    if (sdfast_opt.verbose)
        printf("Reaction forces computed. (CPU: %g MEM: %ld)\n",
          CPU_SECONDS() - gStartTime, BYTES_USED());
}
