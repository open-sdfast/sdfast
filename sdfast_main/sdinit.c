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

/* These declarations are for the fussy SGI compiler which
 * can't find these static routines otherwise.
 */
static void house(FILE *F,
                  sym invar,
                  int jt,
                  sym  sumsq,
                  sym  tmpv, 
                  sym outvar,
                  int cl_flag,
                  expr outvarx);
static int norm_pin(FILE *F,
                    JointDesc_t *jntp,
                    int i,
                    sym pinvar,
                    int pindx,
                    int errnum,
                    int jointno,
                    char *what,
                    char  *name,
                    sym sumsq,
                    sym norminv,
                    expr pin_x);
static int check_perp(FILE *F,
                      JointDesc_t *jntp,
                      int pin1,
                      int pin2,
                      int errnum,
                      int jointno,
                      char *what,
                      char  *name);

/* DECLARE_GLOBALS
 *
 * Print out everything that goes at the beginning of the Dynamics File,
 * before the first line of the first generated subroutine.  Since FORTRAN
 * doesn't have globals, nothing gets printed here except the BLOCK DATA 
 * initialization if the language is FORTRAN.
 * Otherwise, types, structure declarations, and initializations are output
 * here.  
 * 
 * Initializations can be suppressed by setting init to 0 in the call.  This
 * is used if the dyn file is being split into multiple files.
 */
void DECLARE_GLOBALS(FILE *F,
                int  init)
{
    int nosym = (init ? 0 : DECL_NODSYM);
    int extglo = (init ? DECL_GLOBAL : DECL_EXTERN);

    /* the internal sd/calc symbols will be defined here for languages
       which don't have common.  Don't reinitialize the global symbols
       if we're not init'ing here. */

    declare_sdgtopo_vars (F, DECL_STRUCT | nosym);
    declare_sdginput_vars(F, DECL_STRUCT | nosym);
    declare_sdgstate_vars(F, DECL_STRUCT | nosym);
    declare_sdglhs_vars  (F, DECL_STRUCT | nosym);
    declare_sdgrhs_vars  (F, DECL_STRUCT | nosym);
    declare_sdgtemp_vars (F, DECL_STRUCT | nosym);

    if (!(Lang->flags & LANG_COMMON_DECL))
        efprintf(F, "\n");

    if (Lang == &Pascal_language)
        efprintf(F, "var\n%>");

    if (init) {
        declare_sdgtopo_vars(F, DECL_GLOBAL|DECL_NODSYM|DECL_INIT);
        init_sdgtopo(F, DECL_GLOBAL);

        declare_sdginput_vars(F, DECL_GLOBAL|DECL_NODSYM|DECL_INIT);
        init_sdginput(F, DECL_GLOBAL);
    } else {
        declare_sdgtopo_vars(F, DECL_EXTERN|DECL_NODSYM);
        declare_sdginput_vars(F, DECL_EXTERN|DECL_NODSYM);
    }

    declare_sdgstate_vars(F, extglo|DECL_NODSYM);
    declare_sdglhs_vars  (F, extglo|DECL_NODSYM);
    declare_sdgrhs_vars  (F, extglo|DECL_NODSYM);
    declare_sdgtemp_vars (F, extglo|DECL_NODSYM);

    if (Lang == &Pascal_language)
        efprintf(F, "%<");
    if (!(Lang->flags & LANG_COMMON_DECL))
        efprintf(F, "\n");

    if (SysI.s > 1) {
        declare_sdldu_proc(F, DECL_FORWARD);
        declare_sdbsolv_proc(F, DECL_FORWARD);
    }
    if (SysI.nc > 1) 
        declare_sdsolvc_proc(F, DECL_FORWARD);

    /* make block data for FORTRAN */
    if (init && (Lang->flags & LANG_COMMON_DECL)) {
        efprintf(F, "block data %Ablkdata\n");
        declare_sdgtopo_vars(F, 0);
        declare_sdginput_vars(F, 0);
        init_sdgtopo(F, 0);
        init_sdginput(F, 0);
        efprintf(F, "end\n");
    }
}

/* DECLARE_LIB_GLOBALS
 *
 * Print out everything that goes at the beginning of the Library File,
 * before the first line of the first generated subroutine.  Since FORTRAN
 * doesn't have globals, nothing gets printed here if the language is FORTRAN.
 * Otherwise, types, structure declarations, and initializations are output
 * here.
 */
void DECLARE_LIB_GLOBALS(FILE *F)
{
    /* the internal sd/calc symbols will be defined here for languages
       which don't have common */
    declare_sdgerror_vars (F, DECL_STRUCT);

    if (!(Lang->flags & LANG_COMMON_DECL))
        efprintf(F, "\n");

    if (Lang == &Pascal_language)
        efprintf(F, "var\n%>");

    declare_sdgerror_vars(F, DECL_GLOBAL|DECL_NODSYM|DECL_INIT);
    init_sdgerror(F, DECL_GLOBAL);

    if (Lang == &Pascal_language)
        efprintf(F, "%<");
    if (!(Lang->flags & LANG_COMMON_DECL))
        efprintf(F, "\n");

    /* make block data for FORTRAN */
    if (Lang->flags & LANG_COMMON_DECL) {
        efprintf(F, "block data %Alibdata\n");
        declare_sdgerror_vars(F, 0);
        init_sdgerror(F, 0);
        efprintf(F, "end\n");
    }
}

static void 
SDINIT_HEADING(FILE *F,
               int adsim_flag)
{
    /* Print the initialization routine. */

    if (!adsim_flag)
        declare_proc(F, 0, "init", 
          0);
    efprintf(F, "%{\n\
Initialization routine\n\n");
    efprintf(F, "\n\
 This routine must be called before the first call to sdstate(), after\n\
 supplying values for any `?' parameters in the input.\n");
    efprintf(F, "%}");
}

/* PRINT_SDINIT 
 *
 * Generate the sdinit() routine.
 * Computes mtot, all the pin vectors, and pseudobody mass properties.
 *
 * Returns non-zero if we run into an Input File error.  (Some of those
 * can't be detected until we get this far.)
 */ 
int
PRINT_SDINIT(FILE *F)
{
    int  b,i,j,k,thentoo,elsetoo;
    expr mtot_expr, Ikox, mkrkx, tmpx, tmpv;
    expr iperp_expr, operp_expr, pin_x, cond;
    expr ipin_x, ipin2_x, iref_x, opin_x, oref_x, ghand_x;
    sym sumsq,norminv,whichsym;
    JointDesc_t *jntp;
    BodyDesc_t  *w;
    char *what, *name, str_n[10], str_nl[10], str_s[10], str_s1[10], str_sl[10];
    int pinno,errnum,firstdof,startdof;
    extern int gProgramSerialNo;

    /* Print the initialization routine. */
    struct language *Lang_save = Lang;

    esprintf(str_n, "%d", SysI.n);
    esprintf(str_nl, "%d", SysI.nl);
    esprintf(str_s, "%d", SysI.s);
    esprintf(str_s1, "%@d", SysI.s-1);
    esprintf(str_sl, "%d", SysI.sl);

    if (Lang == &ADSIM_language) {
        fprintf(F, "FORTRAN_SECTION declare\nFORTRAN\n");
        SET_LANGUAGE(&FORTRAN_language);
        SDINIT_HEADING(F, 1);
        efprintf(F, Lang->proc_dbegin);
        declare_sdgtopo_vars(F, 0);
        declare_sdginput_vars(F, 0);
        fprintf(F, "\nEND FORTRAN\n\n");
        fprintf(F, "FORTRAN_SECTION start\nFORTRAN\n");
    } else {
        SDINIT_HEADING(F, 0);
        efprintf(F, Lang->proc_dbegin);
        declare_sdgtopo_vars(F, 0);
        declare_sdginput_vars(F, 0);
    }

    declare_vars(F, 0,
        VT_REAL|VT_DSYM, "sumsq", &sumsq,
        VT_DUP|VT_DSYM, "norminv", &norminv,
        VT_INTEGER, "i",
        VT_DUP,     "j",
        VT_DUP,     "k",
        0);

    declare_sdgstate_vars(F, 0);
    declare_sdglhs_vars(F, 0);
    declare_sdgrhs_vars(F, 0);
    declare_sdgtemp_vars(F, 0);

    /* VAX seems to get confused sometimes without this. */
    if (Lang==&FORTRAN_language)
        efprintf(F,"external %Ablkdata%;\n");

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"with %Aginput,%Agstate do\n");
        efprintf(F,"%>");
    }

    /* Sdinit can be called from any execution state. */

    /* Make sure all question mark parameters that have no defaults have
     * been specified.   Prescribed motion ? parameters are checked
     * in sdstate(); Baumgarte stabilization parameters are checked in
     * sdderiv() and sdresid().
     */

    efprintf(F, "\n\
%{Check that all `?' parameters have been assigned values%}\n");

    /* gravity */

    FORCNT("100", "k", "3");
      IFCOND efprintf(F, "gravq%(k%)%s%d", EQ, ISQUESFLG);
        THEN SETERR(F, ROU_sdinit, ERR_GravityMustBeSpecified);
      ENDIF;
    ENDFOR("100");

    /* mass, inertia, tree bodytojoint, inbtojoint */

    FORCNT("140", "k", str_n);
      IFCOND efprintf(F, "mkq%(k%)%s%d", EQ, ISQUESFLG);
        THEN SETERR(F, ROU_sdinit, ERR_MassMustBeSpecified);
      ENDIF;
      FORCNT("130", "i", "3");
        IFCOND efprintf(F, "rkq%(k%,i%)%s%d", EQ, ISQUESFLG);
          THEN SETERR(F, ROU_sdinit, ERR_TreeBtjMustBeSpecified);
        ENDIF;
        IFCOND efprintf(F, "riq%(k%,i%)%s%d", EQ, ISQUESFLG);
          THEN SETERR(F, ROU_sdinit, ERR_TreeItjMustBeSpecified);
        ENDIF;
        FORCNT("120", "j", "3");
          IFCOND efprintf(F, "ikq%(k%,i%,j%)%s%d", EQ, ISQUESFLG);
            THEN SETERR(F, ROU_sdinit, ERR_InertiaMustBeSpecified);
          ENDIF;
        ENDFOR("120");
      ENDFOR("130");
    ENDFOR("140");

    /* tree pins */

    FORCNT("160", "k", str_s);
      FORCNT("150", "i", "3");
        IFCOND efprintf(F, "pinq%(k%,i%)%s%d", EQ, ISQUESFLG);
          THEN SETERR(F, ROU_sdinit, ERR_TreePinMustBeSpecified);
        ENDIF;
      ENDFOR("150");
    ENDFOR("160");

    if (SysI.nl) {

        /* loop stuff */
    
        FORCNT("210", "k", str_nl);
          FORCNT("200", "i", "3");
            IFCOND efprintf(F, "inbpin1q%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopIpinMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "inbpin2q%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopIpinMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "inbpin3q%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopIpinMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "inbrefq%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopIrefMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "bodypinq%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopBpinMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "bodyrefq%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopBrefMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "lbtjq%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopBtjMustBeSpecified);
            ENDIF;
            IFCOND efprintf(F, "litjq%(k%,i%)%s%d", EQ, ISQUESFLG);
              THEN SETERR(F, ROU_sdinit, ERR_LoopItjMustBeSpecified);
            ENDIF;
          ENDFOR("200");
        ENDFOR("210");
    }

    efprintf(F, "\n%{Normalize pin vectors if necessary%}\n");

    /* tree pins 
     *
     * At this point, the SysI.pin "global" symbol has no value.
     * Here we're going to assign values to SysI.pin, including normalizing 
     * if necessary.  Note that there is an element of SysI.pin corresponding
     * to every degree of freedom of every joint.  There is not necessarily
     * a one-to-one mapping of pins from the input file to joint dof's.  For 
     * example, a cylinder joint has two dofs but only one input pin; that pin
     * is duplicated in SysI.pin to provide an axis for each dof.  
     * Duplicate SysI.pin entries for pins with question marks simply refer
     * to the original entry; that is, if pin[i] is a duplicate of pin[j]
     * the generated code will contain no references to pin[i] since the
     * pin[i] values will have been set to appropriate VREFs to pin[j].
     * SysI.pin entries corresponding to ball joint dof's are set to 0.
     *
     * Because of the above, the runtime global variable which contains
     * the current values of the pin axes is only correct for the "original"
     * pins.  The "duplicates" are never set and should never be referenced.
     * SDPIN/SDGETPIN only set/reference the originals.
     */
    if (SysI.s == 0)
        goto noTreePins;

    pin_x = PERM(NEW_1dARRAY(cVectorVal, SysI.s));
    for (b = 0; b < SysI.n; b++) {
        int nmax;

        jntp = &SysI.Bodies[b].jnt;
        what = "body";
        name = SysI.Bodies[b].BodyName;
        firstdof = SysI.FirstDOF[b];

        /* Point startdof at the jt dof corresponding to the 1st 
         * "original" pin.  This is the first dof for all joints but
         * reverse 6dof and non-reverse bearing.
         */
        startdof = firstdof;
        if (jntp->JointKind == cRev6dJoint)
            startdof += 3; /* start with the first sliding axis */
        else if (jntp->JointKind == cBearingJoint)
            startdof++; /* start with the first rotational axis */
        
        /* Now fill in all "original" pins.  Note that tree weld joint
         * allows no pins, while loop weld does so nmax will be wrong here.
         */
        nmax = JointInfo[(int)jntp->JointKind].nmax;
        if (jntp->JointKind == cWeldJoint)
            nmax = 0;
        for (i = 0; i < nmax; i++) {
            if (!jntp->Pins[i]) {
                /* missing optional pin -- default to b1,b2,b3 */
                if      (i==0) jntp->Pins[i] = B1();
                else if (i==1) jntp->Pins[i] = B2();
                else if (i==2) jntp->Pins[i] = B3();
                /* i is never > 2 */
                jntp->PinFlg[i][0]=jntp->PinFlg[i][1]=jntp->PinFlg[i][2] = 0;
            }
            /* First set `?' pin elements to the appropriate VREFs */
            for (j = 0; j < 3; j++)
                if (jntp->PinFlg[i][j] & ISQUESFLG)
                    SINDX(jntp->Pins[i], j, 
                          INDX(VREF1(SysI.pin, startdof + i), j));

            /* Normalize this pin, putting the normalized, IS_SIMPLE 
               value in the appropriate slot of pin_x. */
            if (norm_pin(F, jntp, i, SysI.pin, startdof+i, ERR_ZeroTreePin,
                         b, what, name, sumsq, norminv, pin_x))
                return 1;
        }

        /* Zero out any "pins" corresponding to ball joints. */
        if (JointInfo[(int)jntp->JointKind].hasball)
            for (i=0; i<JointInfo[(int)jntp->JointKind].dof; i++)
                if (JointInfo[(int)jntp->JointKind].doftype[i] == AX_BALL)
                    SINDX(pin_x, firstdof+i, VECTOR_ZERO());
                
        /* Now fill in the duplicate pins to reference the originals. */
        switch (jntp->JointKind) {
            case cCylJoint: /* 2nd axis is copy of first */
                SINDX(pin_x, firstdof+1, INDX(pin_x, firstdof));
                break;

            case cBushingJoint:    /* last 3 axes are copies of 1st 3 axes */
            case cRevBushingJoint:
                for (i=0; i<3; i++)
                    SINDX(pin_x, firstdof+3+i, INDX(pin_x, firstdof+i));
                break;

            case cBearingJoint:    /* first axis is copy of second */
                SINDX(pin_x, firstdof, INDX(pin_x, firstdof+1));
                break;

            case cRevBearingJoint: /* last (fourth) axis is copy of third */
                SINDX(pin_x, firstdof+3, INDX(pin_x, firstdof+2));
                break;

            default: /* nothing more to do */
                break;
        }
    }
    ASSIGN(SysI.pin, pin_x); /* no need to cleanvar */

  noTreePins:

    if (SysI.nl == 0)
        goto skiploops;

    /* Now compute loop pins.
     * Internally, we have a different model for loop joints than we
     * present to the user.  Externally, we try to make loop joints
     * look as much like tree joints as possible.  Internally, they
     * are totally unrelated.  Consequently, the user's idea of 
     * pin's and reference lines do not map nicely to the internal
     * variables.  Externally, for a joint providing d degrees of freedom,
     * there are d pins attached to the inboard body (inbpins), except for ball
     * joints which don't have pins, and welds which can have a pin even though
     * there is no degree of freedom.  (Actually sometimes one pin is used
     * for multiple dofs, as in cylinder, bearing and bushing joints.)
     * There are sometimes an optional 
     * pin on the outboard body (bodypin) and two optional reference
     * lines (inbref and bodyref).  Internally, we have the following
     * parameters to describe loop joints: ipin, ipin2, iref, iperp, opin, 
     * oref, operp.  Ipin2 is used only for joints containing gimbals.
     *
     * Warning: do not assume that there is a direct correspondence between
     * the user supplied pins (e.g. INBPIN2) and the internal `pins'
     * (e.g. ipin2).  The internal parameters are loaded from the external
     * ones as appropriate for the type of joint, without regard for 
     * similarity in the names.
     *
     * Ordering is important here since the defaults for some vectors are
     * that they be the same as some already-calculated vector.
     */

    what = "loop joint";

    /* First we'll check that the user's input vectors meet all the
     * perpendicularity constraints.  These are as follows:
     *    inbpin1 -- no restriction
     *    inbpin2 -- must be perp. to inbpin1
     *    inbpin3 -- must be perp. to inbpin2 
     *            -- for planar, sixdof, & bushing must also be perp. to inbpin1
     *    inbref  -- (pin,slider,cyl,weld only) must be perp. to inbpin1
     *    bodypin -- no restriction
     *    bodyref -- must be perp. to bodypin
     */
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        for (i = 0; i < JointInfo[(int)jntp->JointKind].nmax; i++)
            if (!jntp->Pins[i]) {
                /* missing optional pin -- default to b1,b2,b3 */
                if      (i==0) jntp->Pins[i] = B1();
                else if (i==1) jntp->Pins[i] = B2();
                else if (i==2) jntp->Pins[i] = B3();
                /* i is never > 2 */
                jntp->PinFlg[i][0]=jntp->PinFlg[i][1]=jntp->PinFlg[i][2] = 0;
            }
        if (jntp->Pins[INBPIN2] 
            && check_perp(F, jntp, INBPIN1, INBPIN2, ERR_Inbpin12notPerp, 
                          SysI.n+b, what, name))
            return 1;
        if (jntp->Pins[INBPIN3]) {
            if (check_perp(F, jntp, INBPIN2, INBPIN3, ERR_Inbpin23notPerp, 
                           SysI.n+b, what, name))
                return 1;
            if ((jntp->JointKind == cPlanarJoint || jntp->JointKind == c6dJoint
                 || jntp->JointKind == cBushingJoint)
                && check_perp(F, jntp, INBPIN1, INBPIN3, ERR_Inbpin13notPerp, 
                              SysI.n+b, what, name))
                return 1;
        }
        if (jntp->Pins[INBREF]
            && check_perp(F, jntp, INBPIN1, INBREF, ERR_Inbpin1refNotPerp, 
                          SysI.n+b, what, name))
            return 1;
        if (jntp->Pins[BODYREF]
            && check_perp(F, jntp, BODYPIN, BODYREF, ERR_BodypinRefNotPerp, 
                          SysI.n+b, what, name))
            return 1;
    }

    /* Loop ipin:
     *   Ball
     *     zero
     *   Planar
     *     third input file `pin'
     *   Pin,Slider,Ujoint,Gimbal,Cylinder,Bearing,Bushing,Sixdof
     *     first input file `pin'
     *   Weld
     *     first input file `pin', if present, else [1,0,0]
     */
    ipin_x = PERM(NEW_1dARRAY(cVectorVal, SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];
        
        switch (jntp->JointKind) {
            case cBallJoint: 
                SINDX(ipin_x, b, VECTOR_ZERO());
                break;

            case cPlanarJoint:
                pinno = INBPIN3;
                whichsym = SysI.inbpin3;
                errnum = ERR_ZeroLoopInbPin3;
                goto getipin;

            default:
                pinno = INBPIN1;
                whichsym = SysI.inbpin1;
                errnum = ERR_ZeroLoopInbPin1;
            getipin:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[pinno][j] & ISQUESFLG)
                        SINDX(jntp->Pins[pinno], j, 
                              INDX(VREF1(whichsym, b), j));
                if (norm_pin(F, jntp, pinno, ipin, b, errnum, 
                             SysI.n+b, what, name, sumsq, norminv, ipin_x))
                    return 1;
                break;
        }
    }
    ASSIGN(ipin, ipin_x); 

    /* Loop ipin2: (gimbal only)
     *   Gimbal, Bearing, Bushing
     *     second input file `pin'
     *   Others
     *     zero
     */
    ipin2_x = PERM(NEW_1dARRAY(cVectorVal, SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];
        
        switch (jntp->JointKind) {
            case c3dJoint:
            case cBearingJoint:
            case cBushingJoint:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[INBPIN2][j] & ISQUESFLG)
                        SINDX(jntp->Pins[INBPIN2], j, 
                              INDX(VREF1(SysI.inbpin2, b), j));
                if (norm_pin(F, jntp, INBPIN2, ipin2, b, ERR_ZeroLoopInbPin2,
                             SysI.n+b, what, name, sumsq, norminv, ipin2_x))
                    return 1;
                break;

            default: 
                SINDX(ipin2_x, b, VECTOR_ZERO());
                break;
        }
    }
    ASSIGN(ipin2, ipin2_x); 

    /* Loop iref:   
     *   Ball
     *     Zero
     *         Pin, Slider, Cylinder, Weld
     *     This is the optional vector given by `inbref' in the input file.  
     *     If absent, this vector defaults to any convenient perpendicular 
     *     to ipin.
     *   Ujoint, Sixdof
     *     This is the required second `pin' in the input file.
     *   Gimbal, Bearing, Bushing
     *     This is the required third `pin' in the input file.
     *   Planar
     *     This is the required first `pin' in the input file.
     */
    iref_x = PERM(NEW_1dARRAY(cVectorVal, SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];
        
        switch (jntp->JointKind) {
            case cBallJoint: 
                SINDX(iref_x, b, VECTOR_ZERO());
                break;

            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cWeldJoint:
                if (jntp->Pins[INBREF] == NULL) {
                    house(F, ipin, b, sumsq, tvec1, 
                          iref, CL_FLUSHNONCONST, iref_x);
                    for (j=0; j<3; j++)
                        jntp->PinFlg[INBREF][j] = 0;
                    break;
                } 
                pinno = INBREF;
                whichsym = SysI.inbref;
                errnum = ERR_ZeroLoopInbRef;
                goto getiref;

            case cUjoint:
            case c6dJoint:
                pinno = INBPIN2;
                whichsym = SysI.inbpin2;
                errnum = ERR_ZeroLoopInbPin2;
                goto getiref;

            case c3dJoint:
            case cBearingJoint:
            case cBushingJoint:
                pinno = INBPIN3;
                whichsym = SysI.inbpin3;
                errnum = ERR_ZeroLoopInbPin3;
                goto getiref;

            case cPlanarJoint:
                pinno = INBPIN1;
                whichsym = SysI.inbpin1;
                errnum = ERR_ZeroLoopInbPin1;
                goto getiref;
 
            default:
            getiref:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[pinno][j] & ISQUESFLG)
                        SINDX(jntp->Pins[pinno], j, 
                              INDX(VREF1(whichsym, b), j));
                if (norm_pin(F, jntp, pinno, iref, b, errnum,
                             SysI.n+b, what, name, sumsq, norminv, iref_x))
                    return 1;
                break;
        }
    }
    ASSIGN(iref, iref_x); 

    /* Loop iperp:
     *   Ball
     *     zero
     *   Pin, Slider, Ujoint, Cyl, Weld
     *     ipin X iref
     *   Gimbal, Bearing, Bushing
     *     ipin X ipin2
     *     For bushing, user's third `pin' must equal ipin X ipin2
     *   Planar
     *     ipin X iref
     *     User's second `pin' must equal ipin X iref
     *   Sixdof
     *     ipin X iref
     *     User's third `pin' must equal ipin X iref
     *
     * For planar, weld, sixdof, bushing we report an error if the user-supplied
     * iperp is not equal to ipin X iref or ipin X ipin2 as appropriate.  We 
     * use the correct value, however.
     */
    iperp_expr = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];

        switch (jntp->JointKind) {
            case cBallJoint: 
                SINDX(iperp_expr, b, VECTOR_ZERO());
                break;

            case cPinJoint:
            case cSlidingJoint:
            case cUjoint:
            case cCylJoint:
            case cWeldJoint:
                FLUSH_VEC(F, iperp, b, iperp_expr, CROSS(VAL1(ipin,b),
                                                         VAL1(iref,b)));
                break;

            case c3dJoint:
            case cBearingJoint:
                FLUSH_VEC(F, iperp, b, iperp_expr, CROSS(VAL1(ipin,b),
                                                         VAL1(ipin2,b)));
                break;

            /* These last 3 joint types each take three input pins, but
             * the pins must form a mutually perpendicular right handed
             * set.  That means the third pin can be calculated from the
             * first two, but since we still make the user give us 3 we
             * have to check for consistency.
             */

            case cPlanarJoint:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[INBPIN2][j] & ISQUESFLG)
                        SINDX(jntp->Pins[INBPIN2], j, 
                              INDX(VREF1(SysI.inbpin2, b), j));
                whichsym = iref;
                pinno = INBPIN2;
                errnum = ERR_ZeroLoopInbPin2;
                goto getiperp;

            case cBushingJoint:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[INBPIN3][j] & ISQUESFLG)
                        SINDX(jntp->Pins[INBPIN3], j, 
                              INDX(VREF1(SysI.inbpin3, b), j));
                whichsym = ipin2;
                pinno = INBPIN3;
                errnum = ERR_ZeroLoopInbPin3;
                goto getiperp;

            case c6dJoint:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[INBPIN3][j] & ISQUESFLG)
                        SINDX(jntp->Pins[INBPIN3], j, 
                              INDX(VREF1(SysI.inbpin3, b), j));
                whichsym = iref;
                pinno = INBPIN3;
                errnum = ERR_ZeroLoopInbPin3;
                goto getiperp;

            getiperp:
                if (norm_pin(F, jntp, pinno, iperp, b, errnum, 
                             SysI.n+b, what, name, sumsq, norminv, iperp_expr))
                    return 1;
                ASSIGN_CLN(F, tvec1, 
                           CROSS(VAL1(ipin,b), VAL1(whichsym,b)));
                cond = NOT(
                  AND(NEARTO(VAL1(tvec1,0),INDX11(iperp_expr,b,0),cNearZero),
                  AND(NEARTO(VAL1(tvec1,1),INDX11(iperp_expr,b,1),cNearZero),
                      NEARTO(VAL1(tvec1,2),INDX11(iperp_expr,b,2),cNearZero))));
                if (IFTHEN(F, cond, &elsetoo)) {
                    if (IS_CONST(cond)) {
                        PRINTERR(ERR_NotRtHandSet, SysI.n+b, what, name);
                        return 1;
                    } else
                        SETERR(F, ROU_sdinit, ERR_NotRtHandSet);
                }
                IFEND(F, cond);
                DISPOSE_EXPR(cond);
                FLUSH_VEC(F, iperp, b, iperp_expr, CROSS(VAL1(ipin,b),
                                                         VAL1(whichsym,b)));
                break;
        }
    }
    ASSIGN_CLN(F, iperp, UNUSE(iperp_expr));

    /* Loop ghand (gimbal handedness):
     *   Gimbal, Bearing, Bushing
     *      1 if iperp*iref >= 0
     *     -1 if iperp*iref < 0
     *   All others
     *     0
     * This is used in DECOMPOSE_GIMBAL to decide on appropriate loop q's
     * for the loop gimbal joint.
     */
    ghand_x = INUSE(NEW_1dARRAY(cScalarVal,SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;

        if (JointInfo[(int)jntp->JointKind].hasgimbal)
            SINDX(ghand_x, b, 
                QUES(LESSTHAN(DOT(VAL1(iperp,b),VAL1(iref,b)),SCALAR_ZERO()),
                     SC(-1.0), 
                     SCALAR_ONE()));
        else
            SINDX(ghand_x, b, SCALAR_ZERO());
    }
    ASSIGN_CLN(F, ghand, UNUSE(ghand_x));

    /* Loop opin:   
     *   Ball, Sixdof
     *     Not allowed.
     *         All others
     *     This is the optional vector given by `bodypin' in the input file.  
     *     If absent, this vector defaults to the last inboard pin, which
     *     varies by joint type:
     *       Pin, Slider, Cylinder, Planar, Weld -- ipin
     *       Ujoint,Gimbal,Bearing,Bushing       -- iref
     */
    opin_x = PERM(NEW_1dARRAY(cVectorVal, SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];
        
        switch (jntp->JointKind) {
            case cBallJoint: 
            case c6dJoint: 
                SINDX(opin_x, b, VECTOR_ZERO());
                break;
 
            default:
                if (jntp->Pins[BODYPIN] == NULL) {
                    for (j=0; j<3; j++)
                        jntp->PinFlg[BODYPIN][j] = 0;

                    /* just copy from the right place */
                    switch(jntp->JointKind) {
                        case cPinJoint:
                        case cSlidingJoint:
                        case cCylJoint:
                        case cPlanarJoint:
                        case cWeldJoint:
                            SINDX(opin_x, b, VAL1(ipin, b));
                            break;

                        case cUjoint:
                        case c3dJoint:
                        case cBearingJoint:
                        case cBushingJoint:
                            SINDX(opin_x, b, VAL1(iref, b));
                            break;
                    }
                } else {
                    for (j = 0; j < 3; j++)
                        if (jntp->PinFlg[BODYPIN][j] & ISQUESFLG)
                            SINDX(jntp->Pins[BODYPIN], j, 
                                  INDX(VREF1(SysI.bodypin, b), j));
                    if (norm_pin(F, jntp, BODYPIN, opin, b, ERR_ZeroLoopBodyPin,
                                 SysI.n+b, what, name, sumsq, norminv, opin_x))
                        return 1;
                }
                break;
        }
    }
    ASSIGN(opin, opin_x); 

    /* Loop oref:   
     *   Ball, Sixdof
     *     Zero
     *         Pin, Slider, Ujoint, Gimbal, Cylinder, Planar, Weld, Bearing, Bushing
     *     This is the optional vector given by `bodyref' in the input file.  
     *     If absent, this vector defaults as follows:
     *       Pin, Slider, Cylinder, Planar, Weld
     *         if (opin == ipin) oref = iref
     *         else              oref = some perpendicular to opin
     *       Ujoint
     *         if (opin == iref) oref = ipin
     *         else              oref = some perpendicular to opin
     *       Gimbal, Bearing, Bushing
     *         if (opin == iref) oref = ipin2
     *         else              oref = some perpendicular to opin
     */
    oref_x = PERM(NEW_1dARRAY(cVectorVal, SysI.nl));
    for (b = 0; b < SysI.nl; b++) {
        jntp = &SysI.LoopConst[b].jnt;
        name = SysI.LoopConst[b].OutbBodyName;
        firstdof = SysI.FirstDOF[b];
        
        switch (jntp->JointKind) {
            case cBallJoint: 
            case c6dJoint:
                SINDX(oref_x, b, VECTOR_ZERO());
                break;

            case cPinJoint:
            case cSlidingJoint:
            case cCylJoint:
            case cPlanarJoint:
            case cWeldJoint:
                if (jntp->Pins[BODYREF])
                    goto getoref;
                cond = AND(EQUAL(INDX(VAL1(opin,b),0),
                                 INDX(VAL1(ipin,b),0)),
                       AND(EQUAL(INDX(VAL1(opin,b),1),
                                 INDX(VAL1(ipin,b),1)),
                           EQUAL(INDX(VAL1(opin,b),2),
                                 INDX(VAL1(ipin,b),2))));
                if (IFTHEN(F, cond, &elsetoo)) 
                    FLUSH_VEC_GEN(F, elsetoo ? CL_FLUSHALL : CL_FLUSHNONCONST, 
                                  CL_FORGET, oref, b, oref_x, VAL1(iref,b));
                if (IFELSE(F, cond, &thentoo))
                    house(F, opin, b, sumsq, tvec1, oref, 
                          thentoo ? CL_FLUSHALL : CL_FLUSHNONCONST, oref_x);
                IFEND(F, cond); 
                DISPOSE_EXPR(cond);
                if (thentoo && elsetoo)
                    SINDX(oref_x, b, VREF1(oref, b));
                break;

            case cUjoint:
                if (jntp->Pins[BODYREF])
                    goto getoref;
                cond = AND(EQUAL(INDX(VAL1(opin,b),0),
                                 INDX(VAL1(iref,b),0)),
                       AND(EQUAL(INDX(VAL1(opin,b),1),
                                 INDX(VAL1(iref,b),1)),
                           EQUAL(INDX(VAL1(opin,b),2),
                                 INDX(VAL1(iref,b),2))));
                if (IFTHEN(F, cond, &elsetoo)) 
                    FLUSH_VEC_GEN(F, elsetoo ? CL_FLUSHALL : CL_FLUSHNONCONST, 
                                  CL_FORGET, oref, b, oref_x, VAL1(ipin,b));
                if (IFELSE(F, cond, &thentoo))
                    house(F, opin, b, sumsq, tvec1, oref, 
                          thentoo ? CL_FLUSHALL : CL_FLUSHNONCONST, oref_x);
                IFEND(F, cond); 
                DISPOSE_EXPR(cond);
                if (thentoo && elsetoo)
                    SINDX(oref_x, b, VREF1(oref, b));
                break;

            case c3dJoint:
            case cBearingJoint:
            case cBushingJoint:
                if (jntp->Pins[BODYREF])
                    goto getoref;
                cond = AND(EQUAL(INDX(VAL1(opin,b),0),
                                 INDX(VAL1(iref,b),0)),
                       AND(EQUAL(INDX(VAL1(opin,b),1),
                                 INDX(VAL1(iref,b),1)),
                           EQUAL(INDX(VAL1(opin,b),2),
                                 INDX(VAL1(iref,b),2))));
                if (IFTHEN(F, cond, &elsetoo)) 
                    FLUSH_VEC_GEN(F, elsetoo ? CL_FLUSHALL : CL_FLUSHNONCONST, 
                                  CL_FORGET, oref, b, oref_x, VAL1(ipin2,b));
                if (IFELSE(F, cond, &thentoo))
                    house(F, opin, b, sumsq, tvec1, oref, 
                          thentoo ? CL_FLUSHALL : CL_FLUSHNONCONST, oref_x);
                IFEND(F, cond); 
                DISPOSE_EXPR(cond);
                if (thentoo && elsetoo)
                    SINDX(oref_x, b, VREF1(oref, b));
                break;

            default:
            getoref:
                for (j = 0; j < 3; j++)
                    if (jntp->PinFlg[BODYREF][j] & ISQUESFLG)
                        SINDX(jntp->Pins[BODYREF], j, 
                              INDX(VREF1(SysI.bodyref, b), j));
                if (norm_pin(F, jntp, BODYREF, oref, b, ERR_ZeroLoopBodyRef, 
                             SysI.n+b, what, name, sumsq, norminv, oref_x))
                    return 1;
                break;
        }
    }
    ASSIGN(oref, oref_x); 

    /* loop operp -- all opin X oref */

    operp_expr = INUSE(NEW_1dARRAY(cVectorVal,SysI.nl));
    for (k=0; k<SysI.nl; k++) 
        SINDX(operp_expr, k, CROSS(VAL1(opin,k), VAL1(oref,k)));
    ASSIGN_CLN(F, operp, UNUSE(operp_expr));

  skiploops:

    if (sdfast_opt.formulation == OPT_KANE) {
        efprintf(F, "\n%{Zero out Vpk and Wpk%}\n");
        /* Zero out Vpk and Wpk so we don't have to set zero values at
         * run time in order to be able to use them in SDREL2CART().
         * Only need worry about the upper triangle.
         */
        FORCNT("520", "i", str_s);
          FOR("510", "j", "i", str_s1);
            FORCNT("500", "k", "3");
              RSET("Vpk%(i%,j%,k%)", 0.);
              RSET("Wpk%(i%,j%,k%)", 0.);
            ENDFOR("500");
          ENDFOR("510");
        ENDFOR("520");
    } else {
        efprintf(F, "\n%{Zero out ping and hngpt%}\n");
        /* This is for SDREL2CART() when using Order(N) */
        FORCNT("510", "i", str_s);
          FORCNT("500", "j", "3");
            RSET("ping%(i%,j%)", 0.);
            RSET("hngpt%(i%,j%)", 0.);
          ENDFOR("500");
        ENDFOR("510");
    }

    /* Calculate constants involved with mass properties: mtot, 
     * pseudobody mass props (psmk[g], psik[g], psrk[g], psri[g]),
     * Iko, mkrk 
     */
    
    efprintf(F, "\n%{Compute pseudobody-related constants%}\n");

    /* Pseudo body masses */
    tmpx = SCALAR_ZERO();
    for (w = SysI.GndPseudoBody.weldlist; w; w = w->weldlist)
        tmpx = ADD(tmpx, w->Mass);
    ASSIGN_CLN(F, SysI.psmkg, tmpx);

    if (SysI.s) {
        tmpx = INUSE(NEW_1dARRAY(cScalarVal, SysI.s));
        for (k=0; k < SysI.s; k++) {
            SINDX(tmpx, k, SysI.PseudoBodies[k].Mass);
            for (w = SysI.PseudoBodies[k].weldlist; w; w = w->weldlist)
                SINDX(tmpx, k, ADD(INDX(tmpx, k), w->Mass));
        }
        ASSIGN_CLN(F, SysI.psmk, UNUSE(tmpx));
    }

    /* For each pseudo body set, compute the vectors from its composite COM to 
     * the COMs of its constituent (real) bodies.  Store the vectors in 
     * rcom[b] for all real bodies b.  Rcom must be accessible numerically.
     *
     * First we need some intermediate results: the vectors from the pseudo
     * body set's "head" body COM to each of its welded-on body's COMs.  Call
     * this rhead[b] for all real bodies b.  This is computed recursively
     * by noting that if i is body b's inboard body, then 
     *   rhead[b] = rhead[i] + ri[b] - rk[b],    i != ground
     *            =            ri[b] - rk[b],    i == ground
     *
     * Flush as we go to avoid unnecessary recomputations.
     */
    tmpx = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    for (b=0; b < SysI.n; b++)
        SINDX(tmpx, b, VECTOR_ZERO());
    for (w = SysI.GndPseudoBody.weldlist; w; w = w->weldlist) {
        i = w->jnt.InbBody;
        FLUSH_VEC(F, SysI.rhead, w->jnt.OutbBody, tmpx, 
            ADD(i == cGroundBody ? VECTOR_ZERO() : INDX(tmpx, i), 
                SUB(w->jnt.InbToJoint, w->jnt.BodyToJoint)));
    }
    for (k=0; k < SysI.s; k++)
        for (w = SysI.PseudoBodies[k].weldlist; w; w = w->weldlist) {
            i = w->jnt.InbBody;
            FLUSH_VEC(F, SysI.rhead, w->jnt.OutbBody, tmpx, 
                ADD(i == cGroundBody ? VECTOR_ZERO() : INDX(tmpx, i), 
                    SUB(w->jnt.InbToJoint, w->jnt.BodyToJoint)));
        }
    ASSIGN_CLN(F, SysI.rhead, UNUSE(tmpx));

    /* The next intermediate result needed is psrcom[g], the vector
     * from each pseudobody set's composite COM to the COM of its "head" body.
     *   psrcom[p] = -sum(rhead[b]*mk[b])/psmk[p] for each pseudobody p and
     * welded-on real body b.
     */
    tmpx = VECTOR_ZERO();
    if (!IS_ZERO(VAL(SysI.psmkg))) {
        int isconst;

        for (w = SysI.GndPseudoBody.weldlist; w; w = w->weldlist)
           tmpx = SUB(tmpx, MUL(VAL1(SysI.rhead, w->jnt.OutbBody), w->Mass));
        tmpx = INUSE(DVD(tmpx, VAL(SysI.psmkg)));
        isconst = IS_CONST(INDX(tmpx,0)) 
                  && IS_CONST(INDX(tmpx,1))
                  && IS_CONST(INDX(tmpx,2));
        if (!(isconst || IS_CONST(VAL(SysI.psmkg)))) {
            IFCOND PRINT_E(F, VAL(SysI.psmkg)); efprintf(F, "%s%r", NE, 0.);
            THEN
              for (i=0; i<3; i++)
                  PRINT_ASSN1(F, PRINTNAME(SysI.psrcomg), i, INDX(tmpx, i));
            ELSE
              for (i=0; i<3; i++)
                  PRINT_ASSN1(F, PRINTNAME(SysI.psrcomg), i, SCALAR_ZERO());
            ENDIF;
            DISPOSE_EXPR(UNUSE(tmpx));
            tmpx = INUSE(VREF(SysI.psrcomg));
        }
        tmpx = UNUSE(tmpx);
    }
    ASSIGN_CLN(F, SysI.psrcomg, tmpx);
        
    if (SysI.s) {
        tmpx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
        for (k=0; k < SysI.s; k++) {
            tmpv = VECTOR_ZERO();
            if (!IS_ZERO(VAL1(SysI.psmk, k))) {
                int isconst; 
                for (w = SysI.PseudoBodies[k].weldlist; w; w = w->weldlist)
                   tmpv = SUB(tmpv, MUL(VAL1(SysI.rhead, w->jnt.OutbBody), 
                                        w->Mass));
                tmpv = INUSE(DVD(tmpv, VAL1(SysI.psmk, k)));
                isconst = IS_CONST(INDX(tmpv,0)) 
                          && IS_CONST(INDX(tmpv,1))
                          && IS_CONST(INDX(tmpv,2));
                if (!(isconst || IS_CONST(VAL1(SysI.psmk, k)))) {
                    IFCOND PRINT_E(F, VAL1(SysI.psmk, k)); 
                           efprintf(F, "%s%r", NE, 0.);
                    THEN
                      for (i=0; i<3; i++)
                          PRINT_ASSN2(F, PRINTNAME(SysI.psrcom), k, i,
                                      INDX(tmpv, i));
                    ELSE
                      for (i=0; i<3; i++)
                          PRINT_ASSN2(F, PRINTNAME(SysI.psrcom), k, i,
                                      SCALAR_ZERO());
                    ENDIF;
                    DISPOSE_EXPR(UNUSE(tmpv));
                    tmpv = INUSE(VREF1(SysI.psrcom, k));
                }
                tmpv = UNUSE(tmpv);
            }
            SINDX(tmpx, k, tmpv);
        }
        ASSIGN_CLN(F, SysI.psrcom, UNUSE(tmpx));
    }

    /* Now we can compute rcom[b] = rhead[b] + psrcom[p], where p is 
     * real body b's "head" pseudobody.  Make sure that all values of
     * rcom are set so that rcom can be accessed numerically.
     */
    tmpx = INUSE(NEW_1dARRAY(cVectorVal, SysI.n));
    for (b = 0; b < SysI.n; b++) {
        int p = SysI.LastDOF[b];
        FLUSH_VEC_ALL(F, SysI.rcom, b, tmpx, 
                      ADD(VAL1(SysI.rhead, b), 
                          p == -1 ? VAL(SysI.psrcomg) : VAL1(SysI.psrcom, p)));
    }
    ASSIGN_CLN(F, SysI.rcom, UNUSE(tmpx));

    /* Now compute psrk[g].  This is just rk[p]+psrcom[p] for the head 
     * pseudobody.  (rk is 0 for ground)
     */
    ASSIGN_CLN(F, SysI.psrkg, VAL(SysI.psrcomg));

    if (SysI.s) {
        tmpx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
        for (k=0; k<SysI.s; k++)
            SINDX(tmpx, k, ADD(SysI.PseudoBodies[k].jnt.BodyToJoint,
                               VAL1(SysI.psrcom, k)));
        ASSIGN_CLN(F, SysI.psrk, UNUSE(tmpx));
    }

    /* Next compute psri[g].  This is just the initial pseudobody ri[k] if
     * pseudobody k's inboard pseudobody is not a realbody, and it is
     * ri[k]+rcom[i] if the inboard corresponds to real body i.
     * (ri is 0 for ground)
     */
    ASSIGN_CLN(F, SysI.psrig, VECTOR_ZERO());

    if (SysI.s) {
        tmpx = INUSE(NEW_1dARRAY(cVectorVal, SysI.s));
        for (b=0; b<SysI.n; b++) {
            if (SysI.Bodies[b].jnt.JointKind == cWeldJoint)
                continue;        /* no welded-on bodies need be considered */

            k = SysI.FirstDOF[b];
            i = SysI.Bodies[b].jnt.InbBody;
            SINDX(tmpx, k, ADD(SysI.PseudoBodies[k].jnt.InbToJoint,
                               i==cGroundBody ? VAL(SysI.psrcomg)
                                              : VAL1(SysI.rcom, i)));

            for (k=SysI.FirstDOF[b]+1; k <= SysI.LastDOF[b]; k++)
                SINDX(tmpx, k, SysI.PseudoBodies[k].jnt.InbToJoint);
        }
        ASSIGN_CLN(F, SysI.psri, UNUSE(tmpx));
    }

    /* Now for psik[g].  For each pseudobody set, we will independently
     * shift each welded body's inertia to the composite COM, and add
     * them all together.
     *
     *   psik[p] = sum(ik[b] +
     *                 mk[b]*(DOT(rcom[b],rcom[b])*E - OUTER(rcom[b],rcom[b])));
     *
     * This can be computed more neatly like this:
     *
     *   mkrcomt[b] = mk[b]*TILDA(rcom[b])
     *   psik[p] = sum(ik[b] - mkrcomt[b]*TILDA(rcom[b]))
     */

    tmpx = INUSE(NEW_1dARRAY(cMatrixVal,SysI.n));
    for (b = 0; b < SysI.n; b++)
        SINDX(tmpx, b, MUL(VAL1(SysI.mk, b), TILDA(VAL1(SysI.rcom, b))));
    ASSIGN_CLN(F, SysI.mkrcomt, UNUSE(tmpx));

    /* psikg */
    tmpx = MATRIX_ZERO();
    for (w = SysI.GndPseudoBody.weldlist; w; w = w->weldlist) {
       b = w->jnt.OutbBody;
       tmpx = ADD(tmpx, SUB(w->Inertia, 
                            MATMUL(VAL1(SysI.mkrcomt, b),
                                   TILDA(VAL1(SysI.rcom, b)))));
    }
    ASSIGN_CLN(F, SysI.psikg, tmpx);

    /* psik */
    if (SysI.s) {
        tmpx = INUSE(NEW_1dARRAY(cMatrixVal,SysI.s));
        for (k = 0; k < SysI.s; k++) {
            /* Shift the head body's inertia.  Its real body number is
             * the inboard body of the first welded-on body.
             */
            SINDX(tmpx, k, SysI.PseudoBodies[k].Inertia);
            if ((w = SysI.PseudoBodies[k].weldlist)) {
               b = w->jnt.InbBody;
               SINDX(tmpx, k, SUB(INDX(tmpx, k),
                                  MATMUL(VAL1(SysI.mkrcomt, b),
                                         TILDA(VAL1(SysI.rcom, b)))));
            }
            /* Now shift and add in the rest of the inertias. */
            for (w = SysI.PseudoBodies[k].weldlist; w; w = w->weldlist) {
               b = w->jnt.OutbBody;
               SINDX(tmpx, k, ADD(INDX(tmpx, k), 
                                  SUB(w->Inertia, 
                                      MATMUL(VAL1(SysI.mkrcomt, b),
                                             TILDA(VAL1(SysI.rcom, b))))));
            }
        }
        ASSIGN_CLN(F, SysI.psik, tmpx);
    }

    /* Calculate constants Wkk, Vkk, rkWkk, dik */
    COMPUTE_JOINT_CONSTS(F);

    efprintf(F, "\n%{Compute mass properties-related constants%}\n");
    mtot_expr = SCALAR_ZERO();
    for (b = 0; b < SysI.n; b++)
        mtot_expr = ADD(mtot_expr,VAL1(SysI.mk,b));
    PRINT_ASSN(F, PRINTNAME(mtot), mtot_expr, 0);
    ASSIGN(mtot,USEXIF(mtot_expr,VREF(mtot)));

    if (SysI.s) {
        mkrkx = INUSE(NEW_1dARRAY(cMatrixVal,SysI.s));
        for (k = 0; k < SysI.s; k++)
            SINDX(mkrkx,k,MUL(VAL1(SysI.psmk,k),TILDA(VAL1(SysI.psrk,k))));
        ASSIGN_CLN(F,mkrk,UNUSE(mkrkx));

        Ikox = INUSE(NEW_1dARRAY(cMatrixVal,SysI.s));
        for (k = 0; k < SysI.s; k++)
            SINDX(Ikox,k,SUB(VAL1(SysI.psik,k),
                             MATMUL(VAL1(mkrk,k),TILDA(VAL1(SysI.psrk,k)))));
        ASSIGN_CLN(F,Iko,UNUSE(Ikox));
    }

    /* Check that library serial number matches the serial no of the SD/FAST
     * that generated this sdinit().
     */
    CALL("%Aserialno(%Ri)");
    IFCOND efprintf(F, "i%s%d", NE, gProgramSerialNo);
    THEN
      SETERR(F, ROU_sdinit, ERR_LibraryMismatch);
    ENDIF;

    /* After sdinit, we're in state ST_INIT */
    ISET("roustate", ST_INIT);

    if (Lang == &Pascal_language) {
        efprintf(F,"%<");
        efprintf(F,"end; {with}\n");
        efprintf(F,"%>");
    }

    if (Lang_save == &ADSIM_language) {
        fprintf(F, "END FORTRAN\n\nFORTRAN_SECTION *\n\n");
        SET_LANGUAGE(&ADSIM_language);
        REGION_INITIAL(F);
        fprintf(F, "INCLUDE '%s'\n\nEXECUTE 'sdcontinuous'\n", 
            sdfast_opt.dynname);
    } else
        efprintf(F, Lang->proc_end);

    if (sdfast_opt.verbose) {
        printf("SDINIT generated.  CPU seconds used so far: %g\n\
Bytes of memory used so far: %lu\n", CPU_SECONDS() - gStartTime,
(unsigned long)BYTES_USED());
    }

    return 0;
}

/* check_perp
 * 
 * Verify that two input file loop joint vectors are perpendicular, and
 * set lasterr appropriately if not.  They are perpendicular if their
 * dot product is near zero.
 *
 * Returns 1 if we discover an input file error, else 0.
 */
static int
check_perp(FILE *F,
           JointDesc_t *jntp,
           int pin1,
           int pin2,
           int errnum,
           int jointno,
           char *what,
           char  *name)
{
    expr cond;
    int elsetoo;

    cond = NOT(NEARTO(DOT(jntp->Pins[pin1],jntp->Pins[pin2]),
                      SCALAR_ZERO(), cNearZero));
    if (IFTHEN(F, cond, &elsetoo)) {
        if (IS_CONST(cond)) {
            PRINTERR(errnum, jointno, what, name);
            return 1;
        } else
            SETERR(F, ROU_sdinit, errnum);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);

    return 0;
}

/* norm_pin
 *
 * Normalize the i'th pin of the joint pointed to by jntp.  In case of
 * `?' pin elements, VAL1(pinvar,pindx) is the symbol to be used to reference
 * the pin.  Sumsq and norminv are scalar temps.  The pindx'th element of
 * pin_x will be set on output to the normalized vector.  Pin_x will not
 * reference sumsq or norminv, so they can be reused.   In fact, each 
 * pin_x element will be either a constant or a VREF to a pin element,
 * so it is always IS_SIMPLE.
 *
 * If the pin has length near zero, errnum provides the error number to
 * report.
 *
 * Returns 1 if we run into an Input File error, else 0.
 */
static int
norm_pin(FILE *F,
         JointDesc_t *jntp,
         int i,
         sym pinvar,
         int pindx,
         int errnum,
         int jointno,
         char *what,
         char  *name,
         sym sumsq,
         sym norminv,
         expr pin_x)
{
    int thentoo, elsetoo;
    expr cond;

    ASSIGN_CLN(F, sumsq, DOT(jntp->Pins[i],jntp->Pins[i]));

    cond = LESSTHAN(VAL(sumsq),SC(cNearZero));
    if (IFTHEN(F, cond, &elsetoo)) {
        /* norm was near 0 */
        ASSIGN(norminv, SCALAR_ZERO());
        if (elsetoo)
            CLEANVAR(F, norminv, CL_FLUSHALL, CL_FORGET);
        if (IS_CONST(cond)) {
            PRINTERR(errnum, jointno, what, name);
            return 1;
        } else 
            SETERR(F, ROU_sdinit, errnum);
    }
    if (IFELSE(F, cond, &thentoo)) {
        ASSIGN(norminv, DVD(SCALAR_ONE(),SQRTT(VAL(sumsq))));
        CLEANVAR(F, norminv, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX,
                 CL_FORGET);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);

    if (thentoo && elsetoo)
        ASSIGN(norminv, VREF(norminv));

    FLUSH_VEC_NONCONST(F, pinvar, pindx, pin_x, 
                       MUL(VAL(norminv),jntp->Pins[i]));

    return 0;
}

/* house
 *
 * Use the beautiful Householder transformation to generate a unit vector 
 * perpendicular to a given unit vector.  The return is meaningless if the 
 * input vector is 0.
 * (No need to issue error since an error would have been reported when the
 * 0 vector was produced.)
 * 
 * Here's how it works to produce a perpendicular p to a vector v:
 *
 *      if (v[0] < 0)
 *         t = v - [1 0 0]
 *      else 
 *         t = v + [1 0 0]
 *
 *      H = E - (2*outer(t,t))/dot(t,t)   (H is 3x3, E is 3x3 identity)
 *
 *      p = [H[1,0] H[1,1] H[1,2]]        (i.e., 2nd row of H)
 *      
 * That's it!  It always works -- never blows up no matter what you shove
 * in for v.  It's always numerically well-behaved, because |t| > 1 by 
 * construction.  But don't forget that v should be a unit vector.
 *
 * We use a passed-in temp here but the returned expression won't reference
 * it if cl_flag is CL_FLUSHALL or CL_FLUSHNONCONST.
 */
static void 
house(FILE *F,
      sym invar,
      int jt,
      sym  sumsq,
      sym  tmpv, 
      sym outvar,
      int cl_flag,
      expr outvarx)
{
    expr temp;

    temp = NEW_VECX(cScalarVal);
    SINDX(temp, 0, 
        QUES(LESSTHAN(INDX(VAL1(invar,jt),0),SCALAR_ZERO()),
                      SUB(INDX(VAL1(invar,jt),0),SCALAR_ONE()),
                      ADD(INDX(VAL1(invar,jt),0),SCALAR_ONE())));
    SINDX(temp, 1, INDX(VAL1(invar,jt),1));
    SINDX(temp, 2, INDX(VAL1(invar,jt),2));
    ASSIGN_CLN(F, tmpv, temp);

    ASSIGN_CLN(F, sumsq, DOT(VAL(tmpv),VAL(tmpv)));

    /* get the second row from the householder matrix (2nd and 3rd are
       both perpendicular to pinvar */
    FLUSH_VEC_GEN(F, cl_flag, CL_FORGET, outvar, jt, outvarx, 
            INDX(SUB(MATRIX_IDENT(),
                     MUL(SC(2.0),
                         DVD(OUTER(VAL(tmpv),VAL(tmpv)), VAL(sumsq)))),
                 1));
}

/*
 * Initialize the sdgtopo global structure or common block.  We start with
 * the initialization block begin text (Lang->initblk_b) and end with
 * Lang->initblk_e.  Then
 * we use init_b, init_m, and init_e on each line, with the fully indexed
 * variable name between init_b and init_m, and the desired value between
 * init_m and init_e.
 *
 * For a non-common block language, we only initialize when decl_flags
 * has DECL_GLOBAL set.  For common block language, we output when
 * none of DECL_GLOBAL, DECL_EXTERN and DECL_STRUCT are set.
 */
void init_sdgtopo(FILE *F,
             int decl_flags)
{
    int i,j,k;
    JointDesc_t *jntp;
    JointKind_t jkind;
    int hascommon  = Lang->flags & LANG_COMMON_DECL,
        declglob   = decl_flags & DECL_GLOBAL,
        decllocal  = !(decl_flags & (DECL_GLOBAL|DECL_EXTERN|DECL_STRUCT));

    if (hascommon && !decllocal || !hascommon && !declglob)
        return;

    efprintf(F, Lang->initblk_b);

    /* load up some convenient macros */
    efprintf(F, "%:Ls%:Ms%:Rs", Lang->init_b, Lang->init_m, Lang->init_e);

    efprintf(F, "%{\
 Topological information\n%}\
%@Lground%@M%d%@R\
%@Lnbod%@M%d%@R\
%@Lndof%@M%d%@R\
%@Lncons%@M%d%@R\
%@Lnloop%@M%d%@R\
%@Lnldof%@M%d%@R\
%@Lnloopc%@M%d%@R\
%@Lnball%@M%d%@R\
%@Lnlball%@M%d%@R\
%@Lnpres%@M%d%@R\
%@Lnuser%@M%d%@R",
SysI.Grounded, SysI.n, SysI.s, SysI.nc, SysI.nl, SysI.sl, 
SysI.nlc,SysI.nb,SysI.nlb,SysI.np,SysI.nu);

    for (k = 0; k < SysI.n+SysI.nl; k++) {
        if (k < SysI.n)
             jkind = SysI.Bodies[k].jnt.JointKind;
        else jkind = SysI.LoopConst[k-SysI.n].jnt.JointKind;
        efprintf(F, "%@Ljtype%(%@d%)%@M%d%@R", k, (int)jkind);
    }

    for (i = 0; i < SysI.n; i++)
        efprintf(F, "%@Linb%(%@d%)%@M%@d%@R", i, SysI.Bodies[i].jnt.InbBody);
    for (i = 0; i < SysI.nl; i++)
        efprintf(F, "%@Linb%(%@d%)%@M%@d%@R", SysI.n+i, 
            SysI.LoopConst[i].jnt.InbBody);

    for (i = 0; i < SysI.n; i++)
        efprintf(F, "%@Loutb%(%@d%)%@M%@d%@R", i, SysI.Bodies[i].jnt.OutbBody);
    for (i = 0; i < SysI.nl; i++)
        efprintf(F, "%@Loutb%(%@d%)%@M%@d%@R", SysI.n+i, 
            SysI.LoopConst[i].jnt.OutbBody);

    for (i = 0; i < SysI.nj; i++)
        efprintf(F, "%@Lnjntdof%(%@d%)%@M%d%@R", i, 
                        SysI.LastDOF[i]-SysI.FirstDOF[i]+1);

    for (i = 0; i < SysI.n; i++)
        efprintf(F, "%@Lnjntc%(%@d%)%@M%d%@R", i, 0);
    for (i = 0; i < SysI.nl; i++)
        efprintf(F, "%@Lnjntc%(%@d%)%@M%d%@R", SysI.n+i, 
                                    6 - SysI.LoopConst[i].jnt.JointDOF);
    for (i = 0; i < SysI.n; i++)
        efprintf(F, "%@Lnjntp%(%@d%)%@M%d%@R", i, 
                                SysI.Bodies[i].jnt.JointPres);
    for (i = 0; i < SysI.nl; i++)
        efprintf(F, "%@Lnjntp%(%@d%)%@M%d%@R", SysI.n+i, 
                                    SysI.LoopConst[i].jnt.JointPres);

    for (i = 0; i < SysI.nj; i++)
        efprintf(F, "%@Lfirstq%(%@d%)%@M%@d%@R", i, SysI.FirstDOF[i]);

    for (i = 0; i < SysI.nj; i++)
        efprintf(F, "%@Lballq%(%@d%)%@M%@d%@R", i, SysI.BallQ[i]);

    for (i = 0; i < SysI.nj; i++)
        efprintf(F, "%@Lfirstm%(%@d%)%@M%@d%@R", i, 
            i < SysI.n ? -1 : SysI.FirstM[i-SysI.n]);

    for (i = 0; i < SysI.nj; i++)
        efprintf(F, "%@Lfirstp%(%@d%)%@M%@d%@R", i, SysI.PresM[i]);

    /* trans is always declared at least 1, so leave a space if necessary */
    if (SysI.nh) {
        for (i = 0; i < SysI.n; i++) {
            jntp = &SysI.Bodies[i].jnt;
            for (j = SysI.FirstDOF[i]; j <= SysI.LastDOF[i]; j++)
                efprintf(F, "%@Ltrans%(%@d%)%@M%d%@R", j,
                   (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[i]]
                               == AX_TRANS));
        }

        for (i = SysI.n; i < SysI.nj; i++) {
            jntp = &SysI.LoopConst[i-SysI.n].jnt;
            for (j = SysI.FirstDOF[i]; j <= SysI.LastDOF[i]; j++)
                efprintf(F, "%@Ltrans%(%@d%)%@M%d%@R", SysI.s+j,
                   (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[i]]
                               == AX_TRANS));
        }
    } else
        efprintf(F, "%@Ltrans%(%@d%)%@M%d%@R", 0, 0);

    for (i = 0; i < SysI.nu; i++)
        efprintf(F, "%@Lfirstu%(%@d%)%@M%@d%@R", i, 
            SysI.Const[i].Mindex);

    efprintf(F, Lang->initblk_e);
}

void init_sdginput(FILE *F,
              int decl_flags)
{
    int i,j,k;
    expr temp;
    int hascommon  = Lang->flags & LANG_COMMON_DECL,
        declglob   = decl_flags & DECL_GLOBAL,
        decllocal  = !(decl_flags & (DECL_GLOBAL|DECL_EXTERN|DECL_STRUCT));

    if (hascommon && !decllocal || !hascommon && !declglob)
        return;

    efprintf(F, Lang->initblk_b);

    efprintf(F, "%:Ls%:Ms%:Rs", Lang->init_b, Lang->init_m, Lang->init_e);

    efprintf(F, "%{Model parameters from the input file%}");

    efprintf(F, "\n%{gravity%}");
    for (i = 0; i < 3; i++) {
        efprintf(F, "%@L");
        temp = VREF1(SysI.grav,i);
        PRINT_E(F, temp);
        DISPOSE_EXPR(temp);
        efprintf(F, "%@M");
        PRINT_E(F, INDX(SysI.grav_nom, i));
        efprintf(F, "%@R");
    }

    efprintf(F, "\n%{mass%}");
    for (k = 0; k < SysI.n; k++) {
        efprintf(F, "%@L");
        temp = VREF1(SysI.mk, k);
        PRINT_E(F, temp);
        DISPOSE_EXPR(temp);
        efprintf(F, "%@M");
        PRINT_E(F, INDX(SysI.mk_nom, k));
        efprintf(F, "%@R");
    }

    efprintf(F, "\n%{inertia%}");
    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++)
            for (j = 0; j < 3; j++) {
                efprintf(F, "%@L");
                temp = INDX2(VREF1(SysI.ik,k), i,j);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX2(INDX(SysI.ik_nom,k), i, j));
                efprintf(F, "%@R");
            }

    efprintf(F, "\n%{tree hinge axis vectors%}");
    if (SysI.s)
        for (k = 0; k < SysI.n; k++)
            for (i = SysI.FirstDOF[k]; i <= SysI.LastDOF[k]; i++)
                for (j = 0; j < 3; j++) {
                    efprintf(F, "%@L");
                    temp = INDX(VREF1(SysI.pin, i), j);
                    PRINT_E(F, temp);
                    DISPOSE_EXPR(temp);
                    efprintf(F, "%@M");
                    PRINT_E(F, INDX11(SysI.pin_nom, i, j));
                    efprintf(F, "%@R");
                }
    else /* emit placeholder since pin is dimensioned at least 1 */
        for (j = 0; j < 3; j++) {
            efprintf(F, "%@L");
            temp = INDX(VREF1(SysI.pin, 0), j);
            PRINT_E(F, temp);
            DISPOSE_EXPR(temp);
            efprintf(F, "%@M%r%@R", 0.0);
        }

    efprintf(F, "\n%{tree bodytojoint vectors%}");
    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++) {
            efprintf(F, "%@L");
            temp = INDX(VREF1(SysI.rk, k), i);
            PRINT_E(F, temp);
            DISPOSE_EXPR(temp);
            efprintf(F, "%@M");
            PRINT_E(F, INDX11(SysI.rk_nom, k, i));
            efprintf(F, "%@R");
        }

    efprintf(F, "\n%{tree inbtojoint vectors%}");
    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++) {
            efprintf(F, "%@L");
            temp = INDX(VREF1(SysI.ri, k), i);
            PRINT_E(F, temp);
            DISPOSE_EXPR(temp);
            efprintf(F, "%@M");
            PRINT_E(F, INDX11(SysI.ri_nom, k, i));
            efprintf(F, "%@R");
        }
    
    efprintf(F, "\n%{tree prescribed motion%}");
    if (SysI.s)
        for (k = 0; k < SysI.s; k++) {
            efprintf(F, "%@L");
            temp = VREF1(SysI.pres, k);
            PRINT_E(F, temp);
            DISPOSE_EXPR(temp);
            efprintf(F, "%@M");
            PRINT_E(F, INDX(SysI.pres_nom, k));
            efprintf(F, "%@R");
        }
    else {
        /* always dimensioned at least 1 */
        efprintf(F, "%@L");
        temp = VREF1(SysI.pres, 0);
        PRINT_E(F, temp);
        DISPOSE_EXPR(temp);
        efprintf(F, "%@M%r%@R", 0.0);
    }

    efprintf(F, "\n%{stabilization parameters%}");
    efprintf(F, "%@L");
    temp = VREF(SysI.stabvel);
    PRINT_E(F, temp);
    DISPOSE_EXPR(temp);
    efprintf(F, "%@M");
    PRINT_E(F, SysI.stabvel_nom);
    efprintf(F, "%@R");

    efprintf(F, "%@L");
    temp = VREF(SysI.stabpos);
    PRINT_E(F, temp);
    DISPOSE_EXPR(temp);
    efprintf(F, "%@M");
    PRINT_E(F, SysI.stabpos_nom);
    efprintf(F, "%@R");

    if (SysI.nl) {
        efprintf(F, "\n%{loop first inboard pin vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.inbpin1, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.inbpin1_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop second inboard pin vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.inbpin2, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.inbpin2_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop third inboard pin vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.inbpin3, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.inbpin3_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop inboard reference vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.inbref, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.inbref_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop body pin vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.bodypin, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.bodypin_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop body reference vectors%}");
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.bodyref, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.bodyref_nom, k, i));
                efprintf(F, "%@R");
            }

        efprintf(F, "\n%{loop bodytojoint vectors%}");
        for (k = 0; k < SysI.nl; k++)
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.lbtj, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.lbtj_nom, k, i));
                efprintf(F, "%@R");
            }
        efprintf(F, "\n%{loop inbtojoint vectors%}");
        for (k = 0; k < SysI.nl; k++)
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@L");
                temp = INDX(VREF1(SysI.litj, k), i);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX11(SysI.litj_nom, k, i));
                efprintf(F, "%@R");
            }
        
        efprintf(F, "\n%{loop prescribed motion%}");
        if (SysI.sl) {
            for (k = 0; k < SysI.sl; k++) {
                efprintf(F, "%@L");
                temp = VREF1(SysI.lpres, k);
                PRINT_E(F, temp);
                DISPOSE_EXPR(temp);
                efprintf(F, "%@M");
                PRINT_E(F, INDX(SysI.lpres_nom, k));
                efprintf(F, "%@R");
            }
        } else {
            /* emit placeholder since lpres is dimensioned 1 */
            efprintf(F, "%@L");
            temp = VREF1(SysI.lpres, 0);
            PRINT_E(F, temp);
            DISPOSE_EXPR(temp);
            efprintf(F, "%@M%r%@R", 0.0);
        }

    }

    efprintf(F, "\n%{miscellaneous%}");
    efprintf(F, "%@Lmfrcflg%@M0%@R");
    efprintf(F, "%@Lroustate%@M%d%@R", ST_START);
    efprintf(F, "%@Lvpkflg%@M0%@R");
    efprintf(F, "%@Linerflg%@M0%@R");
    efprintf(F, "%@Lmmflg%@M0%@R");
    efprintf(F, "%@Lmmlduflg%@M0%@R");
    efprintf(F, "%@Lwwflg%@M0%@R");
    efprintf(F, "%@Lltauflg%@M0%@R");
    efprintf(F, "%@Lfs0flg%@M0%@R");
    efprintf(F, "%@Lii%@M0%@R");
    if (SysI.s)
        for (i = 0; i < SysI.s; i++) {
            efprintf(F, "%@Lmmap%(%@d%)",i);
            efprintf(F, "%@M%@d",i);
            efprintf(F, "%@R");
        }
    else {
        /* always dimensioned at least 1 */
        efprintf(F, "%@Lmmap%(%@d%)",0);
        efprintf(F, "%@M%@d",0);
        efprintf(F, "%@R");
    }

    efprintf(F, "\n%{Which parameters were \"?\" (1) or \"<nominal>?\" (3)%}");

    for (i = 0; i < 3; i++) {
        efprintf(F, "%@Lgravq%(%@d%)",i);
        efprintf(F, "%@M%d",SysI.GravFlg[i]);
        efprintf(F, "%@R");
    }
    for (k = 0; k < SysI.n; k++) {
        efprintf(F, "%@Lmkq%(%@d%)",k);
        efprintf(F, "%@M%d",SysI.Bodies[k].MassFlg);
        efprintf(F, "%@R");
    }
    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++)
            for (j = 0; j < 3; j++) {
                efprintf(F, "%@Likq%(%@d%,%@d%,%@d%)",k,i,j);
                efprintf(F, "%@M%d",SysI.Bodies[k].InerFlg[i][j]);
                efprintf(F, "%@R");
            }

    if (SysI.s) 
        for (k = 0; k < SysI.n; k++)
            for (i = SysI.FirstDOF[k]; i <= SysI.LastDOF[k]; i++)
                for (j = 0; j < 3; j++) {
                    efprintf(F, "%@Lpinq%(%@d%,%@d%)",i,j);
                    efprintf(F, "%@M%d",
                        SysI.Bodies[k].jnt.PinFlg[i-SysI.FirstDOF[k]][j]);
                    efprintf(F, "%@R");
                }
    else /* emit placeholder since pinq is dimensioned 1 */
        for (j = 0; j < 3; j++) {
            efprintf(F, "%@Lpinq%(%@d%,%@d%)",0,j);
            efprintf(F, "%@M%d", 0);
            efprintf(F, "%@R");
        }

    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++) {
            efprintf(F, "%@Lrkq%(%@d%,%@d%)",k,i);
            efprintf(F, "%@M%d",SysI.Bodies[k].jnt.BtjFlg[i]);
            efprintf(F, "%@R");
        }
    for (k = 0; k < SysI.n; k++)
        for (i = 0; i < 3; i++) {
            efprintf(F, "%@Lriq%(%@d%,%@d%)",k,i);
            efprintf(F, "%@M%d",SysI.Bodies[k].jnt.ItjFlg[i]);
            efprintf(F, "%@R");
        }
    if (SysI.s)
        for (k = 0; k < SysI.n; k++)
            for (i = SysI.FirstDOF[k]; i <= SysI.LastDOF[k]; i++) {
                efprintf(F, "%@Lpresq%(%@d%)",i);
                efprintf(F, "%@M%d",
                         SysI.Bodies[k].jnt.PresFlg[i-SysI.FirstDOF[k]]);
                efprintf(F, "%@R");
            }
    else {
        efprintf(F, "%@Lpresq%(%@d%)",0);
        efprintf(F, "%@M%d", 0);
        efprintf(F, "%@R");
    }
    efprintf(F, "%@Lstabvelq");
    efprintf(F, "%@M%d",SysI.StabVelFlg);
    efprintf(F, "%@R");
    efprintf(F, "%@Lstabposq");
    efprintf(F, "%@M%d",SysI.StabPosFlg);
    efprintf(F, "%@R");

    if (SysI.nl) {
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Linbpin1q%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[INBPIN1][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Linbpin2q%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[INBPIN2][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Linbpin3q%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[INBPIN3][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Linbrefq%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[INBREF][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Lbodypinq%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[BODYPIN][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Lbodyrefq%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.PinFlg[BODYREF][i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Llbtjq%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.BtjFlg[i]);
                efprintf(F, "%@R");
            }
        for (k = 0; k < SysI.nl; k++) 
            for (i = 0; i < 3; i++) {
                efprintf(F, "%@Llitjq%(%@d%,%@d%)",k,i);
                efprintf(F, "%@M%d",SysI.LoopConst[k].jnt.ItjFlg[i]);
                efprintf(F, "%@R");
            }
        if (SysI.sl)
            for (k = SysI.n; k < SysI.nj; k++)
                for (i = SysI.FirstDOF[k]; i<=SysI.LastDOF[k]; i++) {
                    efprintf(F, "%@Llpresq%(%@d%)",i);
                    efprintf(F, "%@M%d",
                      SysI.LoopConst[k-SysI.n].jnt.PresFlg[i-SysI.FirstDOF[k]]);
                    efprintf(F, "%@R");
                }
        else {
            /* emit placeholder since lpresq is dimensioned 1 */
            efprintf(F, "%@Llpresq%(%@d%)", 0);
            efprintf(F, "%@M%d%@R", 0);
        }
    }

    efprintf(F, "\n%{End of values from input file%}\n");

    efprintf(F, Lang->initblk_e);
}

/*
 * Initialize the sdgerror global structure or common block (this is
 * used in the library file).  See above init_ routines for more info.
 */
void init_sdgerror(FILE *F,
              int decl_flags)
{
    int hascommon  = Lang->flags & LANG_COMMON_DECL,
        declglob   = decl_flags & DECL_GLOBAL,
        decllocal  = !(decl_flags & (DECL_GLOBAL|DECL_EXTERN|DECL_STRUCT));

    if (hascommon && !decllocal || !hascommon && !declglob)
        return;

    efprintf(F, Lang->initblk_b);

    /* load up some convenient macros */
    efprintf(F, "%:Ls%:Ms%:Rs", Lang->init_b, Lang->init_m, Lang->init_e);

    efprintf(F, "%{Error parameters\n%}");
    efprintf(F, "%@Llasterr%@M0%@R");
    efprintf(F, "%@Llastrou%@M0%@R");

    efprintf(F, Lang->initblk_e);
}

void REGION_INITIAL(FILE *F)
{
    /* Prints out 'REGION initial' for ADSIM - user edittable. */
    Index_t i, k;
    int AnyPrescribed;

    fprintf(F, "REGION initial\n");
    efprintf(F, "%{\nSet this flag non-zero whenever you want to compute\n");
    efprintf(F, "angular momentum, kinetic energy, etc.\n%}");
    efprintf(F, "%Achek_flag%=%r\n", 0.);
    efprintf(F, "%{\nInitial conditions for state variables\n%}");

    /* Initialize all the Q's and U's. */
    for (k = 0; k < SysI.s; k++)
        efprintf(F, "q_%@d@%=?\n", k);
    for (k = 0; k < SysI.s; k++)
        efprintf(F, "u_%@d@%=?\n", k);

    AnyPrescribed = (SysI.s_free != SysI.s);

    if (AnyPrescribed) {
        efprintf(F, "%{\nPrescribed motion initializations\n%}");
        for (k = 0; k < SysI.s; k++)
            if (!IS_ZERO(VAL1(SysI.pres,k)))
                efprintf(F, "udot_%@d%=?\n", k);
    }

    efprintf(F, "%{\nExternal force and torque initializations\n%}");
    for (k = 0; k < SysI.n; k++) {
        for (i = 0; i < 3; i++) {
            if (IS_CONST(INDX(VAL1(ufk, k), i)))
                PRINT_ASSN2(F, "ufk", k, i, INDX(VAL1(ufk, k), i));
            else
                PRINT_ASSN2(F, "ufk", k, i, NULLEXPR);
        }
        for (i = 0; i < 3; i++) {
            if (IS_CONST(INDX(VAL1(utk, k), i)))
                PRINT_ASSN2(F, "utk", k, i, INDX(VAL1(utk, k), i));
            else
                PRINT_ASSN2(F, "utk", k, i, NULLEXPR);
        }
        efprintf(F, "\n");
    }
    efprintf(F, "%{Hinge force or torque initializations\n%}");
    for (k = 0; k < SysI.s; k++)
        if (!IS_ONE(VAL1(SysI.pres,k)))
            efprintf(F, "tau_%@d%=?\n", k);
        else
            efprintf(F,
              "%{%>tau_%@d is an output since D.O.F. %@d is prescribed%}", k, k);

    fprintf(F, "\nEND REGION initial\n\n");
}
