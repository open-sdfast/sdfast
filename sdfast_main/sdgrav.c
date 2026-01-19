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

/*
 * Code to generate the utility routines:
 *   sdindx        sderror         sdclearerr        sdseterr
 *   sdchkbnum           sdchkjnum         sdchkucnum        sdchkjaxis
 *   sdconschg
 *
 * and the data-initialization routines:
 *   sdgrav        sdmass        sdpres
 *   sdiner        sdbtj         sdstab
 *   sditj         sdpin         
 *
 *   sdpresacc     sdpresvel     sdprespos
 *   sdhinget      sdpointf      sdbodyt
 *
 * and information-gathering routines:
 *   sdgetgrav     sdgetmass     sdgetpres
 *   sdgetiner     sdgetbtj      sdgetstab
 *   sdgetitj      sdgetpin      sdcons
 *   sdinfo        sdjnt      
 *
 *
 * These routines use data expected already to be in a global or common
 * area (this is initialized data; i.e., it was put there by the linker),
 * including the map indicating which values were input with `?'.
 * This is used to allow a generic simulation
 * package to determine which parameters can be changed, and to avoid
 * changing parameters which will have no effect which changed.
 *
 * These routines are not generated for ADSIM output.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#include "../calc/gencode.h"

/* documented length of the info arrays -- not all of these are used */
#define NINFO        50

/* for sdinfo -- where does stuff go in info array */
#define GROUND_LOC        0
#define NBOD_LOC        1
#define NDOF_LOC        2
#define NC_LOC                3
#define NLOOP_LOC        4
#define NLDOF_LOC        5
#define NLOOPC_LOC        6
#define NBALL_LOC        7
#define NLBALL_LOC        8
#define NPRES_LOC        9
#define NUSER_LOC        10
#define RANK_LOC        11
#define INFORES_LOC        12

/* for sdjnt -- where does stuff go in info array */
#define JTYPE_LOC        0
#define ISLOOP_LOC        1
#define INB_LOC                2
#define OUTB_LOC        3
#define NJNTDOF_LOC        4
#define NJNTC_LOC        5
#define NJNTP_LOC        6
#define FIRSTQ_LOC        7
#define BALLQ_LOC        8
#define FIRSTM_LOC        9
#define FIRSTP_LOC        10
#define JNTRES_LOC        11

/* for sdcons -- where does stuff go in info array */
#define CTYPE_LOC        0
#define     USERCONS 1
#define MULT_LOC        1
#define CONSRES_LOC        2

static void make_jnt_entry(FILE *F, int loc, char *var);
static void make_info_entry(FILE *F,int loc,char *var);

/* This stuff goes in the library. */
void PRINT_ERRSTUFF(FILE *F)
{
    PRINT_SDERROR(F);
    PRINT_SDPRINTERR(F);
    PRINT_SDCLEARERR(F);
    PRINT_SDSETERR(F);
}

void PRINT_SDERROR(FILE *F)
{
    /* Print the sderror(routine,errnum) routine. */
    declare_proc(F, 0, "error", 
      VT_INTEGER|VT_BYREF, "routine",
      VT_DUP|VT_BYREF,           "errnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdgerror_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    SETREF("errnum", "lasterr");
    SETREF("routine", "lastrou");

    efprintf(F, Lang->proc_end); 
}

void PRINT_SDPRINTERR(FILE *F)
{
    /* Print the sdprinterr(fnum) routine. */
    if (Lang->flags & LANG_C_FAMILY) {
        declare_proc(F, 0, "printerr", 
          VT_TYPENAME|VT_BYREF, "FILE", "fnum",
          0);
    } else if (Lang == &FORTRAN_language) {
        declare_proc(F, 0, "printerr", 
          VT_INTEGER, "fnum",
          0);
    } else if (Lang == &Ada_language) {
        declare_proc(F, 0, "printerr", 
          VT_TYPENAME, "FILE_TYPE", "fnum",
          0);
    }

    efprintf(F, Lang->proc_dbegin);
    declare_sdgerror_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CALL("%@Aprerrmsg(fnum,lastrou,lasterr)");
    efprintf(F, Lang->proc_end); 
}

void PRINT_SDCLEARERR(FILE *F)
{
    /* Print the sdclearerr() routine. */
    declare_proc(F, 0, "clearerr", 
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdgerror_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    ISET("lasterr", 0);
    ISET("lastrou", 0);

    efprintf(F, Lang->proc_end); 
}

/* Print the sdseterr() routine (internal use only). 
 * This routine sets lasterr and lastrou to the passed-in errnum
 * and routine unless lasterr was already non-zero, in which case
 * nothing happens.
 */
void PRINT_SDSETERR(FILE *F)
{
    declare_proc(F, 0, "seterr", 
      VT_INTEGER, "routine",
      VT_DUP,          "errnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdgerror_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IF("lasterr", EQ, "0");
    THEN
        SET("lasterr", "errnum");
        SET("lastrou", "routine");
    ENDIF;

    efprintf(F, Lang->proc_end); 
}

/* Print the sdchkbnum, sdchkjnum, etc. routines (internal use only). 
 * These routines check whether a certain parameter is legitimate
 * and if not raise the appropriate error (assuming there has not
 * been a prior error).
 */
void PRINT_CHECKSTUFF(FILE *F)
{
    char callstr[50], lastbod[20];

    esprintf(lastbod, "%@d", SysI.n-1);

    /* sdchkbnum */

    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "chkbnum", 
      VT_INTEGER, "routine",
      VT_DUP,          "bnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "(bnum%s%@d)%s(bnum%s%@d)", LT, cGroundBody, 
                                                   OR_OP, GT, SysI.n-1);
    THEN  
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadBodyNum);
        CALL(callstr);
        FRETURN("%Achkbnum", "1");
    ENDIF;

    FRETURN("%Achkbnum", "0");
    efprintf(F, Lang->proc_end_noret); 

    /* sdchkjnum */

    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "chkjnum", 
      VT_INTEGER, "routine",
      VT_DUP,          "jnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "(jnum%s%@d)%s(jnum%s%@d)", 
                    LT, 0, OR_OP, GT, SysI.n+SysI.nl-1);
    THEN  
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadJointNum);
        CALL(callstr);
        FRETURN("%Achkjnum", "1");
    ENDIF;

    FRETURN("%Achkjnum", "0");
    efprintf(F, Lang->proc_end_noret); 

    /* sdchkucnum */

    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "chkucnum", 
      VT_INTEGER, "routine",
      VT_DUP,          "ucnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "(ucnum%s%@d)%s(ucnum%s%@d)", 
                    LT, 0, OR_OP, GT, SysI.nu-1);
    THEN  
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadUserConstraintNum);
        CALL(callstr);
        FRETURN("%Achkucnum", "1");
    ENDIF;

    FRETURN("%Achkucnum", "0");
    efprintf(F, Lang->proc_end_noret); 

    /* sdchkjaxis -- checks both joint number and axis number */

    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "chkjaxis", 
      VT_INTEGER, "routine",
      VT_DUP,          "jnum",
      VT_DUP,          "axnum",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);

    esprintf(callstr, "%Achkjnum");
    declare_vars(F, 0, 
      VT_INTEGER, "maxax",
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, callstr,
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "%Achkjnum(routine,jnum)%s0", NE);
    THEN  
        FRETURN("%Achkjaxis", "1");
    ENDIF;

    IFCOND efprintf(F, "(axnum%s%@d)%s(axnum%s%@d)", LT, 0, OR_OP, GT, 6);
    THEN
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadAxisNum);
        CALL(callstr);
        FRETURN("%Achkjaxis", "1");
    ENDIF;

    efprintf(F, "maxax%=njntdof%(jnum%)");
    if (Lang->subs_offset == 0)
        efprintf(F, "-1");
    ENDSTMT;

    /* Joints with balls allow one extra "axis" number to refer to the
       fourth Euler parameter. */
    IFCOND efprintf(F, 
        "(jtype%(jnum%)%s%d)%s(jtype%(jnum%)%s%d)%s(jtype%(jnum%)%s%d)",
        EQ, cBallJoint, OR_OP, EQ, c6dJoint, OR_OP, EQ, cRev6dJoint);
    THEN
        SET("maxax", "maxax+1");
    ENDIF;

    IF("axnum", GT, "maxax");
    THEN
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadAxisNumForThisJoint);
        CALL(callstr);
        FRETURN("%Achkjaxis", "1");
    ENDIF;

    FRETURN("%Achkjaxis", "0");
    efprintf(F, Lang->proc_end_noret); 

    /* sdchkjpin -- checks joint number and verifies that the pin number
     * is legitimate for that joint.  This is different than sdchkjaxis,
     * since it can, for example, include outboard body pins, and ball
     * joints can have no pins.  This is used for referencing user-supplied
     * pin vectors in SDPIN() and SDGETPIN().  The pin numbers correspond
     * to the input file ordering (1,2,3), with 4,5,6 used for inbref,
     * bodypin, bodyref, resp.
     */

    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "chkjpin", 
      VT_INTEGER, "routine",
      VT_DUP,          "jnum",
      VT_DUP,          "pinno",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);

    esprintf(callstr, "%Achkjnum");
    declare_vars(F, 0, 
      VT_INTEGER, "maxax",
      VT_DUP,     "pinok",
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, callstr,
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    IFCOND efprintf(F, "%Achkjnum(routine,jnum)%s0", NE);
    THEN  
        FRETURN("%Achkjpin", "1");
    ENDIF;

    /* Pins are numbered 0-5 */
    IFCOND efprintf(F, "(pinno%s%@d)%s(pinno%s%@d)", LT, 0, OR_OP, GT, 5);
    THEN
        esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadAxisNum);
        CALL(callstr);
        FRETURN("%Achkjpin", "1");
    ENDIF;

    /* Get maximum pin numbers.  For most joints, max is ndof-1.  But balls
     * have no pins, and cylinder, bearing and bushing joints don't allow
     * user input of all the pins.  No joint allows more than 3 input axes.
     */
    IFCOND efprintf(F, "njntdof%(jnum%)%s3", GE);
      THEN efprintf(F, "maxax%=%@d%;\n",2);
      ELSE efprintf(F, "maxax%=njntdof%(jnum%)%s%;\n",
                    Lang->subs_offset==0 ? "-1" : "");
    ENDIF;

    IFCOND efprintf(F, "jtype%(jnum%)%s%d", EQ, cBallJoint);
      THEN efprintf(F, "maxax%=%@d%;\n", -1);
    ENDIF;
    IFCOND efprintf(F, "jtype%(jnum%)%s%d", EQ, cCylJoint);
      THEN efprintf(F, "maxax%=%@d%;\n", 0);
    ENDIF;

    ISET("pinok", 0);

    IF("pinno", LE, "maxax")
      THEN ISET("pinok", 1);
    ENDIF;

    if (SysI.nl) {
        /* Loop joints sometimes allow additional pins:
         *   pin,slider,cyl,weld allow inbref (4th pin)
         *   all but ball & 6dof allow bodypin (5th pin)
         *   all but ball & 6dof allow bodyref (6th pin)
         */
        IF("jnum", GT, lastbod);
        THEN
          IFCOND efprintf(F,"\
((pinno%s%@d)%s((jtype%(jnum%)%s%d)))",
            EQ, 0, AND_OP, EQ, cWeldJoint);
          THEN
            ISET("pinok", 1);
          ENDIF;
          IFCOND efprintf(F,"\
(pinno%s%@d)%s((jtype%(jnum%)%s%d)%s(jtype%(jnum%)%s%d)%s(jtype%(jnum%)%s%d)\
%s(jtype%(jnum%)%s%d))",
            EQ, 3, AND_OP, EQ, cPinJoint, OR_OP, EQ, cSlidingJoint, OR_OP, 
            EQ, cCylJoint, OR_OP, EQ, cWeldJoint);
          THEN
            ISET("pinok", 1);
          ENDIF;
          IFCOND efprintf(F,"\
((pinno%s%@d)%s(pinno%s%@d))%s((jtype%(jnum%)%s%d)%s(jtype%(jnum%)%s%d))",
            EQ, 4, OR_OP, EQ, 5, AND_OP, NE, cBallJoint, AND_OP, NE, c6dJoint);
          THEN
            ISET("pinok", 1);
          ENDIF;
        ENDIF;
    }

    IF("pinok", EQ, "0");
    THEN
      esprintf(callstr, "%@Aseterr(routine,%d)", ERR_BadAxisNumForThisJoint);
      CALL(callstr);
      FRETURN("%Achkjpin", "1");
    ENDIF;

    FRETURN("%Achkjpin", "0");
    efprintf(F, Lang->proc_end_noret); 
}

void PRINT_FORCE_MOTION(FILE *F)
{
    PRINT_SDPRESACC(F);
    PRINT_SDPRESVEL(F);
    PRINT_SDPRESPOS(F);
    PRINT_SDGETHT(F);
    PRINT_SDHINGET(F);
    PRINT_SDPOINTF(F);
    PRINT_SDBODYT(F);
}

void PRINT_SDINDX(FILE *F)
{
    char strball[10],str6dof[10],strr6dof[10],axball[10],ax6dof[10];
    char str0[10];

    esprintf(str0, "%@d", 0);

    /* Print the i=sdindx(joint,axis) routine. */
    declare_proc(F, DECL_FUNCTION, VT_INTEGER, "indx",
      VT_INTEGER,             "joint", 
      VT_DUP,                 "axis", 
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER, "offs",
      VT_DUP,     "gotit",
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    /* If we're given an illegal joint,axis pair post an error and
     * return the lowest legal index to avoid core dumps. 
     */
    IFCOND efprintf(F, "%Achkjaxis(%d,joint,axis)%s0", ROU_sdindx, NE);
    THEN  
        FRETURN("%Aindx", str0);
    ENDIF;

    esprintf(strball, "%d", cBallJoint);
    esprintf(str6dof, "%d", c6dJoint);
    esprintf(strr6dof, "%d", cRev6dJoint);
    esprintf(axball, "%@d", 3); /* which axis is 4th Euler parameter? */
    esprintf(ax6dof, "%@d", 6);

    efprintf(F, "gotit%=0%;\n");

    IF("jtype%(joint%)", EQ, strball);
    THEN
      IF("axis", EQ, axball);
      THEN
        SET("offs", "ballq%(joint%)");
        ISET("gotit", 1);
      ENDIF;
    ELSE 
      IFCOND efprintf(F, "(jtype%(joint%)%s%s)%s(jtype%(joint%)%s%s)",
                          EQ, str6dof, OR_OP, EQ, strr6dof);
      THEN
        IF("axis", EQ, ax6dof);
        THEN
          SET("offs", "ballq%(joint%)");
          ISET("gotit", 1);
        ENDIF;
      ENDIF;
    ENDIF;

    IF("gotit", EQ, "0");
    THEN
        efprintf(F, "offs%=firstq%(joint%)+axis");
        if (Lang->subs_offset)
                efprintf(F, "-%d",Lang->subs_offset);
        ENDSTMT;
    ENDIF;

    FRETURN("%Aindx", "offs");

    efprintf(F, Lang->proc_end_noret); /* we already returned with FRETURN */
}

/* Note: PRINT_SDINDX must be called before either of the following
   two routines */

void PRINT_SET_ROUTINES(FILE *F)
{
    PRINT_SDGRAV(F); 
    PRINT_SDMASS(F);
    PRINT_SDINER(F);
    PRINT_SDBTJ(F);
    PRINT_SDITJ(F);
    PRINT_SDPIN(F);
    PRINT_SDPRES(F);
    PRINT_SDCONSCHG(F);
    PRINT_SDSTAB(F);
}

/* nonred says how many constraints appear initially to be non redundant */
void PRINT_GET_ROUTINES(FILE*F,
                   int nonred)
{
    PRINT_SDGETGRAV(F);
    PRINT_SDGETMASS(F);
    PRINT_SDGETINER(F);
    PRINT_SDGETBTJ(F);
    PRINT_SDGETITJ(F);
    PRINT_SDGETPIN(F);
    PRINT_SDGETPRES(F);
    PRINT_SDGETSTAB(F);
    PRINT_SDINFO(F,nonred);
    PRINT_SDJNT(F);
    PRINT_SDCONS(F);
}

void PRINT_SDGRAV(FILE *F)
{
    int i, anyques;

    /* Print the sdgrav(gravin) routine. */
    declare_proc(F, 0, "grav",
      VT_VECTOR, "gravin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    anyques = 0;
    for (i = 0; i < 3; i++)
        if (SysI.GravFlg[i] & ISQUESFLG) {
            efprintf(F, "grav%(%@d%)%=gravin%(%@d%)%;\n", i, i);
            efprintf(F, "gravq%(%@d%)%=%d%;\n", i, ISQUESFLG|HASNOMFLG);
            anyques = 1;
        }

    if (!anyques) 
        SETERR(F, ROU_sdgrav, ERR_TriedToSetNonQues);

    /* all parameter modification routines leave us in ST_START state */
    ISET("roustate", ST_START);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETGRAV(FILE *F)
{
    int i;

    /* Print the sdgetgrav(gravout) routine. */
    declare_proc(F, 0, "getgrav",
      VT_REAL|VT_VECTOR,    "gravout",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);
    for (i = 0; i < 3; i++)
        efprintf(F, "gravout%(%@d%)%=grav%(%@d%)%;\n", i, i);
    efprintf(F, Lang->proc_end);
}

void PRINT_SDMASS(FILE *F)
{
    /* Print the sdmass(body,massin) routine. */
    declare_proc(F, 0, "mass",
      VT_INTEGER, "body",
      VT_REAL, "massin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdmass);
    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
    THEN 
      SETERR(F, ROU_sdmass, ERR_BadBodyNum);
      RETURN;
    ENDIF;

    IF("mkq%(body%)", NE, "0");
    THEN
      SET("mk%(body%)", "massin");
      ISET("mkq%(body%)", ISQUESFLG|HASNOMFLG);
    ELSE
      SETERR(F, ROU_sdmass, ERR_TriedToSetNonQues);
    ENDIF;

    ISET("roustate", ST_START);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETMASS(FILE *F)
{
    /* Print the sdgetmass(body,massout) routine. */
    declare_proc(F, 0, "getmass",
      VT_INTEGER,            "body",
      VT_REAL|VT_BYREF,           "massout",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdgetmass);
    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
    THEN 
      SETERR(F, ROU_sdgetmass, ERR_BadBodyNum);
      RETURN;
    ENDIF;

    efprintf(F, "%smassout%=mk%(body%)%;\n",Lang->deref);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDINER(FILE *F)
{
    int i,j;

    /* Print the sdiner(body, inerin) routine. */
    declare_proc(F, 0, "iner",
      VT_INTEGER,           "body", 
      VT_REAL|VT_MATRIX,    "inerin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_vars(F, 0, 
      VT_INTEGER,        "anyques",
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdiner);
    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
    THEN 
      SETERR(F, ROU_sdiner, ERR_BadBodyNum);
      RETURN;
    ENDIF;

    ISET("anyques", 0);
    for (i = 0; i < 3; i++)
        for (j = i; j < 3; j++) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "ikq%(body%,%@d%,%@d%)", i,j);
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
            efprintf(F,     "ik%(body%,%@d%,%@d%)%=inerin%(%@d%,%@d%)%;\n",
                             i,j,i,j);
            efprintf(F,     "ikq%(body%,%@d%,%@d%)%=%d%;\n", 
                             i,j, ISQUESFLG|HASNOMFLG);
            if (i != j) {
                efprintf(F,     "ik%(body%,%@d%,%@d%)%=inerin%(%@d%,%@d%)%;\n",
                                j,i,i,j);
                efprintf(F,     "ikq%(body%,%@d%,%@d%)%=%d%;\n", 
                                j,i, ISQUESFLG|HASNOMFLG);
            }
            ISET(           "anyques", 1);
            efprintf(F, Lang->stmt_if2_e);
        }

    IF("anyques", EQ, "0");
      THEN SETERR(F, ROU_sdiner, ERR_TriedToSetNonQues);
    ENDIF;

    ISET("roustate", ST_START);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETINER(FILE *F)
{
    int i,j;

    /* Print the sdgetiner(body,inerout) routine. */
    declare_proc(F, 0, "getiner",
      VT_INTEGER,           "body", 
      VT_REAL|VT_MATRIX,    "inerout",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdgetiner);
    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
    THEN 
      SETERR(F, ROU_sdgetiner, ERR_BadBodyNum);
      RETURN;
    ENDIF;

    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            efprintf(F, "inerout%(%@d%,%@d%)%=ik%(body%,%@d%,%@d%)%;\n", 
                        i,j,i,j);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDBTJ(FILE *F)
{
        PRINT_SDBTJITJ(F, ROU_sdbtj, "btj", SysI.rk, "rkq", SysI.lbtj, "lbtjq");
}

void PRINT_SDGETBTJ(FILE *F)
{
        PRINT_SDGETBTJITJ(F, ROU_sdgetbtj, "btj", SysI.rk, SysI.lbtj);
}

void PRINT_SDITJ(FILE *F)
{
        PRINT_SDBTJITJ(F, ROU_sditj, "itj", SysI.ri, "riq", SysI.litj, "litjq");
}

void PRINT_SDGETITJ(FILE *F)
{
        PRINT_SDGETBTJITJ(F, ROU_sdgetitj, "itj", SysI.ri, SysI.litj);
}

/*
 * Prints sdbtj and sditj subroutines.
 *     `which' is "btj" or "itj".
 *     `vsym' is SysI.rk or SysI.ri.
 *     `vsymq' is "rkq" or "riq"
 *     `lvsym' is SysI.lbtj or SysI.litj
 *     `lvsymq' is "lbtjq" or "litjq"
 * Don't access lvsym etc. unless there are loops.
 */
void PRINT_SDBTJITJ(FILE *F,
               int routine,
               char *which,
               sym vsym, 
               char  *vsymq,
               sym lvsym,
               char *lvsymq)
{
    int i;
    char sdwhich[10], iwhich[10];
    char lastbod[20];

    sprintf(sdwhich,"%s",which);
    sprintf(iwhich,"%sin",which);

    /* Print the sd?tj(joint,?tjin) routine. */
    declare_proc(F, 0, sdwhich,
      VT_INTEGER,           "joint", 
      VT_REAL|VT_VECTOR,    iwhich,
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_vars(F, 0, 
      VT_INTEGER,        "anyques",
      0);
    DECL_CHKJNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JNUM(F, "joint", routine);

    ISET("anyques", 0);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
        for (i = 0; i < 3; i++) {
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "%s%(joint-%d%,%@d%)", lvsymq, SysI.n, i);
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
            efprintf(F,     "%s%(joint-%d%,%@d%)%=%s%(%@d%)%;\n",
                                 PRINTNAME(lvsym),SysI.n,i,iwhich,i);
            efprintf(F,     "%s%(joint-%d%,%@d%)%=%d%;\n",
                                 lvsymq,SysI.n,i, ISQUESFLG|HASNOMFLG);
            ISET(            "anyques", 1);
            efprintf(F, Lang->stmt_if2_e);
        }
        efprintf(F, Lang->stmt_if2_else);
    }

    for (i = 0; i < 3; i++) {
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "%s%(joint%,%@d%)", vsymq, i);
        efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
        efprintf(F,     "%s%(joint%,%@d%)%=%s%(%@d%)%;\n",
                        PRINTNAME(vsym),i,iwhich,i);
        efprintf(F,     "%s%(joint%,%@d%)%=%d%;\n",
                             vsymq,i, ISQUESFLG|HASNOMFLG);
        ISET(                "anyques", 1);
        efprintf(F, Lang->stmt_if2_e);
    }

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    IF("anyques", EQ, "0");
      THEN SETERR(F, routine, ERR_TriedToSetNonQues);
    ENDIF;

    ISET("roustate", ST_START);

    efprintf(F, Lang->proc_end);
}

/*
 * Prints sdgetbtj and sdgetitj subroutines.
 *     `which' is "btj" or "itj".
 *     `vsym' is SysI.rk or SysI.ri.
 *     `lvsym' is SysI.lbtj or SysI.litj.
 * Don't reference lvsym unless there are loops.
 */
void PRINT_SDGETBTJITJ(FILE *F,
                  int routine,
                  char *which,
                  sym vsym,
                  sym lvsym)
{
    int i;
    char sdwhich[10], owhich[10];
    char lastbod[20];

    sprintf(sdwhich,"get%s",which);
    sprintf(owhich,"%sout",which);

    /* Print the sdget?tj(joint,?tjout) routine. */
    declare_proc(F, 0, sdwhich,
      VT_INTEGER,           "joint", 
      VT_REAL|VT_VECTOR,    owhich,
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    DECL_CHKJNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JNUM(F, "joint", routine);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
            for (i = 0; i < 3; i++) {
                efprintf(F, "%s%(%@d%)%=%s%(joint-%d%,%@d%)%;\n",
                         owhich, i, PRINTNAME(lvsym),SysI.n, i);
            } 
        efprintf(F, Lang->stmt_if2_else);
    } 

    for (i = 0; i < 3; i++) {
        efprintf(F, "%s%(%@d%)%=%s%(joint%,%@d%)%;\n",
                 owhich, i, PRINTNAME(vsym),i);
    } 

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDPIN(FILE *F)
{
    int i,axno;
    char lastbod[20],nbod[20],axstr[20];
    char *ax[6], *axq[6];

    /* Assignment of loop joint pins to pin number. */
    ax[0] = "inbpin1"; axq[0] = "inbpin1q";
    ax[1] = "inbpin2"; axq[1] = "inbpin2q";
    ax[2] = "inbpin3"; axq[2] = "inbpin3q";
    ax[3] = "inbref";  axq[3] = "inbrefq";
    ax[4] = "bodypin"; axq[4] = "bodypinq";
    ax[5] = "bodyref"; axq[5] = "bodyrefq";

    /* Print the sdpin(joint,pinno,pinin) routine. */
    declare_proc(F, 0, "pin",
      VT_INTEGER,             "joint", 
      VT_DUP,                 "pinno", 
      VT_REAL|VT_VECTOR,    "pinin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, SysI.nl, "ljnum",
      VT_INTEGER,        "anyques",
      VT_DUP,                "offs",
      0);
    DECL_CHKJPIN(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JPIN(F, "joint", "pinno", ROU_sdpin);

    ISET("anyques", 0);
    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        esprintf(nbod, "%d", SysI.n);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
            efprintf(F, "ljnum%=joint-%s%;\n",nbod);
            for (axno = 0; axno < 6; axno++) {
                esprintf(axstr, "%@d", axno);
                efprintf(F, Lang->stmt_if2_b);
                efprintf(F, "pinno");
                efprintf(F, Lang->stmt_if2_then, Lang->eq_op, axstr);
                for (i=0; i<3; i++) {
                  efprintf(F, Lang->stmt_if2_b);
                  efprintf(F, "%s%(ljnum%,%@d%)", axq[axno], i);
                  efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
                  efprintf(F, "%s%(ljnum%,%@d%)%=pinin%(%@d%)%;\n", 
                              ax[axno], i, i);
                  efprintf(F, "%s%(ljnum%,%@d%)%=%d%;\n", 
                              axq[axno], i, ISQUESFLG|HASNOMFLG);
                  ISET(       "anyques", 1);
                  efprintf(F, Lang->stmt_if2_e);
                }
                efprintf(F, Lang->stmt_if2_e);
            }
        efprintf(F, Lang->stmt_if2_else);
    } 

    efprintf(F, "offs%=firstq%(joint%)+pinno");
    if (Lang->subs_offset)
            efprintf(F, "-%d",Lang->subs_offset);
    ENDSTMT;

    /* For most joints, the n user pins correspond to the first n dofs 
     * provided by the joint.  But bearing joints have three pins starting
     * with the 2nd dof (the 1st is a duplicate of the 2nd) and reverse
     * 6dof joints have three pins in the last 3 dofs.
     */
    IFCOND efprintf(F, "jtype%(joint%)%s%d",EQ,cRev6dJoint);
    THEN
        SET("offs", "offs+3");
    ENDIF;
    IFCOND efprintf(F, "jtype%(joint%)%s%d",EQ,cBearingJoint);
    THEN
        SET("offs", "offs+1");
    ENDIF;

    for (i = 0; i < 3; i++) {
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "pinq%(offs%,%@d%)", i);
        efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
            efprintf(F, "pin%(offs%,%@d%)%=pinin%(%@d%)%;\n", i, i);
            efprintf(F, "pinq%(offs%,%@d%)%=%d%;\n", 
                          i, ISQUESFLG|HASNOMFLG);
            ISET(       "anyques", 1);
        efprintf(F, Lang->stmt_if2_e);
    }

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    IF("anyques", EQ, "0");
      THEN SETERR(F, ROU_sdpin, ERR_TriedToSetNonQues);
    ENDIF;

    ISET("roustate", ST_START);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETPIN(FILE *F)
{
    int i,axno;
    char lastbod[20],nbod[20],axstr[20];
    char *ax[6], *axq[6];

    /* Assignment of loop joint pins to pin number. */
    ax[0] = "inbpin1"; axq[0] = "inbpin1q";
    ax[1] = "inbpin2"; axq[1] = "inbpin2q";
    ax[2] = "inbpin3"; axq[2] = "inbpin3q";
    ax[3] = "inbref";  axq[3] = "inbrefq";
    ax[4] = "bodypin"; axq[4] = "bodypinq";
    ax[5] = "bodyref"; axq[5] = "bodyrefq";

    /* Print the sdgetpin(joint,axis,pinout) routine. */
    declare_proc(F, 0, "getpin",
      VT_INTEGER,             "joint", 
      VT_DUP,                 "pinno", 
      VT_REAL|VT_VECTOR,    "pinout",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, SysI.nl, "ljnum",
      VT_INTEGER,                   "offs",
      0);
    DECL_CHKJPIN(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JPIN(F, "joint", "pinno", ROU_sdgetpin);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        esprintf(nbod, "%d", SysI.n);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
            efprintf(F, "ljnum%=joint-%s%;\n",nbod);
            for (axno = 0; axno < 6; axno++) {
                esprintf(axstr, "%@d", axno);
                efprintf(F, Lang->stmt_if2_b);
                efprintf(F, "pinno");
                efprintf(F, Lang->stmt_if2_then, Lang->eq_op, axstr);
                for (i=0; i<3; i++)
                  efprintf(F, "pinout%(%@d%)%=%s%(ljnum%,%@d%)%;\n", 
                              i, ax[axno], i);
                efprintf(F, Lang->stmt_if2_e);
            }
        efprintf(F, Lang->stmt_if2_else);
    } 

    efprintf(F, "offs%=firstq%(joint%)+pinno");
    if (Lang->subs_offset)
            efprintf(F, "-%d",Lang->subs_offset);
    ENDSTMT;

    /* See SDPIN above for an explanation of the next two "IF"s. */
    IFCOND efprintf(F, "jtype%(joint%)%s%d",EQ,cRev6dJoint);
    THEN
        SET("offs", "offs+3");
    ENDIF;
    IFCOND efprintf(F, "jtype%(joint%)%s%d",EQ,cBearingJoint);
    THEN
        SET("offs", "offs+1");
    ENDIF;

    for (i = 0; i < 3; i++)
        efprintf(F, "pinout%(%@d%)%=pin%(offs%,%@d%)%;\n", i, i);

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDPRES(FILE *F)
{
    char lastbod[20],sdindxnm[20];

    /* Print the sdpres(joint,axis,presin) routine. */
    declare_proc(F, 0, "pres",
      VT_INTEGER,      "joint", 
      VT_DUP,          "axis", 
      VT_DUP,          "presin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    esprintf(sdindxnm, "%@Aindx");
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, sdindxnm,
      VT_INTEGER,        "anyques",
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JAXIS(F, "joint", "axis", ROU_sdpres);

    IFCOND efprintf(F, "(presin%s0)%s(presin%s1)", NE, AND_OP, NE);
    THEN
      SETERR(F, ROU_sdpres, ERR_BadValueForPres);
    ENDIF;

    ISET("anyques", 0);
    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "lpresq%(%s(joint,axis)%)", sdindxnm);
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
            efprintf(F,     Lang->stmt_if2_b);
            efprintf(F,     "presin");
            efprintf(F,     Lang->stmt_if2_then, Lang->ne_op, "0");
            efprintf(F,         "lpres%(%s(joint,axis)%)%=%r%;\n",sdindxnm,1.0);
            efprintf(F,     Lang->stmt_if2_else);
            efprintf(F,         "lpres%(%s(joint,axis)%)%=%r%;\n",sdindxnm,0.0);
            efprintf(F,     Lang->stmt_if2_e);
            efprintf(F,     "lpresq%(%s(joint,axis)%)%=%d%;\n", 
                                sdindxnm, ISQUESFLG|HASNOMFLG);
            ISET(           "anyques", 1);
            efprintf(F, Lang->stmt_if2_e);
        efprintf(F, Lang->stmt_if2_else);
    } 

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "presq%(%s(joint,axis)%)", sdindxnm);
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, "0");
    efprintf(F,     Lang->stmt_if2_b);
    efprintf(F,     "presin");
    efprintf(F,     Lang->stmt_if2_then, Lang->ne_op, "0");
    efprintf(F,         "pres%(%s(joint,axis)%)%=%r%;\n",sdindxnm,1.0);
    efprintf(F,     Lang->stmt_if2_else);
    efprintf(F,         "pres%(%s(joint,axis)%)%=%r%;\n",sdindxnm,0.0);
    efprintf(F,     Lang->stmt_if2_e);
    efprintf(F,     "presq%(%s(joint,axis)%)%=%d%;\n", 
                        sdindxnm, ISQUESFLG|HASNOMFLG);
    ISET(           "anyques", 1);
    efprintf(F, Lang->stmt_if2_e);

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    IF("anyques", EQ, "0");
      THEN SETERR(F, ROU_sdpres, ERR_TriedToSetNonQues);
    ENDIF;

    /* WW must now be re-calculated since a constraint which may have
     * been on might now be off or vice versa.
     */
    ISET("wwflg", 0);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDCONSCHG(FILE *F)
{
    /* Print the sdconschg() routine. */
    declare_proc(F, 0, "conschg",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    /* WW must now be re-calculated since some constraint
     * has changed.
     */
    ISET("wwflg", 0);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETPRES(FILE *F)
{
    char lastbod[20],sdindxnm[20],str0[20];

    /* Print the sdgetpres(joint,axis,presout) routine. */
    declare_proc(F, 0, "getpres",
      VT_INTEGER,            "joint", 
      VT_DUP,              "axis", 
      VT_INTEGER|VT_BYREF, "presout",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    esprintf(sdindxnm, "%@Aindx");
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, sdindxnm,
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JAXIS(F, "joint", "axis", ROU_sdgetpres);

    esprintf(str0, "%r", 0.0);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
        efprintf(F,     Lang->stmt_if2_b);
        efprintf(F,     "lpres%(%s(joint,axis)%)",sdindxnm);
        efprintf(F,     Lang->stmt_if2_then, Lang->ne_op, str0);
        efprintf(F,         "%spresout%=%d%;\n", Lang->deref, 1);
        efprintf(F,     Lang->stmt_if2_else);
        efprintf(F,         "%spresout%=%d%;\n", Lang->deref, 0);
        efprintf(F,     Lang->stmt_if2_e);
        efprintf(F, Lang->stmt_if2_else);
    } 

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "pres%(%s(joint,axis)%)",sdindxnm);
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str0);
    efprintf(F,     "%spresout%=%d%;\n", Lang->deref, 1);
    efprintf(F, Lang->stmt_if2_else);
    efprintf(F,     "%spresout%=%d%;\n", Lang->deref, 0);
    efprintf(F, Lang->stmt_if2_e);

    if (SysI.nl)
        efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDSTAB(FILE *F)
{
    int anyques;

    /* Print the sdstab(velin,posin) routine. */
    declare_proc(F, 0, "stab",
      VT_REAL, "velin",
      VT_DUP,  "posin",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);
    anyques = 0;
    if (SysI.StabVelFlg & ISQUESFLG) {
        efprintf(F, "stabvel%=velin%;\n");
        efprintf(F, "stabvelq%=%d%;\n", ISQUESFLG|HASNOMFLG);
        anyques = 1;
    }
    if (SysI.StabPosFlg & ISQUESFLG) {
        efprintf(F, "stabpos%=posin%;\n");
        efprintf(F, "stabposq%=%d%;\n", ISQUESFLG|HASNOMFLG);
        anyques = 1;
    }

    if (!anyques)
      SETERR(F, ROU_sdstab, ERR_TriedToSetNonQues);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETSTAB(FILE *F)
{
    /* Print the sdgetstab(velout, posout) routine. */
    declare_proc(F, 0, "getstab",
      VT_REAL|VT_BYREF,    "velout",
      VT_DUP|VT_BYREF,     "posout",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);
    efprintf(F, "%svelout%=stabvel%;\n",Lang->deref);
    efprintf(F, "%sposout%=stabpos%;\n",Lang->deref);
    efprintf(F, Lang->proc_end);
}


static void
make_info_entry(FILE *F,
                int loc,
                char *var)
{
    efprintf(F, "info%(%@d%)%=%s%;\n", loc, var);
}

void PRINT_SDINFO(FILE *F,
             int nonred)
{
    /* Print the sdinfo(info) routine. */
    declare_proc(F, 0, "info",
      VT_INTEGER|VT_ARRAY, "info", NINFO, 0,
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    make_info_entry(F, GROUND_LOC, "ground");
    make_info_entry(F, NBOD_LOC,   "nbod");
    make_info_entry(F, NDOF_LOC,   "ndof");
    make_info_entry(F, NC_LOC,     "ncons");
    make_info_entry(F, NLOOP_LOC,  "nloop");
    make_info_entry(F, NLDOF_LOC,  "nldof");
    make_info_entry(F, NLOOPC_LOC, "nloopc");
    make_info_entry(F, NBALL_LOC,  "nball");
    make_info_entry(F, NLBALL_LOC, "nlball");
    make_info_entry(F, NPRES_LOC,  "npres");
    make_info_entry(F, NUSER_LOC,  "nuser");
    efprintf(F, "info%(%@d%)%=%d%;\n", RANK_LOC, nonred);

    efprintf(F, "%{info entries from %@d-%@d are reserved%}",
                INFORES_LOC,NINFO-1);

    efprintf(F, Lang->proc_end);
}

static void
make_jnt_entry(FILE *F,
               int loc,
               char *var)
{
    efprintf(F, "info%(%@d%)%=%s%(joint%)%;\n", loc, var);
}

void PRINT_SDJNT(FILE *F)
{
    char lastbod[20], ndof[40];

    /* Print the sdjnt(joint,info,tran) routine. */
    declare_proc(F, 0, "jnt",
      VT_INTEGER, "joint",
      VT_INTEGER|VT_ARRAY, "info", NINFO, 0,
      VT_INTEGER|VT_ARRAY, "tran", 6, 0,
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER, "i",
      VT_DUP,     "offs",
      0);
    DECL_CHKJNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JNUM(F, "joint", ROU_sdjnt);

    make_jnt_entry(F, JTYPE_LOC,     "jtype");

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->le_op, lastbod);
        efprintf(F,     "info%(%@d%)%=%d%;\n", ISLOOP_LOC, 0);
        efprintf(F,     "offs%=%d%;\n", 0);
        efprintf(F, Lang->stmt_if2_else);
        efprintf(F,     "info%(%@d%)%=%d%;\n", ISLOOP_LOC, 1);
        efprintf(F,     "offs%=%d%;\n", SysI.s);
        efprintf(F, Lang->stmt_if2_e);
    } else {
        efprintf(F,"info%(%@d%)%=%d%;\n", ISLOOP_LOC, 0);
        efprintf(F,"offs%=%d%;\n", 0);
    }

    make_jnt_entry(F, INB_LOC,     "inb");
    make_jnt_entry(F, OUTB_LOC,    "outb");
    make_jnt_entry(F, NJNTDOF_LOC, "njntdof");
    make_jnt_entry(F, NJNTC_LOC,   "njntc");
    make_jnt_entry(F, NJNTP_LOC,   "njntp");
    make_jnt_entry(F, FIRSTQ_LOC,  "firstq");
    make_jnt_entry(F, BALLQ_LOC,   "ballq");
    make_jnt_entry(F, FIRSTM_LOC,  "firstm");
    make_jnt_entry(F, FIRSTP_LOC,  "firstp");
    efprintf(F, "%{info entries from %@d-%@d are reserved%}\n",
                JNTRES_LOC,NINFO-1);

    esprintf(ndof, "njntdof%(joint%)");
    efprintf(F, Lang->stmt_for_b, "100", "i", "0", "5");
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "i");
    efprintf(F, Lang->stmt_if2_then, Lang->lt_op, ndof);
    efprintf(F,     "tran%(i");
    if (Lang->subs_offset)
        efprintf(F, "+%d",Lang->subs_offset);
    efprintf(F,     "%)%=trans%(offs+firstq%(joint%)+i%)%;\n");
    efprintf(F, Lang->stmt_if2_else);
    efprintf(F,     "tran%(i");
    if (Lang->subs_offset)
        efprintf(F, "+%d",Lang->subs_offset);
    efprintf(F,     "%)%=-1%;\n");
    efprintf(F, Lang->stmt_if2_e);
    efprintf(F, Lang->stmt_for_e, "100");

    efprintf(F, Lang->proc_end);
}

void PRINT_SDCONS(FILE *F)
{
    /* Print the sdcons(consno, info) routine. */
    declare_proc(F, 0, "cons",
      VT_INTEGER, "consno",
      VT_INTEGER|VT_ARRAY, "info", NINFO, 0,
      0);
    efprintf(F, Lang->proc_dbegin);
    if (SysI.nu)
        declare_sdgtopo_vars(F, DECL_NODSYM);
    efprintf(F, Lang->proc_dend);
    DECL_CHKUCNUM(F);
    efprintf(F, Lang->proc_sbegin);

    CHECK_UCNUM(F, "consno", ROU_sdcons);

    if (SysI.nu) {
        efprintf(F, "info%(%@d%)%=%d%;\n", CTYPE_LOC, USERCONS);
        efprintf(F, "info%(%@d%)%=firstu%(consno%)%;\n", MULT_LOC);
        efprintf(F, "%{info entries from %@d-%@d are reserved%}",
                    CONSRES_LOC,NINFO-1);
    } else
        efprintf(F, "%{There are no user constraints in this problem.%}");

    efprintf(F, Lang->proc_end);
}

void PRINT_SDPRESACC(FILE *F)
{
    PRINT_SETPRESVAL(F, ROU_sdpresacc, "presacc", "uacc", "lacc");
}

void PRINT_SDPRESVEL(FILE *F)
{
    PRINT_SETPRESVAL(F, ROU_sdpresvel, "presvel", "uvel", "lvel");
}

void PRINT_SDPRESPOS(FILE *F)
{
    PRINT_SETPRESVAL(F, ROU_sdprespos, "prespos", "upos", "lpos");
}

/* Generate the sdpresacc, sdpresvel, and sdprespos routines. */
void PRINT_SETPRESVAL(FILE *F,
                 int routine,
                 char *pname,
                 char  *vname,
                 char *lvname)
{
    char lastbod[20],sdindxnm[20],indx[30],str_flt0[10];

    declare_proc(F, 0,  pname,
      VT_INTEGER,         "joint", 
      VT_DUP,             "axis", 
      VT_REAL,            "prval",
      0);

    if (!SysI.np) {
        PRINT_STUB(F);
        return;
    }

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    esprintf(sdindxnm, "%@Aindx");
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, sdindxnm,
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JAXIS(F, "joint", "axis", routine);

    CHECK_STATE(F, ST_STATEREADY, ST_NOSTATE, 
                routine, ERR_sdstateMustBeCalledFirst);

    esprintf(indx, "%s(joint,axis)", sdindxnm);
    esprintf(str_flt0, "%r", 0.0);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
            efprintf(F, Lang->stmt_if2_b);
            efprintf(F, "lpres%(%s%)", indx);
            efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
            efprintf(F,     "%s%(%s%)%=prval%;\n",lvname,indx);
            efprintf(F, Lang->stmt_if2_e);
        efprintf(F, Lang->stmt_if2_else);
    }

    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "pres%(%s%)", indx);
    efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
    efprintf(F,     "%s%(%s%)%=prval%;\n",vname,indx);
    efprintf(F, Lang->stmt_if2_e);

    if (SysI.nl) 
        efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDGETHT(FILE *F)
{
    char lastbod[20],sdindxnm[20],indx[30];

    esprintf(sdindxnm, "%@Aindx");

    /* Print the sdgetht(joint,axis,torque) routine. */
    declare_proc(F, 0, "getht",
      VT_INTEGER,         "joint", 
      VT_DUP,             "axis", 
      VT_REAL|VT_BYREF, "torque",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, sdindxnm,
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JAXIS(F, "joint", "axis", ROU_sdgetht);

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE, 
                ROU_sdgetht, ERR_sdderivMustBeCalledFirst);

    esprintf(indx, "%s(joint,axis)",sdindxnm);

    if (SysI.nl) {
        esprintf(lastbod, "%@d", SysI.n-1);
        efprintf(F, Lang->stmt_if2_b);
        efprintf(F, "joint");
        efprintf(F, Lang->stmt_if2_then, Lang->gt_op, lastbod);
        efprintf(F,     "%storque%=ltauc%(%s%)%;\n",Lang->deref,indx);
        efprintf(F, Lang->stmt_if2_else);
        efprintf(F,     "%storque%=tauc%(%s%)%;\n",Lang->deref,indx);
        efprintf(F, Lang->stmt_if2_e);
    } else
        efprintf(F, "%storque%=tauc%(%s%)%;\n",Lang->deref,indx);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDHINGET(FILE *F)
{
    char lastbod[20],sdindxnm[20],indx[30];

    esprintf(lastbod, "%@d", SysI.n-1);
    esprintf(sdindxnm, "%@Aindx");

    /* Print the sdhinget(joint,axis,torque) routine. */
    declare_proc(F, 0, "hinget",
      VT_INTEGER,         "joint", 
      VT_DUP,             "axis", 
      VT_REAL,            "torque",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_INTEGER|VT_COND, Lang == &FORTRAN_language, sdindxnm,
      0);
    DECL_CHKJAXIS(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_JAXIS(F, "joint", "axis", ROU_sdhinget);

    CHECK_STATE(F, ST_STATEREADY, ST_NOSTATE, 
                ROU_sdhinget, ERR_sdstateMustBeCalledFirst);

    esprintf(indx, "%s(joint,axis)",sdindxnm);

    IF("mfrcflg", NE, "0")
    THEN
      if (SysI.nl) {
          IF("joint", GT, lastbod)
          THEN
            efprintf(F,"mltau%(%s%)%=mltau%(%s%)+torque%;\n",indx,indx);
          ELSE
            efprintf(F,"mtau%(%s%)%=mtau%(%s%)+torque%;\n",indx,indx);
          ENDIF;
      } else
          efprintf(F, "mtau%(%s%)%=mtau%(%s%)+torque%;\n",indx,indx);
    ELSE
      ISET("fs0flg", 0);

      if (SysI.nl) {
          IF("joint", GT, lastbod)
          THEN
            efprintf(F,"ltau%(%s%)%=ltau%(%s%)+torque%;\n",indx,indx);
            ISET("ltauflg", 0);
          ELSE
            efprintf(F,"utau%(%s%)%=utau%(%s%)+torque%;\n",indx,indx);
          ENDIF;
      } else
          efprintf(F, "utau%(%s%)%=utau%(%s%)+torque%;\n",indx,indx);
    ENDIF;

    efprintf(F, Lang->proc_end);
}

void PRINT_SDPOINTF(FILE *F)
{
    int i;

    /* Print the sdpointf(body,point,force) routine. */
    declare_proc(F, 0, "pointf",
      VT_INTEGER,         "body", 
      VT_VECTOR,             "point", 
      VT_DUP,            "force",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_VECTOR, "torque",
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdpointf);

    CHECK_STATE(F, ST_STATEREADY, ST_NOSTATE, 
                ROU_sdpointf, ERR_sdstateMustBeCalledFirst);

    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
      THEN RETURN;
    ENDIF;

    efprintf(F, "torque%(%@d%)%=", 0);
    efprintf(F, "point%(%@d%)*force%(%@d%)",1,2);
    efprintf(F, "-point%(%@d%)*force%(%@d%)%;\n",2,1);
    efprintf(F, "torque%(%@d%)%=", 1);
    efprintf(F, "point%(%@d%)*force%(%@d%)",2,0);
    efprintf(F, "-point%(%@d%)*force%(%@d%)%;\n",0,2);
    efprintf(F, "torque%(%@d%)%=", 2);
    efprintf(F, "point%(%@d%)*force%(%@d%)",0,1);
    efprintf(F, "-point%(%@d%)*force%(%@d%)%;\n",1,0);

    IF("mfrcflg", NE, "0")
    THEN
      for (i=0; i<3; i++) {
          efprintf(F, "mfk%(body%,%@d%)%=mfk%(body%,%@d%)+force%(%@d%)%;\n",
                   i,i,i);
          efprintf(F, "mtk%(body%,%@d%)%=mtk%(body%,%@d%)+torque%(%@d%)%;\n",
                   i,i,i);
      }
    ELSE
      ISET("fs0flg", 0);

      for (i=0; i<3; i++) {
          efprintf(F, "ufk%(body%,%@d%)%=ufk%(body%,%@d%)+force%(%@d%)%;\n",
                   i,i,i);
          efprintf(F, "utk%(body%,%@d%)%=utk%(body%,%@d%)+torque%(%@d%)%;\n",
                   i,i,i);
      }
    ENDIF;

    efprintf(F, Lang->proc_end);
}

void PRINT_SDBODYT(FILE *F)
{
    int i;

    /* Print the sdbodyt(body,torque) routine. */
    declare_proc(F, 0, "bodyt",
      VT_INTEGER,         "body", 
      VT_VECTOR,            "torque",
      0);
    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdbodyt);

    CHECK_STATE(F, ST_STATEREADY, ST_NOSTATE, 
                ROU_sdbodyt, ERR_sdstateMustBeCalledFirst);

    IFCOND efprintf(F, "body%s%@d", EQ, cGroundBody);
      THEN RETURN;
    ENDIF;

    IF("mfrcflg", NE, "0")
    THEN
      for (i=0; i<3; i++)
          efprintf(F, "mtk%(body%,%@d%)%=mtk%(body%,%@d%)+torque%(%@d%)%;\n",
                   i,i,i);
    ELSE
      ISET("fs0flg", 0);
        
      for (i=0; i<3; i++)
          efprintf(F, "utk%(body%,%@d%)%=utk%(body%,%@d%)+torque%(%@d%)%;\n",
                   i,i,i);
    ENDIF;

    efprintf(F, Lang->proc_end);
}
