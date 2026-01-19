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
 * Code to generate the auxiliary routines:
 *   sdpos         sdvel        sdacc
 *   sdorient      sdangvel        sdangacc
 *   sdtrans
 * 
 * These routines use data expected already to be in a global or common
 * area as a result of an immediately preceding call to SDSTATE.
 */

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#include "../calc/gencode.h"

/* 
 * sdpos(body,pt, loc)
 *
 * Returns location in inertial space of the given point on the given body.
 * User provides point relative to body COM, in body frame.
 */
void PRINT_SDPOS(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "pos",
      VT_INTEGER, "body", 
      VT_VECTOR,  "pt", 
      VT_VECTOR,  "loc", 
      0);

    efprintf(F, "%{\n\
Return inertial frame location of a point on a body.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_VECTOR, "pv", 
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdpos);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdpos, ERR_sdstateMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "body");
    efprintf(F, Lang->stmt_if2_then, Lang->eq_op, groundbod);
        for (i=0; i<3; i++) 
            efprintf(F, "loc%(%@d%)%=pt%(%@d%)%;\n",i,i);
    efprintf(F, Lang->stmt_if2_else);
        /* Calculate into a temp in case pt and loc are the same vector. */
        for (i=0; i<3; i++) {
            efprintf(F, "pv%(%@d%)%=",i);
            efprintf(F, "rnb%(body%,%@d%)+pt%(%@d%)*",i,0);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pt%(%@d%)*",i,0,1);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pt%(%@d%)*",i,1,2);
            efprintf(F, "cnb%(body%,%@d%,%@d%)%;\n",i,2);
        }
        for (i=0; i<3; i++) 
            efprintf(F, "loc%(%@d%)%=pv%(%@d%)%;\n",i,i);
    efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDVEL(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "vel",
      VT_INTEGER, "body", 
      VT_VECTOR,  "pt", 
      VT_VECTOR,  "velo", 
      0);

    efprintf(F, "%{\n\
Return inertial frame velocity of a point on a body.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_VECTOR, "pv", 
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdvel);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdvel, ERR_sdstateMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "body");
    efprintf(F, Lang->stmt_if2_then, Lang->eq_op, groundbod);
        for (i=0; i<3; i++) 
            efprintf(F, "velo%(%@d%)%=%r%;\n",i,0.0);
    efprintf(F, Lang->stmt_if2_else);
        efprintf(F, 
          "pv%(%@d%)%=wb%(body%,%@d%)*pt%(%@d%)-wb%(body%,%@d%)*pt%(%@d%)%;\n",
          0,1,2,2,1);
        efprintf(F, 
          "pv%(%@d%)%=wb%(body%,%@d%)*pt%(%@d%)-wb%(body%,%@d%)*pt%(%@d%)%;\n",
          1,2,0,0,2);
        efprintf(F, 
          "pv%(%@d%)%=wb%(body%,%@d%)*pt%(%@d%)-wb%(body%,%@d%)*pt%(%@d%)%;\n",
          2,0,1,1,0);

        for (i=0; i<3; i++) {
            efprintf(F, "velo%(%@d%)%=",i);
            efprintf(F, "vnb%(body%,%@d%)+pv%(%@d%)*",i,0);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pv%(%@d%)*",i,0,1);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pv%(%@d%)*",i,1,2);
            efprintf(F, "cnb%(body%,%@d%,%@d%)%;\n",i,2);
        }
    efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDORIENT(FILE *F)
{
    register int i,j;
    char groundbod[20];

    declare_proc(F, 0, "orient",
      VT_INTEGER, "body", 
      VT_MATRIX,  "dircos", 
      0);

    efprintf(F, "%{\n\
Return orientation of body w.r.t. ground frame.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdorient);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdorient, ERR_sdstateMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);
    IF("body", EQ, groundbod)
      THEN
        for (i=0; i<3; i++) 
            for (j=0; j<3; j++)
                efprintf(F, 
                    "dircos%(%@d%,%@d%)%=%r%;\n",i,j,(i==j?1.0:0.0));
      ELSE
        for (i=0; i<3; i++) 
            for (j=0; j<3; j++) {
                efprintf(F, "dircos%(%@d%,%@d%)%=",i,j);
                efprintf(F, "cnb%(body%,%@d%,%@d%)%;\n",i,j);
            }
    ENDIF;

    efprintf(F, Lang->proc_end);
}

void PRINT_SDANGVEL(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "angvel",
      VT_INTEGER,  "body", 
      VT_VECTOR,   "avel",
      0);

    efprintf(F, "%{\n\
Return angular velocity of the body.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdangvel);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdangvel, ERR_sdstateMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);

    IF("body", EQ, groundbod)
      THEN
        for (i=0; i<3; i++) 
            efprintf(F, "avel%(%@d%)%=%r%;\n",i,0.0);
      ELSE
        for (i=0; i<3; i++) {
            efprintf(F, "avel%(%@d%)%=",i);
            efprintf(F, "wb%(body%,%@d%)%;\n",i);
        }
    ENDIF;

    efprintf(F, Lang->proc_end);
}

/* 
 * sdtrans(frbod, vec, tobod, ovec)
 *
 * Transform vec (in frbod frame) to ovec (in tobod frame).
 */
void PRINT_SDTRANS(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "trans",
      VT_INTEGER, "frbod", 
      VT_VECTOR,  "ivec", 
      VT_INTEGER, "tobod", 
      VT_VECTOR,  "ovec", 
      0);

    efprintf(F, "%{\n\
Transform ivec from frbod frame to tobod frame.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_VECTOR,  "pv",
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "frbod", ROU_sdtrans);
    CHECK_BNUM(F, "tobod", ROU_sdtrans);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdtrans, ERR_sdstateMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);

    IF ("frbod", EQ, "tobod")
    THEN
        CALL("%Avcopy(ivec,ovec)");
        RETURN;
    ENDIF;

    IF("frbod", EQ, groundbod)
    THEN
        CALL("%Avcopy(ivec,pv)");
        for (i=0; i<3; i++) {
            efprintf(F, "ovec%(%@d%)%=",i);
            efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)+",0,0,i);
            efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)+",1,1,i);
            efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)%;\n",2,2,i);
        }
        RETURN;
    ENDIF;

    IF("tobod", EQ, groundbod)
    THEN
        CALL("%Avcopy(ivec,pv)");
        for (i=0; i<3; i++) {
            efprintf(F, "ovec%(%@d%)%=",i);
            efprintf(F, "pv%(%@d%)*cnb%(frbod%,%@d%,%@d%)+",0,i,0);
            efprintf(F, "pv%(%@d%)*cnb%(frbod%,%@d%,%@d%)+",1,i,1);
            efprintf(F, "pv%(%@d%)*cnb%(frbod%,%@d%,%@d%)%;\n",2,i,2);
        }
        RETURN;
    ENDIF;

    /* Neither is on ground */

    for (i=0; i<3; i++) {
        efprintf(F, "pv%(%@d%)%=",i);
        efprintf(F, "ivec%(%@d%)*cnb%(frbod%,%@d%,%@d%)+",0,i,0);
        efprintf(F, "ivec%(%@d%)*cnb%(frbod%,%@d%,%@d%)+",1,i,1);
        efprintf(F, "ivec%(%@d%)*cnb%(frbod%,%@d%,%@d%)%;\n",2,i,2);
    }
    for (i=0; i<3; i++) {
        efprintf(F, "ovec%(%@d%)%=",i);
        efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)+",0,0,i);
        efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)+",1,1,i);
        efprintf(F, "pv%(%@d%)*cnb%(tobod%,%@d%,%@d%)%;\n",2,2,i);
    }

    efprintf(F, Lang->proc_end);
}

void PRINT_SDACC(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "acc",
      VT_INTEGER, "body", 
      VT_VECTOR,  "pt",
      VT_DUP,     "accel",
      0);

    efprintf(F, "%{\n\
Return linear acceleration a point of the specified body.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_vars(F, DECL_NODSYM,
      VT_VECTOR,  "pv", 
      0);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdacc);

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE, 
                ROU_sdacc, ERR_sdderivMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);
    efprintf(F, Lang->stmt_if2_b);
    efprintf(F, "body");
    efprintf(F, Lang->stmt_if2_then, Lang->eq_op, groundbod);
        for (i=0; i<3; i++)
            efprintf(F, "accel%(%@d%)%=%r%;\n",i,0.0);
    efprintf(F, Lang->stmt_if2_else);
        for (i=0; i<3; i++) {
            efprintf(F, "pv%(%@d%)%=",i);
            efprintf(F, "pt%(%@d%)*dyad%(body%,%@d%,%@d%)+",0,i,0);
            efprintf(F, "pt%(%@d%)*dyad%(body%,%@d%,%@d%)+",1,i,1);
            efprintf(F, "pt%(%@d%)*dyad%(body%,%@d%,%@d%)%;\n",2,i,2);
        }

        for (i=0; i<3; i++) {
            efprintf(F, "accel%(%@d%)%=",i);
            efprintf(F, "anb%(body%,%@d%)+pv%(%@d%)*",i,0);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pv%(%@d%)*",i,0,1);
            efprintf(F, "cnb%(body%,%@d%,%@d%)+pv%(%@d%)*",i,1,2);
            efprintf(F, "cnb%(body%,%@d%,%@d%)%;\n",i,2);
        }

    efprintf(F, Lang->stmt_if2_e);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDANGACC(FILE *F)
{
    register int i;
    char groundbod[20];

    declare_proc(F, 0, "angacc",
      VT_INTEGER, "body", 
      VT_VECTOR,  "aacc",
      0);

    efprintf(F, "%{\n\
Return angular acceleration of the body.\n\n");
    efprintf(F, "%}");

    efprintf(F, Lang->proc_dbegin);
    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgtopo_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    DECL_CHKBNUM(F);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_BNUM(F, "body", ROU_sdangacc);

    CHECK_STATE(F, ST_DERIVREADY, ST_NOSTATE, 
                ROU_sdangacc, ERR_sdderivMustBeCalledFirst);

    esprintf(groundbod, "%@d", cGroundBody);

    IF("body", EQ, groundbod)
      THEN
        for (i=0; i<3; i++)
            efprintf(F, "aacc%(%@d%)%=%r%;\n",i,0.0);
      ELSE
        for (i=0; i<3; i++) {
            efprintf(F, "aacc%(%@d%)%=",i);
            efprintf(F, "onb%(body%,%@d%)%;\n",i);
        }
    ENDIF;

    efprintf(F, Lang->proc_end);
}
