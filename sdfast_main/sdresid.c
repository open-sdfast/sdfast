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

/* PRINT_SDRESID
 *
 * Generate the sdresid() subroutine.
 */
void PRINT_SDRESID(FILE *F,
              opstats_t *opcnt)
{
    long ladd, lmul, lasg;
    char str_nq[10], str_s[10], str_nc[10], str_flt0[10];
    int  nresid;

    nresid = SysI.nq + SysI.s + SysI.nc;
    if (nresid < 1)
        nresid = 1;        /* keep up appearances */

    esprintf(str_nq, "%d", SysI.nq);
    esprintf(str_s, "%d", SysI.s);
    esprintf(str_nc, "%d", SysI.nc);
    esprintf(str_flt0, "%r", 0.);

    START_COMPSUB(opcnt);
    ladd = 0; /* locally printed adds and subtracts */
    lmul = 0; /* locally printed multiplies */
    lasg = 0; /* locally printed `=' */

    efprintf(F, "%{\nCompute residuals for use with DAE integrator\n%}");

    /* Declare the SDRESID routine heading. */
    declare_proc(F, 0, "resid",
      VT_USER, &SysI.type_Arr_nq,           "eqdot",
      VT_USER, &SysI.type_Arr_s,            "eudot", 
      VT_USER, &SysI.type_Arr_nc,           "emults",
      VT_ARRAY,                             "resid", nresid, 0,
      0);
    
    efprintf(F, Lang->proc_dbegin);

    declare_sdginput_vars(F, DECL_NODSYM);
    declare_sdgstate_vars(F, DECL_NODSYM);
    declare_sdglhs_vars(F, DECL_NODSYM);
    declare_sdgrhs_vars(F, DECL_NODSYM);
    declare_sdgtemp_vars(F, DECL_NODSYM);

    declare_vars(F, 0, 
      VT_INTEGER,                                          "i",
      VT_USER,                  &SysI.type_Arr_s,         "uderrs",
      VT_USER|VT_COND, SysI.nc, &SysI.type_Arr_nc,         "p0",
      0);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    CHECK_STATE(F, ST_STATEREADY, ST_DERIVREADY, 
                ROU_sdresid, ERR_sdstateMustBeCalledFirst);

    /* Make sure stabpos parameter has been supplied if it was
     * entered as stabpos=? in the input file.
     */
    IFCOND efprintf(F, "stabposq%s%d", EQ, ISQUESFLG);
      THEN SETERR(F, ROU_sdresid, ERR_StabposMustBeSpecified);
    ENDIF;

    CALL("%Afulltrq(eudot,emults,uderrs)");
    opcnt->nfulltrq++;

    /* Load up the residual array. */
    FORCNT("100", "i", str_nq);
      SET("resid%(i%)", "eqdot%(i%)-qdot%(i%)");
    ENDFOR("100");
    ladd += SysI.nq;
    lasg += SysI.nq;

    FORCNT("200", "i", str_s);
      efprintf(F, "resid%(%d+i%)%=uderrs%(i%)%;\n", SysI.nq);
    ENDFOR("200");
    lasg += SysI.s;

    if (SysI.nc) {
        CALL1("%Averr(%Rresid%(%@d%))", SysI.nq+SysI.s);

        /* Stabilize position errors with Baumgarte's help. */
        if (!IS_ZERO(VAL(SysI.stabpos))) {
            if (SysI.StabPosFlg & ISQUESFLG) {
                efprintf(F, Lang->stmt_if2_b);
                efprintf(F, "%s", PRINTNAME(SysI.stabpos));
                efprintf(F, Lang->stmt_if2_then, Lang->ne_op, str_flt0);
            }
            CALL("%Aperr(p0)");
            FORCNT("250", "i", str_nc);
                efprintf(F, "resid%(%d+i%)%=resid%(%d+i%)+",
                         SysI.nq+SysI.s, SysI.nq+SysI.s);
                if (IS_ONE(VAL(SysI.stabpos)))
                    efprintf(F, "p0%(i%)%;\n");
                else {
                    PRINT_E(F, VAL(SysI.stabpos));
                    efprintf(F, "*p0%(i%)%;\n");
                }
            ENDFOR("250");
            if (SysI.StabPosFlg & ISQUESFLG)
                efprintf(F, Lang->stmt_if2_e);
            lasg += SysI.nc;
            ladd += SysI.nc;
            lmul += SysI.nc;
        }
    }

    /* So that we can do further computations by making
     * a call to SDRHS(), we need to make sure
     * that the acceleration/multiplier-related globals contain Dassl's 
     * estimated values, not the ones we may have calculated above.
     * Qdot's are calculated by sdstate, not in sdresid, so they shouldn't
     * be changed here (otherwise a subsequent call to sdresid would yield
     * a zero residual for qdots).
     */
    FORCNT("350", "i", str_s);
      SET("udot%(i%)","eudot%(i%)");
    ENDFOR("350");

    lasg += SysI.s;

    if (SysI.nc) {  
        FORCNT("375", "i", str_nc);
          SET("mult%(i%)","emults%(i%)");
        ENDFOR("375");
        lasg += SysI.nc;
    }

    CALL0("%Arhs");                /* SDRHS() */
    opcnt->nrhs++;

    END_COMPSUB(F,opcnt,ladd,lmul,0L,lasg);
    efprintf(F, Lang->proc_end);
}
