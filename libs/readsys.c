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

#include "../calc/language.h"
#define MAX(a,b) ((a)>(b)?(a):(b))
#include "libs.h"
#include "libsprot.h"
#include "words.h"

/* READ_SYSTEM 
 *
 * READ_SYSTEM is called to parse and error-check the user's
 * input.  This is converted into a machine readable form in
 * SystemInfo, along with derived parameters such as the
 * total number of system degrees of freedom.
 *
 * After this call (if it succeeds), every field in SystemInfo is valid, and
 * "?" values are replaced with references to the appropriate symbols.
 * Space for tables in SystemInfo is allocated here.  In case of an
 * error return, all space that may have been allocated will have been
 * freed.
 *
 * Note: parse_cmdline must already have been called so that the 
 * global sdfast_opt structure has been filled in with command line
 * options.
 *
 * Returns zero iff all's well.
 */

char 
READ_SYSTEM(FILE *System,
            SystemInfo_t *SystemInfo)
{
    string32 Id;
    register Index_t i, j, k;
    register ConstraintDesc_t *ConstraintP;
    long NextM,NextDOF,NextBallQ;
    int IdNum, nreq, nmax;
    int loopjoint;        /* state memory for the parser */
    JointDesc_t *jntp;

    /* The order here must match the numbers in words.h */
    static char *ReservedWords[] = {
        "BODY", "INBOARD", "JOINT", "MASS", "INERTIA", "BODYTOJOINT",
        "INBOARDTOJOINT", "PIN", "PINS", "BODYPIN", "INB", "BTJ",
        "INBTOJOINT", "ITJ", "GROUNDED", "PRESCRIBED", "INBPIN",
        "INBOARDPIN", "GRAVITY", "6D", "SINGLE", "DOUBLE", "BODYREF",
        "INBREF", "INBOARDREF", "WELD", "LANGUAGE", "CONSTRAINT",
        "CONSTRAINTS", "LOOP", "USER", "RSIXDOF", "RPLANAR", "SIXDOF",
        "PREFIX", "FREE", "PERP", "PERPENDICULAR", "COORD", "COORDINATE",
        "DISTANCE", "CYLINDRICAL", "CYLINDER", "PLANAR", "U", "UJOINT",
        "U-JOINT", "1D", "2D", "3D", "GIMBAL", "BALL", "SLIDER", "SLIDING",
        "FORTRAN", "ADSIM", "C", "PASCAL", "ADA", "STABVEL", "STABPOS",
        "TYPE", "RFREE", "unused64", "POINT", "VECTOR", "GEAR", "SCREW",
        "PITCH", "SCALAR", "JNAME", "BUSHING", "BEARING", 
        "RBUSHING", "RBEARING", "KRC", "ANSIC", "C++"
    };

    /* Allocate and zero stuff in the SystemInfo structure. */

    if (!(SystemInfo->Bodies = (BodyDesc_t *)malloc(sizeof(BodyTable_t)))
      || !(SystemInfo->PseudoBodies =
      (BodyDesc_t *)malloc(sizeof(PseudoBodyTable_t)))
      || !(SystemInfo->LoopConst =
      (LoopDesc_t *)malloc(sizeof(LoopConstTable_t)))
      || !(SystemInfo->Const =
      (ConstraintDesc_t *)malloc(sizeof(ConstTable_t)))) 
    {
        perror("malloc");
        exit(2);
        /*NOTREACHED*/
    }

    (void)memset((char *)SystemInfo->Bodies,      0, sizeof(BodyTable_t));
    (void)memset((char *)SystemInfo->PseudoBodies,0, sizeof(PseudoBodyTable_t));
    (void)memset((char *)SystemInfo->LoopConst,   0, sizeof(LoopConstTable_t));
    (void)memset((char *)SystemInfo->Const,       0, sizeof(ConstTable_t));

    /* Space has now been allocated in SystemInfo.  Any error returns
     * past this point require a FREE_SYSTEM first.
     */

    /* This option may be changed in the input file. */
    SystemInfo->Grounded = 0;       /* assume system is not grounded */

    SystemInfo->GravExpr = NULL;    /* no gravity or stabilization yet */
    SystemInfo->StabVelExpr = NULL;        
    SystemInfo->StabPosExpr = NULL;        
    /* This can be any strange value.  It's the unique ADDRESS that makes
       it a `?', rather than the value. */
    SystemInfo->QuestionMark = PERM(SC(1234.5678));

    /* These are accumulated as the input file is read in. */
    SystemInfo->n = 0;   /* body count (don't count ground) */
    SystemInfo->s = 0;   /* `tree' degrees of freedom */
    SystemInfo->nl = 0;  /* number of loop joints */
    SystemInfo->sl = 0;  /* loop joint `degrees of freedom' */
    SystemInfo->nu = 0;  /* number of user constraints */

    /* These are calculated below after the input file is read in. */
    SystemInfo->nb  = 0; /* # ball joints in the tree system */
    SystemInfo->nlb = 0; /* # loop joints containing balls */
    SystemInfo->np  = 0; /* # prescribed motion constraints */
    SystemInfo->nlc = 0; /* # loop constraints */
    SystemInfo->nxc = 0; /* # explicit constraints */
    SystemInfo->nc  = 0; /* total # constraints */
    SystemInfo->nq  = 0; /* # of q's (=s+nb) */
    SystemInfo->nlq = 0; /* # of lq's (=sl+nlb) */
    SystemInfo->nj  = 0; /* # of joints (=n+nl) */
    SystemInfo->nh  = 0; /* # of hinges (=s+sl) */

    SystemInfo->s_free = 0;    /* # of guaranteed free tree DOF */
    SystemInfo->s_pres = 0;    /* # of guaranteed prescribed tree DOF */
    SystemInfo->s_runtime = 0; /* # of runtime-switchable tree DOF */
    
    /* Process the input file. */

    loopjoint = 0;        /* not currently processing a loop joint */

    if (!READ_ID(System, Id))
        goto error_return;
    while (!feof(System)) {
        if (!(IdNum = LOOKUP(ReservedWords, Id))) {
            USER_ERR();
            fprintf(stderr, "Unrecognized keyword `%s'.\n", Id);
            goto error_return;
        }
        if (!PROCESS_VAR(System,SystemInfo,ReservedWords,IdNum,&loopjoint)
            || !READ_ID(System, Id))
            goto error_return;
    }

    /* Input file has been processed. */

    /* If inboard body for the first body wasn't specified, it's $ground. */
    if (SystemInfo->Bodies[0].jnt.InbBody == cUnspecifiedBody)
        SystemInfo->Bodies[0].jnt.InbBody = cGroundBody;
    SystemInfo->Bodies[0].jnt.OutbBody = 0; /* i.e., the first body */

    /* InbToJoint can be specified for the 1st body, but it's not required. */
    if (SystemInfo->Bodies[0].jnt.InbToJoint == NULL) {
        SystemInfo->Bodies[0].jnt.InbToJoint = VECTOR_ZERO();
        for (i=0; i<3; i++)
            SystemInfo->Bodies[0].jnt.ItjFlg[i] = 0;
    }

    /* If no joint was specified for the first body in the input file,
       we'll fake up a 6dof joint between it and ground unless the system
       is explicitly a `grounded' system, in which case the lack of a
       joint will be treated later as an error. */
    if (!SystemInfo->Grounded &&
        SystemInfo->Bodies[0].jnt.JointKind == cUnknownJoint) 
    {
            SystemInfo->Bodies[0].jnt.JointKind = c6dJoint;        
            SystemInfo->Bodies[0].jnt.JointDOF  = 6;

            if (SystemInfo->Bodies[0].jnt.BodyToJoint == NULL) {
                SystemInfo->Bodies[0].jnt.BodyToJoint = VECTOR_ZERO();
                for (i=0; i<3; i++)
                    SystemInfo->Bodies[0].jnt.BtjFlg[i] = 0;
            }

            if (   SystemInfo->Bodies[0].jnt.Pins[0] == NULL
                && SystemInfo->Bodies[0].jnt.Pins[1] == NULL
                && SystemInfo->Bodies[0].jnt.Pins[2] == NULL)
            {
                SystemInfo->Bodies[0].jnt.Pins[0] = B1();
                SystemInfo->Bodies[0].jnt.Pins[1] = B2();
                SystemInfo->Bodies[0].jnt.Pins[2] = B3();
                for (i=0; i<3; i++)
                    for (j=0; j<3; j++)
                        SystemInfo->Bodies[0].jnt.PinFlg[i][j] = 0;
            }
    }

    /* 
     * Check that user specified everything required. 
     */

    if (SystemInfo->n == 0) {
        USER_ERR();
        fprintf(stderr, "There must be at least one body specified.\n");
        goto error_return;
    }

    /* Check body-specific information. */
    for (i = 0; i < SystemInfo->n; i++) {
        if (!SystemInfo->Bodies[i].Mass) {
            USER_ERR();
            fprintf(stderr, "No mass specified for body `%s'.\n",
              SystemInfo->Bodies[i].BodyName);
            goto error_return;
        }
        if (!SystemInfo->Bodies[i].Inertia) {
            USER_ERR();
            fprintf(stderr, "No inertia matrix specified for body `%s'.\n",
              SystemInfo->Bodies[i].BodyName);
            goto error_return;
        }
    }

    /* Check joint-specific information (both tree and loop joints). */
    for (i = 0; i < SystemInfo->n + SystemInfo->nl; i++) {
        char *what, *name;

        if (i < SystemInfo->n) {
            jntp = &SystemInfo->Bodies[i].jnt;
            what = "body";
            name = SystemInfo->Bodies[i].BodyName;
        } else {
            jntp = &SystemInfo->LoopConst[i-SystemInfo->n].jnt;
            what = "loop joint";
            name = SystemInfo->LoopConst[i-SystemInfo->n].OutbBodyName;
        }

        if (jntp->JointKind == cUnknownJoint) {
            USER_ERR();
            fprintf(stderr, "No joint type specified for %s `%s'.\n",
                    what, name);
            goto error_return;
        }
        if (jntp->InbBody == cUnspecifiedBody) {
            USER_ERR();
            fprintf(stderr, "No inboard body specified for %s `%s'.\n",
                    what,name);
            goto error_return;
        }
        if (!jntp->BodyToJoint) {
            USER_ERR();
            fprintf(stderr, "No BodyToJoint vector specified for %s `%s'.\n",
              what,name);
            goto error_return;
        }
        if (!jntp->InbToJoint) {
            USER_ERR();
            fprintf(stderr, "No InboardToJoint vector specified for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        nreq = JointInfo[(int)jntp->JointKind].nreq;
        nmax = JointInfo[(int)jntp->JointKind].nmax;

        if (nmax  < 3 && jntp->Pins[nmax]) {
            USER_ERR();
            fprintf(stderr, "Too many pins specified for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        if (nreq > 0 && !jntp->Pins[nreq-1]) {
            USER_ERR();
            fprintf(stderr, "Not enough pins specified for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        if (!JointInfo[(int)jntp->JointKind].inbrefOK && jntp->Pins[INBREF]) {
            USER_ERR();
            fprintf(stderr, "No `inbref' allowed for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        if (!JointInfo[(int)jntp->JointKind].bodypinOK && jntp->Pins[BODYPIN]) {
            USER_ERR();
            fprintf(stderr, "No `bodypin' allowed for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        if (!JointInfo[(int)jntp->JointKind].bodyrefOK && jntp->Pins[BODYREF]) {
            USER_ERR();
            fprintf(stderr, "No `bodyref' allowed for %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        if (jntp->Pins[BODYREF] && !jntp->Pins[BODYPIN]) {
            USER_ERR();
            fprintf(stderr, 
            "`bodyref' not allowed without `bodypin' (%s `%s').\n",
                    what,name);
            goto error_return;
        }

        if (jntp->Pins[INBREF] && !jntp->Pins[INBPIN1]) {
            USER_ERR();
            fprintf(stderr, 
            "`inbref' not allowed without specification of a pin (%s `%s').\n",
                    what,name);
            goto error_return;
        }

        if (i >= SystemInfo->n && (   jntp->JointKind == cRevPlanarJoint
                                   || jntp->JointKind == cRev6dJoint
                                   || jntp->JointKind == cRevBushingJoint
                                   || jntp->JointKind == cRevBearingJoint))
        {
            USER_ERR();
            fprintf(stderr, "Reverse joints cannot be used as loop joints.\n");
            goto error_return;
        }

        if (jntp->JointKind == cBallJoint) {
            /* Fill in vectors for convenience */
            for (j=LOW_PIN; j<=HIGH_PIN; j++) {
                jntp->Pins[j] = VECTOR_ZERO();
                for (k=0; k<3; k++)
                    jntp->PinFlg[j][k] = 0;
            }
        }
        if (jntp->JointKind == c6dJoint) {
            jntp->Pins[INBREF] = VECTOR_ZERO();
            jntp->Pins[BODYPIN] = VECTOR_ZERO();
            jntp->Pins[BODYREF] = VECTOR_ZERO();
            for (j=0; j<3; j++) {
                jntp->PinFlg[INBREF][j] = 0;
                jntp->PinFlg[BODYPIN][j] = 0;
                jntp->PinFlg[BODYREF][j] = 0;
            }
        }

        if (JointInfo[(int)jntp->JointKind].dof == 0 && jntp->Pres[0]) {
            USER_ERR();
            fprintf(stderr, 
            "Prescribed motion doesn't make sense for 0-dof %s `%s'.\n",
                    what,name);
            goto error_return;
        }

        /* If `prescribed' keyword was missing (indicated by 
         * Pres[0]==NULLEXPR), all axes should be set to free (0).  
         * If `prescribed' was present but not followed by '= ...' 
         * (indicated by Pres[0]==SPECIALEXPR) then all axes should
         * be set to prescribed (1).  Otherwise, we just make sure
         * the user provided the right number of 1's, 0's, or ?'s
         * after the `=' (one per joint DOF).
         */
        if (jntp->Pres[0] == NULLEXPR)
            for (j = 0; j < jntp->JointDOF; j++) {
                jntp->Pres[j] = SCALAR_ZERO();
                jntp->PresFlg[j] = 0;
            }
        else if (jntp->Pres[0] == SPECIALEXPR)
            for (j = 0; j < jntp->JointDOF; j++) {
                jntp->Pres[j] = SCALAR_ONE();
                jntp->PresFlg[j] = 0;
            }
        else {
            for (j = 0; j < 6; j++)
                if (j < jntp->JointDOF) {
                    if (!jntp->Pres[j]) {
                        USER_ERR();
                        fprintf(stderr,
  "Not enough prescribed indicators specified for %s `%s' -- should be %d.\n",
                                what,name, jntp->JointDOF);
                        goto error_return;
                    }
                } else {
                    if (jntp->Pres[j]) {
                        USER_ERR();
                        fprintf(stderr,
  "Too many prescribed indicators specified for %s `%s' -- should be %d.\n",
                                what, name, jntp->JointDOF);
                        goto error_return;
                    }
                }
        }
    }

    /* Check constraint-specific information. */

    for (i=0; i < SystemInfo->nxc+SystemInfo->nu; i++) {
        int cnt,need;
        char *name;

        ConstraintP = &SystemInfo->Const[i];
        name = ConstraintP->ConstraintName;

        /* the default constraint type is user constraint */
        if (ConstraintP->ConstraintType == cUnknownCons) 
            ConstraintP->ConstraintType = cUserCons;

        /* right number of bodies? */
        for (cnt=0; cnt < MAXCONSBODS 
                    && ConstraintP->Bodies[cnt] != cUnspecifiedBody; cnt++);
        need = ConstraintInfo[(int)ConstraintP->ConstraintType].nbod;
        if (cnt != need) {
            USER_ERR();
            fprintf(stderr,
            "Wrong number of bodies for constraint `%s' (need %d).\n",
                    name, need);
            goto error_return;
        }

        /* right number of joints?  (No need to check axes separately.) */
        for (cnt=0; cnt < MAXCONSJNTS 
                    && ConstraintP->Joints[cnt] != cUnspecifiedJoint; cnt++);
        need = ConstraintInfo[(int)ConstraintP->ConstraintType].njnt;
        if (cnt != need) {
            USER_ERR();
            fprintf(stderr,
            "Wrong number of joint axes for constraint `%s' (need %d).\n",
                    name, need);
            goto error_return;
        }

        /* right number of scalars? */
        for (cnt=0; cnt < MAXCONSSCS 
                    && ConstraintP->Scalars[cnt] != NULLEXPR; cnt++);
        need = ConstraintInfo[(int)ConstraintP->ConstraintType].nsc;
        if (cnt != need) {
            USER_ERR();
            fprintf(stderr,
            "Wrong number of scalars for constraint `%s' (need %d).\n",
                    name, need);
            goto error_return;
        }

        /* right number of points? */
        for (cnt=0; cnt < MAXCONSPTS 
                && ConstraintP->Points[cnt].Body != cUnspecifiedBody; cnt++);
        need = ConstraintInfo[(int)ConstraintP->ConstraintType].npt;
        if (cnt != need) {
            USER_ERR();
            fprintf(stderr,
            "Wrong number of points for constraint `%s' (need %d).\n",
                    name, need);
            goto error_return;
        }

        /* right number of vectors? */
        for (cnt=0; cnt < MAXCONSVECS
                && ConstraintP->Vectors[cnt].Body != cUnspecifiedBody; cnt++);
        need = ConstraintInfo[(int)ConstraintP->ConstraintType].nvec;
        if (cnt != need) {
            USER_ERR();
            fprintf(stderr,
            "Wrong number of vectors for constraint `%s' (need %d).\n",
                    name, need);
            goto error_return;
        }
    }
        
    /* compute several handy quantities */

    NextDOF = 0;   /* next available DOF */
    NextM = 0;           /* next available slot in mults array */
    NextBallQ = SystemInfo->s; /* where in q to stick 4th Euler parameters */
    for (i = 0; i < SystemInfo->n; i++) {
        register int ndof;  /* # of DOF for this joint */
        int first;

        jntp = &SystemInfo->Bodies[i].jnt;
        if ((ndof = jntp->JointDOF) != 0) {
            SystemInfo->FirstDOF[i] = NextDOF;
            NextDOF += ndof;
            SystemInfo->LastDOF[i] = NextDOF - 1;
        } else {
            register int bnum;
            /* Tree weld joint.  Follow down the inboard bodies until we
             * find the first non-welded one or ground.  Then make our
             * LastDOF the same as the non-welded inboard body's LastDOF,
             * FirstDOF = LastDOF+1.  If we're on ground, set LastDOF to -1,
             * FirstDOF to 0.
             */
            for (bnum = jntp->InbBody;
                 bnum != cGroundBody 
                   && SystemInfo->Bodies[bnum].jnt.JointKind == cWeldJoint;
                 bnum = SystemInfo->Bodies[bnum].jnt.InbBody)
                /* loop */ ;
            if (bnum == cGroundBody)
                SystemInfo->LastDOF[i]  = -1;
            else
                SystemInfo->LastDOF[i] = SystemInfo->LastDOF[bnum];
            SystemInfo->FirstDOF[i] = SystemInfo->LastDOF[i] + 1;
        }

        if (JointInfo[(int)jntp->JointKind].hasball) {
            SystemInfo->nb++;
            SystemInfo->BallQ[i] = NextBallQ++;
        } else 
            SystemInfo->BallQ[i] = cUninitializedIndex;

        SystemInfo->PresM[i] = -1;
        first = 1;
        for (j = 0; j < ndof; j++)
            if (!(jntp->PresFlg[j] & ISQUESFLG) && IS_ZERO(jntp->Pres[j])) 
                SystemInfo->s_free++;
            else {
                SystemInfo->np++;
                jntp->JointPres++;
                if (first) {
                    SystemInfo->PresM[i] = NextM;
                    first = 0;
                }
                NextM++;
                if (IS_ONE(jntp->Pres[j])) 
                    SystemInfo->s_pres++;
                else
                    SystemInfo->s_runtime++;
            }
    }

    /* Handle the prescribed loop hinges before the loop constraints so we can 
       put their multipliers next to the prescribed tree hinge multipliers. */
    for (i = 0; i < SystemInfo->nl; i++) {
        int first;
        jntp = &SystemInfo->LoopConst[i].jnt;
        SystemInfo->PresM[SystemInfo->n+i] = -1;
        first = 1;
        for (j = 0; j < jntp->JointDOF; j++)
            if ((jntp->PresFlg[j] & ISQUESFLG) || IS_ONE(jntp->Pres[j])) {
                SystemInfo->np++;
                jntp->JointPres++;
                if (first) {
                    SystemInfo->PresM[SystemInfo->n+i] = NextM;
                    first = 0;
                }
                NextM++;
            }
    }

    NextDOF = 0;
    NextBallQ = SystemInfo->sl;
    for (i = 0; i < SystemInfo->nl; i++) {
        register int ndof;  /* # of DOF for this loop joint */
        register int nmults;  /* # of mults slots for this loop joint */

        jntp = &SystemInfo->LoopConst[i].jnt;

        SystemInfo->FirstDOF[SystemInfo->n + i] = NextDOF;
        ndof = jntp->JointDOF;
        NextDOF += ndof;
        SystemInfo->LastDOF[SystemInfo->n + i] = NextDOF - 1;

        nmults = 6 - ndof;
        SystemInfo->FirstM[i] = (nmults > 0 ? NextM : cUninitializedIndex);
        NextM += nmults;
        SystemInfo->nlc += nmults;

        if (JointInfo[(int)jntp->JointKind].hasball) {
            SystemInfo->nlb++;
            SystemInfo->BallQ[SystemInfo->n + i] = NextBallQ++;
        } else 
            SystemInfo->BallQ[SystemInfo->n + i] = cUninitializedIndex;
    }

    /* Assign explicit and user constraint multipliers. */

    for (i = 0; i < SystemInfo->nxc+SystemInfo->nu; i++) {
        ConstraintP = &SystemInfo->Const[i];
        ConstraintP->Mindex = NextM++;
    }

    /* total number of constraints requiring multipliers */
    SystemInfo->nc  = SystemInfo->np+SystemInfo->nlc
                        + SystemInfo->nxc+SystemInfo->nu; 
                                                   
    SystemInfo->nq  = SystemInfo->s+SystemInfo->nb;    /* size of q array */
    SystemInfo->nlq = SystemInfo->sl+SystemInfo->nlb;  /* size of lq array */
    SystemInfo->nj  = SystemInfo->n+SystemInfo->nl;    /* number of joints */
    SystemInfo->nh  = SystemInfo->s+SystemInfo->sl;    /* number of hinges */

    /* now declare the symbols that are carried in SystemInfo */

    declare_sys_types((FILE *)NULL, DECL_NOPRINT, SystemInfo);
    declare_input_parms((FILE *)NULL, DECL_NOPRINT, SystemInfo);

    /* Process question marks, create pseudobodies, and fill in all
       symbols with their appropriate values or references. */
    PROCESS_INPUTS(SystemInfo);

    return 0;        /* success */

    /* If we branch here, we encountered an error.  Make sure we dispose
     * of allocated space in SystemInfo before returning.
     */
  error_return:
    FREE_SYSTEM(SystemInfo);
    return 1;   /* failure */
}

/*
 * Free up space being used by the SystemInfo struct.  We do not
 * free the struct itself (which might not be on the heap at all)
 * but we free those (large) pieces of it which are dynamically
 * allocated in READ_SYSTEM and PROCESS_INPUTS above.
 */
void FREE_SYSTEM(SystemInfo_t *SystemInfo)
{
    if (SystemInfo->Bodies)
        free((char *)SystemInfo->Bodies);

    if (SystemInfo->PseudoBodies) {
        int                i;
        BodyDesc_t        *b, *w;

        for (i=0; i < cMaxNumDOF; i++) {
            b = SystemInfo->PseudoBodies[i].weldlist;
            while (b) {
                w = b->weldlist;
                free((char *)b);
                b = w;
            }
        }
        
        free((char *)SystemInfo->PseudoBodies);
    }

    if (SystemInfo->LoopConst)
        free((char *)SystemInfo->LoopConst);
    if (SystemInfo->Const)
        free((char *)SystemInfo->Const);
}

/*
 * Declare all the types we need to define the system and sddyn()
 * parameters.  We do no output here, just load up the types with
 * their definitions.
 * Declare the types used to describe the system parameters.  Use
 * decl_flags|DECL_NOPRINT to suppress output of the types.
 * If a type has a dimension which is 0, we declare it anyway using a
 * dimension of 1.  This way you don't have to check all the time before
 * declaring variables -- a legitimate declaration can be produced, although
 * the variable will never be used.
 */
void declare_sys_types(FILE *F,
                  int  decl_flags,
                  register SystemInfo_t *sys)
{
    int s  = MAX(sys->s, 1),  nl = MAX(sys->nl, 1), nlq = MAX(sys->nlq, 1),
        sl = MAX(sys->sl, 1), nh = MAX(sys->nh, 1), nq  = MAX(sys->nq, 1),
        nc = MAX(sys->nc, 1), nxc = MAX(sys->nxc, 1), 
        n = sys->n, nj = sys->nj;

    /* There is always at least one real body and one joint, so n and nj
     * are always >= 1.
     */
        
    declare_type(F, decl_flags,
      &sys->type_Int,      VT_INTEGER,          "tInt",
      &sys->type_Vec,      VT_VECTOR,           "tVec",
      &sys->type_Mat,      VT_MATRIX,           "tMat",
      &sys->type_Arr_n,    VT_ARRAY,            "tArr_n",    n,         0,
      &sys->type_Vec_n,    VT_ARRAY|VT_VECTOR,  "tVec_n",    n,         0,
      &sys->type_Mat_n,    VT_ARRAY|VT_MATRIX,  "tMat_n",    n,         0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_IntArr_s, VT_INTEGER|VT_ARRAY, "tIntArr_s", s,         0,
      &sys->type_Arr_s,    VT_ARRAY,            "tArr_s",    s,         0,
      &sys->type_Vec_s,    VT_ARRAY|VT_VECTOR,  "tVec_s",    s,         0,
      &sys->type_Mat_s,    VT_ARRAY|VT_MATRIX,  "tMat_s",    s,         0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_Arr_s_s,  VT_ARRAY,            "tArr_s_s",  s,   s, 0,
      &sys->type_Vec_s_s,  VT_ARRAY|VT_VECTOR,  "tVec_s_s",  s,   s, 0,
      &sys->type_Arr_nq,   VT_ARRAY,            "tArr_nq",   nq,     0,
      &sys->type_Arr_nh,   VT_ARRAY,            "tArr_nh",   nh,     0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_Vec_nh,   VT_ARRAY|VT_VECTOR,  "tVec_nh",   nh,  0,
      &sys->type_Arr_nj,   VT_ARRAY,            "tArr_nj",   nj,  0,
      &sys->type_Vec_nj,   VT_ARRAY|VT_VECTOR,  "tVec_nj",   nj,  0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_IntVec,      VT_VECTOR|VT_INTEGER,          "tIntVec",
      &sys->type_IntMat,      VT_MATRIX|VT_INTEGER,          "tIntMat",
      &sys->type_IntArr_n,    VT_ARRAY|VT_INTEGER,           "tIntArr_n", n, 0,
      &sys->type_IntVec_n,    VT_ARRAY|VT_VECTOR|VT_INTEGER, "tIntVec_n", n, 0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_IntMat_n,    VT_ARRAY|VT_MATRIX|VT_INTEGER,  "tIntMat_n", n, 0,
      &sys->type_IntArr_s,    VT_ARRAY|VT_INTEGER,            "tIntArr_s", s, 0,
      &sys->type_IntVec_s,    VT_ARRAY|VT_VECTOR|VT_INTEGER,  "tIntVec_s", s, 0,
      NULL);

    declare_type(F, decl_flags,
      &sys->type_Arr_nl,   VT_ARRAY,            "tArr_nl",  nl,  0,
      &sys->type_Vec_nl,   VT_ARRAY|VT_VECTOR,  "tVec_nl",  nl,  0,
      &sys->type_Mat_nl,   VT_ARRAY|VT_MATRIX,  "tMat_nl",  nl,  0,
      &sys->type_Arr_sl,   VT_ARRAY,            "tArr_sl",  sl,  0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_Arr_nlq,   VT_ARRAY,            "tArr_nlq",   nlq, 0,
      &sys->type_IntArr_nc, VT_ARRAY|VT_INTEGER, "tIntArr_nc",  nc, 0,
      &sys->type_Arr_nc,    VT_ARRAY,            "tArr_nc",     nc, 0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_Arr_nc_nc,VT_ARRAY, "tArr_nc_nc", nc, nc, 0,
      &sys->type_Arr_nc_s, VT_ARRAY, "tArr_nc_s",  nc,  s, 0,
      &sys->type_Arr_s_nc, VT_ARRAY, "tArr_s_nc",   s, nc, 0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_IntVec_nl, VT_ARRAY|VT_VECTOR|VT_INTEGER, "tIntVec_nl", nl, 0,
      &sys->type_IntArr_sl, VT_ARRAY|VT_INTEGER,           "tIntArr_nl", sl, 0,
      NULL);
    declare_type(F, decl_flags,
      &sys->type_Arr_nxc, VT_ARRAY,           "tArr_nxc",  nxc,  0,
      &sys->type_Vec_nxc, VT_ARRAY|VT_VECTOR, "tVec_nxc",  nxc,  0,
      NULL);
}

/*
 * Declare the symbols used to describe the system parameters, that is,
 * anything which can be set in the input file.
 * Use decl_flags|DECL_NODSYM to suppress declaration of internal symbols.
 * The internal symbols should be declared only once!
 */
void declare_input_parms(FILE *F,
                         int  decl_flags,
                         register SystemInfo_t *sys)
{
    int i;

    declare_vars(F, decl_flags,
      VT_USER|VT_DSYM, &sys->type_Vec,     "grav", &sys->grav,
      VT_USER|VT_DSYM, &sys->type_Arr_n,   "mk",   &sys->mk,
      VT_USER|VT_DSYM, &sys->type_Mat_n,   "ik",   &sys->ik,
      VT_USER|VT_DSYM, &sys->type_Vec_s,   "pin",  &sys->pin,
      0);
    declare_vars(F, decl_flags,
      VT_USER|VT_DSYM, &sys->type_Vec_n,   "rk",      &sys->rk,
      VT_DUP|VT_DSYM,                      "ri",      &sys->ri,
      VT_USER|VT_DSYM, &sys->type_Arr_s,   "pres",    &sys->pres,
      VT_REAL|VT_DSYM,                     "stabvel", &sys->stabvel,
      VT_DUP|VT_DSYM,                      "stabpos", &sys->stabpos,
      0);

    if (sys->nl > 0) {
        declare_vars(F, decl_flags,
          VT_USER|VT_DSYM, &sys->type_Vec_nl,  "inbpin1", &sys->inbpin1,
          VT_DUP|VT_DSYM,                      "inbpin2", &sys->inbpin2,
          VT_DUP|VT_DSYM,                      "inbpin3", &sys->inbpin3,
          VT_DUP|VT_DSYM,                      "inbref",  &sys->inbref,
          0);
        declare_vars(F, decl_flags,
          VT_USER|VT_DSYM, &sys->type_Vec_nl,  "bodypin", &sys->bodypin,
          VT_DUP|VT_DSYM,                      "bodyref", &sys->bodyref,
          VT_DUP|VT_DSYM,                      "lbtj",    &sys->lbtj,
          VT_DUP|VT_DSYM,                      "litj",    &sys->litj,
          VT_USER|VT_DSYM, &sys->type_Arr_sl,  "lpres",   &sys->lpres,
          0);
    }

    if (sys->nxc > 0) {
        for (i=0; i<MAXCONSPTS; i++) 
            declare_vars(F, decl_flags|DECL_NUMSUFFIX, i,
                VT_USER|VT_DSYM, &sys->type_Vec_nxc,  "conspt", &sys->conspt[i],
                0);
        for (i=0; i<MAXCONSVECS; i++) 
            declare_vars(F, decl_flags|DECL_NUMSUFFIX, i,
                VT_USER|VT_DSYM, &sys->type_Vec_nxc, "consvec",&sys->consvec[i],
                0);
        for (i=0; i<MAXCONSSCS; i++) 
            declare_vars(F, decl_flags|DECL_NUMSUFFIX, i,
                VT_USER|VT_DSYM, &sys->type_Arr_nxc,  "conssc", &sys->conssc[i],
                0);
    }
}
