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

#include "libs.h"
#include "libsprot.h"

/* These declarations are for the fussy SGI compiler which
 * can't find these static routines otherwise.
 */
static void INIT_PSEUDO_BODY(register BodyDesc_t *UserBody,
                             int whichaxis,
                             register BodyDesc_t *PseudoBody);
static void PROCESS_QUESTION_MARKS(register SystemInfo_t *SystemInfo);
static void MAKE_PSEUDO_BODIES(register SystemInfo_t *SystemInfo);

/* PROCESS_QUESTION_MARKS 
 * 
 * This routine goes through the input file parameters,
 * replacing QuestionMark expressions with the appropriate symbols and
 * assigning the resulting expressions to the global symbols in
 * SystemInfo which represent these parameters.
 * We also set the nominal-value expressions for each input file
 * parameter.  
 *
 * Note: for parameters which must be processed before use, such as
 * pin axes (which must be normalized), we handle the nominal values
 * here but leave the assignment of the global symbol to be done during
 * generation of SDINIT.
 */
static void
PROCESS_QUESTION_MARKS(register SystemInfo_t *SystemInfo)
{
    register Index_t b, i, j;
    expr temp_x,temp_nom,mk_x,ik_x,rk_x,ri_x,pres_x;
    expr lbtj_x,litj_x,lpres_x;
    expr conspt_x[MAXCONSPTS], consvec_x[MAXCONSVECS], conssc_x[MAXCONSSCS];

    /* gravity */
    SystemInfo->grav_nom = PERM(NEW_VECX(cScalarVal));
    if (SystemInfo->GravExpr)
        for (i = 0; i < 3; i++) {
            SINDX(SystemInfo->grav_nom, i, INDX(SystemInfo->GravExpr, i));
            if (SystemInfo->GravFlg[i] & ISQUESFLG)
                SINDX(SystemInfo->GravExpr, i, VREF1(SystemInfo->grav, i));
        }
    else  {
        SystemInfo->GravExpr = SystemInfo->grav_nom = VECTOR_ZERO();
        for (i=0; i<3; i++)
            SystemInfo->GravFlg[i] = 0;
    }
    ASSIGN(SystemInfo->grav, SystemInfo->GravExpr);

    /* stabvel */
    if (SystemInfo->StabVelExpr) {
        SystemInfo->stabvel_nom = SystemInfo->StabVelExpr;
        if (SystemInfo->StabVelFlg & ISQUESFLG)
            SystemInfo->StabVelExpr = PERM(VREF(SystemInfo->stabvel));
    } else {
        SystemInfo->stabvel_nom = SCALAR_ZERO();
        SystemInfo->StabVelExpr = PERM(VREF(SystemInfo->stabvel));
        SystemInfo->StabVelFlg = ISQUESFLG|HASNOMFLG;
    }
    ASSIGN(SystemInfo->stabvel, SystemInfo->StabVelExpr);

    /* stabpos */
    if (SystemInfo->StabPosExpr) {
        SystemInfo->stabpos_nom = SystemInfo->StabPosExpr;
        if (SystemInfo->StabPosFlg & ISQUESFLG)
            SystemInfo->StabPosExpr = PERM(VREF(SystemInfo->stabpos));
    } else {
        SystemInfo->stabpos_nom = SCALAR_ZERO();
        SystemInfo->StabPosExpr = PERM(VREF(SystemInfo->stabpos));
        SystemInfo->StabPosFlg = ISQUESFLG|HASNOMFLG;
    }
    ASSIGN(SystemInfo->stabpos, SystemInfo->StabPosExpr);

    /* process bodies and tree joints */

    SystemInfo->mk_nom = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->n));
    mk_x = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->n));
    SystemInfo->ik_nom = PERM(NEW_1dARRAY(cMatrixVal, SystemInfo->n));
    ik_x = PERM(NEW_1dARRAY(cMatrixVal, SystemInfo->n));
    SystemInfo->rk_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->n));
    rk_x = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->n));
    SystemInfo->ri_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->n));
    ri_x = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->n));
    if (SystemInfo->s) {
        SystemInfo->pres_nom = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->s));
        pres_x = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->s));
        SystemInfo->pin_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->s));
    }

    for (b = 0; b < SystemInfo->n; b++) {
        BodyDesc_t *BodyP;
        JointDesc_t *jntp;
        int firstdof = SystemInfo->FirstDOF[b];
        BodyP = &SystemInfo->Bodies[b];
        jntp = &SystemInfo->Bodies[b].jnt;

        /* mass */
        SINDX(SystemInfo->mk_nom, b, BodyP->Mass);
        if (BodyP->MassFlg & ISQUESFLG)
            BodyP->Mass = PERM(VREF1(SystemInfo->mk, b));
        SINDX(mk_x, b, BodyP->Mass);

        /* inertia */
        temp_nom = INUSE(NEW_MATX(cScalarVal));
        temp_x = INUSE(NEW_MATX(cScalarVal));
        for (i = 0; i < 3; i++) 
            for (j = 0; j < 3; j++) {
                SINDX2(temp_nom, i,j, INDX2(BodyP->Inertia, i,j));
                if (BodyP->InerFlg[i][j] & ISQUESFLG)
                    SINDX2(BodyP->Inertia, i, j,
                      INDX2(VREF1(SystemInfo->ik, b), i, j));
                SINDX2(temp_x, i,j, INDX2(BodyP->Inertia, i,j));
            }
        SINDX(SystemInfo->ik_nom, b, UNUSE(temp_nom));
        SINDX(ik_x, b, UNUSE(temp_x));

        /* pres */
        for (i = 0; i < jntp->JointDOF; i++) {
            SINDX(SystemInfo->pres_nom, firstdof+i, jntp->Pres[i]);
            if (jntp->PresFlg[i] & ISQUESFLG)
                jntp->Pres[i] = PERM(VREF1(SystemInfo->pres, firstdof+i));
            SINDX(pres_x, firstdof+i, jntp->Pres[i]);
        }

        /* rk */
        temp_nom = INUSE(NEW_VECX(cScalarVal));
        temp_x = INUSE(NEW_VECX(cScalarVal));
        for (i = 0; i < 3; i++)
            if (jntp->BodyToJoint) {
                SINDX(temp_nom, i, INDX(jntp->BodyToJoint,i));
                if (jntp->BtjFlg[i] & ISQUESFLG)
                    SINDX(jntp->BodyToJoint, i,
                        INDX(VREF1(SystemInfo->rk, b), i));
                SINDX(temp_x, i, INDX(jntp->BodyToJoint,i));
            }
        SINDX(SystemInfo->rk_nom, b, UNUSE(temp_nom));
        SINDX(rk_x, b, UNUSE(temp_x));

        /* ri */
        temp_nom = INUSE(NEW_VECX(cScalarVal));
        temp_x = INUSE(NEW_VECX(cScalarVal));
        for (i = 0; i < 3; i++)
            if (jntp->InbToJoint) {
                SINDX(temp_nom, i, INDX(jntp->InbToJoint,i));
                if (jntp->ItjFlg[i] & ISQUESFLG)
                    SINDX(jntp->InbToJoint, i,
                        INDX(VREF1(SystemInfo->ri, b), i));
                SINDX(temp_x, i, INDX(jntp->InbToJoint,i));
            }
        SINDX(SystemInfo->ri_nom, b, UNUSE(temp_nom));
        SINDX(ri_x, b, UNUSE(temp_x));

        /* pin -- just do nominals here.  The rest is calculated
         * during SDINIT generation.  
         */
        for (i = 0; i < jntp->JointDOF; i++) {
            switch (jntp->JointKind) {
            case cBushingJoint:
            case cRevBushingJoint:
                /* Duplicate the three pins for both the translational
                 * and rotational parts of the joints.
                 */
                if (i < 3) j = i;
                else       j = i-3;
                SINDX(SystemInfo->pin_nom, firstdof+i, 
                    jntp->Pins[j] ? jntp->Pins[j] : VECTOR_ZERO());
                break;
            case cBearingJoint:
                /* The first pin is used for the sliding axis and the
                 * first rotational axis.
                 */
                if (i == 0) j = 0;
                else        j = i-1;
                SINDX(SystemInfo->pin_nom, firstdof+i, 
                    jntp->Pins[j] ? jntp->Pins[j] : VECTOR_ZERO());
                break;
            case cRevBearingJoint:
                /* The last pin is used for the sliding axis as well as
                 * the last rotational axis.
                 */
                if (i == 4) j = 3;
                else        j = i;
                SINDX(SystemInfo->pin_nom, firstdof+i, 
                    jntp->Pins[j] ? jntp->Pins[j] : VECTOR_ZERO());
                break;
            case cRev6dJoint:
                /* Reverse 6dof has its important pins at the end in `pin', 
                 * so we load pins in the order 345012 instead of the normal 
                 * 012345.
                 */
                SINDX(SystemInfo->pin_nom, firstdof+((i+3)%6), 
                    jntp->Pins[i] ? jntp->Pins[i] : VECTOR_ZERO());
                break;

            default:
                SINDX(SystemInfo->pin_nom, firstdof+i, 
                    jntp->Pins[i] ? jntp->Pins[i] : VECTOR_ZERO());
                break;
            }
        }
    }

    ASSIGN(SystemInfo->mk, mk_x);
    ASSIGN(SystemInfo->ik, ik_x);
    ASSIGN(SystemInfo->rk, rk_x);
    ASSIGN(SystemInfo->ri, ri_x);

    if (SystemInfo->s)
        ASSIGN(SystemInfo->pres, pres_x);

    /* if no loop joints, go on to constraints */
    if (SystemInfo->nl == 0)
        goto skipLoops;

    /* now process the loop joints (just nominal values for loop
       pins since they have to be calculated during SDINIT generation) */

    SystemInfo->lbtj_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    lbtj_x = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->litj_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    litj_x = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->inbpin1_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->inbpin2_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->inbpin3_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->inbref_nom  = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->bodypin_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));
    SystemInfo->bodyref_nom = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nl));

    if (SystemInfo->sl) {
        SystemInfo->lpres_nom = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->sl));
        lpres_x = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->sl));
    }

    for (b = 0; b < SystemInfo->nl; b++) {
        JointDesc_t *jntp;
        int firstdof = SystemInfo->FirstDOF[SystemInfo->n + b];

        jntp = &SystemInfo->LoopConst[b].jnt;

        /* lpres */
        for (i = 0; i < jntp->JointDOF; i++) {
            SINDX(SystemInfo->lpres_nom, firstdof+i, jntp->Pres[i]);
            if (jntp->PresFlg[i] & ISQUESFLG)
                jntp->Pres[i] = PERM(VREF1(SystemInfo->lpres, firstdof+i));
            SINDX(lpres_x, firstdof+i, jntp->Pres[i]);
        }

        /* lbtj */
        temp_nom = INUSE(NEW_VECX(cScalarVal));
        temp_x = INUSE(NEW_VECX(cScalarVal));
        for (i = 0; i < 3; i++)
            if (jntp->BodyToJoint) {
                SINDX(temp_nom, i, INDX(jntp->BodyToJoint,i));
                if (jntp->BtjFlg[i] & ISQUESFLG)
                    SINDX(jntp->BodyToJoint, i,
                        INDX(VREF1(SystemInfo->lbtj, b), i));
                SINDX(temp_x, i, INDX(jntp->BodyToJoint,i));
            }
        SINDX(SystemInfo->lbtj_nom, b, UNUSE(temp_nom));
        SINDX(lbtj_x, b, UNUSE(temp_x));

        /* litj */
        temp_nom = INUSE(NEW_VECX(cScalarVal));
        temp_x = INUSE(NEW_VECX(cScalarVal));
        for (i = 0; i < 3; i++)
            if (jntp->InbToJoint) {
                SINDX(temp_nom, i, INDX(jntp->InbToJoint,i));
                if (jntp->ItjFlg[i] & ISQUESFLG)
                    SINDX(jntp->InbToJoint, i,
                        INDX(VREF1(SystemInfo->litj, b), i));
                SINDX(temp_x, i, INDX(jntp->InbToJoint,i));
            }
        SINDX(SystemInfo->litj_nom, b, UNUSE(temp_nom));
        SINDX(litj_x, b, UNUSE(temp_x));

        /* nominal inbpin1 */
        SINDX(SystemInfo->inbpin1_nom, b, 
           jntp->Pins[INBPIN1] ? jntp->Pins[INBPIN1] : VECTOR_ZERO());

        /* nominal inbpin2 */
        SINDX(SystemInfo->inbpin2_nom, b, 
           jntp->Pins[INBPIN2] ? jntp->Pins[INBPIN2] : VECTOR_ZERO());

        /* nominal inbpin3 */
        SINDX(SystemInfo->inbpin3_nom, b, 
           jntp->Pins[INBPIN3] ? jntp->Pins[INBPIN3] : VECTOR_ZERO());

        /* nominal inbref */
        SINDX(SystemInfo->inbref_nom, b, 
           jntp->Pins[INBREF] ? jntp->Pins[INBREF] : VECTOR_ZERO());

        /* nominal bodypin */
        SINDX(SystemInfo->bodypin_nom, b, 
           jntp->Pins[BODYPIN] ? jntp->Pins[BODYPIN] : VECTOR_ZERO());

        /* nominal bodyref */
        SINDX(SystemInfo->bodyref_nom, b, 
           jntp->Pins[BODYREF] ? jntp->Pins[BODYREF] : VECTOR_ZERO());
    }

    ASSIGN(SystemInfo->lbtj, lbtj_x);
    ASSIGN(SystemInfo->litj, litj_x);
    if (SystemInfo->sl)
        ASSIGN(SystemInfo->lpres, lpres_x);

  skipLoops:

    /* we're done if there are no explicit constraints */
    if (SystemInfo->nxc == 0)
        return;

    /* now process the constraint elements */

    for (i=0; i<MAXCONSPTS; i++) {
        SystemInfo->conspt_nom[i] = 
            PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nxc));
        conspt_x[i] = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nxc));
    }
    for (i=0; i<MAXCONSVECS; i++) {
        SystemInfo->consvec_nom[i] = 
            PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nxc));
        consvec_x[i] = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->nxc));
    }
    for (i=0; i<MAXCONSSCS; i++) {
        SystemInfo->conssc_nom[i] = 
            PERM(NEW_1dARRAY(cScalarVal, SystemInfo->nxc));
        conssc_x[i] = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->nxc));
    }

    for (b = 0; b < SystemInfo->nxc; b++) {
        ConstraintDesc_t *consp;
        int npt, nvec, nsc;

        consp = &SystemInfo->Const[b];

        /* conspt */
        npt = ConstraintInfo[(int)consp->ConstraintType].npt;
        for (i=0; i < npt; i++) {
            temp_nom = INUSE(NEW_VECX(cScalarVal));
            temp_x = INUSE(NEW_VECX(cScalarVal));
            for (j = 0; j < 3; j++) {
                SINDX(temp_nom, j, INDX(consp->Points[i].Vec,j));
                if (consp->PointsFlg[i][j] & ISQUESFLG)
                    SINDX(consp->Points[i].Vec, j,
                        INDX(VREF1(SystemInfo->conspt[i], b), j));
                SINDX(temp_x, i, INDX(consp->Points[i].Vec,j));
            }
            SINDX(SystemInfo->conspt_nom[i], b, UNUSE(temp_nom));
            SINDX(conspt_x[i], b, UNUSE(temp_x));
        }
        for (i=npt; i<MAXCONSPTS; i++) {
            SINDX(SystemInfo->conspt_nom[i], b, VECTOR_ZERO());
            SINDX(conspt_x[i], b, VECTOR_ZERO());
        }

        /* consvec */
        nvec = ConstraintInfo[(int)consp->ConstraintType].nvec;
        for (i=0; i < nvec; i++) {
            temp_nom = INUSE(NEW_VECX(cScalarVal));
            temp_x = INUSE(NEW_VECX(cScalarVal));
            for (j = 0; j < 3; j++) {
                SINDX(temp_nom, j, INDX(consp->Vectors[i].Vec,j));
                if (consp->VectorsFlg[i][j] & ISQUESFLG)
                    SINDX(consp->Vectors[i].Vec, j,
                        INDX(VREF1(SystemInfo->consvec[i], b), j));
                SINDX(temp_x, i, INDX(consp->Vectors[i].Vec,j));
            }
            SINDX(SystemInfo->consvec_nom[i], b, UNUSE(temp_nom));
            SINDX(consvec_x[i], b, UNUSE(temp_x));
        }
        for (i=nvec; i<MAXCONSVECS; i++) {
            SINDX(SystemInfo->consvec_nom[i], b, VECTOR_ZERO());
            SINDX(consvec_x[i], b, VECTOR_ZERO());
        }

        /* conssc */
        nsc = ConstraintInfo[(int)consp->ConstraintType].nsc;
        for (i=0; i < nsc; i++) {
            SINDX(SystemInfo->conssc_nom[i], b, consp->Scalars[i]);
            if (consp->ScalarsFlg[i] & ISQUESFLG)
                consp->Scalars[i] = PERM(VREF1(SystemInfo->conssc[i], b));
            SINDX(conssc_x[i], b, consp->Scalars[i]);
        }
        for (i=nsc; i<MAXCONSSCS; i++) {
            SINDX(SystemInfo->conssc_nom[i], b, SCALAR_ZERO());
            SINDX(conssc_x[i], b, SCALAR_ZERO());
        }
    }

    for (i=0; i<MAXCONSPTS; i++)
        ASSIGN(SystemInfo->conspt[i], conspt_x[i]);
    for (i=0; i<MAXCONSVECS; i++)
        ASSIGN(SystemInfo->consvec[i], consvec_x[i]);
    for (i=0; i<MAXCONSSCS; i++)
        ASSIGN(SystemInfo->conssc[i], conssc_x[i]);
}

/* MAKE_PSEUDO_BODIES
 * 
 * Generate the PseudoBodies list from information in the Bodies list.
 * We also generate the GndPseudoBody here.
 */

static void
MAKE_PSEUDO_BODIES(register SystemInfo_t *SystemInfo)
{
    /* Setup pseudo bodies. */
    register Index_t ndof, dof, inb, i, j;
    BodyDesc_t       *weldedbody, *w;
    int              realinb;

    /* Do ground first. */
    INIT_PSEUDO_BODY(NULL, -1, &SystemInfo->GndPseudoBody);

    for (i = 0; i < SystemInfo->n; i++) {
        dof  = SystemInfo->FirstDOF[i];
        ndof = SystemInfo->Bodies[i].jnt.JointDOF;
        realinb = SystemInfo->Bodies[i].jnt.InbBody;
        inb = realinb == cGroundBody ? cGroundBody 
                                     : SystemInfo->LastDOF[realinb];

        if (ndof == 0) {
            /* Tree weld joint.  Link onto the end of the weldlist for the
             * associated pseudobody (which must already exist by now).
             */
            weldedbody = (BodyDesc_t *)malloc(sizeof(BodyDesc_t));
            INIT_PSEUDO_BODY(&SystemInfo->Bodies[i], -1, weldedbody);
            weldedbody->jnt.InbBody = realinb;
            weldedbody->jnt.OutbBody = SystemInfo->Bodies[i].jnt.OutbBody;

            w = (inb >= 0 ? &SystemInfo->PseudoBodies[inb]
                          : &SystemInfo->GndPseudoBody);
            while(w->weldlist)
                 w = w->weldlist;
            w->weldlist = weldedbody;
        } else
            for (j = 0; j < ndof; j++) {
                INIT_PSEUDO_BODY(&SystemInfo->Bodies[i], (int)j,
                    &SystemInfo->PseudoBodies[dof+j]);
                SystemInfo->PseudoBodies[dof+j].jnt.InbBody = 
                    j > 0 ? dof+j - 1 : inb;
                SystemInfo->PseudoBodies[dof+j].jnt.OutbBody = dof+j;
            }
    }

/*XXX this has to go */
#ifdef NOTDEF
    /* Generate body-oriented input file parameters renumbered to
       match pseudobody numbering. */

    /* mass */
    SystemInfo->psMk = PERM(NEW_1dARRAY(cScalarVal, SystemInfo->s));
    for (i = 0; i < SystemInfo->s; i++)
        SINDX(SystemInfo->psMk, i, SystemInfo->PseudoBodies[i].Mass);

    /* inertia */
    SystemInfo->psIk = PERM(NEW_1dARRAY(cMatrixVal, SystemInfo->s));
    for (i = 0; i < SystemInfo->s; i++)
        SINDX(SystemInfo->psIk, i, SystemInfo->PseudoBodies[i].Inertia);

    /* rk and ri */
    SystemInfo->psRk = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->s));
    SystemInfo->psRi = PERM(NEW_1dARRAY(cVectorVal, SystemInfo->s));
    for (i = 0; i < SystemInfo->s; i++) {
        SINDX(SystemInfo->psRk, i, SystemInfo->PseudoBodies[i].jnt.BodyToJoint);
        SINDX(SystemInfo->psRi, i, SystemInfo->PseudoBodies[i].jnt.InbToJoint);
    }
#endif
}

/* INIT_PSEUDO_BODY
 *
 * This is called once for each pin, slider, and ball `pin' associated with
 * the inboard joint for each real system body.  `whichaxis' says which 
 * hinge axis of the real joint is represented by this pseudobody.  It is
 * 0 when we're building the `most inboard' pseudobody for this real body,
 * -1 if the pseudobody corresponds to a "welded" body.
 *
 * InbToJoint is inherited only by the first (inboard) pseudobody.  Mass, 
 * Inertia and BodyToJoint are inherited only by the last (outboard).  A
 * "welded" pseudobody inherits everything.
 *
 * If the passed-in UserBody is NULL, this is the special "ground" 
 * pseudobody which is massless, has zero itj & btj vectors, etc.  (Make
 * sure "whichaxis" is -1 in this case.)
 */
static void INIT_PSEUDO_BODY(register BodyDesc_t *UserBody,
                             int whichaxis,
                             register BodyDesc_t *PseudoBody)
{
    register int i,j;

    strcpy(PseudoBody->BodyName, 
           UserBody ? "pseudo body" : "ground pseudo body");
    PseudoBody->Mass = SCALAR_ZERO();
    PseudoBody->MassFlg = 0;
    PseudoBody->Inertia = MATRIX_ZERO();
    PseudoBody->jnt.BodyToJoint = VECTOR_ZERO();
    PseudoBody->jnt.InbToJoint = VECTOR_ZERO();
    for (i=0; i<3; i++) {
        PseudoBody->jnt.BtjFlg[i] = 0;
        PseudoBody->jnt.ItjFlg[i] = 0;
        for (j=0; j<3; j++)
            PseudoBody->InerFlg[i][j] = 0;
    }
    PseudoBody->jnt.InbBody = cUnspecifiedBody;
    PseudoBody->jnt.OutbBody = cUnspecifiedBody;
    for (i=0; i<6; i++) {
        PseudoBody->jnt.Pins[i] = NULL;
        PseudoBody->jnt.Pres[i] = NULL;
        PseudoBody->jnt.PresFlg[i] = 0;
        for (j=0; j<3; j++)
            PseudoBody->jnt.PinFlg[i][j] = 0;
    }
    PseudoBody->weldlist = NULL;

    if (whichaxis == -1) {
        PseudoBody->jnt.JointDOF  = 0;
        PseudoBody->jnt.JointPres = 0;
        PseudoBody->jnt.JointKind = cWeldJoint;
        PseudoBody->jnt.whichaxis = -1;
    } else {
        PseudoBody->jnt.Pins[0] = UserBody->jnt.Pins[whichaxis];
        for (i=0; i<3; i++)
            PseudoBody->jnt.PinFlg[0][i] = UserBody->jnt.PinFlg[whichaxis][i];
        PseudoBody->jnt.Pres[0] = UserBody->jnt.Pres[whichaxis];
        PseudoBody->jnt.PresFlg[0] = UserBody->jnt.PresFlg[whichaxis];
        PseudoBody->jnt.JointDOF = 1;
        if (IS_ZERO(UserBody->jnt.Pres[whichaxis]))
            PseudoBody->jnt.JointPres = 0;
        else
            PseudoBody->jnt.JointPres = 1;

        switch (JointInfo[(int)UserBody->jnt.JointKind].doftype[whichaxis]) {
            case AX_ROT:
                PseudoBody->jnt.JointKind = cPinJoint;
                break;
            case AX_TRANS:
                PseudoBody->jnt.JointKind = cSlidingJoint;
                break;
            case AX_BALL:
                PseudoBody->jnt.JointKind = cBallJoint;
                /* jnt.whichaxis must be 0,1, or 2 */
                if (UserBody->jnt.JointKind == c6dJoint)
                    PseudoBody->jnt.whichaxis = whichaxis-3;
                else 
                    PseudoBody->jnt.whichaxis = whichaxis;
                break;
        }
    }

    if (UserBody == NULL) {
        /* ground */
        PseudoBody->realbody = 1;
        return;
    }

    /* First pseudo body inherits inboard to joint */
    if (whichaxis <= 0) {
        PseudoBody->jnt.InbToJoint = UserBody->jnt.InbToJoint;
        for (i=0; i<3; i++)
            PseudoBody->jnt.ItjFlg[i] = UserBody->jnt.ItjFlg[i];
    }

    /* Last pseudo body inherits everything else */
    if (whichaxis == UserBody->jnt.JointDOF - 1) {
        /* last ("real") pseudo body */
        strcpy(PseudoBody->BodyName, UserBody->BodyName);
        PseudoBody->Mass    = UserBody->Mass;
        PseudoBody->MassFlg = UserBody->MassFlg;
        PseudoBody->Inertia = UserBody->Inertia;
        PseudoBody->jnt.BodyToJoint = UserBody->jnt.BodyToJoint;
        for (i=0; i<3; i++) {
            PseudoBody->jnt.BtjFlg[i] = UserBody->jnt.BtjFlg[i];
            for (j=0; j<3; j++)
                PseudoBody->InerFlg[i][j] = UserBody->InerFlg[i][j];
        }
        PseudoBody->realbody = 1;
    } else 
        PseudoBody->realbody = 0;
}

/*================*/
/* PROCESS_INPUTS */
/*================*/

void PROCESS_INPUTS(SystemInfo_t *SystemInfo)
{
    PROCESS_QUESTION_MARKS(SystemInfo); 
    MAKE_PSEUDO_BODIES(SystemInfo);
}
