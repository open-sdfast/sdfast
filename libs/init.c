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

/*
 * Initialize a joint descriptor.
 */
static void init_joint(JointDesc_t *jntp,
                       Index_t BodyNum);

static void 
init_joint(JointDesc_t *jntp,
           Index_t BodyNum)
{
    register Index_t i,j;

    jntp->JointName[0] = '\0';

    jntp->InbBody = cUnspecifiedBody;
    jntp->OutbBody = BodyNum;
    jntp->JointKind = cUnknownJoint;
    for (i = 0; i < 6; i++) {
        jntp->Pins[i]  = NULL;
        jntp->Pres[i]  = NULL;
        jntp->PresFlg[i] = 0;
        for (j = 0; j < 3; j++)
            jntp->PinFlg[i][j] = 0;
    }
    for (i = 0; i < 3; i++) {
        jntp->BtjFlg[i] = 0;
        jntp->ItjFlg[i] = 0;
    }
    jntp->BodyToJoint = NULL;
    jntp->InbToJoint = NULL;
    jntp->JointDOF = 0;
    jntp->JointPres = 0;
    jntp->whichaxis = 0;
}

/* INIT_BODY 
 *
 * Initialize SystemInfo.Bodies[BodyNum].
 */
void INIT_BODY(SystemInfo_t *SystemInfo,
          Index_t BodyNum,
          char *Name)
{
    register BodyDesc_t *BodyP = &SystemInfo->Bodies[BodyNum];
    register int i,j;

    strcpy(BodyP->BodyName, Name);
    BodyP->Mass = NULL;
    BodyP->MassFlg = 0;
    BodyP->Inertia = NULL;
    for (i=0; i<3; i++)
        for (j=0; j<3; j++)
            BodyP->InerFlg[i][j] = 0;
    BodyP->realbody = 1;
    init_joint(&BodyP->jnt, BodyNum);
    BodyP->weldlist = NULL;
}

/* INIT_LOOP
 *
 * Initialize SystemInfo.LoopConst[LoopNum].
 */
void INIT_LOOP(SystemInfo_t *SystemInfo,
          Index_t LoopNum,
          Index_t BodyNum,
          char *Name)
{
    register LoopDesc_t *LoopP = &SystemInfo->LoopConst[LoopNum];

    strcpy(LoopP->OutbBodyName, Name);
    init_joint(&LoopP->jnt, BodyNum);
}


/* INIT_CONSTRAINT
 * 
 * Initialize SystemInfo.Const[ConstraintNum].
 */
void INIT_CONSTRAINT(SystemInfo_t *SystemInfo,
                Index_t ConstraintNum,
                char *Name,
                ConstraintKind_t ConstraintKind)
{
    register ConstraintDesc_t *ConstP =
      &SystemInfo->Const[ConstraintNum];
    int i;

    ConstP->nbod = ConstP->njnt = ConstP->npt = 
        ConstP->nvec = ConstP->nsc = ConstP->nmult = 0;

    ConstP->ConstraintType = ConstraintKind;
    strcpy(ConstP->ConstraintName, Name);

    for (i=0; i<MAXCONSBODS; i++) 
        ConstP->Bodies[i] = cUnspecifiedBody;
    for (i=0; i<MAXCONSJNTS; i++) {
        ConstP->Joints[i] = cUnspecifiedJoint;
        ConstP->Axes[i] = cUnspecifiedJoint;
    }
    for (i=0; i<MAXCONSPTS; i++) {
        ConstP->Points[i].Body = cUnspecifiedBody;
        ConstP->Points[i].Vec  = NULLEXPR;
    }
    for (i=0; i<MAXCONSVECS; i++) {
        ConstP->Vectors[i].Body = cUnspecifiedBody;
        ConstP->Vectors[i].Vec  = NULLEXPR;
    }
    for (i=0; i<MAXCONSSCS; i++)
        ConstP->Scalars[i] = NULLEXPR;

    ConstP->Mindex = cUninitializedIndex;
}
