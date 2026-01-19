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

/* FINDCONSTRAINT 
 * Look up name in SystemInfo.Const.  Return index or 
 * cUnspecifiedConstraint if not found.
 */
int
FINDCONSTRAINT(SystemInfo_t *SystemInfo,
               char *Constraint)
{
    string32 UpName;
    string32 UpTable;
    Index_t consno;

    strcpy(UpName, Constraint);
    UPSTR(UpName);
    for (consno = 0; consno < SystemInfo->nc; consno++) {
        strcpy(UpTable, SystemInfo->Const[consno].ConstraintName);
        UPSTR(UpTable);
        if (!strcmp(UpTable, UpName))
            return consno;
    }
    return cUnspecifiedConstraint;
}

/* FINDBODY 
 * Look up name in SystemInfo.Bodies.  Return index or 
 * cUnspecifiedBody if not found.
 */
int
FINDBODY(SystemInfo_t *SystemInfo,
         char *Body)
{
    /* look up name in SystemInfo.Bodies, return bodyno if found, 
        or cUnspecifiedBody if not found */

    string32 UpName;
    string32 UpTable;
    Index_t bodyno;

    strcpy(UpName, Body);
    UPSTR(UpName);

    if (!strcmp("$GROUND", UpName))
        return cGroundBody;

    for (bodyno = 0; bodyno < SystemInfo->n; bodyno++) {
        strcpy(UpTable, SystemInfo->Bodies[bodyno].BodyName);
        UPSTR(UpTable);
        if (!strcmp(UpTable, UpName))
            return bodyno;
    }
    return cUnspecifiedBody;
}

/* FINDJOINT 
 *
 * Look up name in SystemInfo.Bodies[].jnt and SystemInfo.LoopConst[].jnt.
 * Return jointno if found, or cUnspecifiedJoint if not found.  Joint no.
 * is body number if found in Bodies[], otherwise it is max body number
 * plus the index into LoopConst[].
 */
int
FINDJOINT(SystemInfo_t *SystemInfo,
          char *Jname)
{

    string32 UpName;
    string32 UpTable;
    Index_t bodyno, loopno;

    strcpy(UpName, Jname);
    UPSTR(UpName);

    for (bodyno = 0; bodyno < SystemInfo->n; bodyno++) {
        strcpy(UpTable, SystemInfo->Bodies[bodyno].jnt.JointName);
        UPSTR(UpTable);
        if (!strcmp(UpTable, UpName))
            return bodyno;
    }

    for (loopno = 0; loopno < SystemInfo->nl; loopno++) {
        strcpy(UpTable, SystemInfo->LoopConst[loopno].jnt.JointName);
        UPSTR(UpTable);
        if (!strcmp(UpTable, UpName))
            return SystemInfo->n+loopno;
    }

    return cUnspecifiedJoint;
}
