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

/*===============*/
/* PRINT_ROADMAP */
/*===============*/

#define MAX1LINENM 15 /* longest name for which we can fit all info on
                         one line */

void PRINT_ROADMAP(FILE *F)
{
    /* Print out nice table showing system structure and body number/ */
    /* coordinate number assignments.                                 */
    /* It is assumed the output is currently in comment mode, and it
       will remain that way on return */

    int mxlen, actlen;
    register Index_t i, j, indx, need;
    char *pres, *bname;
    JointDesc_t *jntp;

    /* Find longest body name -- but at least 9 */
    mxlen = 9;
    for (i = 0; i < SysI.n; i++)
        if ((j = strlen(SysI.Bodies[i].BodyName)) > mxlen && j <= MAX1LINENM)
            mxlen = j;

    efprintf(F, "%:P*s\nROADMAP (%s)\n\n\
Bodies  %P Inb\n\
No  Name%P body Joint type  Coords q",
      mxlen - 4, "", sdfast_opt.infile);
    if (SysI.nc)
        efprintf(F, "         Multipliers");
    efprintf(F, "\n--- ");
    for (i = 0; i < mxlen; i++)
        efprintf(F, "-");
    efprintf(F, " ---- ----------- ----------------");
    if (SysI.nc)
        efprintf(F, " -----------------------");

    efprintf(F, "\n%@3d %@-*s", cGroundBody, mxlen+34, "$ground");
    if (SysI.nc)
        efprintf(F, "|");
    efprintf(F, "\n");
    for (i = 0; i < SysI.n; i++) {
        bname = SysI.Bodies[i].BodyName;
        jntp  = &SysI.Bodies[i].jnt;

        /* need this many spaces in the `q' section */
        need = 16;

        efprintf(F, "%@3d %@-*s ", i, mxlen, bname);
        if (strlen(bname) > MAX1LINENM)
            efprintf(F, "%@*s\n%@-*s ", 
                34+mxlen-strlen(bname), SysI.nc?"|":"", mxlen+4, "");

        efprintf(F, "%@3d  %@-12s", jntp->InbBody,
          JointInfo[(int)jntp->JointKind].jtname);
        if (JointInfo[(int)jntp->JointKind].dof == 6) {
            for (j = SysI.FirstDOF[i]; j < SysI.FirstDOF[i]+3; j++) {
                pres = IS_ZERO(VAL1(SysI.pres,j)) ? " " : 
                       (IS_ONE(VAL1(SysI.pres,j)) ? "p" : "?");
                efprintf(F, "%@3d%s", j, pres);
            }
            if (jntp->JointKind == cRev6dJoint)
                efprintf(F, "%@3d ", SysI.BallQ[i]);
            else if (SysI.nc)
                efprintf(F, "    |");
            efprintf(F, "\n%@-*s      %@-12s", 
                mxlen+4, "", "...");
            for (j = SysI.FirstDOF[i]+3; j <= SysI.LastDOF[i]; j++) {
                pres = IS_ZERO(VAL1(SysI.pres,j)) ? " " : 
                       (IS_ONE(VAL1(SysI.pres,j)) ? "p" : "?");
                efprintf(F, "%@3d%s", j, pres);
                need -= 4;
            }
            if (jntp->JointKind == c6dJoint) {
                efprintf(F, "%@3d ", SysI.BallQ[i]);
                need -= 4;
            }
        } else {
            for (j = SysI.FirstDOF[i]; j <= SysI.LastDOF[i]; j++) {
                pres = IS_ZERO(VAL1(SysI.pres,j)) ? " " : 
                       (IS_ONE(VAL1(SysI.pres,j)) ? "p" : "?");
                efprintf(F, "%@3d%s", j, pres);
                need -= 4;
            }
            if (jntp->JointKind == cBallJoint) {
                efprintf(F, "%@3d ", SysI.BallQ[i]);
                need -= 4;
            }
        }
        for (j = 0; j < need; j++)
            efprintf(F, " ");
        if (SysI.nc)
            efprintf(F, "|");
        for (j = 0; j < jntp->JointPres; j++)
            efprintf(F, "%@3dp", SysI.PresM[i]+j);
        efprintf(F, "\n");
    }

    if (SysI.nl) {
        efprintf(F, "\nLoop Joints %@-*s Pseudo Coords lq\n\n", mxlen+9, "");

        /* First, pseudocoordinates for use in lq, etc.  Then, indexes 
           into the mult array for constraint multipliers. */

        for (i = 0; i < SysI.nl; i++) {
            bname = SysI.LoopConst[i].OutbBodyName;
            jntp  = &SysI.LoopConst[i].jnt;
            need = 16;

            efprintf(F, "%@3d %@-*s ", SysI.n+i, mxlen, bname);
            if (strlen(bname) > MAX1LINENM)
                efprintf(F, "%@*s\n%@-*s ", 
                    34+mxlen-strlen(bname), SysI.nc?"|":"", mxlen+4, "");

            efprintf(F, "%@3d  %-12s", jntp->InbBody,
              JointInfo[(int)jntp->JointKind].jtname);
            if (JointInfo[(int)jntp->JointKind].dof == 6) {
                for (j = 0; j < 3; j++) {
                    indx = SysI.FirstDOF[SysI.n+i] + j;
                    pres = IS_ZERO(VAL1(SysI.lpres,indx)) ? " " : 
                           (IS_ONE(VAL1(SysI.lpres,indx)) ? "p" : "?");
                    efprintf(F, "%@3d%s", indx, pres);
                }
                efprintf(F, "\n%@-*s%-12s", 
                    mxlen+10, "", "...");
                for (j = 3; j < 6; j++) {
                    indx = SysI.FirstDOF[SysI.n+i] + j;
                    pres = IS_ZERO(VAL1(SysI.lpres,indx)) ? " " : 
                           (IS_ONE(VAL1(SysI.lpres,indx)) ? "p" : "?");
                    efprintf(F, "%@3d%s", indx, pres);
                    need -= 4;
                }
                if (jntp->JointKind == c6dJoint) {
                    efprintf(F, "%@3d ", SysI.BallQ[SysI.n+i]);
                    need -= 4;
                }
            } else {
                for (j = 0; j < jntp->JointDOF; j++) {
                    indx = SysI.FirstDOF[SysI.n+i] + j;
                    pres = IS_ZERO(VAL1(SysI.lpres,indx)) ? " " : 
                           (IS_ONE(VAL1(SysI.lpres,indx)) ? "p" : "?");
                    efprintf(F, "%@3d%s", indx, pres);
                    need -= 4;
                }
                if (jntp->JointKind == cBallJoint) {
                    efprintf(F, "%@3d ", SysI.BallQ[SysI.n+i]);
                    need -= 4;
                }
            }
            for (j = 0; j < need; j++)
                efprintf(F, " ");
            if (SysI.nc)
                efprintf(F, "|");
            for (j = 0; j < jntp->JointPres; j++)
                efprintf(F, "%@3dp", SysI.PresM[SysI.n+i]+j);
            for (j = 0; j < 6 - jntp->JointDOF; j++)
                efprintf(F, "%@3d ", SysI.FirstM[i]+j);
            efprintf(F, "\n");
        }
    }

    /* Final slots in mults go to user constraints. */
    if (SysI.nu) {
        efprintf(F, "\nUser Constraints\n\n");
        for (i = 0; i < SysI.nu; i++) {
            /* always room on one line for the name & info */
            efprintf(F, "%@3d %@-*s",i,mxlen,SysI.Const[i].ConstraintName);
            actlen = strlen(SysI.Const[i].ConstraintName);
            if (actlen < mxlen) 
               actlen = mxlen;
            efprintf(F, "%@-*s", 34+mxlen-actlen,"");
            efprintf(F, "|%@3d\n", SysI.Const[i].Mindex);
        }
    }

    efprintf(F, "\n");
}

/* PRINT_SYSTEM_DOC
 * 
 * Print out general documentation about the system being simulated. 
 */
void PRINT_SYSTEM_DOC(FILE *F)
{
efprintf(F,
"\nSYSTEM PARAMETERS (%s)\n\n", sdfast_opt.infile);
efprintf(F,
"Parameter  Value  Description\n\n");

efprintf(F, 
"nbod       %5d  no. bodies (also, no. of tree joints)\n", SysI.n);
efprintf(F, 
"njnt       %5d  total number of joints (tree+loop)\n", SysI.nj);
efprintf(F, 
"ndof       %5d  no. degrees of freedom allowed by tree joints\n", SysI.s);
efprintf(F, 
"nloop      %5d  no. loop joints\n", SysI.nl);
efprintf(F, 
"nldof      %5d  no. degrees of freedom allowed by loop joints\n", SysI.sl);
efprintf(F, "\n");
efprintf(F, 
"nq         %5d  no. position coordinates in state (tree joints)\n", 
         SysI.nq);
efprintf(F, 
"nu         %5d  no. rate coordinates in state (tree joints)\n", SysI.s);
efprintf(F, 
"nlq        %5d  no. position coordinates describing loop joints\n", 
         SysI.nlq);
efprintf(F, 
"nlu        %5d  no. rate coordinates describing loop joints\n", SysI.sl);
efprintf(F, "\n");
efprintf(F, 
"nc         %5d  total no. constraints defined\n", SysI.nc);
efprintf(F, 
"nlc        %5d  no. loop joint constraints\n", SysI.nlc);
efprintf(F, 
"npresc     %5d  no. prescribed motion constraints\n", SysI.np);
efprintf(F, 
"nuserc     %5d  no. user constraints\n", SysI.nu);
efprintf(F, "\n");
}

/* PRINT_JTAXIS_DOC
 * 
 * Print out a table mapping state variables to (joint,axis) pairs.
 */
void PRINT_JTAXIS_DOC(FILE *F)
{
    int trans, quat;
    register Index_t i, j, ix;
    char *pres;
    JointDesc_t *jntp;

    efprintf(F,
        "\nSTATE INDEX TO JOINT/AXIS MAP (%s)\n\n", sdfast_opt.infile);

    efprintf(F, 
      " Index\n");
    efprintf(F, 
      "  q|u   Joint  Axis   Joint type    Axis type    Joint Name\n");
    efprintf(F, 
      " -----  -----  ----   -----------   ----------   ----------\n");

    for (i = 0; i < SysI.n; i++) {
        jntp  = &SysI.Bodies[i].jnt;
        for (j = SysI.FirstDOF[i]; j <= SysI.LastDOF[i]; j++) {
            efprintf(F, "%@3d|%@-3d ", j, j+SysI.nq);
            if (j == SysI.FirstDOF[i])
                efprintf(F, "%@5d  ", i);
            else
                efprintf(F, "%5s  ", ".");
            pres = IS_ZERO(VAL1(SysI.pres,j)) ? " " : 
                   (IS_ONE(VAL1(SysI.pres,j)) ? "p" : "?");
            efprintf(F, "%@4d%s  ", j-SysI.FirstDOF[i], pres);
            efprintf(F, "%@-11s   ", j == SysI.FirstDOF[i] ? 
                     JointInfo[(int)jntp->JointKind].jtname : ".");
            trans = 
              (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[i]]
               == AX_TRANS);
            quat = 
              (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[i]]
               == AX_BALL);
            efprintf(F, "%@-10s", trans ? "translate" 
                                : quat ? "quaternion"
                                : "rotate");
            if (j == SysI.FirstDOF[i])
                efprintf(F, "   %s", jntp->JointName);
            efprintf(F, "\n");
        }
    }
    for (i = 0; i < SysI.n; i++) {
        jntp  = &SysI.Bodies[i].jnt;
        if (SysI.BallQ[i] > 0) {
            efprintf(F, "%@3d|    ", SysI.BallQ[i]);
            efprintf(F, "%@5d  ", i);
            efprintf(F, "%@4d   ", jntp->JointKind == cBallJoint ? 3 : 6);
            efprintf(F, "%@-11s   ", JointInfo[(int)jntp->JointKind].jtname);
            efprintf(F, "%@-10s\n", "4th quat");
            efprintf(F, "   %s", jntp->JointName);
        }
    }

    efprintf(F, "\n");

    if (SysI.sl == 0)
        return;

    efprintf(F, " lq|lu\n\n");

    for (i = 0; i < SysI.nl; i++) {
        ix = SysI.n+i;
        jntp  = &SysI.LoopConst[i].jnt;
        for (j = SysI.FirstDOF[ix]; j <= SysI.LastDOF[ix]; j++) {
            efprintf(F, "%@3d|%@-3d ", j, j+SysI.nlq);
            if (j == SysI.FirstDOF[ix])
                efprintf(F, "%@5d  ", i);
            else
                efprintf(F, "%5s  ", ".");
            pres = IS_ZERO(VAL1(SysI.lpres,j)) ? " " : 
                   (IS_ONE(VAL1(SysI.lpres,j)) ? "p" : "?");
            efprintf(F, "%@4d%s  ", j-SysI.FirstDOF[ix], pres);
            efprintf(F, "%@-11s   ", j == SysI.FirstDOF[ix] ? 
                     JointInfo[(int)jntp->JointKind].jtname : ".");
            trans = 
              (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[ix]]
               == AX_TRANS);
            quat = 
              (JointInfo[(int)jntp->JointKind].doftype[j-SysI.FirstDOF[ix]]
               == AX_BALL);
            efprintf(F, "%@-10s", trans ? "translate" 
                                : quat ? "quaternion"
                                : "rotate");
            if (j == SysI.FirstDOF[ix])
                efprintf(F, "   %s", jntp->JointName);
            efprintf(F, "\n");
        }
    }
    for (i = 0; i < SysI.nl; i++) {
        ix = SysI.n+i;
        jntp  = &SysI.LoopConst[i].jnt;
        if (SysI.BallQ[ix] > 0) {
            efprintf(F, "%@3d|    ", SysI.BallQ[ix]);
            efprintf(F, "%@5d  ", i);
            efprintf(F, "%@4d   ", jntp->JointKind == cBallJoint ? 3 : 6);
            efprintf(F, "%@-11s   ", JointInfo[(int)jntp->JointKind].jtname);
            efprintf(F, "%@-10s", "4th quat");
            efprintf(F, "   %s\n", jntp->JointName);
        }
    }

    efprintf(F, "\n");
}
