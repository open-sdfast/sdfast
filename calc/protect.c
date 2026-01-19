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

#include "calc.h"
#include "calcprot.h"

static tProtectionLevel Level;
static void DO_PROTECT();
static void DO_UNPROTECT();

/* Profiling shows a significant amount of time spent in these routines.
   Due to their heavily recursive nature, thay have been optimized for
   minimum startup code and stack operations [TRE] */

/*=========*/
/* PROTECT */
/*=========*/

void PROTECT(pExpr E,
        tProtectionLevel Lev)
{
    if (!Lev) return;        /* nothing changes */
    Level = Lev;
    DO_PROTECT(E);
}

static void DO_PROTECT(register pExpr E)
{
    /* This raises the protection level of E and all nodes below */
    /* it in the expression tree by the amount in Level.  We     */
    /* complain if E is a free node.  If E is permanent we don't */
    /* do anything.  If E is nil we don't do anything either.    */
    /* Death occurs if a protection level would exceed the max.  */

    if (!E || E->Protection == cPermanent) return;
    if (Level == cPermanent)
        E->Protection = cPermanent;
    else {
        if ((E->Protection += Level) > cMaxProtection) {
            fprintf(stderr, "PROTECT: %d+%d exceeds max of %d\n",
              E->Protection - Level, Level, cMaxProtection);
            abort();
        }
    }

    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            DO_PROTECT(E->Opnd);
            break;
        case cBinaryOperatorNode:
            DO_PROTECT(E->LeftOpnd);
            DO_PROTECT(E->RtOpnd);
            break;
        case cTernaryOperatorNode:
            DO_PROTECT(E->FirstOpnd);
            DO_PROTECT(E->SecondOpnd);
            DO_PROTECT(E->ThirdOpnd);
            break;
        case cScalarConstNode:
            /* nothing */
            break;
        case cArray1dNode: {
            pExpr *ex = E->Array1dValue, *ex_end;

            ex_end = ex + E->NodeValueType.Dim1;
            while (ex < ex_end)
                DO_PROTECT(*ex++);
            break;
        }
        case cArray2dNode: {
            /* treat as 1d array for efficiency */
            pExpr *ex = E->Array1dValue, *ex_end;

            ex_end = ex + E->NodeValueType.Dim1 * E->NodeValueType.Dim2;
            while (ex < ex_end)
                DO_PROTECT(*ex++);
            break;
        }
        case cVarRefNode:
            /* nothing */
            break;
        case cFunctionCallNode:
            DO_PROTECT(E->FuncCallParm);
            break;
        case cFunction2CallNode:
            DO_PROTECT(E->Func2CallParm1);
            DO_PROTECT(E->Func2CallParm2);
            break;
    }
}

/*===========*/
/* UNPROTECT */
/*===========*/

void UNPROTECT(pExpr E,
          tProtectionLevel Lev)
{
    C_ASSERT(Lev != cPermanent, 1, "UNPROTECT");
    if (!Lev) return;
    Level = Lev;
    DO_UNPROTECT(E);
}

static void DO_UNPROTECT(register pExpr E)
{
    /* This trudges down E and its subtree, reducing the protection */
    /* level of each node by Level until hitting a leaf or a node   */
    /* whose protection is cPermanent.  We die if we hit any        */
    /* level 0 (temporary) nodes.                                   */
    /* If E is nil we just return quietly.                          */

    if (!E || E->Protection == cPermanent) return;
    if (E->Protection == cTemporary) {
        fputs("UNPROTECT: tried to unprotect a temp node:", stderr);
        SHOW_TYPE(E);
        abort();
    }
    if ((E->Protection -= Level) < 0) {
        fprintf(stderr, "UNPROTECT: level %d > protection %d\n", Level,
          E->Protection + Level);
        abort();
    }
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            DO_UNPROTECT(E->Opnd);
            break;
        case cBinaryOperatorNode:
            DO_UNPROTECT(E->LeftOpnd);
            DO_UNPROTECT(E->RtOpnd);
            break;
        case cTernaryOperatorNode:
            DO_UNPROTECT(E->FirstOpnd);
            DO_UNPROTECT(E->SecondOpnd);
            DO_UNPROTECT(E->ThirdOpnd);
            break;
        case cScalarConstNode: /* nothing */ ;
            break;
        case cArray1dNode: {
            pExpr *ex = E->Array1dValue, *ex_end;

            ex_end = ex + E->NodeValueType.Dim1;
            while (ex < ex_end)
                DO_UNPROTECT(*ex++);
            break;
        }
        case cArray2dNode: {
            /* treat as 1d array for efficiency */
            pExpr *ex = E->Array1dValue, *ex_end;

            ex_end = ex + E->NodeValueType.Dim1 * E->NodeValueType.Dim2;
            while (ex < ex_end)
                DO_UNPROTECT(*ex++);
            break;
        }
        case cVarRefNode: /* nothing */ ;
            break;
        case cFunctionCallNode:
            DO_UNPROTECT(E->FuncCallParm);
            break;
        case cFunction2CallNode:
            DO_UNPROTECT(E->Func2CallParm1);
            DO_UNPROTECT(E->Func2CallParm2);
            break;
    }
}

/*======*/
/* PERM */
/*======*/

pExpr PERM(pExpr E)
{
    /* Returns E, but with protection of the entire subtree */
    /* beneath E raised to cPermanent.                      */

    PROTECT(E, cPermanent);
    return E;
}

/*========*/
/* INUSE  */
/*========*/

/* FORWARD !! */
pExpr INUSE(pExpr E)
{
    /* Returns E, but with protection of E's subtree raised */
    /* by one level.  Any nodes under E which were            */
    /* already cPermanent will stay that way.               */

    PROTECT(E, 1);
    return E;
}

/*=======*/
/* UNUSE */
/*=======*/

/* FORWARD !! */
pExpr UNUSE(pExpr E)
{
    /* Reduces the protection level of every node in E's tree */
    /* by one.  Permanent nodes aren't affected.  If E has    */
    /* any level 0 (temp) nodes, you are in big trouble.      */

    UNPROTECT(E, 1);
    return E;
}
