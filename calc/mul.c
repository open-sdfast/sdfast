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

static pExpr DUMBMUL(pExpr E1,
                     pExpr E2)
{
    pExpr E;
    pExpr S;        /* Holds E1 if it is the scalar, else E2. */
    pExpr O;        /* The "other" expr after scalar is found. */
    register tIndex I;
    char NegFlag;

    C_ASSERT(E1 && E2, 1, "MUL");
    if (IS_SCALAR(E1)) {
        S = E1;
        O = E2;
    } else if (IS_SCALAR(E2)) {
        S = E2;
        O = E1;
    } else        /* neither expression is a scalar */
        fatal("MUL: at least one expr must be scalar.");

    /* First we'll take care of negatives:                   */
    /*   mul(neg(x),y)=mul(x,neg(y))=neg(mul(x,y))   and     */
    /*   mul(neg(x),neg(y))=mul(x,y)                         */
    /* Note that the first transformations make the second   */
    /* one more likely.                                      */

    if (IS_NEG(E1)) {        /* E1 is neg(x) */
        if (IS_NEG(E2)) {        /* so is E2 */
            E1 = INUSE(E1);
            E2 = INUSE(E2);
            E = INUSE(MUL(NEG(E1), NEG(E2)));
        } else {        /* only E1 is neg */
            E1 = INUSE(E1);
            E2 = INUSE(E2);
            E = INUSE(NEG(MUL(NEG(E1), E2)));
        }
        DISPOSE_EXPR(UNUSE(E1));
        DISPOSE_EXPR(UNUSE(E2));
        return UNUSE(E);
    } else if (IS_NEG(E2)) {        /* only E2 is neg */
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(NEG(MUL(E1, NEG(E2))));
        DISPOSE_EXPR(UNUSE(E1));
        DISPOSE_EXPR(UNUSE(E2));
        return UNUSE(E);
    }

    /* Next we'll look for the special cases when S=0,1 or -1. */
    else if (IS_ONE(S)) {
        O = INUSE(O);
        DISPOSE_EXPR(S);
        return UNUSE(O);
    } else if (IS_MINUSONE(S)) {
        S = INUSE(S);
        O = INUSE(O);
        E = INUSE(NEG(O));
        DISPOSE_EXPR(UNUSE(S));
        DISPOSE_EXPR(UNUSE(O));
        return UNUSE(E);
    } else if (IS_ZERO(S)) {
        if (IS_SCALAR(O)) {
            S = INUSE(S);        /* S and O might be the same -- careful */
            DISPOSE_EXPR(O);
            DISPOSE_EXPR(UNUSE(S));
            return gScalarZero;
        } else {
            E = MAKE_ZERO_LIKE(O);
            S = INUSE(S);
            DISPOSE_EXPR(O);        /* can't hurt S here */
            DISPOSE_EXPR(UNUSE(S));
            return E;
        }
    }

        /* There are some special cases if O is a scalar also. */

    else if (IS_SCALAR(O)) {
        if (IS_ONE(O)) {
            S = INUSE(S);
            DISPOSE_EXPR(O);
            return UNUSE(S);
        } else if (IS_MINUSONE(O)) {
            S = INUSE(S);
            O = INUSE(O);
            E = INUSE(NEG(S));
            DISPOSE_EXPR(UNUSE(S));
            DISPOSE_EXPR(UNUSE(O));
            return UNUSE(E);
        } else if (IS_ZERO(O)) {
            O = INUSE(O);
            DISPOSE_EXPR(S);
            DISPOSE_EXPR(UNUSE(O));
            return gScalarZero;
        } else if (IS_CONST(S) && IS_CONST(O)) { /* we'll do a real multiply! */
            switch (E1->NodeKind) {
                case cScalarConstNode:
                    E = SC(E1->ScalarValue * E2->ScalarValue);
                    E2 = INUSE(E2);
                    DISPOSE_EXPR(E1);
                    DISPOSE_EXPR(UNUSE(E2));
                    return E;
            }
            return NULL; /*NOTREACHED*/
        } else {        /* can't actually multiply. */
            if (ORDER(E1, E2) == cAfterOrder) {
                E = E1;        /* swap the two expressions */
                E1 = E2;
                E2 = E;
            }
            /* now if E1 is a negative constant, pull out the neg */
            NegFlag = 0;
            if (IS_CONST(E1) && E1->ScalarValue < 0) {
                    S = NEG(E1);
                    NegFlag = 1;
            } else
                S = E1;
            E = NEWX(cBinaryOperatorNode, 0, 0);
            SCALAR_TYPE(&E->NodeValueType);
            E->BinOp = cMultiply;
            E->LeftOpnd = S;
            E->RtOpnd = E2;
            return NegFlag ? NEG(E) : E;
        }
    }

        /* At this point we know that O is not a scalar, so we're */
        /* going to slice it along one dimension and recursively  */
        /* multiply each slice by S, which IS a scalar.           */

    else {
        E = INUSE(MAKE_EXPR_LIKE(O));        /* E has same dimensions as O */
        S = INUSE(S);
        O = INUSE(O);
        for (I = 0; I < LEN1d(E); I++)
            SINDX(E, I, MUL(S, INDX(O, I)));
        DISPOSE_EXPR(UNUSE(S));
        DISPOSE_EXPR(UNUSE(O));
        return UNUSE(E);
    }
}

pExpr DO_MUL(pExpr E1, 
             pExpr E2)
{
    /* This is the function which actually does a MUL. */
    register pExpr E, F, G;

    /* If the arguments are products, we'll shuffle things around to try */
    /* to get constants to multiply each other.                          */

    if (ORDER(E1, E2) == cAfterOrder) {
        E = E2;        /* swap the expressions */
        F = E1;
    } else {
        E = E1;
        F = E2;
    }

    if (IS_MUL(E) || IS_MUL(F)) {
        E = INUSE(E);        /* protect the arguments */
        F = INUSE(F);
        if (IS_MUL(E)) {
            if (IS_MUL(F)) {
                if (IS_CONST(E->LeftOpnd)) {
                    if (IS_CONST(F->LeftOpnd)) {
                        G = INUSE(DUMBMUL(DUMBMUL(E->LeftOpnd, F->LeftOpnd),
                          DUMBMUL(E->RtOpnd, F->RtOpnd)));
                    } else
                        G = INUSE(DUMBMUL(E->LeftOpnd, DUMBMUL(E->RtOpnd, F)));
                } else {
                    if (IS_CONST(F->LeftOpnd))
                        G = INUSE(DUMBMUL(F->LeftOpnd, DUMBMUL(E, F->RtOpnd)));
                    else
                        G = INUSE(DUMBMUL(E, F));
                }
            } else {        /*E is mul, not F*/
                if (IS_CONST(E->LeftOpnd)) {
                    if (IS_CONST(F))
                        G = INUSE(DUMBMUL(DUMBMUL(E->LeftOpnd, F), E->RtOpnd));
                    else
                        G = INUSE(DUMBMUL(E->LeftOpnd, DUMBMUL(E->RtOpnd, F)));
                } else
                    G = INUSE(DUMBMUL(E, F));
            }
        } else {        /* IS_MUL(F) but not E */
            if (IS_CONST(F->LeftOpnd)) {
                if (IS_CONST(E))
                    G = INUSE(DUMBMUL(DUMBMUL(E, F->LeftOpnd), F->RtOpnd));
                else
                    G = INUSE(DUMBMUL(F->LeftOpnd, DUMBMUL(E, F->RtOpnd)));
            } else
                G = INUSE(DUMBMUL(E, F));
        }
        DISPOSE_EXPR(UNUSE(E));
        DISPOSE_EXPR(UNUSE(F));
        return UNUSE(G);
    }

    if (IS_DVD(E) || IS_DVD(F)) {
        E = INUSE(E);        /* protect the arguments */
        F = INUSE(F);
        if (IS_DVD(E)) {
            if (SAME_EXPR(E->RtOpnd, F))
                G = INUSE(E->LeftOpnd);
            else if (IS_DVD(F))
                G = INUSE(DVD(MUL(E->LeftOpnd, F->LeftOpnd),
                  MUL(E->RtOpnd, F->RtOpnd)));
            else
                G = INUSE(DUMBMUL(E, F));
        } else {        /* only F is a division */
            if (SAME_EXPR(E, F->RtOpnd))
                G = INUSE(F->LeftOpnd);
            else
                G = INUSE(DUMBMUL(E, F));
        }
        DISPOSE_EXPR(UNUSE(E));
        DISPOSE_EXPR(UNUSE(F));
        return UNUSE(G);
    }

    return DUMBMUL(E, F);
}        /* DO_MUL */

static char HASREM(pExpr E)
{
    /* is this expr a vref with a remembered val? */

    return IS_VREF(E) && E->VarRef->RememberedVal;
}

/*=====*/
/* MUL */
/*=====*/

pExpr MUL(pExpr E1, 
          pExpr E2)
{
    /* Multiply the two expressions, producing a third which is */
    /* their product.  This is SCALAR multiplication.  That     */
    /* means that at least one of E1,E2 must be a scalar.  To   */
    /* multiply two non-scalars, use DOT, CROSS or MATMUL  as   */
    /* appropriate.                                             */
    register pExpr E, F;
    pExpr best;

    /* If this is A*(C*D), and some of A,C, and D are VREFs with       */
    /* remembered values, we'll try shuffling the terms around to see  */
    /* if we can get a cheaper expression.                             */
    /* We try (A*C)*D and (A*D)*C as well as A*(C*D).                  */

    /* Reorder first to change (C*D)*A to A*(C*D) for convenience.     */
    /*****
  if (ORDER(E1, E2) == cAfterOrder) {
    E = E2;
    F = E1;
  } else {
    E = E1;
    F = E2;
  }
*****/
    E = E1;
    F = E2;

    /* *** JUST TRY (A*C)*D - costs too much to do all three */
    /*****
  if IS_MUL(F) then begin
    if HASREM(E) then
      DoTrials := true
    else begin
      if HASREM(F^.LeftOpnd) then
        DoTrials := true
      else if HASREM(F^.RtOpnd) then
        DoTrials := true;
      end;
    end;
*****/

    if (!IS_MUL(F) || !HASREM(E) && !HASREM(F->LeftOpnd))
        return APPLYBIN_OP(cMultiply, E, F);

    F = INUSE(F);
    best = DO_MUL(APPLYBIN_OP(cMultiply, E, F->LeftOpnd), F->RtOpnd);
    best = INUSE(best);
    DISPOSE_EXPR(UNUSE(F));
    return UNUSE(best);
}
