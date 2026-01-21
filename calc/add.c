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

static char IS_SINORCOSSQ(pExpr E, 
                          pExpr *Parm, 
                          enum tKnownFunction *Need)
{
    /* Returns true if E is either sin^2(something) or cos^2(something), and */
    /* if so sets Parm to the something and Need to cSine if this is a cos   */
    /* and vice versa.                                                       */
    enum tKnownFunction Func;

    if (!IS_MUL(E) ||
        E->LeftOpnd->NodeKind != cFunctionCallNode ||
        E->RtOpnd->NodeKind != cFunctionCallNode)
        return 0;
    Func = E->LeftOpnd->FuncVarRef->WhichFunction;
    if (Func != cSine && Func != cCosine || 
        E->RtOpnd->FuncVarRef->WhichFunction != Func ||
        !SAME_EXPR(E->LeftOpnd->FuncCallParm, E->RtOpnd->FuncCallParm))
        return 0;
    *Parm = E->LeftOpnd->FuncCallParm;
    *Need = Func == cSine ? cCosine : cSine;
    return 1;
}

static char IS_FUNCSQ(pExpr E, 
                      pExpr Parm, 
                      enum tKnownFunction Func)
{
    /* Returns true if E is Func^2(Parm) */

    return IS_MUL(E) &&
      E->LeftOpnd->NodeKind == cFunctionCallNode &&
      E->RtOpnd->NodeKind == cFunctionCallNode &&
      E->LeftOpnd->FuncVarRef->WhichFunction == Func &&
      E->RtOpnd->FuncVarRef->WhichFunction == Func &&
      SAME_EXPR(E->LeftOpnd->FuncCallParm, Parm) &&
      SAME_EXPR(E->RtOpnd->FuncCallParm, Parm);
}

static char IS_ASINORCOSSQ(pExpr E,
                           pExpr *Parm, 
                           pExpr *a,
                           enum tKnownFunction *Need)
{
    /* Returns true if E is a * sin^2(Parm) or a * cos^2(Parm) for   */
    /* some a and Parm, which are returned.  The function we need    */
    /* to make sin^2 + cos^2 is returned.                            */

    if (IS_SINORCOSSQ(E, Parm, Need)) {
        *a = gScalarOne;
        return 1;
    }
    if (!IS_MUL(E))
        return 0;
    if (IS_SINORCOSSQ(E->RtOpnd, Parm, Need)) {
        *a = E->LeftOpnd;
        return 1;
    }
    return 0;
}

static char IS_BSINORCOSSQ(pExpr E, 
                           pExpr Parm,
                           enum tKnownFunction Func,
                           pExpr *b,
                           pExpr *fsq)
{
    /* Returns true if E is b * Func^2(Parm) for some b.  b and Func^2 */
    /* are returned.                                                   */

    if (IS_FUNCSQ(E, Parm, Func)) {
        *b = gScalarOne;
        *fsq = E;
        return 1;
    }
    if (!IS_MUL(E))
        return 0;
    if (IS_FUNCSQ(E->RtOpnd, Parm, Func)) {
        *b = E->LeftOpnd;
        *fsq = E->RtOpnd;
        return 1;
    }
    return 0;
}

/*=====*/
/* ADD */
/*=====*/

pExpr DO_ADD(register pExpr E1,
             register pExpr E2
)
{
    /* This is the function that actually does an ADD. */
    register pExpr E;
    pExpr Parm, a, b, fsq;
    tIndex I;
    enum tKnownFunction NeedFunc;

    C_ASSERT(E1 && E2, 1, "ADD");
    if (!SAME_TYPE(&E1->NodeValueType, &E2->NodeValueType))
        fatal("ADD: expressions incompatible.");
    if (IS_ZERO(E1)) {
        E2 = INUSE(E2);
        DISPOSE_EXPR(E1);
        return UNUSE(E2);
    } else if (IS_ZERO(E2)) {
        E1 = INUSE(E1);
        DISPOSE_EXPR(E2);
        return UNUSE(E1);
    } else if (IS_CONST(E1) && IS_CONST(E2)) {        /*we'll do a real add!*/
        /* Both E1 & E2 are cScalarConstNode's.  Add the scalars, but
           if we produce a number very near zero call it zero. */
        E = SC(E1->ScalarValue + E2->ScalarValue);
        if (IS_ZERO(E)) {
            DISPOSE_EXPR(E);
            E = gScalarZero;
        }
        DISPOSE_EXPR(E1);
        DISPOSE_EXPR(E2);
        return E;
    } else if (IS_NEG(E1)) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(SUB(E2, NEG(E1)));
    } else if (IS_NEG(E2)) {
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        E = INUSE(SUB(E1, NEG(E2)));
    } else if (E1->NodeValueType.ValueType != cScalarVal) {
        E = INUSE(MAKE_EXPR_LIKE(E1));        /* E has same dimensions as E1 */
        E1 = INUSE(E1);
        E2 = INUSE(E2);
        for (I = 0; I < LEN1d(E); I++)
            SINDX(E, I, ADD(INDX(E1, I), INDX(E2, I)));
    } else {        /* can't add */
        /* First ORDER the expressions so E1 comes before E2 in the ordering */
        if (ORDER(E1, E2) == cAfterOrder) {
            E = E1;        /* swap expressions */
            E1 = E2;
            E2 = E;
        }
        E1 = INUSE(E1);
        E2 = INUSE(E2);

        /* Now try combining constants (if E1&E2 contain constants) */
        if (IS_CONST(E1)) {        /* Note: E2 can't be a constant, due to ORDER */
            if (IS_ADD(E2)) {
                /* because of ORDERing, only E2^.LeftOpnd can possibly be constant */
                if (IS_CONST(E2->LeftOpnd)) {
                    E = INUSE(ADD(ADD(E1, E2->LeftOpnd), E2->RtOpnd));
                    goto L2;
                }
            } else if (IS_SUB(E2)) {
                /* here either term of E2 could be a constant */
                if (IS_CONST(E2->LeftOpnd)) {
                    E = INUSE(SUB(ADD(E1, E2->LeftOpnd), E2->RtOpnd));
                    goto L2;
                } else if (IS_CONST(E2->RtOpnd)) {
                    E = INUSE(ADD(E2->LeftOpnd, SUB(E1, E2->RtOpnd)));
                    goto L2;
                }
            }
        }        /* of IS_CONST(E1) */

        /* E1 wasn't a constant -- maybe it's a constant + expression */
        else if (IS_ADD(E1)) {
            if (IS_CONST(E1->LeftOpnd)) {
                if (IS_ADD(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(ADD(ADD(E1->LeftOpnd, E2->LeftOpnd),
                          ADD(E1->RtOpnd, E2->RtOpnd)));
                        goto L2;
                    }
                } else if (IS_SUB(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(ADD(ADD(E1->LeftOpnd, E2->LeftOpnd),
                          SUB(E1->RtOpnd, E2->RtOpnd)));
                        goto L2;
                    } else if (IS_CONST(E2->RtOpnd)) {
                        E = INUSE(ADD(ADD(E1->RtOpnd, E2->LeftOpnd),
                          SUB(E1->LeftOpnd, E2->RtOpnd)));
                        goto L2;
                    }
                }

                /* E1 is const+expr.  This may help factoring. */
                E = INUSE(ADD(E1->LeftOpnd, ADD(E1->RtOpnd, E2)));
                goto L2;
            }
        }        /* of IS_ADD(E1) */

        /* E1 wasn't constant+expr, perhaps constant-expr or expr-const? */
        else if (IS_SUB(E1)) {
            if (IS_CONST(E1->LeftOpnd)) {
                if (IS_ADD(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(ADD(ADD(E1->LeftOpnd, E2->LeftOpnd),
                          SUB(E2->RtOpnd, E1->RtOpnd)));
                        goto L2;
                    }
                } else if (IS_SUB(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(SUB(ADD(E1->LeftOpnd, E2->LeftOpnd),
                          ADD(E1->RtOpnd, E2->RtOpnd)));
                        goto L2;
                    } else if (IS_CONST(E2->RtOpnd)) {
                        E = INUSE(ADD(SUB(E1->LeftOpnd, E2->RtOpnd),
                          SUB(E2->LeftOpnd, E1->RtOpnd)));
                        goto L2;
                    }
                }

                /* E1 is const-expr. This may help factoring. */
                E = INUSE(SUB(E1->LeftOpnd, SUB(E1->RtOpnd, E2)));
                goto L2;
            } else if (IS_CONST(E1->RtOpnd)) {
                if (IS_ADD(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(ADD(SUB(E2->LeftOpnd, E1->RtOpnd),
                          ADD(E1->LeftOpnd, E2->RtOpnd)));
                        goto L2;
                    }
                } else if (IS_SUB(E2)) {
                    if (IS_CONST(E2->LeftOpnd)) {
                        E = INUSE(ADD(SUB(E2->LeftOpnd, E1->RtOpnd),
                          SUB(E1->LeftOpnd, E2->RtOpnd)));
                        goto L2;
                    } else if (IS_CONST(E2->RtOpnd)) {
                        E = INUSE(SUB(ADD(E1->LeftOpnd, E2->LeftOpnd),
                          ADD(E1->RtOpnd, E2->RtOpnd)));
                        goto L2;
                    }
                }

                /* E1 is expr-const.  This may help factoring. */
                E = INUSE(ADD(NEG(E1->RtOpnd), ADD(E1->LeftOpnd, E2)));
                goto L2;
            }
        }        /* of IS_SUB(E1) */

        /* Look for a*sin^2 + b*cos^2 (= a + (b-a)cos^2) or */
        /*          a*cos^2 + b*sin^2 (= a + (b-a)sin^2.    */
        /* We'll only do this if a=b or both are constants. */
        if (IS_ASINORCOSSQ(E1, &Parm, &a, &NeedFunc))
            if (IS_BSINORCOSSQ(E2, Parm, NeedFunc, &b, &fsq))
            {
                if (SAME_EXPR(a, b)) {
                    E = INUSE(a);
                    goto L2;
                } else if (IS_CONST(a) && IS_CONST(b)) {
                    E = INUSE(ADD(a, MUL(SC(b->ScalarValue - a->ScalarValue), fsq)));
                    goto L2;
                }
            }

        /* try factoring out a constant */
        if (SAME_EXPR(OTHER_PART(E1), OTHER_PART(E2))) {
            E = INUSE(MUL(ADD(CONST_PART(E1), CONST_PART(E2)), OTHER_PART(E1)));
            goto L2;
        }
        if (SAME_EXPR(CONST_PART(E1), CONST_PART(E2))
          && !SAME_EXPR(CONST_PART(E1), gScalarOne)) {
            E = INUSE(MUL(CONST_PART(E1), ADD(OTHER_PART(E1), OTHER_PART(E2))));
            goto L2;
        }

        /* it's hopeless */
        E = NEWX(cBinaryOperatorNode, 0, 0);
        E->NodeValueType = E1->NodeValueType;
        E->BinOp = cAdd;
        E->LeftOpnd = E1;        /* Note: these are already ordered properly */
        E->RtOpnd = E2;
        E = INUSE(E);
    }
L2:
    DISPOSE_EXPR(UNUSE(E1));
    DISPOSE_EXPR(UNUSE(E2));
    return UNUSE(E);
}

pExpr ADD(
          pExpr E1,
          pExpr E2   )
{
    /* Add the two expressions, producing a third which is */
    /* their sum.                                          */

    return APPLYBIN_OP(cAdd, E1, E2);
}
 
 
 
 
 
