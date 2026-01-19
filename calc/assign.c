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

/*========*/
/* ASSIGN */
/*========*/

void ASSIGN(register pSym S,
       pExpr Val)
{
    /* Assign a value to symbol S.  The expression must be */
    /* of the correct dimensionality for S.                */
    /* The protection level of Val is increased by 1.      */

    if (!S)
        fatal("ASSIGN: undeclared variable.");

    Val = INUSE(Val);
    DISPOSE_EXPR(UNUSE(S->SymValue));
    DISPOSE_EXPR(UNUSE(S->RememberedVal));
    S->RememberedVal = NULL;

    if (!Val)
        S->SymValue = NULL;
    else {
        if (!SAME_TYPE(&Val->NodeValueType, &S->SymValueType)) {
            fprintf(stderr, "ASSIGN: expr has wrong type for \"%s\"\n",
              S->PrintName);
            abort();
        }
        switch (S->SymbolKind) {
            case cFunctionSym:
                fatal("ASSIGN: can't assign to a function.");
            default:        /* Okay */
                break;
        }        /*case*/
        /* *** FIXUP_VREFS(Val, S, S->AssignCountS+1); *** */
        S->SymValue = Val;        /* already INUSEd above */
    }

    S->AssignCountS++;
}        /* ASSIGN */

/* ASSIGN_CLN
 *
 * Performs an ASSIGN followed by a CLEANVAR.
 */
void ASSIGN_CLN(FILE *F,
           pSym S,
           pExpr Val)
{
    ASSIGN(S, Val);
    CLEANVAR(F, S, 0, 0);
}
