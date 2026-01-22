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

/*======*/
/* VAL  */
/*======*/

pExpr VAL(pSym V)
{
    /* Returns the value of the given symbol.  If the symbol */
    /* doesn't have a value, we'll just return a VREF to it. */
    pExpr e;

    C_ASSERT(V != NULL, 1, "VAL");
    switch (V->SymbolKind) {
        case cVariableSym:
            if (!V->SymValue) {
                e = VREF(V);
#ifdef DEBUG
                fputs("VAL: var had no value - making one up", stderr);
                SHOW_TYPE(e);
#endif
                return e;
            } else
                return V->SymValue;

        default:
            fatal("VAL: only variables allowed.");
    }
    return NULL; /*NOTREACHED*/
}

/*======*/
/* VAL1 */
/*======*/

pExpr VAL1(pSym V,
           tIndex I)
{
    /* Returns the value of the given symbol.  If the symbol */
    /* doesn't have a value, we'll just return a VREF to it. */
    pExpr e;

    C_ASSERT(V != NULL, 1, "VAL1");
    switch (V->SymbolKind) {
        case cVariableSym:
            if (!V->SymValue) {
                e = VREF1(V, I);
#ifdef DEBUG
                fputs("VAL1: var had no value - making one up", stderr);
                SHOW_TYPE(e);
#endif
                return e;
            } else
                return INDX(V->SymValue, I);

        default:
            fatal("VAL1: only variables allowed.");
    }
    return NULL; /*NOTREACHED*/
}

/*======*/
/* VAL2 */
/*======*/

pExpr VAL2(pSym V,
           tIndex I,
           tIndex J)
{
    /* Returns the value of the given symbol.  If the symbol */
    /* doesn't have a value, we'll just return a VREF to it. */
    pExpr e;

    C_ASSERT(V != NULL, 1, "VAL2");
    switch (V->SymbolKind) {
        case cVariableSym:
            if (!V->SymValue) {
                e = VREF2(V, I, J);
#ifdef DEBUG
                fputs("VAL2: var had no value - making one up", stderr);
                SHOW_TYPE(e);
#endif
                return e;
            } else
                return INDX2(V->SymValue, I, J);

        default:
            fatal("VAL2: only variables allowed.");
    }
    return NULL; /*NOTREACHED*/
}
