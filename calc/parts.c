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

/*============*/
/* CONST_PART */
/*============*/

pExpr CONST_PART(pExpr F)
{
    /* Returns the constant factor of F, 1 if none.            */
    char Negated;

    if (Negated = IS_NEGOP(F))
        F = F->Opnd;

    F = IS_MUL(F) && IS_CONST(F->LeftOpnd) ? F->LeftOpnd : gScalarOne;
    return Negated ? NEG(F) : F;
}

/*============*/
/* OTHER_PART */
/*============*/

pExpr OTHER_PART(pExpr F)
{
    /* Returns the part not returned by CONST_PART.            */

    F = IS_NEGOP(F) ? F->Opnd : F;
    return IS_MUL(F) && IS_CONST(F->LeftOpnd) ? F->RtOpnd : F;
}
