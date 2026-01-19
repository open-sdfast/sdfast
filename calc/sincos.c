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

/* CALL_KNOWN_FUNC
 *
 * Generic routine for calling a known one-argument function 
 * like sin, cos, etc.
 */
pExpr 
CALL_KNOWN_FUNC(enum tKnownFunction func,
                pExpr arg)
{
    /* produces the expression func(arg) */
    register pExpr X;
    double (*cfunc)();
    pSym calcfunc;
    extern pSym gSineFunction,gCosineFunction,gAsinFunction,gAcosFunction,
                gAbsFunction,gSqrtFunction;

    C_ASSERT(arg != NULL, 1, "CALL_KNOWN_FUNC");
    if (arg->NodeValueType.ValueType != cScalarVal)
        fatal("CALL_KNOWN_FUNC: only scalars allowed.");

    switch (func) {
        case cSine:         cfunc = sin;
                        calcfunc = gSineFunction;
                        break;
        case cCosine:        cfunc = cos;
                        calcfunc = gCosineFunction;
                        break;
        case cAsin:        cfunc = asin;
                        calcfunc = gAsinFunction;
                        break;
        case cAcos:        cfunc = acos;
                        calcfunc = gAcosFunction;
                        break;
        case cAbs:        cfunc = fabs;
                        calcfunc = gAbsFunction;
                        break;
        case cSqrt:        cfunc = sqrt;
                        calcfunc = gSqrtFunction;
                        break;
        default:
            fatal("CALL_KNOWN_FUNC: unknown one-arg function.");
    }

    if (IS_CONST(arg))
        return SC((*cfunc)(arg->ScalarValue));
    else {
        X = NEWX(cFunctionCallNode, 0, 0);
        SCALAR_TYPE(&X->NodeValueType);
        X->FuncVarRef = calcfunc;
        X->FuncCallParm = arg;
        return X;
    }
} 

/* CALL_KNOWN_FUNC2
 *
 * Generic routine for calling a known two-argument function 
 * like atan2.
 */
pExpr 
CALL_KNOWN_FUNC2(enum tKnownFunction func,
                 pExpr arg1, pExpr arg2)
{
    /* produces the expression func(arg1,arg2) */
    register pExpr X;
    double (*cfunc)();
    pSym calcfunc;
    extern pSym gAtan2Function;

    C_ASSERT(arg1 != NULL && arg2 != NULL, 1, "CALL_KNOWN_FUNC2");
    if (arg1->NodeValueType.ValueType != cScalarVal ||
            arg2->NodeValueType.ValueType != cScalarVal)
        fatal("CALL_KNOWN_FUNC2: only scalars allowed.");

    switch (func) {
        case cAtan2:         cfunc = atan2;
                        calcfunc = gAtan2Function;
                        break;
        default:
            fatal("CALL_KNOWN_FUNC2: unknown two-arg function.");
    }

    if (IS_CONST(arg1) && IS_CONST(arg2))
        return SC((*cfunc)(arg1->ScalarValue, arg2->ScalarValue));
    else {
        X = NEWX(cFunction2CallNode, 0, 0);
        SCALAR_TYPE(&X->NodeValueType);
        X->Func2VarRef = calcfunc;
        X->Func2CallParm1 = arg1;
        X->Func2CallParm2 = arg2;
        return X;
    }
} 

/* SINE
 * Produces the expression sin(E).
 */
pExpr 
SINE(pExpr E)
{
    return CALL_KNOWN_FUNC(cSine,E);
}

/* COSINE
 * Produces the expression cos(E).
 */
pExpr 
COSINE(pExpr E)
{
    return CALL_KNOWN_FUNC(cCosine,E);
}

/* ASINE
 * Produces the expression asin(E).
 */
pExpr 
ASINE(pExpr E)
{
    return CALL_KNOWN_FUNC(cAsin,E);
}

/* ACOSN
 * Produces the expression acos(E).
 */
pExpr 
ACOSN(pExpr E)
{
    return CALL_KNOWN_FUNC(cAcos,E);
}

/* ABS
 * Produces the expression abs(E).
 */
pExpr 
ABS(pExpr E)
{
    return CALL_KNOWN_FUNC(cAbs,E);
}

/* SQRTT
 * Produces the expression sqrt(E).
 */
pExpr 
SQRTT(pExpr E)
{
    return CALL_KNOWN_FUNC(cSqrt,E);
}

/* ATANG2
 * Produces the expression atan2(E1,E2).
 */
pExpr 
ATANG2(pExpr E1, pExpr E2)
{
    return CALL_KNOWN_FUNC2(cAtan2,E1,E2);
}
