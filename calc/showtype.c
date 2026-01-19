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

static void
SHOW_STUFF(pExpr E)
{
    static char TypeNames[][8] =
      { "scalar", "vector", "matrix", "array1d", "array2d" };

    fprintf(stderr,
      "Node type: %s\nDim1=%d Dim2=%d  Base type: %s  Protection: %d\n",
      TypeNames[(int)E->NodeValueType.ValueType], E->NodeValueType.Dim1,
      E->NodeValueType.Dim2, TypeNames[(int)E->NodeValueType.BaseType],
      E->Protection);
}

/*===========*/
/* SHOW_TYPE */
/*===========*/

void SHOW_TYPE(register pExpr E)
{
    /* A debugging routine which prints out a description of */
    /* the passed expression.                                */

    register tIndex i, j;
    extern char BinOpNames[][6];
    extern char UnOpNames[][2];

    fprintf(stderr, "**SHOW_TYPE** (E=0x%lx)\n", (uintptr_t)E);
    if (!E) {
        fputs("Nil expression\n", stderr);
        return;
    }
    switch (E->NodeKind) {
        case cUnaryOperatorNode:
            fprintf(stderr, "Unary op: %s\n", UnOpNames[E->UnOp]);
            SHOW_STUFF(E);
            fputs("Operand:\n", stderr);
            SHOW_TYPE(E->Opnd);
            fputs("*** end Unary op\n", stderr);
            break;
        case cBinaryOperatorNode:
            fprintf(stderr, "Binary op: %s\n", BinOpNames[E->BinOp]);
            SHOW_STUFF(E);
            fputs("Left:\n", stderr);
            SHOW_TYPE(E->LeftOpnd);
            fputs("Right:\n", stderr);
            SHOW_TYPE(E->RtOpnd);
            fputs("*** end Binary op\n", stderr);
            break;
        case cTernaryOperatorNode:
            fprintf(stderr, "Ternary op: %s\n", 
                E->TerOp == cIfThenElse ? "if-then-else" : "????");
            SHOW_STUFF(E);
            fputs("First:\n", stderr);
            SHOW_TYPE(E->FirstOpnd);
            fputs("Second:\n", stderr);
            SHOW_TYPE(E->SecondOpnd);
            fputs("Third:\n", stderr);
            SHOW_TYPE(E->ThirdOpnd);
            fputs("*** end Ternary op\n", stderr);
            break;
        case cScalarConstNode:
            fprintf(stderr, "Scalar: %g\n", E->ScalarValue);
            SHOW_STUFF(E);
            break;
        case cArray1dNode:
            fputs("Array1d.\n", stderr);
            SHOW_STUFF(E);
            for (i = 0; i < E->NodeValueType.Dim1; i++) {
                fprintf(stderr, "Elt %d:\n", i);
                SHOW_TYPE(E->Array1dValue[i]);
            }
            fputs("*** end Array1d\n", stderr);
            break;
        case cArray2dNode:
            fputs("Array2d.\n", stderr);
            SHOW_STUFF(E);
            for (i = 0; i < E->NodeValueType.Dim1; i++)
                for (j = 0; j < E->NodeValueType.Dim2; j++) {
                    fprintf(stderr, "Elt %d,%d:\n", i, j);
                    SHOW_TYPE(G2d(E, i, j));
                }
            fputs("*** end Array2d\n", stderr);
            break;
        case cVarRefNode:
            fprintf(stderr,
              "Var ref. Var is \"%s\"\nAssignCount=%ld\nNum indices: %d\n",
              E->VarRef->PrintName, E->AssignCountE, E->NumIndices);
            for (i = 0; i < E->NumIndices; i++)
                fprintf(stderr, "%d ", E->Indices[i]);
            if (E->NumIndices)
                putc('\n', stderr);
            SHOW_STUFF(E);
            break;
        case cFunctionCallNode:
            fprintf(stderr, "One-arg func call to %s\n", E->FuncVarRef->PrintName);
            SHOW_STUFF(E);
            break;
        case cFunction2CallNode:
            fprintf(stderr, "Two-arg func call to %s\n", E->Func2VarRef->PrintName);
            SHOW_STUFF(E);
            break;
    }
}        /*SHOW_TYPE*/
