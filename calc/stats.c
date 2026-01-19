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

extern long gBinOpCount[], gUnOpCount[];
extern long gAsgCount;        /* counts # assignment statements printed */
extern long gNewCount;        /* counts # expression nodes allocated */
extern long gNewSize;        /* # bytes allocated for expr nodes */
extern long gDispCount;        /*counts # expresson nodes disposed. */
extern long gDispSize;        /* # bytes disposed for expr nodes */

#ifdef THINK_C
#include <MemoryMgr.h>
#else
char *sbrk();
#endif
static char *start_brk;

/*===========*/
/* BYTES_USED*/
/*===========*/

long BYTES_USED(void)
{
    /* Return number of bytes allocated so far. */

#ifdef THINK_C
    return HeapEnd - start_brk;
#else
    return sbrk(0) - start_brk;
#endif
}

/*==============*/
/* ADDOPS_USED  */
/*==============*/

long ADDOPS_USED(void)
{
    /* Return number of binary "+" and "-" and unary "-" printed so far. */

    return gBinOpCount[cAdd] + gBinOpCount[cSubtract] + gUnOpCount[cNegate];
}

/*==============*/
/* MULOPS_USED  */
/*==============*/

long MULOPS_USED(void)
{
    /* Return number of multiplies ("*") printed so far. */

    return gBinOpCount[cMultiply];
}

/*==============*/
/* DIVOPS_USED  */
/*==============*/

long DIVOPS_USED(void)
{
    /* Return number of divides ("/") printed so far. */

    return gBinOpCount[cDivide];
}

/*==============*/
/* ASGOPS_USED  */
/*==============*/

long ASGOPS_USED(void)
{
    /* Return number of assignments printed so far. */

    return gAsgCount;
}

/*===========*/
/* RESET_OPS */
/*===========*/

void RESET_OPS(void)
{
    /* Set OP counts to zero. */
    register tBinaryOperator binop;
    register tUnaryOperator unop;

    for (binop = cAdd; binop <= cDeriv; binop++)
        gBinOpCount[binop] = 0;
    for (unop = cNegate; unop <= cNegate; unop++)
        gUnOpCount[unop] = 0;
    gAsgCount = 0;
}

/*================*/
/* SHOW_COUNTS    */
/*================*/

void SHOW_COUNTS(void)
{
    /* Print out # expression nodes allocated and disposed. */
    printf("%ld nodes allocated (%ld bytes).\n", gNewCount, gNewSize);
    printf("%ld nodes disposed (%ld bytes).\n", gDispCount, gDispSize);
}

/*==============*/
/* RESET_COUNTS */
/*==============*/

void RESET_COUNTS(void)
{
    /* Set Node allocation counts to zero. */

    gNewCount = gNewSize = gDispCount = gDispSize = 0;
#ifdef THINK_C
    start_brk = HeapEnd;
#else
    start_brk = sbrk(0);
#endif
}
