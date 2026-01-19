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

/* variable types used by declare_vars(), declare_proc(), and declare_types() */
#define VT_DUP          1        /* same type as previous variable */
#define VT_USER         2        /* user defined type, pointer follows name */
#define VT_INTEGER      3        /* integral type */
#define VT_REAL         4        /* floating point type */
#define VT_TYPENAME     5        /* the name of the type to use follows */
#define VT_PROCNAME     6        /* procedure formal parameter */
#define VT_PAUSE        7        /* don't finish declaration - must be highest
                                    numbered type */
#define VT_BASETYPE     0xff     /* mask for base type */

#define VT_ARRAY        0x0100   /* array of base type, vector, or matrix;
                                    dimension list follows (implies VT_BYREF) */
#define VT_BYREF        0x0200   /* pass to proc by reference */
#define VT_ISUSER       0x0400   /* set in a user_type struct */
#define VT_VECTOR       0x0800   /* is a vector (x3) array (implies VT_BYREF) */
#define VT_MATRIX       0x1000   /* is a matrix (x3x3) array (implies VT_BYREF) */
#define VT_DSYM         0x2000   /* declare a symbol of this type */
#define VT_COND         0x4000   /* conditional declaration */
#define VT_SARRAY       0x8000   /* array with dimensions given as strings */

/* Holds user-defined type.  If array, dimensions will either be integers
   or strings according as type&VT_ARRAY or type&VT_SARRAY. */

/* decl_flags passed to declare_vars and declare_proc */
#define DECL_PROC        0x001   /* when declaring formal parameters */
#define DECL_GLOBAL      0x002   /* for global declarations */
#define DECL_EXTERN      0x004   /* for external declarations */
#define DECL_NODSYM      0x008   /* don't declare symbols */
#define DECL_NUMSUFFIX   0x010   /* add numeric suffix to name */
#define DECL_STRUCT      0x020   /* declare a structured type */
#define DECL_INIT        0x040   /* will initialize this var after decl */
#define DECL_STATIC      0x080   /* for static declarations */
#define DECL_NOPRINT     0x100   /* declare internal symbols only */
#define DECL_FORWARD     0x200   /* forward procedure declaration */
#define DECL_FWDDEFN     0x400   /* procedure definition that was forward */
#define DECL_FUNCTION    0x800   /* this procedure is actually a function */
#define DECL_PACKED      0x1000  /* passed-in vars will be packed in structs */

#include <stdio.h>
