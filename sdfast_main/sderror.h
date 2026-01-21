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

/* Program error returns. */

/* Exit (or return) status:
        0        success
        1        bad command line syntax
        2        out of memory
        3        key problem
        4        file open or other OS error
        5        error in input file
        6        singular mass matrix
    -1000       a user interrupt occurred (MEC/Motion return only)

   Not all of these will be returned in MM mode -- out of memory, for
   example, is still an exit.

   *** WARNING ***
   If changes are made here, make sure corresponding changes are
   made to exec/include/exec.h. 
*/

#define EXIT_SUCCESS     0
#define EXIT_BADCMDLINE  1
#define EXIT_OUTOFMEMORY 2
#define EXIT_KEYPROBLEM  3
#define EXIT_FILEERR     4
#define EXIT_INFILEERR   5
#define EXIT_SINGMASSMAT 6
#define EXIT_INTRSEEN    -1000

/* During execution, the generated routines are in one of four states:
 *
 *     1. new system parameters (start)
 *     2. initialized           (init)
 *     3. state ready           (state)
 *     4. derivatives ready     (deriv)
 *
 * Some routines can be executed only in certain states; these are listed
 * below.  Also, some routines cause transition from one state to another.
 * These transitions are shown following an `->' symbol below.
 *
 * In general, before any routines have been called we're in `start', and
 * any change to a system `?' parameter (except prescribed motion or 
 * Baumgarte constant) returns us to `start'.  Sdinit()
 * causes a transition to `init'.  Sdstate() takes us to `state', and 
 * `sdderiv' takes us to `deriv'.
 */

#define ST_NOSTATE        -1        /* the execution states */
#define ST_START        0        
#define ST_INIT                1
#define ST_STATEREADY        2
#define ST_DERIVREADY         3

/* SD/FAST generated routine numbers as returned by sderror() */

#define ROU_sdgrav      1        /* -> start */
#define ROU_sdmass      2        /* -> start */
#define ROU_sdiner      3        /* -> start */
#define ROU_sdbtj       4        /* -> start */
#define ROU_sditj       5        /* -> start */
#define ROU_sdpin       6        /* -> start */
#define ROU_sdinit      7        /* -> init  */
#define ROU_sdstate     8        /* init|state|deriv -> state */
#define ROU_sdpsstate   9        /* state */
#define ROU_sdhinget   10        /* state */
#define ROU_sdpointf   11        /* state */
#define ROU_sdbodyt    12        /* state */
#define ROU_sdpresacc  13        /* state */
#define ROU_sdpresvel  14        /* state */
#define ROU_sdprespos  15        /* state */
#define ROU_sdderiv    17        /* state|deriv -> deriv */
#define ROU_sdresid    16        /* state|deriv -> deriv */
#define ROU_sdpseudo   18        /* state|deriv */
#define ROU_sdmom      19        /* state|deriv */
#define ROU_sdsys      20        /* state|deriv */
#define ROU_sdpos      21        /* state|deriv */
#define ROU_sdvel      22        /* state|deriv */
#define ROU_sdorient   23        /* state|deriv */
#define ROU_sdangvel   24        /* state|deriv */
#define ROU_sdtrans    25        /* state|deriv */
#define ROU_sdperr     26        /* state|deriv */
#define ROU_sdverr     27        /* state|deriv */
#define ROU_sdpsqdot   28        /* state|deriv */
#define ROU_sdpsudot   29        /* deriv */
#define ROU_sdgetht    30        /* deriv */
#define ROU_sdreac     31        /* deriv */
#define ROU_sdacc      32        /* deriv */
#define ROU_sdangacc   33        /* deriv */
#define ROU_sdmult     34        /* deriv */
#define ROU_sdaerr     35        /* deriv */
#define ROU_sdindx     36
#define ROU_sdpres     37
#define ROU_sdstab     38
#define ROU_sdgetgrav  39
#define ROU_sdgetmass  40
#define ROU_sdgetiner  41
#define ROU_sdgetbtj   42
#define ROU_sdgetitj   43
#define ROU_sdgetpin   44
#define ROU_sdgetpres  45
#define ROU_sdgetstab  46
#define ROU_sdinfo     47
#define ROU_sdjnt      48
#define ROU_sdcons     49
#define ROU_sdassemble 50
#define ROU_sdinitvel  51
#define ROU_sdstatic   52
#define ROU_sdsteady   53
#define ROU_sdmotion   54
#define ROU_sdfmotion  55
#define ROU_sdequivht  56      /* state|deriv */
#define ROU_sdmassmat  57      /* state|deriv */
#define ROU_sdfrcmat   58      /* state|deriv */
#define ROU_sdrel2cart 59      /* state|deriv */
#define ROU_sdcomptrq  60      /* state|deriv */
#define ROU_sdfulltrq  61      /* state|deriv */
#define ROU_sdvrot     62
#define ROU_sdqdot     63      /* state|deriv */
#define ROU_sdu2qdot   64      /* state|deriv */
#define ROU_sdmulttrq  65      /* state|deriv */
#define ROU_sdudot0    66      /* state|deriv -> state */
#define ROU_sdudotm    67      /* state|deriv -> state */
#define ROU_sdsetudot  68      /* state|deriv -> deriv */

/* SD/FAST error numbers as returned by sderror() */

#define ERR_ZeroTreePin               1
#define ERR_ZeroLoopInbPin1           2
#define ERR_ZeroLoopInbPin2           3
#define ERR_ZeroLoopInbPin3           4
#define ERR_ZeroLoopInbRef            5
#define ERR_NotRtHandSet              6
#define ERR_ZeroLoopBodyPin           7
#define ERR_ZeroLoopBodyRef           8
#define ERR_Inbpin12notPerp           9
#define ERR_Inbpin23notPerp          10
#define ERR_Inbpin13notPerp          11
#define ERR_Inbpin1refNotPerp        12
#define ERR_BodypinRefNotPerp        13
#define ERR_BadlyUnnormEulerParms    14
#define ERR_BadBodyNum               15
#define ERR_BadJointNum              16
#define ERR_BadAxisNum               17
#define ERR_BadAxisNumForThisJoint   18
#define ERR_TriedToSetNonQues        19
#define ERR_BadValueForPres          20
#define ERR_BadUserConstraintNum     21
#define ERR_sdinitMustBeCalledFirst  22
#define ERR_sdstateMustBeCalledFirst 23
#define ERR_sdderivMustBeCalledFirst 24
#define ERR_GravityMustBeSpecified   25
#define ERR_MassMustBeSpecified      26
#define ERR_InertiaMustBeSpecified   27
#define ERR_TreePinMustBeSpecified   28
#define ERR_TreeBtjMustBeSpecified   29
#define ERR_TreeItjMustBeSpecified   30
#define ERR_TreePresMustBeSpecified  31
#define ERR_StabvelMustBeSpecified   32
#define ERR_StabposMustBeSpecified   33
#define ERR_LoopIpinMustBeSpecified  34
#define ERR_LoopIrefMustBeSpecified  35
#define ERR_LoopBpinMustBeSpecified  36
#define ERR_LoopBrefMustBeSpecified  37
#define ERR_LoopBtjMustBeSpecified   38
#define ERR_LoopItjMustBeSpecified   39
#define ERR_LoopPresMustBeSpecified  40
#define ERR_LibraryMismatch          41
#define ERR_DynamicsAnalysisMismatch 42
#define ERR_TreeGimbalLocked         43
#define ERR_LoopGimbalLocked         44
#define ERR_BadTreeCoordNum          45
#define ERR_CantRotAboutZeroVector   46
#define ERR_SingularMassMatrix       47
