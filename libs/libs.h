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

/*=========================================================================*/
/*  Constants and types shared by "libs" module and client programs.       */
/*=========================================================================*/

/* Different OS's use different characters to separate directory names
 * in a pathname.  On the MAC it is (apparently) ":".  Under Unix it
 * is "/".  VMS puts the directory in square brackets, e.g. [dir]file.ext,
 * so the "]" serves as the separator.  Under NT, names can be
 * d:\blah\blah\blah.ext or d:blah.ext so we have to look for either ":"
 * or "\".  For convenience, we also accept "/".  
 * We allow up to 3 pathname delimiters.  If there are fewer, just repeat
 * one of them.
 *
 * IMPORTANT: put these in the order that you want them searched.  In the
 * dos case d:\blah\blah.ext, for example, you MUST search for "\" before
 * searching for ":"!
 */

#ifdef THINK_C
#  include "::calc:calc.h"
#  include "::calc:decl.h"
#  define PATHNAME_DELIM1        ':'
#  define PATHNAME_DELIM2        ':'
#  define PATHNAME_DELIM3        ':'
#else
#  include "../calc/calc.h"
#  include "../calc/decl.h"
#  ifdef apollo
#  else
#    ifndef vms
#      ifndef RWIN32
#        include <values.h>
#      endif
#    endif
#  endif
#  ifdef RWIN32
#    define PATHNAME_DELIM1        '\\'
#    define PATHNAME_DELIM2        '/'
#    define PATHNAME_DELIM3        ':'
#  else
#    ifdef vms
#      define PATHNAME_DELIM1        ']'
#      define PATHNAME_DELIM2        ']'
#      define PATHNAME_DELIM3        ']'
#    else /* unix */
#      define PATHNAME_DELIM1        '/'
#      define PATHNAME_DELIM2        '/'
#      define PATHNAME_DELIM3        '/'
#    endif
#  endif
#endif

typedef pExpr expr;
typedef pSym sym;
typedef char flags_t;        /* holds up to 7 flags (don't use high bit) */
typedef char small_t;        /* holds non-neg number < 128 (don't use high bit) */
typedef char bool_t;        /* holds 0 or 1 for false or true */

#define NULLEXPR        ((expr)NULL)
#define SPECIALEXPR        ((expr)-1)        /* used as a flag */

#ifdef THINK_C
#define cMaxNumBodies             150
#define cMaxNumDOF             200                /* array must be < 32k */
#define cMaxNumLoops             100
#define cMaxNumExplicitConst 100
#else
#define cMaxNumBodies             500
#define cMaxNumDOF            3000
#define cMaxNumLoops             300
#define cMaxNumExplicitConst 300
#endif

#define cMaxNumJoints             (cMaxNumBodies+cMaxNumLoops)

#define cGroundBody               -1   /* fake body number for ground */
#define cUnspecifiedBody       -100 /* used to init body nos. (-1 is legit) */
#define cUnspecifiedJoint      -101 /* used to initialize jt nos. */
#define cUnspecifiedConstraint -102 /* used to initialize constraint nos. */
#define cUnspecifiedAxis       -103 /* used to initialize jt axis nos. */
#define cUninitializedIndex    -104 /* an array indx that hasn't been set yet */

/* IBM fortran at Hughes goes nuts with a limit on expression length 
   much bigger than this. */
#define cMaxNumTerms 100

/* In many places, anything with absolute value less than this is assumed 
   zero.  This should be a little smaller than the square root of the 
   machine precision. */
#define cNearZero 1e-10

/* In matrix decompositions, diagonal elements less than this fraction
 * of the largest diagonal are negligible.  This is what eliminates
 * redundant constraints and massless bodies.  This should be a few 
 * orders of magnitude larger than the machine precision.
 */
#define cEquationNegligible        1e-13

/* These are option codes for lines in the keyfile. */
#define cOptionKane        1 /* Actually needed by both Kane & Order(N) */
#define cOptionOrderN        2
#define cOptionADSIM        3
#define cOptionAda        4

typedef char string3[4];
typedef char string10[11];
typedef char string11[12];
typedef char string20[21];
typedef char string32[33];

typedef enum {cEndOfFileNext, cNumberNext, cEqualNext, cQuestionNext, 
              cIdentNext} ItemType_t;

/*
 * Joint descriptor.  This is used both for ordinary joints between
 * bodies and for the `joints' used to close loops.  For ordinary joints,
 * the inboard body is the lower-numbered body, that is, the body closer
 * to the base body.  For loop joints, the choice of inboard and outboard
 * is arbitrary and is selected by the user in the input file.
 *
 * Joints have 0-6 degrees of freedom.  For tree joints, this produces
 * 0-6 generalized coordinates, plus 1 extra for ball joints.  For loop
 * joints, there are 6-(# dof) constraint equations generated.  For any 
 * joint, each prescribed (or runtime prescribed) hinge adds a constraint
 * equation.
 *
 * For a tree joint the pins are the rotational or translational axes of the 
 * joint.  For a loop joint, there are 0, 1 or 2 pins on the inboard body and 0 
 * or 1 pin on the outboard body, stored in Pins[INBPIN1..3]
 * and Pins[BODYPIN] respectively.  In addition, there can be 
 * reference lines specified which will be in Pins[INBREF] and Pins[BODYREF].  
 * Some of these are optional and will be NULL if not present.  Inboard Pin1 
 * and Pin2 must be perpendicular if both are specified.  Inboard Pin3 must
 * be perpendicular to Pin2.  A reference line must be perpendicular to
 * the pin specified on the same body.  Reference lines are always optional.
 */
#define INBPIN1 0         /* pin1, pin2, and pin3 *must* be 0, 1, 2 here */
#define INBPIN2 1
#define INBPIN3 2
#define INBREF  3
#define BODYPIN 4
#define BODYREF 5
#define LOW_PIN INBPIN1                /* for iterating over loop pins and refs */
#define HIGH_PIN BODYREF

/* Bit numbers for per-input value flags. */
#define ISQUESFLG 1                /* is this element variable? */
#define HASNOMFLG 2             /* if so, is there a nominal value? */

/* Information about joint pins.  (This is just a fancy constant.) */
typedef struct {
    char        *jtname;        /* printable joint name for _info file */
    small_t     dof;                /* no. of degrees of freedom provided by jt. */
    small_t        nreq;                /* min number of (inb)pins required */
    small_t        nmax;                /* max number of (inb)pins allowed */
    bool_t      inbrefOK;        /* can have inbref? */
    bool_t      bodypinOK;        /* can have bodypin? */
    bool_t      bodyrefOK;        /* can have bodyref? */
    bool_t      hasball;        /* does this jt contain a ball? */
    bool_t      hasgimbal;      /* does this jt contain a gimbal? */

    small_t     doftype[6];     /* axis type for each dof */
#define AX_NONE  0                /* doftype defines */
#define AX_ROT   1
#define AX_TRANS 2
#define AX_BALL  3
    small_t     whichpin[6];    /* which input pin to use for each axis */
} JointInfo_t;

/* Applied Motion depends on these numbers, and they are documented
 * in the SD/FAST User's manual.  DON'T CHANGE THEM when you add more!
 * Feel free to occupy the "Unused" slots.
 */
typedef enum {cUnknownJoint        = 0, 
              cPinJoint                = 1, 
              cUjoint                = 2, 
              c3dJoint                = 3, 
              cBallJoint        = 4, 
              cSlidingJoint        = 5,
              c6dJoint                = 6,
              cCylJoint                = 7,
              cPlanarJoint        = 8,
              cWeldJoint        = 9,
              cBushingJoint        = 10,
              cBearingJoint        = 11,

              cUnusedJt12        = 12,
              cUnusedJt13        = 13,
              cUnusedJt14        = 14,
              cUnusedJt15        = 15,
              cUnusedJt16        = 16,
              cUnusedJt17        = 17,
              cUnusedJt18        = 18,
              cUnusedJt19        = 19,

              /* tree only! */
              cRevPlanarJoint         = 20,
              cRev6dJoint             = 21,
              cRevBushingJoint        = 22,
              cRevBearingJoint        = 23  }  
JointKind_t;

#ifdef SDMAIN
JointInfo_t JointInfo[] = {
/* Fill in JointInfo -- order must match the above enum.  For UnknownJoint,
 * we fill in values that will cause input parser to accept just about
 * anything.  Errors will be caught later when the joint type is known. 
 *
 * Caution on weld joints: the tree weld allows no pins while the loop
 * weld allows one for alignment.  "nmax" in this table is correct for
 * the loop joint, but should be zero for tree welds.  This has to be
 * handled in the code as a special case.
 */

 /* Joint         inbref bodypin bodyref  has   has
    dof nreq nmax   OK     OK      OK    ball  gimbal */
{/*cUnknownJoint   */ "?!?!?(bug)", 
    3,   0,   3,     1,     1,      1,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cPinJoint       */ "Pin", 
    1,   1,   1,     1,     1,      1,     0,        0,
    { AX_ROT,   AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUjoint         */ "U-joint",
    2,   2,   2,     0,     1,      1,     0,        0,
    { AX_ROT,   AX_ROT,   AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*c3dJoint        */ "Gimbal",
    3,   3,   3,     0,     1,      1,     0,        1,
    { AX_ROT,   AX_ROT,   AX_ROT,   AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cBallJoint      */ "Ball",
    3,   0,   0,     0,     0,      0,     1,        0,
    { AX_BALL,  AX_BALL,  AX_BALL,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cSlidingJoint   */  "Sliding",
    1,   1,   1,     1,     1,      1,     0,        0,
    { AX_TRANS, AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*c6dJoint        */  "Sixdof",
    6,   3,   3,     0,     0,      0,     1,        0,
    { AX_TRANS, AX_TRANS, AX_TRANS, AX_BALL,  AX_BALL,  AX_BALL },
        { 0 } },
{/*cCylJoint       */  "Cylinder",
    2,   1,   1,     1,     1,      1,     0,        0,
    { AX_TRANS, AX_ROT,   AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cPlanarJoint    */  "Planar", 
    3,   3,   3,     0,     1,      1,     0,        0,
    { AX_TRANS, AX_TRANS, AX_ROT,   AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
/* SEE ABOVE FOR NOTE ABOUT WELD JOINT "nmax" */
{/*cWeldJoint      */  "Weld", 
    0,   0,   1/*!*/,1,     1,      1,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cBushingJoint   */  "Bushing", 
    6,   3,   3,     0,     1,      1,     0,        1,
    { AX_TRANS, AX_TRANS, AX_TRANS, AX_ROT,   AX_ROT,   AX_ROT  },
        { 0 } },
{/*cBearingJoint   */  "Bearing",
    4,   3,   3,     0,     1,      1,     0,        1,
    { AX_TRANS, AX_ROT,   AX_ROT,   AX_ROT,   AX_NONE,  AX_NONE },
        { 0 } },

/* unused joint slots */
{/*cUnusedJt12     */ "?!jt12(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt13     */ "?!jt13(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt14     */ "?!jt14(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt15     */ "?!jt15(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt16     */ "?!jt16(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt17     */ "?!jt17(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt18     */ "?!jt18(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cUnusedJt19     */ "?!jt19(bug)", 
    0,   0,   0,     0,     0,      0,     0,        0,
    { AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },

/* tree only */
{/*cRevPlanarJoint */  "Rplanar",
    3,   3,   3,     0,     0,      0,     0,        0,
    { AX_ROT,   AX_TRANS, AX_TRANS, AX_NONE,  AX_NONE,  AX_NONE },
        { 0 } },
{/*cRev6dJoint     */  "Rsixdof",
    6,   3,   3,     0,     0,      0,     1,        0,
    { AX_BALL,  AX_BALL,  AX_BALL,  AX_TRANS, AX_TRANS, AX_TRANS},
        { 0 } },
{/*cRevBushingJoint*/  "Rbushing",
    6,   3,   3,     0,     0,      0,     0,        1,
    { AX_ROT,   AX_ROT,   AX_ROT,   AX_TRANS, AX_TRANS, AX_TRANS},
        { 0 } },
{/*cRevBearingJoint*/  "Rbearing",
    4,   3,   3,     0,     0,      0,     0,        1,
    { AX_ROT,   AX_ROT,   AX_ROT,   AX_TRANS, AX_NONE,  AX_NONE },
        { 0 } },
};
#else
extern JointInfo_t         JointInfo[];
#endif

typedef struct {
    string32        JointName;
    int         InbBody;        /* inboard and outboard body numbers    */
    int                OutbBody;        /*   (can be real or pseudo body index) */
    JointKind_t JointKind;

    expr        Pins[6];        /* 0-6 vectors */
    expr        Pres[6];        /* 0-6 `booleans' (scalar 0,1,?) */
    expr         BodyToJoint;        /* vector */
    expr         InbToJoint;        /* vector */
    flags_t        PinFlg[6][3];        /* flags for each input element */
    flags_t        PresFlg[6];     /*             |                */
    flags_t        BtjFlg[3];      /*             |                */
    flags_t        ItjFlg[3];      /*             v                */

    int         JointDOF;        /* 1-6 hinges */
    int         JointPres;        /* 1-6 # of hinges that may be prescribed */
    int                whichaxis;        /* 0 except for pseudo-BallJoint is 0-2 */
} JointDesc_t;

/* Information about constraints.  (This is just a fancy constant.) */
typedef struct {
    small_t        nbod;                /* number of bodies needed */
    small_t        njnt;                /* number of joints needed */
    small_t        nsc;                /* number of scalars needed */
    small_t        npt;                /* number of points needed */
    small_t        nvec;                /* number of vectors needed */
    small_t        nmult;                /* number of multipliers generated */
    bool_t        holonomic;        /* is this a position constraint? */
} ConstraintInfo_t;
typedef enum {cUnknownCons, cUserCons, cGearCons, cScrewCons, 
              cDistanceCons,cPerpCons,cCoordCons} 
ConstraintKind_t;

/* Different constraints involve different numbers of bodies, joints,
 * points, vectors, and scalars.  Set the maximum number for any of the
 * supported constraint types here.  Numbers in the ConstraintInfo table should
 * never exceed the associated constant defined here.
 */
#define MAXCONSBODS  2        /* bodies */
#define MAXCONSJNTS  2        /* joints */
#define MAXCONSSCS   1        /* scalars */
#define MAXCONSPTS   2        /* points */
#define MAXCONSVECS  2        /* vectors */
#define MAXCONSMULTS 1        /* multipliers */

#ifdef SDMAIN
ConstraintInfo_t ConstraintInfo[] = {
/* Fill in ConstraintInfo -- order must match the above ConstraintKind_t enum.
 * For cUnknownCons, just fill in zeroes: we'll make the guy tell
 * us what type of constraint before supplying any parameters.
 */

 /* Constraint              nbod  njnt  nsc  npt  nvec  nmult   holonomic */
{/*cUnknownCons           */   0,    0,   0,   0,    0,     0,           0},
{/*cUserCons              */   0,    0,   0,   0,    0,     1,  /*maybe*/1},
{/*cGearCons              */   2,    0,   0,   1,    2,     1,           0},
{/*cScrewCons             */   0,    2,   1,   0,    0,     1,           0},
{/*cDistanceCons   */   0,    0,   1,   2,    0,     1,           1},
{/*cPerpCons       */   0,    0,   0,   0,    2,     1,           1},
{/*cCoordCons      */   0,    0,   1,   2,    1,     1,           1},
};
#else
extern ConstraintInfo_t ConstraintInfo[];
#endif

/* This type is a vector expression, along with the body number of
 * the body to which the vector is attached.  If this represents a
 * point location, it is the vector from the Body's COM to the point.
 */
typedef struct {
    int Body;        /* body number */
    expr Vec;        /* expression of vector type */
} vexpr_t;

/*
 * Constraint descriptor, used to contain all information about a
 * single constraint element.  Each constraint element can generate
 * one or more constraint equations (currently always one).
 */

typedef struct {
    ConstraintKind_t ConstraintType;
    string32 ConstraintName;

    int nbod,njnt,npt,nvec,nsc,nmult;

    int  Bodies [MAXCONSBODS];
    int  Joints [MAXCONSJNTS];
    int  Axes   [MAXCONSJNTS];
    vexpr_t Points [MAXCONSPTS];
    vexpr_t Vectors[MAXCONSVECS];
    expr    Scalars[MAXCONSSCS];

    flags_t PointsFlg [MAXCONSPTS][3];
    flags_t VectorsFlg[MAXCONSPTS][3];
    flags_t ScalarsFlg[MAXCONSPTS];

    Index_t Mindex;        /* index in mults array for first assoc. multiplier */
} ConstraintDesc_t;
typedef ConstraintDesc_t ConstTable_t[cMaxNumExplicitConst];

/*
 * Body descriptor, used both for real bodies and pseudo bodies.
 * Bodies are numbered 0 to n-1, while pseudo bodies are numbered 
 * 0 to s-1.  The inboard joint of a pseudo body is guaranteed to be
 * a pin, slider or one `pin' of a ball joint.  The inboard body of the 
 * first body in the system is cGroundBody.
 *
 * The `realbody' field is true for all real bodies, and for the last
 * body in a series of pseudobodies (that is, the one which inherits the
 * real body's mass, etc.)  A `realbody' pseudobody may also have a set
 * of otherwise disconnected bodies which are attached to it by weld joints.
 * These are hung off the pseudobody via the weldlist pointer, by real body
 * number (that is, the lowest body-numbered welded body comes first).  In
 * the weldlist body's "jnt" field, the InbBody and OutbBody fields are
 * just regular body (not pseudobody) indices.  These serve to make the
 * weldlist a tree.
 *
 * Later, the mass properties of all welded-together bodies will be combined
 * into a composite pseudobody which will be used for most computations.
 */
typedef struct BodyDesc_s        BodyDesc_t;
struct BodyDesc_s {
    string32         BodyName;        /* the body name */
    expr         Mass;                /* scalar */
    expr         Inertia;        /* matrix */
    flags_t        MassFlg;
    flags_t        InerFlg[3][3];
    int                realbody;        /* boolean */
    JointDesc_t        jnt;
    BodyDesc_t        *weldlist;
};
typedef BodyDesc_t BodyTable_t[cMaxNumBodies];
typedef BodyDesc_t PseudoBodyTable_t[cMaxNumDOF];

typedef struct {
    string32         OutbBodyName;        /* name of the `outboard' body */
    JointDesc_t        jnt;
} LoopDesc_t;
typedef LoopDesc_t LoopConstTable_t[cMaxNumLoops];

/*
 * This record contains a parsed form of the input file, plus a lot
 * of commonly useful information derived from the input file.  Also contains 
 * a second "view" of the input data for the tree system, in which all
 * joints are either pins, sliders or ball `pins'.  This view is in the 
 * table of "PseudoBodies" in which massless bodies have been added
 * where required to split up multiple-axis joints into several single-axis
 * joints.  Also, any bodies which are welded together by tree weld joints
 * are combined into a single composite pseudobody.  The PseudoBody view 
 * simplifies later processing.
 *
 * Information mapping joint axes and other constraints into the q,u, and 
 * m (multipliers) arrays is stored here.  The mapping is as follows:
 *
 *     <------------------ s -------------------> <------ nb ------>
 * q: | `dof' generalized coords for tree joints | 4th Euler Params |
 *     ------------------------------------------ ------------------
 *                  ^    ^                                   ^
 *     <------------|----| s ------------------->      BallQ |
 * u: | `dof' generalized speeds                 |
 *     ------------------------------------------ 
 *                  ^    ^
 *         FirstDOF |    | LastDOF                
 *
 *     <---- np ---> <--- nlc --> <-------- nxc --------><------ nu ------>
 * m: | Pres. const | Loop const | Explicit constraints / User constraints |
 *     --------------------------------------------------------------------
 *           ^              ^           ^
 *     PresM |       FirstM |    Mindex |
 *
 * A body whose inboard tree joint is a weld is called a "welded" body.  
 * For a welded body w, we follow the convention that LastDOF[w] = LastDOF[b] 
 * where b is the innermost (topologically closest to ground) body in the set 
 * of welded-together bodies including both body b and body w.  FirstDOF[w]
 * is set to maintain the relation LastDOF = FirstDOF + ndof - 1, so for
 * a weld (ndof=0) this implies FirstDOF[w] = LastDOF[w] + 1.  By convention,
 * if body w is welded to ground then LastDOF[w] is -1 (and FirstDOF[w] is 0).
 *
 * There are also pseudo-coordinates (unrelated to pseudobodies) generated 
 * for loop joints, which are laid out as follows:
 *
 *      <------------------ sl ------------------> <----- nlb ------>
 * lq: | `dof' generalized coords for loop joints | 4th Euler Params |
 *      ------------------------------------------ ------------------
 *                   ^    ^                                   ^
 *      <------------|----|- s ------------------>      BallQ |
 * lu: | `dof' generalized speeds                 |
 *      ------------------------------------------ 
 *                   ^    ^
 *          FirstDOF |    | LastDOF                
 *
 * All indices are relative, in the sense that they give position measured
 * from the beginning of the appropriate array (q, u, m, lq, or lu).  For
 * FirstDOF, LastDOF, and BallQ the numbering scheme measures from the 
 * beginning of q for tree joints and then switches to lq for loop joints.
 * For loop weld joints, FirstDOF=LastDOF+1, as for trees, but we do not
 * have a concept of a "set of welded-together bodies" so the particular
 * value of LastDOF is not important for loop welds.
 *
 * Mindex is kept in the ConstTable array rather than directly in this struct.
 * Note that Mindex is used both for explicit and user constraints.
 */

typedef struct {
    int Grounded;        /* is this a grounded system? */

    /* Expressions associated with system as a whole. */
    expr GravExpr;        /* holds the gravity vector if specified */
    expr StabVelExpr;        /* holds the stabvel value if specified */
    expr StabPosExpr;        /* holds the stabpos value if specified */
    expr QuestionMark;        /* dummy expr which stands for `?' */
    flags_t GravFlg[3];
    flags_t StabVelFlg;
    flags_t StabPosFlg;

    /* Tree and loop system counts. */
    Index_t n;         /* # of real bodies in the system, not counting ground */
    Index_t nb;         /* # of tree ball joints */
    Index_t s;         /* # of hinges (DOF) in tree joints */
    Index_t nl;         /* # of loop joints */
    Index_t nlb; /* # of loop ball joints */
    Index_t sl;         /* # of hinges in loop joints */

    /* Real body and joint stuff */
    Index_t FirstDOF[cMaxNumJoints];         /* q/lq indx of 1st DOF of joint */
    Index_t LastDOF[cMaxNumJoints];          /* q/lq indx of last DOF of joint */
    Index_t BallQ[cMaxNumJoints];          /* q/lq indx for 4th Euler param */
    Index_t FirstM[cMaxNumLoops];        /* 1st mults indx for loop constraint
                                           (loop joints only) */
    Index_t PresM[cMaxNumJoints];          /* 1st mults indx for pres constraint */
    BodyDesc_t *Bodies;                 /* points to a BodyTable_t */

    /* Pseudo-body stuff (tree joints only).  This includes topology, 
     * geometry and mass properties for each DOF in the system, plus a
     * special pseudobody representing bodies which are welded to ground.
     * Note that each pseudobody may be a composite of several welded-together
     * real bodies. psmk[p] is the composite mass for pseudobody p (if p
     * is pseudo-ground, use psmkg instead).  psik[p] is the composite inertia
     * matrix about p's composite COM, expressed in p's frame.  psrk[p] is 
     * the vector from the composite COM to the inboard hinge point, expressed
     * in p's frame.  psri[p] is the vector from the inboard body's composite
     * COM to the inboard hinge point, expressed in the inboard body's frame.
     * Real ground's "COM" is defined to be at (0,0,0), and the ground   
     * pseudobody's inboard "joint" is also at (0,0,0), so psrig is always 0.
     */

    /* Intermediate symbols associated with pseudobody properties. */
    sym rhead, rcom, psrcomg, psrcom, mkrcomt;

    BodyDesc_t GndPseudoBody;                /* special PseudoBody for ground */
    sym psmkg, psikg, psrkg, psrig;

    BodyDesc_t *PseudoBodies;                /* points to a PseudoBodyTable_t */
    sym psmk, psik, psrk, psri;

    /* Constraint stuff */
    Index_t np;         /* # of (maybe) prescribed hinges (1 constraint per) */
    Index_t nlc; /* # of loop constraints */
    Index_t nxc; /* # of explicit constraints */
    Index_t nu;         /* # of user constraints */
    Index_t nc;         /* total # of constraints (np+nlc+nxc+nu) */
    LoopDesc_t            *LoopConst;                /* points to a LoopConstTable_t */
    ConstraintDesc_t *Const;                /* points to a ConstTable_t */

    /* Handy stuff. */
    Index_t nq;  /* # of q's (= s+nb) */
    Index_t nlq; /* # of lq's (= sl+nlb) */
    Index_t nj;  /* # of joints (= n+nl) */
    Index_t nh;  /* # of hinges (= s+sl) */

    /* Prescribed motion stuff.  
     *   Notes: 
     *     s_free           is min # tree DOF system could actually have
     *     s_free+s_runtime is max # tree DOF system could actually have
     *            s_free+s_pres+s_runtime = s
     */
    Index_t s_free;        /* # of definitely NOT prescribed tree DOF */
    Index_t s_pres;        /* # of definitely prescribed tree DOF */
    Index_t s_runtime;        /* # of tree DOF which may be prescribed at run time */

    /* type definitions used for declaring variables */
    struct user_type type_Int;
    struct user_type type_IntVec;
    struct user_type type_IntMat;
    struct user_type type_Vec;
    struct user_type type_Mat;
    struct user_type type_IntArr_n;
    struct user_type type_IntVec_n;
    struct user_type type_IntMat_n;
    struct user_type type_Arr_n;
    struct user_type type_Vec_n;
    struct user_type type_Mat_n;
    struct user_type type_IntArr_s;
    struct user_type type_IntVec_s;
    struct user_type type_Arr_s;
    struct user_type type_Vec_s;
    struct user_type type_Mat_s;
    struct user_type type_Arr_s_s;
    struct user_type type_Vec_s_s;
    struct user_type type_Arr_nq;
    struct user_type type_Arr_nlq;
    struct user_type type_IntVec_nl;
    struct user_type type_Arr_nl;
    struct user_type type_Vec_nl;
    struct user_type type_Mat_nl;
    struct user_type type_IntArr_sl;
    struct user_type type_Arr_sl;
    struct user_type type_Arr_nh;
    struct user_type type_Vec_nh;
    struct user_type type_Arr_nj;
    struct user_type type_Vec_nj;
    struct user_type type_IntArr_nc;
    struct user_type type_Arr_nc;
    struct user_type type_Arr_nc_nc;
    struct user_type type_Arr_nc_s;
    struct user_type type_Arr_s_nc;
    struct user_type type_Arr_nxc;
    struct user_type type_Vec_nxc;

    /* Input file-related symbols and their nominal values */

    /* system and body parameters */
    sym  grav;        /* gravity */
    expr grav_nom;
    sym  mk;        /* masses */
    expr mk_nom;
    sym  ik;        /* Inertia matrices for each body, at its mass center */
    expr ik_nom;

    /* tree joint parameters */
    sym  pin;        /* pin axis orientation vectors */
    expr pin_nom;
    sym  rk;        /* vectors from COM to joints */
    expr rk_nom;
    sym  ri;        /* vectors from COM of inboard body to joint */
    expr ri_nom;
    sym  pres;        /* which tree degrees of freedom are prescribed? */
    expr pres_nom;

    /* loop parameter stuff */
    sym  inbpin1;   /* 1st inboard pins for loop joints */
    expr inbpin1_nom;
    sym  inbpin2;   /* 2nd inboard pins for loop joints */
    expr inbpin2_nom;
    sym  inbpin3;   /* 3rd inboard pins for loop joints */
    expr inbpin3_nom;
    sym  inbref;    /* inboard reference line for loop joints (=inbref) */
    expr inbref_nom;
    sym  bodypin;   /* outboard pins for loop joints */
    expr bodypin_nom;
    sym  bodyref;   /* outboard reference line for loop joints */
    expr bodyref_nom;
    sym  lbtj;      /* outboard body to joint vector for loop joints */
    expr lbtj_nom;
    sym  litj;      /* inboard to joint vector for loop joints */
    expr litj_nom;
    sym  lpres;     /* which loop degrees of freedom are prescribed? */
    expr lpres_nom;

    /* constraint stuff */
    sym  conspt     [MAXCONSPTS];            /* array of points */
    expr conspt_nom [MAXCONSPTS];
    sym  consvec    [MAXCONSVECS];            /* array of vectors */
    expr consvec_nom[MAXCONSVECS];
    sym  conssc     [MAXCONSSCS];            /* array of scalars */
    expr conssc_nom [MAXCONSSCS];

    /* numerical stuff */
    sym  stabvel; /* velocity stabilization constant */
    expr stabvel_nom;
    sym  stabpos; /* position stabilization constant */
    expr stabpos_nom;
} SystemInfo_t;

/* Command line option stuff. */

#ifdef unix
#define DYNSUF                 "_dyn"
#define INFOSUF                "_info"
#define SARSUF                "_sar"
#define LIBNAME         "lib"                /* must add prefix (e.g. sdlib) */
#endif

#ifdef vms
#define DYNSUF                 "_dyn"
#define INFOSUF                "_info"
#define SARSUF                "_sar"
#define LIBNAME         "lib"                /* must add prefix (e.g. sdlib) */
#endif

#ifdef RWIN32
#define DYNSUF                 "_d"
#define INFOSUF                "_i"
#define SARSUF                 "_s"
#define LIBNAME         "lib"                /* must add prefix (e.g. sdlib) */
#endif

#define OPT_DEFAULT        -1        /* means `use default for this option' */
#define OPT_STRDEFAULT        ((char *)0)
#define OPT_LANGDEFAULT        ((struct language *)0)

#define OPT_KANE        1        /* formulation */
#define OPT_ORDERN        2
#define OPT_EXP                3
#define OPT_EXP2        4

#define OPT_SINGLE        1        /* precision */
#define OPT_DOUBLE        2

/* char which introduces an option on cmdline */
#define OPT_CHAR        '-'        

typedef struct {
    int                formulation;        /* kane or ordern */
    int                precision;        /* single or double */
    int                verbose;        /* 0, 1, or 2 */
    int                breakup;        /* break up dyn file into multiple files? */
    int                magic_no;        /* magic number for security (0 if none) */
    struct language *lang;        /* C, FORTRAN */
    char         *prefix;        /* external symbol prefix */
    char        *infile;        /* Input File name */
    char        *basename;        /* Prefix for constructing output file names */
    int                gendyn;                /* generate Dynamics File? */
    int                geninfo;        /* generate Information File? */
    int                gensar;                /* generate Simplified Analysis File? */
    int                genlib;                /* generate Library File? */
    char        *dynname;        /* special name for Dynamics File */
    char        *infoname;        /* special name for Info File */
    char        *sarname;        /* special name for Simplified Analysis File */
    char        *libname;        /* special name for Library File */
    char        *progname;        /* e.g. `sdfast' */
} options_t;

extern options_t sdfast_opt;
extern options_t sdfast_optdef;

