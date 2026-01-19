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

#include "sdfast.h"
#include "sdfaprot.h"
#include "sderror.h"
#include "../calc/gencode.h"

#define Q(i) VAL1(q,i)
#define QN(i) VAL1(qn,i)

/*===========*/
/* Sq and Cq */
/*===========*/

static expr 
Sq(Index_t i)
{
    /* returns sin(q[i]) */

    return SINE(Q(i));
}

static expr 
Cq(Index_t i)
{
    /* returns cos(q[I]) */

    return COSINE(Q(i));
}

/*================*/
/* Delta and Levi */
/*================*/

static expr Delta(Index_t I,
                  Index_t J)
{
    /* returns Kronecker delta (always 1 or 0) */

    ASSERT(0 <= I && I < 3 && 0 <= J && J < 3, 1, "Delta");
    return I == J ? SCALAR_ONE() : SCALAR_ZERO();
}

static expr Levi(Index_t I,
                 Index_t J)
{
    /* returns Levi-Civita density (1,-1 or 0) */
    /* actually returns a vector [Levi(I,J,1),Levi(I,J,2), */
    /* Levi(I,J,3)].                                       */
    register Index_t K;
    expr X;

    ASSERT(0 <= I && I < 3 && 0 <= J && J < 3, 1, "Levi");
    X = NEW_VECX(cScalarVal);
    for (K = 0; K < 3; K++)
        SINDX(X, K, SC((double)((I - J) * (J - K) * (K - I) / 2)));
    return X;
}

/*=========*/
/* PIN_COS */
/*=========*/

expr PIN_COS(Index_t d,
             expr Lambda)
{
    /* Returns the cosine matrix for a pin joint (or one axis */
    /* of a U-joint) at DOF d (using q[d]), assuming          */
    /* that the hinge axis is given by the Lambda vector.     */
    expr F;
    register Index_t X, Y;

    Lambda = INUSE(Lambda);
    F = INUSE(NEW_MATX(cScalarVal));
    for (X = 0; X < 3; X++)
        for (Y = 0; Y < 3; Y++)
            SINDX2(F, X, Y,
              ADD(
                MUL(SUB(Delta(X, Y),
                        MUL(INDX(Lambda, X),
                            INDX(Lambda, Y))),
                    Cq(d)),
                SUB(MUL(INDX(Lambda, X),
                        INDX(Lambda, Y)),
                    MUL(DOT(Levi(X, Y), Lambda), Sq(d)))));
    Lambda = UNUSE(Lambda);
    return UNUSE(F);
}

/* BALL_COS
 *
 * Computes the direction cosine matrix for a ball joint at K.  Ball joints
 * are represented by four Euler parameters.  We must use the normalized
 * Euler parameters in the qn array rather than the unnormalized ones
 * in q; otherwise, we would produce an invalid direction cosine here.
 * The first three Euler parameters are
 * at QN(k), QN(k+1) and QN(k+2).  The last is at QN(s+bnum).  This
 * formula for the direction cosines comes from Kane's Spacecraft
 * Dynamics book, page 13.
 */
expr 
BALL_COS(Index_t k,
         Index_t bnum)
{
    expr bcos = NEW_MATX(cScalarVal);

#define e1        QN(k)
#define e2        QN(k+1)
#define e3        QN(k+2)
#define e4        QN(SysI.s+bnum)

    SINDX2(bcos,0,0,SUB(SCALAR_ONE(),
                        MUL(SC(2.0),ADD(MUL(e2,e2), MUL(e3,e3)))));
    SINDX2(bcos,0,1,MUL(SC(2.0),SUB(MUL(e1,e2), MUL(e3,e4))));
    SINDX2(bcos,0,2,MUL(SC(2.0),ADD(MUL(e3,e1), MUL(e2,e4))));
    SINDX2(bcos,1,0,MUL(SC(2.0),ADD(MUL(e1,e2), MUL(e3,e4))));

    SINDX2(bcos,1,1,SUB(SCALAR_ONE(),
                        MUL(SC(2.0),ADD(MUL(e3,e3), MUL(e1,e1)))));
    SINDX2(bcos,1,2,MUL(SC(2.0),SUB(MUL(e2,e3), MUL(e1,e4))));
    SINDX2(bcos,2,0,MUL(SC(2.0),SUB(MUL(e3,e1), MUL(e2,e4))));
    SINDX2(bcos,2,1,MUL(SC(2.0),ADD(MUL(e2,e3), MUL(e1,e4))));

    SINDX2(bcos,2,2,SUB(SCALAR_ONE(),
                        MUL(SC(2.0),ADD(MUL(e1,e1), MUL(e2,e2)))));

#undef e1
#undef e2
#undef e3
#undef e4

    return bcos;
}

/* DECOMPOSE_123
 * 
 * Decompose an expression representing a direction cosine matrix
 * into 1-2-3 Euler angles.  The answer is returned in new expressions
 * e1,e2,e3.  Each of these angles will be between -pi and pi on return,
 * and e2 will be between -pi/2 and pi/2.
 *
 * This algorithm is due to Erin Catto, and replace the old SD/FAST
 * version which was taken from Kane's Spacecraft Dynamics book, pg. 32.
 * (sherm 3/12/97)
 *
 *     Note: C13 here is the same as INDX2(dcx,0,2), etc.
 *
 *     if (|C13| >= 1-eps) {
 *         th2 = C13 > 0 ? pi/2 : -pi/2
 *         th1 = atan2(C32,C22)
 *         th3 = 0
 *     } else {
 *         th1 = atan2(-C23,C33)
 *         th2 = asin(C13)
 *         th3 = atan2(-C12,C11)
 *     }
 *     e1 = th1
 *     e2 = th2
 *     e3 = th3
 *
 * All of the above calculations are done symbolically if possible so
 * in many cases a lot of the above code will not show in the output.
 * On return, e1, e2 and e3 may reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed-in temps
 * are reused.
 *
 * The passed-in direction cosine expression is disposed when we're done.
 *
 * This is an Order(1) algorithm.  
 */
void DECOMPOSE_123(FILE *F,
              expr dcx,
              sym th1,
              sym th2,
              sym th3,
              expr *e1,
              expr *e2,
              expr *e3)
{
    expr pi,pi2,zero,cond;
    int  elsetoo,thentoo;

    dcx = INUSE(dcx);

    /* miscellaneous handy expressions */
    pi     = PERM(SC(4.0*atan(1.0)));
    pi2    = PERM(MUL(pi,SC(0.5)));
    zero   = SCALAR_ZERO();

    cond = GREATEROREQ(SUB(ABS(INDX2(dcx,0,2)), SC(1.0)), SC(-cVeryClose));
    if (IFTHEN(F, cond, &elsetoo)) {
        ASSIGN(th1, ATANG2(INDX2(dcx,2,1), INDX2(dcx,1,1)));
        ASSIGN(th2, QUES(GREATERTHAN(INDX2(dcx,0,2),zero),
                           pi2,
                           NEG(pi2)));
        ASSIGN(th3, zero);

        CLEANVAR(F, th1, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
        CLEANVAR(F, th2, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
        CLEANVAR(F, th3, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    } 
    if (IFELSE(F, cond, &thentoo)) {
        ASSIGN(th1, ATANG2(NEG(INDX2(dcx,1,2)), INDX2(dcx,2,2)));
        ASSIGN(th2, ASINE(INDX2(dcx,0,2)));
        ASSIGN(th3, ATANG2(NEG(INDX2(dcx,0,1)), INDX2(dcx,0,0)));

        CLEANVAR(F, th1, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
        CLEANVAR(F, th2, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
        CLEANVAR(F, th3, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);

    if (thentoo && elsetoo) {
        ASSIGN(th1, VREF(th1)); /* current VAL(th1), etc. are meaningless */ 
        ASSIGN(th2, VREF(th2));
        ASSIGN(th3, VREF(th3));
    }

    *e1 = VAL(th1);
    *e2 = VAL(th2);
    *e3 = VAL(th3);

    DISPOSE_EXPR(UNUSE(dcx));
}


/* OLD_DECOMPOSE_123
 * 
 * Decompose an expression representing a direction cosine matrix
 * into 1-2-3 Euler angles.  The answer is returned in new expressions
 * e1,e2,e3.  Each of these angles will be between -pi and pi on return,
 * and e2 will be between -pi/2 and pi/2.
 *
 * This algorithm is taken from Kane's Spacecraft Dynamics book, pg. 32,
 * with modifications to make it work properly on a finite-precision
 * computer.  Here's the implemented algorithm:
 *
 *     Note: C13 here is the same as INDX2(dcx,0,2), etc.
 *
 *     if (|C13| near 1) {
 *         th2 = C13 > 0 ? pi/2 : -pi/2
 *         angle = asin(C32)
 *         th1 = C22 >= 0 ? angle : pi - angle
 *         th3 = 0
 *     } else {
 *         th2 = asin(C13)
 *         quot = -C23/cos(th2)
 *         if (quot >  1) quot = 1     -- can occur for numerical reasons
 *         if (quot < -1) quot = -1
 *         angle = asin(quot)
 *         th1 = C33 >= 0 ? angle : pi - angle
 *         quot = -C12/cos(th2)
 *         if (quot >  1) quot = 1    
 *         if (quot < -1) quot = -1
 *         angle = asin(quot)
 *         th3 = C11 >= 0 ? angle : pi - angle
 *     }
 *     e1 = th1
 *     e2 = th2
 *     e3 = th3
 *         -- at this point 
 *         --   -pi/2 <= e2 <= pi/2
 *         --   -pi/2 <= e1 <= 3pi/2
 *         --   -pi/2 <= e3 <= 3pi/2
 *         -- we want all angles between -pi and pi, so:
 *     if (e1 > pi) e1 -= 2pi
 *     if (e3 > pi) e3 -= 2pi
 *
 * All of the above calculations are done symbolically if possible so
 * in many cases a lot of the above code will not show in the output.
 * On return, e1, e2 and e3 may reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed in temps
 * are reused.
 *
 * The passed-in direction cosine expression is disposed when we're done.
 *
 * This is an Order(1) algorithm.  
 */
void OLD_DECOMPOSE_123(FILE *F,
              expr dcx,
              sym quot,
              sym angle,
              sym th1,
              sym th2,
              sym th3,
              sym costh2,
              expr *e1,
              expr *e2,
              expr *e3)
{
    expr pi,pi2,twopi,one,negone,zero,cond;
    int  elsetoo,thentoo;

    dcx = INUSE(dcx);

    /* miscellaneous handy expressions */
    pi     = PERM(SC(4.0*atan(1.0)));
    pi2    = PERM(MUL(pi,SC(0.5)));
    twopi  = PERM(MUL(pi,SC(2.0)));
    one    = SCALAR_ONE();
    negone = PERM(SC(-1.0));
    zero   = SCALAR_ZERO();

    cond = NEARTO(ABS(INDX2(dcx,0,2)), one, cNearZero);
    if (IFTHEN(F, cond, &elsetoo)) {
        ASSIGN(th2, QUES(GREATERTHAN(INDX2(dcx,0,2),zero),
                           pi2,
                           NEG(pi2)));
        CLEANVAR(F, th2, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(INDX2(dcx, 2, 1), one), one,
                 QUES(LESSTHAN(INDX2(dcx, 2, 1), negone), negone,
                                                          INDX2(dcx, 2, 1)))));
        ASSIGN(th1, QUES(GREATEROREQ(INDX2(dcx,1,1), zero),
                         VAL(angle),
                         SUB(pi,VAL(angle))));
        CLEANVAR(F, th1, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN(th3, zero);
        CLEANVAR(F, th3, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    } 
    if (IFELSE(F, cond, &thentoo)) {
        ASSIGN(th2, 
            ASINE(QUES(GREATERTHAN(INDX2(dcx, 0, 2), one), one,
                 QUES(LESSTHAN(INDX2(dcx, 0, 2), negone), negone,
                                                          INDX2(dcx, 0, 2)))));
        CLEANVAR(F, th2, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN_CLN(F, costh2, COSINE(VAL(th2)));
        ASSIGN_CLN(F, quot, DVD(NEG(INDX2(dcx,1,2)), VAL(costh2)));
        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(VAL(quot), one), one,
                 QUES(LESSTHAN(VAL(quot), negone), negone,
                                                   VAL(quot)))));
        ASSIGN(th1, QUES(GREATEROREQ(INDX2(dcx,2,2),zero),
                                VAL(angle),
                                SUB(pi, VAL(angle))));
        /* use CL_FLUSHNONCONST to prevent future reference to angle 
           and quot through th1 because they are about to be reused */
        CLEANVAR(F, th1, thentoo ? CL_FLUSHALL : CL_FLUSHNONCONST, CL_FORGET);

        ASSIGN_CLN(F, quot, DVD(NEG(INDX2(dcx,0,1)), VAL(costh2)));
        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(VAL(quot), one), one,
                 QUES(LESSTHAN(VAL(quot), negone), negone,
                                                   VAL(quot)))));
        ASSIGN(th3, QUES(GREATEROREQ(INDX2(dcx,0,0),zero),
                         VAL(angle),
                                SUB(pi, VAL(angle))));
        CLEANVAR(F, th3, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);

    if (thentoo && elsetoo) {
        ASSIGN(th1, VREF(th1)); /* current VAL(th1), etc. are meaningless */ 
        ASSIGN(th2, VREF(th2));
        ASSIGN(th3, VREF(th3));
    }

    /* Bring th1 and th3 into range if necessary. */

    *e1 = QUES(GREATERTHAN(VAL(th1), pi), 
               SUB(VAL(th1), twopi), VAL(th1));
    *e3 = QUES(GREATERTHAN(VAL(th3), pi), 
               SUB(VAL(th3), twopi), VAL(th3));
    *e2 = VAL(th2);

    DISPOSE_EXPR(UNUSE(dcx));
}

/* DECOMPOSE_UJOINT
 * 
 * Decompose an expression representing a direction cosine matrix
 * into *almost* 1-3-1 Euler angles (at the last minute, we subtract
 * pi from theta3.)  The answer is returned in new expressions
 * e1,e2,e3.  Each of these angles will be between -pi and pi on return,
 * and e2 will be between 0 and pi.
 *
 * This algorithm is similar to the one used above for DECOMPOSE_123,
 * but was derived by Dan Rosenthal rather than Prof. Kane.  
 * Here's the implemented algorithm:
 *
 *     Note: C13 here is the same as INDX2(dcx,0,2), etc.
 *
 *     if (|C11| near 1) {
 *         th2 = C11 > 0 ? 0 : pi
 *         angle = asin(C32)
 *         th3 = C33 >= 0 ? angle : pi - angle
 *         th1 = 0
 *     } else {
 *         th2 = acos(C11)
 *         quot = C31/sin(th2)
 *         if (quot >  1) quot = 1     -- can occur for numerical reasons
 *         if (quot < -1) quot = -1
 *         angle = asin(quot)
 *         th1 = C21 >= 0 ? angle : pi - angle
 *         quot = C13/sin(th2)
 *         if (quot >  1) quot = 1    
 *         if (quot < -1) quot = -1
 *         angle = asin(quot)
 *         th3 = C12 <= 0 ? angle - pi : -angle    -- 1-3-1 would add pi to this
 *     }
 *     e1 = th1
 *     e2 = th2
 *     e3 = th3
 *         -- at this point 
 *         --       0 <= e2 <= pi
 *         --   -pi/2 <= e1 <= 3pi/2
 *         --   -3pi/2 <= e3 <= pi/2   (1-3-1 would add pi here)
 *         -- we want all angles between -pi and pi, so:
 *     if (e1 > pi) e1 -= 2pi
 *     if (e3 < -pi) e3 += 2pi         -- would be same as e1 for 1-3-1
 *
 * All of the above calculations are done symbolically if possible so
 * in many cases a lot of the above code will not show in the output.
 * On return, e1, e2 and e3 may reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed in temps
 * are reused.
 *
 * The passed-in direction cosine expression is disposed when we're done.
 *
 * This is an Order(1) algorithm.  
 */
void DECOMPOSE_UJOINT(
                 FILE *F,
                 expr dcx,
                 sym quot,
                 sym angle,
                 sym th1,
                 sym th2,
                 sym th3,
                 sym sinth2,
                 expr *e1,
                 expr *e2,
                 expr *e3)
{
    expr pi,twopi,one,negone,zero,cond;
    int  elsetoo,thentoo;

    dcx = INUSE(dcx);

    /* miscellaneous handy expressions */
    pi     = PERM(SC(4.0*atan(1.0)));
    twopi  = PERM(MUL(pi,SC(2.0)));
    one    = SCALAR_ONE();
    negone = PERM(SC(-1.0));
    zero   = SCALAR_ZERO();

    cond = NEARTO(ABS(INDX2(dcx,0,0)), one, cNearZero);
    if (IFTHEN(F, cond, &elsetoo)) {
        ASSIGN(th2, QUES(GREATERTHAN(INDX2(dcx,0,0),zero),
                           zero,
                           pi));
        CLEANVAR(F, th2, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(INDX2(dcx, 2, 1), one), one,
                 QUES(LESSTHAN(INDX2(dcx, 2, 1), negone), negone,
                                                          INDX2(dcx, 2, 1)))));
        ASSIGN(th3, QUES(GREATEROREQ(INDX2(dcx,2,2), zero),
                         VAL(angle),
                         SUB(pi,VAL(angle))));
        CLEANVAR(F, th3, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN(th1, zero);
        CLEANVAR(F, th1, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    } 
    if (IFELSE(F, cond, &thentoo)) {
        ASSIGN(th2, 
            ACOSN(QUES(GREATERTHAN(INDX2(dcx, 0, 0), one), one,
                 QUES(LESSTHAN(INDX2(dcx, 0, 0), negone), negone,
                                                          INDX2(dcx, 0, 0)))));
        CLEANVAR(F, th2, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);

        ASSIGN_CLN(F, sinth2, SINE(VAL(th2)));
        ASSIGN_CLN(F, quot, DVD(INDX2(dcx,2,0), VAL(sinth2)));
        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(VAL(quot), one), one,
                 QUES(LESSTHAN(VAL(quot), negone), negone,
                                                   VAL(quot)))));
        ASSIGN(th1, QUES(GREATEROREQ(INDX2(dcx,1,0),zero),
                                VAL(angle),
                                SUB(pi, VAL(angle))));
        /* use CL_FLUSHNONCONST to prevent future reference to angle 
           and quot through th1 because they are about to be reused */
        CLEANVAR(F, th1, thentoo ? CL_FLUSHALL : CL_FLUSHNONCONST, CL_FORGET);

        ASSIGN_CLN(F, quot, DVD(INDX2(dcx,0,2), VAL(sinth2)));
        ASSIGN_CLN(F, angle, 
            ASINE(QUES(GREATERTHAN(VAL(quot), one), one,
                 QUES(LESSTHAN(VAL(quot), negone), negone,
                                                   VAL(quot)))));
        ASSIGN(th3, QUES(LESSOREQ(INDX2(dcx,0,1),zero),
                         SUB(VAL(angle), pi),
                                NEG(VAL(angle))));
        CLEANVAR(F, th3, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);

    if (thentoo && elsetoo) {
        ASSIGN(th1, VREF(th1)); /* current VAL(th1), etc. are meaningless */ 
        ASSIGN(th2, VREF(th2));
        ASSIGN(th3, VREF(th3));
    }

    /* Bring th1 and th3 into range if necessary. */

    *e1 = QUES(GREATERTHAN(VAL(th1), pi), 
               SUB(VAL(th1), twopi), VAL(th1));
    *e3 = QUES(LESSTHAN(VAL(th3), NEG(pi)), 
               ADD(VAL(th3), twopi), VAL(th3));
    *e2 = VAL(th2);

    DISPOSE_EXPR(UNUSE(dcx));
}

/* DECOMPOSE_GIMBAL
 * 
 * Decompose an expression representing a direction cosine matrix
 * into Gimbal angles.  The answer is returned in new expressions
 * e1,e2,e3.  Each of these angles will be between -pi and pi on return,
 * with e2 between -pi/2 and pi/2.
 *
 * The algorithm is as follows:
 * 
 *     (Note: vt1 = opin * Coi, vt10 = oref * Coi, iperp = ipin1 X ipin2)
 * 
 *     tmpv = vt1 X ipin1
 *     t = sqrt(tmpv * tmpv)
 *     if (t near 0) 
 *         pin2x = vt10
 *     else
 *         pin2x = ghand * tmpv / t
 *
 *     c = pin2x * ipin2
 *     if (c > 1) c = 1
 *     else if (c < -1) c = -1
 *     s = pin2x * iperp
 *     e1 = acos(c)
 *     if (s < 0) e1 = -e1
 *
 *     irefx = c*iref + s*(ipin1 X iref) + (1-c)*(iref*ipin1)*ipin1
 *     s = vt1 * (pin2x X irefx)
 *     if (s > 1) s = 1
 *     else if (s < -1) s = -1
 *     e2 = asin(s)
 *
 *     c = vt10 * pin2x
 *     if (c > 1) c = 1
 *     else if (c < -1) c = -1
 *     s = vt10 * (vt1 X pin2x)
 *     e3 = acos(c)
 *     if (s < 0) e3 = -e3
 *
 *     -- at this point -pi <= e1,e2,e3 <= pi so no adjustment needed
 *
 * All of the above calculations are done symbolically if possible so
 * in some cases a lot of the above code will not show in the output.
 * Pin2x is returned set appropriately.  On return, pin2x, e1, e2 and e3 may 
 * reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed-in temps
 * are reused.
 *
 * This is an Order(1) algorithm.  
 */
void DECOMPOSE_GIMBAL(FILE *F,
                 int lj, /* the loop joint number */
                 sym t,
                 sym  c,
                 sym  s,
                 sym  th1,
                 sym  th2,
                 sym  th3,
                 sym  tmpv,
                 sym  irefx,
                 sym  pin2x,
                 expr *e1,
                 expr *e2,
                 expr *e3)
{
    expr cond,zero,one,negone;
    int thentoo,elsetoo;

    zero = SCALAR_ZERO();
    one = SCALAR_ONE();
    negone = PERM(SC(-1.0));

    ASSIGN_CLN(F, tmpv, CROSS(VAL1(vt1,lj),VAL1(ipin,lj)));
    ASSIGN_CLN(F, t, SQRTT(DOT(VAL(tmpv), VAL(tmpv))));

    cond = NEARTO(VAL(t), zero, cNearZero);
    if (IFTHEN(F, cond, &elsetoo)) {
        ASSIGN(pin2x, VAL1(vt10,lj));
        CLEANVAR(F, pin2x, elsetoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    }
    if (IFELSE(F, cond, &thentoo)) {
        ASSIGN(pin2x, MUL(VAL1(ghand,lj),DVD(VAL(tmpv),VAL(t))));
        CLEANVAR(F, pin2x, thentoo ? CL_FLUSHALL : CL_FLUSHCOMPLEX, CL_FORGET);
    }
    IFEND(F, cond);
    DISPOSE_EXPR(cond);
    if (thentoo && elsetoo) 
        ASSIGN(pin2x, VREF(pin2x));

    ASSIGN_CLN(F, c, DOT(VAL(pin2x), VAL1(ipin2,lj)));
    ASSIGN_CLN(F, c, QUES(GREATERTHAN(VAL(c),one), one,
                          QUES(LESSTHAN(VAL(c), negone), negone,
                               VAL(c))));
    ASSIGN_CLN(F, s, DOT(VAL(pin2x), VAL1(iperp, lj)));
    /* flush non-const so we can reuse c and s */
    ASSIGN(th1, QUES(LESSTHAN(VAL(s), zero), NEG(ACOSN(VAL(c))), ACOSN(VAL(c))));
    CLEANVAR(F, th1, CL_FLUSHNONCONST, CL_FORGET);

    ASSIGN(irefx, ADD(MUL(VAL(c), VAL1(iref,lj)),
                  ADD(MUL(VAL(s), CROSS(VAL1(ipin,lj),VAL1(iref,lj))),
                      MUL(SUB(SCALAR_ONE(),VAL(c)),
                          MUL(DOT(VAL1(iref,lj),VAL1(ipin,lj)),
                              VAL1(ipin,lj))))));
    CLEANVAR(F, irefx, CL_FLUSHNONCONST, CL_FORGET); /* don't dangle c & s */
    ASSIGN_CLN(F, s, DOT(VAL1(vt1,lj), CROSS(VAL(pin2x),VAL(irefx))));
    ASSIGN_CLN(F, s, QUES(GREATERTHAN(VAL(s),one), one,
                          QUES(LESSTHAN(VAL(s), negone), negone,
                               VAL(s))));
    ASSIGN(th2, ASINE(VAL(s)));
    CLEANVAR(F, th2, CL_FLUSHNONCONST, CL_FORGET);

    ASSIGN_CLN(F, c, DOT(VAL1(vt10,lj), VAL(pin2x)));
    ASSIGN_CLN(F, c, QUES(GREATERTHAN(VAL(c),one), one,
                          QUES(LESSTHAN(VAL(c), negone), negone,
                               VAL(c))));
    ASSIGN_CLN(F, s, DOT(VAL1(vt10,lj), CROSS(VAL1(vt1,lj),VAL(pin2x))));
    ASSIGN(th3, QUES(LESSTHAN(VAL(s), zero), NEG(ACOSN(VAL(c))), ACOSN(VAL(c))));
    CLEANVAR(F, th3, CL_FLUSHCOMPLEX, CL_FORGET);

    *e1 = VAL(th1);
    *e2 = VAL(th2);
    *e3 = VAL(th3);
}

/* GIMBAL_DOT
 *
 * Compute expressions for the gimbal velocities for loop gimbal joint lj.
 *
 *    seul1,ceul1 = sin and cos of first gimbal angle
 *    vt1 = opin * Coi
 *
 *    wab = wk[o]*Coi - wk[i] 
 *    tmpv2 = pin2x X vt1
 *    tmp1 = ipin * tmpv2
 *    tmp2 = wab * tmpv2
 *    if (tmp1 near 0)
 *       e1d = wab * p1
 *    else
 *       e1d = tmp2/tmp1
 *
 *    e2d = wab * pin2x
 *
 *    tmpv3 = ipin X pin2x
 *    tmp3 = vt1 * tmpv3
 *    if (tmp3 near 0)
 *       e3d = 0
 *    else
 *       e3d = wab*tmpv3/tmp3
 *    
 * All of the above calculations are done symbolically if possible so
 * in some cases a lot of the above code will not show in the output.
 * On return, e1d, e2d and e3d may reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed-in temps
 * are reused.
 *
 * This is an Order(1) algorithm.  
 */
void GIMBAL_DOT(FILE *F,
           int lj,
           sym pin2x,
           sym tmp1,
           sym tmp2,
           sym tmp3,
           sym wab,
           sym tmpv2,
           sym tmpv3,
           expr *e1d,
           expr *e2d,
           expr *e3d)
{
    int inb,outb,ip,op;

    inb = SysI.LoopConst[lj].jnt.InbBody;
    outb = SysI.LoopConst[lj].jnt.OutbBody;
    /* pseudo-body numbers for inb and outb */
    if (!rbod_is_gnd(inb)) 
        ip = SysI.LastDOF[inb];        
    if (!rbod_is_gnd(outb)) 
        op = SysI.LastDOF[outb];

    ASSIGN_CLN(F, wab, 
        SUB(MATMUL(rbod_is_gnd(outb) ? VECTOR_ZERO() : VAL1(wk,op),
                   TRANSPOSE(VAL1(Cio,lj))),
            rbod_is_gnd(inb) ? VECTOR_ZERO() : VAL1(wk,ip)));
    ASSIGN_CLN(F, tmpv2, CROSS(VAL1(pin2x,lj),VAL1(vt1,lj)));
    ASSIGN_CLN(F, tmp1, DOT(VAL1(ipin,lj),VAL(tmpv2)));
    ASSIGN_CLN(F, tmp2, DOT(VAL(wab),VAL(tmpv2)));
    *e1d = QUESDVD(NEARTO(VAL(tmp1), SCALAR_ZERO(), cNearZero),
                   DOT(VAL(wab),VAL1(ipin,lj)),
                   VAL(tmp2),
                   VAL(tmp1));

    *e2d = DOT(VAL(wab),VAL1(pin2x,lj));

    ASSIGN_CLN(F, tmpv3, CROSS(VAL1(ipin,lj),VAL1(pin2x,lj)));
    ASSIGN_CLN(F, tmp3, DOT(VAL1(vt1,lj),VAL(tmpv3)));
    *e3d = QUESDVD(NEARTO(VAL(tmp3), SCALAR_ZERO(), cNearZero),
                   SCALAR_ZERO(),
                   DOT(VAL(wab),VAL(tmpv3)),
                   VAL(tmp3));
}

/* GIMBAL_DOTDOT
 *
 * Compute expressions for the gimbal accelerations for loop gimbal joint lj.
 *
 *    seul1,ceul1 = sin and cos of first gimbal angle
 *    opa = vt1 = opin * Coi
 *    opd = vt4 = (wk[o] X opin) * Coi
 *    p1d = vt13 = wk[i] X ipin
 *
 *    tmpv1 = pin2x X vt1
 *    tmpv2 = ipin X pin2x
 *    p2d = wk[i] X ipin2
 *    p2xd = p2d*ceul1 + (iperp*ceul1 - ipin2*seul1)*eul1dot
 *           + (vt13 X ipin2 + ipin X p2d)*seul1
 *    tmpv5 = onk[o]*Coi - (onk[i] + eul1dot*vt13 + eul2dot*p2xd + eul3dot*vt4)
 *    tmp1 = ipin * tmpv1
 *    if (tmp1 near 0)
 *       e1dd = tmpv5 * ipin
 *    else
 *       e1dd = tmpv5*tmpv1/tmp1
 *
 *    e2dd = tmpv5 * pin2x
 *
 *    tmp2 = vt1 * tmpv2
 *    if (tmp2 near 0)
 *       e3dd = 0
 *    else
 *       e3dd = tmpv5*tmpv2/tmp2
 *    
 * All of the above calculations are done symbolically if possible so
 * in some cases a lot of the above code will not show in the output.
 * On return, e1d, e2d and e3d may reference some temporary symbols, so they
 * MUST be CL_FLUSHNONCONST'ed by the caller before any of the passed-in temps
 * are reused.
 *
 * This is an Order(1) algorithm.  
 */
void GIMBAL_DOTDOT(FILE *F,
              int lj,
              sym pin2x,
              sym tmp1,
              sym tmp2,
              sym tmpv1,
              sym tmpv2,
              sym p2d,
              sym p2xd,
              sym tmpv5,
              expr *e1dd,
              expr *e2dd,
              expr *e3dd)
{
    int inb,outb,ip,op;

    inb = SysI.LoopConst[lj].jnt.InbBody;
    outb = SysI.LoopConst[lj].jnt.OutbBody;
    /* get pseudo-body numbers for inb and outb */
    if (!rbod_is_gnd(inb)) 
        ip = SysI.LastDOF[inb];        
    if (!rbod_is_gnd(outb)) 
        op = SysI.LastDOF[outb];

    ASSIGN_CLN(F, tmpv1, CROSS(VAL1(pin2x,lj),VAL1(vt1,lj)));
    ASSIGN_CLN(F, tmpv2, CROSS(VAL1(ipin,lj),VAL1(pin2x,lj)));
    ASSIGN_CLN(F, p2d, 
        CROSS(rbod_is_gnd(inb) ? VECTOR_ZERO() : VAL1(wk,ip),
              VAL1(ipin2,lj)));
    ASSIGN_CLN(F, p2xd,
        ADD(MUL(VAL(p2d),VAL1(ceul1,lj)),
        ADD(MUL(SUB(MUL(VAL1(iperp,lj),VAL1(ceul1,lj)),
                    MUL(VAL1(ipin2,lj),VAL1(seul1,lj))),
                VAL1(eul1dot,lj)),
            MUL(ADD(CROSS(VAL1(vt13,lj),VAL1(ipin2,lj)),
                    CROSS(VAL1(ipin,lj),VAL(p2d))),
                VAL1(seul1,lj)))));
    ASSIGN_CLN(F, tmpv5,
        SUB(MATMUL(rbod_is_gnd(outb) ? VECTOR_ZERO() : VAL1(onk,op),
                   TRANSPOSE(VAL1(Cio,lj))),
            ADD(rbod_is_gnd(inb) ? VECTOR_ZERO() : VAL1(onk,ip),
            ADD(MUL(VAL1(eul1dot,lj),VAL1(vt13,lj)),
            ADD(MUL(VAL1(eul2dot,lj),VAL(p2xd)),
                MUL(VAL1(eul3dot,lj),VAL1(vt4,lj)))))));
    ASSIGN_CLN(F, tmp1, DOT(VAL1(ipin,lj),VAL(tmpv1)));
    *e1dd = QUESDVD(NEARTO(VAL(tmp1), SCALAR_ZERO(), cNearZero),
                    DOT(VAL(tmpv5),VAL1(ipin,lj)),
                    DOT(VAL(tmpv5),VAL(tmpv1)),
                    VAL(tmp1));

    *e2dd = DOT(VAL(tmpv5),VAL1(pin2x,lj));

    ASSIGN_CLN(F, tmp2, DOT(VAL1(vt1,lj),VAL(tmpv2)));
    *e3dd = QUESDVD(NEARTO(VAL(tmp2), SCALAR_ZERO(), cNearZero),
                    SCALAR_ZERO(),
                    DOT(VAL(tmpv5),VAL(tmpv2)),
                    VAL(tmp2));
}

/* DECOMPOSE_QUAT
 *
 * Decompose a direction cosine into quaternions, specifically
 * Euler parameters.
 *
 * The algorithm is from R.A.Spurrier, "Comment on 'Singularity-Free
 * Extraction of a Quaternion from a Direction-Cosine Matrix'", 
 * J. Spacecraft 1978 Vol. 15, No. 4, pg. 255.  (Please note that 
 * Kane's Spacecraft Dynamics book (pg. 13) doesn't even come close 
 * to being something you can compute reliably.)
 *
 * NOTE: the original Spurrier algorithm does not produce
 * normalized quaternions unless the input direction cosine
 * matrix is perfectly normalized.  I modified the algorithm
 * to include a normalization step at the end.  Then I shuffled
 * the terms around and it turned out I could fold all the
 * expensive stuff (the divide and square root) into the 
 * normalization step at the end.  So we get normalized quaternions
 * here at the same cost as Spurrier takes to get unnormalized ones.
 * (sherm 3/11/97)
 *
 * Here is a Matlab implementation of the method:
 *
 *  -------------------------------------------------------------
 *  function q = fastdc2quat(C)
 *  
 *  tr = trace(C);
 *  [mx, ix] = max([C(1,1) C(2,2) C(3,3) tr]);
 *  
 *  if ix == 4 
 *      q(1) = C(3,2) - C(2,3);
 *      q(2) = C(1,3) - C(3,1);
 *      q(3) = C(2,1) - C(1,2);
 *      q(4) = 1 + tr;
 *  else
 *      if ix == 1; i1 = 3; else i1 = ix-1; end 
 *      if i1 == 1; i2 = 3; else i2 = i1-1; end
 *  
 *      q( 4) = C(i1,i2) - C(i2,i1);
 *      q(i1) = C(i1,ix) + C(ix,i1);
 *      q(i2) = C(i2,ix) + C(ix,i2);
 *      q(ix) = 2*mx + (1 - tr);
 *  end
 *  
 *  % Note: this last step is not optional.  The q's
 *  % won't be quaternions, not even approximately,
 *  % until this statement is executed.
 *  q = q/norm(q);
 *  -------------------------------------------------------------
 *
 * In the implementation here we lay out all four cases so
 * that we can hope to perform some symbolic simplifications:
 *
 *  trace = C11+C22+C33
 *
 *  if (trace > C11 && trace > C22 && trace > C33)
 *      e1 = C32 - C23                        -- trace is largest
 *      e2 = C13 - C31
 *      e3 = C21 - C12
 *      e4 = 1 + trace
 *  else if (C11 > C22 && C11 > C33)
 *      e1 = 2*C11 + (1 - trace)        -- C11 is largest
 *      e2 = C21 + C12
 *      e3 = C31 + C13
 *      e4 = C32 - C23
 *  else if (C22 > C33)
 *      e1 = C12 + C21                        -- C22 is largest
 *      e2 = 2*C22 + (1 - trace)
 *      e3 = C32 + C23
 *      e4 = C13 - C31
 *  else -- C33 is largest
 *      e1 = C13 + C31                        -- C33 is largest
 *      e2 = C23 + C32
 *      e3 = 2*C33 + (1 - trace)
 *      e4 = C21 - C12
 *
 *  tmp = 1/sqrt(e1**2 + e2**2 + e3**2 + e4**2)
 *  e1 *= tmp
 *  e2 *= tmp
 *  e3 *= tmp
 *  e4 *= tmp
 *
 * The actual code performs the above computation but avoids extra
 * temp variables and indexing.
 *
 * The passed-in direction cosine expression is disposed when we're done.
 *
 * This is an Order(1) computation.
 */
void DECOMPOSE_QUAT(FILE *F,
               expr dcx,
               sym tmp,
               sym  tmp1,
               sym  tmp2,
               sym  tmp3,
               sym  tmp4,
               expr *e1,
               expr *e2,
               expr *e3,
               expr *e4)
{
    expr zero,one,cond,cond2,cond3;
    int  elsetoo,thentoo,else2too,then2too,else3too,then3too,casecnt;

    dcx = INUSE(dcx);
    one = SCALAR_ONE();
    zero = SCALAR_ZERO();

    ASSIGN_CLN(F, tmp, ADD(INDX2(dcx, 0, 0),                /* tmp = trace(dcx) */
                       ADD(INDX2(dcx, 1, 1),
                           INDX2(dcx, 2, 2))));

    /* Count the number of cases which will be present in the generated
     * code.  If this ends up 1 there will be no if statements in the
     * generated code, except those generated by QUES expressions.
     */
    casecnt = 0;

    cond = INUSE(AND(GREATEROREQ(VAL(tmp),INDX2(dcx, 0, 0)),
                 AND(GREATEROREQ(VAL(tmp),INDX2(dcx, 1, 1)),
                     GREATEROREQ(VAL(tmp),INDX2(dcx, 2, 2)))));
    if (IFTHEN(F, cond, &elsetoo)) {
        /* trace is the biggest */
        casecnt++;
        ASSIGN_CLN(F, tmp1, SUB(INDX2(dcx, 2, 1), INDX2(dcx, 1, 2)));
        ASSIGN_CLN(F, tmp2, SUB(INDX2(dcx, 0, 2), INDX2(dcx, 2, 0)));
        ASSIGN_CLN(F, tmp3, SUB(INDX2(dcx, 1, 0), INDX2(dcx, 0, 1)));
        ASSIGN_CLN(F, tmp4, ADD(one, VAL(tmp)));

        if (elsetoo) {
            CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
            CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
            CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
            CLEANVAR(F, tmp4, CL_FLUSHALL, CL_FORGET);
        }
    }
    if (IFELSE(F, cond, &thentoo)) {
        /* at least one diagonal is greater than trace */
        cond2 = INUSE(AND(GREATEROREQ(INDX2(dcx, 0, 0),INDX2(dcx, 1, 1)),
                          GREATEROREQ(INDX2(dcx, 0, 0),INDX2(dcx, 2, 2))));
        if (IFTHEN(F, cond2, &else2too)) {
            /* C11 is the biggest */
            casecnt++;
            ASSIGN_CLN(F, tmp1, ADD(MUL(SC(2.0), INDX2(dcx, 0, 0)),
                                    SUB(one, VAL(tmp))));
            ASSIGN_CLN(F, tmp2, ADD(INDX2(dcx, 1, 0), INDX2(dcx, 0, 1)));
            ASSIGN_CLN(F, tmp3, ADD(INDX2(dcx, 2, 0), INDX2(dcx, 0, 2)));
            ASSIGN_CLN(F, tmp4, SUB(INDX2(dcx, 2, 1), INDX2(dcx, 1, 2)));

            if (casecnt > 1 || else2too) {
                CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp4, CL_FLUSHALL, CL_FORGET);
            }
        }
        if (IFELSE(F, cond2, &then2too)) {
            /* neither trace nor C11 is the biggest; must be C22 or C33 */
            cond3 = INUSE(GREATEROREQ(INDX2(dcx, 1, 1), INDX2(dcx, 2, 2)));
            if (IFTHEN(F, cond3, &else3too)) {
                /* C22 is the biggest */
                casecnt++;
                ASSIGN_CLN(F, tmp1, ADD(INDX2(dcx, 0, 1), INDX2(dcx, 1, 0)));
                ASSIGN_CLN(F, tmp2, ADD(MUL(SC(2.0), INDX2(dcx, 1, 1)),
                                        SUB(one, VAL(tmp))));
                ASSIGN_CLN(F, tmp3, ADD(INDX2(dcx, 2, 1), INDX2(dcx, 1, 2)));
                ASSIGN_CLN(F, tmp4, SUB(INDX2(dcx, 0, 2), INDX2(dcx, 2, 0)));

                if (casecnt > 1 || else3too) {
                    CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp4, CL_FLUSHALL, CL_FORGET);
                }
            }
            if (IFELSE(F, cond3, &then3too)) {
                /* C33 is the biggest */
                casecnt++;
                ASSIGN_CLN(F, tmp1, ADD(INDX2(dcx, 0, 2), INDX2(dcx, 2, 0)));
                ASSIGN_CLN(F, tmp2, ADD(INDX2(dcx, 1, 2), INDX2(dcx, 2, 1)));
                ASSIGN_CLN(F, tmp3, ADD(MUL(SC(2.0), INDX2(dcx, 2, 2)),
                                        SUB(one, VAL(tmp))));
                ASSIGN_CLN(F, tmp4, SUB(INDX2(dcx, 1, 0), INDX2(dcx, 0, 1)));

                if (casecnt > 1) {
                    CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
                    CLEANVAR(F, tmp4, CL_FLUSHALL, CL_FORGET);
                }
            }
            IFEND(F, cond3);
            DISPOSE_EXPR(UNUSE(cond3));
        }
        IFEND(F, cond2);
        DISPOSE_EXPR(UNUSE(cond2));
    }
    IFEND(F, cond);
    DISPOSE_EXPR(UNUSE(cond));

    /* If there were multiple cases generated, then the current VAL(tmp1), etc.
     * are meaningless since the runtime value could be different.  In that
     * they should only be referenced by name.  
     *
     * Even if there was a single case, flush everything out to eliminate
     * references to `tmp' which we want to re-use.
     */
    if (casecnt > 1) {
        ASSIGN(tmp1, VREF(tmp1));
        ASSIGN(tmp2, VREF(tmp2));
        ASSIGN(tmp3, VREF(tmp3));
        ASSIGN(tmp4, VREF(tmp4));
    } else {
        CLEANVAR(F, tmp1, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, tmp2, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, tmp3, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, tmp4, CL_FLUSHNONCONST, CL_FORGET);
    }

    /* Now normalize and get out. */
    ASSIGN_CLN(F, tmp, DVD(one, SQRTT(ADD(MUL(VAL(tmp1),VAL(tmp1)),
                                     ADD(MUL(VAL(tmp2),VAL(tmp2)),
                                     ADD(MUL(VAL(tmp3),VAL(tmp3)),
                                         MUL(VAL(tmp4),VAL(tmp4))))))));
    *e1 = MUL(VAL(tmp1),VAL(tmp));
    *e2 = MUL(VAL(tmp2),VAL(tmp));
    *e3 = MUL(VAL(tmp3),VAL(tmp));
    *e4 = MUL(VAL(tmp4),VAL(tmp));

    DISPOSE_EXPR(UNUSE(dcx));
}

#ifdef NOTDEF
/* OLD_DECOMPOSE_QUAT
 *
 * Decompose a direction cosine into quaternions, specifically
 * Euler parameters.
 *
 * The algorithm is from J. Spacecraft 1976 Vol. 13, No. 12, pp. 754-5 
 * "Singularity-free Extraction of a Quaternion from a Direction-Cosine 
 * Matrix" by A.R. Klumpp.  (Please note that Kane's Spacecraft Dynamics
 * book (pg. 13) doesn't even come close to being something you can 
 * compute reliably.)
 *
 *
 *      trace = C11+C22+C33
 *        trp1  = (trace+1)/4
 *        tr1m  = (1-trace)/4
 *
 *      e4 = sqrt(trp1)                 -- we choose e4 arbitrarily >= 0
 *
 *      for i=1,2,3
 *          ei = sqrt(Cii/2 + tr1m)     -- compute magnitudes for e1,2,3
 *
 *      m = index of the maximum of e1,e2,e3
 *      j,k = the other two indices
 *
 *      if (Ckj < Cjk) em = -em         -- now compute the signs
 *      if (em*(Cjm+Cmj) < 0) ej = -ej
 *      if (em*(Ckm+Cmk) < 0) ek = -ek
 *
 *      -- normalize
 *
 *      tmp = 1/sqrt(e1**2 + e2**2 + e3**2 + e4**2)
 *      e1 *= tmp
 *      e2 *= tmp
 *      e3 *= tmp
 *      e4 *= tmp
 *
 * In the above, we must be very careful to avoid taking the sqrt of 
 * negative numbers.  For numerical reasons, elements of the direction
 * cosine might be slightly < -1 (e.g. -(1+1e-16)) in which case expressions
 * like C11+1 might be less than zero.  Since they can't be *much* less,
 * however, we can just make them zero.
 *
 * The actual code performs the above computation but avoids extra
 * temp variables and indexing.
 *
 * The passed-in direction cosine expression is disposed when we're done.
 *
 * This is an Order(1) computation.
 */
void OLD_DECOMPOSE_QUAT(FILE *F,
               expr dcx,
               sym tmp,
               sym  tmp1,
               sym  tmp2,
               sym  tmp3,
               sym  tmp4,
               expr *e1,
               expr *e2,
               expr *e3,
               expr *e4)
{
    expr zero,one,cond,cond2;
    int  elsetoo,thentoo,else2too,then2too,casecnt;

    dcx = INUSE(dcx);
    one = SCALAR_ONE();
    zero = SCALAR_ZERO();

    ASSIGN_CLN(F, tmp, MUL(SC(0.25),SUB(one, ADD(INDX2(dcx, 0, 0),
                                              ADD(INDX2(dcx, 1, 1),
                                                  INDX2(dcx, 2, 2))))));

    ASSIGN_CLN(F,tmp4, SUB(SC(0.5), VAL(tmp)));
    ASSIGN(tmp4, QUES(LESSOREQ(VAL(tmp4), zero), zero, SQRTT(VAL(tmp4))));
    CLEANVAR(F, tmp4, CL_FLUSHNONCONST, CL_FORGET);

    ASSIGN_CLN(F,tmp1, ADD(VAL(tmp), MUL(SC(0.5), INDX2(dcx,0,0))));
    ASSIGN_CLN(F,tmp1, QUES(LESSOREQ(VAL(tmp1), zero), zero, SQRTT(VAL(tmp1))));

    ASSIGN_CLN(F,tmp2, ADD(VAL(tmp), MUL(SC(0.5), INDX2(dcx,1,1))));
    ASSIGN_CLN(F,tmp2, QUES(LESSOREQ(VAL(tmp2), zero), zero, SQRTT(VAL(tmp2))));

    ASSIGN_CLN(F,tmp3, ADD(VAL(tmp), MUL(SC(0.5), INDX2(dcx,2,2))));
    ASSIGN_CLN(F,tmp3, QUES(LESSOREQ(VAL(tmp3), zero), zero, SQRTT(VAL(tmp3))));

    /* At this point tmp4 is done, and tmp1,2,3 are all set to their 
     * ultimate magnitudes.  tmp4 has been flushed unless they
     * are constants, but tmp1,2,3 can still contain non-constant expressions,
     * including references to symbol `tmp'.  Don't re-use `tmp' until
     * these references have been eliminated.
     *
     * Now we have to decide on signs for tmp1,2,3.  There are three
     * cases.
     */

    /* Count the number of cases which will be present in the generated
     * code.  If this ends up 1 there will be no if statements in the
     * generated code, except those generated by QUES expressions.
     */
    casecnt = 0;

    cond = INUSE(AND(GREATEROREQ(VAL(tmp1),VAL(tmp2)),
                     GREATEROREQ(VAL(tmp1),VAL(tmp3))));
    if (IFTHEN(F, cond, &elsetoo)) {
        /* tmp1 is the biggest */
        casecnt++;

        ASSIGN_CLN(F, tmp1, MUL(QUES(LESSTHAN(INDX2(dcx,2,1),INDX2(dcx,1,2)),
                                       SC(-1.0), one),
                                VAL(tmp1)));

        ASSIGN_CLN(F, tmp2, MUL(QUES(LESSTHAN(
                                       MUL(VAL(tmp1),
                                           ADD(INDX2(dcx,1,0),INDX2(dcx,0,1))),
                                       zero),
                                     SC(-1.0), one),
                                VAL(tmp2)));

        ASSIGN_CLN(F, tmp3, MUL(QUES(LESSTHAN(
                                       MUL(VAL(tmp1),
                                           ADD(INDX2(dcx,2,0),INDX2(dcx,0,2))),
                                       zero),
                                     SC(-1.0), one),
                                VAL(tmp3)));
        if (elsetoo) {
            CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
            CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
            CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
        }
    }
    if (IFELSE(F, cond, &thentoo)) {
        cond2 = INUSE(AND(GREATEROREQ(VAL(tmp2),VAL(tmp1)),
                          GREATEROREQ(VAL(tmp2),VAL(tmp3))));
        if (IFTHEN(F, cond2, &else2too)) {
            /* tmp2 is the biggest */
            casecnt++;

            ASSIGN_CLN(F,tmp2, MUL(QUES(LESSTHAN(INDX2(dcx,0,2),INDX2(dcx,2,0)),
                                        SC(-1.0), one),
                                   VAL(tmp2)));

            ASSIGN_CLN(F,tmp3, MUL(QUES(LESSTHAN(
                                          MUL(VAL(tmp2),
                                            ADD(INDX2(dcx,2,1),INDX2(dcx,1,2))),
                                          zero),
                                        SC(-1.0), one),
                                   VAL(tmp3)));

            ASSIGN_CLN(F,tmp1, MUL(QUES(LESSTHAN(
                                          MUL(VAL(tmp2),
                                            ADD(INDX2(dcx,0,1),INDX2(dcx,1,0))),
                                          zero),
                                        SC(-1.0), one),
                                   VAL(tmp1)));
            if (thentoo || else2too) {
                CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
            }
        }
        if (IFELSE(F, cond2, &then2too)) {
            /* tmp3 is the biggest */
            casecnt++;

            ASSIGN_CLN(F,tmp3, MUL(QUES(LESSTHAN(INDX2(dcx,1,0),INDX2(dcx,0,1)),
                                        SC(-1.0), one),
                                   VAL(tmp3)));

            ASSIGN_CLN(F,tmp1, MUL(QUES(LESSTHAN(
                                          MUL(VAL(tmp3),
                                            ADD(INDX2(dcx,0,2),INDX2(dcx,2,0))),
                                          zero),
                                        SC(-1.0), one),
                                   VAL(tmp1)));

            ASSIGN_CLN(F,tmp2, MUL(QUES(LESSTHAN(
                                          MUL(VAL(tmp3),
                                            ADD(INDX2(dcx,1,2),INDX2(dcx,2,1))),
                                          zero),
                                        SC(-1.0), one),
                                   VAL(tmp2)));
            if (thentoo || then2too) {
                CLEANVAR(F, tmp1, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp2, CL_FLUSHALL, CL_FORGET);
                CLEANVAR(F, tmp3, CL_FLUSHALL, CL_FORGET);
            }
        }
        IFEND(F, cond2);
        DISPOSE_EXPR(UNUSE(cond2));
    }
    IFEND(F, cond);
    DISPOSE_EXPR(UNUSE(cond));

    /* If there were multiple cases generated, then the current VAL(tmp1), etc.
     * are meaningless since the runtime value could be different.  In that
     * they should only be referenced by name.  
     *
     * Even if there was a single case, flush everything out to eliminate
     * references to `tmp' which we want to re-use.
     *
     * This doesn't apply to tmp4 since it was computed outside any `if'
     * statements, and already flushnonconst'd.
     */
    if (casecnt > 1) {
        ASSIGN(tmp1, VREF(tmp1));
        ASSIGN(tmp2, VREF(tmp2));
        ASSIGN(tmp3, VREF(tmp3));
    } else {
        CLEANVAR(F, tmp1, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, tmp2, CL_FLUSHNONCONST, CL_FORGET);
        CLEANVAR(F, tmp3, CL_FLUSHNONCONST, CL_FORGET);
    }

    /* Now normalize and get out. */
    ASSIGN_CLN(F, tmp, DVD(one, SQRTT(ADD(MUL(VAL(tmp1),VAL(tmp1)),
                                     ADD(MUL(VAL(tmp2),VAL(tmp2)),
                                     ADD(MUL(VAL(tmp3),VAL(tmp3)),
                                         MUL(VAL(tmp4),VAL(tmp4))))))));
    *e1 = MUL(VAL(tmp1),VAL(tmp));
    *e2 = MUL(VAL(tmp2),VAL(tmp));
    *e3 = MUL(VAL(tmp3),VAL(tmp));
    *e4 = MUL(VAL(tmp4),VAL(tmp));

    DISPOSE_EXPR(UNUSE(dcx));
}
#endif

/* The following routines generate numerical utility routines
 *    sdquat2dc(e1,e2,e3,e4,dircos)
 *    sddc2quat(dircos,&e1,&e2,&e3,&e4)
 *    sdang2dc(a1,a2,a3,dircos)
 *    sddc2ang(dircos, &a1,&a2,&a3)
 * for conversion between quaternions and direction cosines and between
 * 1-2-3 Euler angles and direction cosines.
 */
void PRINT_SDDC2ANG(FILE *F)
{
    sym dircos,a1,a2,a3,quot,angle,th1,th2,th3,costh2;
    expr a1x,a2x,a3x;

    declare_proc(F, 0, "dc2ang",
      VT_USER|VT_DSYM, &SysI.type_Mat,  "dircos", &dircos,
      VT_REAL|VT_BYREF|VT_DSYM,         "a1",     &a1,
      VT_DUP|VT_BYREF|VT_DSYM,          "a2",     &a2,
      VT_DUP|VT_BYREF|VT_DSYM,          "a3",     &a3,
      0);

    efprintf(F, Lang->proc_dbegin);

    /* Note: allocate a local temp array here since we can't conveniently 
     * access the global one from the Library File.  Really only need 2 
     * elements, but I'll allocate 10 just for insurance.
     */
    declare_vars(F, 0,
      VT_REAL|VT_DSYM, "th1",     &th1,
      VT_DUP|VT_DSYM,  "th2",     &th2,
      VT_DUP|VT_DSYM,  "th3",     &th3,
      VT_REAL|VT_ARRAY, "temp", 10, 0,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    DECOMPOSE_123(F, VAL(dircos), th1, th2, th3, &a1x, &a2x, &a3x);

    PRINT_ASSN(F, PRINTNAME(a1), a1x, 1/*byref*/);
    PRINT_ASSN(F, PRINTNAME(a2), a2x, 1);
    PRINT_ASSN(F, PRINTNAME(a3), a3x, 1);

    DISPOSE_EXPR(a1x);
    DISPOSE_EXPR(a2x);
    DISPOSE_EXPR(a3x);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDDC2QUAT(FILE *F)
{
    sym dircos,e1,e2,e3,e4,tmp,tmp1,tmp2,tmp3,tmp4;
    expr e1x,e2x,e3x,e4x;

    declare_proc(F, 0, "dc2quat",
      VT_USER|VT_DSYM, &SysI.type_Mat,  "dircos", &dircos,
      VT_REAL|VT_BYREF|VT_DSYM,         "e1",     &e1,
      VT_DUP|VT_BYREF|VT_DSYM,          "e2",     &e2,
      VT_DUP|VT_BYREF|VT_DSYM,          "e3",     &e3,
      VT_DUP|VT_BYREF|VT_DSYM,          "e4",     &e4,
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0,
      VT_REAL|VT_DSYM, "tmp",    &tmp,
      VT_DUP|VT_DSYM,  "tmp1",   &tmp1,
      VT_DUP|VT_DSYM,  "tmp2",   &tmp2,
      VT_DUP|VT_DSYM,  "tmp3",   &tmp3,
      VT_DUP|VT_DSYM,  "tmp4",   &tmp4,
      VT_REAL|VT_ARRAY, "temp", 10, 0,
      0);

    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    DECOMPOSE_QUAT(F, VAL(dircos), tmp, tmp1, tmp2, tmp3, tmp4, 
                  &e1x, &e2x, &e3x, &e4x);

    PRINT_ASSN(F, PRINTNAME(e1), e1x, 1/*byref*/);
    PRINT_ASSN(F, PRINTNAME(e2), e2x, 1);
    PRINT_ASSN(F, PRINTNAME(e3), e3x, 1);
    PRINT_ASSN(F, PRINTNAME(e4), e4x, 1);

    DISPOSE_EXPR(e1x);
    DISPOSE_EXPR(e2x);
    DISPOSE_EXPR(e3x);
    DISPOSE_EXPR(e4x);

    efprintf(F, Lang->proc_end);
}

void PRINT_SDANG2DC(FILE *F)
{
    sym dircos,a1,a2,a3,cos1,cos2,cos3,sin1,sin2,sin3;
    expr a1x,a2x,a3x,cos1x,cos2x,cos3x,sin1x,sin2x,sin3x;

    declare_proc(F, 0, "ang2dc",
      VT_REAL|VT_DSYM,         "a1", &a1,
      VT_DUP|VT_DSYM,          "a2", &a2,
      VT_DUP|VT_DSYM,          "a3", &a3,
      VT_USER|VT_DSYM, &SysI.type_Mat,  "dircos", &dircos,
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0,
      VT_REAL|VT_DSYM,                   "cos1", &cos1,
      VT_DUP|VT_DSYM,                   "cos2", &cos2,
      VT_DUP|VT_DSYM,                   "cos3", &cos3,
      VT_DUP|VT_DSYM,                   "sin1", &sin1,
      VT_DUP|VT_DSYM,                   "sin2", &sin2,
      VT_DUP|VT_DSYM,                   "sin3", &sin3,
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    a1x = INUSE(VREF(a1)); a2x = INUSE(VREF(a2)); a3x = INUSE(VREF(a3));

    cos1x = COSINE(a1x); cos2x = COSINE(a2x); cos3x = COSINE(a3x);
    sin1x = SINE(a1x); sin2x = SINE(a2x); sin3x = SINE(a3x);

    PRINT_ASSN(F, PRINTNAME(cos1), cos1x, 0);
    PRINT_ASSN(F, PRINTNAME(cos2), cos2x, 0);
    PRINT_ASSN(F, PRINTNAME(cos3), cos3x, 0);
    PRINT_ASSN(F, PRINTNAME(sin1), sin1x, 0);
    PRINT_ASSN(F, PRINTNAME(sin2), sin2x, 0);
    PRINT_ASSN(F, PRINTNAME(sin3), sin3x, 0);

    DISPOSE_EXPR(cos1x); DISPOSE_EXPR(cos2x); DISPOSE_EXPR(cos3x);
    DISPOSE_EXPR(sin1x); DISPOSE_EXPR(sin2x); DISPOSE_EXPR(sin3x);

    cos1x = INUSE(VREF(cos1)); 
    cos2x = INUSE(VREF(cos2));
    cos3x = INUSE(VREF(cos3));
    sin1x = INUSE(VREF(sin1)); 
    sin2x = INUSE(VREF(sin2));
    sin3x = INUSE(VREF(sin3));

    /* Memory used for these temporary expressions is lost. */
    PRINT_ASSN2(F, PRINTNAME(dircos),0,0,MUL(cos2x,cos3x));
    PRINT_ASSN2(F, PRINTNAME(dircos),0,1,NEG(MUL(cos2x,sin3x)));
    PRINT_ASSN2(F, PRINTNAME(dircos),0,2,sin2x);
    PRINT_ASSN2(F, PRINTNAME(dircos),1,0,
                ADD(MUL(sin1x,MUL(sin2x,cos3x)),MUL(sin3x,cos1x)));
    PRINT_ASSN2(F, PRINTNAME(dircos),1,1,
                SUB(MUL(cos3x,cos1x),MUL(sin1x,MUL(sin2x,sin3x))));
    PRINT_ASSN2(F, PRINTNAME(dircos),1,2,NEG(MUL(sin1x,cos2x)));
    PRINT_ASSN2(F, PRINTNAME(dircos),2,0,
                SUB(MUL(sin3x,sin1x),MUL(cos1x,MUL(sin2x,cos3x))));
    PRINT_ASSN2(F, PRINTNAME(dircos),2,1,
                ADD(MUL(cos1x,MUL(sin2x,sin3x)),MUL(cos3x,sin1x)));
    PRINT_ASSN2(F, PRINTNAME(dircos),2,2,MUL(cos1x,cos2x));

    DISPOSE_EXPR(UNUSE(a1x)); 
    DISPOSE_EXPR(UNUSE(a2x));
    DISPOSE_EXPR(UNUSE(a3x));
    DISPOSE_EXPR(UNUSE(cos1x));
    DISPOSE_EXPR(UNUSE(cos2x));
    DISPOSE_EXPR(UNUSE(cos3x));
    DISPOSE_EXPR(UNUSE(sin1x));
    DISPOSE_EXPR(UNUSE(sin2x));
    DISPOSE_EXPR(UNUSE(sin3x));

    efprintf(F, Lang->proc_end);
}

void PRINT_SDQUAT2DC(FILE *F)
{
    char str_flt0[10], str_flt1[10];

    esprintf(str_flt0, "%r", 0.);
    esprintf(str_flt1, "%r", 1.);

    declare_proc(F, 0, "quat2dc",
      VT_REAL,         "ie1",
      VT_DUP,          "ie2",
      VT_DUP,          "ie3",
      VT_DUP,          "ie4",
      VT_USER, &SysI.type_Mat,  "dircos", 
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0,
      VT_REAL,        "e1",        
      VT_DUP,        "e2",        
      VT_DUP,        "e3",        
      VT_DUP,        "e4",        
      VT_DUP,        "e11",        
      VT_DUP,        "e22",        
      VT_DUP,        "e33",        
      VT_DUP,        "e44",        
      VT_DUP,        "norm",        
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    SET("e11", "ie1*ie1");
    SET("e22", "ie2*ie2");
    SET("e33", "ie3*ie3");
    SET("e44", "ie4*ie4");

    efprintf(F, "norm%=%@D%s(e11+e22+e33+e44)%;\n", Lang->func_sqrt);

    IF("norm", EQ, str_flt0);
    THEN
      SET("e4", str_flt1);
      SET("norm", str_flt1);
    ELSE
      SET("e4", "ie4");
    ENDIF;
    efprintf(F, "norm%=%r/norm%;\n", 1.);
    SET("e1", "ie1*norm");
    SET("e2", "ie2*norm");
    SET("e3", "ie3*norm");
    SET("e4", "e4*norm");

    SET("e11", "e1*e1");
    SET("e22", "e2*e2");
    SET("e33", "e3*e3");

    efprintf(F, "dircos%(%@d%,%@d%)%=%r-(%r*(e22+e33))%;\n",0,0,1.,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e1*e2-e3*e4)%;\n",0,1,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e1*e3+e2*e4)%;\n",0,2,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e1*e2+e3*e4)%;\n",1,0,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r-(%r*(e11+e33))%;\n",1,1,1.,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e2*e3-e1*e4)%;\n",1,2,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e1*e3-e2*e4)%;\n",2,0,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r*(e2*e3+e1*e4)%;\n",2,1,2.);
    efprintf(F, "dircos%(%@d%,%@d%)%=%r-(%r*(e11+e22))%;\n",2,2,1.,2.);

    efprintf(F, Lang->proc_end);
}

/*
 * Print vector-manipulation library routines:
 *
 *      d = sdvdot(v1,v2)                      dot product
 *      n = sdvnorm(v)                         norm of a vector
 *      sdvcopy(ivec,ovec)                       copy ivec to ovec
 *      sdvset(s1,s2,s3,ovec)                       set ovec to [s1,s2,s3]
 *      sdvmul(s,ivec,ovec)                    scalar multiply
 *      sdvaxpy(s,v1,v2,ovec)                  ovec=s*v1+v2
 *      sdvadd(v1,v2,ovec)                     add two vectors
 *      sdvsub(v1,v2,ovec)                     subtract two vectors
 *      sdvcross(v1,v2,ovec)                   cross product
 *      sdvrot(ivec,rvec,theta,ovec)           rotation about a vector
 *
 * In these it is OK for the output vector to be one of the input
 * vectors.
 */
void PRINT_VECSUBS(FILE *F)
{
    char sdvdotnm[20], sdvnormnm[20];
    int i;

    esprintf(sdvdotnm,  "%Avdot");  /* name of the SDVDOT() routine */
    esprintf(sdvnormnm, "%Avnorm"); /* name of the SDVNORM() routine */

    /* SDVDOT() */

    declare_proc(F, DECL_FUNCTION, VT_REAL, "vdot",
      VT_VECTOR,       "ivec1",
      VT_DUP,          "ivec2",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0,
      VT_REAL,        "dprod",        
      0);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Compute the dot product of two vectors\n%}");

    efprintf(F, "dprod%=ivec1%(%@d%)*ivec2%(%@d%)", 0,0);
    efprintf(F, "+ivec1%(%@d%)*ivec2%(%@d%)+ivec1%(%@d%)*ivec2%(%@d%)%;\n",
             1,1, 2,2);

    FRETURN("%Avdot", "dprod");
    efprintf(F, Lang->proc_end_noret);

    /* SDVNORM() */

    declare_proc(F, DECL_FUNCTION, VT_REAL, "vnorm",
      VT_VECTOR,       "ivec",
      0);

    efprintf(F, Lang->proc_dbegin);
    declare_vars(F, 0,
      VT_REAL,        "norm",        
      VT_REAL|VT_COND, Lang == &FORTRAN_language, sdvdotnm,
      0);
    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Compute the length (norm) of a vector\n%}");

    efprintf(F, "norm%=%@D%s(%Avdot(ivec,ivec))%;\n",Lang->func_sqrt);

    FRETURN("%Avnorm", "norm");
    efprintf(F, Lang->proc_end_noret);

    /* SDVCOPY() */

    declare_proc(F, 0, "vcopy",
      VT_VECTOR,       "ivec",
      VT_DUP,          "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Copy vector ivec to vector ovec\n%}");

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=ivec%(%@d%)%;\n", i, i);

    efprintf(F, Lang->proc_end);

    /* SDVSET() */

    declare_proc(F, 0, "vset",
      VT_REAL,               "sclr1",
      VT_DUP,               "sclr2",
      VT_DUP,               "sclr3",
      VT_VECTOR,       "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Set ovec to [sclr1,sclr2,sclr3]\n%}");

    efprintf(F, "ovec%(%@d%)%=sclr1%;\n", 0);
    efprintf(F, "ovec%(%@d%)%=sclr2%;\n", 1);
    efprintf(F, "ovec%(%@d%)%=sclr3%;\n", 2);

    efprintf(F, Lang->proc_end);

    /* SDVADD() */

    declare_proc(F, 0, "vadd",
      VT_VECTOR,       "ivec1",
      VT_DUP,          "ivec2",
      VT_DUP,          "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Add two vectors\n%}");

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=ivec1%(%@d%)+ivec2%(%@d%)%;\n", i, i, i);

    efprintf(F, Lang->proc_end);

    /* SDVSUB() */

    declare_proc(F, 0, "vsub",
      VT_VECTOR,       "ivec1",
      VT_DUP,          "ivec2",
      VT_DUP,          "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Subtract two vectors\n%}");

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=ivec1%(%@d%)-ivec2%(%@d%)%;\n", i, i, i);

    efprintf(F, Lang->proc_end);

    /* SDVMUL() */

    declare_proc(F, 0, "vmul",
      VT_REAL,         "sclr",
      VT_VECTOR,       "ivec",
      VT_VECTOR,       "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Multiply a vector by a scalar\n%}");

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=sclr*ivec%(%@d%)%;\n", i, i);

    efprintf(F, Lang->proc_end);

    /* SDVAXPY() */

    declare_proc(F, 0, "vaxpy",
      VT_REAL,         "sclr",
      VT_VECTOR,       "ivec1",
      VT_DUP,          "ivec2",
      VT_VECTOR,       "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);
    efprintf(F, Lang->proc_dend);
    efprintf(F, Lang->proc_sbegin);

    efprintf(F, 
      "%{Multiply a vector by a scalar and add in another vector\n%}");

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=sclr*ivec1%(%@d%)+ivec2%(%@d%)%;\n", i, i, i);

    efprintf(F, Lang->proc_end);

    /* SDVCROSS() */

    declare_proc(F, 0, "vcross",
      VT_VECTOR,       "ivec1",
      VT_DUP,          "ivec2",
      VT_DUP,               "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0,
      VT_VECTOR, "tempout",        
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    efprintf(F, "%{Compute the cross product of two vectors\n%}");

    efprintf(F, "tempout%(%@d%)%=ivec1%(%@d%)*ivec2%(%@d%)",0,1,2);
    efprintf(F, "-ivec1%(%@d%)*ivec2%(%@d%)%;\n", 2,1);
    efprintf(F, "tempout%(%@d%)%=ivec1%(%@d%)*ivec2%(%@d%)",1,2,0);
    efprintf(F, "-ivec1%(%@d%)*ivec2%(%@d%)%;\n", 0,2);
    efprintf(F, "tempout%(%@d%)%=ivec1%(%@d%)*ivec2%(%@d%)",2,0,1);
    efprintf(F, "-ivec1%(%@d%)*ivec2%(%@d%)%;\n", 1,0);

    for (i=0; i<3; i++)
        efprintf(F, "ovec%(%@d%)%=tempout%(%@d%)%;\n", i, i);

    efprintf(F, Lang->proc_end);

    /* SDVROT() */

    declare_proc(F, 0, "vrot",
      VT_VECTOR,       "ivec",
      VT_DUP,          "rvec",
      VT_REAL,         "theta",
      VT_VECTOR,       "ovec",
      0);

    efprintf(F, Lang->proc_dbegin);

    declare_vars(F, 0,
      VT_REAL|VT_COND, Lang == &FORTRAN_language, sdvdotnm,
      VT_REAL|VT_COND, Lang == &FORTRAN_language, sdvnormnm,
      VT_REAL,         "norm",        
      VT_DUP,         "idotu",        
      VT_DUP,         "ctheta",        
      VT_DUP,         "stheta",        
      VT_DUP,         "dotcos",        
      VT_VECTOR, "uvec",
      VT_DUP,    "icosvec",
      VT_DUP,    "isinvec",
      VT_DUP,    "uxisin",
      0);

    efprintf(F, Lang->proc_dend);

    efprintf(F, Lang->proc_sbegin);

    efprintf(F, 
     "%{Rotate a vector ivec around vector rvec by angle theta\n%}");

    SET("norm", "%Avnorm(rvec)");

    /* If norm is zero, perform no rotation and report error. */
    IFCOND efprintf(F, "norm%s%r", EQ, 0.);
    THEN
      for (i=0; i<3; i++)
          efprintf(F, "ovec%(%@d%)%=ivec%(%@d%)%;\n", i, i);
      SETERR(F, ROU_sdvrot, ERR_CantRotAboutZeroVector);
      RETURN;
    ENDIF;

    /* Calculate sin and cosine of theta. */
    efprintf(F, "stheta%=%@D%s(theta)%;\n", Lang->func_sin);
    efprintf(F, "ctheta%=%@D%s(theta)%;\n", Lang->func_cos);

    /* Calculate ivec*sin(theta),ivec*cos(theta),
     * uvec=normalized rvec, idotu=ivec*uvec. 
     */
    CALL("%Avmul(stheta,ivec,isinvec)");
    CALL("%Avmul(ctheta,ivec,icosvec)");
    efprintf(F, "norm%=%r/norm%;\n", 1.);
    CALL("%Avmul(norm,rvec,uvec)");
    efprintf(F, "idotu%=%Avdot(ivec,uvec)%;\n");

    /* Set uxisin=uvec X ivec*sin(theta) */
    CALL("%Avcross(uvec,isinvec,uxisin)");

    /* Calculate idotu*(1-cos(theta)) */
    efprintf(F, "dotcos%=idotu*(%r-ctheta)%;\n", 1.);

    /* Sum   uvec * (ivec*uvec)*(1-cos(theta))
     *     + ivec*cos(theta)
     *     + uvec X ivec*sin(theta)
     * to produce the rotated vector.
     */
    CALL("%Avaxpy(dotcos,uvec,icosvec,ovec)");
    CALL("%Avadd(ovec,uxisin,ovec)");

    efprintf(F, Lang->proc_end);
}
