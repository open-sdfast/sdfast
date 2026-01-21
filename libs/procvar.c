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

#include "libs.h"
#include "libsprot.h"
#include "words.h"
#include "../calc/language.h"

#define ANYBODIESYET                (SystemInfo->n > 0)
#define ONFIRSTBODY                (SystemInfo->n == 1)
#define ANYCONSTRAINTS                (SystemInfo->nxc+SystemInfo->nu > 0)
#define ANYUSERCONSTRAINTS        (SystemInfo->nu > 0)
#define ONLOOPJOINT                (*loopjoint)

/* PROCESS_VAR
 * 
 * Process one `statement,' that is, a keyword and its parameters.
 * Returns 1 if successful, otherwise puts an error on stderr and returns 0.
 * IdNum is the number of the reserved word.  When we see the keyword
 * `body' it introduces either a new body or a loop joint.  We use the
 * `loopjoint' parameter to remember what kind of `body' we're working on.
 */
char
PROCESS_VAR(FILE                  *System,
            register SystemInfo_t *SystemInfo,
            char                  *ReservedWords[],
            int                   IdNum,
            int                   *loopjoint)
{
    register Index_t i;
    string32         Str32;
    expr             Rexpr;        /* real expression */
    int              IsNumeric;
    flags_t          QuesFlg;
    ItemType_t       NextThing;

    char *what = ONLOOPJOINT ? "loop joint" : "body";

    register JointDesc_t *jntp =
      ONLOOPJOINT ? &SystemInfo->LoopConst[SystemInfo->nl - 1].jnt
                  : &SystemInfo->Bodies[SystemInfo->n - 1].jnt;

    char *bname =
      ONLOOPJOINT ? SystemInfo->LoopConst[SystemInfo->nl - 1].OutbBodyName
                  : SystemInfo->Bodies[SystemInfo->n - 1].BodyName;

    register BodyDesc_t *BodyP = 
                  &SystemInfo->Bodies[SystemInfo->n - 1];
    register ConstraintDesc_t *ConstP =
                  &SystemInfo->Const[SystemInfo->nxc+SystemInfo->nu - 1];

    switch (IdNum) {
        case cGroundedWord:
            if (ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
"`grounded' or `manipulator' keyword must be specified before any bodies.\n");
                return 0;
            }
            if (SystemInfo->Grounded) {
                USER_ERR();
                fprintf(stderr,
            "`grounded' or `manipulator' keyword was respecified.\n");
                return 0;
            }

            SystemInfo->Grounded = 1;
            break;

        case cLanguage: {
            struct language *lang;

            if (ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
            "`language' keyword must be specified before any bodies.\n");
                return 0;
            }
            if (!FIND_EQUAL(System) || !READ_ID(System, Str32))
                return 0;
            if (!(i = LOOKUP(ReservedWords, Str32))) {
                USER_ERR();
                fprintf(stderr, "Unrecognized language name `%s'.\n", Str32);
                return 0;
            }
            switch (i) {
                case cFORTRANword:
                    lang = &FORTRAN_language;
                    break;
                case cAdsimWord:
                    if (sdfast_opt.precision == OPT_SINGLE) {
                        USER_ERR();
                        fprintf(stderr,
                "Language Adsim can be used only with double precision.\n");
                        return 0;
                    }
                    lang = &ADSIM_language;
                    break;
                case cKRCword:
                    lang = &KRC_language;
                    break;
                case cCword:
                case cANSICword:
                    lang = &ANSIC_language;
                    break;
                case cCPPword:
                    lang = &Cpp_language;
                    break;
                case cAdaWord:
                    lang = &Ada_language;
                    break;
                case cPascalWord:
                    lang = &Pascal_language;
                    break;
                default:
                    USER_ERR();
                    fprintf(stderr,
                      "Sorry, %s is not yet supported as an output language.\n",
                      Str32);
                    return 0;
            }
            if (sdfast_opt.lang != OPT_LANGDEFAULT) {
                /* already specified on command line */
                if (lang != sdfast_opt.lang)
                    fprintf(stderr,
  "Warning: Language specified in Input File overridden by command line.\n");
            } else {
                sdfast_opt.lang = lang;
                SET_LANGUAGE(lang);
            }
            break;
        }

        case cPrefixWord: {
            static char prefix[50];
            if (ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
            "`prefix' keyword must be specified before any bodies.\n");
                return 0;
            }
            if (!FIND_EQUAL(System) || !READ_ID(System, prefix))
                return 0;
            if (sdfast_opt.prefix != OPT_STRDEFAULT) {
                if (!strcmp(sdfast_opt.prefix, prefix))
                    fprintf(stderr,
  "Warning: Prefix specified in Input File overridden by command line.\n");
            } else {
                sdfast_opt.prefix = prefix;
                SET_PREFIX(prefix);
            }
            break;
        }

        case cSingle:
            if (ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
            "The `single' keyword must be specified before any bodies.\n");
                return 0;
            }
            if (Lang == &ADSIM_language) {
                USER_ERR();
                fprintf(stderr, "ADSIM does not support single precision.\n");
                return 0;
            }

            if (sdfast_opt.precision != OPT_DEFAULT) {
                if (sdfast_opt.precision != OPT_SINGLE)
                    fprintf(stderr,
      "Warning: `single' in Input File overridden by command line.\n");
            } else {
                sdfast_opt.precision = OPT_SINGLE;
                SET_PRECISION(sdfast_opt.precision == OPT_SINGLE); 
            }
            break;

        case cDouble:
            if (ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
            "The `double' keyword must be specified before any bodies.\n");
                return 0;
            }
            if (sdfast_opt.precision != OPT_DEFAULT) {
                if (sdfast_opt.precision != OPT_DOUBLE)
                    fprintf(stderr,
      "Warning: `double' in Input File overridden by command line.\n");
            } else {
                sdfast_opt.precision = OPT_DOUBLE;
                SET_PRECISION(sdfast_opt.precision == OPT_SINGLE); 
            }
            break;

        case cGravity:
            if (SystemInfo->GravExpr) {
                USER_ERR();
                fprintf(stderr, "`gravity' keyword respecified.\n");
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            SystemInfo->GravExpr = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg))
                        return 0;
                    SINDX(SystemInfo->GravExpr, i, Rexpr);
                    SystemInfo->GravFlg[i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr, "Need 3 numbers for gravity vector.\n");
                    return 0;
                }
            }
            break;

        /* "body" is use in three places:
         *   (1) to introduce a body paragraph in the tree joint section
         *   (2) introduce a loop joint  
         *   (3) provide a body parameter to a constraint.
         *  It's case (3) if we've seen ANYCONSTRAINTS.  Otherwise, 
         *  it's a loop joint (case (2)) if we've already seen this body 
         *  name.
         */
        case cBody: {
            string32 BodyName;

            if (!FIND_EQUAL(System) || !WHATS_NEXT(System, &NextThing))
                return 0;
            if (!READ_ID(System, BodyName))
                return 0;

            if (ANYCONSTRAINTS) {
                /* this is a body parameter to a constraint */
                int nbod,need;

                if (ANYUSERCONSTRAINTS) {
                    USER_ERR();
                    fprintf(stderr,
                      "`Body' keyword not allowed for user constraint.\n");
                    return 0;
                }

                if (NextThing != cIdentNext) {
                    USER_ERR();
                    fprintf(stderr, 
                        "Invalid name `%s' for body in constraint `%s'.\n", 
                        BodyName, ConstP->ConstraintName);
                    return 0;
                }
                if (ConstP->ConstraintType == cUnknownCons) {
                    USER_ERR();
                    fprintf(stderr,
                      "`Type' keyword expected for constraint `%s'.\n",
                      ConstP->ConstraintName);
                    return 0;
                }
                nbod = ConstP->nbod;
                need = ConstraintInfo[(int)ConstP->ConstraintType].nbod;
                if (nbod >= need) {
                    USER_ERR();
                    fprintf(stderr,
                      "Too many bodies given for constraint `%s' (need %d).\n",
                      ConstP->ConstraintName,need);
                    return 0;
                }

                if ((ConstP->Bodies[nbod] = FINDBODY(SystemInfo, BodyName)) 
                     == cUnspecifiedBody) 
                {
                    USER_ERR();
                    fprintf(stderr,
                           "Unknown body `%s' used in constraint `%s'.\n",
                           BodyName, ConstP->ConstraintName);
                    return 0;
                }

                ConstP->nbod++;
                break;
            }

            if (NextThing != cIdentNext) {
                USER_ERR();
                fprintf(stderr, "Invalid name `%s' for body.\n", BodyName);
                return 0;
            }

            if ((i=FINDBODY(SystemInfo, BodyName)) != cUnspecifiedBody) {
                /* This is a loop joint's outboard body. */
                *loopjoint = 1;

                if (SystemInfo->nl == cMaxNumLoops) {
                    USER_ERR();
                    fprintf(stderr, "Too many loop joints. Maximum is %d.\n", 
                            cMaxNumLoops);
                    return 0;
                }

                INIT_LOOP(SystemInfo, SystemInfo->nl++, i, BodyName);

            } else {
                /* This is a new body. */
                *loopjoint = 0;

                if (SystemInfo->n == cMaxNumBodies) {
                    USER_ERR();
                    fprintf(stderr,
                      "Too many bodies. Maximum is %d.\n", cMaxNumBodies);
                    return 0;
                }

                INIT_BODY(SystemInfo, SystemInfo->n++, BodyName);

                /* By default, the first body is connected to ground by
                   a 6dof joint.  This may be revised if we see a joint
                   specification later. */
                if (ONFIRSTBODY)
                    SystemInfo->s = 6;
            }
            break;
        }

        case cInboard:
        case cINB:        {
            string32 InbName;

            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`inboard' must be part of a body description.\n");
                return 0;
            }
            if (jntp->InbBody != cUnspecifiedBody) {
                USER_ERR();
                fprintf(stderr, "`inboard' respecified for %s `%s'.\n", 
                        what, bname);
                return 0;
            }
            if (!FIND_EQUAL(System) || !READ_ID(System, InbName))
                return 0;
            if ((jntp->InbBody = FINDBODY(SystemInfo, InbName)) 
                 == cUnspecifiedBody) 
            {
                USER_ERR();
                fprintf(stderr,
                  "Unknown body `%s' used as inboard body for %s `%s'.\n",
                  InbName, what, bname);
                return 0;
            }
            if (jntp->InbBody == FINDBODY(SystemInfo, bname)) {
                USER_ERR();
                fprintf(stderr,
                  "`%s' was used as its own inboard body.\n",
                  InbName);
                return 0;
            }
            break;
        }

        case cPrescribedWord:
            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`prescribed' must be part of a joint description.\n");
                return 0;
            }
            if (jntp->Pres[0] != NULLEXPR) {
                USER_ERR();
                fprintf(stderr, 
                        "`prescribed' keyword respecified in %s `%s'.\n",
                        what, bname);
                return 0;
            }
            if (JointInfo[(int)jntp->JointKind].dof == 0) {
                USER_ERR();
                fprintf(stderr, 
            "Prescribed motion doesn't make sense for 0 dof joint (%s `%s').\n",
                        what, bname);
                return 0;
            }
            {
                int EqualNext;
                int NumericNext;
                int cnt;
                if (!EQUAL_NEXT(System,&EqualNext)) 
                    return 0;
                if (! EqualNext) {
                    jntp->Pres[0] = SPECIALEXPR;
                    goto EndPresCase;
                }
                if (!FIND_EQUAL(System)) return 0;
                cnt = 0;
                while (cnt < 6) {
                    if (!NUMERIC_NEXT(System,&NumericNext)) return 0;
                    if (! NumericNext)
                        goto EndPresCase;
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                        return 0;
                    if (IS_ZERO(Rexpr) || IS_ONE(Rexpr)) {
                        jntp->Pres[cnt] = Rexpr;
                        jntp->PresFlg[cnt] = QuesFlg;
                    } else {
                        USER_ERR();
                        fprintf(stderr,
      "Expected 0, 1, or ? after `prescribed' for hinge %d of %s `%s'.\n",
                          cnt,what, bname);
                        return 0;
                    }
                    cnt++;
                }
            }

        EndPresCase:
            break;

        case cJoint: {
            string32 JointType;

            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`joint' must be part of a body description.\n");
                return 0;
            }
            if (jntp->JointKind != cUnknownJoint) {
                USER_ERR();
                fprintf(stderr, "Joint type respecified in %s `%s'.\n",
                        what, bname);
                return 0;
            }
            if (!FIND_EQUAL(System) || !READ_ID(System, JointType))
                return 0;
            if (!(i = LOOKUP(ReservedWords, JointType))) {
                USER_ERR();
                fprintf(stderr, "Unrecognized joint type `%s' for %s `%s'.\n",
                        JointType, what, bname);
                return 0;
            }
            switch (i) {
                case cPin:
                case c1dWord:
                    jntp->JointKind = cPinJoint;
                    break;
                case cUword:
                case cUjointWord:
                case cUdashJoint:
                case c2dWord:
                    jntp->JointKind = cUjoint;
                    break;
                case cGimbal:
                case c3dWord:
                    jntp->JointKind = c3dJoint;
                    break;
                case cBallWord:
                    jntp->JointKind = cBallJoint;
                    break;
                case cSliderWord:
                case cSlidingWord:
                    jntp->JointKind = cSlidingJoint;
                    break;
                case c6dWord:
                case cSixdofWord:
                case cFreeWord:
                    jntp->JointKind = c6dJoint;
                    break;
                case cCylinderWord:
                case cCylindricalWord:
                    jntp->JointKind = cCylJoint;
                    break;
                case cPlanarWord:
                    jntp->JointKind = cPlanarJoint;
                    break;
                case cWeldWord:
                    jntp->JointKind = cWeldJoint;
                    break;
                case cBushingWord:
                    jntp->JointKind = cBushingJoint;
                    break;
                case cBearingWord:
                    jntp->JointKind = cBearingJoint;
                    break;
                case cRplanarWord:
                    jntp->JointKind = cRevPlanarJoint;
                    break;
                case cRsixdofWord:
                case cRfreeWord:
                    jntp->JointKind = cRev6dJoint;
                    break;
                case cRbushingWord:
                    jntp->JointKind = cRevBushingJoint;
                    break;
                case cRbearingWord:
                    jntp->JointKind = cRevBearingJoint;
                    break;
            }
            jntp->JointDOF = JointInfo[(int)jntp->JointKind].dof;

            if (ONLOOPJOINT) {
                if (jntp->JointKind == cRevPlanarJoint 
                    || jntp->JointKind == cRev6dJoint
                    || jntp->JointKind == cRevBushingJoint
                    || jntp->JointKind == cRevBearingJoint)
                {
                    USER_ERR();
                    fprintf(stderr,
  "No reverse loop joints -- reverse inboard and outboard bodies instead.\n");
                    return 0;
                }
                SystemInfo->sl += jntp->JointDOF;
            } else {
                if (ONFIRSTBODY) {
                    SystemInfo->s = jntp->JointDOF;
                    SystemInfo->Grounded = 1;
                } else
                    SystemInfo->s += jntp->JointDOF;
            }

            if (SystemInfo->s > cMaxNumDOF) {
                USER_ERR();
                fprintf(stderr,
                  "Too many degrees of freedom.  Maximum is %d.\n", cMaxNumDOF);
                return 0;
            }
            break;
        }

        /* Joint names and constraint names come from the same namespace. */
        case cJnameWord: {
            string32 Jname;

            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`Jname' must be part of a joint specification.\n");
                return 0;
            }
            if (jntp->JointName[0] != '\0') {
                USER_ERR();
                fprintf(stderr, "`Jname' respecified for %s `%s'.\n",
                        what, bname);
                return 0;
            }
            if (!FIND_EQUAL(System) || !WHATS_NEXT(System, &NextThing))
                return 0;
            if (!READ_ID(System, Jname))
                return 0;
            if (NextThing != cIdentNext) {
                USER_ERR();
                fprintf(stderr, "Invalid joint name `%s' (%s `%s').\n", 
                    Jname, what, bname);
                return 0;
            }

            if (FINDJOINT(SystemInfo, Jname) != cUnspecifiedJoint) {
                USER_ERR();
                fprintf(stderr, "Joint name `%s' used twice (%s `%s').\n", 
                    Jname, what, bname);
                return 0;
            }

            strcpy(jntp->JointName, Jname);
            break;
        }

        case cMass:
            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, "`mass' must be part of a body description.\n");
                return 0;
            }
            if (ONLOOPJOINT) {
                USER_ERR();
                fprintf(stderr, 
                        "`mass' not allowed in loop joint specification.\n");
                return 0;
            }
            if (BodyP->Mass) {
                USER_ERR();
                fprintf(stderr,
                  "`mass' respecified for body `%s'.\n", bname);
                return 0;
            }
            if (!FIND_EQUAL(System) || !NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                if (!GET_REAL_EXPR(System, &BodyP->Mass, &BodyP->MassFlg))
                    return 0;
            } else {
                USER_ERR();
                fprintf(stderr,
              "Expected a number or question mark for mass of body `%s'.\n",
                  bname);
                return 0;
            }
            break; /* cMass */

        case cInertia:
            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`inertia' must be part of a body description.\n");
                return 0;
            }
            if (ONLOOPJOINT) {
                USER_ERR();
                fprintf(stderr, 
                        "`inertia' not allowed in loop joint specification.\n");
                return 0;
            }
            if (BodyP->Inertia) {
                USER_ERR();
                fprintf(stderr,
                  "`inertia' respecified for body `%s'.\n", bname);
                return 0;
            }
            if (DO_INERTIA(System, SystemInfo))
                return 0;
            break;

        case cBodyToJoint:
        case cBTJ:
            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`bodyToJoint' must be part of a body description.\n");
                return 0;
            }
            if (jntp->BodyToJoint) {
                USER_ERR();
                fprintf(stderr, "`bodyToJoint' respecified for %s `%s'.\n",
                          what, bname);
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            jntp->BodyToJoint = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric)) return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                        return 0;
                    SINDX(jntp->BodyToJoint, i, Rexpr);
                    jntp->BtjFlg[i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr,
                      "Need 3 numbers for bodyToJoint vector for %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            break;

        case cInboardToJoint:
        case cInbToJoint:
        case cITJ:
            if (!ANYBODIESYET || ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr, 
                    "`inboardToJoint' must be part of a body description.\n");
                return 0;
            }
            if (jntp->InbToJoint) {
                USER_ERR();
                fprintf(stderr, "`inboardToJoint' respecified for %s `%s'.\n",
                        what, bname);
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            jntp->InbToJoint = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                        return 0;
                    SINDX(jntp->InbToJoint, i, Rexpr);
                    jntp->ItjFlg[i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr,
                      "Need 3 numbers for inbToJoint vector for %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            break;

        case cBodyPinWord:
            if (!ONLOOPJOINT) {
                USER_ERR();
                fprintf(stderr,
              "`bodypin' keyword is only allowed as part of a loop joint.\n");
                return 0;
            }

            if (jntp->Pins[BODYPIN]) {
                USER_ERR();
                fprintf(stderr, "`bodypin' respecified in %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!JointInfo[(int)jntp->JointKind].bodypinOK) {
                USER_ERR();
                fprintf(stderr, "No `bodypin' allowed for %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!FIND_EQUAL(System))
                return 0;
            jntp->Pins[BODYPIN] = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg))
                        return 0;
                    SINDX(jntp->Pins[BODYPIN], i, Rexpr);
                    jntp->PinFlg[BODYPIN][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr, 
                            "Need 3 numbers for `bodypin' in %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                USER_ERR();
                fprintf(stderr, 
                  "Too many numbers for `bodypin' in %s `%s' (need 3).\n",
                        what, bname);
                return 0;
            }
            break;
        
        case cInbRefWord:
        case cInboardRefWord:
            if (!ONLOOPJOINT) {
                USER_ERR();
                fprintf(stderr,
              "`inbref' keyword is only allowed as part of a loop joint.\n");
                return 0;
            }

            if (jntp->Pins[INBREF]) {
                USER_ERR();
                fprintf(stderr, "`inbref' respecified in %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!JointInfo[(int)jntp->JointKind].inbrefOK) {
                USER_ERR();
                fprintf(stderr, "No `inbref' allowed for %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!FIND_EQUAL(System))
                return 0;
            jntp->Pins[INBREF] = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg))
                        return 0;
                    SINDX(jntp->Pins[INBREF], i, Rexpr);
                    jntp->PinFlg[INBREF][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr, 
                            "Need 3 numbers for `inbref' in %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                USER_ERR();
                fprintf(stderr, 
                  "Too many numbers for `inbref' in %s `%s' (need 3).\n",
                        what, bname);
                return 0;
            }
            break;
        
        case cBodyRefWord:
            if (!ONLOOPJOINT) {
                USER_ERR();
                fprintf(stderr,
              "`bodyref' keyword is only allowed as part of a loop joint.\n");
                return 0;
            }

            if (jntp->Pins[BODYREF]) {
                USER_ERR();
                fprintf(stderr, "`bodyref' respecified in %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!JointInfo[(int)jntp->JointKind].bodyrefOK) {
                USER_ERR();
                fprintf(stderr, "No `bodyref' allowed for %s `%s'.\n",
                  what, bname);
                return 0;
            }

            if (!FIND_EQUAL(System))
                return 0;
            jntp->Pins[BODYREF] = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg))
                        return 0;
                    SINDX(jntp->Pins[BODYREF], i, Rexpr);
                    jntp->PinFlg[BODYREF][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr, 
                            "Need 3 numbers for `bodyref' in %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                USER_ERR();
                fprintf(stderr, 
                  "Too many numbers for `bodyref' in %s `%s' (need 3).\n",
                        what, bname);
                return 0;
            }
            break;
        
        case cInbPinWord:
        case cInboardPinWord:
        case cPin:
        case cPins: {
            Index_t NextPin;
            int nmax;

            if (!jntp->Pins[INBPIN1])
                NextPin = INBPIN1;
            else if (!jntp->Pins[INBPIN2])
                NextPin = INBPIN2;
            else if (!jntp->Pins[INBPIN3])
                NextPin = INBPIN3;
            else {
                USER_ERR();
                fprintf(stderr, "More than 3 pins supplied for %s `%s'.\n",
                          what, bname);
                return 0;
            }
            /* For tree weld, no pin allowed although loop weld allows one. */
            nmax = JointInfo[(int)jntp->JointKind].nmax;
            if (jntp->JointKind == cWeldJoint && !ONLOOPJOINT)
                nmax = 0;
            if (NextPin >= nmax) {
                USER_ERR();
                fprintf(stderr, 
                  "Too many axes provided for %s `%s' (max is %d).\n", 
                        what, bname, nmax);
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            jntp->Pins[NextPin] = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg))
                        return 0;
                    SINDX(jntp->Pins[NextPin], i, Rexpr);
                    jntp->PinFlg[NextPin][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr, 
                            "Need 3 numbers for hinge axis in %s `%s'.\n",
                            what, bname);
                    return 0;
                }
            }
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                USER_ERR();
                fprintf(stderr, 
                  "Too many numbers for hinge axis in %s `%s' (need 3).\n",
                        what, bname);
                return 0;
            }
            break;
        }

        case cConstraints: {
            long numc;

            if (!ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
 "Constraints must be defined after all bodies and loop joints are defined.\n");
                return 0;
            }

            if (!FIND_EQUAL(System) || !WHATS_NEXT(System, &NextThing))
                return 0;
            if (NextThing != cNumberNext) {
                USER_ERR();
                fprintf(stderr,
 "Expected a number following the `constraints' keyword.\n");
                return 0;
            }

            /* This is just an abbreviation for some no. of user constraints. */
            if (!GET_INT(System, &numc))
                return 0;
            if (numc <= 0) {
                USER_ERR();
                fprintf(stderr,
                  "Number of constraints must be greater than 0.\n");
                return 0;
            }
            while (numc-- > 0) {
                esprintf(Str32, "user_%@d", SystemInfo->nu);
                INIT_CONSTRAINT(SystemInfo, SystemInfo->nxc+SystemInfo->nu++, 
                                Str32, cUserCons);
            }
            break;
        }

        case cConstraint:
            if (!ANYBODIESYET) {
                USER_ERR();
                fprintf(stderr,
 "Constraints must be defined after all bodies and loop joints are defined.\n");
                return 0;
            }

            if (!FIND_EQUAL(System) || !WHATS_NEXT(System, &NextThing))
                return 0;
            if (!READ_ID(System, Str32))
                return 0;
            if (NextThing != cIdentNext) {
                USER_ERR();
                fprintf(stderr, "Invalid name `%s' for constraint.\n", 
                    Str32);
                return 0;
            }

            if (FINDCONSTRAINT(SystemInfo, Str32) != cUnspecifiedConstraint) {
                USER_ERR();
                fprintf(stderr,
                  "Constraint name `%s' used twice.\n", Str32);
                return 0;
            }
            if (FINDJOINT(SystemInfo, Str32) != cUnspecifiedJoint) {
                USER_ERR();
                fprintf(stderr,
                  "Constraint name `%s' already used as a joint name.\n", 
                  Str32);
                return 0;
            }
            /* We'll assume this is a user constraint and change our
               mind later if we see a 'type' keyword. */
            INIT_CONSTRAINT(SystemInfo, SystemInfo->nu++, 
                            Str32, cUnknownCons);
            break;

        case cTypeWord: {
            ConstraintKind_t        constype;

            if (!ANYCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr,
                  "`Type' keyword is only allowed as part of a constraint.\n");
                return 0;
            }
            if (!FIND_EQUAL(System) || !READ_ID(System, Str32))
                return 0;
            if (!(i = LOOKUP(ReservedWords, Str32))) {
                USER_ERR();
                fprintf(stderr, "Unrecognized constraint type `%s'.\n", Str32);
                return 0;
            }
            switch (i) {
                case cUserWord:
                    constype = cUserCons;
                    break;
                case cGearWord:
                    constype = cGearCons;
                    break;
                case cScrewWord:
                    constype = cScrewCons;
                    break;
                case cDistanceWord:
                    constype = cDistanceCons;
                    break;
                case cPerpWord:
                case cPerpendicularWord:
                    constype = cPerpCons;
                    break;
                case cCoordWord:
                case cCoordinateWord:
                    constype = cCoordCons;
                    break;

                case cLoopWord:
                    USER_ERR();
                    fprintf(stderr, 
"Loop constraints are no longer specified this way -- please check manual.\n");
                    return 0;

                default:
                    USER_ERR();
                    fprintf(stderr, 
                            "Unrecognized constraint type `%s'.\n", Str32);
                    return 0;
            }
            if (ConstP->ConstraintType != cUnknownCons) {
                USER_ERR();
                fprintf(stderr,
                  "Type respecified for constraint %s.\n",
                  ConstP->ConstraintName);
                return 0;
            }

            /* See if we misinterpreted the beginning of an explicit 
             * constraint as a user constraint above, and correct if so. 
             * (This works because ALL explicit constraints must precede
             * ALL user constraints.) 
             */

            if (SystemInfo->nu == 1 && constype != cUserCons) {
                SystemInfo->nu = 0;
                INIT_CONSTRAINT(SystemInfo, SystemInfo->nxc++, 
                                ConstP->ConstraintName, constype);
            }

            if (SystemInfo->nu && constype != cUserCons) {
                USER_ERR();
                fprintf(stderr,
              "All explicit constraints must precede all user constraints.\n");
                return 0;
            }
            ConstP->ConstraintType = constype;
            break;
        }

        case cPointWord: {
            int npt, need;
            string32 bodyname;

            if (!ANYCONSTRAINTS || ANYUSERCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr,
      "`Point' keyword is only allowed as part of an explicit constraint.\n");
                return 0;
            }
            if (ConstP->ConstraintType == cUnknownCons) {
                USER_ERR();
                fprintf(stderr,
                  "`Type' keyword expected for constraint `%s'.\n",
                  ConstP->ConstraintName);
                return 0;
            }
            npt = ConstP->npt;
            need = ConstraintInfo[(int)ConstP->ConstraintType].npt;
            if (npt >= need) {
                USER_ERR();
                fprintf(stderr,
                  "Too many points given for constraint `%s' (need %d).\n",
                  ConstP->ConstraintName, need);
                return 0;
            }

            /* expect the body name next */
            if (!FIND_EQUAL(System) || !READ_ID(System, bodyname))
                return 0;
            if ((ConstP->Points[npt].Body = FINDBODY(SystemInfo, bodyname)) 
                 == cUnspecifiedBody) 
            {
                USER_ERR();
                fprintf(stderr,
      "Unknown body `%s' used in point specification for constraint `%s'.\n",
                  bodyname, ConstP->ConstraintName);
                return 0;
            }

            /* got the body, now the point */
            ConstP->Points[npt].Vec = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                        return 0;
                    SINDX(ConstP->Points[npt].Vec, i, Rexpr);
                    ConstP->PointsFlg[npt][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr,
                  "Need 3 numbers in point specification (constraint `%s').\n",
                      ConstP->ConstraintName);
                    return 0;
                }
            }

            ConstP->npt++;
            break;
        }

        case cVectorWord: {
            int nvec, need;
            string32 bodyname;

            if (!ANYCONSTRAINTS || ANYUSERCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr,
      "`Vector' keyword is only allowed as part of an explicit constraint.\n");
                return 0;
            }
            if (ConstP->ConstraintType == cUnknownCons) {
                USER_ERR();
                fprintf(stderr,
                  "`Type' keyword expected for constraint `%s'.\n",
                  ConstP->ConstraintName);
                return 0;
            }
            nvec = ConstP->nvec;
            need = ConstraintInfo[(int)ConstP->ConstraintType].nvec;
            if (nvec >= need) {
                USER_ERR();
                fprintf(stderr,
                  "Too many vectors given for constraint `%s' (need %d).\n",
                  ConstP->ConstraintName, need);
                return 0;
            }

            /* expect the body name next */
            if (!FIND_EQUAL(System) || !READ_ID(System, bodyname))
                return 0;
            if ((ConstP->Vectors[nvec].Body = FINDBODY(SystemInfo, bodyname)) 
                 == cUnspecifiedBody) 
            {
                USER_ERR();
                fprintf(stderr,
      "Unknown body `%s' used in vector specification for constraint `%s'.\n",
                  bodyname, ConstP->ConstraintName);
                return 0;
            }

            /* got the body, now the vector */
            ConstP->Vectors[nvec].Vec = PERM(NEW_VECX(cScalarVal));
            for (i = 0; i < 3; i++) {
                if (!NUMERIC_NEXT(System, &IsNumeric))
                    return 0;
                if (IsNumeric) {
                    if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                        return 0;
                    SINDX(ConstP->Vectors[nvec].Vec, i, Rexpr);
                    ConstP->VectorsFlg[nvec][i] = QuesFlg;
                } else {
                    USER_ERR();
                    fprintf(stderr,
                  "Need 3 numbers in vector specification (constraint `%s').\n",
                      ConstP->ConstraintName);
                    return 0;
                }
            }

            ConstP->nvec++;
            break;
        }

        case cScalarWord: {
            int nsc, need;

            if (!ANYCONSTRAINTS || ANYUSERCONSTRAINTS) {
                USER_ERR();
                fprintf(stderr,
      "`Scalar' keyword is only allowed as part of an explicit constraint.\n");
                return 0;
            }
            if (ConstP->ConstraintType == cUnknownCons) {
                USER_ERR();
                fprintf(stderr,
                  "`Type' keyword expected for constraint `%s'.\n",
                  ConstP->ConstraintName);
                return 0;
            }
            nsc = ConstP->nsc;
            need = ConstraintInfo[(int)ConstP->ConstraintType].nsc;
            if (nsc >= need) {
                USER_ERR();
                fprintf(stderr,
                  "Too many scalars given for constraint `%s' (need %d).\n",
                  ConstP->ConstraintName, need);
                return 0;
            }

            if (!FIND_EQUAL(System) || !NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                if (!GET_REAL_EXPR(System, &ConstP->Scalars[nsc], 
                        &ConstP->ScalarsFlg[nsc]))
                    return 0;
            } else {
                USER_ERR();
                fprintf(stderr,
          "Expected a number or question mark for scalar in constraint `%s'.\n",
                  ConstP->ConstraintName);
                return 0;
            }

            ConstP->nsc++;
            break;
        }

        case cStabvel:
            if (SystemInfo->StabVelExpr) {
                USER_ERR();
                fprintf(stderr, "`stabvel' keyword respecified.\n");
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                if (!GET_REAL_EXPR(System,
                        &SystemInfo->StabVelExpr, &SystemInfo->StabVelFlg))
                    return 0;
            } else {
                USER_ERR();
                fprintf(stderr, "Need a number or `?' for stabvel value.\n");
                return 0;
            }
            break;

        case cStabpos:
            if (SystemInfo->StabPosExpr) {
                USER_ERR();
                fprintf(stderr, "`stabpos' keyword respecified.\n");
                return 0;
            }
            if (!FIND_EQUAL(System))
                return 0;
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 0;
            if (IsNumeric) {
                if (!GET_REAL_EXPR(System,
                        &SystemInfo->StabPosExpr, &SystemInfo->StabPosFlg))
                    return 0;
            } else {
                USER_ERR();
                fprintf(stderr, "Need a number or `?' for stabpos value.\n");
                return 0;
            }
            break;

        default:
            USER_ERR();
            fprintf(stderr,
              "Unrecognized keyword `%s'.\n", ReservedWords[IdNum - 1]);
            return 0;
    }        /* switch */

    return 1;        /*success*/
}

char
DO_INERTIA(FILE *System,
           SystemInfo_t *SystemInfo)
{
    register Index_t i, j, ii, jj;
    register BodyDesc_t *BodyP = &SystemInfo->Bodies[SystemInfo->n - 1];
    int IsNumeric;
    flags_t QuesFlg;
    expr Rexpr;
    expr Inertia[3][3];
    flags_t InerFlg[3][3];

    if (!FIND_EQUAL(System)) return 1;
    BodyP->Inertia = PERM(NEW_MATX(cScalarVal));
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) {
            if (!NUMERIC_NEXT(System, &IsNumeric))
                return 1;
            if (!IsNumeric) {
                if (i == 1 && j == 0) {        /* just got 3 diagonal elts */
                    Inertia[1][1] = Inertia[0][1]; /* put them in right place */
                    InerFlg[1][1] = InerFlg[0][1];
                    Inertia[2][2] = Inertia[0][2];
                    InerFlg[2][2] = InerFlg[0][2];
                    for (ii = 0; ii < 3; ii++)
                        for (jj = 0; jj < 3; jj++)
                            if (ii != jj) {
                                Inertia[ii][jj] = SCALAR_ZERO();
                                InerFlg[ii][jj] = 0;
                            }
                    goto inertia_read;        /* stop looking for inertias */
                } else {
                    USER_ERR();
                    fprintf(stderr,
                      "Need 3 or 9 numbers for inertia for body `%s'.\n",
                      BodyP->BodyName);
                    return 1;
                }
            }
            if (!GET_REAL_EXPR(System, &Rexpr, &QuesFlg)) 
                return 1;
            Inertia[i][j] = Rexpr;
            InerFlg[i][j] = QuesFlg;
        }

    if (!NUMERIC_NEXT(System, &IsNumeric))
        return 1;
    if (IsNumeric) {
        USER_ERR();
        fprintf(stderr,
          "Too many numbers for inertia for body `%s' (need 3 or 9).\n", 
          BodyP->BodyName);
        return 1;
    }

inertia_read: /* successfully read in an inertia matrix */

    /* If off-diagonal question mark on only one side of the
     * diagonal, replace it with the symmetric element.  Note that
     * we're checking `== ISQUESFLG', not `& ISQUESFLG', since we
     * only pull this trick for lone `?', not `<nominal>?'.
     */
    for (ii = 0; ii < 2; ii++)
        for (jj = ii + 1; jj < 3; jj++)
            if (!SAME_EXPR(Inertia[ii][jj], Inertia[jj][ii])
                || InerFlg[ii][jj] != InerFlg[jj][ii]) 
            {
                if (InerFlg[ii][jj] == ISQUESFLG) {
                    Inertia[ii][jj] = Inertia[jj][ii];
                    InerFlg[ii][jj] = InerFlg[jj][ii];
                } else if (InerFlg[jj][ii] == ISQUESFLG) {
                    Inertia[jj][ii] = Inertia[ii][jj];
                    InerFlg[jj][ii] = InerFlg[ii][jj];
                }
            }

    /* Is Inertia symmetric?  A few elements get checked twice here,
     * but it's more convenient this way for setting all the elements.
     */
    for (ii = 0; ii < 3; ii++)
        for (jj = 0; jj < 3; jj++)
            if (!SAME_EXPR(Inertia[ii][jj], Inertia[jj][ii])
                || InerFlg[ii][jj] != InerFlg[jj][ii]) 
            {
                USER_ERR();
                fprintf(stderr, "Asymmetric inertia matrix in body `%s'.\n",
                  BodyP->BodyName);
                return 1;
            } else {
                SINDX2(BodyP->Inertia, ii, jj, Inertia[ii][jj]);
                BodyP->InerFlg[ii][jj] = InerFlg[ii][jj];
            }

    return 0;        /* success */
}
