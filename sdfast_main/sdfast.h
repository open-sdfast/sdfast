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

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <stdio.h>
#include "../libs/libs.h"
#include "../calc/language.h"

/* Operation counts for each computational routine are reported in 
 * structures of this type. 
 *
 *  IF YOU ADD SOMETHING HERE, DON'T FORGET TO ZERO IT IN ZERO_OPCNTS()
 *  IN print.c!
 */
typedef struct {
    long nadd;
    long nmul;
    long ndiv;
    long nassign;
    long nldudcomp,nldubsl,nldubsd,nmfrc,nlhs,nrhs,nfs0,nfsmult,nfsfull,
         nqrdcomp, nqrbslv,ndoww,ndoltau,ndoiner,ndofs0,
         ndomm,ndommldu,ndovpk,nfulltrq,nudot0,nudotm;
} opstats_t;

/*===============*/
/* GLOBAL VARS   */
/*===============*/

/* Computed variables */

/* Each (pseudo) joint is used to compute an entry in the following three
 * variables.  These are then used to make the rest of the code joint-
 * independent (mostly).
 */
#ifndef SDMAIN
extern 
#endif
sym
  Cik,  /* direction cosines across the (psuedo)joint (relative orientation) */
  Wkk,  /* vectors along which rotation occurs, for rotational axes */
  Vkk,  /* vectors along which translation occurs, for sliding axes */
  dik;  /* vectors from inb body's inb hinge pt to this inb hinge pt */

#ifndef SDMAIN
extern 
#endif
sym 
  /* temps globally available for brief use */
  tmat1,       /* temporary matrices */
  tmat2,
  tvec1,       /* temporary vectors */
  tvec2,
  tvec3, 
  tvec4, 
  tvec5, 
  tsc1,        /* temporary scalars */
  tsc2,
  tsc3,

  /* state and state derivatives */
  curtim,  /* the current simulation time */
  q,       /* primary-joint hinge positions */
  qn,      /* normalized version of q */
  u,       /* primary-joint hinge velocities */
  qdot,    /* derivatives of q */
  udot,    /* derivatives of u */

  /* forces */
  ufk,     /* User-supplied forces */
  utk,     /* User-supplied body torques. */
  utau,    /* User-supplied hinge torques. */
  ltau,    /* User-supplied hinge torques for loop hinges. */
  mfk,     /* Multiplier-generated forces and torques */
  mtk,     /*               |                         */
  mtau,    /*               |                         */
  mltau,   /*               V                           */

  /* motions */
  uacc,    /* Prescribed tree hinge accelerations. */
  uvel,    /* Prescribed tree hinge velocities. */
  upos,    /* Prescribed tree hinge positions. */
  lacc,    /* Prescribed loop hinge accelerations. */
  lvel,    /* Prescribed loop hinge velocities. */
  lpos,    /* Prescribed loop hinge positions. */

  ee,      /* Euler parameter stabilization variables */
  stab,

  Cib,     /* direction cosines across whole tree joints */
  Wpk,     /* partial angular velocities */
  VWri,    /* intermediate variable used in Vpk calculation */
  Vpk,     /* partial velocities */
  ping,    /* used instead of Vpk/Wpk in SDREL2CART() with Order(N) */
  hngpt,   /*                     "                                 */
  Otk,     /* inertial angular accelerations */
  AiOiWi,  /* intermediate variable in Atk calculation */
  Atk,     /* inertial linear accelerations */
  gk,      /* gravity vectors for each body */
  Fstar,   /* inertial remainder and active forces */
  IkWk,    /* intermediate variable in Tstar calculation */
  IkbWk,   /* similar to IkWk, used in weld reactions */
  WkIkWk,  /* intermediate variable in Tstar calculation */
  WkIkbWk, /* similar to WkIkbWk, used in weld reactions */
  Tstar,   /* inertial remainder and active torques */
  mm,      /* mass matrix (LHS) */
  mlo,     /* lower triangle of LDU decomposed mm */
  mdi,     /* diagonal of LDU decomposed mm */
  IkWpk,   /* temp used in building mm */
  fs0,        /* force matrix (LHS) (used to calculate fs later) */
  Fstark,  /* temp used for Order(N) force matrix calculations */
  Tstark,  /*        "           */
  fs,      /* force matrix (RHS) */
  Onkb,    /* intermediate variable used in onk calculation */
  onk,     /* angular acceleration of pseudobody k w.r.t. N (in k) */
  onb,     /* angular acceleration of realbody b w.r.t. N (in b) */
  AOnkri,  /* intermediate variable used in ank calculation */
  Ankb,    /* intermediate variable used in ank calculation */
  AnkAtk,  /* intermediate variable used in ank calculation */
  ank,     /* linear accel of pseudobody k's COM w.r.t. N (in N) */
  dyrcom,  /* intermediate variable used in anb calculation */
  anb,     /* linear accel of realbody b's COM w.r.t. N (in N) */
  w0w0,    /* temps used in dyad calculation */
  w1w1,    /*                |               */
  w2w2,    /*                |               */
  w0w1,    /*                |               */
  w0w2,    /*                |               */
  w1w2,    /*                |               */
  w00w11,  /*                |               */
  w00w22,  /*                |               */
  w11w22,  /*                V               */
  dyad,    /* handy acceleration dyadics */

  PH1,     /* Order N gain computation temps */
  PH2,     /*               |                */
  HL1,     /*               |                */
  HL2,     /*               |                */
  P11,     /*               |                */
  Pd,      /*               |                */
  P22,     /*               |                */
  L11,     /*               |                */
  L21,     /*               |                */
  L22,     /*               |                */
  D11,     /*               |                */
  D22,     /*               |                */
  N11,     /*               |                */
  N21,     /*               |                */
  N22,     /*               |                */
  psi11,   /*               |                */
  psi12,   /*               |                */
  psi21,   /*               |                */
  psi22,   /*               |                */
  psiD11,  /*               |                */
  psiD12,  /*               |                */
  psiD21,  /*               |                */
  psiD22,  /*               V                */
  sL11,    /* Order N ldu temps              */
  sL21,    /*        |                       */
  sL22,    /*        |                       */
  sD1,     /*        |                       */
  sD2,     /*        |                       */
  sD1INV,  /*        |                       */
  sD2INV,  /*        |                       */
  sL11D1,  /*        |                       */
  sL22D2,  /*        |                       */
  sD1L21,  /*        V                       */
  DD,      /* Order N gain computation outputs   */
  G1,      /*                |                   */
  G2,      /*                V                   */

  /* kinematics */
  rkWkk,   /* rk X Wkk */
  rpp,     /* translation vector across the joint (rel. displacement) */
  rpk,     /* vec from inb hinge pt to outb COM, adjusted for sliders */
  rik,     /* vec from inb COM to outb COM, adjusted for sliders */
  rik2,    /* vec from inb body's inb hinge pt, to this body's hinge pt */
  rpri,    /* vec from inb COM to outb hinge pt, adjusted for sliders */
  Wik,     /* relative angular velocity vector */
  Vik,     /* relative displacement velocity vector */
  Wirk,    /* intermediate variable in Atk calculation (and elsewhere) */
  Wkrpk,   /* intermediate variable in Atk calculation (and elsewhere) */
  cnk,     /* direction cosine from N to pseudobody frame k */
  cnb,     /* direction cosine from N to realbody frame b */
  rnkg,    /* vector from N origin to ground pseudobody's COM (in N) */
  rnk,     /* vector from N origin to pseudobody k's COM (in N) */
  rnb,     /* vector from N origin to realbody b's COM (in N) */
  wk,      /* angular vel of pseudobody k w.r.t. N, expressed in k */
  wb,      /* angular vel of realbody b w.r.t. N, expressed in b */
  VikWkr,  /* intermediate variable used in Vnk calculation */
  vnk,     /* linear vel of pseudobody k's COM w.r.t. N (in N) */
  wbrcom,  /* intermediate variable used in vnb calculation */
  vnb,     /* linear vel of realbody b's COM w.r.t. N (in N) */
  rikt,    /* rik2*Cik, operator used in Order(N) */
  Iko,     /* inertia referred to inb hinge pt */
  mkrk,    /* mk*TILDA(rk) */


  /* loop joint kinematics */
  lq,      /* secondary-joint hinge positions (pseudo-coordinates) */
  Cibo,    /* convenient direction cosines */
  Cibob,   /*            "                 */
  Woio,    /* convenient angular velocities */
  Woiob,   /*            "                  */
  Ooiob,   /* convenient angular accelerations */
  eul1,    /* loop joint euler angles and euler parameters */
  eul2,    /*                       "                      */
  eul3,    /*                       "                      */
  eul4,    /*                       "                      */
  eul1dot, /* derivatives of loop joint euler angles and euler parameters */
  eul2dot, /*                            "                                */
  eul3dot, /*                            "                                */
  eul4dot, /*                            "                                */
  eul1a,   /* loop joint angular accelerations */
  eul2a,   /*               "                  */
  eul3a,   /*               "                  */
  ceul1,   /* cosines and sines of loop joint euler angles */
  ceul2,   /*                     "                        */
  ceul3,   /*                     "                        */
  seul1,   /*                     "                        */
  seul2,   /*                     "                        */
  seul3,   /*                     "                        */
  sli1,    /* sliding loop joint translations */
  sli2,    /*               "                 */
  sli3,    /*               "                 */
  sli1v,   /* sliding loop joint translation velocities */
  sli2v,   /*                    "                      */
  sli3v,   /*                    "                      */
  sli1a,   /* sliding loop joint translation accelerations */
  sli2a,   /*                       "                      */
  sli3a,   /*                       "                      */
  ipin2x,  /* for gimbal loop jt, ipin2's image in 1st intermediate frame,
              expressed in inboard body's frame */

  /* sdmom and sdsys stuff */
  mtot,    /* total system mass (computed in sdinit) */
  com,     /* system center of mass (computed in sdnsim) */

  /* reaction forces and torques at tree joints */
  fc,      /* reaction forces for tree joints */
  tc,      /* reaction torques for tree joints */
  tauc,    /* hinge torques for tree hinges */
  ffk,     /* accumulation variable in reaction force calculation */
  ttk,     /* accumulation variable in reaction torque calculation */
  fccikt,  /* intermediate variable in reaction force/torque */ 
  ffkb,    /* sum of ufk+ltaufk+mfk for each body */
  ttkb,    /* sum of utk+ltautk+mtk for each body */

  /* loop joint forces and torques */
  lfci,   /* loop joint reaction forces (on inboard body) */
  ltci,   /* loop joint reaction torques (on inboard body) */
  lfc,    /* loop joint reaction forces (on outboard body) */
  ltc,    /* loop joint reaction torques (on outboard body) */
  Tinb,   /* multiplier-generated torque on inboard body of loop joint */
  Toutb,  /* multiplier-generated torque on outboard body of loop joint */
  ltaufk, /* body forces due to loop taus */
  ltautk, /* body torques due to loop taus */
  ltauc,  /* hinge torques for loop joints */
  ltaufi, /* force on inboard body due to taus applied at the loop joint */
  ltaufo, /* force on outboard body              "                       */
  ltauti, /* torque on inboard body              "                       */
  ltauto, /* torque on outboard body             "                       */
  mltaufi, /* force on inboard body due to prescribed motion at loop joint*/
  mltaufo, /* force on outboard body              "                       */
  mltauti, /* torque on inboard body              "                       */
  mltauto, /* torque on outboard body             "                       */

  /* constraint stuff */
  Cio,     /* direction cosines between bodies involved in a loop joint */
  vt1, vt2, vt2a, vt3, vt4, vt5, vt6, vt7,  /* constraint error temporaries */
  vt8, vt9, vt10, vt10a, vt10b, vt10c, vt11, vt12, vt13, vt13a,
  vt14, vt15, vt16, vt17, vt18, vt19, vt20, vt21, vt22, vt23,
  vt24, vt25, vt26, vt26a, vt27,
  perr,  /* position constraint errors */
  verr,  /* velocity constraint errors */
  aerr,  /* acceleration constraint errors */
  mult,  /* constraint multipliers */

  /* loop joint parameters for the internal loop joint model */
  ipin,  
  ipin2, /* gimbals only (gimbal,bearing,bushing) */
  ghand, /* gimbal handedness 1=right -1=left */
  iref, 
  iperp, 
  opin,
  oref,
  operp 
  ;

typedef struct {
    sym umult_, lfci_, lfc_, ltc_, ltci_, Tinb_, Toutb_;   /* temporaries */
    sym ltaufi_, ltaufo_, ltauti_, ltauto_;                /*     "       */
    sym tmpv1_, tmpv2_, tmpv3_;                            /*     "       */
    sym fk_, tk_, tau_;                                    /* outputs */
} mfrcsym_t;

/* Global expressions */

#ifndef SDMAIN
extern 
#endif
expr 
/* This expression is used as part of security checking in MAIN. */
  UtoQexpr;

#ifndef SDMAIN
extern 
#endif
SystemInfo_t SysI; /* This is the parsed form of the input file. */

#ifndef SDMAIN
extern 
#endif
double gStartTime;  /* clock setting at beginning of program.  All CPU */
                    /* times are relative to this time.                */

#ifndef SDMAIN
extern 
#endif
long gGenerateTime;  /* time of day (hhmmss) at which this run of sdfast
                        was begun. */

// expr BALL_COS(), PIN_COS(), U_COS();
int64_t BYTES_USED();
