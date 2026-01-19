/*
Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123)

ROADMAP (sphere.sd)

Bodies           Inb
No  Name         body Joint type  Coords q         Multipliers
--- ------------ ---- ----------- ---------------- -----------------------
 -1 $ground                                       |
  0 lat_and_long  -1  U-joint       0   1         |
  1 radius         0  Sliding       2?            |  0p
  2 inner          1  Ball          3   4   5   6 |

User Constraints

  0 slipx                                         |  1
  1 slipz                                         |  2

*/
#include <math.h>
#include <stdio.h>

typedef struct {
    int ground_,nbod_,ndof_,ncons_,nloop_,nldof_,nloopc_,nball_,nlball_,npres_,
      nuser_;
    int jtype_[3],inb_[3],outb_[3],njntdof_[3],njntc_[3],njntp_[3],firstq_[3],
      ballq_[3],firstm_[3],firstp_[3];
    int trans_[6],firstu_[2];
} sdgtopo_t;
#define ground (sdgtopo.ground_)
#define nbod (sdgtopo.nbod_)
#define ndof (sdgtopo.ndof_)
#define ncons (sdgtopo.ncons_)
#define nloop (sdgtopo.nloop_)
#define nldof (sdgtopo.nldof_)
#define nloopc (sdgtopo.nloopc_)
#define nball (sdgtopo.nball_)
#define nlball (sdgtopo.nlball_)
#define npres (sdgtopo.npres_)
#define nuser (sdgtopo.nuser_)
#define jtype (sdgtopo.jtype_)
#define inb (sdgtopo.inb_)
#define outb (sdgtopo.outb_)
#define njntdof (sdgtopo.njntdof_)
#define njntc (sdgtopo.njntc_)
#define njntp (sdgtopo.njntp_)
#define firstq (sdgtopo.firstq_)
#define ballq (sdgtopo.ballq_)
#define firstm (sdgtopo.firstm_)
#define firstp (sdgtopo.firstp_)
#define trans (sdgtopo.trans_)
#define firstu (sdgtopo.firstu_)

typedef struct {
    double grav_[3],mk_[3],ik_[3][3][3],pin_[6][3];
    double rk_[3][3],ri_[3][3],pres_[6],stabvel_,stabpos_;
    int mfrcflg_,roustate_,vpkflg_,inerflg_,mmflg_,mmlduflg_,wwflg_,ltauflg_,
      fs0flg_,ii_,mmap_[6];
    int gravq_[3],mkq_[3],ikq_[3][3][3],pinq_[6][3],rkq_[3][3],riq_[3][3],presq_
      [6],stabvelq_,stabposq_;
    double mtot_,psmkg_,rhead_[3][3],rcom_[3][3],mkrcomt_[3][3][3],psikg_[3][3],
      psrcomg_[3],psrkg_[3],psrig_[3],psmk_[6],psik_[6][3][3],psrcom_[6][3],
      psrk_[6][3],psri_[6][3];
} sdginput_t;
#define grav (sdginput.grav_)
#define mk (sdginput.mk_)
#define ik (sdginput.ik_)
#define pin (sdginput.pin_)
#define rk (sdginput.rk_)
#define ri (sdginput.ri_)
#define pres (sdginput.pres_)
#define stabvel (sdginput.stabvel_)
#define stabpos (sdginput.stabpos_)
#define rhead (sdginput.rhead_)
#define rcom (sdginput.rcom_)
#define psrcomg (sdginput.psrcomg_)
#define psrcom (sdginput.psrcom_)
#define mkrcomt (sdginput.mkrcomt_)
#define psmk (sdginput.psmk_)
#define psik (sdginput.psik_)
#define psrk (sdginput.psrk_)
#define psri (sdginput.psri_)
#define psmkg (sdginput.psmkg_)
#define psikg (sdginput.psikg_)
#define psrkg (sdginput.psrkg_)
#define psrig (sdginput.psrig_)
#define mtot (sdginput.mtot_)
#define mfrcflg (sdginput.mfrcflg_)
#define roustate (sdginput.roustate_)
#define vpkflg (sdginput.vpkflg_)
#define inerflg (sdginput.inerflg_)
#define mmflg (sdginput.mmflg_)
#define mmlduflg (sdginput.mmlduflg_)
#define wwflg (sdginput.wwflg_)
#define ltauflg (sdginput.ltauflg_)
#define fs0flg (sdginput.fs0flg_)
#define ii (sdginput.ii_)
#define mmap (sdginput.mmap_)
#define gravq (sdginput.gravq_)
#define mkq (sdginput.mkq_)
#define ikq (sdginput.ikq_)
#define pinq (sdginput.pinq_)
#define rkq (sdginput.rkq_)
#define riq (sdginput.riq_)
#define presq (sdginput.presq_)
#define stabvelq (sdginput.stabvelq_)
#define stabposq (sdginput.stabposq_)

typedef struct {
    double curtim_,q_[7],qn_[7],u_[6],cnk_[6][3][3],cnb_[3][3][3];
    double rnk_[6][3],vnk_[6][3],wk_[6][3],rnb_[3][3],vnb_[3][3],wb_[3][3],
      wbrcom_[3][3],com_[3],rnkg_[3];
    double Cik_[6][3][3],rikt_[6][3][3],Iko_[6][3][3],mkrk_[6][3][3],Cib_[3][3][
      3];
    double Wkk_[6][3],Vkk_[6][3],dik_[6][3],rpp_[6][3],rpk_[6][3],rik_[6][3],
      rik2_[6][3];
    double rpri_[6][3],Wik_[6][3],Vik_[6][3],Wirk_[6][3],rkWkk_[6][3],Wkrpk_[6][
      3],VikWkr_[6][3];
    double perr_[3],verr_[3],aerr_[3],mult_[3],ufk_[3][3],utk_[3][3],mfk_[3][3],
      mtk_[3][3];
    double utau_[6],mtau_[6],uacc_[6],uvel_[6],upos_[7];
    double s0_,c0_,s1_,c1_;
} sdgstate_t;
#define curtim (sdgstate.curtim_)
#define q (sdgstate.q_)
#define qn (sdgstate.qn_)
#define u (sdgstate.u_)
#define cnk (sdgstate.cnk_)
#define cnb (sdgstate.cnb_)
#define rnkg (sdgstate.rnkg_)
#define rnk (sdgstate.rnk_)
#define rnb (sdgstate.rnb_)
#define vnk (sdgstate.vnk_)
#define vnb (sdgstate.vnb_)
#define wk (sdgstate.wk_)
#define wb (sdgstate.wb_)
#define com (sdgstate.com_)
#define Cik (sdgstate.Cik_)
#define Cib (sdgstate.Cib_)
#define rikt (sdgstate.rikt_)
#define Iko (sdgstate.Iko_)
#define mkrk (sdgstate.mkrk_)
#define Wkk (sdgstate.Wkk_)
#define Vkk (sdgstate.Vkk_)
#define dik (sdgstate.dik_)
#define rpp (sdgstate.rpp_)
#define rpk (sdgstate.rpk_)
#define rik (sdgstate.rik_)
#define rik2 (sdgstate.rik2_)
#define rpri (sdgstate.rpri_)
#define Wik (sdgstate.Wik_)
#define Vik (sdgstate.Vik_)
#define Wirk (sdgstate.Wirk_)
#define rkWkk (sdgstate.rkWkk_)
#define Wkrpk (sdgstate.Wkrpk_)
#define VikWkr (sdgstate.VikWkr_)
#define wbrcom (sdgstate.wbrcom_)
#define perr (sdgstate.perr_)
#define verr (sdgstate.verr_)
#define aerr (sdgstate.aerr_)
#define mult (sdgstate.mult_)
#define ufk (sdgstate.ufk_)
#define utk (sdgstate.utk_)
#define utau (sdgstate.utau_)
#define mfk (sdgstate.mfk_)
#define mtk (sdgstate.mtk_)
#define mtau (sdgstate.mtau_)
#define uacc (sdgstate.uacc_)
#define uvel (sdgstate.uvel_)
#define upos (sdgstate.upos_)
#define s0 (sdgstate.s0_)
#define c0 (sdgstate.c0_)
#define s1 (sdgstate.s1_)
#define c1 (sdgstate.c1_)

typedef struct {
    double fs0_[6],qdot_[7],Otk_[6][3],Atk_[6][3],AiOiWi_[6][3],Fstar_[6][3];
    double Tstar_[6][3],Fstark_[6][3],Tstark_[6][3],IkWk_[6][3],WkIkWk_[6][3],
      gk_[6][3],IkbWk_[3][3],WkIkbWk_[3][3];
    double w0w0_[3],w1w1_[3],w2w2_[3],w0w1_[3],w0w2_[3],w1w2_[3];
    double w00w11_[3],w00w22_[3],w11w22_[3],ww_[3][3],qraux_[3];
    double DD_[6],PH1_[6][3],PH2_[6][3],HL1_[6][3],HL2_[6][3],G1_[6][3],G2_[6][3
      ];
    double P11_[6][3][3],Pd_[6][3][3],P22_[6][3][3],L11_[6][3][3],L21_[6][3][3],
      L22_[6][3][3],D11_[6][3][3],D22_[6][3][3];
    double N11_[6][3][3],N21_[6][3][3],N22_[6][3][3],psi11_[6][3][3],psi12_[6][3
      ][3],psi21_[6][3][3],psi22_[6][3][3];
    double psiD11_[6][3][3],psiD12_[6][3][3],psiD21_[6][3][3],psiD22_[6][3][3];
    double sL11_[3][3],sL21_[3][3],sL22_[3][3],sD1_[3][3],sD2_[3][3];
    double sD1INV_[3][3],sD2INV_[3][3],sL11D1_[3][3],sL22D2_[3][3],sD1L21_[3][3]
      ;
    double ping_[6][3],hngpt_[6][3];
    int wmap_[3],multmap_[3],jpvt_[3],wsiz_,wrank_;
} sdglhs_t;
#define qdot (sdglhs.qdot_)
#define Otk (sdglhs.Otk_)
#define Atk (sdglhs.Atk_)
#define AiOiWi (sdglhs.AiOiWi_)
#define Fstar (sdglhs.Fstar_)
#define Tstar (sdglhs.Tstar_)
#define fs0 (sdglhs.fs0_)
#define Fstark (sdglhs.Fstark_)
#define Tstark (sdglhs.Tstark_)
#define IkWk (sdglhs.IkWk_)
#define IkbWk (sdglhs.IkbWk_)
#define WkIkWk (sdglhs.WkIkWk_)
#define WkIkbWk (sdglhs.WkIkbWk_)
#define gk (sdglhs.gk_)
#define w0w0 (sdglhs.w0w0_)
#define w1w1 (sdglhs.w1w1_)
#define w2w2 (sdglhs.w2w2_)
#define w0w1 (sdglhs.w0w1_)
#define w0w2 (sdglhs.w0w2_)
#define w1w2 (sdglhs.w1w2_)
#define w00w11 (sdglhs.w00w11_)
#define w00w22 (sdglhs.w00w22_)
#define w11w22 (sdglhs.w11w22_)
#define ww (sdglhs.ww_)
#define qraux (sdglhs.qraux_)
#define PH1 (sdglhs.PH1_)
#define PH2 (sdglhs.PH2_)
#define P11 (sdglhs.P11_)
#define Pd (sdglhs.Pd_)
#define P22 (sdglhs.P22_)
#define L11 (sdglhs.L11_)
#define L21 (sdglhs.L21_)
#define L22 (sdglhs.L22_)
#define D11 (sdglhs.D11_)
#define D22 (sdglhs.D22_)
#define N11 (sdglhs.N11_)
#define N21 (sdglhs.N21_)
#define N22 (sdglhs.N22_)
#define HL1 (sdglhs.HL1_)
#define HL2 (sdglhs.HL2_)
#define psi11 (sdglhs.psi11_)
#define psi12 (sdglhs.psi12_)
#define psi21 (sdglhs.psi21_)
#define psi22 (sdglhs.psi22_)
#define psiD11 (sdglhs.psiD11_)
#define psiD12 (sdglhs.psiD12_)
#define psiD21 (sdglhs.psiD21_)
#define psiD22 (sdglhs.psiD22_)
#define sL11 (sdglhs.sL11_)
#define sL21 (sdglhs.sL21_)
#define sL22 (sdglhs.sL22_)
#define sD1 (sdglhs.sD1_)
#define sD2 (sdglhs.sD2_)
#define sD1INV (sdglhs.sD1INV_)
#define sD2INV (sdglhs.sD2INV_)
#define sL11D1 (sdglhs.sL11D1_)
#define sL22D2 (sdglhs.sL22D2_)
#define sD1L21 (sdglhs.sD1L21_)
#define DD (sdglhs.DD_)
#define G1 (sdglhs.G1_)
#define G2 (sdglhs.G2_)
#define ping (sdglhs.ping_)
#define hngpt (sdglhs.hngpt_)
#define wmap (sdglhs.wmap_)
#define multmap (sdglhs.multmap_)
#define jpvt (sdglhs.jpvt_)
#define wsiz (sdglhs.wsiz_)
#define wrank (sdglhs.wrank_)

typedef struct {
    double fs_[6],udot_[6],tauc_[6],dyad_[3][3][3],fc_[6][3],tc_[6][3];
    double ank_[6][3],onk_[6][3],Onkb_[6][3],AOnkri_[6][3],Ankb_[6][3],AnkAtk_[6
      ][3],anb_[3][3],onb_[3][3],dyrcom_[3][3];
    double ffk_[6][3],ttk_[6][3],fccikt_[6][3],ffkb_[3][3],ttkb_[3][3];
} sdgrhs_t;
#define fs (sdgrhs.fs_)
#define udot (sdgrhs.udot_)
#define ank (sdgrhs.ank_)
#define anb (sdgrhs.anb_)
#define onk (sdgrhs.onk_)
#define onb (sdgrhs.onb_)
#define Onkb (sdgrhs.Onkb_)
#define AOnkri (sdgrhs.AOnkri_)
#define Ankb (sdgrhs.Ankb_)
#define AnkAtk (sdgrhs.AnkAtk_)
#define dyrcom (sdgrhs.dyrcom_)
#define ffk (sdgrhs.ffk_)
#define ttk (sdgrhs.ttk_)
#define fccikt (sdgrhs.fccikt_)
#define ffkb (sdgrhs.ffkb_)
#define ttkb (sdgrhs.ttkb_)
#define dyad (sdgrhs.dyad_)
#define fc (sdgrhs.fc_)
#define tc (sdgrhs.tc_)
#define tauc (sdgrhs.tauc_)

typedef struct {
    double temp_[3000],tmat1_[3][3],tmat2_[3][3],tvec1_[3],tvec2_[3],tvec3_[3],
      tvec4_[3],tvec5_[3];
    double tsc1_,tsc2_,tsc3_;
} sdgtemp_t;
#define temp (sdgtemp.temp_)
#define tmat1 (sdgtemp.tmat1_)
#define tmat2 (sdgtemp.tmat2_)
#define tvec1 (sdgtemp.tvec1_)
#define tvec2 (sdgtemp.tvec2_)
#define tvec3 (sdgtemp.tvec3_)
#define tvec4 (sdgtemp.tvec4_)
#define tvec5 (sdgtemp.tvec5_)
#define tsc1 (sdgtemp.tsc1_)
#define tsc2 (sdgtemp.tsc2_)
#define tsc3 (sdgtemp.tsc3_)

sdgtopo_t sdgtopo = {
/*  Topological information
*/
    /* ground */ 1,
    /* nbod */ 3,
    /* ndof */ 6,
    /* ncons */ 3,
    /* nloop */ 0,
    /* nldof */ 0,
    /* nloopc */ 0,
    /* nball */ 1,
    /* nlball */ 0,
    /* npres */ 1,
    /* nuser */ 2,
    /* jtype[0] */ 2,
    /* jtype[1] */ 5,
    /* jtype[2] */ 4,
    /* inb[0] */ -1,
    /* inb[1] */ 0,
    /* inb[2] */ 1,
    /* outb[0] */ 0,
    /* outb[1] */ 1,
    /* outb[2] */ 2,
    /* njntdof[0] */ 2,
    /* njntdof[1] */ 1,
    /* njntdof[2] */ 3,
    /* njntc[0] */ 0,
    /* njntc[1] */ 0,
    /* njntc[2] */ 0,
    /* njntp[0] */ 0,
    /* njntp[1] */ 1,
    /* njntp[2] */ 0,
    /* firstq[0] */ 0,
    /* firstq[1] */ 2,
    /* firstq[2] */ 3,
    /* ballq[0] */ -104,
    /* ballq[1] */ -104,
    /* ballq[2] */ 6,
    /* firstm[0] */ -1,
    /* firstm[1] */ -1,
    /* firstm[2] */ -1,
    /* firstp[0] */ -1,
    /* firstp[1] */ 0,
    /* firstp[2] */ -1,
    /* trans[0] */ 0,
    /* trans[1] */ 0,
    /* trans[2] */ 1,
    /* trans[3] */ 0,
    /* trans[4] */ 0,
    /* trans[5] */ 0,
    /* firstu[0] */ 1,
    /* firstu[1] */ 2,
};
sdginput_t sdginput = {
/* Model parameters from the input file */

/* gravity */
    /* grav[0] */ 0.,
    /* grav[1] */ -9.8,
    /* grav[2] */ 0.,

/* mass */
    /* mk[0] */ 0.,
    /* mk[1] */ 0.,
    /* mk[2] */ 1.,

/* inertia */
    /* ik[0][0][0] */ 0.,
    /* ik[0][0][1] */ 0.,
    /* ik[0][0][2] */ 0.,
    /* ik[0][1][0] */ 0.,
    /* ik[0][1][1] */ 0.,
    /* ik[0][1][2] */ 0.,
    /* ik[0][2][0] */ 0.,
    /* ik[0][2][1] */ 0.,
    /* ik[0][2][2] */ 0.,
    /* ik[1][0][0] */ 0.,
    /* ik[1][0][1] */ 0.,
    /* ik[1][0][2] */ 0.,
    /* ik[1][1][0] */ 0.,
    /* ik[1][1][1] */ 0.,
    /* ik[1][1][2] */ 0.,
    /* ik[1][2][0] */ 0.,
    /* ik[1][2][1] */ 0.,
    /* ik[1][2][2] */ 0.,
    /* ik[2][0][0] */ .4,
    /* ik[2][0][1] */ 0.,
    /* ik[2][0][2] */ 0.,
    /* ik[2][1][0] */ 0.,
    /* ik[2][1][1] */ .4,
    /* ik[2][1][2] */ 0.,
    /* ik[2][2][0] */ 0.,
    /* ik[2][2][1] */ 0.,
    /* ik[2][2][2] */ .4,

/* tree hinge axis vectors */
    /* pin[0][0] */ 0.,
    /* pin[0][1] */ 1.,
    /* pin[0][2] */ 0.,
    /* pin[1][0] */ 1.,
    /* pin[1][1] */ 0.,
    /* pin[1][2] */ 0.,
    /* pin[2][0] */ 0.,
    /* pin[2][1] */ 1.,
    /* pin[2][2] */ 0.,
    /* pin[3][0] */ 0.,
    /* pin[3][1] */ 0.,
    /* pin[3][2] */ 0.,
    /* pin[4][0] */ 0.,
    /* pin[4][1] */ 0.,
    /* pin[4][2] */ 0.,
    /* pin[5][0] */ 0.,
    /* pin[5][1] */ 0.,
    /* pin[5][2] */ 0.,

/* tree bodytojoint vectors */
    /* rk[0][0] */ 0.,
    /* rk[0][1] */ 0.,
    /* rk[0][2] */ 0.,
    /* rk[1][0] */ 0.,
    /* rk[1][1] */ 0.,
    /* rk[1][2] */ 0.,
    /* rk[2][0] */ 0.,
    /* rk[2][1] */ 0.,
    /* rk[2][2] */ 0.,

/* tree inbtojoint vectors */
    /* ri[0][0] */ 0.,
    /* ri[0][1] */ 0.,
    /* ri[0][2] */ 0.,
    /* ri[1][0] */ 0.,
    /* ri[1][1] */ 0.,
    /* ri[1][2] */ 0.,
    /* ri[2][0] */ 0.,
    /* ri[2][1] */ 0.,
    /* ri[2][2] */ 0.,

/* tree prescribed motion */
    /* pres[0] */ 0.,
    /* pres[1] */ 0.,
    /* pres[2] */ 0.,
    /* pres[3] */ 0.,
    /* pres[4] */ 0.,
    /* pres[5] */ 0.,

/* stabilization parameters */
    /* stabvel */ 0.,
    /* stabpos */ 0.,

/* miscellaneous */
    /* mfrcflg */ 0,
    /* roustate */ 0,
    /* vpkflg */ 0,
    /* inerflg */ 0,
    /* mmflg */ 0,
    /* mmlduflg */ 0,
    /* wwflg */ 0,
    /* ltauflg */ 0,
    /* fs0flg */ 0,
    /* ii */ 0,
    /* mmap[0] */ 0,
    /* mmap[1] */ 1,
    /* mmap[2] */ 2,
    /* mmap[3] */ 3,
    /* mmap[4] */ 4,
    /* mmap[5] */ 5,

/* Which parameters were "?" (1) or "<nominal>?" (3) */
    /* gravq[0] */ 0,
    /* gravq[1] */ 0,
    /* gravq[2] */ 0,
    /* mkq[0] */ 0,
    /* mkq[1] */ 0,
    /* mkq[2] */ 0,
    /* ikq[0][0][0] */ 0,
    /* ikq[0][0][1] */ 0,
    /* ikq[0][0][2] */ 0,
    /* ikq[0][1][0] */ 0,
    /* ikq[0][1][1] */ 0,
    /* ikq[0][1][2] */ 0,
    /* ikq[0][2][0] */ 0,
    /* ikq[0][2][1] */ 0,
    /* ikq[0][2][2] */ 0,
    /* ikq[1][0][0] */ 0,
    /* ikq[1][0][1] */ 0,
    /* ikq[1][0][2] */ 0,
    /* ikq[1][1][0] */ 0,
    /* ikq[1][1][1] */ 0,
    /* ikq[1][1][2] */ 0,
    /* ikq[1][2][0] */ 0,
    /* ikq[1][2][1] */ 0,
    /* ikq[1][2][2] */ 0,
    /* ikq[2][0][0] */ 0,
    /* ikq[2][0][1] */ 0,
    /* ikq[2][0][2] */ 0,
    /* ikq[2][1][0] */ 0,
    /* ikq[2][1][1] */ 0,
    /* ikq[2][1][2] */ 0,
    /* ikq[2][2][0] */ 0,
    /* ikq[2][2][1] */ 0,
    /* ikq[2][2][2] */ 0,
    /* pinq[0][0] */ 0,
    /* pinq[0][1] */ 0,
    /* pinq[0][2] */ 0,
    /* pinq[1][0] */ 0,
    /* pinq[1][1] */ 0,
    /* pinq[1][2] */ 0,
    /* pinq[2][0] */ 0,
    /* pinq[2][1] */ 0,
    /* pinq[2][2] */ 0,
    /* pinq[3][0] */ 0,
    /* pinq[3][1] */ 0,
    /* pinq[3][2] */ 0,
    /* pinq[4][0] */ 0,
    /* pinq[4][1] */ 0,
    /* pinq[4][2] */ 0,
    /* pinq[5][0] */ 0,
    /* pinq[5][1] */ 0,
    /* pinq[5][2] */ 0,
    /* rkq[0][0] */ 0,
    /* rkq[0][1] */ 0,
    /* rkq[0][2] */ 0,
    /* rkq[1][0] */ 0,
    /* rkq[1][1] */ 0,
    /* rkq[1][2] */ 0,
    /* rkq[2][0] */ 0,
    /* rkq[2][1] */ 0,
    /* rkq[2][2] */ 0,
    /* riq[0][0] */ 0,
    /* riq[0][1] */ 0,
    /* riq[0][2] */ 0,
    /* riq[1][0] */ 0,
    /* riq[1][1] */ 0,
    /* riq[1][2] */ 0,
    /* riq[2][0] */ 0,
    /* riq[2][1] */ 0,
    /* riq[2][2] */ 0,
    /* presq[0] */ 0,
    /* presq[1] */ 0,
    /* presq[2] */ 3,
    /* presq[3] */ 0,
    /* presq[4] */ 0,
    /* presq[5] */ 0,
    /* stabvelq */ 3,
    /* stabposq */ 3,

/* End of values from input file */

};
sdgstate_t sdgstate;
sdglhs_t sdglhs;
sdgrhs_t sdgrhs;
sdgtemp_t sdgtemp;


void sdinit(void)
{
/*
Initialization routine


 This routine must be called before the first call to sdstate(), after
 supplying values for any `?' parameters in the input.
*/
    double sumsq,norminv;
    int i,j,k;


/* Check that all `?' parameters have been assigned values */

    for (k = 0; k < 3; k++) {
        if (gravq[k] == 1) {
            sdseterr(7,25);
        }
    }
    for (k = 0; k < 3; k++) {
        if (mkq[k] == 1) {
            sdseterr(7,26);
        }
        for (i = 0; i < 3; i++) {
            if (rkq[k][i] == 1) {
                sdseterr(7,29);
            }
            if (riq[k][i] == 1) {
                sdseterr(7,30);
            }
            for (j = 0; j < 3; j++) {
                if (ikq[k][i][j] == 1) {
                    sdseterr(7,27);
                }
            }
        }
    }
    for (k = 0; k < 6; k++) {
        for (i = 0; i < 3; i++) {
            if (pinq[k][i] == 1) {
                sdseterr(7,28);
            }
        }
    }

/* Normalize pin vectors if necessary */


/* Zero out ping and hngpt */

    for (i = 0; i < 6; i++) {
        for (j = 0; j < 3; j++) {
            ping[i][j] = 0.;
            hngpt[i][j] = 0.;
        }
    }

/* Compute pseudobody-related constants */

    rcom[0][0] = 0.;
    rcom[0][1] = 0.;
    rcom[0][2] = 0.;
    rcom[1][0] = 0.;
    rcom[1][1] = 0.;
    rcom[1][2] = 0.;
    rcom[2][0] = 0.;
    rcom[2][1] = 0.;
    rcom[2][2] = 0.;

/* Compute mass properties-related constants */

    mtot = 1.;
    sdserialno(&i);
    if (i != 30123) {
        sdseterr(7,41);
    }
    roustate = 1;
}

/* Convert state to form using 1-2-3 Euler angles for ball joints. */

void sdst2ang(double st[7],
    double stang[6])
{
    int i;
    double dc[3][3];

    for (i = 0; i < 6; i++) {
        stang[i] = st[i];
    }
    sdquat2dc(st[3],st[4],st[5],st[6],dc);
    sddc2ang(dc,&stang[3],&stang[4],&stang[5]);
}

/* Convert 1-2-3 form of state back to Euler parameters for ball joints. */

void sdang2st(double stang[6],
    double st[7])
{
    int i;
    double dc[3][3];

    for (i = 0; i < 6; i++) {
        st[i] = stang[i];
    }
    sdang2dc(stang[3],stang[4],stang[5],dc);
    sddc2quat(dc,&st[3],&st[4],&st[5],&st[6]);
}

/* Normalize Euler parameters in state. */

void sdnrmsterr(double st[7],
    double normst[7],
    int routine)
{
    int i;
    double norm;

    for (i = 0; i < 7; i++) {
        normst[i] = st[i];
    }
    norm = sqrt(st[3]*st[3]+st[4]*st[4]+st[5]*st[5]+st[6]*st[6]);
    if (routine != 0) {
        if ((norm < .9) || (norm > 1.1)) {
            sdseterr(routine,14);
        }
    }
    if (norm == 0.) {
        normst[6] = 1.;
        norm = 1.;
    }
    norm = 1./norm;
    normst[3] = normst[3]*norm;
    normst[4] = normst[4]*norm;
    normst[5] = normst[5]*norm;
    normst[6] = normst[6]*norm;
}

void sdnormst(double st[7],
    double normst[7])
{

    sdnrmsterr(st,normst,0);
}

void sdstate(double timein,
    double qin[7],
    double uin[6])
{
/*
Compute kinematic information and store it in sdgstate.

Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123) on machine ID unknown
Copyright (c) 1990-1997 Symbolic Dynamics, Inc.
Copyright (c) 1990-1997 Parametric Technology Corp.
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.
Government is subject to restrictions as set forth in subparagraph
(c)(1)(ii) of the Rights in Technical Data and Computer Software
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041
*/
    int i,j,qchg,uchg;
    double ee,stab;

    if ((roustate != 1) && (roustate != 2) && (roustate != 3)) {
        sdseterr(8,22);
        return;
    }
    if (roustate == 1) {
        for (i = 0; i < 6; i++) {
            if (presq[i] == 1) {
                sdseterr(8,31);
            }
        }
    }
/*
See if time or any qs have changed since last call
*/
    if ((roustate != 1) && (timein == curtim)) {
        qchg = 0;
        for (i = 0; i < 7; i++) {
            if (qin[i] != q[i]) {
                qchg = 1;
                break;
            }
        }
    } else {
        qchg = 1;
    }
/*
If time and qs are unchanged, check us
*/
    if (qchg == 0) {
        uchg = 0;
        for (i = 0; i < 6; i++) {
            if (uin[i] != u[i]) {
                uchg = 1;
                break;
            }
        }
    } else {
        uchg = 1;
    }
    curtim = timein;
    roustate = 2;
    if (qchg == 0) {
        goto skipqs;
    }
/*
Position-related variables need to be computed
*/
    vpkflg = 0;
    mmflg = 0;
    mmlduflg = 0;
    wwflg = 0;
    for (i = 0; i < 7; i++) {
        q[i] = qin[i];
    }
/*
Normalize Euler parameters in state
*/
    sdnrmsterr(q,qn,8);
/*
Compute sines and cosines of q
*/
    s0 = sin(q[0]);
    c0 = cos(q[0]);
    s1 = sin(q[1]);
    c1 = cos(q[1]);
/*
Compute across-axis direction cosines Cik
*/
    Cik[3][0][0] = (1.-(2.*((qn[4]*qn[4])+(qn[5]*qn[5]))));
    Cik[3][0][1] = (2.*((qn[3]*qn[4])-(qn[5]*qn[6])));
    Cik[3][0][2] = (2.*((qn[3]*qn[5])+(qn[4]*qn[6])));
    Cik[3][1][0] = (2.*((qn[3]*qn[4])+(qn[5]*qn[6])));
    Cik[3][1][1] = (1.-(2.*((qn[3]*qn[3])+(qn[5]*qn[5]))));
    Cik[3][1][2] = (2.*((qn[4]*qn[5])-(qn[3]*qn[6])));
    Cik[3][2][0] = (2.*((qn[3]*qn[5])-(qn[4]*qn[6])));
    Cik[3][2][1] = (2.*((qn[3]*qn[6])+(qn[4]*qn[5])));
    Cik[3][2][2] = (1.-(2.*((qn[3]*qn[3])+(qn[4]*qn[4]))));
/*
Compute across-joint direction cosines Cib
*/
    Cib[0][0][1] = (s0*s1);
    Cib[0][0][2] = (s0*c1);
    Cib[0][2][1] = (s1*c0);
    Cib[0][2][2] = (c0*c1);
/*
Compute gravity
*/
    gk[3][0] = (9.8*((Cik[3][2][0]*s1)-(Cik[3][1][0]*c1)));
    gk[3][1] = (9.8*((Cik[3][2][1]*s1)-(Cik[3][1][1]*c1)));
    gk[3][2] = (9.8*((Cik[3][2][2]*s1)-(Cik[3][1][2]*c1)));
/*
Compute cnk & cnb (direction cosines in N)
*/
    cnk[1][0][1] = (s0*s1);
    cnk[1][0][2] = (s0*c1);
    cnk[1][2][1] = (s1*c0);
    cnk[1][2][2] = (c0*c1);
    cnk[3][0][0] = ((Cik[3][2][0]*cnk[1][0][2])+((Cik[3][0][0]*c0)+(Cik[3][1][0]
      *cnk[1][0][1])));
    cnk[3][0][1] = ((Cik[3][2][1]*cnk[1][0][2])+((Cik[3][0][1]*c0)+(Cik[3][1][1]
      *cnk[1][0][1])));
    cnk[3][0][2] = ((Cik[3][2][2]*cnk[1][0][2])+((Cik[3][0][2]*c0)+(Cik[3][1][2]
      *cnk[1][0][1])));
    cnk[3][1][0] = ((Cik[3][1][0]*c1)-(Cik[3][2][0]*s1));
    cnk[3][1][1] = ((Cik[3][1][1]*c1)-(Cik[3][2][1]*s1));
    cnk[3][1][2] = ((Cik[3][1][2]*c1)-(Cik[3][2][2]*s1));
    cnk[3][2][0] = ((Cik[3][2][0]*cnk[1][2][2])+((Cik[3][1][0]*cnk[1][2][1])-(
      Cik[3][0][0]*s0)));
    cnk[3][2][1] = ((Cik[3][2][1]*cnk[1][2][2])+((Cik[3][1][1]*cnk[1][2][1])-(
      Cik[3][0][1]*s0)));
    cnk[3][2][2] = ((Cik[3][2][2]*cnk[1][2][2])+((Cik[3][1][2]*cnk[1][2][1])-(
      Cik[3][0][2]*s0)));
    cnb[0][0][0] = c0;
    cnb[0][0][1] = cnk[1][0][1];
    cnb[0][0][2] = cnk[1][0][2];
    cnb[0][1][0] = 0.;
    cnb[0][1][1] = c1;
    cnb[0][1][2] = -s1;
    cnb[0][2][0] = -s0;
    cnb[0][2][1] = cnk[1][2][1];
    cnb[0][2][2] = cnk[1][2][2];
    cnb[1][0][0] = c0;
    cnb[1][0][1] = cnk[1][0][1];
    cnb[1][0][2] = cnk[1][0][2];
    cnb[1][1][0] = 0.;
    cnb[1][1][1] = c1;
    cnb[1][1][2] = -s1;
    cnb[1][2][0] = -s0;
    cnb[1][2][1] = cnk[1][2][1];
    cnb[1][2][2] = cnk[1][2][2];
    cnb[2][0][0] = cnk[3][0][0];
    cnb[2][0][1] = cnk[3][0][1];
    cnb[2][0][2] = cnk[3][0][2];
    cnb[2][1][0] = cnk[3][1][0];
    cnb[2][1][1] = cnk[3][1][1];
    cnb[2][1][2] = cnk[3][1][2];
    cnb[2][2][0] = cnk[3][2][0];
    cnb[2][2][1] = cnk[3][2][1];
    cnb[2][2][2] = cnk[3][2][2];
/*
Compute q-related auxiliary variables
*/
/*
Compute rnk & rnb (mass center locations in N)
*/
    rnk[2][0] = (cnk[1][0][1]*q[2]);
    rnk[2][1] = (q[2]*c1);
    rnk[2][2] = (cnk[1][2][1]*q[2]);
    rnb[0][0] = 0.;
    rnb[0][1] = 0.;
    rnb[0][2] = 0.;
    rnb[1][0] = rnk[2][0];
    rnb[1][1] = rnk[2][1];
    rnb[1][2] = rnk[2][2];
    rnb[2][0] = rnk[2][0];
    rnb[2][1] = rnk[2][1];
    rnb[2][2] = rnk[2][2];
/*
Compute com (system mass center location in N)
*/
    com[0] = rnk[2][0];
    com[1] = rnk[2][1];
    com[2] = rnk[2][2];
/*
Compute constraint position errors
*/
    skipqs: ;
    sduperr(curtim,q,&perr[1]);
    if (uchg == 0) {
        goto skipus;
    }
/*
Velocity-related variables need to be computed
*/
    inerflg = 0;
    for (i = 0; i < 6; i++) {
        u[i] = uin[i];
    }
/*
Compute u-related auxiliary variables
*/
/*
Compute wk & wb (angular velocities)
*/
    wk[1][1] = (u[0]*c1);
    wk[1][2] = -(u[0]*s1);
    wk[3][0] = (u[3]+((Cik[3][2][0]*wk[1][2])+((Cik[3][0][0]*u[1])+(Cik[3][1][0]
      *wk[1][1]))));
    wk[3][1] = ((Cik[3][2][1]*wk[1][2])+((Cik[3][0][1]*u[1])+(Cik[3][1][1]*
      wk[1][1])));
    wk[3][2] = ((Cik[3][2][2]*wk[1][2])+((Cik[3][0][2]*u[1])+(Cik[3][1][2]*
      wk[1][1])));
    wk[4][1] = (u[4]+wk[3][1]);
    wk[5][2] = (u[5]+wk[3][2]);
    wb[0][0] = u[1];
    wb[0][1] = wk[1][1];
    wb[0][2] = wk[1][2];
    wb[1][0] = u[1];
    wb[1][1] = wk[1][1];
    wb[1][2] = wk[1][2];
    wb[2][0] = wk[3][0];
    wb[2][1] = wk[4][1];
    wb[2][2] = wk[5][2];
/*
Compute auxiliary variables involving wk
*/
    Wkrpk[2][0] = -(q[2]*wk[1][2]);
    Wkrpk[2][2] = (q[2]*u[1]);
/*
Compute temporaries for use in SDRHS
*/
    w0w0[0] = (u[1]*u[1]);
    w0w0[1] = (u[1]*u[1]);
    w0w0[2] = (wk[3][0]*wk[3][0]);
    w1w1[0] = (wk[1][1]*wk[1][1]);
    w1w1[1] = (wk[1][1]*wk[1][1]);
    w1w1[2] = (wk[4][1]*wk[4][1]);
    w2w2[0] = (wk[1][2]*wk[1][2]);
    w2w2[1] = (wk[1][2]*wk[1][2]);
    w2w2[2] = (wk[5][2]*wk[5][2]);
    w0w1[0] = (u[1]*wk[1][1]);
    w0w1[1] = (u[1]*wk[1][1]);
    w0w1[2] = (wk[3][0]*wk[4][1]);
    w0w2[0] = (u[1]*wk[1][2]);
    w0w2[1] = (u[1]*wk[1][2]);
    w0w2[2] = (wk[3][0]*wk[5][2]);
    w1w2[0] = (wk[1][1]*wk[1][2]);
    w1w2[1] = (wk[1][1]*wk[1][2]);
    w1w2[2] = (wk[4][1]*wk[5][2]);
    w00w11[0] = -(w0w0[0]+w1w1[0]);
    w00w11[1] = -(w0w0[1]+w1w1[1]);
    w00w11[2] = -(w0w0[2]+w1w1[2]);
    w00w22[0] = -(w0w0[0]+w2w2[0]);
    w00w22[1] = -(w0w0[1]+w2w2[1]);
    w00w22[2] = -(w0w0[2]+w2w2[2]);
    w11w22[0] = -(w1w1[0]+w2w2[0]);
    w11w22[1] = -(w1w1[1]+w2w2[1]);
    w11w22[2] = -(w1w1[2]+w2w2[2]);
/*
Compute vnk & vnb (mass center linear velocities in N)
*/
    vnk[2][0] = ((cnk[1][0][2]*Wkrpk[2][2])+((cnk[1][0][1]*u[2])+(Wkrpk[2][0]*c0
      )));
    vnk[2][1] = ((u[2]*c1)-(Wkrpk[2][2]*s1));
    vnk[2][2] = ((cnk[1][2][2]*Wkrpk[2][2])+((cnk[1][2][1]*u[2])-(Wkrpk[2][0]*s0
      )));
    vnb[0][0] = 0.;
    vnb[0][1] = 0.;
    vnb[0][2] = 0.;
    vnb[1][0] = vnk[2][0];
    vnb[1][1] = vnk[2][1];
    vnb[1][2] = vnk[2][2];
    vnb[2][0] = vnk[2][0];
    vnb[2][1] = vnk[2][1];
    vnb[2][2] = vnk[2][2];
/*
Compute qdot (kinematical equations)
*/
    qdot[0] = u[0];
    qdot[1] = u[1];
    qdot[2] = u[2];
    qdot[3] = (.5*((q[6]*u[3])+((q[4]*u[5])-(q[5]*u[4]))));
    qdot[4] = (.5*((q[5]*u[3])+((q[6]*u[4])-(q[3]*u[5]))));
    qdot[5] = (.5*(((q[3]*u[4])+(q[6]*u[5]))-(q[4]*u[3])));
    qdot[6] = -(.5*((q[3]*u[3])+((q[4]*u[4])+(q[5]*u[5]))));
    if (stabvel  !=  0.) {
        ee = ((q[6]*q[6])+((q[5]*q[5])+((q[3]*q[3])+(q[4]*q[4]))));
        stab = ((stabvel*(1.-ee))/ee);
        qdot[3] = (qdot[3]+(q[3]*stab));
        qdot[4] = (qdot[4]+(q[4]*stab));
        qdot[5] = (qdot[5]+(q[5]*stab));
        qdot[6] = (qdot[6]+(q[6]*stab));
    }
/*
Compute constraint velocity errors
*/
    skipus: ;
    sduverr(curtim,q,u,&verr[1]);
/*
Initialize applied forces and torques to zero
*/
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            ufk[i][j] = 0.;
            utk[i][j] = 0.;
        }
    }
    for (i = 0; i < 6; i++) {
        utau[i] = 0.;
    }
    ltauflg = 0;
    fs0flg = 0;
/*
Initialize prescribed motions
*/
    uacc[2] = 0.;
    uvel[2] = u[2];
    upos[2] = q[2];
/*
 Used 0.02 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   85 adds/subtracts/negates
                    135 multiplies
                      1 divides
                    181 assignments
*/
}

void sdqdot(double oqdot[7])
{
/*
Return position coordinate derivatives for tree joints.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(63,23);
        return;
    }
    for (i = 0; i <= 6; i++) {
        oqdot[i] = qdot[i];
    }
}

void sdu2qdot(double uin[6],
    double oqdot[7])
{
/*
Convert velocities to qdots.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(64,23);
        return;
    }
    for (i = 0; i <= 5; i++) {
        oqdot[i] = uin[i];
    }
    oqdot[3] = (.5*((q[6]*uin[3])+((q[4]*uin[5])-(q[5]*uin[4]))));
    oqdot[4] = (.5*((q[5]*uin[3])+((q[6]*uin[4])-(q[3]*uin[5]))));
    oqdot[5] = (.5*(((q[3]*uin[4])+(q[6]*uin[5]))-(q[4]*uin[3])));
    oqdot[6] = -(.5*((q[3]*uin[3])+((q[4]*uin[4])+(q[5]*uin[5]))));
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    9 adds/subtracts/negates
                     16 multiplies
                      0 divides
                     10 assignments
*/
}

void sdpsstate(double lqin[1])
{

    if (roustate != 2) {
        sdseterr(9,23);
        return;
    }
}

void sddoping(void)
{

    if (vpkflg == 0) {
/*
Compute ping (jt pins in ground frame)
*/
        ping[0][1] = 1.;
        ping[1][0] = c0;
        ping[1][2] = -s0;
        ping[2][0] = cnk[1][0][1];
        ping[2][1] = c1;
        ping[2][2] = cnk[1][2][1];
        ping[3][0] = cnk[3][0][0];
        ping[3][1] = cnk[3][1][0];
        ping[3][2] = cnk[3][2][0];
        ping[4][0] = cnk[3][0][1];
        ping[4][1] = cnk[3][1][1];
        ping[4][2] = cnk[3][2][1];
        ping[5][0] = cnk[3][0][2];
        ping[5][1] = cnk[3][1][2];
        ping[5][2] = cnk[3][2][2];
/*
Compute hngpt (hinge pts in ground frame)
*/
        hngpt[2][0] = rnk[2][0];
        hngpt[2][1] = rnk[2][1];
        hngpt[2][2] = rnk[2][2];
        hngpt[3][0] = rnk[2][0];
        hngpt[3][1] = rnk[2][1];
        hngpt[3][2] = rnk[2][2];
        hngpt[4][0] = rnk[2][0];
        hngpt[4][1] = rnk[2][1];
        hngpt[4][2] = rnk[2][2];
        hngpt[5][0] = rnk[2][0];
        hngpt[5][1] = rnk[2][1];
        hngpt[5][2] = rnk[2][2];
        vpkflg = 1;
    }
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    1 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     27 assignments
*/
}

void sddoltau(void)
{

/*
Compute effect of loop hinge torques
*/
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      0 assignments
*/
}

void sddoiner(void)
{

/*
Compute inertial accelerations and related temps
*/
    if (inerflg == 0) {
/*
Compute Otk (inertial angular acceleration)
*/
        Otk[1][1] = (u[1]*wk[1][2]);
        Otk[1][2] = -(u[1]*wk[1][1]);
        Otk[3][0] = ((Cik[3][1][0]*Otk[1][1])+(Cik[3][2][0]*Otk[1][2]));
        Otk[3][1] = ((Cik[3][1][1]*Otk[1][1])+(Cik[3][2][1]*Otk[1][2]));
        Otk[3][2] = ((Cik[3][1][2]*Otk[1][1])+(Cik[3][2][2]*Otk[1][2]));
        Otk[5][0] = (Otk[3][0]+((u[5]*wk[4][1])-(u[4]*wk[5][2])));
        Otk[5][1] = (Otk[3][1]+((u[3]*wk[5][2])-(u[5]*wk[3][0])));
        Otk[5][2] = (Otk[3][2]+((u[4]*wk[3][0])-(u[3]*wk[4][1])));
/*
Compute Atk (inertial linear acceleration)
*/
        Atk[2][0] = (((wk[1][1]*Wkrpk[2][2])-(Otk[1][2]*q[2]))-(2.*(u[2]*
          wk[1][2])));
        Atk[2][1] = ((wk[1][2]*Wkrpk[2][0])-(u[1]*Wkrpk[2][2]));
        Atk[2][2] = ((2.*(u[1]*u[2]))-(wk[1][1]*Wkrpk[2][0]));
        Atk[3][0] = ((Atk[2][2]*Cik[3][2][0])+((Atk[2][0]*Cik[3][0][0])+(
          Atk[2][1]*Cik[3][1][0])));
        Atk[3][1] = ((Atk[2][2]*Cik[3][2][1])+((Atk[2][0]*Cik[3][0][1])+(
          Atk[2][1]*Cik[3][1][1])));
        Atk[3][2] = ((Atk[2][2]*Cik[3][2][2])+((Atk[2][0]*Cik[3][0][2])+(
          Atk[2][1]*Cik[3][1][2])));
        inerflg = 1;
    }
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   20 adds/subtracts/negates
                     32 multiplies
                      0 divides
                     14 assignments
*/
}

void sddofs0(void)
{

/*
Compute effect of all applied loads
*/
    if (fs0flg == 0) {
        sddoltau();
        sddoiner();
/*
Compute Fstar (forces)
*/
        Fstar[5][0] = ((Atk[3][0]-gk[3][0])-ufk[2][0]);
        Fstar[5][1] = ((Atk[3][1]-gk[3][1])-ufk[2][1]);
        Fstar[5][2] = ((Atk[3][2]-gk[3][2])-ufk[2][2]);
/*
Compute Tstar (torques)
*/
        Tstar[5][0] = ((.4*Otk[5][0])-utk[2][0]);
        Tstar[5][1] = ((.4*Otk[5][1])-utk[2][1]);
        Tstar[5][2] = ((.4*Otk[5][2])-utk[2][2]);
/*
Compute fs0 (RHS ignoring constraints)
*/
        fs0[5] = (utau[5]-Tstar[5][2]);
        Fstark[4][0] = Fstar[5][0];
        Fstark[4][1] = Fstar[5][1];
        Fstark[4][2] = Fstar[5][2];
        Tstark[4][0] = Tstar[5][0];
        Tstark[4][1] = Tstar[5][1];
        Tstark[4][2] = Tstar[5][2];
        fs0[4] = (utau[4]-Tstark[4][1]);
        Fstark[3][0] = Fstark[4][0];
        Fstark[3][1] = Fstark[4][1];
        Fstark[3][2] = Fstark[4][2];
        Tstark[3][0] = Tstark[4][0];
        Tstark[3][1] = Tstark[4][1];
        Tstark[3][2] = Tstark[4][2];
        fs0[3] = (utau[3]-Tstark[3][0]);
        Fstark[2][0] = (((Cik[3][0][2]*Fstark[3][2])+((Cik[3][0][0]*Fstark[3][0]
          )+(Cik[3][0][1]*Fstark[3][1])))-ufk[1][0]);
        Fstark[2][1] = (((Cik[3][1][2]*Fstark[3][2])+((Cik[3][1][0]*Fstark[3][0]
          )+(Cik[3][1][1]*Fstark[3][1])))-ufk[1][1]);
        Fstark[2][2] = (((Cik[3][2][2]*Fstark[3][2])+((Cik[3][2][0]*Fstark[3][0]
          )+(Cik[3][2][1]*Fstark[3][1])))-ufk[1][2]);
        Tstark[2][0] = (((Cik[3][0][2]*Tstark[3][2])+((Cik[3][0][0]*Tstark[3][0]
          )+(Cik[3][0][1]*Tstark[3][1])))-utk[1][0]);
        Tstark[2][1] = (((Cik[3][1][2]*Tstark[3][2])+((Cik[3][1][0]*Tstark[3][0]
          )+(Cik[3][1][1]*Tstark[3][1])))-utk[1][1]);
        Tstark[2][2] = (((Cik[3][2][2]*Tstark[3][2])+((Cik[3][2][0]*Tstark[3][0]
          )+(Cik[3][2][1]*Tstark[3][1])))-utk[1][2]);
        fs0[2] = (utau[2]-Fstark[2][1]);
        Fstark[1][0] = (Fstark[2][0]-ufk[0][0]);
        Fstark[1][1] = (Fstark[2][1]-ufk[0][1]);
        Fstark[1][2] = (Fstark[2][2]-ufk[0][2]);
        tvec1[0] = (Tstark[2][0]+(Fstark[2][2]*q[2]));
        tvec1[2] = (Tstark[2][2]-(Fstark[2][0]*q[2]));
        Tstark[1][0] = (tvec1[0]-utk[0][0]);
        Tstark[1][1] = (Tstark[2][1]-utk[0][1]);
        Tstark[1][2] = (tvec1[2]-utk[0][2]);
        fs0[1] = (utau[1]-Tstark[1][0]);
        Fstark[0][0] = Fstark[1][0];
        Fstark[0][1] = ((Fstark[1][1]*c1)-(Fstark[1][2]*s1));
        Fstark[0][2] = ((Fstark[1][1]*s1)+(Fstark[1][2]*c1));
        Tstark[0][0] = Tstark[1][0];
        Tstark[0][1] = ((Tstark[1][1]*c1)-(Tstark[1][2]*s1));
        Tstark[0][2] = ((Tstark[1][1]*s1)+(Tstark[1][2]*c1));
        fs0[0] = (utau[0]-Tstark[0][1]);
        fs0flg = 1;
    }
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   45 adds/subtracts/negates
                     31 multiplies
                      0 divides
                     44 assignments
*/
}

void sddomm(int routine)
{
    int dumroutine,errnum;

    if (mmflg == 0) {
/*
Compute gains (DD, G)
*/
        DD[5] = 2.5;
        DD[4] = 2.5;
        DD[3] = 2.5;
        P11[2][0][0] = ((Cik[3][0][2]*Cik[3][0][2])+((Cik[3][0][0]*Cik[3][0][0])
          +(Cik[3][0][1]*Cik[3][0][1])));
        P11[2][0][1] = ((Cik[3][0][2]*Cik[3][1][2])+((Cik[3][0][0]*Cik[3][1][0])
          +(Cik[3][0][1]*Cik[3][1][1])));
        P11[2][0][2] = ((Cik[3][0][2]*Cik[3][2][2])+((Cik[3][0][0]*Cik[3][2][0])
          +(Cik[3][0][1]*Cik[3][2][1])));
        P11[2][1][0] = P11[2][0][1];
        P11[2][1][1] = ((Cik[3][1][2]*Cik[3][1][2])+((Cik[3][1][0]*Cik[3][1][0])
          +(Cik[3][1][1]*Cik[3][1][1])));
        P11[2][1][2] = ((Cik[3][1][2]*Cik[3][2][2])+((Cik[3][1][0]*Cik[3][2][0])
          +(Cik[3][1][1]*Cik[3][2][1])));
        P11[2][2][0] = P11[2][0][2];
        P11[2][2][1] = P11[2][1][2];
        P11[2][2][2] = ((Cik[3][2][2]*Cik[3][2][2])+((Cik[3][2][0]*Cik[3][2][0])
          +(Cik[3][2][1]*Cik[3][2][1])));
        PH1[2][0] = P11[2][0][1];
        PH1[2][1] = P11[2][1][1];
        PH1[2][2] = P11[2][2][1];
        if (PH1[2][1] < 1e-13) {
            sdseterr(routine,47);
        } else {
            DD[2] = (1./PH1[2][1]);
        }
        G1[2][0] = (DD[2]*PH1[2][0]);
        G1[2][2] = (DD[2]*PH1[2][2]);
        P11[2][0][0] = (P11[2][0][0]-(G1[2][0]*PH1[2][0]));
        P11[2][0][1] = (P11[2][0][1]-PH1[2][0]);
        P11[2][0][2] = (P11[2][0][2]-(G1[2][2]*PH1[2][0]));
        P11[2][1][1] = (P11[2][1][1]-PH1[2][1]);
        P11[2][1][2] = (P11[2][1][2]-PH1[2][2]);
        P11[2][2][2] = (P11[2][2][2]-(G1[2][2]*PH1[2][2]));
        sD1INV[0][0] = 0.;
        if (P11[2][0][0] >= 1e-13) {
            sD1INV[0][0] = (1./P11[2][0][0]);
        }
        sL11[1][0] = (P11[2][0][1]*sD1INV[0][0]);
        sL11[2][0] = (P11[2][0][2]*sD1INV[0][0]);
        sL11D1[1][1] = (P11[2][1][1]-(P11[2][0][1]*sL11[1][0]));
        sL11D1[2][1] = (P11[2][1][2]-(P11[2][0][2]*sL11[1][0]));
        sL11D1[2][2] = (P11[2][2][2]-(P11[2][0][2]*sL11[2][0]));
        sD1INV[1][1] = 0.;
        if (sL11D1[1][1] >= 1e-13) {
            sD1INV[1][1] = (1./sL11D1[1][1]);
        }
        sL11[2][1] = (sD1INV[1][1]*sL11D1[2][1]);
        sL11D1[2][2] = (sL11D1[2][2]-(sL11[2][1]*sL11D1[2][1]));
        sD1INV[2][2] = 0.;
        if (sL11D1[2][2] >= 1e-13) {
            sD1INV[2][2] = (1./sL11D1[2][2]);
        }
        N21[2][0][0] = (q[2]*sL11[2][0]);
        N21[2][0][1] = (q[2]*sL11[2][1]);
        psiD11[2][1][0] = (P11[2][0][0]*sL11[1][0]);
        psiD11[2][2][0] = (P11[2][0][0]*sL11[2][0]);
        psiD11[2][2][1] = (sL11[2][1]*sL11D1[1][1]);
        psiD21[2][0][0] = (N21[2][0][0]*P11[2][0][0]);
        psiD21[2][0][1] = (N21[2][0][1]*sL11D1[1][1]);
        psiD21[2][0][2] = (q[2]*sL11D1[2][2]);
        psiD21[2][2][0] = -(P11[2][0][0]*q[2]);
        P11[1][0][0] = P11[2][0][0];
        P11[1][0][1] = (P11[2][0][0]*sL11[1][0]);
        P11[1][0][2] = (P11[2][0][0]*sL11[2][0]);
        P11[1][1][0] = P11[1][0][1];
        P11[1][1][1] = (sL11D1[1][1]+(psiD11[2][1][0]*sL11[1][0]));
        P11[1][1][2] = ((psiD11[2][1][0]*sL11[2][0])+(sL11[2][1]*sL11D1[1][1]));
        P11[1][2][0] = P11[1][0][2];
        P11[1][2][1] = P11[1][1][2];
        P11[1][2][2] = (sL11D1[2][2]+((psiD11[2][2][0]*sL11[2][0])+(
          psiD11[2][2][1]*sL11[2][1])));
        Pd[1][0][0] = (N21[2][0][0]*P11[2][0][0]);
        Pd[1][0][2] = -(P11[2][0][0]*q[2]);
        Pd[1][1][0] = ((N21[2][0][0]*psiD11[2][1][0])+(N21[2][0][1]*sL11D1[1][1]
          ));
        Pd[1][1][2] = -(psiD11[2][1][0]*q[2]);
        Pd[1][2][0] = ((q[2]*sL11D1[2][2])+((N21[2][0][0]*psiD11[2][2][0])+(
          N21[2][0][1]*psiD11[2][2][1])));
        Pd[1][2][2] = -(psiD11[2][2][0]*q[2]);
        P22[1][0][0] = ((psiD21[2][0][2]*q[2])+((N21[2][0][0]*psiD21[2][0][0])+(
          N21[2][0][1]*psiD21[2][0][1])));
        P22[1][0][2] = -(psiD21[2][0][0]*q[2]);
        P22[1][2][0] = P22[1][0][2];
        P22[1][2][2] = -(psiD21[2][2][0]*q[2]);
        PH1[1][0] = Pd[1][0][0];
        PH1[1][1] = Pd[1][1][0];
        PH1[1][2] = Pd[1][2][0];
        PH2[1][0] = P22[1][0][0];
        PH2[1][2] = P22[1][2][0];
        if (PH2[1][0] < 1e-13) {
            sdseterr(routine,47);
        } else {
            DD[1] = (1./PH2[1][0]);
        }
        G1[1][0] = (DD[1]*PH1[1][0]);
        G1[1][1] = (DD[1]*PH1[1][1]);
        G1[1][2] = (DD[1]*PH1[1][2]);
        G2[1][2] = (DD[1]*PH2[1][2]);
        P11[1][0][0] = (P11[1][0][0]-(G1[1][0]*PH1[1][0]));
        P11[1][0][1] = (P11[1][0][1]-(G1[1][1]*PH1[1][0]));
        P11[1][0][2] = (P11[1][0][2]-(G1[1][2]*PH1[1][0]));
        P11[1][1][1] = (P11[1][1][1]-(G1[1][1]*PH1[1][1]));
        P11[1][1][2] = (P11[1][1][2]-(G1[1][2]*PH1[1][1]));
        P11[1][2][2] = (P11[1][2][2]-(G1[1][2]*PH1[1][2]));
        Pd[1][0][0] = (Pd[1][0][0]-PH1[1][0]);
        Pd[1][0][2] = (Pd[1][0][2]-(G2[1][2]*PH1[1][0]));
        Pd[1][1][0] = (Pd[1][1][0]-PH1[1][1]);
        Pd[1][1][2] = (Pd[1][1][2]-(G2[1][2]*PH1[1][1]));
        Pd[1][2][0] = (Pd[1][2][0]-PH1[1][2]);
        Pd[1][2][2] = (Pd[1][2][2]-(G2[1][2]*PH1[1][2]));
        P22[1][0][0] = (P22[1][0][0]-PH2[1][0]);
        P22[1][0][2] = (P22[1][0][2]-PH2[1][2]);
        P22[1][2][2] = (P22[1][2][2]-(G2[1][2]*PH2[1][2]));
        sD1INV[0][0] = 0.;
        if (P11[1][0][0] >= 1e-13) {
            sD1INV[0][0] = (1./P11[1][0][0]);
        }
        sL11[1][0] = (P11[1][0][1]*sD1INV[0][0]);
        sL11[2][0] = (P11[1][0][2]*sD1INV[0][0]);
        sL11D1[1][1] = (P11[1][1][1]-(P11[1][0][1]*sL11[1][0]));
        sL11D1[2][1] = (P11[1][1][2]-(P11[1][0][2]*sL11[1][0]));
        sL11D1[2][2] = (P11[1][2][2]-(P11[1][0][2]*sL11[2][0]));
        sD1INV[1][1] = 0.;
        if (sL11D1[1][1] >= 1e-13) {
            sD1INV[1][1] = (1./sL11D1[1][1]);
        }
        sL11[2][1] = (sD1INV[1][1]*sL11D1[2][1]);
        sL11D1[2][2] = (sL11D1[2][2]-(sL11[2][1]*sL11D1[2][1]));
        sD1INV[2][2] = 0.;
        if (sL11D1[2][2] >= 1e-13) {
            sD1INV[2][2] = (1./sL11D1[2][2]);
        }
        sD1L21[1][0] = (Pd[1][1][0]-(Pd[1][0][0]*sL11[1][0]));
        sD1L21[1][2] = (Pd[1][1][2]-(Pd[1][0][2]*sL11[1][0]));
        sD1L21[2][0] = (Pd[1][2][0]-((Pd[1][0][0]*sL11[2][0])+(sD1L21[1][0]*
          sL11[2][1])));
        sD1L21[2][2] = (Pd[1][2][2]-((Pd[1][0][2]*sL11[2][0])+(sD1L21[1][2]*
          sL11[2][1])));
        sL21[0][0] = (Pd[1][0][0]*sD1INV[0][0]);
        sL21[0][1] = (sD1INV[1][1]*sD1L21[1][0]);
        sL21[0][2] = (sD1INV[2][2]*sD1L21[2][0]);
        sL21[2][0] = (Pd[1][0][2]*sD1INV[0][0]);
        sL21[2][1] = (sD1INV[1][1]*sD1L21[1][2]);
        sL21[2][2] = (sD1INV[2][2]*sD1L21[2][2]);
        sL22D2[0][0] = (P22[1][0][0]-((sD1L21[2][0]*sL21[0][2])+((Pd[1][0][0]*
          sL21[0][0])+(sD1L21[1][0]*sL21[0][1]))));
        sL22D2[2][0] = (P22[1][0][2]-((sD1L21[2][0]*sL21[2][2])+((Pd[1][0][0]*
          sL21[2][0])+(sD1L21[1][0]*sL21[2][1]))));
        sL22D2[2][2] = (P22[1][2][2]-((sD1L21[2][2]*sL21[2][2])+((Pd[1][0][2]*
          sL21[2][0])+(sD1L21[1][2]*sL21[2][1]))));
        sD2INV[0][0] = 0.;
        if (sL22D2[0][0] >= 1e-13) {
            sD2INV[0][0] = (1./sL22D2[0][0]);
        }
        sL22[2][0] = (sD2INV[0][0]*sL22D2[2][0]);
        sL22D2[2][2] = (sL22D2[2][2]-(sL22[2][0]*sL22D2[2][0]));
        N11[1][1][0] = ((sL11[1][0]*c1)-(sL11[2][0]*s1));
        N11[1][1][1] = (c1-(sL11[2][1]*s1));
        N11[1][2][0] = ((sL11[1][0]*s1)+(sL11[2][0]*c1));
        N11[1][2][1] = (s1+(sL11[2][1]*c1));
        N21[1][1][0] = -(sL21[2][0]*s1);
        N21[1][1][1] = -(sL21[2][1]*s1);
        N21[1][1][2] = -(sL21[2][2]*s1);
        N21[1][2][0] = (sL21[2][0]*c1);
        N21[1][2][1] = (sL21[2][1]*c1);
        N21[1][2][2] = (sL21[2][2]*c1);
        N22[1][1][0] = -(sL22[2][0]*s1);
        N22[1][2][0] = (sL22[2][0]*c1);
        psiD11[1][1][0] = (N11[1][1][0]*P11[1][0][0]);
        psiD11[1][1][1] = (N11[1][1][1]*sL11D1[1][1]);
        psiD11[1][1][2] = -(sL11D1[2][2]*s1);
        psiD11[1][2][0] = (N11[1][2][0]*P11[1][0][0]);
        psiD11[1][2][1] = (N11[1][2][1]*sL11D1[1][1]);
        psiD11[1][2][2] = (sL11D1[2][2]*c1);
        psiD21[1][0][0] = (P11[1][0][0]*sL21[0][0]);
        psiD21[1][0][1] = (sL11D1[1][1]*sL21[0][1]);
        psiD21[1][0][2] = (sL11D1[2][2]*sL21[0][2]);
        psiD21[1][1][0] = (N21[1][1][0]*P11[1][0][0]);
        psiD21[1][1][1] = (N21[1][1][1]*sL11D1[1][1]);
        psiD21[1][1][2] = (N21[1][1][2]*sL11D1[2][2]);
        psiD21[1][2][0] = (N21[1][2][0]*P11[1][0][0]);
        psiD21[1][2][1] = (N21[1][2][1]*sL11D1[1][1]);
        psiD21[1][2][2] = (N21[1][2][2]*sL11D1[2][2]);
        psiD22[1][1][0] = (N22[1][1][0]*sL22D2[0][0]);
        psiD22[1][1][2] = -(sL22D2[2][2]*s1);
        psiD22[1][2][0] = (N22[1][2][0]*sL22D2[0][0]);
        psiD22[1][2][2] = (sL22D2[2][2]*c1);
        P11[0][0][0] = P11[1][0][0];
        P11[0][0][1] = (N11[1][1][0]*P11[1][0][0]);
        P11[0][0][2] = (N11[1][2][0]*P11[1][0][0]);
        P11[0][1][0] = P11[0][0][1];
        P11[0][1][1] = (((N11[1][1][0]*psiD11[1][1][0])+(N11[1][1][1]*
          psiD11[1][1][1]))-(psiD11[1][1][2]*s1));
        P11[0][1][2] = ((psiD11[1][1][2]*c1)+((N11[1][2][0]*psiD11[1][1][0])+(
          N11[1][2][1]*psiD11[1][1][1])));
        P11[0][2][0] = P11[0][0][2];
        P11[0][2][1] = P11[0][1][2];
        P11[0][2][2] = ((psiD11[1][2][2]*c1)+((N11[1][2][0]*psiD11[1][2][0])+(
          N11[1][2][1]*psiD11[1][2][1])));
        Pd[0][0][0] = (P11[1][0][0]*sL21[0][0]);
        Pd[0][0][1] = (N21[1][1][0]*P11[1][0][0]);
        Pd[0][0][2] = (N21[1][2][0]*P11[1][0][0]);
        Pd[0][1][0] = ((psiD11[1][1][2]*sL21[0][2])+((psiD11[1][1][0]*sL21[0][0]
          )+(psiD11[1][1][1]*sL21[0][1])));
        Pd[0][1][1] = ((N21[1][1][2]*psiD11[1][1][2])+((N21[1][1][0]*
          psiD11[1][1][0])+(N21[1][1][1]*psiD11[1][1][1])));
        Pd[0][1][2] = ((N21[1][2][2]*psiD11[1][1][2])+((N21[1][2][0]*
          psiD11[1][1][0])+(N21[1][2][1]*psiD11[1][1][1])));
        Pd[0][2][0] = ((psiD11[1][2][2]*sL21[0][2])+((psiD11[1][2][0]*sL21[0][0]
          )+(psiD11[1][2][1]*sL21[0][1])));
        Pd[0][2][1] = ((N21[1][1][2]*psiD11[1][2][2])+((N21[1][1][0]*
          psiD11[1][2][0])+(N21[1][1][1]*psiD11[1][2][1])));
        Pd[0][2][2] = ((N21[1][2][2]*psiD11[1][2][2])+((N21[1][2][0]*
          psiD11[1][2][0])+(N21[1][2][1]*psiD11[1][2][1])));
        P22[0][0][0] = (sL22D2[0][0]+((psiD21[1][0][2]*sL21[0][2])+((
          psiD21[1][0][0]*sL21[0][0])+(psiD21[1][0][1]*sL21[0][1]))));
        P22[0][0][1] = ((N22[1][1][0]*sL22D2[0][0])+((N21[1][1][2]*
          psiD21[1][0][2])+((N21[1][1][0]*psiD21[1][0][0])+(N21[1][1][1]*
          psiD21[1][0][1]))));
        P22[0][0][2] = ((N22[1][2][0]*sL22D2[0][0])+((N21[1][2][2]*
          psiD21[1][0][2])+((N21[1][2][0]*psiD21[1][0][0])+(N21[1][2][1]*
          psiD21[1][0][1]))));
        P22[0][1][0] = P22[0][0][1];
        P22[0][1][1] = (((N21[1][1][2]*psiD21[1][1][2])+((N21[1][1][0]*
          psiD21[1][1][0])+(N21[1][1][1]*psiD21[1][1][1])))+((N22[1][1][0]*
          psiD22[1][1][0])-(psiD22[1][1][2]*s1)));
        P22[0][1][2] = (((N21[1][2][2]*psiD21[1][1][2])+((N21[1][2][0]*
          psiD21[1][1][0])+(N21[1][2][1]*psiD21[1][1][1])))+((N22[1][2][0]*
          psiD22[1][1][0])+(psiD22[1][1][2]*c1)));
        P22[0][2][0] = P22[0][0][2];
        P22[0][2][1] = P22[0][1][2];
        P22[0][2][2] = (((N21[1][2][2]*psiD21[1][2][2])+((N21[1][2][0]*
          psiD21[1][2][0])+(N21[1][2][1]*psiD21[1][2][1])))+((N22[1][2][0]*
          psiD22[1][2][0])+(psiD22[1][2][2]*c1)));
        PH1[0][0] = Pd[0][0][1];
        PH1[0][1] = Pd[0][1][1];
        PH1[0][2] = Pd[0][2][1];
        PH2[0][0] = P22[0][0][1];
        PH2[0][1] = P22[0][1][1];
        PH2[0][2] = P22[0][2][1];
        if (PH2[0][1] < 1e-13) {
            sdseterr(routine,47);
        } else {
            DD[0] = (1./PH2[0][1]);
        }
        sderror(&dumroutine,&errnum);
        if (errnum == 0) {
            mmflg = 1;
        }
    }
/*
 Used 0.03 seconds CPU time,
 0 additional bytes of memory.
 Equations contain  121 adds/subtracts/negates
                    195 multiplies
                     10 divides
                    185 assignments
*/
}

void sdlhs(int routine)
{
/* Compute all remaining state- and force-dependent quantities
*/

    roustate = 2;
    sddomm(routine);
    sddofs0();
}

void sdmfrc(double imult[3])
{
/*
Calculate forces due to constraint multipliers.

*/
    int i,j;
    double umult[2];

/*
Initialize all multiplier forces to zero.
*/
    for (i = 0; i <= 2; i++) {
        for (j = 0; j <= 2; j++) {
            mfk[i][j] = 0.;
            mtk[i][j] = 0.;
        }
    }
    for (i = 0; i <= 5; i++) {
        mtau[i] = 0.;
    }
/*
Compute user-generated multiplier forces
*/
    umult[0] = imult[1];
    umult[1] = imult[2];
    mfrcflg = 1;
    sduconsfrc(curtim,q,u,umult);
    mfrcflg = 0;
    if (pres[2]  !=  0.) {
        mtau[2] = (imult[0]+mtau[2]);
    }
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    1 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     27 assignments
*/
}

void sdequivht(double tau[6])
{
/* Compute tree hinge torques to match effect of applied loads
*/
    double fstareq[6][3],tstareq[6][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(56,23);
        return;
    }
/*
Compute fstareq (forces)
*/
    fstareq[5][0] = -(gk[3][0]+ufk[2][0]);
    fstareq[5][1] = -(gk[3][1]+ufk[2][1]);
    fstareq[5][2] = -(gk[3][2]+ufk[2][2]);
/*
Compute tstareq (torques)
*/
/*
Compute taus (RHS ignoring constraints and inertial forces)
*/
    tau[5] = (utau[5]+utk[2][2]);
    Fstark[4][0] = fstareq[5][0];
    Fstark[4][1] = fstareq[5][1];
    Fstark[4][2] = fstareq[5][2];
    Tstark[4][0] = -utk[2][0];
    Tstark[4][1] = -utk[2][1];
    Tstark[4][2] = -utk[2][2];
    tau[4] = (utau[4]-Tstark[4][1]);
    Fstark[3][0] = Fstark[4][0];
    Fstark[3][1] = Fstark[4][1];
    Fstark[3][2] = Fstark[4][2];
    Tstark[3][0] = Tstark[4][0];
    Tstark[3][1] = Tstark[4][1];
    Tstark[3][2] = Tstark[4][2];
    tau[3] = (utau[3]-Tstark[3][0]);
    Fstark[2][0] = (((Cik[3][0][2]*Fstark[3][2])+((Cik[3][0][0]*Fstark[3][0])+(
      Cik[3][0][1]*Fstark[3][1])))-ufk[1][0]);
    Fstark[2][1] = (((Cik[3][1][2]*Fstark[3][2])+((Cik[3][1][0]*Fstark[3][0])+(
      Cik[3][1][1]*Fstark[3][1])))-ufk[1][1]);
    Fstark[2][2] = (((Cik[3][2][2]*Fstark[3][2])+((Cik[3][2][0]*Fstark[3][0])+(
      Cik[3][2][1]*Fstark[3][1])))-ufk[1][2]);
    Tstark[2][0] = (((Cik[3][0][2]*Tstark[3][2])+((Cik[3][0][0]*Tstark[3][0])+(
      Cik[3][0][1]*Tstark[3][1])))-utk[1][0]);
    Tstark[2][1] = (((Cik[3][1][2]*Tstark[3][2])+((Cik[3][1][0]*Tstark[3][0])+(
      Cik[3][1][1]*Tstark[3][1])))-utk[1][1]);
    Tstark[2][2] = (((Cik[3][2][2]*Tstark[3][2])+((Cik[3][2][0]*Tstark[3][0])+(
      Cik[3][2][1]*Tstark[3][1])))-utk[1][2]);
    tau[2] = (utau[2]-Fstark[2][1]);
    Fstark[1][0] = (Fstark[2][0]-ufk[0][0]);
    Fstark[1][1] = (Fstark[2][1]-ufk[0][1]);
    Fstark[1][2] = (Fstark[2][2]-ufk[0][2]);
    tvec1[0] = (Tstark[2][0]+(Fstark[2][2]*q[2]));
    tvec1[2] = (Tstark[2][2]-(Fstark[2][0]*q[2]));
    Tstark[1][0] = (tvec1[0]-utk[0][0]);
    Tstark[1][1] = (Tstark[2][1]-utk[0][1]);
    Tstark[1][2] = (tvec1[2]-utk[0][2]);
    tau[1] = (utau[1]-Tstark[1][0]);
    Fstark[0][0] = Fstark[1][0];
    Fstark[0][1] = ((Fstark[1][1]*c1)-(Fstark[1][2]*s1));
    Fstark[0][2] = ((Fstark[1][1]*s1)+(Fstark[1][2]*c1));
    Tstark[0][0] = Tstark[1][0];
    Tstark[0][1] = ((Tstark[1][1]*c1)-(Tstark[1][2]*s1));
    Tstark[0][2] = ((Tstark[1][1]*s1)+(Tstark[1][2]*c1));
    tau[0] = (utau[0]-Tstark[0][1]);
/*
Op counts below do not include called subroutines
*/
/*
 Used 0.01 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   45 adds/subtracts/negates
                     28 multiplies
                      0 divides
                     41 assignments
*/
}

void sdfs0(void)
{

/*
Compute Fs (ignoring multiplier forces)
*/
    fs[0] = fs0[0];
    fs[1] = fs0[1];
    fs[2] = fs0[2];
    fs[3] = fs0[3];
    fs[4] = fs0[4];
    fs[5] = fs0[5];
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      6 assignments
*/
}

void sdfsmult(void)
{

/*
Compute Fs (multiplier-generated forces only)
*/
    fs[5] = (mtau[5]+mtk[2][2]);
    Fstark[4][0] = -mfk[2][0];
    Fstark[4][1] = -mfk[2][1];
    Fstark[4][2] = -mfk[2][2];
    Tstark[4][0] = -mtk[2][0];
    Tstark[4][1] = -mtk[2][1];
    Tstark[4][2] = -mtk[2][2];
    fs[4] = (mtau[4]-Tstark[4][1]);
    Fstark[3][0] = Fstark[4][0];
    Fstark[3][1] = Fstark[4][1];
    Fstark[3][2] = Fstark[4][2];
    Tstark[3][0] = Tstark[4][0];
    Tstark[3][1] = Tstark[4][1];
    Tstark[3][2] = Tstark[4][2];
    fs[3] = (mtau[3]-Tstark[3][0]);
    Fstark[2][0] = (((Cik[3][0][2]*Fstark[3][2])+((Cik[3][0][0]*Fstark[3][0])+(
      Cik[3][0][1]*Fstark[3][1])))-mfk[1][0]);
    Fstark[2][1] = (((Cik[3][1][2]*Fstark[3][2])+((Cik[3][1][0]*Fstark[3][0])+(
      Cik[3][1][1]*Fstark[3][1])))-mfk[1][1]);
    Fstark[2][2] = (((Cik[3][2][2]*Fstark[3][2])+((Cik[3][2][0]*Fstark[3][0])+(
      Cik[3][2][1]*Fstark[3][1])))-mfk[1][2]);
    Tstark[2][0] = (((Cik[3][0][2]*Tstark[3][2])+((Cik[3][0][0]*Tstark[3][0])+(
      Cik[3][0][1]*Tstark[3][1])))-mtk[1][0]);
    Tstark[2][1] = (((Cik[3][1][2]*Tstark[3][2])+((Cik[3][1][0]*Tstark[3][0])+(
      Cik[3][1][1]*Tstark[3][1])))-mtk[1][1]);
    Tstark[2][2] = (((Cik[3][2][2]*Tstark[3][2])+((Cik[3][2][0]*Tstark[3][0])+(
      Cik[3][2][1]*Tstark[3][1])))-mtk[1][2]);
    fs[2] = (mtau[2]-Fstark[2][1]);
    Fstark[1][0] = (Fstark[2][0]-mfk[0][0]);
    Fstark[1][1] = (Fstark[2][1]-mfk[0][1]);
    Fstark[1][2] = (Fstark[2][2]-mfk[0][2]);
    tvec1[0] = (Tstark[2][0]+(Fstark[2][2]*q[2]));
    tvec1[2] = (Tstark[2][2]-(Fstark[2][0]*q[2]));
    Tstark[1][0] = (tvec1[0]-mtk[0][0]);
    Tstark[1][1] = (Tstark[2][1]-mtk[0][1]);
    Tstark[1][2] = (tvec1[2]-mtk[0][2]);
    fs[1] = (mtau[1]-Tstark[1][0]);
    Fstark[0][0] = Fstark[1][0];
    Fstark[0][1] = ((Fstark[1][1]*c1)-(Fstark[1][2]*s1));
    Fstark[0][2] = ((Fstark[1][1]*s1)+(Fstark[1][2]*c1));
    Tstark[0][0] = Tstark[1][0];
    Tstark[0][1] = ((Tstark[1][1]*c1)-(Tstark[1][2]*s1));
    Tstark[0][2] = ((Tstark[1][1]*s1)+(Tstark[1][2]*c1));
    fs[0] = (mtau[0]-Tstark[0][1]);
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   42 adds/subtracts/negates
                     28 multiplies
                      0 divides
                     38 assignments
*/
}

void sdfsfull(void)
{

/*
Compute Fs (including all forces)
*/
    sdfsmult();
    fs[0] = (fs[0]+fs0[0]);
    fs[1] = (fs[1]+fs0[1]);
    fs[2] = (fs[2]+fs0[2]);
    fs[3] = (fs[3]+fs0[3]);
    fs[4] = (fs[4]+fs0[4]);
    fs[5] = (fs[5]+fs0[5]);
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    6 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      6 assignments
*/
}

void sdfsgenmult(void)
{

/*
Compute Fs (generic multiplier-generated forces)
*/
    fs[5] = (mtau[5]+mtk[2][2]);
    Fstark[4][0] = -mfk[2][0];
    Fstark[4][1] = -mfk[2][1];
    Fstark[4][2] = -mfk[2][2];
    Tstark[4][0] = -mtk[2][0];
    Tstark[4][1] = -mtk[2][1];
    Tstark[4][2] = -mtk[2][2];
    fs[4] = (mtau[4]-Tstark[4][1]);
    Fstark[3][0] = Fstark[4][0];
    Fstark[3][1] = Fstark[4][1];
    Fstark[3][2] = Fstark[4][2];
    Tstark[3][0] = Tstark[4][0];
    Tstark[3][1] = Tstark[4][1];
    Tstark[3][2] = Tstark[4][2];
    fs[3] = (mtau[3]-Tstark[3][0]);
    Fstark[2][0] = (((Cik[3][0][2]*Fstark[3][2])+((Cik[3][0][0]*Fstark[3][0])+(
      Cik[3][0][1]*Fstark[3][1])))-mfk[1][0]);
    Fstark[2][1] = (((Cik[3][1][2]*Fstark[3][2])+((Cik[3][1][0]*Fstark[3][0])+(
      Cik[3][1][1]*Fstark[3][1])))-mfk[1][1]);
    Fstark[2][2] = (((Cik[3][2][2]*Fstark[3][2])+((Cik[3][2][0]*Fstark[3][0])+(
      Cik[3][2][1]*Fstark[3][1])))-mfk[1][2]);
    Tstark[2][0] = (((Cik[3][0][2]*Tstark[3][2])+((Cik[3][0][0]*Tstark[3][0])+(
      Cik[3][0][1]*Tstark[3][1])))-mtk[1][0]);
    Tstark[2][1] = (((Cik[3][1][2]*Tstark[3][2])+((Cik[3][1][0]*Tstark[3][0])+(
      Cik[3][1][1]*Tstark[3][1])))-mtk[1][1]);
    Tstark[2][2] = (((Cik[3][2][2]*Tstark[3][2])+((Cik[3][2][0]*Tstark[3][0])+(
      Cik[3][2][1]*Tstark[3][1])))-mtk[1][2]);
    fs[2] = (mtau[2]-Fstark[2][1]);
    Fstark[1][0] = (Fstark[2][0]-mfk[0][0]);
    Fstark[1][1] = (Fstark[2][1]-mfk[0][1]);
    Fstark[1][2] = (Fstark[2][2]-mfk[0][2]);
    tvec1[0] = (Tstark[2][0]+(Fstark[2][2]*q[2]));
    tvec1[2] = (Tstark[2][2]-(Fstark[2][0]*q[2]));
    Tstark[1][0] = (tvec1[0]-mtk[0][0]);
    Tstark[1][1] = (Tstark[2][1]-mtk[0][1]);
    Tstark[1][2] = (tvec1[2]-mtk[0][2]);
    fs[1] = (mtau[1]-Tstark[1][0]);
    Fstark[0][0] = Fstark[1][0];
    Fstark[0][1] = ((Fstark[1][1]*c1)-(Fstark[1][2]*s1));
    Fstark[0][2] = ((Fstark[1][1]*s1)+(Fstark[1][2]*c1));
    Tstark[0][0] = Tstark[1][0];
    Tstark[0][1] = ((Tstark[1][1]*c1)-(Tstark[1][2]*s1));
    Tstark[0][2] = ((Tstark[1][1]*s1)+(Tstark[1][2]*c1));
    fs[0] = (mtau[0]-Tstark[0][1]);
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   42 adds/subtracts/negates
                     28 multiplies
                      0 divides
                     38 assignments
*/
}

void sdfsgenfull(void)
{

/*
Compute Fs (incl generic mult & other forces)
*/
    sdfsgenmult();
    fs[0] = (fs[0]+fs0[0]);
    fs[1] = (fs[1]+fs0[1]);
    fs[2] = (fs[2]+fs0[2]);
    fs[3] = (fs[3]+fs0[3]);
    fs[4] = (fs[4]+fs0[4]);
    fs[5] = (fs[5]+fs0[5]);
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain    6 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      6 assignments
*/
}

void sdfulltrq(double udotin[6],
    double multin[3],
    double trqout[6])
{
/* Compute hinge torques which would produce indicated udots
*/
    double fstarr[6][3],tstarr[6][3],Otkr[6][3],Atir[6][3],Atkr[6][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(61,23);
        return;
    }
/*
Compute multiplier-generated forces
*/
    sdmfrc(multin);
/*
Account for inertial accelerations and supplied udots
*/
    Otkr[1][1] = ((u[1]*wk[1][2])+(udotin[0]*c1));
    Otkr[1][2] = -((u[1]*wk[1][1])+(udotin[0]*s1));
    Otkr[3][0] = (udotin[3]+((Cik[3][2][0]*Otkr[1][2])+((Cik[3][0][0]*udotin[1])
      +(Cik[3][1][0]*Otkr[1][1]))));
    Otkr[3][1] = ((Cik[3][2][1]*Otkr[1][2])+((Cik[3][0][1]*udotin[1])+(
      Cik[3][1][1]*Otkr[1][1])));
    Otkr[3][2] = ((Cik[3][2][2]*Otkr[1][2])+((Cik[3][0][2]*udotin[1])+(
      Cik[3][1][2]*Otkr[1][1])));
    Otkr[4][1] = (Otkr[3][1]+udotin[4]);
    Otkr[5][0] = (Otkr[3][0]+((u[5]*wk[4][1])-(u[4]*wk[5][2])));
    Otkr[5][1] = (Otkr[4][1]+((u[3]*wk[5][2])-(u[5]*wk[3][0])));
    Otkr[5][2] = (Otkr[3][2]+(udotin[5]+((u[4]*wk[3][0])-(u[3]*wk[4][1]))));
    Atkr[2][0] = (((wk[1][1]*Wkrpk[2][2])-(Otkr[1][2]*q[2]))-(2.*(u[2]*wk[1][2])
      ));
    Atkr[2][1] = (udotin[2]+((wk[1][2]*Wkrpk[2][0])-(u[1]*Wkrpk[2][2])));
    Atkr[2][2] = ((2.*(u[1]*u[2]))+((q[2]*udotin[1])-(wk[1][1]*Wkrpk[2][0])));
    Atkr[3][0] = ((Atkr[2][2]*Cik[3][2][0])+((Atkr[2][0]*Cik[3][0][0])+(
      Atkr[2][1]*Cik[3][1][0])));
    Atkr[3][1] = ((Atkr[2][2]*Cik[3][2][1])+((Atkr[2][0]*Cik[3][0][1])+(
      Atkr[2][1]*Cik[3][1][1])));
    Atkr[3][2] = ((Atkr[2][2]*Cik[3][2][2])+((Atkr[2][0]*Cik[3][0][2])+(
      Atkr[2][1]*Cik[3][1][2])));
/*
Accumulate all forces and torques
*/
    fstarr[1][0] = (mfk[0][0]+ufk[0][0]);
    fstarr[1][1] = (mfk[0][1]+ufk[0][1]);
    fstarr[1][2] = (mfk[0][2]+ufk[0][2]);
    fstarr[2][0] = (mfk[1][0]+ufk[1][0]);
    fstarr[2][1] = (mfk[1][1]+ufk[1][1]);
    fstarr[2][2] = (mfk[1][2]+ufk[1][2]);
    fstarr[5][0] = ((gk[3][0]-Atkr[3][0])+(mfk[2][0]+ufk[2][0]));
    fstarr[5][1] = ((gk[3][1]-Atkr[3][1])+(mfk[2][1]+ufk[2][1]));
    fstarr[5][2] = ((gk[3][2]-Atkr[3][2])+(mfk[2][2]+ufk[2][2]));
    tstarr[1][0] = (mtk[0][0]+utk[0][0]);
    tstarr[1][1] = (mtk[0][1]+utk[0][1]);
    tstarr[1][2] = (mtk[0][2]+utk[0][2]);
    tstarr[2][0] = (mtk[1][0]+utk[1][0]);
    tstarr[2][1] = (mtk[1][1]+utk[1][1]);
    tstarr[2][2] = (mtk[1][2]+utk[1][2]);
    tstarr[5][0] = ((mtk[2][0]+utk[2][0])-(.4*Otkr[5][0]));
    tstarr[5][1] = ((mtk[2][1]+utk[2][1])-(.4*Otkr[5][1]));
    tstarr[5][2] = ((mtk[2][2]+utk[2][2])-(.4*Otkr[5][2]));
/*
Now calculate the torques
*/
    trqout[5] = -(tstarr[5][2]+(mtau[5]+utau[5]));
    Fstark[4][0] = fstarr[5][0];
    Fstark[4][1] = fstarr[5][1];
    Fstark[4][2] = fstarr[5][2];
    Tstark[4][0] = tstarr[5][0];
    Tstark[4][1] = tstarr[5][1];
    Tstark[4][2] = tstarr[5][2];
    trqout[4] = -(Tstark[4][1]+(mtau[4]+utau[4]));
    Fstark[3][0] = Fstark[4][0];
    Fstark[3][1] = Fstark[4][1];
    Fstark[3][2] = Fstark[4][2];
    Tstark[3][0] = Tstark[4][0];
    Tstark[3][1] = Tstark[4][1];
    Tstark[3][2] = Tstark[4][2];
    trqout[3] = -(Tstark[3][0]+(mtau[3]+utau[3]));
    Fstark[2][0] = (fstarr[2][0]+((Cik[3][0][2]*Fstark[3][2])+((Cik[3][0][0]*
      Fstark[3][0])+(Cik[3][0][1]*Fstark[3][1]))));
    Fstark[2][1] = (fstarr[2][1]+((Cik[3][1][2]*Fstark[3][2])+((Cik[3][1][0]*
      Fstark[3][0])+(Cik[3][1][1]*Fstark[3][1]))));
    Fstark[2][2] = (fstarr[2][2]+((Cik[3][2][2]*Fstark[3][2])+((Cik[3][2][0]*
      Fstark[3][0])+(Cik[3][2][1]*Fstark[3][1]))));
    Tstark[2][0] = (tstarr[2][0]+((Cik[3][0][2]*Tstark[3][2])+((Cik[3][0][0]*
      Tstark[3][0])+(Cik[3][0][1]*Tstark[3][1]))));
    Tstark[2][1] = (tstarr[2][1]+((Cik[3][1][2]*Tstark[3][2])+((Cik[3][1][0]*
      Tstark[3][0])+(Cik[3][1][1]*Tstark[3][1]))));
    Tstark[2][2] = (tstarr[2][2]+((Cik[3][2][2]*Tstark[3][2])+((Cik[3][2][0]*
      Tstark[3][0])+(Cik[3][2][1]*Tstark[3][1]))));
    trqout[2] = -(Fstark[2][1]+(mtau[2]+utau[2]));
    Fstark[1][0] = (Fstark[2][0]+fstarr[1][0]);
    Fstark[1][1] = (Fstark[2][1]+fstarr[1][1]);
    Fstark[1][2] = (Fstark[2][2]+fstarr[1][2]);
    tvec1[0] = (Tstark[2][0]+(Fstark[2][2]*q[2]));
    tvec1[2] = (Tstark[2][2]-(Fstark[2][0]*q[2]));
    Tstark[1][0] = (tstarr[1][0]+tvec1[0]);
    Tstark[1][1] = (Tstark[2][1]+tstarr[1][1]);
    Tstark[1][2] = (tstarr[1][2]+tvec1[2]);
    trqout[1] = -(Tstark[1][0]+(mtau[1]+utau[1]));
    Fstark[0][0] = Fstark[1][0];
    Fstark[0][1] = ((Fstark[1][1]*c1)-(Fstark[1][2]*s1));
    Fstark[0][2] = ((Fstark[1][1]*s1)+(Fstark[1][2]*c1));
    Tstark[0][0] = Tstark[1][0];
    Tstark[0][1] = ((Tstark[1][1]*c1)-(Tstark[1][2]*s1));
    Tstark[0][2] = ((Tstark[1][1]*s1)+(Tstark[1][2]*c1));
    trqout[0] = -(Tstark[0][1]+(mtau[0]+utau[0]));
/*
Op counts below do not include called subroutines
*/
/*
 Used 0.02 seconds CPU time,
 0 additional bytes of memory.
 Equations contain  105 adds/subtracts/negates
                     69 multiplies
                      0 divides
                     71 assignments
*/
}

void sdcomptrq(double udotin[6],
    double trqout[6])
{
/* Compute hinge torques to produce these udots, ignoring constraints
*/
    int i;
    double multin[3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(60,23);
        return;
    }
    for (i = 0; i < 3; i++) {
        multin[i] = 0.;
    }
    sdfulltrq(udotin,multin,trqout);
}

void sdmulttrq(double multin[3],
    double trqout[6])
{
/* Compute hinge trqs which would be produced by these mults.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(65,23);
        return;
    }
    sdmfrc(multin);
    sdfsmult();
    for (i = 0; i < 6; i++) {
        trqout[i] = fs[i];
    }
}

void sdrhs(void)
{
/*
Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123) on machine ID unknown
Copyright (c) 1990-1997 Symbolic Dynamics, Inc.
Copyright (c) 1990-1997 Parametric Technology Corp.
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.
Government is subject to restrictions as set forth in subparagraph
(c)(1)(ii) of the Rights in Technical Data and Computer Software
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041
*/

/*
Compute hinge torques for tree hinges
*/
    tauc[0] = (mtau[0]+utau[0]);
    tauc[1] = (mtau[1]+utau[1]);
    tauc[2] = (mtau[2]+utau[2]);
    tauc[3] = (mtau[3]+utau[3]);
    tauc[4] = (mtau[4]+utau[4]);
    tauc[5] = (mtau[5]+utau[5]);
    sddoiner();
/*
Compute onk & onb (angular accels in N)
*/
    Onkb[1][1] = (udot[0]*c1);
    Onkb[1][2] = -(udot[0]*s1);
    Onkb[3][0] = (udot[3]+((Cik[3][2][0]*Onkb[1][2])+((Cik[3][0][0]*udot[1])+(
      Cik[3][1][0]*Onkb[1][1]))));
    Onkb[3][1] = ((Cik[3][2][1]*Onkb[1][2])+((Cik[3][0][1]*udot[1])+(
      Cik[3][1][1]*Onkb[1][1])));
    Onkb[3][2] = ((Cik[3][2][2]*Onkb[1][2])+((Cik[3][0][2]*udot[1])+(
      Cik[3][1][2]*Onkb[1][1])));
    Onkb[4][1] = (Onkb[3][1]+udot[4]);
    Onkb[5][2] = (Onkb[3][2]+udot[5]);
    onk[1][1] = (Onkb[1][1]+Otk[1][1]);
    onk[1][2] = (Onkb[1][2]+Otk[1][2]);
    onk[2][1] = (Onkb[1][1]+Otk[1][1]);
    onk[2][2] = (Onkb[1][2]+Otk[1][2]);
    onk[5][0] = (Onkb[3][0]+Otk[5][0]);
    onk[5][1] = (Onkb[4][1]+Otk[5][1]);
    onk[5][2] = (Onkb[5][2]+Otk[5][2]);
    onb[0][0] = udot[1];
    onb[0][1] = onk[1][1];
    onb[0][2] = onk[1][2];
    onb[1][0] = udot[1];
    onb[1][1] = onk[2][1];
    onb[1][2] = onk[2][2];
    onb[2][0] = onk[5][0];
    onb[2][1] = onk[5][1];
    onb[2][2] = onk[5][2];
/*
Compute acceleration dyadics
*/
    dyad[0][0][0] = w11w22[0];
    dyad[0][0][1] = (w0w1[0]-onk[1][2]);
    dyad[0][0][2] = (onk[1][1]+w0w2[0]);
    dyad[0][1][0] = (onk[1][2]+w0w1[0]);
    dyad[0][1][1] = w00w22[0];
    dyad[0][1][2] = (w1w2[0]-udot[1]);
    dyad[0][2][0] = (w0w2[0]-onk[1][1]);
    dyad[0][2][1] = (udot[1]+w1w2[0]);
    dyad[0][2][2] = w00w11[0];
    dyad[1][0][0] = w11w22[1];
    dyad[1][0][1] = (w0w1[1]-onk[2][2]);
    dyad[1][0][2] = (onk[2][1]+w0w2[1]);
    dyad[1][1][0] = (onk[2][2]+w0w1[1]);
    dyad[1][1][1] = w00w22[1];
    dyad[1][1][2] = (w1w2[1]-udot[1]);
    dyad[1][2][0] = (w0w2[1]-onk[2][1]);
    dyad[1][2][1] = (udot[1]+w1w2[1]);
    dyad[1][2][2] = w00w11[1];
    dyad[2][0][0] = w11w22[2];
    dyad[2][0][1] = (w0w1[2]-onk[5][2]);
    dyad[2][0][2] = (onk[5][1]+w0w2[2]);
    dyad[2][1][0] = (onk[5][2]+w0w1[2]);
    dyad[2][1][1] = w00w22[2];
    dyad[2][1][2] = (w1w2[2]-onk[5][0]);
    dyad[2][2][0] = (w0w2[2]-onk[5][1]);
    dyad[2][2][1] = (onk[5][0]+w1w2[2]);
    dyad[2][2][2] = w00w11[2];
/*
Compute ank & anb (mass center linear accels in N)
*/
    Ankb[2][0] = -(Onkb[1][2]*q[2]);
    Ankb[2][2] = (q[2]*udot[1]);
    Ankb[3][0] = ((Ankb[2][2]*Cik[3][2][0])+((Ankb[2][0]*Cik[3][0][0])+(
      Cik[3][1][0]*udot[2])));
    Ankb[3][1] = ((Ankb[2][2]*Cik[3][2][1])+((Ankb[2][0]*Cik[3][0][1])+(
      Cik[3][1][1]*udot[2])));
    Ankb[3][2] = ((Ankb[2][2]*Cik[3][2][2])+((Ankb[2][0]*Cik[3][0][2])+(
      Cik[3][1][2]*udot[2])));
    AnkAtk[2][0] = (Ankb[2][0]+Atk[2][0]);
    AnkAtk[2][1] = (Atk[2][1]+udot[2]);
    AnkAtk[2][2] = (Ankb[2][2]+Atk[2][2]);
    ank[2][0] = ((AnkAtk[2][2]*cnk[1][0][2])+((AnkAtk[2][0]*c0)+(AnkAtk[2][1]*
      cnk[1][0][1])));
    ank[2][1] = ((AnkAtk[2][1]*c1)-(AnkAtk[2][2]*s1));
    ank[2][2] = ((AnkAtk[2][2]*cnk[1][2][2])+((AnkAtk[2][1]*cnk[1][2][1])-(
      AnkAtk[2][0]*s0)));
    AnkAtk[5][0] = (Ankb[3][0]+Atk[3][0]);
    AnkAtk[5][1] = (Ankb[3][1]+Atk[3][1]);
    AnkAtk[5][2] = (Ankb[3][2]+Atk[3][2]);
    ank[5][0] = ((AnkAtk[5][2]*cnk[3][0][2])+((AnkAtk[5][0]*cnk[3][0][0])+(
      AnkAtk[5][1]*cnk[3][0][1])));
    ank[5][1] = ((AnkAtk[5][2]*cnk[3][1][2])+((AnkAtk[5][0]*cnk[3][1][0])+(
      AnkAtk[5][1]*cnk[3][1][1])));
    ank[5][2] = ((AnkAtk[5][2]*cnk[3][2][2])+((AnkAtk[5][0]*cnk[3][2][0])+(
      AnkAtk[5][1]*cnk[3][2][1])));
    anb[0][0] = 0.;
    anb[0][1] = 0.;
    anb[0][2] = 0.;
    anb[1][0] = ank[2][0];
    anb[1][1] = ank[2][1];
    anb[1][2] = ank[2][2];
    anb[2][0] = ank[5][0];
    anb[2][1] = ank[5][1];
    anb[2][2] = ank[5][2];
/*
Compute constraint acceleration errors
*/
    roustate = 3;
    sduaerr(curtim,q,u,udot,&aerr[1]);
/*
 Used -0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   65 adds/subtracts/negates
                     39 multiplies
                      0 divides
                     82 assignments
*/
}

void sdmassmat(double mmat[6][6])
{
/* Calculate the system mass matrix
*/
    int i,j;
    double udotin[6],mmrow[6],biastrq[6];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(57,23);
        return;
    }
    for (i = 0; i < 6; i++) {
        udotin[i] = 0.;
    }
    sdcomptrq(udotin,biastrq);
    for (i = 0; i < 6; i++) {
        udotin[i] = 1.;
        sdcomptrq(udotin,mmrow);
        udotin[i] = 0.;
        for (j = i; j <= 5; j++) {
            mmat[i][j] = mmrow[j]-biastrq[j];
            mmat[j][i] = mmat[i][j];
        }
    }
/*
Check for singular mass matrix
*/
    for (i = 0; i < 6; i++) {
        if (mmat[i][i] <= 1e-13) {
            sdseterr(57,47);
        }
    }
}

void sdfrcmat(double fmat[6])
{
/* Return the system force matrix (RHS), excluding constraints
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(58,23);
        return;
    }
    sddofs0();
    for (i = 0; i < 6; i++) {
        fmat[i] = fs0[i];
    }
}

void sdpseudo(double lqout[1],
    double luout[1])
{
/*
Return pseudo-coordinates for loop joints.

*/
/*
There are no loop joints in this system.

*/
}

void sdpsqdot(double lqdout[1])
{
/*
Return pseudo-coordinate derivatives for loop joints.

*/
/*
There are no loop joints in this system.

*/
}

void sdpsudot(double ludout[1])
{
/*
Return pseudo-coordinate accelerations for loop joints.

*/
/*
There are no loop joints in this system.

*/
}

void sdperr(double errs[3])
{
/*
Return position constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(26,23);
        return;
    }
    if (pres[2]  !=  0.) {
        perr[0] = (q[2]-upos[2]);
    } else {
        perr[0] = 0.;
    }
    errs[0] = perr[0];
    errs[1] = perr[1];
    errs[2] = perr[2];
}

void sdverr(double errs[3])
{
/*
Return velocity constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(27,23);
        return;
    }
    if (pres[2]  !=  0.) {
        verr[0] = (u[2]-uvel[2]);
    } else {
        verr[0] = 0.;
    }
    errs[0] = verr[0];
    errs[1] = verr[1];
    errs[2] = verr[2];
}

void sdaerr(double errs[3])
{
/*
Return acceleration constraint errors.

*/

    if (roustate != 3) {
        sdseterr(35,24);
        return;
    }
    if (pres[2]  !=  0.) {
        aerr[0] = (udot[2]-uacc[2]);
    } else {
        aerr[0] = 0.;
    }
    errs[0] = aerr[0];
    errs[1] = aerr[1];
    errs[2] = aerr[2];
}
int 
sdchkbnum(int routine,
    int bnum)
{

    if ((bnum < -1) || (bnum > 2)) {
        sdseterr(routine,15);
        return 1;
    }
    return 0;
}
int 
sdchkjnum(int routine,
    int jnum)
{

    if ((jnum < 0) || (jnum > 2)) {
        sdseterr(routine,16);
        return 1;
    }
    return 0;
}
int 
sdchkucnum(int routine,
    int ucnum)
{

    if ((ucnum < 0) || (ucnum > 1)) {
        sdseterr(routine,21);
        return 1;
    }
    return 0;
}
int 
sdchkjaxis(int routine,
    int jnum,
    int axnum)
{
    int maxax;

    if (sdchkjnum(routine,jnum) != 0) {
        return 1;
    }
    if ((axnum < 0) || (axnum > 6)) {
        sdseterr(routine,17);
        return 1;
    }
    maxax = njntdof[jnum]-1;
    if ((jtype[jnum] == 4) || (jtype[jnum] == 6) || (jtype[jnum] == 21)) {
        maxax = maxax+1;
    }
    if (axnum > maxax) {
        sdseterr(routine,18);
        return 1;
    }
    return 0;
}
int 
sdchkjpin(int routine,
    int jnum,
    int pinno)
{
    int maxax,pinok;

    if (sdchkjnum(routine,jnum) != 0) {
        return 1;
    }
    if ((pinno < 0) || (pinno > 5)) {
        sdseterr(routine,17);
        return 1;
    }
    if (njntdof[jnum] >= 3) {
        maxax = 2;
    } else {
        maxax = njntdof[jnum]-1;
    }
    if (jtype[jnum] == 4) {
        maxax = -1;
    }
    if (jtype[jnum] == 7) {
        maxax = 0;
    }
    pinok = 0;
    if (pinno <= maxax) {
        pinok = 1;
    }
    if (pinok == 0) {
        sdseterr(routine,18);
        return 1;
    }
    return 0;
}
int 
sdindx(int joint,
    int axis)
{
    int offs,gotit;

    if (sdchkjaxis(36,joint,axis) != 0) {
        return 0;
    }
    gotit = 0;
    if (jtype[joint] == 4) {
        if (axis == 3) {
            offs = ballq[joint];
            gotit = 1;
        }
    } else {
        if ((jtype[joint] == 6) || (jtype[joint] == 21)) {
            if (axis == 6) {
                offs = ballq[joint];
                gotit = 1;
            }
        }
    }
    if (gotit == 0) {
        offs = firstq[joint]+axis;
    }
    return offs;
}

void sdpresacc(int joint,
    int axis,
    double prval)
{

    if (sdchkjaxis(13,joint,axis) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(13,23);
        return;
    }
    if (pres[sdindx(joint,axis)]  !=  0.) {
        uacc[sdindx(joint,axis)] = prval;
    }
}

void sdpresvel(int joint,
    int axis,
    double prval)
{

    if (sdchkjaxis(14,joint,axis) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(14,23);
        return;
    }
    if (pres[sdindx(joint,axis)]  !=  0.) {
        uvel[sdindx(joint,axis)] = prval;
    }
}

void sdprespos(int joint,
    int axis,
    double prval)
{

    if (sdchkjaxis(15,joint,axis) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(15,23);
        return;
    }
    if (pres[sdindx(joint,axis)]  !=  0.) {
        upos[sdindx(joint,axis)] = prval;
    }
}

void sdgetht(int joint,
    int axis,
    double *torque)
{

    if (sdchkjaxis(30,joint,axis) != 0) {
        return;
    }
    if (roustate != 3) {
        sdseterr(30,24);
        return;
    }
    *torque = tauc[sdindx(joint,axis)];
}

void sdhinget(int joint,
    int axis,
    double torque)
{

    if (sdchkjaxis(10,joint,axis) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(10,23);
        return;
    }
    if (mfrcflg != 0) {
        mtau[sdindx(joint,axis)] = mtau[sdindx(joint,axis)]+torque;
    } else {
        fs0flg = 0;
        utau[sdindx(joint,axis)] = utau[sdindx(joint,axis)]+torque;
    }
}

void sdpointf(int body,
    double point[3],
    double force[3])
{
    double torque[3];

    if (sdchkbnum(11,body) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(11,23);
        return;
    }
    if (body == -1) {
        return;
    }
    torque[0] = point[1]*force[2]-point[2]*force[1];
    torque[1] = point[2]*force[0]-point[0]*force[2];
    torque[2] = point[0]*force[1]-point[1]*force[0];
    if (mfrcflg != 0) {
        mfk[body][0] = mfk[body][0]+force[0];
        mtk[body][0] = mtk[body][0]+torque[0];
        mfk[body][1] = mfk[body][1]+force[1];
        mtk[body][1] = mtk[body][1]+torque[1];
        mfk[body][2] = mfk[body][2]+force[2];
        mtk[body][2] = mtk[body][2]+torque[2];
    } else {
        fs0flg = 0;
        ufk[body][0] = ufk[body][0]+force[0];
        utk[body][0] = utk[body][0]+torque[0];
        ufk[body][1] = ufk[body][1]+force[1];
        utk[body][1] = utk[body][1]+torque[1];
        ufk[body][2] = ufk[body][2]+force[2];
        utk[body][2] = utk[body][2]+torque[2];
    }
}

void sdbodyt(int body,
    double torque[3])
{

    if (sdchkbnum(12,body) != 0) {
        return;
    }
    if (roustate != 2) {
        sdseterr(12,23);
        return;
    }
    if (body == -1) {
        return;
    }
    if (mfrcflg != 0) {
        mtk[body][0] = mtk[body][0]+torque[0];
        mtk[body][1] = mtk[body][1]+torque[1];
        mtk[body][2] = mtk[body][2]+torque[2];
    } else {
        fs0flg = 0;
        utk[body][0] = utk[body][0]+torque[0];
        utk[body][1] = utk[body][1]+torque[1];
        utk[body][2] = utk[body][2]+torque[2];
    }
}

void sddoww(int routine)
{
    double pp[3][6],dpp[6][3];
    int i,j,c;
    double sum;
    double dfk[3][3],dtk[3][3],dtau[6],dltci[1][3],dltc[1][3],dlfci[1][3],dlfc[1
      ][3];
    double dTinb[1][3],dToutb[1][3],dltaufi[1][3],dltaufo[1][3],dltauti[1][3],
      dltauto[1][3];
    double umult[2];
    double deps[6],dZ1[6][3],dZ2[6][3];

    roustate = 2;
    if (wwflg == 0) {
/*
Compute constraint effects
*/
        sddomm(routine);
/*
Constraint 0 (prescribed motion)
*/
        if (pres[2]  !=  0.) {
            dtau[2] = 1.;
        } else {
            dtau[2] = 0.;
        }
        deps[5] = 0.;
        deps[4] = 0.;
        deps[3] = 0.;
        deps[2] = dtau[2];
        dZ1[2][0] = (deps[2]*G1[2][0]);
        dZ1[2][2] = (deps[2]*G1[2][2]);
        dZ1[1][0] = dZ1[2][0];
        dZ1[1][1] = deps[2];
        dZ1[1][2] = dZ1[2][2];
        dZ2[1][0] = (dZ1[2][2]*q[2]);
        dZ2[1][2] = -(dZ1[2][0]*q[2]);
        deps[1] = -dZ2[1][0];
        dZ1[1][0] = (dZ1[1][0]+(deps[1]*G1[1][0]));
        dZ1[1][1] = (dZ1[1][1]+(deps[1]*G1[1][1]));
        dZ1[1][2] = (dZ1[1][2]+(deps[1]*G1[1][2]));
        dZ2[1][0] = (deps[1]+dZ2[1][0]);
        dZ2[1][2] = (dZ2[1][2]+(deps[1]*G2[1][2]));
        dZ1[0][0] = dZ1[1][0];
        dZ1[0][1] = ((dZ1[1][1]*c1)-(dZ1[1][2]*s1));
        dZ1[0][2] = ((dZ1[1][1]*s1)+(dZ1[1][2]*c1));
        dZ2[0][0] = dZ2[1][0];
        dZ2[0][1] = -(dZ2[1][2]*s1);
        dZ2[0][2] = (dZ2[1][2]*c1);
        deps[0] = -dZ2[0][1];
        for (i = 0; i <= 5; i++) {
            pp[0][i] = deps[i];
            dpp[i][0] = DD[i]*deps[i];
        }
        wmap[0] = 0;
/*
Constraint 1 (user constraint)
*/
/*
Initialize all multiplier forces to zero.
*/
        for (i = 0; i <= 2; i++) {
            for (j = 0; j <= 2; j++) {
                mfk[i][j] = 0.;
                mtk[i][j] = 0.;
            }
        }
        for (i = 0; i <= 5; i++) {
            mtau[i] = 0.;
        }
/*
Compute user-generated multiplier forces
*/
        umult[0] = 1.;
        umult[1] = 0.;
        mfrcflg = 1;
        sduconsfrc(curtim,q,u,umult);
        mfrcflg = 0;
        deps[5] = (mtau[5]+mtk[2][2]);
        dZ2[5][2] = (deps[5]-mtk[2][2]);
        dZ1[4][0] = -mfk[2][0];
        dZ1[4][1] = -mfk[2][1];
        dZ1[4][2] = -mfk[2][2];
        dZ2[4][0] = -mtk[2][0];
        dZ2[4][1] = -mtk[2][1];
        dZ2[4][2] = dZ2[5][2];
        deps[4] = (mtau[4]-dZ2[4][1]);
        dZ2[4][1] = (deps[4]+dZ2[4][1]);
        dZ1[3][0] = dZ1[4][0];
        dZ1[3][1] = dZ1[4][1];
        dZ1[3][2] = dZ1[4][2];
        dZ2[3][0] = dZ2[4][0];
        dZ2[3][1] = dZ2[4][1];
        dZ2[3][2] = dZ2[4][2];
        deps[3] = (mtau[3]-dZ2[3][0]);
        dZ2[3][0] = (deps[3]+dZ2[3][0]);
        dZ1[2][0] = (((Cik[3][0][2]*dZ1[3][2])+((Cik[3][0][0]*dZ1[3][0])+(
          Cik[3][0][1]*dZ1[3][1])))-mfk[1][0]);
        dZ1[2][1] = (((Cik[3][1][2]*dZ1[3][2])+((Cik[3][1][0]*dZ1[3][0])+(
          Cik[3][1][1]*dZ1[3][1])))-mfk[1][1]);
        dZ1[2][2] = (((Cik[3][2][2]*dZ1[3][2])+((Cik[3][2][0]*dZ1[3][0])+(
          Cik[3][2][1]*dZ1[3][1])))-mfk[1][2]);
        dZ2[2][0] = (((Cik[3][0][2]*dZ2[3][2])+((Cik[3][0][0]*dZ2[3][0])+(
          Cik[3][0][1]*dZ2[3][1])))-mtk[1][0]);
        dZ2[2][1] = (((Cik[3][1][2]*dZ2[3][2])+((Cik[3][1][0]*dZ2[3][0])+(
          Cik[3][1][1]*dZ2[3][1])))-mtk[1][1]);
        dZ2[2][2] = (((Cik[3][2][2]*dZ2[3][2])+((Cik[3][2][0]*dZ2[3][0])+(
          Cik[3][2][1]*dZ2[3][1])))-mtk[1][2]);
        deps[2] = (mtau[2]-dZ1[2][1]);
        dZ1[2][0] = (dZ1[2][0]+(deps[2]*G1[2][0]));
        dZ1[2][1] = (deps[2]+dZ1[2][1]);
        dZ1[2][2] = (dZ1[2][2]+(deps[2]*G1[2][2]));
        dZ1[1][0] = (dZ1[2][0]-mfk[0][0]);
        dZ1[1][1] = (dZ1[2][1]-mfk[0][1]);
        dZ1[1][2] = (dZ1[2][2]-mfk[0][2]);
        dZ2[1][0] = ((dZ2[2][0]+(dZ1[2][2]*q[2]))-mtk[0][0]);
        dZ2[1][1] = (dZ2[2][1]-mtk[0][1]);
        dZ2[1][2] = ((dZ2[2][2]-(dZ1[2][0]*q[2]))-mtk[0][2]);
        deps[1] = (mtau[1]-dZ2[1][0]);
        dZ1[1][0] = (dZ1[1][0]+(deps[1]*G1[1][0]));
        dZ1[1][1] = (dZ1[1][1]+(deps[1]*G1[1][1]));
        dZ1[1][2] = (dZ1[1][2]+(deps[1]*G1[1][2]));
        dZ2[1][0] = (deps[1]+dZ2[1][0]);
        dZ2[1][2] = (dZ2[1][2]+(deps[1]*G2[1][2]));
        dZ1[0][0] = dZ1[1][0];
        dZ1[0][1] = ((dZ1[1][1]*c1)-(dZ1[1][2]*s1));
        dZ1[0][2] = ((dZ1[1][1]*s1)+(dZ1[1][2]*c1));
        dZ2[0][0] = dZ2[1][0];
        dZ2[0][1] = ((dZ2[1][1]*c1)-(dZ2[1][2]*s1));
        dZ2[0][2] = ((dZ2[1][1]*s1)+(dZ2[1][2]*c1));
        deps[0] = (mtau[0]-dZ2[0][1]);
        for (i = 0; i <= 5; i++) {
            pp[1][i] = deps[i];
            dpp[i][1] = DD[i]*deps[i];
        }
        wmap[1] = 1;
/*
Constraint 2 (user constraint)
*/
/*
Initialize all multiplier forces to zero.
*/
        for (i = 0; i <= 2; i++) {
            for (j = 0; j <= 2; j++) {
                mfk[i][j] = 0.;
                mtk[i][j] = 0.;
            }
        }
        for (i = 0; i <= 5; i++) {
            mtau[i] = 0.;
        }
/*
Compute user-generated multiplier forces
*/
        umult[0] = 0.;
        umult[1] = 1.;
        mfrcflg = 1;
        sduconsfrc(curtim,q,u,umult);
        mfrcflg = 0;
        deps[5] = (mtau[5]+mtk[2][2]);
        dZ2[5][2] = (deps[5]-mtk[2][2]);
        dZ1[4][0] = -mfk[2][0];
        dZ1[4][1] = -mfk[2][1];
        dZ1[4][2] = -mfk[2][2];
        dZ2[4][0] = -mtk[2][0];
        dZ2[4][1] = -mtk[2][1];
        dZ2[4][2] = dZ2[5][2];
        deps[4] = (mtau[4]-dZ2[4][1]);
        dZ2[4][1] = (deps[4]+dZ2[4][1]);
        dZ1[3][0] = dZ1[4][0];
        dZ1[3][1] = dZ1[4][1];
        dZ1[3][2] = dZ1[4][2];
        dZ2[3][0] = dZ2[4][0];
        dZ2[3][1] = dZ2[4][1];
        dZ2[3][2] = dZ2[4][2];
        deps[3] = (mtau[3]-dZ2[3][0]);
        dZ2[3][0] = (deps[3]+dZ2[3][0]);
        dZ1[2][0] = (((Cik[3][0][2]*dZ1[3][2])+((Cik[3][0][0]*dZ1[3][0])+(
          Cik[3][0][1]*dZ1[3][1])))-mfk[1][0]);
        dZ1[2][1] = (((Cik[3][1][2]*dZ1[3][2])+((Cik[3][1][0]*dZ1[3][0])+(
          Cik[3][1][1]*dZ1[3][1])))-mfk[1][1]);
        dZ1[2][2] = (((Cik[3][2][2]*dZ1[3][2])+((Cik[3][2][0]*dZ1[3][0])+(
          Cik[3][2][1]*dZ1[3][1])))-mfk[1][2]);
        dZ2[2][0] = (((Cik[3][0][2]*dZ2[3][2])+((Cik[3][0][0]*dZ2[3][0])+(
          Cik[3][0][1]*dZ2[3][1])))-mtk[1][0]);
        dZ2[2][1] = (((Cik[3][1][2]*dZ2[3][2])+((Cik[3][1][0]*dZ2[3][0])+(
          Cik[3][1][1]*dZ2[3][1])))-mtk[1][1]);
        dZ2[2][2] = (((Cik[3][2][2]*dZ2[3][2])+((Cik[3][2][0]*dZ2[3][0])+(
          Cik[3][2][1]*dZ2[3][1])))-mtk[1][2]);
        deps[2] = (mtau[2]-dZ1[2][1]);
        dZ1[2][0] = (dZ1[2][0]+(deps[2]*G1[2][0]));
        dZ1[2][1] = (deps[2]+dZ1[2][1]);
        dZ1[2][2] = (dZ1[2][2]+(deps[2]*G1[2][2]));
        dZ1[1][0] = (dZ1[2][0]-mfk[0][0]);
        dZ1[1][1] = (dZ1[2][1]-mfk[0][1]);
        dZ1[1][2] = (dZ1[2][2]-mfk[0][2]);
        dZ2[1][0] = ((dZ2[2][0]+(dZ1[2][2]*q[2]))-mtk[0][0]);
        dZ2[1][1] = (dZ2[2][1]-mtk[0][1]);
        dZ2[1][2] = ((dZ2[2][2]-(dZ1[2][0]*q[2]))-mtk[0][2]);
        deps[1] = (mtau[1]-dZ2[1][0]);
        dZ1[1][0] = (dZ1[1][0]+(deps[1]*G1[1][0]));
        dZ1[1][1] = (dZ1[1][1]+(deps[1]*G1[1][1]));
        dZ1[1][2] = (dZ1[1][2]+(deps[1]*G1[1][2]));
        dZ2[1][0] = (deps[1]+dZ2[1][0]);
        dZ2[1][2] = (dZ2[1][2]+(deps[1]*G2[1][2]));
        dZ1[0][0] = dZ1[1][0];
        dZ1[0][1] = ((dZ1[1][1]*c1)-(dZ1[1][2]*s1));
        dZ1[0][2] = ((dZ1[1][1]*s1)+(dZ1[1][2]*c1));
        dZ2[0][0] = dZ2[1][0];
        dZ2[0][1] = ((dZ2[1][1]*c1)-(dZ2[1][2]*s1));
        dZ2[0][2] = ((dZ2[1][1]*s1)+(dZ2[1][2]*c1));
        deps[0] = (mtau[0]-dZ2[0][1]);
        for (i = 0; i <= 5; i++) {
            pp[2][i] = deps[i];
            dpp[i][2] = DD[i]*deps[i];
        }
        wmap[2] = 2;
/*
Produce constraint coefficient matrix WW
*/
        for (c = 0; c <= 2; c++) {
            for (i = c; i <= 2; i++) {
                sum = 0.;
                for (j = 0; j <= 5; j++) {
                    sum = sum+pp[wmap[c]][j]*dpp[j][wmap[i]];
                }
                ww[wmap[c]][wmap[i]] = sum;
                ww[wmap[i]][wmap[c]] = sum;
            }
        }
/*
Form QR decomposition of WW
*/
        sdqrdcomp(3,3,3,3,wmap,wmap,ww,qraux,jpvt);
        wwflg = 1;
    }
/*
 Used 0.02 seconds CPU time,
 0 additional bytes of memory.
 Equations contain  151 adds/subtracts/negates
                    136 multiplies
                      0 divides
                    262 assignments
*/
}

void sdxudot0(int routine,
    double oudot0[6])
{
/*
Compute unconstrained equations
*/
    int i;
    double eps[6],Z1[6][3],Z2[6][3],A1[6][3],A2[6][3],K1[6][3],K2[6][3];

    sdlhs(routine);
/*
Solve equations ignoring constraints
*/
    eps[5] = fs0[5];
    Z2[4][2] = eps[5];
    eps[4] = fs0[4];
    Z2[3][1] = eps[4];
    Z2[3][2] = Z2[4][2];
    eps[3] = fs0[3];
    Z2[2][0] = ((Cik[3][0][2]*Z2[3][2])+((Cik[3][0][0]*eps[3])+(Cik[3][0][1]*
      Z2[3][1])));
    Z2[2][1] = ((Cik[3][1][2]*Z2[3][2])+((Cik[3][1][0]*eps[3])+(Cik[3][1][1]*
      Z2[3][1])));
    Z2[2][2] = ((Cik[3][2][2]*Z2[3][2])+((Cik[3][2][0]*eps[3])+(Cik[3][2][1]*
      Z2[3][1])));
    eps[2] = fs0[2];
    Z1[2][0] = (eps[2]*G1[2][0]);
    Z1[2][2] = (eps[2]*G1[2][2]);
    Z1[1][0] = Z1[2][0];
    Z1[1][1] = eps[2];
    Z1[1][2] = Z1[2][2];
    Z2[1][0] = (Z2[2][0]+(q[2]*Z1[2][2]));
    Z2[1][1] = Z2[2][1];
    Z2[1][2] = (Z2[2][2]-(q[2]*Z1[2][0]));
    eps[1] = (fs0[1]-Z2[1][0]);
    Z1[1][0] = (Z1[1][0]+(eps[1]*G1[1][0]));
    Z1[1][1] = (Z1[1][1]+(eps[1]*G1[1][1]));
    Z1[1][2] = (Z1[1][2]+(eps[1]*G1[1][2]));
    Z2[1][0] = (eps[1]+Z2[1][0]);
    Z2[1][2] = (Z2[1][2]+(eps[1]*G2[1][2]));
    Z1[0][0] = Z1[1][0];
    Z1[0][1] = ((Z1[1][1]*c1)-(Z1[1][2]*s1));
    Z1[0][2] = ((Z1[1][1]*s1)+(Z1[1][2]*c1));
    Z2[0][0] = Z2[1][0];
    Z2[0][1] = ((Z2[1][1]*c1)-(Z2[1][2]*s1));
    Z2[0][2] = ((Z2[1][1]*s1)+(Z2[1][2]*c1));
    eps[0] = (fs0[0]-Z2[0][1]);
    udot[0] = (DD[0]*eps[0]);
    K2[1][1] = (udot[0]*c1);
    K2[1][2] = -(udot[0]*s1);
    udot[1] = ((DD[1]*eps[1])-(G2[1][2]*K2[1][2]));
    K1[2][0] = -(K2[1][2]*q[2]);
    K1[2][2] = (q[2]*udot[1]);
    udot[2] = ((DD[2]*eps[2])-((G1[2][0]*K1[2][0])+(G1[2][2]*K1[2][2])));
    K1[3][0] = ((Cik[3][2][0]*K1[2][2])+((Cik[3][0][0]*K1[2][0])+(Cik[3][1][0]*
      udot[2])));
    K1[3][1] = ((Cik[3][2][1]*K1[2][2])+((Cik[3][0][1]*K1[2][0])+(Cik[3][1][1]*
      udot[2])));
    K1[3][2] = ((Cik[3][2][2]*K1[2][2])+((Cik[3][0][2]*K1[2][0])+(Cik[3][1][2]*
      udot[2])));
    K2[3][0] = ((Cik[3][2][0]*K2[1][2])+((Cik[3][0][0]*udot[1])+(Cik[3][1][0]*
      K2[1][1])));
    K2[3][1] = ((Cik[3][2][1]*K2[1][2])+((Cik[3][0][1]*udot[1])+(Cik[3][1][1]*
      K2[1][1])));
    K2[3][2] = ((Cik[3][2][2]*K2[1][2])+((Cik[3][0][2]*udot[1])+(Cik[3][1][2]*
      K2[1][1])));
    udot[3] = ((2.5*eps[3])-K2[3][0]);
    A2[3][0] = (K2[3][0]+udot[3]);
    udot[4] = ((2.5*eps[4])-K2[3][1]);
    A2[4][1] = (K2[3][1]+udot[4]);
    udot[5] = ((2.5*eps[5])-K2[3][2]);
    A2[5][2] = (K2[3][2]+udot[5]);
    for (i = 0; i <= 5; i++) {
        oudot0[i] = udot[i];
    }
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   42 adds/subtracts/negates
                     56 multiplies
                      0 divides
                     56 assignments
*/
}

void sdudot0(double oudot0[6])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(66,23);
        return;
    }
    sdxudot0(66,oudot0);
}

void sdsetudot(double iudot[6])
{
/*
Assign udots and advance to stage Dynamics Ready
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(68,23);
        return;
    }
    for (i = 0; i <= 5; i++) {
        udot[i] = iudot[i];
    }
    sdrhs();
}

void sdxudotm(int routine,
    double imult[3],
    double oudotm[6])
{
/*
Compute udots due only to multipliers
*/
    int i;
    double eps[6],Z1[6][3],Z2[6][3],A1[6][3],A2[6][3],K1[6][3],K2[6][3];

    sdlhs(routine);
    sdmfrc(imult);
    eps[5] = (mtau[5]+mtk[2][2]);
    Z2[5][2] = (eps[5]-mtk[2][2]);
    Z1[4][0] = -mfk[2][0];
    Z1[4][1] = -mfk[2][1];
    Z1[4][2] = -mfk[2][2];
    Z2[4][0] = -mtk[2][0];
    Z2[4][1] = -mtk[2][1];
    Z2[4][2] = Z2[5][2];
    eps[4] = (mtau[4]-Z2[4][1]);
    Z2[4][1] = (eps[4]+Z2[4][1]);
    Z1[3][0] = Z1[4][0];
    Z1[3][1] = Z1[4][1];
    Z1[3][2] = Z1[4][2];
    Z2[3][0] = Z2[4][0];
    Z2[3][1] = Z2[4][1];
    Z2[3][2] = Z2[4][2];
    eps[3] = (mtau[3]-Z2[3][0]);
    Z2[3][0] = (eps[3]+Z2[3][0]);
    Z1[2][0] = (((Cik[3][0][2]*Z1[3][2])+((Cik[3][0][0]*Z1[3][0])+(Cik[3][0][1]*
      Z1[3][1])))-mfk[1][0]);
    Z1[2][1] = (((Cik[3][1][2]*Z1[3][2])+((Cik[3][1][0]*Z1[3][0])+(Cik[3][1][1]*
      Z1[3][1])))-mfk[1][1]);
    Z1[2][2] = (((Cik[3][2][2]*Z1[3][2])+((Cik[3][2][0]*Z1[3][0])+(Cik[3][2][1]*
      Z1[3][1])))-mfk[1][2]);
    Z2[2][0] = (((Cik[3][0][2]*Z2[3][2])+((Cik[3][0][0]*Z2[3][0])+(Cik[3][0][1]*
      Z2[3][1])))-mtk[1][0]);
    Z2[2][1] = (((Cik[3][1][2]*Z2[3][2])+((Cik[3][1][0]*Z2[3][0])+(Cik[3][1][1]*
      Z2[3][1])))-mtk[1][1]);
    Z2[2][2] = (((Cik[3][2][2]*Z2[3][2])+((Cik[3][2][0]*Z2[3][0])+(Cik[3][2][1]*
      Z2[3][1])))-mtk[1][2]);
    eps[2] = (mtau[2]-Z1[2][1]);
    Z1[2][0] = (Z1[2][0]+(eps[2]*G1[2][0]));
    Z1[2][1] = (eps[2]+Z1[2][1]);
    Z1[2][2] = (Z1[2][2]+(eps[2]*G1[2][2]));
    Z1[1][0] = (Z1[2][0]-mfk[0][0]);
    Z1[1][1] = (Z1[2][1]-mfk[0][1]);
    Z1[1][2] = (Z1[2][2]-mfk[0][2]);
    Z2[1][0] = ((Z2[2][0]+(q[2]*Z1[2][2]))-mtk[0][0]);
    Z2[1][1] = (Z2[2][1]-mtk[0][1]);
    Z2[1][2] = ((Z2[2][2]-(q[2]*Z1[2][0]))-mtk[0][2]);
    eps[1] = (mtau[1]-Z2[1][0]);
    Z1[1][0] = (Z1[1][0]+(eps[1]*G1[1][0]));
    Z1[1][1] = (Z1[1][1]+(eps[1]*G1[1][1]));
    Z1[1][2] = (Z1[1][2]+(eps[1]*G1[1][2]));
    Z2[1][0] = (eps[1]+Z2[1][0]);
    Z2[1][2] = (Z2[1][2]+(eps[1]*G2[1][2]));
    Z1[0][0] = Z1[1][0];
    Z1[0][1] = ((Z1[1][1]*c1)-(Z1[1][2]*s1));
    Z1[0][2] = ((Z1[1][1]*s1)+(Z1[1][2]*c1));
    Z2[0][0] = Z2[1][0];
    Z2[0][1] = ((Z2[1][1]*c1)-(Z2[1][2]*s1));
    Z2[0][2] = ((Z2[1][1]*s1)+(Z2[1][2]*c1));
    eps[0] = (mtau[0]-Z2[0][1]);
    udot[0] = (DD[0]*eps[0]);
    K2[1][1] = (udot[0]*c1);
    K2[1][2] = -(udot[0]*s1);
    udot[1] = ((DD[1]*eps[1])-(G2[1][2]*K2[1][2]));
    K1[2][0] = -(K2[1][2]*q[2]);
    K1[2][2] = (q[2]*udot[1]);
    udot[2] = ((DD[2]*eps[2])-((G1[2][0]*K1[2][0])+(G1[2][2]*K1[2][2])));
    K1[3][0] = ((Cik[3][2][0]*K1[2][2])+((Cik[3][0][0]*K1[2][0])+(Cik[3][1][0]*
      udot[2])));
    K1[3][1] = ((Cik[3][2][1]*K1[2][2])+((Cik[3][0][1]*K1[2][0])+(Cik[3][1][1]*
      udot[2])));
    K1[3][2] = ((Cik[3][2][2]*K1[2][2])+((Cik[3][0][2]*K1[2][0])+(Cik[3][1][2]*
      udot[2])));
    K2[3][0] = ((Cik[3][2][0]*K2[1][2])+((Cik[3][0][0]*udot[1])+(Cik[3][1][0]*
      K2[1][1])));
    K2[3][1] = ((Cik[3][2][1]*K2[1][2])+((Cik[3][0][1]*udot[1])+(Cik[3][1][1]*
      K2[1][1])));
    K2[3][2] = ((Cik[3][2][2]*K2[1][2])+((Cik[3][0][2]*udot[1])+(Cik[3][1][2]*
      K2[1][1])));
    udot[3] = ((2.5*eps[3])-K2[3][0]);
    A2[3][0] = (K2[3][0]+udot[3]);
    udot[4] = ((2.5*eps[4])-K2[3][1]);
    A2[4][1] = (K2[3][1]+udot[4]);
    udot[5] = ((2.5*eps[5])-K2[3][2]);
    A2[5][2] = (K2[3][2]+udot[5]);
    for (i = 0; i <= 5; i++) {
        oudotm[i] = udot[i];
    }
/*
 Used 0.02 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   75 adds/subtracts/negates
                     65 multiplies
                      0 divides
                     72 assignments
*/
}

void sdudotm(double imult[3],
    double oudotm[6])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(67,23);
        return;
    }
    sdxudotm(67,imult,oudotm);
}

void sdderiv(double oqdot[7],
    double oudot[6])
{
/*
This is the derivative section for a 3-body ground-based
system with 6 hinge degree(s) of freedom.
1 of the degrees of freedom may follow prescribed motion.
There are 3 constraints.
*/
    double workr[3],bb[3],b0[3],v0[3],p0[3];
    int iwork[3];
    int i,j;
    double udot0[6],udot1[6];
    double eps[6],Z1[6][3],Z2[6][3],A1[6][3],A2[6][3],K1[6][3],K2[6][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(17,23);
        return;
    }
    if (stabvelq == 1) {
        sdseterr(17,32);
    }
    if (stabposq == 1) {
        sdseterr(17,33);
    }
    wsiz = 3;
/*
Compute unconstrained equations
*/
    sdxudot0(17,udot0);
    sdrhs();
    sdaerr(b0);
    if (stabvel  !=  0.) {
        sdverr(v0);
    }
    if (stabpos  !=  0.) {
        sdperr(p0);
    }
/*
Stabilize constraints using Baumgarte's method
*/
    for (i = 0; i <= 2; i++) {
        bb[i] = -b0[i];
    }
    if (stabvel  !=  0.) {
        for (i = 0; i <= 2; i++) {
            bb[i] = bb[i]-stabvel*v0[i];
        }
    }
    if (stabpos  !=  0.) {
        for (i = 0; i <= 2; i++) {
            bb[i] = bb[i]-stabpos*p0[i];
        }
    }
/*
Compute and decompose constraint matrix WW
*/
    sddoww(17);
/*
Numerically solve for constraint multipliers
*/
    sdqrbslv(3,3,3,3,wmap,wmap,1e-13,workr,iwork,ww,qraux,jpvt,bb,mult,&wrank);
    for (i = 0; i <= 2; i++) {
        multmap[i] = 0;
    }
    for (i = 0; i < wrank; i++) {
        multmap[jpvt[i]] = 1;
    }
    j = 0;
    for (i = 0; i <= 2; i++) {
        if (multmap[i] != 0) {
            multmap[j] = wmap[i];
            j = j+1;
        }
    }
/*
Compute final udots
*/
    sdxudotm(17,mult,udot1);
    for (i = 0; i <= 5; i++) {
        udot[i] = udot0[i]+udot1[i];
    }
    sdrhs();
    for (i = 0; i <= 6; i++) {
        oqdot[i] = qdot[i];
    }
    for (i = 0; i <= 5; i++) {
        oudot[i] = udot[i];
    }
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   15 adds/subtracts/negates
                      6 multiplies
                      0 divides
                     28 assignments
*/
}
/*
Compute residuals for use with DAE integrator
*/

void sdresid(double eqdot[7],
    double eudot[6],
    double emults[3],
    double resid[16])
{
    int i;
    double uderrs[6],p0[3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(16,23);
        return;
    }
    if (stabposq == 1) {
        sdseterr(16,33);
    }
    sdfulltrq(eudot,emults,uderrs);
    for (i = 0; i < 7; i++) {
        resid[i] = eqdot[i]-qdot[i];
    }
    for (i = 0; i < 6; i++) {
        resid[7+i] = uderrs[i];
    }
    sdverr(&resid[13]);
    if (stabpos  !=  0.) {
        sdperr(p0);
        for (i = 0; i < 3; i++) {
            resid[13+i] = resid[13+i]+stabpos*p0[i];
        }
    }
    for (i = 0; i < 6; i++) {
        udot[i] = eudot[i];
    }
    for (i = 0; i < 3; i++) {
        mult[i] = emults[i];
    }
    sdrhs();
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   10 adds/subtracts/negates
                      3 multiplies
                      0 divides
                     25 assignments
*/
}

void sdmult(double omults[3],
    int *owrank,
    int omultmap[3])
{
    int i;

    if (roustate != 3) {
        sdseterr(34,24);
        return;
    }
    for (i = 0; i < 3; i++) {
        omults[i] = mult[i];
        if (i <= wrank-1) {
            omultmap[i] = multmap[i];
        } else {
            omultmap[i] = -1;
        }
    }
    *owrank = wrank;
}

void sdreac(double force[3][3],
    double torque[3][3])
{
/*
Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123) on machine ID unknown
Copyright (c) 1990-1997 Symbolic Dynamics, Inc.
Copyright (c) 1990-1997 Parametric Technology Corp.
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.
Government is subject to restrictions as set forth in subparagraph
(c)(1)(ii) of the Rights in Technical Data and Computer Software
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041
*/

    if (roustate != 3) {
        sdseterr(31,24);
        return;
    }
/*
Compute reaction forces for non-weld tree joints
*/
    ffkb[0][0] = (mfk[0][0]+ufk[0][0]);
    ffkb[0][1] = (mfk[0][1]+ufk[0][1]);
    ffkb[0][2] = (mfk[0][2]+ufk[0][2]);
    ffkb[1][0] = (mfk[1][0]+ufk[1][0]);
    ffkb[1][1] = (mfk[1][1]+ufk[1][1]);
    ffkb[1][2] = (mfk[1][2]+ufk[1][2]);
    ffkb[2][0] = (mfk[2][0]+ufk[2][0]);
    ffkb[2][1] = (mfk[2][1]+ufk[2][1]);
    ffkb[2][2] = (mfk[2][2]+ufk[2][2]);
    ttkb[0][0] = (mtk[0][0]+utk[0][0]);
    ttkb[0][1] = (mtk[0][1]+utk[0][1]);
    ttkb[0][2] = (mtk[0][2]+utk[0][2]);
    ttkb[1][0] = (mtk[1][0]+utk[1][0]);
    ttkb[1][1] = (mtk[1][1]+utk[1][1]);
    ttkb[1][2] = (mtk[1][2]+utk[1][2]);
    ttkb[2][0] = (mtk[2][0]+utk[2][0]);
    ttkb[2][1] = (mtk[2][1]+utk[2][1]);
    ttkb[2][2] = (mtk[2][2]+utk[2][2]);
    fc[5][0] = ((AnkAtk[5][0]-gk[3][0])-ffkb[2][0]);
    fc[5][1] = ((AnkAtk[5][1]-gk[3][1])-ffkb[2][1]);
    fc[5][2] = ((AnkAtk[5][2]-gk[3][2])-ffkb[2][2]);
    tc[5][0] = ((.4*onk[5][0])-ttkb[2][0]);
    tc[5][1] = ((.4*onk[5][1])-ttkb[2][1]);
    tc[5][2] = ((.4*onk[5][2])-ttkb[2][2]);
    fccikt[5][0] = fc[5][0];
    fccikt[5][1] = fc[5][1];
    fccikt[5][2] = fc[5][2];
    ffk[4][0] = -fccikt[5][0];
    ffk[4][1] = -fccikt[5][1];
    ffk[4][2] = -fccikt[5][2];
    ttk[4][0] = -tc[5][0];
    ttk[4][1] = -tc[5][1];
    ttk[4][2] = -tc[5][2];
    fc[4][0] = -ffk[4][0];
    fc[4][1] = -ffk[4][1];
    fc[4][2] = -ffk[4][2];
    tc[4][0] = -ttk[4][0];
    tc[4][1] = -ttk[4][1];
    tc[4][2] = -ttk[4][2];
    fccikt[4][0] = fc[4][0];
    fccikt[4][1] = fc[4][1];
    fccikt[4][2] = fc[4][2];
    ffk[3][0] = -fccikt[4][0];
    ffk[3][1] = -fccikt[4][1];
    ffk[3][2] = -fccikt[4][2];
    ttk[3][0] = -tc[4][0];
    ttk[3][1] = -tc[4][1];
    ttk[3][2] = -tc[4][2];
    fc[3][0] = -ffk[3][0];
    fc[3][1] = -ffk[3][1];
    fc[3][2] = -ffk[3][2];
    tc[3][0] = -ttk[3][0];
    tc[3][1] = -ttk[3][1];
    tc[3][2] = -ttk[3][2];
    fccikt[3][0] = ((Cik[3][0][2]*fc[3][2])+((Cik[3][0][0]*fc[3][0])+(
      Cik[3][0][1]*fc[3][1])));
    fccikt[3][1] = ((Cik[3][1][2]*fc[3][2])+((Cik[3][1][0]*fc[3][0])+(
      Cik[3][1][1]*fc[3][1])));
    fccikt[3][2] = ((Cik[3][2][2]*fc[3][2])+((Cik[3][2][0]*fc[3][0])+(
      Cik[3][2][1]*fc[3][1])));
    ffk[2][0] = (ffkb[1][0]-fccikt[3][0]);
    ffk[2][1] = (ffkb[1][1]-fccikt[3][1]);
    ffk[2][2] = (ffkb[1][2]-fccikt[3][2]);
    ttk[2][0] = (ttkb[1][0]-((Cik[3][0][2]*tc[3][2])+((Cik[3][0][0]*tc[3][0])+(
      Cik[3][0][1]*tc[3][1]))));
    ttk[2][1] = (ttkb[1][1]-((Cik[3][1][2]*tc[3][2])+((Cik[3][1][0]*tc[3][0])+(
      Cik[3][1][1]*tc[3][1]))));
    ttk[2][2] = (ttkb[1][2]-((Cik[3][2][2]*tc[3][2])+((Cik[3][2][0]*tc[3][0])+(
      Cik[3][2][1]*tc[3][1]))));
    fc[2][0] = -ffk[2][0];
    fc[2][1] = -ffk[2][1];
    fc[2][2] = -ffk[2][2];
    tc[2][0] = -ttk[2][0];
    tc[2][1] = -ttk[2][1];
    tc[2][2] = -ttk[2][2];
    fccikt[2][0] = fc[2][0];
    fccikt[2][1] = fc[2][1];
    fccikt[2][2] = fc[2][2];
    ffk[1][0] = (ffkb[0][0]-fccikt[2][0]);
    ffk[1][1] = (ffkb[0][1]-fccikt[2][1]);
    ffk[1][2] = (ffkb[0][2]-fccikt[2][2]);
    ttk[1][0] = (ttkb[0][0]-(tc[2][0]+(fccikt[2][2]*q[2])));
    ttk[1][1] = (ttkb[0][1]-tc[2][1]);
    ttk[1][2] = (ttkb[0][2]-(tc[2][2]-(fccikt[2][0]*q[2])));
    fc[1][0] = -ffk[1][0];
    fc[1][1] = -ffk[1][1];
    fc[1][2] = -ffk[1][2];
    tc[1][0] = -ttk[1][0];
    tc[1][1] = -ttk[1][1];
    tc[1][2] = -ttk[1][2];
    fccikt[1][0] = fc[1][0];
    fccikt[1][1] = ((fc[1][1]*c1)-(fc[1][2]*s1));
    fccikt[1][2] = ((fc[1][1]*s1)+(fc[1][2]*c1));
    ffk[0][0] = -fccikt[1][0];
    ffk[0][1] = -fccikt[1][1];
    ffk[0][2] = -fccikt[1][2];
    ttk[0][0] = -tc[1][0];
    ttk[0][1] = -((tc[1][1]*c1)-(tc[1][2]*s1));
    ttk[0][2] = -((tc[1][1]*s1)+(tc[1][2]*c1));
    fc[0][0] = -ffk[0][0];
    fc[0][1] = -ffk[0][1];
    fc[0][2] = -ffk[0][2];
    tc[0][0] = -ttk[0][0];
    tc[0][1] = -ttk[0][1];
    tc[0][2] = -ttk[0][2];
    force[0][0] = fc[1][0];
    torque[0][0] = tc[1][0];
    force[0][1] = fc[1][1];
    torque[0][1] = tc[1][1];
    force[0][2] = fc[1][2];
    torque[0][2] = tc[1][2];
    force[1][0] = fc[2][0];
    torque[1][0] = tc[2][0];
    force[1][1] = fc[2][1];
    torque[1][1] = tc[2][1];
    force[1][2] = fc[2][2];
    torque[1][2] = tc[2][2];
    force[2][0] = fc[5][0];
    torque[2][0] = tc[5][0];
    force[2][1] = fc[5][1];
    torque[2][1] = tc[5][1];
    force[2][2] = fc[5][2];
    torque[2][2] = tc[5][2];
/*
Compute reaction forces for tree weld joints
*/
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain  105 adds/subtracts/negates
                     31 multiplies
                      0 divides
                    117 assignments
*/
}

void sdmom(double lm[3],
    double am[3],
    double *ke)
{
/*
Compute system linear and angular momentum, and kinetic energy.

Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123) on machine ID unknown
Copyright (c) 1990-1997 Symbolic Dynamics, Inc.
Copyright (c) 1990-1997 Parametric Technology Corp.
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.
Government is subject to restrictions as set forth in subparagraph
(c)(1)(ii) of the Rights in Technical Data and Computer Software
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041
*/
    double lk[3][3],hnk[3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(19,23);
        return;
    }
    lm[0] = vnk[2][0];
    lm[1] = vnk[2][1];
    lm[2] = vnk[2][2];
    am[0] = (((.4*((cnk[3][0][2]*wk[5][2])+((cnk[3][0][0]*wk[3][0])+(
      cnk[3][0][1]*wk[4][1]))))+((rnk[2][1]*vnk[2][2])-(rnk[2][2]*vnk[2][1])))-(
      (rnk[2][1]*vnk[2][2])-(rnk[2][2]*vnk[2][1])));
    am[1] = (((.4*((cnk[3][1][2]*wk[5][2])+((cnk[3][1][0]*wk[3][0])+(
      cnk[3][1][1]*wk[4][1]))))+((rnk[2][2]*vnk[2][0])-(rnk[2][0]*vnk[2][2])))-(
      (rnk[2][2]*vnk[2][0])-(rnk[2][0]*vnk[2][2])));
    am[2] = (((.4*((cnk[3][2][2]*wk[5][2])+((cnk[3][2][0]*wk[3][0])+(
      cnk[3][2][1]*wk[4][1]))))+((rnk[2][0]*vnk[2][1])-(rnk[2][1]*vnk[2][0])))-(
      (rnk[2][0]*vnk[2][1])-(rnk[2][1]*vnk[2][0])));
    *ke = (.5*((.4*((wk[5][2]*wk[5][2])+((wk[3][0]*wk[3][0])+(wk[4][1]*wk[4][1])
      )))+((vnk[2][2]*vnk[2][2])+((vnk[2][0]*vnk[2][0])+(vnk[2][1]*vnk[2][1]))))
      );
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   23 adds/subtracts/negates
                     32 multiplies
                      0 divides
                      7 assignments
*/
}

void sdsys(double *mtoto,
    double cm[3],
    double icm[3][3])
{
/*
Compute system total mass, and instantaneous center of mass and
inertia matrix.

Generated 07-Aug-2007 13:10:45 by SD/FAST, Order(N) formulation
(sdfast B.2.8 #30123) on machine ID unknown
Copyright (c) 1990-1997 Symbolic Dynamics, Inc.
Copyright (c) 1990-1997 Parametric Technology Corp.
RESTRICTED RIGHTS LEGEND: Use, duplication, or disclosure by the U.S.
Government is subject to restrictions as set forth in subparagraph
(c)(1)(ii) of the Rights in Technical Data and Computer Software
clause at DFARS 52.227-7013 and similar clauses in the FAR and NASA
FAR Supplement.  Symbolic Dynamics, Inc., Mountain View, CA 94041
*/
    double ikcnkt[6][3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(20,23);
        return;
    }
    *mtoto = 1.;
    cm[0] = rnk[2][0];
    cm[1] = rnk[2][1];
    cm[2] = rnk[2][2];
    icm[0][0] = (((.4*((cnk[3][0][2]*cnk[3][0][2])+((cnk[3][0][0]*cnk[3][0][0])+
      (cnk[3][0][1]*cnk[3][0][1]))))+((rnk[2][1]*rnk[2][1])+(rnk[2][2]*rnk[2][2]
      )))-((rnk[2][1]*rnk[2][1])+(rnk[2][2]*rnk[2][2])));
    icm[0][1] = ((rnk[2][0]*rnk[2][1])+((.4*((cnk[3][0][2]*cnk[3][1][2])+((
      cnk[3][0][0]*cnk[3][1][0])+(cnk[3][0][1]*cnk[3][1][1]))))-(rnk[2][0]*
      rnk[2][1])));
    icm[0][2] = ((rnk[2][0]*rnk[2][2])+((.4*((cnk[3][0][2]*cnk[3][2][2])+((
      cnk[3][0][0]*cnk[3][2][0])+(cnk[3][0][1]*cnk[3][2][1]))))-(rnk[2][0]*
      rnk[2][2])));
    icm[1][0] = icm[0][1];
    icm[1][1] = (((.4*((cnk[3][1][2]*cnk[3][1][2])+((cnk[3][1][0]*cnk[3][1][0])+
      (cnk[3][1][1]*cnk[3][1][1]))))+((rnk[2][0]*rnk[2][0])+(rnk[2][2]*rnk[2][2]
      )))-((rnk[2][0]*rnk[2][0])+(rnk[2][2]*rnk[2][2])));
    icm[1][2] = ((rnk[2][1]*rnk[2][2])+((.4*((cnk[3][1][2]*cnk[3][2][2])+((
      cnk[3][1][0]*cnk[3][2][0])+(cnk[3][1][1]*cnk[3][2][1]))))-(rnk[2][1]*
      rnk[2][2])));
    icm[2][0] = icm[0][2];
    icm[2][1] = icm[1][2];
    icm[2][2] = (((.4*((cnk[3][2][2]*cnk[3][2][2])+((cnk[3][2][0]*cnk[3][2][0])+
      (cnk[3][2][1]*cnk[3][2][1]))))+((rnk[2][0]*rnk[2][0])+(rnk[2][1]*rnk[2][1]
      )))-((rnk[2][0]*rnk[2][0])+(rnk[2][1]*rnk[2][1])));
/*
 Used 0.00 seconds CPU time,
 0 additional bytes of memory.
 Equations contain   30 adds/subtracts/negates
                     42 multiplies
                      0 divides
                     13 assignments
*/
}

void sdpos(int body,
    double pt[3],
    double loc[3])
{
/*
Return inertial frame location of a point on a body.

*/
    double pv[3];

    if (sdchkbnum(21,body) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(21,23);
        return;
    }
    if (body  ==  -1) {
        loc[0] = pt[0];
        loc[1] = pt[1];
        loc[2] = pt[2];
    } else {
        pv[0] = rnb[body][0]+pt[0]*cnb[body][0][0]+pt[1]*cnb[body][0][1]+pt[2]*
          cnb[body][0][2];
        pv[1] = rnb[body][1]+pt[0]*cnb[body][1][0]+pt[1]*cnb[body][1][1]+pt[2]*
          cnb[body][1][2];
        pv[2] = rnb[body][2]+pt[0]*cnb[body][2][0]+pt[1]*cnb[body][2][1]+pt[2]*
          cnb[body][2][2];
        loc[0] = pv[0];
        loc[1] = pv[1];
        loc[2] = pv[2];
    }
}

void sdvel(int body,
    double pt[3],
    double velo[3])
{
/*
Return inertial frame velocity of a point on a body.

*/
    double pv[3];

    if (sdchkbnum(22,body) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(22,23);
        return;
    }
    if (body  ==  -1) {
        velo[0] = 0.;
        velo[1] = 0.;
        velo[2] = 0.;
    } else {
        pv[0] = wb[body][1]*pt[2]-wb[body][2]*pt[1];
        pv[1] = wb[body][2]*pt[0]-wb[body][0]*pt[2];
        pv[2] = wb[body][0]*pt[1]-wb[body][1]*pt[0];
        velo[0] = vnb[body][0]+pv[0]*cnb[body][0][0]+pv[1]*cnb[body][0][1]+pv[2]
          *cnb[body][0][2];
        velo[1] = vnb[body][1]+pv[0]*cnb[body][1][0]+pv[1]*cnb[body][1][1]+pv[2]
          *cnb[body][1][2];
        velo[2] = vnb[body][2]+pv[0]*cnb[body][2][0]+pv[1]*cnb[body][2][1]+pv[2]
          *cnb[body][2][2];
    }
}

void sdorient(int body,
    double dircos[3][3])
{
/*
Return orientation of body w.r.t. ground frame.

*/

    if (sdchkbnum(23,body) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(23,23);
        return;
    }
    if (body == -1) {
        dircos[0][0] = 1.;
        dircos[0][1] = 0.;
        dircos[0][2] = 0.;
        dircos[1][0] = 0.;
        dircos[1][1] = 1.;
        dircos[1][2] = 0.;
        dircos[2][0] = 0.;
        dircos[2][1] = 0.;
        dircos[2][2] = 1.;
    } else {
        dircos[0][0] = cnb[body][0][0];
        dircos[0][1] = cnb[body][0][1];
        dircos[0][2] = cnb[body][0][2];
        dircos[1][0] = cnb[body][1][0];
        dircos[1][1] = cnb[body][1][1];
        dircos[1][2] = cnb[body][1][2];
        dircos[2][0] = cnb[body][2][0];
        dircos[2][1] = cnb[body][2][1];
        dircos[2][2] = cnb[body][2][2];
    }
}

void sdangvel(int body,
    double avel[3])
{
/*
Return angular velocity of the body.

*/

    if (sdchkbnum(24,body) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(24,23);
        return;
    }
    if (body == -1) {
        avel[0] = 0.;
        avel[1] = 0.;
        avel[2] = 0.;
    } else {
        avel[0] = wb[body][0];
        avel[1] = wb[body][1];
        avel[2] = wb[body][2];
    }
}

void sdtrans(int frbod,
    double ivec[3],
    int tobod,
    double ovec[3])
{
/*
Transform ivec from frbod frame to tobod frame.

*/
    double pv[3];

    if (sdchkbnum(25,frbod) != 0) {
        return;
    }
    if (sdchkbnum(25,tobod) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(25,23);
        return;
    }
    if (frbod == tobod) {
        sdvcopy(ivec,ovec);
        return;
    }
    if (frbod == -1) {
        sdvcopy(ivec,pv);
        ovec[0] = pv[0]*cnb[tobod][0][0]+pv[1]*cnb[tobod][1][0]+pv[2]*cnb[tobod
          ][2][0];
        ovec[1] = pv[0]*cnb[tobod][0][1]+pv[1]*cnb[tobod][1][1]+pv[2]*cnb[tobod
          ][2][1];
        ovec[2] = pv[0]*cnb[tobod][0][2]+pv[1]*cnb[tobod][1][2]+pv[2]*cnb[tobod
          ][2][2];
        return;
    }
    if (tobod == -1) {
        sdvcopy(ivec,pv);
        ovec[0] = pv[0]*cnb[frbod][0][0]+pv[1]*cnb[frbod][0][1]+pv[2]*cnb[frbod
          ][0][2];
        ovec[1] = pv[0]*cnb[frbod][1][0]+pv[1]*cnb[frbod][1][1]+pv[2]*cnb[frbod
          ][1][2];
        ovec[2] = pv[0]*cnb[frbod][2][0]+pv[1]*cnb[frbod][2][1]+pv[2]*cnb[frbod
          ][2][2];
        return;
    }
    pv[0] = ivec[0]*cnb[frbod][0][0]+ivec[1]*cnb[frbod][0][1]+ivec[2]*cnb[frbod
      ][0][2];
    pv[1] = ivec[0]*cnb[frbod][1][0]+ivec[1]*cnb[frbod][1][1]+ivec[2]*cnb[frbod
      ][1][2];
    pv[2] = ivec[0]*cnb[frbod][2][0]+ivec[1]*cnb[frbod][2][1]+ivec[2]*cnb[frbod
      ][2][2];
    ovec[0] = pv[0]*cnb[tobod][0][0]+pv[1]*cnb[tobod][1][0]+pv[2]*cnb[tobod][2][
      0];
    ovec[1] = pv[0]*cnb[tobod][0][1]+pv[1]*cnb[tobod][1][1]+pv[2]*cnb[tobod][2][
      1];
    ovec[2] = pv[0]*cnb[tobod][0][2]+pv[1]*cnb[tobod][1][2]+pv[2]*cnb[tobod][2][
      2];
}

void sdrel2cart(int coord,
    int body,
    double point[3],
    double linchg[3],
    double rotchg[3])
{
/* Return derivative of pt loc and body orient w.r.t. hinge rate
*/
    int x,i,gnd;
    double lin[3],pv[3];
    double pink[3],ptvec[3];

    if ((coord < 0) || (coord > 5)) {
        sdseterr(59,45);
        return;
    }
    if (sdchkbnum(59,body) != 0) {
        return;
    }
    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(59,23);
        return;
    }
    gnd = -1;
    sdvset(0.,0.,0.,linchg);
    sdvset(0.,0.,0.,rotchg);
    i = body;
    for (;;) {
        if (i == gnd) {
            return;
        }
        x = firstq[i];
        if (x <= coord) {
            if (coord >= x+njntdof[i]) {
                return;
            }
            break;
        }
        i = inb[i];
    }
    sddoping();
    for (i = 0; i < 3; i++) {
        pink[i] = ping[coord][i];
        lin[i] = hngpt[coord][i];
    }
    sdtrans(gnd,pink,body,pink);
    if (trans[coord] != 0) {
        sdvcopy(pink,linchg);
    } else {
        sdvcopy(pink,rotchg);
        sdpos(body,point,ptvec);
        sdvsub(ptvec,lin,ptvec);
        sdtrans(gnd,ptvec,body,ptvec);
        sdvcross(pink,ptvec,linchg);
    }
}

void sdacc(int body,
    double pt[3],
    double accel[3])
{
/*
Return linear acceleration a point of the specified body.

*/
    double pv[3];

    if (sdchkbnum(32,body) != 0) {
        return;
    }
    if (roustate != 3) {
        sdseterr(32,24);
        return;
    }
    if (body  ==  -1) {
        accel[0] = 0.;
        accel[1] = 0.;
        accel[2] = 0.;
    } else {
        pv[0] = pt[0]*dyad[body][0][0]+pt[1]*dyad[body][0][1]+pt[2]*dyad[body][0
          ][2];
        pv[1] = pt[0]*dyad[body][1][0]+pt[1]*dyad[body][1][1]+pt[2]*dyad[body][1
          ][2];
        pv[2] = pt[0]*dyad[body][2][0]+pt[1]*dyad[body][2][1]+pt[2]*dyad[body][2
          ][2];
        accel[0] = anb[body][0]+pv[0]*cnb[body][0][0]+pv[1]*cnb[body][0][1]+pv[2
          ]*cnb[body][0][2];
        accel[1] = anb[body][1]+pv[0]*cnb[body][1][0]+pv[1]*cnb[body][1][1]+pv[2
          ]*cnb[body][1][2];
        accel[2] = anb[body][2]+pv[0]*cnb[body][2][0]+pv[1]*cnb[body][2][1]+pv[2
          ]*cnb[body][2][2];
    }
}

void sdangacc(int body,
    double aacc[3])
{
/*
Return angular acceleration of the body.

*/

    if (sdchkbnum(33,body) != 0) {
        return;
    }
    if (roustate != 3) {
        sdseterr(33,24);
        return;
    }
    if (body == -1) {
        aacc[0] = 0.;
        aacc[1] = 0.;
        aacc[2] = 0.;
    } else {
        aacc[0] = onb[body][0];
        aacc[1] = onb[body][1];
        aacc[2] = onb[body][2];
    }
}

void sdgrav(double gravin[3])
{

    sdseterr(1,19);
    roustate = 0;
}

void sdmass(int body,
    double massin)
{

    if (sdchkbnum(2,body) != 0) {
        return;
    }
    if (body == -1) {
        sdseterr(2,15);
        return;
    }
    if (mkq[body] != 0) {
        mk[body] = massin;
        mkq[body] = 3;
    } else {
        sdseterr(2,19);
    }
    roustate = 0;
}

void sdiner(int body,
    double inerin[3][3])
{
    int anyques;

    if (sdchkbnum(3,body) != 0) {
        return;
    }
    if (body == -1) {
        sdseterr(3,15);
        return;
    }
    anyques = 0;
    if (ikq[body][0][0]  !=  0) {
        ik[body][0][0] = inerin[0][0];
        ikq[body][0][0] = 3;
        anyques = 1;
    }
    if (ikq[body][0][1]  !=  0) {
        ik[body][0][1] = inerin[0][1];
        ikq[body][0][1] = 3;
        ik[body][1][0] = inerin[0][1];
        ikq[body][1][0] = 3;
        anyques = 1;
    }
    if (ikq[body][0][2]  !=  0) {
        ik[body][0][2] = inerin[0][2];
        ikq[body][0][2] = 3;
        ik[body][2][0] = inerin[0][2];
        ikq[body][2][0] = 3;
        anyques = 1;
    }
    if (ikq[body][1][1]  !=  0) {
        ik[body][1][1] = inerin[1][1];
        ikq[body][1][1] = 3;
        anyques = 1;
    }
    if (ikq[body][1][2]  !=  0) {
        ik[body][1][2] = inerin[1][2];
        ikq[body][1][2] = 3;
        ik[body][2][1] = inerin[1][2];
        ikq[body][2][1] = 3;
        anyques = 1;
    }
    if (ikq[body][2][2]  !=  0) {
        ik[body][2][2] = inerin[2][2];
        ikq[body][2][2] = 3;
        anyques = 1;
    }
    if (anyques == 0) {
        sdseterr(3,19);
    }
    roustate = 0;
}

void sdbtj(int joint,
    double btjin[3])
{
    int anyques;

    if (sdchkjnum(4,joint) != 0) {
        return;
    }
    anyques = 0;
    if (rkq[joint][0]  !=  0) {
        rk[joint][0] = btjin[0];
        rkq[joint][0] = 3;
        anyques = 1;
    }
    if (rkq[joint][1]  !=  0) {
        rk[joint][1] = btjin[1];
        rkq[joint][1] = 3;
        anyques = 1;
    }
    if (rkq[joint][2]  !=  0) {
        rk[joint][2] = btjin[2];
        rkq[joint][2] = 3;
        anyques = 1;
    }
    if (anyques == 0) {
        sdseterr(4,19);
    }
    roustate = 0;
}

void sditj(int joint,
    double itjin[3])
{
    int anyques;

    if (sdchkjnum(5,joint) != 0) {
        return;
    }
    anyques = 0;
    if (riq[joint][0]  !=  0) {
        ri[joint][0] = itjin[0];
        riq[joint][0] = 3;
        anyques = 1;
    }
    if (riq[joint][1]  !=  0) {
        ri[joint][1] = itjin[1];
        riq[joint][1] = 3;
        anyques = 1;
    }
    if (riq[joint][2]  !=  0) {
        ri[joint][2] = itjin[2];
        riq[joint][2] = 3;
        anyques = 1;
    }
    if (anyques == 0) {
        sdseterr(5,19);
    }
    roustate = 0;
}

void sdpin(int joint,
    int pinno,
    double pinin[3])
{
    int anyques,offs;

    if (sdchkjpin(6,joint,pinno) != 0) {
        return;
    }
    anyques = 0;
    offs = firstq[joint]+pinno;
    if (jtype[joint] == 21) {
        offs = offs+3;
    }
    if (jtype[joint] == 11) {
        offs = offs+1;
    }
    if (pinq[offs][0]  !=  0) {
        pin[offs][0] = pinin[0];
        pinq[offs][0] = 3;
        anyques = 1;
    }
    if (pinq[offs][1]  !=  0) {
        pin[offs][1] = pinin[1];
        pinq[offs][1] = 3;
        anyques = 1;
    }
    if (pinq[offs][2]  !=  0) {
        pin[offs][2] = pinin[2];
        pinq[offs][2] = 3;
        anyques = 1;
    }
    if (anyques == 0) {
        sdseterr(6,19);
    }
    roustate = 0;
}

void sdpres(int joint,
    int axis,
    int presin)
{
    int anyques;

    if (sdchkjaxis(37,joint,axis) != 0) {
        return;
    }
    if ((presin != 0) && (presin != 1)) {
        sdseterr(37,20);
    }
    anyques = 0;
    if (presq[sdindx(joint,axis)]  !=  0) {
        if (presin  !=  0) {
            pres[sdindx(joint,axis)] = 1.;
        } else {
            pres[sdindx(joint,axis)] = 0.;
        }
        presq[sdindx(joint,axis)] = 3;
        anyques = 1;
    }
    if (anyques == 0) {
        sdseterr(37,19);
    }
    wwflg = 0;
}

void sdconschg(void)
{

    wwflg = 0;
}

void sdstab(double velin,
    double posin)
{

    stabvel = velin;
    stabvelq = 3;
    stabpos = posin;
    stabposq = 3;
}

void sdgetgrav(double gravout[3])
{

    gravout[0] = grav[0];
    gravout[1] = grav[1];
    gravout[2] = grav[2];
}

void sdgetmass(int body,
    double *massout)
{

    if (sdchkbnum(40,body) != 0) {
        return;
    }
    if (body == -1) {
        sdseterr(40,15);
        return;
    }
    *massout = mk[body];
}

void sdgetiner(int body,
    double inerout[3][3])
{

    if (sdchkbnum(41,body) != 0) {
        return;
    }
    if (body == -1) {
        sdseterr(41,15);
        return;
    }
    inerout[0][0] = ik[body][0][0];
    inerout[0][1] = ik[body][0][1];
    inerout[0][2] = ik[body][0][2];
    inerout[1][0] = ik[body][1][0];
    inerout[1][1] = ik[body][1][1];
    inerout[1][2] = ik[body][1][2];
    inerout[2][0] = ik[body][2][0];
    inerout[2][1] = ik[body][2][1];
    inerout[2][2] = ik[body][2][2];
}

void sdgetbtj(int joint,
    double btjout[3])
{

    if (sdchkjnum(42,joint) != 0) {
        return;
    }
    btjout[0] = rk[joint][0];
    btjout[1] = rk[joint][1];
    btjout[2] = rk[joint][2];
}

void sdgetitj(int joint,
    double itjout[3])
{

    if (sdchkjnum(43,joint) != 0) {
        return;
    }
    itjout[0] = ri[joint][0];
    itjout[1] = ri[joint][1];
    itjout[2] = ri[joint][2];
}

void sdgetpin(int joint,
    int pinno,
    double pinout[3])
{
    int offs;

    if (sdchkjpin(44,joint,pinno) != 0) {
        return;
    }
    offs = firstq[joint]+pinno;
    if (jtype[joint] == 21) {
        offs = offs+3;
    }
    if (jtype[joint] == 11) {
        offs = offs+1;
    }
    pinout[0] = pin[offs][0];
    pinout[1] = pin[offs][1];
    pinout[2] = pin[offs][2];
}

void sdgetpres(int joint,
    int axis,
    int *presout)
{

    if (sdchkjaxis(45,joint,axis) != 0) {
        return;
    }
    if (pres[sdindx(joint,axis)]  !=  0.) {
        *presout = 1;
    } else {
        *presout = 0;
    }
}

void sdgetstab(double *velout,
    double *posout)
{

    *velout = stabvel;
    *posout = stabpos;
}

void sdinfo(int info[50])
{

    info[0] = ground;
    info[1] = nbod;
    info[2] = ndof;
    info[3] = ncons;
    info[4] = nloop;
    info[5] = nldof;
    info[6] = nloopc;
    info[7] = nball;
    info[8] = nlball;
    info[9] = npres;
    info[10] = nuser;
    info[11] = 3;
/* info entries from 12-49 are reserved */
}

void sdjnt(int joint,
    int info[50],
    int tran[6])
{
    int i,offs;

    if (sdchkjnum(48,joint) != 0) {
        return;
    }
    info[0] = jtype[joint];
    info[1] = 0;
    offs = 0;
    info[2] = inb[joint];
    info[3] = outb[joint];
    info[4] = njntdof[joint];
    info[5] = njntc[joint];
    info[6] = njntp[joint];
    info[7] = firstq[joint];
    info[8] = ballq[joint];
    info[9] = firstm[joint];
    info[10] = firstp[joint];
/* info entries from 11-49 are reserved */

    for (i = 0; i <= 5; i++) {
        if (i  <  njntdof[joint]) {
            tran[i] = trans[offs+firstq[joint]+i];
        } else {
            tran[i] = -1;
        }
    }
}

void sdcons(int consno,
    int info[50])
{

    if (sdchkucnum(49,consno) != 0) {
        return;
    }
    info[0] = 1;
    info[1] = firstu[consno];
/* info entries from 2-49 are reserved */
}

void sdgentime(int *gentm)
{

    *gentm = 131045;
}
/*
Done. CPU seconds used: 0.20  Memory used: 1687552 bytes.
Equation complexity:
  sdstate:    85 adds   135 multiplies     1 divides   181 assignments
  sdderiv:   669 adds   663 multiplies    28 divides   986 assignments
  sdresid:   181 adds   111 multiplies     0 divides   205 assignments
  sdreac:    105 adds    31 multiplies     0 divides   117 assignments
  sdmom:      23 adds    32 multiplies     0 divides     7 assignments
  sdsys:      30 adds    42 multiplies     0 divides    13 assignments
*/
