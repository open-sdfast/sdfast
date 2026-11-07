/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)


ROADMAP (slew.sd)

Bodies        Inb
No  Name      body Joint type  Coords q         Multipliers
--- --------- ---- ----------- ---------------- -----------------------
 -1 $ground                                    |
  0 bus        -1  Sixdof        0   1   2     |
                   ...           3   4   5  11 |
  1 clock       0  Pin           6             |
  2 camera      1  Pin           7             |
  3 boom        0  U-joint       8   9         |
  4 scanner     2  Pin          10p            |  0p

*/
#include <math.h>
#include <stdio.h>

typedef struct {
    int ground_,nbod_,ndof_,ncons_,nloop_,nldof_,nloopc_,nball_,nlball_,npres_,
      nuser_;
    int jtype_[5],inb_[5],outb_[5],njntdof_[5],njntc_[5],njntp_[5],firstq_[5],
      ballq_[5],firstm_[5],firstp_[5];
    int trans_[11];
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

typedef struct {
    double grav_[3],mk_[5],ik_[5][3][3],pin_[11][3];
    double rk_[5][3],ri_[5][3],pres_[11],stabvel_,stabpos_;
    int mfrcflg_,roustate_,vpkflg_,inerflg_,mmflg_,mmlduflg_,wwflg_,ltauflg_,
      fs0flg_,ii_,mmap_[11];
    int gravq_[3],mkq_[5],ikq_[5][3][3],pinq_[11][3],rkq_[5][3],riq_[5][3],
      presq_[11],stabvelq_,stabposq_;
    double mtot_,psmkg_,rhead_[5][3],rcom_[5][3],mkrcomt_[5][3][3],psikg_[3][3],
      psrcomg_[3],psrkg_[3],psrig_[3],psmk_[11],psik_[11][3][3],psrcom_[11][3],
      psrk_[11][3],psri_[11][3];
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
    double curtim_,q_[12],qn_[12],u_[11],cnk_[11][3][3],cnb_[5][3][3];
    double rnk_[11][3],vnk_[11][3],wk_[11][3],rnb_[5][3],vnb_[5][3],wb_[5][3],
      wbrcom_[5][3],com_[3],rnkg_[3];
    double Cik_[11][3][3],rikt_[11][3][3],Iko_[11][3][3],mkrk_[11][3][3],Cib_[5
      ][3][3];
    double Wkk_[11][3],Vkk_[11][3],dik_[11][3],rpp_[11][3],rpk_[11][3],rik_[11][
      3],rik2_[11][3];
    double rpri_[11][3],Wik_[11][3],Vik_[11][3],Wirk_[11][3],rkWkk_[11][3],
      Wkrpk_[11][3],VikWkr_[11][3];
    double perr_[1],verr_[1],aerr_[1],mult_[1],ufk_[5][3],utk_[5][3],mfk_[5][3],
      mtk_[5][3];
    double utau_[11],mtau_[11],uacc_[11],uvel_[11],upos_[12];
    double s6_,c6_,s7_,c7_,s8_,c8_,s9_,c9_,s10_,c10_;
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
#define s6 (sdgstate.s6_)
#define c6 (sdgstate.c6_)
#define s7 (sdgstate.s7_)
#define c7 (sdgstate.c7_)
#define s8 (sdgstate.s8_)
#define c8 (sdgstate.c8_)
#define s9 (sdgstate.s9_)
#define c9 (sdgstate.c9_)
#define s10 (sdgstate.s10_)
#define c10 (sdgstate.c10_)

typedef struct {
    double fs0_[11],qdot_[12],Otk_[11][3],Atk_[11][3],AiOiWi_[11][3],Fstar_[11][
      3];
    double Tstar_[11][3],Fstark_[11][3],Tstark_[11][3],IkWk_[11][3],WkIkWk_[11][
      3],gk_[11][3],IkbWk_[5][3],WkIkbWk_[5][3];
    double w0w0_[5],w1w1_[5],w2w2_[5],w0w1_[5],w0w2_[5],w1w2_[5];
    double w00w11_[5],w00w22_[5],w11w22_[5],ww_[1][1],qraux_[1];
    double mm_[11][11],mlo_[11][11],mdi_[11],IkWpk_[11][11][3],works_[11],
      workss_[11][11];
    double Wpk_[11][11][3],Vpk_[11][11][3],VWri_[11][11][3];
    int wmap_[1],multmap_[1],jpvt_[1],wsiz_,wrank_;
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
#define mm (sdglhs.mm_)
#define mlo (sdglhs.mlo_)
#define mdi (sdglhs.mdi_)
#define IkWpk (sdglhs.IkWpk_)
#define works (sdglhs.works_)
#define workss (sdglhs.workss_)
#define Wpk (sdglhs.Wpk_)
#define Vpk (sdglhs.Vpk_)
#define VWri (sdglhs.VWri_)
#define wmap (sdglhs.wmap_)
#define multmap (sdglhs.multmap_)
#define jpvt (sdglhs.jpvt_)
#define wsiz (sdglhs.wsiz_)
#define wrank (sdglhs.wrank_)

typedef struct {
    double fs_[11],udot_[11],tauc_[11],dyad_[5][3][3],fc_[11][3],tc_[11][3];
    double ank_[11][3],onk_[11][3],Onkb_[11][3],AOnkri_[11][3],Ankb_[11][3],
      AnkAtk_[11][3],anb_[5][3],onb_[5][3],dyrcom_[5][3];
    double ffk_[11][3],ttk_[11][3],fccikt_[11][3],ffkb_[5][3],ttkb_[5][3];
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
    /* ground */ 0,
    /* nbod */ 5,
    /* ndof */ 11,
    /* ncons */ 1,
    /* nloop */ 0,
    /* nldof */ 0,
    /* nloopc */ 0,
    /* nball */ 1,
    /* nlball */ 0,
    /* npres */ 1,
    /* nuser */ 0,
    /* jtype[0] */ 6,
    /* jtype[1] */ 1,
    /* jtype[2] */ 1,
    /* jtype[3] */ 2,
    /* jtype[4] */ 1,
    /* inb[0] */ -1,
    /* inb[1] */ 0,
    /* inb[2] */ 1,
    /* inb[3] */ 0,
    /* inb[4] */ 2,
    /* outb[0] */ 0,
    /* outb[1] */ 1,
    /* outb[2] */ 2,
    /* outb[3] */ 3,
    /* outb[4] */ 4,
    /* njntdof[0] */ 6,
    /* njntdof[1] */ 1,
    /* njntdof[2] */ 1,
    /* njntdof[3] */ 2,
    /* njntdof[4] */ 1,
    /* njntc[0] */ 0,
    /* njntc[1] */ 0,
    /* njntc[2] */ 0,
    /* njntc[3] */ 0,
    /* njntc[4] */ 0,
    /* njntp[0] */ 0,
    /* njntp[1] */ 0,
    /* njntp[2] */ 0,
    /* njntp[3] */ 0,
    /* njntp[4] */ 1,
    /* firstq[0] */ 0,
    /* firstq[1] */ 6,
    /* firstq[2] */ 7,
    /* firstq[3] */ 8,
    /* firstq[4] */ 10,
    /* ballq[0] */ 11,
    /* ballq[1] */ -104,
    /* ballq[2] */ -104,
    /* ballq[3] */ -104,
    /* ballq[4] */ -104,
    /* firstm[0] */ -1,
    /* firstm[1] */ -1,
    /* firstm[2] */ -1,
    /* firstm[3] */ -1,
    /* firstm[4] */ -1,
    /* firstp[0] */ -1,
    /* firstp[1] */ -1,
    /* firstp[2] */ -1,
    /* firstp[3] */ -1,
    /* firstp[4] */ 0,
    /* trans[0] */ 1,
    /* trans[1] */ 1,
    /* trans[2] */ 1,
    /* trans[3] */ 0,
    /* trans[4] */ 0,
    /* trans[5] */ 0,
    /* trans[6] */ 0,
    /* trans[7] */ 0,
    /* trans[8] */ 0,
    /* trans[9] */ 0,
    /* trans[10] */ 0,
};
sdginput_t sdginput = {
/* Model parameters from the input file */

/* gravity */
    /* grav[0] */ 0.,
    /* grav[1] */ 0.,
    /* grav[2] */ 0.,

/* mass */
    /* mk[0] */ 410.,
    /* mk[1] */ 6.8,
    /* mk[2] */ 57.5,
    /* mk[3] */ 10.7,
    /* mk[4] */ 1.,

/* inertia */
    /* ik[0][0][0] */ 115.,
    /* ik[0][0][1] */ -14.,
    /* ik[0][0][2] */ 14.,
    /* ik[0][1][0] */ -14.,
    /* ik[0][1][1] */ 316.,
    /* ik[0][1][2] */ -34.6,
    /* ik[0][2][0] */ 14.,
    /* ik[0][2][1] */ -34.6,
    /* ik[0][2][2] */ 440.,
    /* ik[1][0][0] */ .35,
    /* ik[1][0][1] */ 0.,
    /* ik[1][0][2] */ 0.,
    /* ik[1][1][0] */ 0.,
    /* ik[1][1][1] */ .35,
    /* ik[1][1][2] */ 0.,
    /* ik[1][2][0] */ 0.,
    /* ik[1][2][1] */ 0.,
    /* ik[1][2][2] */ 0.,
    /* ik[2][0][0] */ 4.85,
    /* ik[2][0][1] */ .41,
    /* ik[2][0][2] */ -.07,
    /* ik[2][1][0] */ .41,
    /* ik[2][1][1] */ 2.2,
    /* ik[2][1][2] */ .54,
    /* ik[2][2][0] */ -.07,
    /* ik[2][2][1] */ .54,
    /* ik[2][2][2] */ 5.5,
    /* ik[3][0][0] */ 27.2,
    /* ik[3][0][1] */ 0.,
    /* ik[3][0][2] */ 0.,
    /* ik[3][1][0] */ 0.,
    /* ik[3][1][1] */ .2,
    /* ik[3][1][2] */ 0.,
    /* ik[3][2][0] */ 0.,
    /* ik[3][2][1] */ 0.,
    /* ik[3][2][2] */ 27.2,
    /* ik[4][0][0] */ .1,
    /* ik[4][0][1] */ 0.,
    /* ik[4][0][2] */ 0.,
    /* ik[4][1][0] */ 0.,
    /* ik[4][1][1] */ .1,
    /* ik[4][1][2] */ 0.,
    /* ik[4][2][0] */ 0.,
    /* ik[4][2][1] */ 0.,
    /* ik[4][2][2] */ .5,

/* tree hinge axis vectors */
    /* pin[0][0] */ 1.,
    /* pin[0][1] */ 0.,
    /* pin[0][2] */ 0.,
    /* pin[1][0] */ 0.,
    /* pin[1][1] */ 1.,
    /* pin[1][2] */ 0.,
    /* pin[2][0] */ 0.,
    /* pin[2][1] */ 0.,
    /* pin[2][2] */ 1.,
    /* pin[3][0] */ 0.,
    /* pin[3][1] */ 0.,
    /* pin[3][2] */ 0.,
    /* pin[4][0] */ 0.,
    /* pin[4][1] */ 0.,
    /* pin[4][2] */ 0.,
    /* pin[5][0] */ 0.,
    /* pin[5][1] */ 0.,
    /* pin[5][2] */ 0.,
    /* pin[6][0] */ 0.,
    /* pin[6][1] */ 0.,
    /* pin[6][2] */ 1.,
    /* pin[7][0] */ -1.,
    /* pin[7][1] */ 0.,
    /* pin[7][2] */ 0.,
    /* pin[8][0] */ 0.,
    /* pin[8][1] */ 0.,
    /* pin[8][2] */ 1.,
    /* pin[9][0] */ 1.,
    /* pin[9][1] */ 0.,
    /* pin[9][2] */ 0.,
    /* pin[10][0] */ 0.,
    /* pin[10][1] */ 0.,
    /* pin[10][2] */ 1.,

/* tree bodytojoint vectors */
    /* rk[0][0] */ 0.,
    /* rk[0][1] */ 0.,
    /* rk[0][2] */ 0.,
    /* rk[1][0] */ 0.,
    /* rk[1][1] */ 0.,
    /* rk[1][2] */ -.75,
    /* rk[2][0] */ 0.,
    /* rk[2][1] */ -.22,
    /* rk[2][2] */ .2,
    /* rk[3][0] */ 0.,
    /* rk[3][1] */ 3.3,
    /* rk[3][2] */ 0.,
    /* rk[4][0] */ 0.,
    /* rk[4][1] */ .01,
    /* rk[4][2] */ 0.,

/* tree inbtojoint vectors */
    /* ri[0][0] */ 0.,
    /* ri[0][1] */ 0.,
    /* ri[0][2] */ 0.,
    /* ri[1][0] */ 0.,
    /* ri[1][1] */ 0.,
    /* ri[1][2] */ -1.5,
    /* ri[2][0] */ 0.,
    /* ri[2][1] */ -.1,
    /* ri[2][2] */ -.75,
    /* ri[3][0] */ 0.,
    /* ri[3][1] */ -1.2,
    /* ri[3][2] */ 0.,
    /* ri[4][0] */ 0.,
    /* ri[4][1] */ 0.,
    /* ri[4][2] */ 0.,

/* tree prescribed motion */
    /* pres[0] */ 0.,
    /* pres[1] */ 0.,
    /* pres[2] */ 0.,
    /* pres[3] */ 0.,
    /* pres[4] */ 0.,
    /* pres[5] */ 0.,
    /* pres[6] */ 0.,
    /* pres[7] */ 0.,
    /* pres[8] */ 0.,
    /* pres[9] */ 0.,
    /* pres[10] */ 1.,

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
    /* mmap[6] */ 6,
    /* mmap[7] */ 7,
    /* mmap[8] */ 8,
    /* mmap[9] */ 9,
    /* mmap[10] */ 10,

/* Which parameters were "?" (1) or "<nominal>?" (3) */
    /* gravq[0] */ 0,
    /* gravq[1] */ 0,
    /* gravq[2] */ 0,
    /* mkq[0] */ 0,
    /* mkq[1] */ 0,
    /* mkq[2] */ 0,
    /* mkq[3] */ 0,
    /* mkq[4] */ 0,
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
    /* ikq[3][0][0] */ 0,
    /* ikq[3][0][1] */ 0,
    /* ikq[3][0][2] */ 0,
    /* ikq[3][1][0] */ 0,
    /* ikq[3][1][1] */ 0,
    /* ikq[3][1][2] */ 0,
    /* ikq[3][2][0] */ 0,
    /* ikq[3][2][1] */ 0,
    /* ikq[3][2][2] */ 0,
    /* ikq[4][0][0] */ 0,
    /* ikq[4][0][1] */ 0,
    /* ikq[4][0][2] */ 0,
    /* ikq[4][1][0] */ 0,
    /* ikq[4][1][1] */ 0,
    /* ikq[4][1][2] */ 0,
    /* ikq[4][2][0] */ 0,
    /* ikq[4][2][1] */ 0,
    /* ikq[4][2][2] */ 0,
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
    /* pinq[6][0] */ 0,
    /* pinq[6][1] */ 0,
    /* pinq[6][2] */ 0,
    /* pinq[7][0] */ 0,
    /* pinq[7][1] */ 0,
    /* pinq[7][2] */ 0,
    /* pinq[8][0] */ 0,
    /* pinq[8][1] */ 0,
    /* pinq[8][2] */ 0,
    /* pinq[9][0] */ 0,
    /* pinq[9][1] */ 0,
    /* pinq[9][2] */ 0,
    /* pinq[10][0] */ 0,
    /* pinq[10][1] */ 0,
    /* pinq[10][2] */ 0,
    /* rkq[0][0] */ 0,
    /* rkq[0][1] */ 0,
    /* rkq[0][2] */ 0,
    /* rkq[1][0] */ 0,
    /* rkq[1][1] */ 0,
    /* rkq[1][2] */ 0,
    /* rkq[2][0] */ 0,
    /* rkq[2][1] */ 0,
    /* rkq[2][2] */ 0,
    /* rkq[3][0] */ 0,
    /* rkq[3][1] */ 0,
    /* rkq[3][2] */ 0,
    /* rkq[4][0] */ 0,
    /* rkq[4][1] */ 0,
    /* rkq[4][2] */ 0,
    /* riq[0][0] */ 0,
    /* riq[0][1] */ 0,
    /* riq[0][2] */ 0,
    /* riq[1][0] */ 0,
    /* riq[1][1] */ 0,
    /* riq[1][2] */ 0,
    /* riq[2][0] */ 0,
    /* riq[2][1] */ 0,
    /* riq[2][2] */ 0,
    /* riq[3][0] */ 0,
    /* riq[3][1] */ 0,
    /* riq[3][2] */ 0,
    /* riq[4][0] */ 0,
    /* riq[4][1] */ 3,
    /* riq[4][2] */ 0,
    /* presq[0] */ 0,
    /* presq[1] */ 0,
    /* presq[2] */ 0,
    /* presq[3] */ 0,
    /* presq[4] */ 0,
    /* presq[5] */ 0,
    /* presq[6] */ 0,
    /* presq[7] */ 0,
    /* presq[8] */ 0,
    /* presq[9] */ 0,
    /* presq[10] */ 0,
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
    for (k = 0; k < 5; k++) {
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
    for (k = 0; k < 11; k++) {
        for (i = 0; i < 3; i++) {
            if (pinq[k][i] == 1) {
                sdseterr(7,28);
            }
        }
    }

/* Normalize pin vectors if necessary */


/* Zero out Vpk and Wpk */

    for (i = 0; i < 11; i++) {
        for (j = i; j <= 10; j++) {
            for (k = 0; k < 3; k++) {
                Vpk[i][j][k] = 0.;
                Wpk[i][j][k] = 0.;
            }
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
    rcom[3][0] = 0.;
    rcom[3][1] = 0.;
    rcom[3][2] = 0.;
    rcom[4][0] = 0.;
    rcom[4][1] = 0.;
    rcom[4][2] = 0.;
    dik[10][1] = (.22+ri[4][1]);

/* Compute mass properties-related constants */

    mtot = 486.;
    sdserialno(&i);
    if (i != 10001) {
        sdseterr(7,41);
    }
    roustate = 1;
}

/* Convert state to form using 1-2-3 Euler angles for ball joints. */

void sdst2ang(double st[12],
    double stang[11])
{
    int i;
    double dc[3][3];

    for (i = 0; i < 11; i++) {
        stang[i] = st[i];
    }
    sdquat2dc(st[3],st[4],st[5],st[11],dc);
    sddc2ang(dc,&stang[3],&stang[4],&stang[5]);
}

/* Convert 1-2-3 form of state back to Euler parameters for ball joints. */

void sdang2st(double stang[11],
    double st[12])
{
    int i;
    double dc[3][3];

    for (i = 0; i < 11; i++) {
        st[i] = stang[i];
    }
    sdang2dc(stang[3],stang[4],stang[5],dc);
    sddc2quat(dc,&st[3],&st[4],&st[5],&st[11]);
}

/* Normalize Euler parameters in state. */

void sdnrmsterr(double st[12],
    double normst[12],
    int routine)
{
    int i;
    double norm;

    for (i = 0; i < 12; i++) {
        normst[i] = st[i];
    }
    norm = sqrt(st[3]*st[3]+st[4]*st[4]+st[5]*st[5]+st[11]*st[11]);
    if (routine != 0) {
        if ((norm < .9) || (norm > 1.1)) {
            sdseterr(routine,14);
        }
    }
    if (norm == 0.) {
        normst[11] = 1.;
        norm = 1.;
    }
    norm = 1./norm;
    normst[3] = normst[3]*norm;
    normst[4] = normst[4]*norm;
    normst[5] = normst[5]*norm;
    normst[11] = normst[11]*norm;
}

void sdnormst(double st[12],
    double normst[12])
{

    sdnrmsterr(st,normst,0);
}

void sdstate(double timein,
    double qin[12],
    double uin[11])
{
/*
Compute kinematic information and store it in sdgstate.

Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
    int i,j,qchg,uchg;
    double ee,stab;

    if ((roustate != 1) && (roustate != 2) && (roustate != 3)) {
        sdseterr(8,22);
        return;
    }
    if (roustate == 1) {
        for (i = 0; i < 11; i++) {
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
        for (i = 0; i < 12; i++) {
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
        for (i = 0; i < 11; i++) {
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
    for (i = 0; i < 12; i++) {
        q[i] = qin[i];
    }
/*
Normalize Euler parameters in state
*/
    sdnrmsterr(q,qn,8);
/*
Compute sines and cosines of q
*/
    s6 = sin(q[6]);
    c6 = cos(q[6]);
    s7 = sin(q[7]);
    c7 = cos(q[7]);
    s8 = sin(q[8]);
    c8 = cos(q[8]);
    s9 = sin(q[9]);
    c9 = cos(q[9]);
    s10 = sin(q[10]);
    c10 = cos(q[10]);
/*
Compute across-axis direction cosines Cik
*/
    Cik[3][0][0] = (1.-(2.*((qn[4]*qn[4])+(qn[5]*qn[5]))));
    Cik[3][0][1] = (2.*((qn[3]*qn[4])-(qn[5]*qn[11])));
    Cik[3][0][2] = (2.*((qn[3]*qn[5])+(qn[4]*qn[11])));
    Cik[3][1][0] = (2.*((qn[3]*qn[4])+(qn[5]*qn[11])));
    Cik[3][1][1] = (1.-(2.*((qn[3]*qn[3])+(qn[5]*qn[5]))));
    Cik[3][1][2] = (2.*((qn[4]*qn[5])-(qn[3]*qn[11])));
    Cik[3][2][0] = (2.*((qn[3]*qn[5])-(qn[4]*qn[11])));
    Cik[3][2][1] = (2.*((qn[3]*qn[11])+(qn[4]*qn[5])));
    Cik[3][2][2] = (1.-(2.*((qn[3]*qn[3])+(qn[4]*qn[4]))));
/*
Compute across-joint direction cosines Cib
*/
    Cib[3][0][1] = -(s8*c9);
    Cib[3][0][2] = (s8*s9);
    Cib[3][1][1] = (c8*c9);
    Cib[3][1][2] = -(s9*c8);
/*
Compute gravity
*/
/*
Compute cnk & cnb (direction cosines in N)
*/
    cnk[6][0][0] = ((Cik[3][0][0]*c6)+(Cik[3][0][1]*s6));
    cnk[6][0][1] = ((Cik[3][0][1]*c6)-(Cik[3][0][0]*s6));
    cnk[6][1][0] = ((Cik[3][1][0]*c6)+(Cik[3][1][1]*s6));
    cnk[6][1][1] = ((Cik[3][1][1]*c6)-(Cik[3][1][0]*s6));
    cnk[6][2][0] = ((Cik[3][2][0]*c6)+(Cik[3][2][1]*s6));
    cnk[6][2][1] = ((Cik[3][2][1]*c6)-(Cik[3][2][0]*s6));
    cnk[7][0][1] = ((cnk[6][0][1]*c7)-(Cik[3][0][2]*s7));
    cnk[7][0][2] = ((Cik[3][0][2]*c7)+(cnk[6][0][1]*s7));
    cnk[7][1][1] = ((cnk[6][1][1]*c7)-(Cik[3][1][2]*s7));
    cnk[7][1][2] = ((Cik[3][1][2]*c7)+(cnk[6][1][1]*s7));
    cnk[7][2][1] = ((cnk[6][2][1]*c7)-(Cik[3][2][2]*s7));
    cnk[7][2][2] = ((Cik[3][2][2]*c7)+(cnk[6][2][1]*s7));
    cnk[8][0][0] = ((Cik[3][0][0]*c8)+(Cik[3][0][1]*s8));
    cnk[8][0][1] = ((Cik[3][0][1]*c8)-(Cik[3][0][0]*s8));
    cnk[8][1][0] = ((Cik[3][1][0]*c8)+(Cik[3][1][1]*s8));
    cnk[8][1][1] = ((Cik[3][1][1]*c8)-(Cik[3][1][0]*s8));
    cnk[8][2][0] = ((Cik[3][2][0]*c8)+(Cik[3][2][1]*s8));
    cnk[8][2][1] = ((Cik[3][2][1]*c8)-(Cik[3][2][0]*s8));
    cnk[9][0][1] = ((Cik[3][0][2]*s9)+(cnk[8][0][1]*c9));
    cnk[9][0][2] = ((Cik[3][0][2]*c9)-(cnk[8][0][1]*s9));
    cnk[9][1][1] = ((Cik[3][1][2]*s9)+(cnk[8][1][1]*c9));
    cnk[9][1][2] = ((Cik[3][1][2]*c9)-(cnk[8][1][1]*s9));
    cnk[9][2][1] = ((Cik[3][2][2]*s9)+(cnk[8][2][1]*c9));
    cnk[9][2][2] = ((Cik[3][2][2]*c9)-(cnk[8][2][1]*s9));
    cnk[10][0][0] = ((cnk[6][0][0]*c10)+(cnk[7][0][1]*s10));
    cnk[10][0][1] = ((cnk[7][0][1]*c10)-(cnk[6][0][0]*s10));
    cnk[10][1][0] = ((cnk[6][1][0]*c10)+(cnk[7][1][1]*s10));
    cnk[10][1][1] = ((cnk[7][1][1]*c10)-(cnk[6][1][0]*s10));
    cnk[10][2][0] = ((cnk[6][2][0]*c10)+(cnk[7][2][1]*s10));
    cnk[10][2][1] = ((cnk[7][2][1]*c10)-(cnk[6][2][0]*s10));
    cnb[0][0][0] = Cik[3][0][0];
    cnb[0][0][1] = Cik[3][0][1];
    cnb[0][0][2] = Cik[3][0][2];
    cnb[0][1][0] = Cik[3][1][0];
    cnb[0][1][1] = Cik[3][1][1];
    cnb[0][1][2] = Cik[3][1][2];
    cnb[0][2][0] = Cik[3][2][0];
    cnb[0][2][1] = Cik[3][2][1];
    cnb[0][2][2] = Cik[3][2][2];
    cnb[1][0][0] = cnk[6][0][0];
    cnb[1][0][1] = cnk[6][0][1];
    cnb[1][0][2] = Cik[3][0][2];
    cnb[1][1][0] = cnk[6][1][0];
    cnb[1][1][1] = cnk[6][1][1];
    cnb[1][1][2] = Cik[3][1][2];
    cnb[1][2][0] = cnk[6][2][0];
    cnb[1][2][1] = cnk[6][2][1];
    cnb[1][2][2] = Cik[3][2][2];
    cnb[2][0][0] = cnk[6][0][0];
    cnb[2][0][1] = cnk[7][0][1];
    cnb[2][0][2] = cnk[7][0][2];
    cnb[2][1][0] = cnk[6][1][0];
    cnb[2][1][1] = cnk[7][1][1];
    cnb[2][1][2] = cnk[7][1][2];
    cnb[2][2][0] = cnk[6][2][0];
    cnb[2][2][1] = cnk[7][2][1];
    cnb[2][2][2] = cnk[7][2][2];
    cnb[3][0][0] = cnk[8][0][0];
    cnb[3][0][1] = cnk[9][0][1];
    cnb[3][0][2] = cnk[9][0][2];
    cnb[3][1][0] = cnk[8][1][0];
    cnb[3][1][1] = cnk[9][1][1];
    cnb[3][1][2] = cnk[9][1][2];
    cnb[3][2][0] = cnk[8][2][0];
    cnb[3][2][1] = cnk[9][2][1];
    cnb[3][2][2] = cnk[9][2][2];
    cnb[4][0][0] = cnk[10][0][0];
    cnb[4][0][1] = cnk[10][0][1];
    cnb[4][0][2] = cnk[7][0][2];
    cnb[4][1][0] = cnk[10][1][0];
    cnb[4][1][1] = cnk[10][1][1];
    cnb[4][1][2] = cnk[7][1][2];
    cnb[4][2][0] = cnk[10][2][0];
    cnb[4][2][1] = cnk[10][2][1];
    cnb[4][2][2] = cnk[7][2][2];
/*
Compute q-related auxiliary variables
*/
    rik[7][1] = (.22+((.75*s7)-(.1*c7)));
    rik[7][2] = -(.2+((.1*s7)+(.75*c7)));
    rik[10][0] = (ri[4][1]*s10);
    rik[10][1] = ((ri[4][1]*c10)-.01);
/*
Compute rnk & rnb (mass center locations in N)
*/
    rnk[6][0] = ((.75*Cik[3][0][2])+(q[0]-(1.5*Cik[3][0][2])));
    rnk[6][1] = ((.75*Cik[3][1][2])+(q[1]-(1.5*Cik[3][1][2])));
    rnk[6][2] = ((.75*Cik[3][2][2])+(q[2]-(1.5*Cik[3][2][2])));
    rnk[7][0] = ((rnk[6][0]-((.1*cnk[6][0][1])+(.75*Cik[3][0][2])))+((.22*
      cnk[7][0][1])-(.2*cnk[7][0][2])));
    rnk[7][1] = ((rnk[6][1]-((.1*cnk[6][1][1])+(.75*Cik[3][1][2])))+((.22*
      cnk[7][1][1])-(.2*cnk[7][1][2])));
    rnk[7][2] = ((rnk[6][2]-((.1*cnk[6][2][1])+(.75*Cik[3][2][2])))+((.22*
      cnk[7][2][1])-(.2*cnk[7][2][2])));
    rnk[8][0] = (q[0]-(1.2*Cik[3][0][1]));
    rnk[8][1] = (q[1]-(1.2*Cik[3][1][1]));
    rnk[8][2] = (q[2]-(1.2*Cik[3][2][1]));
    rnk[9][0] = (rnk[8][0]-(3.3*cnk[9][0][1]));
    rnk[9][1] = (rnk[8][1]-(3.3*cnk[9][1][1]));
    rnk[9][2] = (rnk[8][2]-(3.3*cnk[9][2][1]));
    rnk[10][0] = ((rnk[7][0]+(cnk[7][0][1]*ri[4][1]))-(.01*cnk[10][0][1]));
    rnk[10][1] = ((rnk[7][1]+(cnk[7][1][1]*ri[4][1]))-(.01*cnk[10][1][1]));
    rnk[10][2] = ((rnk[7][2]+(cnk[7][2][1]*ri[4][1]))-(.01*cnk[10][2][1]));
    rnb[0][0] = q[0];
    rnb[0][1] = q[1];
    rnb[0][2] = q[2];
    rnb[1][0] = rnk[6][0];
    rnb[1][1] = rnk[6][1];
    rnb[1][2] = rnk[6][2];
    rnb[2][0] = rnk[7][0];
    rnb[2][1] = rnk[7][1];
    rnb[2][2] = rnk[7][2];
    rnb[3][0] = rnk[9][0];
    rnb[3][1] = rnk[9][1];
    rnb[3][2] = rnk[9][2];
    rnb[4][0] = rnk[10][0];
    rnb[4][1] = rnk[10][1];
    rnb[4][2] = rnk[10][2];
/*
Compute com (system mass center location in N)
*/
    com[0] = (.00205761316872428*(rnk[10][0]+((10.7*rnk[9][0])+((57.5*rnk[7][0])
      +((6.8*rnk[6][0])+(410.*q[0]))))));
    com[1] = (.00205761316872428*(rnk[10][1]+((10.7*rnk[9][1])+((57.5*rnk[7][1])
      +((6.8*rnk[6][1])+(410.*q[1]))))));
    com[2] = (.00205761316872428*(rnk[10][2]+((10.7*rnk[9][2])+((57.5*rnk[7][2])
      +((6.8*rnk[6][2])+(410.*q[2]))))));
/*
Compute constraint position errors
*/
    skipqs: ;
    if (uchg == 0) {
        goto skipus;
    }
/*
Velocity-related variables need to be computed
*/
    inerflg = 0;
    for (i = 0; i < 11; i++) {
        u[i] = uin[i];
    }
/*
Compute u-related auxiliary variables
*/
/*
Compute wk & wb (angular velocities)
*/
    wk[6][0] = ((u[3]*c6)+(u[4]*s6));
    wk[6][1] = ((u[4]*c6)-(u[3]*s6));
    wk[6][2] = (u[5]+u[6]);
    wk[7][0] = (wk[6][0]-u[7]);
    wk[7][1] = ((wk[6][1]*c7)-(wk[6][2]*s7));
    wk[7][2] = ((wk[6][1]*s7)+(wk[6][2]*c7));
    wk[8][0] = ((u[3]*c8)+(u[4]*s8));
    wk[8][1] = ((u[4]*c8)-(u[3]*s8));
    wk[8][2] = (u[5]+u[8]);
    wk[9][0] = (u[9]+wk[8][0]);
    wk[9][1] = ((wk[8][1]*c9)+(wk[8][2]*s9));
    wk[9][2] = ((wk[8][2]*c9)-(wk[8][1]*s9));
    wk[10][0] = ((wk[7][0]*c10)+(wk[7][1]*s10));
    wk[10][1] = ((wk[7][1]*c10)-(wk[7][0]*s10));
    wk[10][2] = (u[10]+wk[7][2]);
    wb[0][0] = u[3];
    wb[0][1] = u[4];
    wb[0][2] = u[5];
    wb[1][0] = wk[6][0];
    wb[1][1] = wk[6][1];
    wb[1][2] = wk[6][2];
    wb[2][0] = wk[7][0];
    wb[2][1] = wk[7][1];
    wb[2][2] = wk[7][2];
    wb[3][0] = wk[9][0];
    wb[3][1] = wk[9][1];
    wb[3][2] = wk[9][2];
    wb[4][0] = wk[10][0];
    wb[4][1] = wk[10][1];
    wb[4][2] = wk[10][2];
/*
Compute auxiliary variables involving wk
*/
    Wirk[7][0] = ((.1*wk[6][2])-(.75*wk[6][1]));
    Wirk[10][0] = -(ri[4][1]*wk[7][2]);
    Wirk[10][2] = (ri[4][1]*wk[7][0]);
    Wkrpk[7][0] = -((.2*wk[7][1])+(.22*wk[7][2]));
    IkWk[5][0] = ((14.*u[5])+((115.*u[3])-(14.*u[4])));
    IkWk[5][1] = (((316.*u[4])-(14.*u[3]))-(34.6*u[5]));
    IkWk[5][2] = ((440.*u[5])+((14.*u[3])-(34.6*u[4])));
    WkIkWk[5][0] = ((IkWk[5][2]*u[4])-(IkWk[5][1]*u[5]));
    WkIkWk[5][1] = ((IkWk[5][0]*u[5])-(IkWk[5][2]*u[3]));
    WkIkWk[5][2] = ((IkWk[5][1]*u[3])-(IkWk[5][0]*u[4]));
    WkIkWk[6][0] = -(.35*(wk[6][1]*wk[6][2]));
    WkIkWk[6][1] = (.35*(wk[6][0]*wk[6][2]));
    IkWk[7][0] = (((.41*wk[7][1])+(4.85*wk[7][0]))-(.07*wk[7][2]));
    IkWk[7][1] = ((.54*wk[7][2])+((.41*wk[7][0])+(2.2*wk[7][1])));
    IkWk[7][2] = ((5.5*wk[7][2])+((.54*wk[7][1])-(.07*wk[7][0])));
    WkIkWk[7][0] = ((IkWk[7][2]*wk[7][1])-(IkWk[7][1]*wk[7][2]));
    WkIkWk[7][1] = ((IkWk[7][0]*wk[7][2])-(IkWk[7][2]*wk[7][0]));
    WkIkWk[7][2] = ((IkWk[7][1]*wk[7][0])-(IkWk[7][0]*wk[7][1]));
    WkIkWk[9][0] = (27.*(wk[9][1]*wk[9][2]));
    WkIkWk[9][2] = -(27.*(wk[9][0]*wk[9][1]));
    WkIkWk[10][0] = (.4*(wk[10][1]*wk[10][2]));
    WkIkWk[10][1] = -(.4*(wk[10][0]*wk[10][2]));
/*
Compute temporaries for use in SDRHS
*/
    w0w0[0] = (u[3]*u[3]);
    w0w0[1] = (wk[6][0]*wk[6][0]);
    w0w0[2] = (wk[7][0]*wk[7][0]);
    w0w0[3] = (wk[9][0]*wk[9][0]);
    w0w0[4] = (wk[10][0]*wk[10][0]);
    w1w1[0] = (u[4]*u[4]);
    w1w1[1] = (wk[6][1]*wk[6][1]);
    w1w1[2] = (wk[7][1]*wk[7][1]);
    w1w1[3] = (wk[9][1]*wk[9][1]);
    w1w1[4] = (wk[10][1]*wk[10][1]);
    w2w2[0] = (u[5]*u[5]);
    w2w2[1] = (wk[6][2]*wk[6][2]);
    w2w2[2] = (wk[7][2]*wk[7][2]);
    w2w2[3] = (wk[9][2]*wk[9][2]);
    w2w2[4] = (wk[10][2]*wk[10][2]);
    w0w1[0] = (u[3]*u[4]);
    w0w1[1] = (wk[6][0]*wk[6][1]);
    w0w1[2] = (wk[7][0]*wk[7][1]);
    w0w1[3] = (wk[9][0]*wk[9][1]);
    w0w1[4] = (wk[10][0]*wk[10][1]);
    w0w2[0] = (u[3]*u[5]);
    w0w2[1] = (wk[6][0]*wk[6][2]);
    w0w2[2] = (wk[7][0]*wk[7][2]);
    w0w2[3] = (wk[9][0]*wk[9][2]);
    w0w2[4] = (wk[10][0]*wk[10][2]);
    w1w2[0] = (u[4]*u[5]);
    w1w2[1] = (wk[6][1]*wk[6][2]);
    w1w2[2] = (wk[7][1]*wk[7][2]);
    w1w2[3] = (wk[9][1]*wk[9][2]);
    w1w2[4] = (wk[10][1]*wk[10][2]);
    w00w11[0] = -(w0w0[0]+w1w1[0]);
    w00w11[1] = -(w0w0[1]+w1w1[1]);
    w00w11[2] = -(w0w0[2]+w1w1[2]);
    w00w11[3] = -(w0w0[3]+w1w1[3]);
    w00w11[4] = -(w0w0[4]+w1w1[4]);
    w00w22[0] = -(w0w0[0]+w2w2[0]);
    w00w22[1] = -(w0w0[1]+w2w2[1]);
    w00w22[2] = -(w0w0[2]+w2w2[2]);
    w00w22[3] = -(w0w0[3]+w2w2[3]);
    w00w22[4] = -(w0w0[4]+w2w2[4]);
    w11w22[0] = -(w1w1[0]+w2w2[0]);
    w11w22[1] = -(w1w1[1]+w2w2[1]);
    w11w22[2] = -(w1w1[2]+w2w2[2]);
    w11w22[3] = -(w1w1[3]+w2w2[3]);
    w11w22[4] = -(w1w1[4]+w2w2[4]);
/*
Compute vnk & vnb (mass center linear velocities in N)
*/
    vnk[6][0] = ((.75*((cnk[6][0][0]*wk[6][1])-(cnk[6][0][1]*wk[6][0])))+(u[0]+(
      1.5*((Cik[3][0][1]*u[3])-(Cik[3][0][0]*u[4])))));
    vnk[6][1] = ((.75*((cnk[6][1][0]*wk[6][1])-(cnk[6][1][1]*wk[6][0])))+(u[1]+(
      1.5*((Cik[3][1][1]*u[3])-(Cik[3][1][0]*u[4])))));
    vnk[6][2] = ((.75*((cnk[6][2][0]*wk[6][1])-(cnk[6][2][1]*wk[6][0])))+(u[2]+(
      1.5*((Cik[3][2][1]*u[3])-(Cik[3][2][0]*u[4])))));
    vnk[7][0] = ((vnk[6][0]+(((.75*(cnk[6][0][1]*wk[6][0]))+(cnk[6][0][0]*
      Wirk[7][0]))-(.1*(Cik[3][0][2]*wk[6][0]))))+((.22*(cnk[7][0][2]*wk[7][0]))
      +((.2*(cnk[7][0][1]*wk[7][0]))+(cnk[6][0][0]*Wkrpk[7][0]))));
    vnk[7][1] = ((vnk[6][1]+(((.75*(cnk[6][1][1]*wk[6][0]))+(cnk[6][1][0]*
      Wirk[7][0]))-(.1*(Cik[3][1][2]*wk[6][0]))))+((.22*(cnk[7][1][2]*wk[7][0]))
      +((.2*(cnk[7][1][1]*wk[7][0]))+(cnk[6][1][0]*Wkrpk[7][0]))));
    vnk[7][2] = ((vnk[6][2]+(((.75*(cnk[6][2][1]*wk[6][0]))+(cnk[6][2][0]*
      Wirk[7][0]))-(.1*(Cik[3][2][2]*wk[6][0]))))+((.22*(cnk[7][2][2]*wk[7][0]))
      +((.2*(cnk[7][2][1]*wk[7][0]))+(cnk[6][2][0]*Wkrpk[7][0]))));
    vnk[8][0] = (u[0]+(1.2*((Cik[3][0][0]*u[5])-(Cik[3][0][2]*u[3]))));
    vnk[8][1] = (u[1]+(1.2*((Cik[3][1][0]*u[5])-(Cik[3][1][2]*u[3]))));
    vnk[8][2] = (u[2]+(1.2*((Cik[3][2][0]*u[5])-(Cik[3][2][2]*u[3]))));
    vnk[9][0] = (vnk[8][0]+(3.3*((cnk[8][0][0]*wk[9][2])-(cnk[9][0][2]*wk[9][0])
      )));
    vnk[9][1] = (vnk[8][1]+(3.3*((cnk[8][1][0]*wk[9][2])-(cnk[9][1][2]*wk[9][0])
      )));
    vnk[9][2] = (vnk[8][2]+(3.3*((cnk[8][2][0]*wk[9][2])-(cnk[9][2][2]*wk[9][0])
      )));
    vnk[10][0] = ((.01*((cnk[10][0][0]*wk[10][2])-(cnk[7][0][2]*wk[10][0])))+(
      vnk[7][0]+((cnk[6][0][0]*Wirk[10][0])+(cnk[7][0][2]*Wirk[10][2]))));
    vnk[10][1] = ((.01*((cnk[10][1][0]*wk[10][2])-(cnk[7][1][2]*wk[10][0])))+(
      vnk[7][1]+((cnk[6][1][0]*Wirk[10][0])+(cnk[7][1][2]*Wirk[10][2]))));
    vnk[10][2] = ((.01*((cnk[10][2][0]*wk[10][2])-(cnk[7][2][2]*wk[10][0])))+(
      vnk[7][2]+((cnk[6][2][0]*Wirk[10][0])+(cnk[7][2][2]*Wirk[10][2]))));
    vnb[0][0] = u[0];
    vnb[0][1] = u[1];
    vnb[0][2] = u[2];
    vnb[1][0] = vnk[6][0];
    vnb[1][1] = vnk[6][1];
    vnb[1][2] = vnk[6][2];
    vnb[2][0] = vnk[7][0];
    vnb[2][1] = vnk[7][1];
    vnb[2][2] = vnk[7][2];
    vnb[3][0] = vnk[9][0];
    vnb[3][1] = vnk[9][1];
    vnb[3][2] = vnk[9][2];
    vnb[4][0] = vnk[10][0];
    vnb[4][1] = vnk[10][1];
    vnb[4][2] = vnk[10][2];
/*
Compute qdot (kinematical equations)
*/
    qdot[0] = u[0];
    qdot[1] = u[1];
    qdot[2] = u[2];
    qdot[3] = (.5*((q[11]*u[3])+((q[4]*u[5])-(q[5]*u[4]))));
    qdot[4] = (.5*((q[5]*u[3])+((q[11]*u[4])-(q[3]*u[5]))));
    qdot[5] = (.5*(((q[3]*u[4])+(q[11]*u[5]))-(q[4]*u[3])));
    qdot[11] = -(.5*((q[3]*u[3])+((q[4]*u[4])+(q[5]*u[5]))));
    if (stabvel  !=  0.) {
        ee = ((q[11]*q[11])+((q[5]*q[5])+((q[3]*q[3])+(q[4]*q[4]))));
        stab = ((stabvel*(1.-ee))/ee);
        qdot[3] = (qdot[3]+(q[3]*stab));
        qdot[4] = (qdot[4]+(q[4]*stab));
        qdot[5] = (qdot[5]+(q[5]*stab));
        qdot[11] = (qdot[11]+(q[11]*stab));
    }
    qdot[6] = u[6];
    qdot[7] = u[7];
    qdot[8] = u[8];
    qdot[9] = u[9];
    qdot[10] = u[10];
/*
Compute constraint velocity errors
*/
    skipus: ;
/*
Initialize applied forces and torques to zero
*/
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 3; j++) {
            ufk[i][j] = 0.;
            utk[i][j] = 0.;
        }
    }
    for (i = 0; i < 11; i++) {
        utau[i] = 0.;
    }
    ltauflg = 0;
    fs0flg = 0;
/*
Initialize prescribed motions
*/
    uacc[10] = 0.;
    uvel[10] = u[10];
    upos[10] = q[10];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  233 adds/subtracts/negates
                    346 multiplies
                      1 divides
                    337 assignments
*/
}

void sdqdot(double oqdot[12])
{
/*
Return position coordinate derivatives for tree joints.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(63,23);
        return;
    }
    for (i = 0; i <= 11; i++) {
        oqdot[i] = qdot[i];
    }
}

void sdu2qdot(double uin[11],
    double oqdot[12])
{
/*
Convert velocities to qdots.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(64,23);
        return;
    }
    for (i = 0; i <= 10; i++) {
        oqdot[i] = uin[i];
    }
    oqdot[3] = (.5*((q[11]*uin[3])+((q[4]*uin[5])-(q[5]*uin[4]))));
    oqdot[4] = (.5*((q[5]*uin[3])+((q[11]*uin[4])-(q[3]*uin[5]))));
    oqdot[5] = (.5*(((q[3]*uin[4])+(q[11]*uin[5]))-(q[4]*uin[3])));
    oqdot[11] = -(.5*((q[3]*uin[3])+((q[4]*uin[4])+(q[5]*uin[5]))));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    9 adds/subtracts/negates
                     16 multiplies
                      0 divides
                     15 assignments
*/
}

void sdpsstate(double lqin[1])
{

    if (roustate != 2) {
        sdseterr(9,23);
        return;
    }
}

void sddovpk(void)
{

    if (vpkflg == 0) {
/*
Compute Wpk (partial angular velocities)
*/
        Wpk[3][3][0] = 1.;
        Wpk[3][4][0] = 1.;
        Wpk[3][5][0] = 1.;
        Wpk[3][6][0] = c6;
        Wpk[3][6][1] = -s6;
        Wpk[3][7][0] = c6;
        Wpk[3][7][1] = -(s6*c7);
        Wpk[3][7][2] = -(s6*s7);
        Wpk[3][8][0] = c8;
        Wpk[3][8][1] = -s8;
        Wpk[3][9][0] = c8;
        Wpk[3][9][1] = -(s8*c9);
        Wpk[3][9][2] = (s8*s9);
        Wpk[3][10][0] = ((Wpk[3][7][1]*s10)+(c6*c10));
        Wpk[3][10][1] = ((Wpk[3][7][1]*c10)-(s10*c6));
        Wpk[3][10][2] = Wpk[3][7][2];
        Wpk[4][4][1] = 1.;
        Wpk[4][5][1] = 1.;
        Wpk[4][6][0] = s6;
        Wpk[4][6][1] = c6;
        Wpk[4][7][0] = s6;
        Wpk[4][7][1] = (c6*c7);
        Wpk[4][7][2] = (s7*c6);
        Wpk[4][8][0] = s8;
        Wpk[4][8][1] = c8;
        Wpk[4][9][0] = s8;
        Wpk[4][9][1] = (c8*c9);
        Wpk[4][9][2] = -(s9*c8);
        Wpk[4][10][0] = ((Wpk[4][7][1]*s10)+(s6*c10));
        Wpk[4][10][1] = ((Wpk[4][7][1]*c10)-(s6*s10));
        Wpk[4][10][2] = Wpk[4][7][2];
        Wpk[5][5][2] = 1.;
        Wpk[5][6][2] = 1.;
        Wpk[5][7][1] = -s7;
        Wpk[5][7][2] = c7;
        Wpk[5][8][2] = 1.;
        Wpk[5][9][1] = s9;
        Wpk[5][9][2] = c9;
        Wpk[5][10][0] = -(s7*s10);
        Wpk[5][10][1] = -(s7*c10);
        Wpk[5][10][2] = c7;
        Wpk[6][6][2] = 1.;
        Wpk[6][7][1] = -s7;
        Wpk[6][7][2] = c7;
        Wpk[6][10][0] = -(s7*s10);
        Wpk[6][10][1] = -(s7*c10);
        Wpk[6][10][2] = c7;
        Wpk[7][7][0] = -1.;
        Wpk[7][10][0] = -c10;
        Wpk[7][10][1] = s10;
        Wpk[8][8][2] = 1.;
        Wpk[8][9][1] = s9;
        Wpk[8][9][2] = c9;
        Wpk[9][9][0] = 1.;
        Wpk[10][10][2] = 1.;
/*
Compute Vpk (partial velocities)
*/
        Vpk[0][0][0] = 1.;
        Vpk[0][1][0] = 1.;
        Vpk[0][2][0] = 1.;
        Vpk[0][3][0] = Cik[3][0][0];
        Vpk[0][3][1] = Cik[3][0][1];
        Vpk[0][3][2] = Cik[3][0][2];
        Vpk[0][4][0] = Cik[3][0][0];
        Vpk[0][4][1] = Cik[3][0][1];
        Vpk[0][4][2] = Cik[3][0][2];
        Vpk[0][5][0] = Cik[3][0][0];
        Vpk[0][5][1] = Cik[3][0][1];
        Vpk[0][5][2] = Cik[3][0][2];
        Vpk[0][6][0] = ((Cik[3][0][0]*c6)+(Cik[3][0][1]*s6));
        Vpk[0][6][1] = ((Cik[3][0][1]*c6)-(Cik[3][0][0]*s6));
        Vpk[0][6][2] = Cik[3][0][2];
        Vpk[0][7][0] = Vpk[0][6][0];
        Vpk[0][7][1] = ((Vpk[0][6][1]*c7)-(Cik[3][0][2]*s7));
        Vpk[0][7][2] = ((Cik[3][0][2]*c7)+(Vpk[0][6][1]*s7));
        Vpk[0][8][0] = ((Cik[3][0][0]*c8)+(Cik[3][0][1]*s8));
        Vpk[0][8][1] = ((Cik[3][0][1]*c8)-(Cik[3][0][0]*s8));
        Vpk[0][8][2] = Cik[3][0][2];
        Vpk[0][9][0] = Vpk[0][8][0];
        Vpk[0][9][1] = ((Cik[3][0][2]*s9)+(Vpk[0][8][1]*c9));
        Vpk[0][9][2] = ((Cik[3][0][2]*c9)-(Vpk[0][8][1]*s9));
        Vpk[0][10][0] = ((Vpk[0][6][0]*c10)+(Vpk[0][7][1]*s10));
        Vpk[0][10][1] = ((Vpk[0][7][1]*c10)-(Vpk[0][6][0]*s10));
        Vpk[0][10][2] = Vpk[0][7][2];
        Vpk[1][1][1] = 1.;
        Vpk[1][2][1] = 1.;
        Vpk[1][3][0] = Cik[3][1][0];
        Vpk[1][3][1] = Cik[3][1][1];
        Vpk[1][3][2] = Cik[3][1][2];
        Vpk[1][4][0] = Cik[3][1][0];
        Vpk[1][4][1] = Cik[3][1][1];
        Vpk[1][4][2] = Cik[3][1][2];
        Vpk[1][5][0] = Cik[3][1][0];
        Vpk[1][5][1] = Cik[3][1][1];
        Vpk[1][5][2] = Cik[3][1][2];
        Vpk[1][6][0] = ((Cik[3][1][0]*c6)+(Cik[3][1][1]*s6));
        Vpk[1][6][1] = ((Cik[3][1][1]*c6)-(Cik[3][1][0]*s6));
        Vpk[1][6][2] = Cik[3][1][2];
        Vpk[1][7][0] = Vpk[1][6][0];
        Vpk[1][7][1] = ((Vpk[1][6][1]*c7)-(Cik[3][1][2]*s7));
        Vpk[1][7][2] = ((Cik[3][1][2]*c7)+(Vpk[1][6][1]*s7));
        Vpk[1][8][0] = ((Cik[3][1][0]*c8)+(Cik[3][1][1]*s8));
        Vpk[1][8][1] = ((Cik[3][1][1]*c8)-(Cik[3][1][0]*s8));
        Vpk[1][8][2] = Cik[3][1][2];
        Vpk[1][9][0] = Vpk[1][8][0];
        Vpk[1][9][1] = ((Cik[3][1][2]*s9)+(Vpk[1][8][1]*c9));
        Vpk[1][9][2] = ((Cik[3][1][2]*c9)-(Vpk[1][8][1]*s9));
        Vpk[1][10][0] = ((Vpk[1][6][0]*c10)+(Vpk[1][7][1]*s10));
        Vpk[1][10][1] = ((Vpk[1][7][1]*c10)-(Vpk[1][6][0]*s10));
        Vpk[1][10][2] = Vpk[1][7][2];
        Vpk[2][2][2] = 1.;
        Vpk[2][3][0] = Cik[3][2][0];
        Vpk[2][3][1] = Cik[3][2][1];
        Vpk[2][3][2] = Cik[3][2][2];
        Vpk[2][4][0] = Cik[3][2][0];
        Vpk[2][4][1] = Cik[3][2][1];
        Vpk[2][4][2] = Cik[3][2][2];
        Vpk[2][5][0] = Cik[3][2][0];
        Vpk[2][5][1] = Cik[3][2][1];
        Vpk[2][5][2] = Cik[3][2][2];
        Vpk[2][6][0] = ((Cik[3][2][0]*c6)+(Cik[3][2][1]*s6));
        Vpk[2][6][1] = ((Cik[3][2][1]*c6)-(Cik[3][2][0]*s6));
        Vpk[2][6][2] = Cik[3][2][2];
        Vpk[2][7][0] = Vpk[2][6][0];
        Vpk[2][7][1] = ((Vpk[2][6][1]*c7)-(Cik[3][2][2]*s7));
        Vpk[2][7][2] = ((Cik[3][2][2]*c7)+(Vpk[2][6][1]*s7));
        Vpk[2][8][0] = ((Cik[3][2][0]*c8)+(Cik[3][2][1]*s8));
        Vpk[2][8][1] = ((Cik[3][2][1]*c8)-(Cik[3][2][0]*s8));
        Vpk[2][8][2] = Cik[3][2][2];
        Vpk[2][9][0] = Vpk[2][8][0];
        Vpk[2][9][1] = ((Cik[3][2][2]*s9)+(Vpk[2][8][1]*c9));
        Vpk[2][9][2] = ((Cik[3][2][2]*c9)-(Vpk[2][8][1]*s9));
        Vpk[2][10][0] = ((Vpk[2][6][0]*c10)+(Vpk[2][7][1]*s10));
        Vpk[2][10][1] = ((Vpk[2][7][1]*c10)-(Vpk[2][6][0]*s10));
        Vpk[2][10][2] = Vpk[2][7][2];
        Vpk[3][6][0] = (.75*s6);
        Vpk[3][6][1] = (.75*c6);
        Vpk[3][7][0] = ((1.5*s6)-((.2*Wpk[3][7][1])+(.22*Wpk[3][7][2])));
        Vpk[3][7][1] = ((.2*c6)+((.1*(s7*c6))+(1.5*(c6*c7))));
        Vpk[3][7][2] = ((.22*c6)+((1.5*(s7*c6))-(.1*(c6*c7))));
        Vpk[3][8][2] = -1.2;
        Vpk[3][9][0] = (3.3*Wpk[3][9][2]);
        Vpk[3][9][1] = -(1.2*s9);
        Vpk[3][9][2] = -((1.2*c9)+(3.3*c8));
        VWri[3][10][0] = (Vpk[3][7][0]-(ri[4][1]*Wpk[3][7][2]));
        VWri[3][10][2] = (Vpk[3][7][2]+(ri[4][1]*c6));
        Vpk[3][10][0] = ((.01*Wpk[3][7][2])+((Vpk[3][7][1]*s10)+(VWri[3][10][0]*
          c10)));
        Vpk[3][10][1] = ((Vpk[3][7][1]*c10)-(VWri[3][10][0]*s10));
        Vpk[3][10][2] = (VWri[3][10][2]-(.01*Wpk[3][10][0]));
        Vpk[4][6][0] = -(.75*c6);
        Vpk[4][6][1] = (.75*s6);
        Vpk[4][7][0] = -((1.5*c6)+((.2*Wpk[4][7][1])+(.22*Wpk[4][7][2])));
        Vpk[4][7][1] = ((.2*s6)+((.1*(s6*s7))+(1.5*(s6*c7))));
        Vpk[4][7][2] = ((.22*s6)+((1.5*(s6*s7))-(.1*(s6*c7))));
        Vpk[4][9][0] = (3.3*Wpk[4][9][2]);
        Vpk[4][9][2] = -(3.3*s8);
        VWri[4][10][0] = (Vpk[4][7][0]-(ri[4][1]*Wpk[4][7][2]));
        VWri[4][10][2] = (Vpk[4][7][2]+(ri[4][1]*s6));
        Vpk[4][10][0] = ((.01*Wpk[4][7][2])+((Vpk[4][7][1]*s10)+(VWri[4][10][0]*
          c10)));
        Vpk[4][10][1] = ((Vpk[4][7][1]*c10)-(VWri[4][10][0]*s10));
        Vpk[4][10][2] = (VWri[4][10][2]-(.01*Wpk[4][10][0]));
        Vpk[5][7][0] = (.1+((.2*s7)-(.22*c7)));
        Vpk[5][8][0] = (1.2*c8);
        Vpk[5][8][1] = -(1.2*s8);
        Vpk[5][9][0] = ((1.2*c8)+(3.3*c9));
        Vpk[5][9][1] = -(1.2*(s8*c9));
        Vpk[5][9][2] = (1.2*(s8*s9));
        VWri[5][10][0] = (Vpk[5][7][0]-(ri[4][1]*c7));
        Vpk[5][10][0] = ((.01*c7)+(VWri[5][10][0]*c10));
        Vpk[5][10][1] = -(VWri[5][10][0]*s10);
        Vpk[5][10][2] = -(.01*Wpk[5][10][0]);
        Vpk[6][7][0] = (.1+((.2*s7)-(.22*c7)));
        VWri[6][10][0] = (Vpk[6][7][0]-(ri[4][1]*c7));
        Vpk[6][10][0] = ((.01*c7)+(VWri[6][10][0]*c10));
        Vpk[6][10][1] = -(VWri[6][10][0]*s10);
        Vpk[6][10][2] = -(.01*Wpk[6][10][0]);
        Vpk[7][7][1] = -.2;
        Vpk[7][7][2] = -.22;
        VWri[7][10][2] = -(.22+ri[4][1]);
        Vpk[7][10][0] = -(.2*s10);
        Vpk[7][10][1] = -(.2*c10);
        Vpk[7][10][2] = (VWri[7][10][2]+(.01*c10));
        Vpk[8][9][0] = (3.3*c9);
        Vpk[9][9][2] = -3.3;
        Vpk[10][10][0] = .01;
        vpkflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   97 adds/subtracts/negates
                    158 multiplies
                      0 divides
                    183 assignments
*/
}

void sddoltau(void)
{

/*
Compute effect of loop hinge torques
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
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
        Otk[6][0] = (u[6]*wk[6][1]);
        Otk[6][1] = -(u[6]*wk[6][0]);
        Otk[7][1] = ((Otk[6][1]*c7)-(u[7]*wk[7][2]));
        Otk[7][2] = ((Otk[6][1]*s7)+(u[7]*wk[7][1]));
        Otk[8][0] = (u[8]*wk[8][1]);
        Otk[8][1] = -(u[8]*wk[8][0]);
        Otk[9][1] = ((Otk[8][1]*c9)+(u[9]*wk[9][2]));
        Otk[9][2] = -((Otk[8][1]*s9)+(u[9]*wk[9][1]));
        Otk[10][0] = ((u[10]*wk[10][1])+((Otk[6][0]*c10)+(Otk[7][1]*s10)));
        Otk[10][1] = (((Otk[7][1]*c10)-(Otk[6][0]*s10))-(u[10]*wk[10][0]));
/*
Compute Atk (inertial linear acceleration)
*/
        AiOiWi[6][0] = -(1.5*(u[3]*u[5]));
        AiOiWi[6][1] = -(1.5*(u[4]*u[5]));
        AiOiWi[6][2] = (1.5*((u[3]*u[3])+(u[4]*u[4])));
        Atk[6][0] = ((.75*(Otk[6][1]+(wk[6][0]*wk[6][2])))+((AiOiWi[6][0]*c6)+(
          AiOiWi[6][1]*s6)));
        Atk[6][1] = ((.75*((wk[6][1]*wk[6][2])-Otk[6][0]))+((AiOiWi[6][1]*c6)-(
          AiOiWi[6][0]*s6)));
        Atk[6][2] = (AiOiWi[6][2]-(.75*((wk[6][0]*wk[6][0])+(wk[6][1]*wk[6][1]))
          ));
        AiOiWi[7][0] = (Atk[6][0]-((.75*Otk[6][1])+((.1*(wk[6][0]*wk[6][1]))+(
          .75*(wk[6][0]*wk[6][2])))));
        AiOiWi[7][1] = (Atk[6][1]+((.75*Otk[6][0])+((.1*(wk[6][0]*wk[6][0]))+(
          Wirk[7][0]*wk[6][2]))));
        AiOiWi[7][2] = (Atk[6][2]+(((.75*(wk[6][0]*wk[6][0]))-(Wirk[7][0]*
          wk[6][1]))-(.1*Otk[6][0])));
        Atk[7][0] = (AiOiWi[7][0]+(((.22*(wk[7][0]*wk[7][1]))-(.2*(wk[7][0]*
          wk[7][2])))-((.2*Otk[7][1])+(.22*Otk[7][2]))));
        Atk[7][1] = (((.2*Otk[6][0])+((wk[7][2]*Wkrpk[7][0])-(.22*(wk[7][0]*
          wk[7][0]))))+((AiOiWi[7][1]*c7)-(AiOiWi[7][2]*s7)));
        Atk[7][2] = (((.22*Otk[6][0])+((.2*(wk[7][0]*wk[7][0]))-(wk[7][1]*
          Wkrpk[7][0])))+((AiOiWi[7][1]*s7)+(AiOiWi[7][2]*c7)));
        AiOiWi[8][0] = -(1.2*(u[3]*u[4]));
        AiOiWi[8][1] = (1.2*((u[3]*u[3])+(u[5]*u[5])));
        AiOiWi[8][2] = -(1.2*(u[4]*u[5]));
        Atk[8][0] = ((AiOiWi[8][0]*c8)+(AiOiWi[8][1]*s8));
        Atk[8][1] = ((AiOiWi[8][1]*c8)-(AiOiWi[8][0]*s8));
        Atk[9][0] = (Atk[8][0]+(3.3*(Otk[9][2]-(wk[9][0]*wk[9][1]))));
        Atk[9][1] = ((3.3*((wk[9][0]*wk[9][0])+(wk[9][2]*wk[9][2])))+((
          AiOiWi[8][2]*s9)+(Atk[8][1]*c9)));
        Atk[9][2] = (((AiOiWi[8][2]*c9)-(Atk[8][1]*s9))-(3.3*(Otk[8][0]+(
          wk[9][1]*wk[9][2]))));
        AiOiWi[10][0] = (Atk[7][0]+((Wirk[10][2]*wk[7][1])-(Otk[7][2]*ri[4][1]))
          );
        AiOiWi[10][1] = (Atk[7][1]+((Wirk[10][0]*wk[7][2])-(Wirk[10][2]*wk[7][0]
          )));
        AiOiWi[10][2] = (Atk[7][2]+((Otk[6][0]*ri[4][1])-(Wirk[10][0]*wk[7][1]))
          );
        Atk[10][0] = ((.01*(Otk[7][2]-(wk[10][0]*wk[10][1])))+((AiOiWi[10][0]*
          c10)+(AiOiWi[10][1]*s10)));
        Atk[10][1] = ((.01*((wk[10][0]*wk[10][0])+(wk[10][2]*wk[10][2])))+((
          AiOiWi[10][1]*c10)-(AiOiWi[10][0]*s10)));
        Atk[10][2] = (AiOiWi[10][2]-(.01*(Otk[10][0]+(wk[10][1]*wk[10][2]))));
        inerflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   70 adds/subtracts/negates
                    106 multiplies
                      0 divides
                     36 assignments
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
        Fstar[6][0] = ((6.8*Atk[6][0])-ufk[1][0]);
        Fstar[6][1] = ((6.8*Atk[6][1])-ufk[1][1]);
        Fstar[6][2] = ((6.8*Atk[6][2])-ufk[1][2]);
        Fstar[7][0] = ((57.5*Atk[7][0])-ufk[2][0]);
        Fstar[7][1] = ((57.5*Atk[7][1])-ufk[2][1]);
        Fstar[7][2] = ((57.5*Atk[7][2])-ufk[2][2]);
        Fstar[9][0] = ((10.7*Atk[9][0])-ufk[3][0]);
        Fstar[9][1] = ((10.7*Atk[9][1])-ufk[3][1]);
        Fstar[9][2] = ((10.7*Atk[9][2])-ufk[3][2]);
        Fstar[10][0] = (Atk[10][0]-ufk[4][0]);
        Fstar[10][1] = (Atk[10][1]-ufk[4][1]);
        Fstar[10][2] = (Atk[10][2]-ufk[4][2]);
/*
Compute Tstar (torques)
*/
        Tstar[5][0] = (WkIkWk[5][0]-utk[0][0]);
        Tstar[5][1] = (WkIkWk[5][1]-utk[0][1]);
        Tstar[5][2] = (WkIkWk[5][2]-utk[0][2]);
        Tstar[6][0] = ((WkIkWk[6][0]+(.35*Otk[6][0]))-utk[1][0]);
        Tstar[6][1] = ((WkIkWk[6][1]+(.35*Otk[6][1]))-utk[1][1]);
        Tstar[7][0] = ((WkIkWk[7][0]+(((.41*Otk[7][1])+(4.85*Otk[6][0]))-(.07*
          Otk[7][2])))-utk[2][0]);
        Tstar[7][1] = ((WkIkWk[7][1]+((.54*Otk[7][2])+((.41*Otk[6][0])+(2.2*
          Otk[7][1]))))-utk[2][1]);
        Tstar[7][2] = ((WkIkWk[7][2]+((5.5*Otk[7][2])+((.54*Otk[7][1])-(.07*
          Otk[6][0]))))-utk[2][2]);
        Tstar[9][0] = ((WkIkWk[9][0]+(27.2*Otk[8][0]))-utk[3][0]);
        Tstar[9][1] = ((.2*Otk[9][1])-utk[3][1]);
        Tstar[9][2] = ((WkIkWk[9][2]+(27.2*Otk[9][2]))-utk[3][2]);
        Tstar[10][0] = ((WkIkWk[10][0]+(.1*Otk[10][0]))-utk[4][0]);
        Tstar[10][1] = ((WkIkWk[10][1]+(.1*Otk[10][1]))-utk[4][1]);
        Tstar[10][2] = ((.5*Otk[7][2])-utk[4][2]);
/*
Compute fs0 (RHS ignoring constraints)
*/
        sddovpk();
        temp[0] = (((Fstar[9][2]*Vpk[0][9][2])+((Fstar[9][0]*Vpk[0][8][0])+(
          Fstar[9][1]*Vpk[0][9][1])))+(((Fstar[7][2]*Vpk[0][7][2])+((Fstar[7][0]
          *Vpk[0][6][0])+(Fstar[7][1]*Vpk[0][7][1])))+(((Cik[3][0][2]*
          Fstar[6][2])+((Fstar[6][0]*Vpk[0][6][0])+(Fstar[6][1]*Vpk[0][6][1])))-
          ((Cik[3][0][2]*ufk[0][2])+((Cik[3][0][0]*ufk[0][0])+(Cik[3][0][1]*
          ufk[0][1]))))));
        fs0[0] = (utau[0]-(((Fstar[10][2]*Vpk[0][7][2])+((Fstar[10][0]*
          Vpk[0][10][0])+(Fstar[10][1]*Vpk[0][10][1])))+temp[0]));
        temp[0] = (((Fstar[9][2]*Vpk[1][9][2])+((Fstar[9][0]*Vpk[1][8][0])+(
          Fstar[9][1]*Vpk[1][9][1])))+(((Fstar[7][2]*Vpk[1][7][2])+((Fstar[7][0]
          *Vpk[1][6][0])+(Fstar[7][1]*Vpk[1][7][1])))+(((Cik[3][1][2]*
          Fstar[6][2])+((Fstar[6][0]*Vpk[1][6][0])+(Fstar[6][1]*Vpk[1][6][1])))-
          ((Cik[3][1][2]*ufk[0][2])+((Cik[3][1][0]*ufk[0][0])+(Cik[3][1][1]*
          ufk[0][1]))))));
        fs0[1] = (utau[1]-(((Fstar[10][2]*Vpk[1][7][2])+((Fstar[10][0]*
          Vpk[1][10][0])+(Fstar[10][1]*Vpk[1][10][1])))+temp[0]));
        temp[0] = (((Fstar[9][2]*Vpk[2][9][2])+((Fstar[9][0]*Vpk[2][8][0])+(
          Fstar[9][1]*Vpk[2][9][1])))+(((Fstar[7][2]*Vpk[2][7][2])+((Fstar[7][0]
          *Vpk[2][6][0])+(Fstar[7][1]*Vpk[2][7][1])))+(((Cik[3][2][2]*
          Fstar[6][2])+((Fstar[6][0]*Vpk[2][6][0])+(Fstar[6][1]*Vpk[2][6][1])))-
          ((Cik[3][2][2]*ufk[0][2])+((Cik[3][2][0]*ufk[0][0])+(Cik[3][2][1]*
          ufk[0][1]))))));
        fs0[2] = (utau[2]-(((Fstar[10][2]*Vpk[2][7][2])+((Fstar[10][0]*
          Vpk[2][10][0])+(Fstar[10][1]*Vpk[2][10][1])))+temp[0]));
        temp[0] = (((Tstar[5][0]+((.75*((Fstar[6][0]*s6)+(Fstar[6][1]*c6)))+((
          Tstar[6][0]*c6)-(Tstar[6][1]*s6))))+(((Fstar[7][2]*Vpk[3][7][2])+((
          Fstar[7][0]*Vpk[3][7][0])+(Fstar[7][1]*Vpk[3][7][1])))+((Tstar[7][2]*
          Wpk[3][7][2])+((Tstar[7][0]*c6)+(Tstar[7][1]*Wpk[3][7][1])))))+(((
          Fstar[9][2]*Vpk[3][9][2])+((3.3*(Fstar[9][0]*Wpk[3][9][2]))-(1.2*(
          Fstar[9][1]*s9))))+((Tstar[9][2]*Wpk[3][9][2])+((Tstar[9][0]*c8)+(
          Tstar[9][1]*Wpk[3][9][1])))));
        fs0[3] = (utau[3]-((((Fstar[10][2]*Vpk[3][10][2])+((Fstar[10][0]*
          Vpk[3][10][0])+(Fstar[10][1]*Vpk[3][10][1])))+((Tstar[10][2]*
          Wpk[3][7][2])+((Tstar[10][0]*Wpk[3][10][0])+(Tstar[10][1]*
          Wpk[3][10][1]))))+temp[0]));
        temp[0] = (((3.3*((Fstar[9][0]*Wpk[4][9][2])-(Fstar[9][2]*s8)))+((
          Tstar[9][2]*Wpk[4][9][2])+((Tstar[9][0]*s8)+(Tstar[9][1]*Wpk[4][9][1])
          )))+((Tstar[5][1]+((.75*((Fstar[6][1]*s6)-(Fstar[6][0]*c6)))+((
          Tstar[6][0]*s6)+(Tstar[6][1]*c6))))+(((Fstar[7][2]*Vpk[4][7][2])+((
          Fstar[7][0]*Vpk[4][7][0])+(Fstar[7][1]*Vpk[4][7][1])))+((Tstar[7][2]*
          Wpk[4][7][2])+((Tstar[7][0]*s6)+(Tstar[7][1]*Wpk[4][7][1]))))));
        fs0[4] = (utau[4]-(temp[0]+(((Fstar[10][2]*Vpk[4][10][2])+((Fstar[10][0]
          *Vpk[4][10][0])+(Fstar[10][1]*Vpk[4][10][1])))+((Tstar[10][2]*
          Wpk[4][7][2])+((Tstar[10][0]*Wpk[4][10][0])+(Tstar[10][1]*
          Wpk[4][10][1]))))));
        fs0[5] = (utau[5]-((((Tstar[5][2]-utk[1][2])+((Fstar[7][0]*Vpk[5][7][0])
          +((Tstar[7][2]*c7)-(Tstar[7][1]*s7))))+(((Fstar[9][2]*Vpk[5][9][2])+((
          Fstar[9][0]*Vpk[5][9][0])+(Fstar[9][1]*Vpk[5][9][1])))+((Tstar[9][1]*
          s9)+(Tstar[9][2]*c9))))+(((Tstar[10][2]*c7)+((Tstar[10][0]*
          Wpk[5][10][0])+(Tstar[10][1]*Wpk[5][10][1])))+(((Fstar[10][0]*
          Vpk[5][10][0])+(Fstar[10][1]*Vpk[5][10][1]))-(.01*(Fstar[10][2]*
          Wpk[5][10][0]))))));
        fs0[6] = (utau[6]-((((Fstar[7][0]*Vpk[6][7][0])+((Tstar[7][2]*c7)-(
          Tstar[7][1]*s7)))-utk[1][2])+(((Tstar[10][2]*c7)+((Tstar[10][0]*
          Wpk[6][10][0])+(Tstar[10][1]*Wpk[6][10][1])))+(((Fstar[10][0]*
          Vpk[6][10][0])+(Fstar[10][1]*Vpk[6][10][1]))-(.01*(Fstar[10][2]*
          Wpk[6][10][0]))))));
        fs0[7] = (utau[7]-((((Fstar[10][2]*Vpk[7][10][2])-(.2*((Fstar[10][0]*s10
          )+(Fstar[10][1]*c10))))+((Tstar[10][1]*s10)-(Tstar[10][0]*c10)))-(
          Tstar[7][0]+((.2*Fstar[7][1])+(.22*Fstar[7][2])))));
        fs0[8] = (utau[8]-((3.3*(Fstar[9][0]*c9))+((Tstar[9][1]*s9)+(Tstar[9][2]
          *c9))));
        fs0[9] = (utau[9]-(Tstar[9][0]-(3.3*Fstar[9][2])));
        fs0[10] = (utau[10]-(Tstar[10][2]+(.01*Fstar[10][0])));
        fs0flg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  172 adds/subtracts/negates
                    158 multiplies
                      0 divides
                     42 assignments
*/
}

void sddomm(int routine)
{
    int dumroutine,errnum;
    int i;

    if (mmflg == 0) {
/*
Compute mass matrix (MM)
*/
        sddovpk();
        IkWpk[3][7][0] = (((.41*Wpk[3][7][1])+(4.85*c6))-(.07*Wpk[3][7][2]));
        IkWpk[3][7][1] = ((.54*Wpk[3][7][2])+((.41*c6)+(2.2*Wpk[3][7][1])));
        IkWpk[3][7][2] = ((5.5*Wpk[3][7][2])+((.54*Wpk[3][7][1])-(.07*c6)));
        IkWpk[4][7][0] = (((.41*Wpk[4][7][1])+(4.85*s6))-(.07*Wpk[4][7][2]));
        IkWpk[4][7][1] = ((.54*Wpk[4][7][2])+((.41*s6)+(2.2*Wpk[4][7][1])));
        IkWpk[4][7][2] = ((5.5*Wpk[4][7][2])+((.54*Wpk[4][7][1])-(.07*s6)));
        IkWpk[5][7][0] = -((.07*c7)+(.41*s7));
        IkWpk[5][7][1] = ((.54*c7)-(2.2*s7));
        IkWpk[5][7][2] = ((5.5*c7)-(.54*s7));
        IkWpk[6][7][0] = -((.07*c7)+(.41*s7));
        IkWpk[6][7][1] = ((.54*c7)-(2.2*s7));
        IkWpk[6][7][2] = ((5.5*c7)-(.54*s7));
        temp[0] = ((57.5*((Vpk[0][7][2]*Vpk[0][7][2])+((Vpk[0][6][0]*
          Vpk[0][6][0])+(Vpk[0][7][1]*Vpk[0][7][1]))))+((6.8*((Cik[3][0][2]*
          Cik[3][0][2])+((Vpk[0][6][0]*Vpk[0][6][0])+(Vpk[0][6][1]*Vpk[0][6][1])
          )))+(410.*((Cik[3][0][2]*Cik[3][0][2])+((Cik[3][0][0]*Cik[3][0][0])+(
          Cik[3][0][1]*Cik[3][0][1]))))));
        mm[0][0] = (((10.7*((Vpk[0][9][2]*Vpk[0][9][2])+((Vpk[0][8][0]*
          Vpk[0][8][0])+(Vpk[0][9][1]*Vpk[0][9][1]))))+temp[0])+((Vpk[0][7][2]*
          Vpk[0][7][2])+((Vpk[0][10][0]*Vpk[0][10][0])+(Vpk[0][10][1]*
          Vpk[0][10][1]))));
        temp[0] = ((57.5*((Vpk[0][7][2]*Vpk[1][7][2])+((Vpk[0][6][0]*
          Vpk[1][6][0])+(Vpk[0][7][1]*Vpk[1][7][1]))))+((6.8*((Cik[3][0][2]*
          Cik[3][1][2])+((Vpk[0][6][0]*Vpk[1][6][0])+(Vpk[0][6][1]*Vpk[1][6][1])
          )))+(410.*((Cik[3][0][2]*Cik[3][1][2])+((Cik[3][0][0]*Cik[3][1][0])+(
          Cik[3][0][1]*Cik[3][1][1]))))));
        mm[0][1] = (((10.7*((Vpk[0][9][2]*Vpk[1][9][2])+((Vpk[0][8][0]*
          Vpk[1][8][0])+(Vpk[0][9][1]*Vpk[1][9][1]))))+temp[0])+((Vpk[0][7][2]*
          Vpk[1][7][2])+((Vpk[0][10][0]*Vpk[1][10][0])+(Vpk[0][10][1]*
          Vpk[1][10][1]))));
        temp[0] = ((57.5*((Vpk[0][7][2]*Vpk[2][7][2])+((Vpk[0][6][0]*
          Vpk[2][6][0])+(Vpk[0][7][1]*Vpk[2][7][1]))))+((6.8*((Cik[3][0][2]*
          Cik[3][2][2])+((Vpk[0][6][0]*Vpk[2][6][0])+(Vpk[0][6][1]*Vpk[2][6][1])
          )))+(410.*((Cik[3][0][2]*Cik[3][2][2])+((Cik[3][0][0]*Cik[3][2][0])+(
          Cik[3][0][1]*Cik[3][2][1]))))));
        mm[0][2] = (((10.7*((Vpk[0][9][2]*Vpk[2][9][2])+((Vpk[0][8][0]*
          Vpk[2][8][0])+(Vpk[0][9][1]*Vpk[2][9][1]))))+temp[0])+((Vpk[0][7][2]*
          Vpk[2][7][2])+((Vpk[0][10][0]*Vpk[2][10][0])+(Vpk[0][10][1]*
          Vpk[2][10][1]))));
        mm[0][3] = (((10.7*((Vpk[0][9][2]*Vpk[3][9][2])+((3.3*(Vpk[0][8][0]*
          Wpk[3][9][2]))-(1.2*(Vpk[0][9][1]*s9)))))+((5.1*((Vpk[0][6][0]*s6)+(
          Vpk[0][6][1]*c6)))+(57.5*((Vpk[0][7][2]*Vpk[3][7][2])+((Vpk[0][6][0]*
          Vpk[3][7][0])+(Vpk[0][7][1]*Vpk[3][7][1]))))))+((Vpk[0][7][2]*
          Vpk[3][10][2])+((Vpk[0][10][0]*Vpk[3][10][0])+(Vpk[0][10][1]*
          Vpk[3][10][1]))));
        mm[0][4] = (((35.31*((Vpk[0][8][0]*Wpk[4][9][2])-(Vpk[0][9][2]*s8)))+((
          5.1*((Vpk[0][6][1]*s6)-(Vpk[0][6][0]*c6)))+(57.5*((Vpk[0][7][2]*
          Vpk[4][7][2])+((Vpk[0][6][0]*Vpk[4][7][0])+(Vpk[0][7][1]*Vpk[4][7][1])
          )))))+((Vpk[0][7][2]*Vpk[4][10][2])+((Vpk[0][10][0]*Vpk[4][10][0])+(
          Vpk[0][10][1]*Vpk[4][10][1]))));
        mm[0][5] = (((10.7*((Vpk[0][9][2]*Vpk[5][9][2])+((Vpk[0][8][0]*
          Vpk[5][9][0])+(Vpk[0][9][1]*Vpk[5][9][1]))))+(57.5*(Vpk[0][6][0]*
          Vpk[5][7][0])))+(((Vpk[0][10][0]*Vpk[5][10][0])+(Vpk[0][10][1]*
          Vpk[5][10][1]))-(.01*(Vpk[0][7][2]*Wpk[5][10][0]))));
        mm[0][6] = ((57.5*(Vpk[0][6][0]*Vpk[6][7][0]))+(((Vpk[0][10][0]*
          Vpk[6][10][0])+(Vpk[0][10][1]*Vpk[6][10][1]))-(.01*(Vpk[0][7][2]*
          Wpk[6][10][0]))));
        mm[0][7] = (((Vpk[0][7][2]*Vpk[7][10][2])-(.2*((Vpk[0][10][0]*s10)+(
          Vpk[0][10][1]*c10))))-(57.5*((.2*Vpk[0][7][1])+(.22*Vpk[0][7][2]))));
        mm[0][8] = (35.31*(Vpk[0][8][0]*c9));
        mm[0][9] = -(35.31*Vpk[0][9][2]);
        mm[0][10] = (.01*Vpk[0][10][0]);
        temp[0] = ((57.5*((Vpk[1][7][2]*Vpk[1][7][2])+((Vpk[1][6][0]*
          Vpk[1][6][0])+(Vpk[1][7][1]*Vpk[1][7][1]))))+((6.8*((Cik[3][1][2]*
          Cik[3][1][2])+((Vpk[1][6][0]*Vpk[1][6][0])+(Vpk[1][6][1]*Vpk[1][6][1])
          )))+(410.*((Cik[3][1][2]*Cik[3][1][2])+((Cik[3][1][0]*Cik[3][1][0])+(
          Cik[3][1][1]*Cik[3][1][1]))))));
        mm[1][1] = (((10.7*((Vpk[1][9][2]*Vpk[1][9][2])+((Vpk[1][8][0]*
          Vpk[1][8][0])+(Vpk[1][9][1]*Vpk[1][9][1]))))+temp[0])+((Vpk[1][7][2]*
          Vpk[1][7][2])+((Vpk[1][10][0]*Vpk[1][10][0])+(Vpk[1][10][1]*
          Vpk[1][10][1]))));
        temp[0] = ((57.5*((Vpk[1][7][2]*Vpk[2][7][2])+((Vpk[1][6][0]*
          Vpk[2][6][0])+(Vpk[1][7][1]*Vpk[2][7][1]))))+((6.8*((Cik[3][1][2]*
          Cik[3][2][2])+((Vpk[1][6][0]*Vpk[2][6][0])+(Vpk[1][6][1]*Vpk[2][6][1])
          )))+(410.*((Cik[3][1][2]*Cik[3][2][2])+((Cik[3][1][0]*Cik[3][2][0])+(
          Cik[3][1][1]*Cik[3][2][1]))))));
        mm[1][2] = (((10.7*((Vpk[1][9][2]*Vpk[2][9][2])+((Vpk[1][8][0]*
          Vpk[2][8][0])+(Vpk[1][9][1]*Vpk[2][9][1]))))+temp[0])+((Vpk[1][7][2]*
          Vpk[2][7][2])+((Vpk[1][10][0]*Vpk[2][10][0])+(Vpk[1][10][1]*
          Vpk[2][10][1]))));
        mm[1][3] = (((10.7*((Vpk[1][9][2]*Vpk[3][9][2])+((3.3*(Vpk[1][8][0]*
          Wpk[3][9][2]))-(1.2*(Vpk[1][9][1]*s9)))))+((5.1*((Vpk[1][6][0]*s6)+(
          Vpk[1][6][1]*c6)))+(57.5*((Vpk[1][7][2]*Vpk[3][7][2])+((Vpk[1][6][0]*
          Vpk[3][7][0])+(Vpk[1][7][1]*Vpk[3][7][1]))))))+((Vpk[1][7][2]*
          Vpk[3][10][2])+((Vpk[1][10][0]*Vpk[3][10][0])+(Vpk[1][10][1]*
          Vpk[3][10][1]))));
        mm[1][4] = (((35.31*((Vpk[1][8][0]*Wpk[4][9][2])-(Vpk[1][9][2]*s8)))+((
          5.1*((Vpk[1][6][1]*s6)-(Vpk[1][6][0]*c6)))+(57.5*((Vpk[1][7][2]*
          Vpk[4][7][2])+((Vpk[1][6][0]*Vpk[4][7][0])+(Vpk[1][7][1]*Vpk[4][7][1])
          )))))+((Vpk[1][7][2]*Vpk[4][10][2])+((Vpk[1][10][0]*Vpk[4][10][0])+(
          Vpk[1][10][1]*Vpk[4][10][1]))));
        mm[1][5] = (((10.7*((Vpk[1][9][2]*Vpk[5][9][2])+((Vpk[1][8][0]*
          Vpk[5][9][0])+(Vpk[1][9][1]*Vpk[5][9][1]))))+(57.5*(Vpk[1][6][0]*
          Vpk[5][7][0])))+(((Vpk[1][10][0]*Vpk[5][10][0])+(Vpk[1][10][1]*
          Vpk[5][10][1]))-(.01*(Vpk[1][7][2]*Wpk[5][10][0]))));
        mm[1][6] = ((57.5*(Vpk[1][6][0]*Vpk[6][7][0]))+(((Vpk[1][10][0]*
          Vpk[6][10][0])+(Vpk[1][10][1]*Vpk[6][10][1]))-(.01*(Vpk[1][7][2]*
          Wpk[6][10][0]))));
        mm[1][7] = (((Vpk[1][7][2]*Vpk[7][10][2])-(.2*((Vpk[1][10][0]*s10)+(
          Vpk[1][10][1]*c10))))-(57.5*((.2*Vpk[1][7][1])+(.22*Vpk[1][7][2]))));
        mm[1][8] = (35.31*(Vpk[1][8][0]*c9));
        mm[1][9] = -(35.31*Vpk[1][9][2]);
        mm[1][10] = (.01*Vpk[1][10][0]);
        temp[0] = ((57.5*((Vpk[2][7][2]*Vpk[2][7][2])+((Vpk[2][6][0]*
          Vpk[2][6][0])+(Vpk[2][7][1]*Vpk[2][7][1]))))+((6.8*((Cik[3][2][2]*
          Cik[3][2][2])+((Vpk[2][6][0]*Vpk[2][6][0])+(Vpk[2][6][1]*Vpk[2][6][1])
          )))+(410.*((Cik[3][2][2]*Cik[3][2][2])+((Cik[3][2][0]*Cik[3][2][0])+(
          Cik[3][2][1]*Cik[3][2][1]))))));
        mm[2][2] = (((10.7*((Vpk[2][9][2]*Vpk[2][9][2])+((Vpk[2][8][0]*
          Vpk[2][8][0])+(Vpk[2][9][1]*Vpk[2][9][1]))))+temp[0])+((Vpk[2][7][2]*
          Vpk[2][7][2])+((Vpk[2][10][0]*Vpk[2][10][0])+(Vpk[2][10][1]*
          Vpk[2][10][1]))));
        mm[2][3] = (((10.7*((Vpk[2][9][2]*Vpk[3][9][2])+((3.3*(Vpk[2][8][0]*
          Wpk[3][9][2]))-(1.2*(Vpk[2][9][1]*s9)))))+((5.1*((Vpk[2][6][0]*s6)+(
          Vpk[2][6][1]*c6)))+(57.5*((Vpk[2][7][2]*Vpk[3][7][2])+((Vpk[2][6][0]*
          Vpk[3][7][0])+(Vpk[2][7][1]*Vpk[3][7][1]))))))+((Vpk[2][7][2]*
          Vpk[3][10][2])+((Vpk[2][10][0]*Vpk[3][10][0])+(Vpk[2][10][1]*
          Vpk[3][10][1]))));
        mm[2][4] = (((35.31*((Vpk[2][8][0]*Wpk[4][9][2])-(Vpk[2][9][2]*s8)))+((
          5.1*((Vpk[2][6][1]*s6)-(Vpk[2][6][0]*c6)))+(57.5*((Vpk[2][7][2]*
          Vpk[4][7][2])+((Vpk[2][6][0]*Vpk[4][7][0])+(Vpk[2][7][1]*Vpk[4][7][1])
          )))))+((Vpk[2][7][2]*Vpk[4][10][2])+((Vpk[2][10][0]*Vpk[4][10][0])+(
          Vpk[2][10][1]*Vpk[4][10][1]))));
        mm[2][5] = (((10.7*((Vpk[2][9][2]*Vpk[5][9][2])+((Vpk[2][8][0]*
          Vpk[5][9][0])+(Vpk[2][9][1]*Vpk[5][9][1]))))+(57.5*(Vpk[2][6][0]*
          Vpk[5][7][0])))+(((Vpk[2][10][0]*Vpk[5][10][0])+(Vpk[2][10][1]*
          Vpk[5][10][1]))-(.01*(Vpk[2][7][2]*Wpk[5][10][0]))));
        mm[2][6] = ((57.5*(Vpk[2][6][0]*Vpk[6][7][0]))+(((Vpk[2][10][0]*
          Vpk[6][10][0])+(Vpk[2][10][1]*Vpk[6][10][1]))-(.01*(Vpk[2][7][2]*
          Wpk[6][10][0]))));
        mm[2][7] = (((Vpk[2][7][2]*Vpk[7][10][2])-(.2*((Vpk[2][10][0]*s10)+(
          Vpk[2][10][1]*c10))))-(57.5*((.2*Vpk[2][7][1])+(.22*Vpk[2][7][2]))));
        mm[2][8] = (35.31*(Vpk[2][8][0]*c9));
        mm[2][9] = -(35.31*Vpk[2][9][2]);
        mm[2][10] = (.01*Vpk[2][10][0]);
        temp[0] = (((10.7*((Vpk[3][9][2]*Vpk[3][9][2])+((1.44*(s9*s9))+(10.89*(
          Wpk[3][9][2]*Wpk[3][9][2])))))+((27.2*(Wpk[3][9][2]*Wpk[3][9][2]))+((
          .2*(Wpk[3][9][1]*Wpk[3][9][1]))+(27.2*(c8*c8)))))+((57.5*((
          Vpk[3][7][2]*Vpk[3][7][2])+((Vpk[3][7][0]*Vpk[3][7][0])+(Vpk[3][7][1]*
          Vpk[3][7][1]))))+((IkWpk[3][7][2]*Wpk[3][7][2])+((IkWpk[3][7][0]*c6)+(
          IkWpk[3][7][1]*Wpk[3][7][1])))));
        mm[3][3] = (119.175+((((.1*((Wpk[3][10][0]*Wpk[3][10][0])+(Wpk[3][10][1]
          *Wpk[3][10][1])))+(.5*(Wpk[3][7][2]*Wpk[3][7][2])))+((Vpk[3][10][2]*
          Vpk[3][10][2])+((Vpk[3][10][0]*Vpk[3][10][0])+(Vpk[3][10][1]*
          Vpk[3][10][1]))))+temp[0]));
        temp[0] = (((10.7*((10.89*(Wpk[3][9][2]*Wpk[4][9][2]))-(3.3*(
          Vpk[3][9][2]*s8))))+((27.2*(Wpk[3][9][2]*Wpk[4][9][2]))+((.2*(
          Wpk[3][9][1]*Wpk[4][9][1]))+(27.2*(s8*c8)))))+(((57.5*((Vpk[3][7][2]*
          Vpk[4][7][2])+((Vpk[3][7][0]*Vpk[4][7][0])+(Vpk[3][7][1]*Vpk[4][7][1])
          )))+((IkWpk[4][7][2]*Wpk[3][7][2])+((IkWpk[4][7][0]*c6)+(
          IkWpk[4][7][1]*Wpk[3][7][1]))))-14.));
        mm[3][4] = ((((.1*((Wpk[3][10][0]*Wpk[4][10][0])+(Wpk[3][10][1]*
          Wpk[4][10][1])))+(.5*(Wpk[3][7][2]*Wpk[4][7][2])))+((Vpk[3][10][2]*
          Vpk[4][10][2])+((Vpk[3][10][0]*Vpk[4][10][0])+(Vpk[3][10][1]*
          Vpk[4][10][1]))))+temp[0]);
        temp[0] = (((10.7*((Vpk[3][9][2]*Vpk[5][9][2])+((3.3*(Vpk[5][9][0]*
          Wpk[3][9][2]))-(1.2*(Vpk[5][9][1]*s9)))))+((.2*(Wpk[3][9][1]*s9))+(
          27.2*(Wpk[3][9][2]*c9))))+((57.5*(Vpk[3][7][0]*Vpk[5][7][0]))+((
          IkWpk[5][7][2]*Wpk[3][7][2])+((IkWpk[5][7][0]*c6)+(IkWpk[5][7][1]*
          Wpk[3][7][1])))));
        mm[3][5] = (14.+((((.1*((Wpk[3][10][0]*Wpk[5][10][0])+(Wpk[3][10][1]*
          Wpk[5][10][1])))+(.5*(Wpk[3][7][2]*c7)))+(((Vpk[3][10][0]*
          Vpk[5][10][0])+(Vpk[3][10][1]*Vpk[5][10][1]))-(.01*(Vpk[3][10][2]*
          Wpk[5][10][0]))))+temp[0]));
        mm[3][6] = (((57.5*(Vpk[3][7][0]*Vpk[6][7][0]))+((IkWpk[6][7][2]*
          Wpk[3][7][2])+((IkWpk[6][7][0]*c6)+(IkWpk[6][7][1]*Wpk[3][7][1]))))+((
          (.1*((Wpk[3][10][0]*Wpk[6][10][0])+(Wpk[3][10][1]*Wpk[6][10][1])))+(.5
          *(Wpk[3][7][2]*c7)))+(((Vpk[3][10][0]*Vpk[6][10][0])+(Vpk[3][10][1]*
          Vpk[6][10][1]))-(.01*(Vpk[3][10][2]*Wpk[6][10][0])))));
        mm[3][7] = (((.1*((Wpk[3][10][1]*s10)-(Wpk[3][10][0]*c10)))+((
          Vpk[3][10][2]*Vpk[7][10][2])-(.2*((Vpk[3][10][0]*s10)+(Vpk[3][10][1]*
          c10)))))+(((.07*Wpk[3][7][2])-((.41*Wpk[3][7][1])+(4.85*c6)))-(57.5*((
          .2*Vpk[3][7][1])+(.22*Vpk[3][7][2])))));
        mm[3][8] = ((116.523*(Wpk[3][9][2]*c9))+((.2*(Wpk[3][9][1]*s9))+(27.2*(
          Wpk[3][9][2]*c9))));
        mm[3][9] = ((27.2*c8)-(35.31*Vpk[3][9][2]));
        mm[3][10] = ((.01*Vpk[3][10][0])+(.5*Wpk[3][7][2]));
        temp[0] = (((57.5*((Vpk[4][7][2]*Vpk[4][7][2])+((Vpk[4][7][0]*
          Vpk[4][7][0])+(Vpk[4][7][1]*Vpk[4][7][1]))))+((IkWpk[4][7][2]*
          Wpk[4][7][2])+((IkWpk[4][7][0]*s6)+(IkWpk[4][7][1]*Wpk[4][7][1]))))+((
          116.523*((Wpk[4][9][2]*Wpk[4][9][2])+(s8*s8)))+((27.2*(Wpk[4][9][2]*
          Wpk[4][9][2]))+((.2*(Wpk[4][9][1]*Wpk[4][9][1]))+(27.2*(s8*s8))))));
        mm[4][4] = (320.175+((((.1*((Wpk[4][10][0]*Wpk[4][10][0])+(Wpk[4][10][1]
          *Wpk[4][10][1])))+(.5*(Wpk[4][7][2]*Wpk[4][7][2])))+((Vpk[4][10][2]*
          Vpk[4][10][2])+((Vpk[4][10][0]*Vpk[4][10][0])+(Vpk[4][10][1]*
          Vpk[4][10][1]))))+temp[0]));
        temp[0] = (((35.31*((Vpk[5][9][0]*Wpk[4][9][2])-(Vpk[5][9][2]*s8)))+((.2
          *(Wpk[4][9][1]*s9))+(27.2*(Wpk[4][9][2]*c9))))+(((57.5*(Vpk[4][7][0]*
          Vpk[5][7][0]))+((IkWpk[5][7][2]*Wpk[4][7][2])+((IkWpk[5][7][0]*s6)+(
          IkWpk[5][7][1]*Wpk[4][7][1]))))-34.6));
        mm[4][5] = ((((.1*((Wpk[4][10][0]*Wpk[5][10][0])+(Wpk[4][10][1]*
          Wpk[5][10][1])))+(.5*(Wpk[4][7][2]*c7)))+(((Vpk[4][10][0]*
          Vpk[5][10][0])+(Vpk[4][10][1]*Vpk[5][10][1]))-(.01*(Vpk[4][10][2]*
          Wpk[5][10][0]))))+temp[0]);
        mm[4][6] = (((57.5*(Vpk[4][7][0]*Vpk[6][7][0]))+((IkWpk[6][7][2]*
          Wpk[4][7][2])+((IkWpk[6][7][0]*s6)+(IkWpk[6][7][1]*Wpk[4][7][1]))))+((
          (.1*((Wpk[4][10][0]*Wpk[6][10][0])+(Wpk[4][10][1]*Wpk[6][10][1])))+(.5
          *(Wpk[4][7][2]*c7)))+(((Vpk[4][10][0]*Vpk[6][10][0])+(Vpk[4][10][1]*
          Vpk[6][10][1]))-(.01*(Vpk[4][10][2]*Wpk[6][10][0])))));
        mm[4][7] = (((.1*((Wpk[4][10][1]*s10)-(Wpk[4][10][0]*c10)))+((
          Vpk[4][10][2]*Vpk[7][10][2])-(.2*((Vpk[4][10][0]*s10)+(Vpk[4][10][1]*
          c10)))))+(((.07*Wpk[4][7][2])-((.41*Wpk[4][7][1])+(4.85*s6)))-(57.5*((
          .2*Vpk[4][7][1])+(.22*Vpk[4][7][2])))));
        mm[4][8] = ((116.523*(Wpk[4][9][2]*c9))+((.2*(Wpk[4][9][1]*s9))+(27.2*(
          Wpk[4][9][2]*c9))));
        mm[4][9] = (143.723*s8);
        mm[4][10] = ((.01*Vpk[4][10][0])+(.5*Wpk[4][7][2]));
        mm[5][5] = (440.2+((((.0001*(Wpk[5][10][0]*Wpk[5][10][0]))+((
          Vpk[5][10][0]*Vpk[5][10][0])+(Vpk[5][10][1]*Vpk[5][10][1])))+((.1*((
          Wpk[5][10][0]*Wpk[5][10][0])+(Wpk[5][10][1]*Wpk[5][10][1])))+(.5*(c7*
          c7))))+(((10.7*((Vpk[5][9][2]*Vpk[5][9][2])+((Vpk[5][9][0]*
          Vpk[5][9][0])+(Vpk[5][9][1]*Vpk[5][9][1]))))+(27.*(c9*c9)))+((57.5*(
          Vpk[5][7][0]*Vpk[5][7][0]))+((IkWpk[5][7][2]*c7)-(IkWpk[5][7][1]*s7)))
          )));
        mm[5][6] = (((57.5*(Vpk[5][7][0]*Vpk[6][7][0]))+((IkWpk[6][7][2]*c7)-(
          IkWpk[6][7][1]*s7)))+(((.0001*(Wpk[5][10][0]*Wpk[6][10][0]))+((
          Vpk[5][10][0]*Vpk[6][10][0])+(Vpk[5][10][1]*Vpk[6][10][1])))+((.1*((
          Wpk[5][10][0]*Wpk[6][10][0])+(Wpk[5][10][1]*Wpk[6][10][1])))+(.5*(c7*
          c7)))));
        mm[5][7] = (((.07*c7)+(.41*s7))+((.1*((Wpk[5][10][1]*s10)-(Wpk[5][10][0]
          *c10)))-((.01*(Vpk[7][10][2]*Wpk[5][10][0]))+(.2*((Vpk[5][10][0]*s10)+
          (Vpk[5][10][1]*c10))))));
        mm[5][8] = (.2+((27.*(c9*c9))+(35.31*(Vpk[5][9][0]*c9))));
        mm[5][9] = -(35.31*Vpk[5][9][2]);
        mm[5][10] = ((.01*Vpk[5][10][0])+(.5*c7));
        mm[6][6] = (((57.5*(Vpk[6][7][0]*Vpk[6][7][0]))+((IkWpk[6][7][2]*c7)-(
          IkWpk[6][7][1]*s7)))+(((.0001*(Wpk[6][10][0]*Wpk[6][10][0]))+((
          Vpk[6][10][0]*Vpk[6][10][0])+(Vpk[6][10][1]*Vpk[6][10][1])))+((.1*((
          Wpk[6][10][0]*Wpk[6][10][0])+(Wpk[6][10][1]*Wpk[6][10][1])))+(.5*(c7*
          c7)))));
        mm[6][7] = (((.07*c7)+(.41*s7))+((.1*((Wpk[6][10][1]*s10)-(Wpk[6][10][0]
          *c10)))-((.01*(Vpk[7][10][2]*Wpk[6][10][0]))+(.2*((Vpk[6][10][0]*s10)+
          (Vpk[6][10][1]*c10))))));
        mm[6][8] = 0.;
        mm[6][9] = 0.;
        mm[6][10] = ((.01*Vpk[6][10][0])+(.5*c7));
        mm[7][7] = (10.073+(Vpk[7][10][2]*Vpk[7][10][2]));
        mm[7][8] = 0.;
        mm[7][9] = 0.;
        mm[7][10] = -(.002*s10);
        mm[8][8] = (.2+(143.523*(c9*c9)));
        mm[8][9] = 0.;
        mm[8][10] = 0.;
        mm[9][9] = 143.723;
        mm[9][10] = 0.;
        mm[10][10] = .5001;
/*
Check for singular mass matrix
*/
        for (i = 0; i < 11; i++) {
            if (mm[i][i] < 1e-13) {
                sdseterr(routine,47);
            }
        }
        sderror(&dumroutine,&errnum);
        if (errnum == 0) {
            mmflg = 1;
        }
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  376 adds/subtracts/negates
                    585 multiplies
                      0 divides
                     89 assignments
*/
}

void sddommldu(int routine)
{
    int i;
    int dumroutine,errnum;

    if (mmlduflg == 0) {
        sddomm(routine);
/*
Numerically decompose the mass matrix
*/
        sdldudcomp(11,11,mmap,1e-13,workss,works,mm,mlo,mdi);
/*
Check for singular mass matrix
*/
        for (i = 0; i < 11; i++) {
            if (mdi[i] == 0.) {
                sdseterr(routine,47);
            }
        }
        sderror(&dumroutine,&errnum);
        if (errnum == 0) {
            mmlduflg = 1;
        }
    }
}

void sdlhs(int routine)
{
/* Compute all remaining state- and force-dependent quantities
*/

    roustate = 2;
    sddommldu(routine);
    sddofs0();
}

void sdmfrc(double imult[1])
{
/*
Calculate forces due to constraint multipliers.

*/

    mtau[10] = imult[0];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      1 assignments
*/
}

void sdequivht(double tau[11])
{
/* Compute tree hinge torques to match effect of applied loads
*/
    double fstareq[11][3],tstareq[11][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(56,23);
        return;
    }
/*
Compute fstareq (forces)
*/
/*
Compute tstareq (torques)
*/
/*
Compute taus (RHS ignoring constraints and inertial forces)
*/
    sddovpk();
    temp[0] = (((ufk[3][2]*Vpk[0][9][2])+((ufk[3][0]*Vpk[0][8][0])+(ufk[3][1]*
      Vpk[0][9][1])))+(((ufk[2][2]*Vpk[0][7][2])+((ufk[2][0]*Vpk[0][6][0])+(
      ufk[2][1]*Vpk[0][7][1])))+(((Cik[3][0][2]*ufk[0][2])+((Cik[3][0][0]*
      ufk[0][0])+(Cik[3][0][1]*ufk[0][1])))+((Cik[3][0][2]*ufk[1][2])+((
      ufk[1][0]*Vpk[0][6][0])+(ufk[1][1]*Vpk[0][6][1]))))));
    tau[0] = (utau[0]+(((ufk[4][2]*Vpk[0][7][2])+((ufk[4][0]*Vpk[0][10][0])+(
      ufk[4][1]*Vpk[0][10][1])))+temp[0]));
    temp[0] = (((ufk[3][2]*Vpk[1][9][2])+((ufk[3][0]*Vpk[1][8][0])+(ufk[3][1]*
      Vpk[1][9][1])))+(((ufk[2][2]*Vpk[1][7][2])+((ufk[2][0]*Vpk[1][6][0])+(
      ufk[2][1]*Vpk[1][7][1])))+(((Cik[3][1][2]*ufk[0][2])+((Cik[3][1][0]*
      ufk[0][0])+(Cik[3][1][1]*ufk[0][1])))+((Cik[3][1][2]*ufk[1][2])+((
      ufk[1][0]*Vpk[1][6][0])+(ufk[1][1]*Vpk[1][6][1]))))));
    tau[1] = (utau[1]+(((ufk[4][2]*Vpk[1][7][2])+((ufk[4][0]*Vpk[1][10][0])+(
      ufk[4][1]*Vpk[1][10][1])))+temp[0]));
    temp[0] = (((ufk[3][2]*Vpk[2][9][2])+((ufk[3][0]*Vpk[2][8][0])+(ufk[3][1]*
      Vpk[2][9][1])))+(((ufk[2][2]*Vpk[2][7][2])+((ufk[2][0]*Vpk[2][6][0])+(
      ufk[2][1]*Vpk[2][7][1])))+(((Cik[3][2][2]*ufk[0][2])+((Cik[3][2][0]*
      ufk[0][0])+(Cik[3][2][1]*ufk[0][1])))+((Cik[3][2][2]*ufk[1][2])+((
      ufk[1][0]*Vpk[2][6][0])+(ufk[1][1]*Vpk[2][6][1]))))));
    tau[2] = (utau[2]+(((ufk[4][2]*Vpk[2][7][2])+((ufk[4][0]*Vpk[2][10][0])+(
      ufk[4][1]*Vpk[2][10][1])))+temp[0]));
    temp[0] = (((((1.2*(ufk[3][1]*s9))-(3.3*(ufk[3][0]*Wpk[3][9][2])))-(
      ufk[3][2]*Vpk[3][9][2]))-((utk[3][2]*Wpk[3][9][2])+((utk[3][0]*c8)+(
      utk[3][1]*Wpk[3][9][1]))))+(((((utk[1][1]*s6)-(utk[1][0]*c6))-(.75*((
      ufk[1][0]*s6)+(ufk[1][1]*c6))))-utk[0][0])-(((ufk[2][2]*Vpk[3][7][2])+((
      ufk[2][0]*Vpk[3][7][0])+(ufk[2][1]*Vpk[3][7][1])))+((utk[2][2]*
      Wpk[3][7][2])+((utk[2][0]*c6)+(utk[2][1]*Wpk[3][7][1]))))));
    tau[3] = (utau[3]-(temp[0]-(((ufk[4][2]*Vpk[3][10][2])+((ufk[4][0]*
      Vpk[3][10][0])+(ufk[4][1]*Vpk[3][10][1])))+((utk[4][2]*Wpk[3][7][2])+((
      utk[4][0]*Wpk[3][10][0])+(utk[4][1]*Wpk[3][10][1]))))));
    temp[0] = (((3.3*((ufk[3][2]*s8)-(ufk[3][0]*Wpk[4][9][2])))-((utk[3][2]*
      Wpk[4][9][2])+((utk[3][0]*s8)+(utk[3][1]*Wpk[4][9][1]))))+((((.75*((
      ufk[1][0]*c6)-(ufk[1][1]*s6)))-((utk[1][0]*s6)+(utk[1][1]*c6)))-utk[0][1])
      -(((ufk[2][2]*Vpk[4][7][2])+((ufk[2][0]*Vpk[4][7][0])+(ufk[2][1]*
      Vpk[4][7][1])))+((utk[2][2]*Wpk[4][7][2])+((utk[2][0]*s6)+(utk[2][1]*
      Wpk[4][7][1]))))));
    tau[4] = (utau[4]-(temp[0]-(((ufk[4][2]*Vpk[4][10][2])+((ufk[4][0]*
      Vpk[4][10][0])+(ufk[4][1]*Vpk[4][10][1])))+((utk[4][2]*Wpk[4][7][2])+((
      utk[4][0]*Wpk[4][10][0])+(utk[4][1]*Wpk[4][10][1]))))));
    tau[5] = (utau[5]-((((.01*(ufk[4][2]*Wpk[5][10][0]))-((ufk[4][0]*
      Vpk[5][10][0])+(ufk[4][1]*Vpk[5][10][1])))-((utk[4][2]*c7)+((utk[4][0]*
      Wpk[5][10][0])+(utk[4][1]*Wpk[5][10][1]))))+(((((utk[2][1]*s7)-(utk[2][2]*
      c7))-(ufk[2][0]*Vpk[5][7][0]))-(utk[0][2]+utk[1][2]))-(((ufk[3][2]*
      Vpk[5][9][2])+((ufk[3][0]*Vpk[5][9][0])+(ufk[3][1]*Vpk[5][9][1])))+((
      utk[3][1]*s9)+(utk[3][2]*c9))))));
    tau[6] = (utau[6]-((((.01*(ufk[4][2]*Wpk[6][10][0]))-((ufk[4][0]*
      Vpk[6][10][0])+(ufk[4][1]*Vpk[6][10][1])))-((utk[4][2]*c7)+((utk[4][0]*
      Wpk[6][10][0])+(utk[4][1]*Wpk[6][10][1]))))+((((utk[2][1]*s7)-(utk[2][2]*
      c7))-(ufk[2][0]*Vpk[6][7][0]))-utk[1][2])));
    tau[7] = (utau[7]-((utk[2][0]+((.2*ufk[2][1])+(.22*ufk[2][2])))+(((.2*((
      ufk[4][0]*s10)+(ufk[4][1]*c10)))-(ufk[4][2]*Vpk[7][10][2]))+((utk[4][0]*
      c10)-(utk[4][1]*s10)))));
    tau[8] = (utau[8]+((3.3*(ufk[3][0]*c9))+((utk[3][1]*s9)+(utk[3][2]*c9))));
    tau[9] = (utau[9]-((3.3*ufk[3][2])-utk[3][0]));
    tau[10] = (utau[10]+(utk[4][2]+(.01*ufk[4][0])));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  131 adds/subtracts/negates
                    132 multiplies
                      0 divides
                     16 assignments
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
    fs[6] = fs0[6];
    fs[7] = fs0[7];
    fs[8] = fs0[8];
    fs[9] = fs0[9];
    fs[10] = fs0[10];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
*/
}

void sdfsmult(void)
{

/*
Compute Fs (multiplier-generated forces only)
*/
    sddovpk();
    fs[10] = mtau[10];
    fs[0] = 0.;
    fs[1] = 0.;
    fs[2] = 0.;
    fs[3] = 0.;
    fs[4] = 0.;
    fs[5] = 0.;
    fs[6] = 0.;
    fs[7] = 0.;
    fs[8] = 0.;
    fs[9] = 0.;
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
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
    fs[6] = (fs[6]+fs0[6]);
    fs[7] = (fs[7]+fs0[7]);
    fs[8] = (fs[8]+fs0[8]);
    fs[9] = (fs[9]+fs0[9]);
    fs[10] = (fs[10]+fs0[10]);
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   11 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
*/
}

void sdfsgenmult(void)
{

/*
Compute Fs (generic multiplier-generated forces)
*/
    sddovpk();
    temp[0] = (((mfk[3][2]*Vpk[0][9][2])+((mfk[3][0]*Vpk[0][8][0])+(mfk[3][1]*
      Vpk[0][9][1])))+(((mfk[2][2]*Vpk[0][7][2])+((mfk[2][0]*Vpk[0][6][0])+(
      mfk[2][1]*Vpk[0][7][1])))+(((Cik[3][0][2]*mfk[0][2])+((Cik[3][0][0]*
      mfk[0][0])+(Cik[3][0][1]*mfk[0][1])))+((Cik[3][0][2]*mfk[1][2])+((
      mfk[1][0]*Vpk[0][6][0])+(mfk[1][1]*Vpk[0][6][1]))))));
    fs[0] = (mtau[0]+(((mfk[4][2]*Vpk[0][7][2])+((mfk[4][0]*Vpk[0][10][0])+(
      mfk[4][1]*Vpk[0][10][1])))+temp[0]));
    temp[0] = (((mfk[3][2]*Vpk[1][9][2])+((mfk[3][0]*Vpk[1][8][0])+(mfk[3][1]*
      Vpk[1][9][1])))+(((mfk[2][2]*Vpk[1][7][2])+((mfk[2][0]*Vpk[1][6][0])+(
      mfk[2][1]*Vpk[1][7][1])))+(((Cik[3][1][2]*mfk[0][2])+((Cik[3][1][0]*
      mfk[0][0])+(Cik[3][1][1]*mfk[0][1])))+((Cik[3][1][2]*mfk[1][2])+((
      mfk[1][0]*Vpk[1][6][0])+(mfk[1][1]*Vpk[1][6][1]))))));
    fs[1] = (mtau[1]+(((mfk[4][2]*Vpk[1][7][2])+((mfk[4][0]*Vpk[1][10][0])+(
      mfk[4][1]*Vpk[1][10][1])))+temp[0]));
    temp[0] = (((mfk[3][2]*Vpk[2][9][2])+((mfk[3][0]*Vpk[2][8][0])+(mfk[3][1]*
      Vpk[2][9][1])))+(((mfk[2][2]*Vpk[2][7][2])+((mfk[2][0]*Vpk[2][6][0])+(
      mfk[2][1]*Vpk[2][7][1])))+(((Cik[3][2][2]*mfk[0][2])+((Cik[3][2][0]*
      mfk[0][0])+(Cik[3][2][1]*mfk[0][1])))+((Cik[3][2][2]*mfk[1][2])+((
      mfk[1][0]*Vpk[2][6][0])+(mfk[1][1]*Vpk[2][6][1]))))));
    fs[2] = (mtau[2]+(((mfk[4][2]*Vpk[2][7][2])+((mfk[4][0]*Vpk[2][10][0])+(
      mfk[4][1]*Vpk[2][10][1])))+temp[0]));
    temp[0] = (((((1.2*(mfk[3][1]*s9))-(3.3*(mfk[3][0]*Wpk[3][9][2])))-(
      mfk[3][2]*Vpk[3][9][2]))-((mtk[3][2]*Wpk[3][9][2])+((mtk[3][0]*c8)+(
      mtk[3][1]*Wpk[3][9][1]))))+(((((mtk[1][1]*s6)-(mtk[1][0]*c6))-(.75*((
      mfk[1][0]*s6)+(mfk[1][1]*c6))))-mtk[0][0])-(((mfk[2][2]*Vpk[3][7][2])+((
      mfk[2][0]*Vpk[3][7][0])+(mfk[2][1]*Vpk[3][7][1])))+((mtk[2][2]*
      Wpk[3][7][2])+((mtk[2][0]*c6)+(mtk[2][1]*Wpk[3][7][1]))))));
    fs[3] = (mtau[3]-(temp[0]-(((mfk[4][2]*Vpk[3][10][2])+((mfk[4][0]*
      Vpk[3][10][0])+(mfk[4][1]*Vpk[3][10][1])))+((mtk[4][2]*Wpk[3][7][2])+((
      mtk[4][0]*Wpk[3][10][0])+(mtk[4][1]*Wpk[3][10][1]))))));
    temp[0] = (((3.3*((mfk[3][2]*s8)-(mfk[3][0]*Wpk[4][9][2])))-((mtk[3][2]*
      Wpk[4][9][2])+((mtk[3][0]*s8)+(mtk[3][1]*Wpk[4][9][1]))))+((((.75*((
      mfk[1][0]*c6)-(mfk[1][1]*s6)))-((mtk[1][0]*s6)+(mtk[1][1]*c6)))-mtk[0][1])
      -(((mfk[2][2]*Vpk[4][7][2])+((mfk[2][0]*Vpk[4][7][0])+(mfk[2][1]*
      Vpk[4][7][1])))+((mtk[2][2]*Wpk[4][7][2])+((mtk[2][0]*s6)+(mtk[2][1]*
      Wpk[4][7][1]))))));
    fs[4] = (mtau[4]-(temp[0]-(((mfk[4][2]*Vpk[4][10][2])+((mfk[4][0]*
      Vpk[4][10][0])+(mfk[4][1]*Vpk[4][10][1])))+((mtk[4][2]*Wpk[4][7][2])+((
      mtk[4][0]*Wpk[4][10][0])+(mtk[4][1]*Wpk[4][10][1]))))));
    fs[5] = (mtau[5]-((((.01*(mfk[4][2]*Wpk[5][10][0]))-((mfk[4][0]*
      Vpk[5][10][0])+(mfk[4][1]*Vpk[5][10][1])))-((mtk[4][2]*c7)+((mtk[4][0]*
      Wpk[5][10][0])+(mtk[4][1]*Wpk[5][10][1]))))+(((((mtk[2][1]*s7)-(mtk[2][2]*
      c7))-(mfk[2][0]*Vpk[5][7][0]))-(mtk[0][2]+mtk[1][2]))-(((mfk[3][2]*
      Vpk[5][9][2])+((mfk[3][0]*Vpk[5][9][0])+(mfk[3][1]*Vpk[5][9][1])))+((
      mtk[3][1]*s9)+(mtk[3][2]*c9))))));
    fs[6] = (mtau[6]-((((.01*(mfk[4][2]*Wpk[6][10][0]))-((mfk[4][0]*
      Vpk[6][10][0])+(mfk[4][1]*Vpk[6][10][1])))-((mtk[4][2]*c7)+((mtk[4][0]*
      Wpk[6][10][0])+(mtk[4][1]*Wpk[6][10][1]))))+((((mtk[2][1]*s7)-(mtk[2][2]*
      c7))-(mfk[2][0]*Vpk[6][7][0]))-mtk[1][2])));
    fs[7] = (mtau[7]-((mtk[2][0]+((.2*mfk[2][1])+(.22*mfk[2][2])))+(((.2*((
      mfk[4][0]*s10)+(mfk[4][1]*c10)))-(mfk[4][2]*Vpk[7][10][2]))+((mtk[4][0]*
      c10)-(mtk[4][1]*s10)))));
    fs[8] = (mtau[8]+((3.3*(mfk[3][0]*c9))+((mtk[3][1]*s9)+(mtk[3][2]*c9))));
    fs[9] = (mtau[9]-((3.3*mfk[3][2])-mtk[3][0]));
    fs[10] = (mtau[10]+(mtk[4][2]+(.01*mfk[4][0])));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  131 adds/subtracts/negates
                    132 multiplies
                      0 divides
                     16 assignments
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
    fs[6] = (fs[6]+fs0[6]);
    fs[7] = (fs[7]+fs0[7]);
    fs[8] = (fs[8]+fs0[8]);
    fs[9] = (fs[9]+fs0[9]);
    fs[10] = (fs[10]+fs0[10]);
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   11 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
*/
}

void sdfulltrq(double udotin[11],
    double multin[1],
    double trqout[11])
{
/* Compute hinge torques which would produce indicated udots
*/
    double fstarr[11][3],tstarr[11][3],Otkr[11][3],Atir[11][3],Atkr[11][3];

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
    Otkr[6][0] = ((u[6]*wk[6][1])+((udotin[3]*c6)+(udotin[4]*s6)));
    Otkr[6][1] = (((udotin[4]*c6)-(udotin[3]*s6))-(u[6]*wk[6][0]));
    Otkr[6][2] = (udotin[5]+udotin[6]);
    Otkr[7][0] = (Otkr[6][0]-udotin[7]);
    Otkr[7][1] = (((Otkr[6][1]*c7)-(Otkr[6][2]*s7))-(u[7]*wk[7][2]));
    Otkr[7][2] = ((u[7]*wk[7][1])+((Otkr[6][1]*s7)+(Otkr[6][2]*c7)));
    Otkr[8][0] = ((u[8]*wk[8][1])+((udotin[3]*c8)+(udotin[4]*s8)));
    Otkr[8][1] = (((udotin[4]*c8)-(udotin[3]*s8))-(u[8]*wk[8][0]));
    Otkr[8][2] = (udotin[5]+udotin[8]);
    Otkr[9][0] = (Otkr[8][0]+udotin[9]);
    Otkr[9][1] = ((u[9]*wk[9][2])+((Otkr[8][1]*c9)+(Otkr[8][2]*s9)));
    Otkr[9][2] = (((Otkr[8][2]*c9)-(Otkr[8][1]*s9))-(u[9]*wk[9][1]));
    Otkr[10][0] = ((u[10]*wk[10][1])+((Otkr[7][0]*c10)+(Otkr[7][1]*s10)));
    Otkr[10][1] = (((Otkr[7][1]*c10)-(Otkr[7][0]*s10))-(u[10]*wk[10][0]));
    Otkr[10][2] = (Otkr[7][2]+udotin[10]);
    Atkr[3][0] = ((Cik[3][2][0]*udotin[2])+((Cik[3][0][0]*udotin[0])+(
      Cik[3][1][0]*udotin[1])));
    Atkr[3][1] = ((Cik[3][2][1]*udotin[2])+((Cik[3][0][1]*udotin[0])+(
      Cik[3][1][1]*udotin[1])));
    Atkr[3][2] = ((Cik[3][2][2]*udotin[2])+((Cik[3][0][2]*udotin[0])+(
      Cik[3][1][2]*udotin[1])));
    Atir[6][0] = (Atkr[3][0]-(1.5*(udotin[4]+(u[3]*u[5]))));
    Atir[6][1] = (Atkr[3][1]+(1.5*(udotin[3]-(u[4]*u[5]))));
    Atir[6][2] = (Atkr[3][2]+(1.5*((u[3]*u[3])+(u[4]*u[4]))));
    Atkr[6][0] = ((.75*(Otkr[6][1]+(wk[6][0]*wk[6][2])))+((Atir[6][0]*c6)+(
      Atir[6][1]*s6)));
    Atkr[6][1] = ((.75*((wk[6][1]*wk[6][2])-Otkr[6][0]))+((Atir[6][1]*c6)-(
      Atir[6][0]*s6)));
    Atkr[6][2] = (Atir[6][2]-(.75*((wk[6][0]*wk[6][0])+(wk[6][1]*wk[6][1]))));
    Atir[7][0] = (Atkr[6][0]+(((.1*Otkr[6][2])-(.75*Otkr[6][1]))-((.1*(wk[6][0]*
      wk[6][1]))+(.75*(wk[6][0]*wk[6][2])))));
    Atir[7][1] = (Atkr[6][1]+((.75*Otkr[6][0])+((.1*(wk[6][0]*wk[6][0]))+(
      Wirk[7][0]*wk[6][2]))));
    Atir[7][2] = (Atkr[6][2]+(((.75*(wk[6][0]*wk[6][0]))-(Wirk[7][0]*wk[6][1]))-
      (.1*Otkr[6][0])));
    Atkr[7][0] = (Atir[7][0]+(((.22*(wk[7][0]*wk[7][1]))-(.2*(wk[7][0]*wk[7][2])
      ))-((.2*Otkr[7][1])+(.22*Otkr[7][2]))));
    Atkr[7][1] = (((.2*Otkr[7][0])+((wk[7][2]*Wkrpk[7][0])-(.22*(wk[7][0]*
      wk[7][0]))))+((Atir[7][1]*c7)-(Atir[7][2]*s7)));
    Atkr[7][2] = (((.22*Otkr[7][0])+((.2*(wk[7][0]*wk[7][0]))-(wk[7][1]*
      Wkrpk[7][0])))+((Atir[7][1]*s7)+(Atir[7][2]*c7)));
    Atir[8][0] = (Atkr[3][0]+(1.2*(udotin[5]-(u[3]*u[4]))));
    Atir[8][1] = (Atkr[3][1]+(1.2*((u[3]*u[3])+(u[5]*u[5]))));
    Atir[8][2] = (Atkr[3][2]-(1.2*(udotin[3]+(u[4]*u[5]))));
    Atkr[8][0] = ((Atir[8][0]*c8)+(Atir[8][1]*s8));
    Atkr[8][1] = ((Atir[8][1]*c8)-(Atir[8][0]*s8));
    Atkr[9][0] = (Atkr[8][0]+(3.3*(Otkr[9][2]-(wk[9][0]*wk[9][1]))));
    Atkr[9][1] = ((3.3*((wk[9][0]*wk[9][0])+(wk[9][2]*wk[9][2])))+((Atir[8][2]*
      s9)+(Atkr[8][1]*c9)));
    Atkr[9][2] = (((Atir[8][2]*c9)-(Atkr[8][1]*s9))-(3.3*(Otkr[9][0]+(wk[9][1]*
      wk[9][2]))));
    Atir[10][0] = (Atkr[7][0]+((Wirk[10][2]*wk[7][1])-(Otkr[7][2]*ri[4][1])));
    Atir[10][1] = (Atkr[7][1]+((Wirk[10][0]*wk[7][2])-(Wirk[10][2]*wk[7][0])));
    Atir[10][2] = (Atkr[7][2]+((Otkr[7][0]*ri[4][1])-(Wirk[10][0]*wk[7][1])));
    Atkr[10][0] = ((.01*(Otkr[10][2]-(wk[10][0]*wk[10][1])))+((Atir[10][0]*c10)+
      (Atir[10][1]*s10)));
    Atkr[10][1] = ((.01*((wk[10][0]*wk[10][0])+(wk[10][2]*wk[10][2])))+((
      Atir[10][1]*c10)-(Atir[10][0]*s10)));
    Atkr[10][2] = (Atir[10][2]-(.01*(Otkr[10][0]+(wk[10][1]*wk[10][2]))));
/*
Accumulate all forces and torques
*/
    fstarr[5][0] = (ufk[0][0]-(410.*Atkr[3][0]));
    fstarr[5][1] = (ufk[0][1]-(410.*Atkr[3][1]));
    fstarr[5][2] = (ufk[0][2]-(410.*Atkr[3][2]));
    fstarr[6][0] = (ufk[1][0]-(6.8*Atkr[6][0]));
    fstarr[6][1] = (ufk[1][1]-(6.8*Atkr[6][1]));
    fstarr[6][2] = (ufk[1][2]-(6.8*Atkr[6][2]));
    fstarr[7][0] = (ufk[2][0]-(57.5*Atkr[7][0]));
    fstarr[7][1] = (ufk[2][1]-(57.5*Atkr[7][1]));
    fstarr[7][2] = (ufk[2][2]-(57.5*Atkr[7][2]));
    fstarr[9][0] = (ufk[3][0]-(10.7*Atkr[9][0]));
    fstarr[9][1] = (ufk[3][1]-(10.7*Atkr[9][1]));
    fstarr[9][2] = (ufk[3][2]-(10.7*Atkr[9][2]));
    fstarr[10][0] = (ufk[4][0]-Atkr[10][0]);
    fstarr[10][1] = (ufk[4][1]-Atkr[10][1]);
    fstarr[10][2] = (ufk[4][2]-Atkr[10][2]);
    tstarr[5][0] = (utk[0][0]-(WkIkWk[5][0]+((14.*udotin[5])+((115.*udotin[3])-(
      14.*udotin[4])))));
    tstarr[5][1] = (utk[0][1]-(WkIkWk[5][1]+(((316.*udotin[4])-(14.*udotin[3]))-
      (34.6*udotin[5]))));
    tstarr[5][2] = (utk[0][2]-(WkIkWk[5][2]+((440.*udotin[5])+((14.*udotin[3])-(
      34.6*udotin[4])))));
    tstarr[6][0] = (utk[1][0]-(WkIkWk[6][0]+(.35*Otkr[6][0])));
    tstarr[6][1] = (utk[1][1]-(WkIkWk[6][1]+(.35*Otkr[6][1])));
    tstarr[7][0] = (utk[2][0]-(WkIkWk[7][0]+(((.41*Otkr[7][1])+(4.85*Otkr[7][0])
      )-(.07*Otkr[7][2]))));
    tstarr[7][1] = (utk[2][1]-(WkIkWk[7][1]+((.54*Otkr[7][2])+((.41*Otkr[7][0])+
      (2.2*Otkr[7][1])))));
    tstarr[7][2] = (utk[2][2]-(WkIkWk[7][2]+((5.5*Otkr[7][2])+((.54*Otkr[7][1])-
      (.07*Otkr[7][0])))));
    tstarr[9][0] = (utk[3][0]-(WkIkWk[9][0]+(27.2*Otkr[9][0])));
    tstarr[9][1] = (utk[3][1]-(.2*Otkr[9][1]));
    tstarr[9][2] = (utk[3][2]-(WkIkWk[9][2]+(27.2*Otkr[9][2])));
    tstarr[10][0] = (utk[4][0]-(WkIkWk[10][0]+(.1*Otkr[10][0])));
    tstarr[10][1] = (utk[4][1]-(WkIkWk[10][1]+(.1*Otkr[10][1])));
    tstarr[10][2] = (utk[4][2]-(.5*Otkr[10][2]));
/*
Now calculate the torques
*/
    sddovpk();
    temp[0] = (((fstarr[9][2]*Vpk[0][9][2])+((fstarr[9][0]*Vpk[0][8][0])+(
      fstarr[9][1]*Vpk[0][9][1])))+(((fstarr[7][2]*Vpk[0][7][2])+((fstarr[7][0]*
      Vpk[0][6][0])+(fstarr[7][1]*Vpk[0][7][1])))+(((Cik[3][0][2]*fstarr[5][2])+
      ((Cik[3][0][0]*fstarr[5][0])+(Cik[3][0][1]*fstarr[5][1])))+((Cik[3][0][2]*
      fstarr[6][2])+((fstarr[6][0]*Vpk[0][6][0])+(fstarr[6][1]*Vpk[0][6][1])))))
      );
    trqout[0] = -(utau[0]+(((fstarr[10][2]*Vpk[0][7][2])+((fstarr[10][0]*
      Vpk[0][10][0])+(fstarr[10][1]*Vpk[0][10][1])))+temp[0]));
    temp[0] = (((fstarr[9][2]*Vpk[1][9][2])+((fstarr[9][0]*Vpk[1][8][0])+(
      fstarr[9][1]*Vpk[1][9][1])))+(((fstarr[7][2]*Vpk[1][7][2])+((fstarr[7][0]*
      Vpk[1][6][0])+(fstarr[7][1]*Vpk[1][7][1])))+(((Cik[3][1][2]*fstarr[5][2])+
      ((Cik[3][1][0]*fstarr[5][0])+(Cik[3][1][1]*fstarr[5][1])))+((Cik[3][1][2]*
      fstarr[6][2])+((fstarr[6][0]*Vpk[1][6][0])+(fstarr[6][1]*Vpk[1][6][1])))))
      );
    trqout[1] = -(utau[1]+(((fstarr[10][2]*Vpk[1][7][2])+((fstarr[10][0]*
      Vpk[1][10][0])+(fstarr[10][1]*Vpk[1][10][1])))+temp[0]));
    temp[0] = (((fstarr[9][2]*Vpk[2][9][2])+((fstarr[9][0]*Vpk[2][8][0])+(
      fstarr[9][1]*Vpk[2][9][1])))+(((fstarr[7][2]*Vpk[2][7][2])+((fstarr[7][0]*
      Vpk[2][6][0])+(fstarr[7][1]*Vpk[2][7][1])))+(((Cik[3][2][2]*fstarr[5][2])+
      ((Cik[3][2][0]*fstarr[5][0])+(Cik[3][2][1]*fstarr[5][1])))+((Cik[3][2][2]*
      fstarr[6][2])+((fstarr[6][0]*Vpk[2][6][0])+(fstarr[6][1]*Vpk[2][6][1])))))
      );
    trqout[2] = -(utau[2]+(((fstarr[10][2]*Vpk[2][7][2])+((fstarr[10][0]*
      Vpk[2][10][0])+(fstarr[10][1]*Vpk[2][10][1])))+temp[0]));
    temp[0] = (((tstarr[5][0]+((.75*((fstarr[6][0]*s6)+(fstarr[6][1]*c6)))+((
      tstarr[6][0]*c6)-(tstarr[6][1]*s6))))+(((fstarr[7][2]*Vpk[3][7][2])+((
      fstarr[7][0]*Vpk[3][7][0])+(fstarr[7][1]*Vpk[3][7][1])))+((tstarr[7][2]*
      Wpk[3][7][2])+((tstarr[7][0]*c6)+(tstarr[7][1]*Wpk[3][7][1])))))+(((
      fstarr[9][2]*Vpk[3][9][2])+((3.3*(fstarr[9][0]*Wpk[3][9][2]))-(1.2*(
      fstarr[9][1]*s9))))+((tstarr[9][2]*Wpk[3][9][2])+((tstarr[9][0]*c8)+(
      tstarr[9][1]*Wpk[3][9][1])))));
    trqout[3] = -(utau[3]+((((fstarr[10][2]*Vpk[3][10][2])+((fstarr[10][0]*
      Vpk[3][10][0])+(fstarr[10][1]*Vpk[3][10][1])))+((tstarr[10][2]*
      Wpk[3][7][2])+((tstarr[10][0]*Wpk[3][10][0])+(tstarr[10][1]*Wpk[3][10][1])
      )))+temp[0]));
    temp[0] = (((3.3*((fstarr[9][0]*Wpk[4][9][2])-(fstarr[9][2]*s8)))+((
      tstarr[9][2]*Wpk[4][9][2])+((tstarr[9][0]*s8)+(tstarr[9][1]*Wpk[4][9][1]))
      ))+((tstarr[5][1]+((.75*((fstarr[6][1]*s6)-(fstarr[6][0]*c6)))+((
      tstarr[6][0]*s6)+(tstarr[6][1]*c6))))+(((fstarr[7][2]*Vpk[4][7][2])+((
      fstarr[7][0]*Vpk[4][7][0])+(fstarr[7][1]*Vpk[4][7][1])))+((tstarr[7][2]*
      Wpk[4][7][2])+((tstarr[7][0]*s6)+(tstarr[7][1]*Wpk[4][7][1]))))));
    trqout[4] = -(utau[4]+(temp[0]+(((fstarr[10][2]*Vpk[4][10][2])+((
      fstarr[10][0]*Vpk[4][10][0])+(fstarr[10][1]*Vpk[4][10][1])))+((
      tstarr[10][2]*Wpk[4][7][2])+((tstarr[10][0]*Wpk[4][10][0])+(tstarr[10][1]*
      Wpk[4][10][1]))))));
    trqout[5] = -(utau[5]+((((tstarr[5][2]+utk[1][2])+((fstarr[7][0]*
      Vpk[5][7][0])+((tstarr[7][2]*c7)-(tstarr[7][1]*s7))))+(((fstarr[9][2]*
      Vpk[5][9][2])+((fstarr[9][0]*Vpk[5][9][0])+(fstarr[9][1]*Vpk[5][9][1])))+(
      (tstarr[9][1]*s9)+(tstarr[9][2]*c9))))+(((tstarr[10][2]*c7)+((
      tstarr[10][0]*Wpk[5][10][0])+(tstarr[10][1]*Wpk[5][10][1])))+(((
      fstarr[10][0]*Vpk[5][10][0])+(fstarr[10][1]*Vpk[5][10][1]))-(.01*(
      fstarr[10][2]*Wpk[5][10][0]))))));
    trqout[6] = -(utau[6]+((utk[1][2]+((fstarr[7][0]*Vpk[6][7][0])+((
      tstarr[7][2]*c7)-(tstarr[7][1]*s7))))+(((tstarr[10][2]*c7)+((tstarr[10][0]
      *Wpk[6][10][0])+(tstarr[10][1]*Wpk[6][10][1])))+(((fstarr[10][0]*
      Vpk[6][10][0])+(fstarr[10][1]*Vpk[6][10][1]))-(.01*(fstarr[10][2]*
      Wpk[6][10][0]))))));
    trqout[7] = -(utau[7]+((((fstarr[10][2]*Vpk[7][10][2])-(.2*((fstarr[10][0]*
      s10)+(fstarr[10][1]*c10))))+((tstarr[10][1]*s10)-(tstarr[10][0]*c10)))-(
      tstarr[7][0]+((.2*fstarr[7][1])+(.22*fstarr[7][2])))));
    trqout[8] = -(utau[8]+((3.3*(fstarr[9][0]*c9))+((tstarr[9][1]*s9)+(
      tstarr[9][2]*c9))));
    trqout[9] = -(utau[9]+(tstarr[9][0]-(3.3*fstarr[9][2])));
    trqout[10] = -((mtau[10]+utau[10])+(tstarr[10][2]+(.01*fstarr[10][0])));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  293 adds/subtracts/negates
                    298 multiplies
                      0 divides
                     89 assignments
*/
}

void sdcomptrq(double udotin[11],
    double trqout[11])
{
/* Compute hinge torques to produce these udots, ignoring constraints
*/
    int i;
    double multin[1];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(60,23);
        return;
    }
    for (i = 0; i < 1; i++) {
        multin[i] = 0.;
    }
    sdfulltrq(udotin,multin,trqout);
}

void sdmulttrq(double multin[1],
    double trqout[11])
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
    for (i = 0; i < 11; i++) {
        trqout[i] = fs[i];
    }
}

void sdrhs(void)
{
/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/

/*
Compute hinge torques for tree hinges
*/
    tauc[0] = utau[0];
    tauc[1] = utau[1];
    tauc[2] = utau[2];
    tauc[3] = utau[3];
    tauc[4] = utau[4];
    tauc[5] = utau[5];
    tauc[6] = utau[6];
    tauc[7] = utau[7];
    tauc[8] = utau[8];
    tauc[9] = utau[9];
    tauc[10] = (mtau[10]+utau[10]);
    sddoiner();
/*
Compute onk & onb (angular accels in N)
*/
    Onkb[6][0] = ((udot[3]*c6)+(udot[4]*s6));
    Onkb[6][1] = ((udot[4]*c6)-(udot[3]*s6));
    Onkb[6][2] = (udot[5]+udot[6]);
    Onkb[7][0] = (Onkb[6][0]-udot[7]);
    Onkb[7][1] = ((Onkb[6][1]*c7)-(Onkb[6][2]*s7));
    Onkb[7][2] = ((Onkb[6][1]*s7)+(Onkb[6][2]*c7));
    Onkb[8][0] = ((udot[3]*c8)+(udot[4]*s8));
    Onkb[8][1] = ((udot[4]*c8)-(udot[3]*s8));
    Onkb[8][2] = (udot[5]+udot[8]);
    Onkb[9][0] = (Onkb[8][0]+udot[9]);
    Onkb[9][1] = ((Onkb[8][1]*c9)+(Onkb[8][2]*s9));
    Onkb[9][2] = ((Onkb[8][2]*c9)-(Onkb[8][1]*s9));
    Onkb[10][0] = ((Onkb[7][0]*c10)+(Onkb[7][1]*s10));
    Onkb[10][1] = ((Onkb[7][1]*c10)-(Onkb[7][0]*s10));
    Onkb[10][2] = (Onkb[7][2]+udot[10]);
    onk[6][0] = (Onkb[6][0]+Otk[6][0]);
    onk[6][1] = (Onkb[6][1]+Otk[6][1]);
    onk[7][0] = (Onkb[7][0]+Otk[6][0]);
    onk[7][1] = (Onkb[7][1]+Otk[7][1]);
    onk[7][2] = (Onkb[7][2]+Otk[7][2]);
    onk[9][0] = (Onkb[9][0]+Otk[8][0]);
    onk[9][1] = (Onkb[9][1]+Otk[9][1]);
    onk[9][2] = (Onkb[9][2]+Otk[9][2]);
    onk[10][0] = (Onkb[10][0]+Otk[10][0]);
    onk[10][1] = (Onkb[10][1]+Otk[10][1]);
    onk[10][2] = (Onkb[10][2]+Otk[7][2]);
    onb[0][0] = udot[3];
    onb[0][1] = udot[4];
    onb[0][2] = udot[5];
    onb[1][0] = onk[6][0];
    onb[1][1] = onk[6][1];
    onb[1][2] = Onkb[6][2];
    onb[2][0] = onk[7][0];
    onb[2][1] = onk[7][1];
    onb[2][2] = onk[7][2];
    onb[3][0] = onk[9][0];
    onb[3][1] = onk[9][1];
    onb[3][2] = onk[9][2];
    onb[4][0] = onk[10][0];
    onb[4][1] = onk[10][1];
    onb[4][2] = onk[10][2];
/*
Compute acceleration dyadics
*/
    dyad[0][0][0] = w11w22[0];
    dyad[0][0][1] = (w0w1[0]-udot[5]);
    dyad[0][0][2] = (udot[4]+w0w2[0]);
    dyad[0][1][0] = (udot[5]+w0w1[0]);
    dyad[0][1][1] = w00w22[0];
    dyad[0][1][2] = (w1w2[0]-udot[3]);
    dyad[0][2][0] = (w0w2[0]-udot[4]);
    dyad[0][2][1] = (udot[3]+w1w2[0]);
    dyad[0][2][2] = w00w11[0];
    dyad[1][0][0] = w11w22[1];
    dyad[1][0][1] = (w0w1[1]-Onkb[6][2]);
    dyad[1][0][2] = (onk[6][1]+w0w2[1]);
    dyad[1][1][0] = (Onkb[6][2]+w0w1[1]);
    dyad[1][1][1] = w00w22[1];
    dyad[1][1][2] = (w1w2[1]-onk[6][0]);
    dyad[1][2][0] = (w0w2[1]-onk[6][1]);
    dyad[1][2][1] = (onk[6][0]+w1w2[1]);
    dyad[1][2][2] = w00w11[1];
    dyad[2][0][0] = w11w22[2];
    dyad[2][0][1] = (w0w1[2]-onk[7][2]);
    dyad[2][0][2] = (onk[7][1]+w0w2[2]);
    dyad[2][1][0] = (onk[7][2]+w0w1[2]);
    dyad[2][1][1] = w00w22[2];
    dyad[2][1][2] = (w1w2[2]-onk[7][0]);
    dyad[2][2][0] = (w0w2[2]-onk[7][1]);
    dyad[2][2][1] = (onk[7][0]+w1w2[2]);
    dyad[2][2][2] = w00w11[2];
    dyad[3][0][0] = w11w22[3];
    dyad[3][0][1] = (w0w1[3]-onk[9][2]);
    dyad[3][0][2] = (onk[9][1]+w0w2[3]);
    dyad[3][1][0] = (onk[9][2]+w0w1[3]);
    dyad[3][1][1] = w00w22[3];
    dyad[3][1][2] = (w1w2[3]-onk[9][0]);
    dyad[3][2][0] = (w0w2[3]-onk[9][1]);
    dyad[3][2][1] = (onk[9][0]+w1w2[3]);
    dyad[3][2][2] = w00w11[3];
    dyad[4][0][0] = w11w22[4];
    dyad[4][0][1] = (w0w1[4]-onk[10][2]);
    dyad[4][0][2] = (onk[10][1]+w0w2[4]);
    dyad[4][1][0] = (onk[10][2]+w0w1[4]);
    dyad[4][1][1] = w00w22[4];
    dyad[4][1][2] = (w1w2[4]-onk[10][0]);
    dyad[4][2][0] = (w0w2[4]-onk[10][1]);
    dyad[4][2][1] = (onk[10][0]+w1w2[4]);
    dyad[4][2][2] = w00w11[4];
/*
Compute ank & anb (mass center linear accels in N)
*/
    Ankb[3][0] = ((Cik[3][2][0]*udot[2])+((Cik[3][0][0]*udot[0])+(Cik[3][1][0]*
      udot[1])));
    Ankb[3][1] = ((Cik[3][2][1]*udot[2])+((Cik[3][0][1]*udot[0])+(Cik[3][1][1]*
      udot[1])));
    Ankb[3][2] = ((Cik[3][2][2]*udot[2])+((Cik[3][0][2]*udot[0])+(Cik[3][1][2]*
      udot[1])));
    AOnkri[6][0] = (Ankb[3][0]-(1.5*udot[4]));
    AOnkri[6][1] = (Ankb[3][1]+(1.5*udot[3]));
    Ankb[6][0] = ((.75*Onkb[6][1])+((AOnkri[6][0]*c6)+(AOnkri[6][1]*s6)));
    Ankb[6][1] = (((AOnkri[6][1]*c6)-(AOnkri[6][0]*s6))-(.75*Onkb[6][0]));
    AOnkri[7][0] = (Ankb[6][0]+((.1*Onkb[6][2])-(.75*Onkb[6][1])));
    AOnkri[7][1] = (Ankb[6][1]+(.75*Onkb[6][0]));
    AOnkri[7][2] = (Ankb[3][2]-(.1*Onkb[6][0]));
    Ankb[7][0] = (AOnkri[7][0]-((.2*Onkb[7][1])+(.22*Onkb[7][2])));
    Ankb[7][1] = ((.2*Onkb[7][0])+((AOnkri[7][1]*c7)-(AOnkri[7][2]*s7)));
    Ankb[7][2] = ((.22*Onkb[7][0])+((AOnkri[7][1]*s7)+(AOnkri[7][2]*c7)));
    AOnkri[8][0] = (Ankb[3][0]+(1.2*udot[5]));
    AOnkri[8][2] = (Ankb[3][2]-(1.2*udot[3]));
    Ankb[8][0] = ((Ankb[3][1]*s8)+(AOnkri[8][0]*c8));
    Ankb[8][1] = ((Ankb[3][1]*c8)-(AOnkri[8][0]*s8));
    Ankb[9][0] = (Ankb[8][0]+(3.3*Onkb[9][2]));
    Ankb[9][1] = ((Ankb[8][1]*c9)+(AOnkri[8][2]*s9));
    Ankb[9][2] = (((AOnkri[8][2]*c9)-(Ankb[8][1]*s9))-(3.3*Onkb[9][0]));
    AOnkri[10][0] = (Ankb[7][0]-(Onkb[7][2]*ri[4][1]));
    AOnkri[10][2] = (Ankb[7][2]+(Onkb[7][0]*ri[4][1]));
    Ankb[10][0] = ((.01*Onkb[10][2])+((Ankb[7][1]*s10)+(AOnkri[10][0]*c10)));
    Ankb[10][1] = ((Ankb[7][1]*c10)-(AOnkri[10][0]*s10));
    Ankb[10][2] = (AOnkri[10][2]-(.01*Onkb[10][0]));
    ank[5][0] = ((Ankb[3][2]*Cik[3][0][2])+((Ankb[3][0]*Cik[3][0][0])+(
      Ankb[3][1]*Cik[3][0][1])));
    ank[5][1] = ((Ankb[3][2]*Cik[3][1][2])+((Ankb[3][0]*Cik[3][1][0])+(
      Ankb[3][1]*Cik[3][1][1])));
    ank[5][2] = ((Ankb[3][2]*Cik[3][2][2])+((Ankb[3][0]*Cik[3][2][0])+(
      Ankb[3][1]*Cik[3][2][1])));
    AnkAtk[6][0] = (Ankb[6][0]+Atk[6][0]);
    AnkAtk[6][1] = (Ankb[6][1]+Atk[6][1]);
    AnkAtk[6][2] = (Ankb[3][2]+Atk[6][2]);
    ank[6][0] = ((AnkAtk[6][2]*Cik[3][0][2])+((AnkAtk[6][0]*cnk[6][0][0])+(
      AnkAtk[6][1]*cnk[6][0][1])));
    ank[6][1] = ((AnkAtk[6][2]*Cik[3][1][2])+((AnkAtk[6][0]*cnk[6][1][0])+(
      AnkAtk[6][1]*cnk[6][1][1])));
    ank[6][2] = ((AnkAtk[6][2]*Cik[3][2][2])+((AnkAtk[6][0]*cnk[6][2][0])+(
      AnkAtk[6][1]*cnk[6][2][1])));
    AnkAtk[7][0] = (Ankb[7][0]+Atk[7][0]);
    AnkAtk[7][1] = (Ankb[7][1]+Atk[7][1]);
    AnkAtk[7][2] = (Ankb[7][2]+Atk[7][2]);
    ank[7][0] = ((AnkAtk[7][2]*cnk[7][0][2])+((AnkAtk[7][0]*cnk[6][0][0])+(
      AnkAtk[7][1]*cnk[7][0][1])));
    ank[7][1] = ((AnkAtk[7][2]*cnk[7][1][2])+((AnkAtk[7][0]*cnk[6][1][0])+(
      AnkAtk[7][1]*cnk[7][1][1])));
    ank[7][2] = ((AnkAtk[7][2]*cnk[7][2][2])+((AnkAtk[7][0]*cnk[6][2][0])+(
      AnkAtk[7][1]*cnk[7][2][1])));
    AnkAtk[9][0] = (Ankb[9][0]+Atk[9][0]);
    AnkAtk[9][1] = (Ankb[9][1]+Atk[9][1]);
    AnkAtk[9][2] = (Ankb[9][2]+Atk[9][2]);
    ank[9][0] = ((AnkAtk[9][2]*cnk[9][0][2])+((AnkAtk[9][0]*cnk[8][0][0])+(
      AnkAtk[9][1]*cnk[9][0][1])));
    ank[9][1] = ((AnkAtk[9][2]*cnk[9][1][2])+((AnkAtk[9][0]*cnk[8][1][0])+(
      AnkAtk[9][1]*cnk[9][1][1])));
    ank[9][2] = ((AnkAtk[9][2]*cnk[9][2][2])+((AnkAtk[9][0]*cnk[8][2][0])+(
      AnkAtk[9][1]*cnk[9][2][1])));
    AnkAtk[10][0] = (Ankb[10][0]+Atk[10][0]);
    AnkAtk[10][1] = (Ankb[10][1]+Atk[10][1]);
    AnkAtk[10][2] = (Ankb[10][2]+Atk[10][2]);
    ank[10][0] = ((AnkAtk[10][2]*cnk[7][0][2])+((AnkAtk[10][0]*cnk[10][0][0])+(
      AnkAtk[10][1]*cnk[10][0][1])));
    ank[10][1] = ((AnkAtk[10][2]*cnk[7][1][2])+((AnkAtk[10][0]*cnk[10][1][0])+(
      AnkAtk[10][1]*cnk[10][1][1])));
    ank[10][2] = ((AnkAtk[10][2]*cnk[7][2][2])+((AnkAtk[10][0]*cnk[10][2][0])+(
      AnkAtk[10][1]*cnk[10][2][1])));
    anb[0][0] = ank[5][0];
    anb[0][1] = ank[5][1];
    anb[0][2] = ank[5][2];
    anb[1][0] = ank[6][0];
    anb[1][1] = ank[6][1];
    anb[1][2] = ank[6][2];
    anb[2][0] = ank[7][0];
    anb[2][1] = ank[7][1];
    anb[2][2] = ank[7][2];
    anb[3][0] = ank[9][0];
    anb[3][1] = ank[9][1];
    anb[3][2] = ank[9][2];
    anb[4][0] = ank[10][0];
    anb[4][1] = ank[10][1];
    anb[4][2] = ank[10][2];
/*
Compute constraint acceleration errors
*/
    roustate = 3;
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  135 adds/subtracts/negates
                    114 multiplies
                      0 divides
                    164 assignments
*/
}

void sdmassmat(double mmat[11][11])
{
/* Return the system mass matrix (LHS)
*/
    int i,j;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(57,23);
        return;
    }
    sddomm(57);
    for (i = 0; i < 11; i++) {
        for (j = i; j <= 10; j++) {
            mmat[i][j] = mm[i][j];
            mmat[j][i] = mm[i][j];
        }
    }
}

void sdfrcmat(double fmat[11])
{
/* Return the system force matrix (RHS), excluding constraints
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(58,23);
        return;
    }
    sddofs0();
    for (i = 0; i < 11; i++) {
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

void sdperr(double errs[1])
{
/*
Return position constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(26,23);
        return;
    }
    perr[0] = (q[10]-upos[10]);
    errs[0] = perr[0];
}

void sdverr(double errs[1])
{
/*
Return velocity constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(27,23);
        return;
    }
    verr[0] = (u[10]-uvel[10]);
    errs[0] = verr[0];
}

void sdaerr(double errs[1])
{
/*
Return acceleration constraint errors.

*/

    if (roustate != 3) {
        sdseterr(35,24);
        return;
    }
    aerr[0] = (udot[10]-uacc[10]);
    errs[0] = aerr[0];
}
int 
sdchkbnum(int routine,
    int bnum)
{

    if ((bnum < -1) || (bnum > 4)) {
        sdseterr(routine,15);
        return 1;
    }
    return 0;
}
int 
sdchkjnum(int routine,
    int jnum)
{

    if ((jnum < 0) || (jnum > 4)) {
        sdseterr(routine,16);
        return 1;
    }
    return 0;
}
int 
sdchkucnum(int routine,
    int ucnum)
{

    if ((ucnum < 0) || (ucnum > -1)) {
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
    double pp[1][11],dpp[11][1];
    int i,j,c;
    double sum;
    double dfk[5][3],dtk[5][3],dtau[11],dltci[1][3],dltc[1][3],dlfci[1][3],dlfc[
      1][3];
    double dTinb[1][3],dToutb[1][3],dltaufi[1][3],dltaufo[1][3],dltauti[1][3],
      dltauto[1][3];
    double dfs[11],row[11],dinvrow[11];

    roustate = 2;
    if (wwflg == 0) {
/*
Compute constraint effects
*/
        sddovpk();
        sddommldu(routine);
/*
Constraint 0 (prescribed motion)
*/
        dfs[0] = 0.;
        dfs[1] = 0.;
        dfs[2] = 0.;
        dfs[3] = 0.;
        dfs[4] = 0.;
        dfs[5] = 0.;
        dfs[6] = 0.;
        dfs[7] = 0.;
        dfs[8] = 0.;
        dfs[9] = 0.;
        dfs[10] = 1.;
        sdldubsl(11,11,mmap,mlo,dfs,row);
        sdldubsd(11,11,mmap,mdi,row,dinvrow);
        for (i = 0; i <= 10; i++) {
            pp[0][i] = row[i];
            dpp[i][0] = dinvrow[i];
        }
        wmap[0] = 0;
/*
Produce constraint coefficient matrix WW
*/
        for (c = 0; c <= 0; c++) {
            for (i = c; i <= 0; i++) {
                sum = 0.;
                for (j = 0; j <= 10; j++) {
                    sum = sum+pp[wmap[c]][j]*dpp[j][wmap[i]];
                }
                ww[wmap[c]][wmap[i]] = sum;
                ww[wmap[i]][wmap[c]] = sum;
            }
        }
/*
Form QR decomposition of WW
*/
        sdqrdcomp(1,1,1,1,wmap,wmap,ww,qraux,jpvt);
        wwflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   11 adds/subtracts/negates
                     11 multiplies
                      0 divides
                     47 assignments
*/
}

void sdxudot0(int routine,
    double oudot0[11])
{
/*
Compute unconstrained equations
*/
    int i;

    sdlhs(routine);
/*
Solve equations ignoring constraints
*/
    sdfs0();
    sdldubslv(11,11,mmap,works,mlo,mdi,fs,udot);
    for (i = 0; i <= 10; i++) {
        oudot0[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
*/
}

void sdudot0(double oudot0[11])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(66,23);
        return;
    }
    sdxudot0(66,oudot0);
}

void sdsetudot(double iudot[11])
{
/*
Assign udots and advance to stage Dynamics Ready
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(68,23);
        return;
    }
    for (i = 0; i <= 10; i++) {
        udot[i] = iudot[i];
    }
    sdrhs();
}

void sdxudotm(int routine,
    double imult[1],
    double oudotm[11])
{
/*
Compute udots due only to multipliers
*/
    int i;

    sdlhs(routine);
    sdmfrc(imult);
    sdfsmult();
    sdldubslv(11,11,mmap,works,mlo,mdi,fs,udot);
    for (i = 0; i <= 10; i++) {
        oudotm[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                     11 assignments
*/
}

void sdudotm(double imult[1],
    double oudotm[11])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(67,23);
        return;
    }
    sdxudotm(67,imult,oudotm);
}

void sdderiv(double oqdot[12],
    double oudot[11])
{
/*
This is the derivative section for a 5-body free-flying
system with 11 degree(s) of freedom, including 6 degrees
of freedom for attitude and translation of the base body.
1 of the degrees of freedom follow(s) prescribed motion.
There is 1 constraint.
*/
    double workr[1],bb[1],b0[1],v0[1],p0[1];
    int iwork[1];
    int i,j;
    double udot0[11],udot1[11];

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
    wsiz = 1;
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
    for (i = 0; i <= 0; i++) {
        bb[i] = -b0[i];
    }
    if (stabvel  !=  0.) {
        for (i = 0; i <= 0; i++) {
            bb[i] = bb[i]-stabvel*v0[i];
        }
    }
    if (stabpos  !=  0.) {
        for (i = 0; i <= 0; i++) {
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
    sdqrbslv(1,1,1,1,wmap,wmap,1e-13,workr,iwork,ww,qraux,jpvt,bb,mult,&wrank);
    for (i = 0; i <= 0; i++) {
        multmap[i] = 0;
    }
    for (i = 0; i < wrank; i++) {
        multmap[jpvt[i]] = 1;
    }
    j = 0;
    for (i = 0; i <= 0; i++) {
        if (multmap[i] != 0) {
            multmap[j] = wmap[i];
            j = j+1;
        }
    }
/*
Compute final udots
*/
    sdxudotm(17,mult,udot1);
    for (i = 0; i <= 10; i++) {
        udot[i] = udot0[i]+udot1[i];
    }
    sdrhs();
    for (i = 0; i <= 11; i++) {
        oqdot[i] = qdot[i];
    }
    for (i = 0; i <= 10; i++) {
        oudot[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   14 adds/subtracts/negates
                      2 multiplies
                      0 divides
                     37 assignments
*/
}
/*
Compute residuals for use with DAE integrator
*/

void sdresid(double eqdot[12],
    double eudot[11],
    double emults[1],
    double resid[24])
{
    int i;
    double uderrs[11],p0[1];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(16,23);
        return;
    }
    if (stabposq == 1) {
        sdseterr(16,33);
    }
    sdfulltrq(eudot,emults,uderrs);
    for (i = 0; i < 12; i++) {
        resid[i] = eqdot[i]-qdot[i];
    }
    for (i = 0; i < 11; i++) {
        resid[12+i] = uderrs[i];
    }
    sdverr(&resid[23]);
    if (stabpos  !=  0.) {
        sdperr(p0);
        for (i = 0; i < 1; i++) {
            resid[23+i] = resid[23+i]+stabpos*p0[i];
        }
    }
    for (i = 0; i < 11; i++) {
        udot[i] = eudot[i];
    }
    for (i = 0; i < 1; i++) {
        mult[i] = emults[i];
    }
    sdrhs();
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   13 adds/subtracts/negates
                      1 multiplies
                      0 divides
                     36 assignments
*/
}

void sdmult(double omults[1],
    int *owrank,
    int omultmap[1])
{
    int i;

    if (roustate != 3) {
        sdseterr(34,24);
        return;
    }
    for (i = 0; i < 1; i++) {
        omults[i] = mult[i];
        if (i <= wrank-1) {
            omultmap[i] = multmap[i];
        } else {
            omultmap[i] = -1;
        }
    }
    *owrank = wrank;
}

void sdreac(double force[5][3],
    double torque[5][3])
{
/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/

    if (roustate != 3) {
        sdseterr(31,24);
        return;
    }
/*
Compute reaction forces for non-weld tree joints
*/
    fc[10][0] = (AnkAtk[10][0]-ufk[4][0]);
    fc[10][1] = (AnkAtk[10][1]-ufk[4][1]);
    fc[10][2] = (AnkAtk[10][2]-ufk[4][2]);
    tc[10][0] = ((WkIkWk[10][0]+(.1*onk[10][0]))-(utk[4][0]+(.01*fc[10][2])));
    tc[10][1] = ((WkIkWk[10][1]+(.1*onk[10][1]))-utk[4][1]);
    tc[10][2] = ((.5*onk[10][2])-(utk[4][2]-(.01*fc[10][0])));
    fccikt[10][0] = ((fc[10][0]*c10)-(fc[10][1]*s10));
    fccikt[10][1] = ((fc[10][0]*s10)+(fc[10][1]*c10));
    fccikt[10][2] = fc[10][2];
    ffk[7][0] = (ufk[2][0]-fccikt[10][0]);
    ffk[7][1] = (ufk[2][1]-fccikt[10][1]);
    ffk[7][2] = (ufk[2][2]-fccikt[10][2]);
    ttk[7][0] = (utk[2][0]-((fccikt[10][2]*ri[4][1])+((tc[10][0]*c10)-(tc[10][1]
      *s10))));
    ttk[7][1] = (utk[2][1]-((tc[10][0]*s10)+(tc[10][1]*c10)));
    ttk[7][2] = (utk[2][2]-(tc[10][2]-(fccikt[10][0]*ri[4][1])));
    fc[9][0] = ((10.7*AnkAtk[9][0])-ufk[3][0]);
    fc[9][1] = ((10.7*AnkAtk[9][1])-ufk[3][1]);
    fc[9][2] = ((10.7*AnkAtk[9][2])-ufk[3][2]);
    tc[9][0] = ((WkIkWk[9][0]+(27.2*onk[9][0]))-(utk[3][0]+(3.3*fc[9][2])));
    tc[9][1] = ((.2*onk[9][1])-utk[3][1]);
    tc[9][2] = ((WkIkWk[9][2]+(27.2*onk[9][2]))-(utk[3][2]-(3.3*fc[9][0])));
    fccikt[9][0] = fc[9][0];
    fccikt[9][1] = ((fc[9][1]*c9)-(fc[9][2]*s9));
    fccikt[9][2] = ((fc[9][1]*s9)+(fc[9][2]*c9));
    ffk[8][0] = -fccikt[9][0];
    ffk[8][1] = -fccikt[9][1];
    ffk[8][2] = -fccikt[9][2];
    ttk[8][0] = -tc[9][0];
    ttk[8][1] = -((tc[9][1]*c9)-(tc[9][2]*s9));
    ttk[8][2] = -((tc[9][1]*s9)+(tc[9][2]*c9));
    fc[8][0] = -ffk[8][0];
    fc[8][1] = -ffk[8][1];
    fc[8][2] = -ffk[8][2];
    tc[8][0] = -ttk[8][0];
    tc[8][1] = -ttk[8][1];
    tc[8][2] = -ttk[8][2];
    fccikt[8][0] = ((fc[8][0]*c8)-(fc[8][1]*s8));
    fccikt[8][1] = ((fc[8][0]*s8)+(fc[8][1]*c8));
    fccikt[8][2] = fc[8][2];
    ffk[5][0] = (ufk[0][0]-fccikt[8][0]);
    ffk[5][1] = (ufk[0][1]-fccikt[8][1]);
    ffk[5][2] = (ufk[0][2]-fccikt[8][2]);
    ttk[5][0] = (utk[0][0]-(((tc[8][0]*c8)-(tc[8][1]*s8))-(1.2*fccikt[8][2])));
    ttk[5][1] = (utk[0][1]-((tc[8][0]*s8)+(tc[8][1]*c8)));
    ttk[5][2] = (utk[0][2]-(tc[8][2]+(1.2*fccikt[8][0])));
    fc[7][0] = ((57.5*AnkAtk[7][0])-ffk[7][0]);
    fc[7][1] = ((57.5*AnkAtk[7][1])-ffk[7][1]);
    fc[7][2] = ((57.5*AnkAtk[7][2])-ffk[7][2]);
    tc[7][0] = ((WkIkWk[7][0]+(((.41*onk[7][1])+(4.85*onk[7][0]))-(.07*onk[7][2]
      )))-(ttk[7][0]-((.2*fc[7][1])+(.22*fc[7][2]))));
    tc[7][1] = ((WkIkWk[7][1]+((.54*onk[7][2])+((.41*onk[7][0])+(2.2*onk[7][1]))
      ))-(ttk[7][1]+(.2*fc[7][0])));
    tc[7][2] = ((WkIkWk[7][2]+((5.5*onk[7][2])+((.54*onk[7][1])-(.07*onk[7][0]))
      ))-(ttk[7][2]+(.22*fc[7][0])));
    fccikt[7][0] = fc[7][0];
    fccikt[7][1] = ((fc[7][1]*c7)+(fc[7][2]*s7));
    fccikt[7][2] = ((fc[7][2]*c7)-(fc[7][1]*s7));
    ffk[6][0] = (ufk[1][0]-fccikt[7][0]);
    ffk[6][1] = (ufk[1][1]-fccikt[7][1]);
    ffk[6][2] = (ufk[1][2]-fccikt[7][2]);
    ttk[6][0] = (utk[1][0]-(tc[7][0]+((.75*fccikt[7][1])-(.1*fccikt[7][2]))));
    ttk[6][1] = (utk[1][1]-(((tc[7][1]*c7)+(tc[7][2]*s7))-(.75*fccikt[7][0])));
    ttk[6][2] = (utk[1][2]-((.1*fccikt[7][0])+((tc[7][2]*c7)-(tc[7][1]*s7))));
    fc[6][0] = ((6.8*AnkAtk[6][0])-ffk[6][0]);
    fc[6][1] = ((6.8*AnkAtk[6][1])-ffk[6][1]);
    fc[6][2] = ((6.8*AnkAtk[6][2])-ffk[6][2]);
    tc[6][0] = ((WkIkWk[6][0]+(.35*onk[6][0]))-(ttk[6][0]+(.75*fc[6][1])));
    tc[6][1] = ((WkIkWk[6][1]+(.35*onk[6][1]))-(ttk[6][1]-(.75*fc[6][0])));
    tc[6][2] = -ttk[6][2];
    fccikt[6][0] = ((fc[6][0]*c6)-(fc[6][1]*s6));
    fccikt[6][1] = ((fc[6][0]*s6)+(fc[6][1]*c6));
    fccikt[6][2] = fc[6][2];
    ffk[5][0] = (ffk[5][0]-fccikt[6][0]);
    ffk[5][1] = (ffk[5][1]-fccikt[6][1]);
    ffk[5][2] = (ffk[5][2]-fccikt[6][2]);
    ttk[5][0] = (ttk[5][0]-((1.5*fccikt[6][1])+((tc[6][0]*c6)-(tc[6][1]*s6))));
    ttk[5][1] = (ttk[5][1]-(((tc[6][0]*s6)+(tc[6][1]*c6))-(1.5*fccikt[6][0])));
    ttk[5][2] = (ttk[5][2]-tc[6][2]);
    fc[5][0] = ((410.*Ankb[3][0])-ffk[5][0]);
    fc[5][1] = ((410.*Ankb[3][1])-ffk[5][1]);
    fc[5][2] = ((410.*Ankb[3][2])-ffk[5][2]);
    tc[5][0] = ((WkIkWk[5][0]+((14.*udot[5])+((115.*udot[3])-(14.*udot[4]))))-
      ttk[5][0]);
    tc[5][1] = ((WkIkWk[5][1]+(((316.*udot[4])-(14.*udot[3]))-(34.6*udot[5])))-
      ttk[5][1]);
    tc[5][2] = ((WkIkWk[5][2]+((440.*udot[5])+((14.*udot[3])-(34.6*udot[4]))))-
      ttk[5][2]);
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
    ffk[2][0] = -fccikt[3][0];
    ffk[2][1] = -fccikt[3][1];
    ffk[2][2] = -fccikt[3][2];
    ttk[2][0] = -((Cik[3][0][2]*tc[3][2])+((Cik[3][0][0]*tc[3][0])+(Cik[3][0][1]
      *tc[3][1])));
    ttk[2][1] = -((Cik[3][1][2]*tc[3][2])+((Cik[3][1][0]*tc[3][0])+(Cik[3][1][1]
      *tc[3][1])));
    ttk[2][2] = -((Cik[3][2][2]*tc[3][2])+((Cik[3][2][0]*tc[3][0])+(Cik[3][2][1]
      *tc[3][1])));
    fc[2][0] = -ffk[2][0];
    fc[2][1] = -ffk[2][1];
    fc[2][2] = -ffk[2][2];
    tc[2][0] = -ttk[2][0];
    tc[2][1] = -ttk[2][1];
    tc[2][2] = -ttk[2][2];
    fccikt[2][0] = fc[2][0];
    fccikt[2][1] = fc[2][1];
    fccikt[2][2] = fc[2][2];
    ffk[1][0] = -fccikt[2][0];
    ffk[1][1] = -fccikt[2][1];
    ffk[1][2] = -fccikt[2][2];
    ttk[1][0] = -(tc[2][0]-(fccikt[2][1]*q[2]));
    ttk[1][1] = -(tc[2][1]+(fccikt[2][0]*q[2]));
    ttk[1][2] = -tc[2][2];
    fc[1][0] = -ffk[1][0];
    fc[1][1] = -ffk[1][1];
    fc[1][2] = -ffk[1][2];
    tc[1][0] = -ttk[1][0];
    tc[1][1] = -ttk[1][1];
    tc[1][2] = -ttk[1][2];
    fccikt[1][0] = fc[1][0];
    fccikt[1][1] = fc[1][1];
    fccikt[1][2] = fc[1][2];
    ffk[0][0] = -fccikt[1][0];
    ffk[0][1] = -fccikt[1][1];
    ffk[0][2] = -fccikt[1][2];
    ttk[0][0] = -(tc[1][0]+(fccikt[1][2]*q[1]));
    ttk[0][1] = -tc[1][1];
    ttk[0][2] = -(tc[1][2]-(fccikt[1][0]*q[1]));
    fc[0][0] = -ffk[0][0];
    fc[0][1] = -ffk[0][1];
    fc[0][2] = -ffk[0][2];
    tc[0][0] = -ttk[0][0];
    tc[0][1] = -ttk[0][1];
    tc[0][2] = -ttk[0][2];
    force[0][0] = fc[5][0];
    torque[0][0] = tc[5][0];
    force[0][1] = fc[5][1];
    torque[0][1] = tc[5][1];
    force[0][2] = fc[5][2];
    torque[0][2] = tc[5][2];
    force[1][0] = fc[6][0];
    torque[1][0] = tc[6][0];
    force[1][1] = fc[6][1];
    torque[1][1] = tc[6][1];
    force[1][2] = fc[6][2];
    torque[1][2] = tc[6][2];
    force[2][0] = fc[7][0];
    torque[2][0] = tc[7][0];
    force[2][1] = fc[7][1];
    torque[2][1] = tc[7][1];
    force[2][2] = fc[7][2];
    torque[2][2] = tc[7][2];
    force[3][0] = fc[9][0];
    torque[3][0] = tc[9][0];
    force[3][1] = fc[9][1];
    torque[3][1] = tc[9][1];
    force[3][2] = fc[9][2];
    torque[3][2] = tc[9][2];
    force[4][0] = fc[10][0];
    torque[4][0] = tc[10][0];
    force[4][1] = fc[10][1];
    torque[4][1] = tc[10][1];
    force[4][2] = fc[10][2];
    torque[4][2] = tc[10][2];
/*
Compute reaction forces for tree weld joints
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  206 adds/subtracts/negates
                    120 multiplies
                      0 divides
                    186 assignments
*/
}

void sdmom(double lm[3],
    double am[3],
    double *ke)
{
/*
Compute system linear and angular momentum, and kinetic energy.

Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
    double lk[5][3],hnk[5][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(19,23);
        return;
    }
    hnk[0][0] = ((14.*u[5])+((115.*u[3])-(14.*u[4])));
    hnk[0][1] = (((316.*u[4])-(14.*u[3]))-(34.6*u[5]));
    hnk[0][2] = ((440.*u[5])+((14.*u[3])-(34.6*u[4])));
    hnk[2][0] = (((.41*wk[7][1])+(4.85*wk[7][0]))-(.07*wk[7][2]));
    hnk[2][1] = ((.54*wk[7][2])+((.41*wk[7][0])+(2.2*wk[7][1])));
    hnk[2][2] = ((5.5*wk[7][2])+((.54*wk[7][1])-(.07*wk[7][0])));
    lm[0] = (vnk[10][0]+((10.7*vnk[9][0])+((57.5*vnk[7][0])+((6.8*vnk[6][0])+(
      410.*u[0])))));
    lm[1] = (vnk[10][1]+((10.7*vnk[9][1])+((57.5*vnk[7][1])+((6.8*vnk[6][1])+(
      410.*u[1])))));
    lm[2] = (vnk[10][2]+((10.7*vnk[9][2])+((57.5*vnk[7][2])+((6.8*vnk[6][2])+(
      410.*u[2])))));
    temp[0] = (((.35*((cnk[6][0][0]*wk[6][0])+(cnk[6][0][1]*wk[6][1])))+(6.8*((
      vnk[6][2]*(rnk[6][1]-com[1]))-(vnk[6][1]*(rnk[6][2]-com[2])))))+((410.*((
      u[2]*(q[1]-com[1]))-(u[1]*(q[2]-com[2]))))+((Cik[3][0][2]*hnk[0][2])+((
      Cik[3][0][0]*hnk[0][0])+(Cik[3][0][1]*hnk[0][1])))));
    temp[1] = (((10.7*((vnk[9][2]*(rnk[9][1]-com[1]))-(vnk[9][1]*(rnk[9][2]-
      com[2]))))+((27.2*(cnk[9][0][2]*wk[9][2]))+((.2*(cnk[9][0][1]*wk[9][1]))+(
      27.2*(cnk[8][0][0]*wk[9][0])))))+(((57.5*((vnk[7][2]*(rnk[7][1]-com[1]))-(
      vnk[7][1]*(rnk[7][2]-com[2]))))+((cnk[7][0][2]*hnk[2][2])+((cnk[6][0][0]*
      hnk[2][0])+(cnk[7][0][1]*hnk[2][1]))))+temp[0]));
    am[0] = ((((.1*((cnk[10][0][0]*wk[10][0])+(cnk[10][0][1]*wk[10][1])))+(.5*(
      cnk[7][0][2]*wk[10][2])))+((vnk[10][2]*(rnk[10][1]-com[1]))-(vnk[10][1]*(
      rnk[10][2]-com[2]))))+temp[1]);
    temp[0] = (((.35*((cnk[6][1][0]*wk[6][0])+(cnk[6][1][1]*wk[6][1])))+(6.8*((
      vnk[6][0]*(rnk[6][2]-com[2]))-(vnk[6][2]*(rnk[6][0]-com[0])))))+((410.*((
      u[0]*(q[2]-com[2]))-(u[2]*(q[0]-com[0]))))+((Cik[3][1][2]*hnk[0][2])+((
      Cik[3][1][0]*hnk[0][0])+(Cik[3][1][1]*hnk[0][1])))));
    temp[1] = (((10.7*((vnk[9][0]*(rnk[9][2]-com[2]))-(vnk[9][2]*(rnk[9][0]-
      com[0]))))+((27.2*(cnk[9][1][2]*wk[9][2]))+((.2*(cnk[9][1][1]*wk[9][1]))+(
      27.2*(cnk[8][1][0]*wk[9][0])))))+(((57.5*((vnk[7][0]*(rnk[7][2]-com[2]))-(
      vnk[7][2]*(rnk[7][0]-com[0]))))+((cnk[7][1][2]*hnk[2][2])+((cnk[6][1][0]*
      hnk[2][0])+(cnk[7][1][1]*hnk[2][1]))))+temp[0]));
    am[1] = ((((.1*((cnk[10][1][0]*wk[10][0])+(cnk[10][1][1]*wk[10][1])))+(.5*(
      cnk[7][1][2]*wk[10][2])))+((vnk[10][0]*(rnk[10][2]-com[2]))-(vnk[10][2]*(
      rnk[10][0]-com[0]))))+temp[1]);
    temp[0] = (((.35*((cnk[6][2][0]*wk[6][0])+(cnk[6][2][1]*wk[6][1])))+(6.8*((
      vnk[6][1]*(rnk[6][0]-com[0]))-(vnk[6][0]*(rnk[6][1]-com[1])))))+((410.*((
      u[1]*(q[0]-com[0]))-(u[0]*(q[1]-com[1]))))+((Cik[3][2][2]*hnk[0][2])+((
      Cik[3][2][0]*hnk[0][0])+(Cik[3][2][1]*hnk[0][1])))));
    temp[1] = (((10.7*((vnk[9][1]*(rnk[9][0]-com[0]))-(vnk[9][0]*(rnk[9][1]-
      com[1]))))+((27.2*(cnk[9][2][2]*wk[9][2]))+((.2*(cnk[9][2][1]*wk[9][1]))+(
      27.2*(cnk[8][2][0]*wk[9][0])))))+(((57.5*((vnk[7][1]*(rnk[7][0]-com[0]))-(
      vnk[7][0]*(rnk[7][1]-com[1]))))+((cnk[7][2][2]*hnk[2][2])+((cnk[6][2][0]*
      hnk[2][0])+(cnk[7][2][1]*hnk[2][1]))))+temp[0]));
    am[2] = ((((.1*((cnk[10][2][0]*wk[10][0])+(cnk[10][2][1]*wk[10][1])))+(.5*(
      cnk[7][2][2]*wk[10][2])))+((vnk[10][1]*(rnk[10][0]-com[0]))-(vnk[10][0]*(
      rnk[10][1]-com[1]))))+temp[1]);
    temp[0] = (((.35*((wk[6][0]*wk[6][0])+(wk[6][1]*wk[6][1])))+(6.8*((vnk[6][2]
      *vnk[6][2])+((vnk[6][0]*vnk[6][0])+(vnk[6][1]*vnk[6][1])))))+((410.*((u[2]
      *u[2])+((u[0]*u[0])+(u[1]*u[1]))))+((hnk[0][2]*u[5])+((hnk[0][0]*u[3])+(
      hnk[0][1]*u[4])))));
    temp[1] = (((10.7*((vnk[9][2]*vnk[9][2])+((vnk[9][0]*vnk[9][0])+(vnk[9][1]*
      vnk[9][1]))))+((27.2*(wk[9][2]*wk[9][2]))+((.2*(wk[9][1]*wk[9][1]))+(27.2*
      (wk[9][0]*wk[9][0])))))+(((57.5*((vnk[7][2]*vnk[7][2])+((vnk[7][0]*
      vnk[7][0])+(vnk[7][1]*vnk[7][1]))))+((hnk[2][2]*wk[7][2])+((hnk[2][0]*
      wk[7][0])+(hnk[2][1]*wk[7][1]))))+temp[0]));
    *ke = (.5*((((.1*((wk[10][0]*wk[10][0])+(wk[10][1]*wk[10][1])))+(.5*(
      wk[10][2]*wk[10][2])))+((vnk[10][2]*vnk[10][2])+((vnk[10][0]*vnk[10][0])+(
      vnk[10][1]*vnk[10][1]))))+temp[1]));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  151 adds/subtracts/negates
                    172 multiplies
                      0 divides
                     21 assignments
*/
}

void sdsys(double *mtoto,
    double cm[3],
    double icm[3][3])
{
/*
Compute system total mass, and instantaneous center of mass and
inertia matrix.

Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
    double ikcnkt[11][3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(20,23);
        return;
    }
    *mtoto = 486.;
    cm[0] = com[0];
    cm[1] = com[1];
    cm[2] = com[2];
    ikcnkt[5][0][0] = ((14.*Cik[3][0][2])+((115.*Cik[3][0][0])-(14.*Cik[3][0][1]
      )));
    ikcnkt[5][0][1] = ((14.*Cik[3][1][2])+((115.*Cik[3][1][0])-(14.*Cik[3][1][1]
      )));
    ikcnkt[5][0][2] = ((14.*Cik[3][2][2])+((115.*Cik[3][2][0])-(14.*Cik[3][2][1]
      )));
    ikcnkt[5][1][0] = (((316.*Cik[3][0][1])-(14.*Cik[3][0][0]))-(34.6*
      Cik[3][0][2]));
    ikcnkt[5][1][1] = (((316.*Cik[3][1][1])-(14.*Cik[3][1][0]))-(34.6*
      Cik[3][1][2]));
    ikcnkt[5][1][2] = (((316.*Cik[3][2][1])-(14.*Cik[3][2][0]))-(34.6*
      Cik[3][2][2]));
    ikcnkt[5][2][0] = ((440.*Cik[3][0][2])+((14.*Cik[3][0][0])-(34.6*
      Cik[3][0][1])));
    ikcnkt[5][2][1] = ((440.*Cik[3][1][2])+((14.*Cik[3][1][0])-(34.6*
      Cik[3][1][1])));
    ikcnkt[5][2][2] = ((440.*Cik[3][2][2])+((14.*Cik[3][2][0])-(34.6*
      Cik[3][2][1])));
    ikcnkt[7][0][0] = (((.41*cnk[7][0][1])+(4.85*cnk[6][0][0]))-(.07*
      cnk[7][0][2]));
    ikcnkt[7][0][1] = (((.41*cnk[7][1][1])+(4.85*cnk[6][1][0]))-(.07*
      cnk[7][1][2]));
    ikcnkt[7][0][2] = (((.41*cnk[7][2][1])+(4.85*cnk[6][2][0]))-(.07*
      cnk[7][2][2]));
    ikcnkt[7][1][0] = ((.54*cnk[7][0][2])+((.41*cnk[6][0][0])+(2.2*cnk[7][0][1])
      ));
    ikcnkt[7][1][1] = ((.54*cnk[7][1][2])+((.41*cnk[6][1][0])+(2.2*cnk[7][1][1])
      ));
    ikcnkt[7][1][2] = ((.54*cnk[7][2][2])+((.41*cnk[6][2][0])+(2.2*cnk[7][2][1])
      ));
    ikcnkt[7][2][0] = ((5.5*cnk[7][0][2])+((.54*cnk[7][0][1])-(.07*cnk[6][0][0])
      ));
    ikcnkt[7][2][1] = ((5.5*cnk[7][1][2])+((.54*cnk[7][1][1])-(.07*cnk[6][1][0])
      ));
    ikcnkt[7][2][2] = ((5.5*cnk[7][2][2])+((.54*cnk[7][2][1])-(.07*cnk[6][2][0])
      ));
    temp[0] = (((.35*((cnk[6][0][0]*cnk[6][0][0])+(cnk[6][0][1]*cnk[6][0][1])))+
      (6.8*((rnk[6][1]*rnk[6][1])+(rnk[6][2]*rnk[6][2]))))+((410.*((q[1]*q[1])+(
      q[2]*q[2])))+((Cik[3][0][2]*ikcnkt[5][2][0])+((Cik[3][0][0]*
      ikcnkt[5][0][0])+(Cik[3][0][1]*ikcnkt[5][1][0])))));
    temp[1] = (((10.7*((rnk[9][1]*rnk[9][1])+(rnk[9][2]*rnk[9][2])))+((27.2*(
      cnk[9][0][2]*cnk[9][0][2]))+((.2*(cnk[9][0][1]*cnk[9][0][1]))+(27.2*(
      cnk[8][0][0]*cnk[8][0][0])))))+(((57.5*((rnk[7][1]*rnk[7][1])+(rnk[7][2]*
      rnk[7][2])))+((cnk[7][0][2]*ikcnkt[7][2][0])+((cnk[6][0][0]*
      ikcnkt[7][0][0])+(cnk[7][0][1]*ikcnkt[7][1][0]))))+temp[0]));
    icm[0][0] = (((((.1*((cnk[10][0][0]*cnk[10][0][0])+(cnk[10][0][1]*
      cnk[10][0][1])))+(.5*(cnk[7][0][2]*cnk[7][0][2])))+((rnk[10][1]*rnk[10][1]
      )+(rnk[10][2]*rnk[10][2])))+temp[1])-(486.*((com[1]*com[1])+(com[2]*com[2]
      ))));
    temp[0] = ((((.35*((cnk[6][0][0]*cnk[6][1][0])+(cnk[6][0][1]*cnk[6][1][1])))
      -(6.8*(rnk[6][0]*rnk[6][1])))+(((Cik[3][0][2]*ikcnkt[5][2][1])+((
      Cik[3][0][0]*ikcnkt[5][0][1])+(Cik[3][0][1]*ikcnkt[5][1][1])))-(410.*(q[0]
      *q[1]))))+(((cnk[7][0][2]*ikcnkt[7][2][1])+((cnk[6][0][0]*ikcnkt[7][0][1])
      +(cnk[7][0][1]*ikcnkt[7][1][1])))-(57.5*(rnk[7][0]*rnk[7][1]))));
    icm[0][1] = ((486.*(com[0]*com[1]))+((((.1*((cnk[10][0][0]*cnk[10][1][0])+(
      cnk[10][0][1]*cnk[10][1][1])))+(.5*(cnk[7][0][2]*cnk[7][1][2])))-(
      rnk[10][0]*rnk[10][1]))+((((27.2*(cnk[9][0][2]*cnk[9][1][2]))+((.2*(
      cnk[9][0][1]*cnk[9][1][1]))+(27.2*(cnk[8][0][0]*cnk[8][1][0]))))-(10.7*(
      rnk[9][0]*rnk[9][1])))+temp[0])));
    temp[0] = ((((.35*((cnk[6][0][0]*cnk[6][2][0])+(cnk[6][0][1]*cnk[6][2][1])))
      -(6.8*(rnk[6][0]*rnk[6][2])))+(((Cik[3][0][2]*ikcnkt[5][2][2])+((
      Cik[3][0][0]*ikcnkt[5][0][2])+(Cik[3][0][1]*ikcnkt[5][1][2])))-(410.*(q[0]
      *q[2]))))+(((cnk[7][0][2]*ikcnkt[7][2][2])+((cnk[6][0][0]*ikcnkt[7][0][2])
      +(cnk[7][0][1]*ikcnkt[7][1][2])))-(57.5*(rnk[7][0]*rnk[7][2]))));
    icm[0][2] = ((486.*(com[0]*com[2]))+((((.1*((cnk[10][0][0]*cnk[10][2][0])+(
      cnk[10][0][1]*cnk[10][2][1])))+(.5*(cnk[7][0][2]*cnk[7][2][2])))-(
      rnk[10][0]*rnk[10][2]))+((((27.2*(cnk[9][0][2]*cnk[9][2][2]))+((.2*(
      cnk[9][0][1]*cnk[9][2][1]))+(27.2*(cnk[8][0][0]*cnk[8][2][0]))))-(10.7*(
      rnk[9][0]*rnk[9][2])))+temp[0])));
    icm[1][0] = icm[0][1];
    temp[0] = (((.35*((cnk[6][1][0]*cnk[6][1][0])+(cnk[6][1][1]*cnk[6][1][1])))+
      (6.8*((rnk[6][0]*rnk[6][0])+(rnk[6][2]*rnk[6][2]))))+((410.*((q[0]*q[0])+(
      q[2]*q[2])))+((Cik[3][1][2]*ikcnkt[5][2][1])+((Cik[3][1][0]*
      ikcnkt[5][0][1])+(Cik[3][1][1]*ikcnkt[5][1][1])))));
    temp[1] = (((10.7*((rnk[9][0]*rnk[9][0])+(rnk[9][2]*rnk[9][2])))+((27.2*(
      cnk[9][1][2]*cnk[9][1][2]))+((.2*(cnk[9][1][1]*cnk[9][1][1]))+(27.2*(
      cnk[8][1][0]*cnk[8][1][0])))))+(((57.5*((rnk[7][0]*rnk[7][0])+(rnk[7][2]*
      rnk[7][2])))+((cnk[7][1][2]*ikcnkt[7][2][1])+((cnk[6][1][0]*
      ikcnkt[7][0][1])+(cnk[7][1][1]*ikcnkt[7][1][1]))))+temp[0]));
    icm[1][1] = (((((.1*((cnk[10][1][0]*cnk[10][1][0])+(cnk[10][1][1]*
      cnk[10][1][1])))+(.5*(cnk[7][1][2]*cnk[7][1][2])))+((rnk[10][0]*rnk[10][0]
      )+(rnk[10][2]*rnk[10][2])))+temp[1])-(486.*((com[0]*com[0])+(com[2]*com[2]
      ))));
    temp[0] = ((((.35*((cnk[6][1][0]*cnk[6][2][0])+(cnk[6][1][1]*cnk[6][2][1])))
      -(6.8*(rnk[6][1]*rnk[6][2])))+(((Cik[3][1][2]*ikcnkt[5][2][2])+((
      Cik[3][1][0]*ikcnkt[5][0][2])+(Cik[3][1][1]*ikcnkt[5][1][2])))-(410.*(q[1]
      *q[2]))))+(((cnk[7][1][2]*ikcnkt[7][2][2])+((cnk[6][1][0]*ikcnkt[7][0][2])
      +(cnk[7][1][1]*ikcnkt[7][1][2])))-(57.5*(rnk[7][1]*rnk[7][2]))));
    icm[1][2] = ((486.*(com[1]*com[2]))+((((.1*((cnk[10][1][0]*cnk[10][2][0])+(
      cnk[10][1][1]*cnk[10][2][1])))+(.5*(cnk[7][1][2]*cnk[7][2][2])))-(
      rnk[10][1]*rnk[10][2]))+((((27.2*(cnk[9][1][2]*cnk[9][2][2]))+((.2*(
      cnk[9][1][1]*cnk[9][2][1]))+(27.2*(cnk[8][1][0]*cnk[8][2][0]))))-(10.7*(
      rnk[9][1]*rnk[9][2])))+temp[0])));
    icm[2][0] = icm[0][2];
    icm[2][1] = icm[1][2];
    temp[0] = (((.35*((cnk[6][2][0]*cnk[6][2][0])+(cnk[6][2][1]*cnk[6][2][1])))+
      (6.8*((rnk[6][0]*rnk[6][0])+(rnk[6][1]*rnk[6][1]))))+((410.*((q[0]*q[0])+(
      q[1]*q[1])))+((Cik[3][2][2]*ikcnkt[5][2][2])+((Cik[3][2][0]*
      ikcnkt[5][0][2])+(Cik[3][2][1]*ikcnkt[5][1][2])))));
    temp[1] = (((10.7*((rnk[9][0]*rnk[9][0])+(rnk[9][1]*rnk[9][1])))+((27.2*(
      cnk[9][2][2]*cnk[9][2][2]))+((.2*(cnk[9][2][1]*cnk[9][2][1]))+(27.2*(
      cnk[8][2][0]*cnk[8][2][0])))))+(((57.5*((rnk[7][0]*rnk[7][0])+(rnk[7][1]*
      rnk[7][1])))+((cnk[7][2][2]*ikcnkt[7][2][2])+((cnk[6][2][0]*
      ikcnkt[7][0][2])+(cnk[7][2][1]*ikcnkt[7][1][2]))))+temp[0]));
    icm[2][2] = (((((.1*((cnk[10][2][0]*cnk[10][2][0])+(cnk[10][2][1]*
      cnk[10][2][1])))+(.5*(cnk[7][2][2]*cnk[7][2][2])))+((rnk[10][0]*rnk[10][0]
      )+(rnk[10][1]*rnk[10][1])))+temp[1])-(486.*((com[0]*com[0])+(com[1]*com[1]
      ))));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  168 adds/subtracts/negates
                    258 multiplies
                      0 divides
                     40 assignments
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

    if ((coord < 0) || (coord > 10)) {
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
    if (body == gnd) {
        x = -1;
    } else {
        x = firstq[body]+njntdof[body]-1;
    }
    if (x < coord) {
        sdvset(0.,0.,0.,linchg);
        sdvset(0.,0.,0.,rotchg);
        return;
    }
    sddovpk();
    for (i = 0; i < 3; i++) {
        rotchg[i] = Wpk[coord][x][i];
        lin[i] = Vpk[coord][x][i];
    }
    if (body == gnd) {
        sdvcopy(point,pv);
    } else {
        pv[0] = rcom[body][0]+point[0];
        pv[1] = rcom[body][1]+point[1];
        pv[2] = rcom[body][2]+point[2];
    }
    sdvcross(rotchg,pv,linchg);
    sdvadd(linchg,lin,linchg);
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
    info[11] = 1;
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
/* There are no user constraints in this problem. */
}

void sdgentime(int *gentm)
{

    *gentm = TIMESTAMP;
}
/*
Done. CPU seconds used: TIMESTAMP  Memory used: TIMESTAMP bytes.
Equation complexity:
  sdstate:   233 adds   346 multiplies     1 divides   337 assignments
  sdderiv:  1518 adds  1844 multiplies    13 divides  1821 assignments
  sdresid:   538 adds   571 multiplies     0 divides   473 assignments
  sdreac:    206 adds   120 multiplies     0 divides   186 assignments
  sdmom:     151 adds   172 multiplies     0 divides    21 assignments
  sdsys:     168 adds   258 multiplies     0 divides    40 assignments
*/
