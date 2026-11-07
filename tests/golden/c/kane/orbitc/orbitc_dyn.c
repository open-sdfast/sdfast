/*
Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)


ROADMAP (orbitc.sd)

Bodies        Inb
No  Name      body Joint type  Coords q         Multipliers
--- --------- ---- ----------- ---------------- -----------------------
 -1 $ground                                    |
  0 A          -1  Pin           0p            |  0p
  1 B           0  Planar        1   2   3     |
  2 C           1  Pin           4             |
  3 D           2  Pin           5             |
  4 E           3  Pin           6             |
  5 F           4  Pin           7             |

*/
#include <math.h>
#include <stdio.h>

typedef struct {
    int ground_,nbod_,ndof_,ncons_,nloop_,nldof_,nloopc_,nball_,nlball_,npres_,
      nuser_;
    int jtype_[6],inb_[6],outb_[6],njntdof_[6],njntc_[6],njntp_[6],firstq_[6],
      ballq_[6],firstm_[6],firstp_[6];
    int trans_[8];
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
    double grav_[3],mk_[6],ik_[6][3][3],pin_[8][3];
    double rk_[6][3],ri_[6][3],pres_[8],stabvel_,stabpos_;
    int mfrcflg_,roustate_,vpkflg_,inerflg_,mmflg_,mmlduflg_,wwflg_,ltauflg_,
      fs0flg_,ii_,mmap_[8];
    int gravq_[3],mkq_[6],ikq_[6][3][3],pinq_[8][3],rkq_[6][3],riq_[6][3],presq_
      [8],stabvelq_,stabposq_;
    double mtot_,psmkg_,rhead_[6][3],rcom_[6][3],mkrcomt_[6][3][3],psikg_[3][3],
      psrcomg_[3],psrkg_[3],psrig_[3],psmk_[8],psik_[8][3][3],psrcom_[8][3],
      psrk_[8][3],psri_[8][3];
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
    double curtim_,q_[8],qn_[8],u_[8],cnk_[8][3][3],cnb_[6][3][3];
    double rnk_[8][3],vnk_[8][3],wk_[8][3],rnb_[6][3],vnb_[6][3],wb_[6][3],
      wbrcom_[6][3],com_[3],rnkg_[3];
    double Cik_[8][3][3],rikt_[8][3][3],Iko_[8][3][3],mkrk_[8][3][3],Cib_[6][3][
      3];
    double Wkk_[8][3],Vkk_[8][3],dik_[8][3],rpp_[8][3],rpk_[8][3],rik_[8][3],
      rik2_[8][3];
    double rpri_[8][3],Wik_[8][3],Vik_[8][3],Wirk_[8][3],rkWkk_[8][3],Wkrpk_[8][
      3],VikWkr_[8][3];
    double perr_[1],verr_[1],aerr_[1],mult_[1],ufk_[6][3],utk_[6][3],mfk_[6][3],
      mtk_[6][3];
    double utau_[8],mtau_[8],uacc_[8],uvel_[8],upos_[8];
    double s0_,c0_,s3_,c3_,s4_,c4_,s5_,c5_,s6_,c6_,s7_,c7_;
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
#define s3 (sdgstate.s3_)
#define c3 (sdgstate.c3_)
#define s4 (sdgstate.s4_)
#define c4 (sdgstate.c4_)
#define s5 (sdgstate.s5_)
#define c5 (sdgstate.c5_)
#define s6 (sdgstate.s6_)
#define c6 (sdgstate.c6_)
#define s7 (sdgstate.s7_)
#define c7 (sdgstate.c7_)

typedef struct {
    double fs0_[8],qdot_[8],Otk_[8][3],Atk_[8][3],AiOiWi_[8][3],Fstar_[8][3];
    double Tstar_[8][3],Fstark_[8][3],Tstark_[8][3],IkWk_[8][3],WkIkWk_[8][3],
      gk_[8][3],IkbWk_[6][3],WkIkbWk_[6][3];
    double w0w0_[6],w1w1_[6],w2w2_[6],w0w1_[6],w0w2_[6],w1w2_[6];
    double w00w11_[6],w00w22_[6],w11w22_[6],ww_[1][1],qraux_[1];
    double mm_[8][8],mlo_[8][8],mdi_[8],IkWpk_[8][8][3],works_[8],workss_[8][8];
    double Wpk_[8][8][3],Vpk_[8][8][3],VWri_[8][8][3];
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
    double fs_[8],udot_[8],tauc_[8],dyad_[6][3][3],fc_[8][3],tc_[8][3];
    double ank_[8][3],onk_[8][3],Onkb_[8][3],AOnkri_[8][3],Ankb_[8][3],AnkAtk_[8
      ][3],anb_[6][3],onb_[6][3],dyrcom_[6][3];
    double ffk_[8][3],ttk_[8][3],fccikt_[8][3],ffkb_[6][3],ttkb_[6][3];
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
    /* nbod */ 6,
    /* ndof */ 8,
    /* ncons */ 1,
    /* nloop */ 0,
    /* nldof */ 0,
    /* nloopc */ 0,
    /* nball */ 0,
    /* nlball */ 0,
    /* npres */ 1,
    /* nuser */ 0,
    /* jtype[0] */ 1,
    /* jtype[1] */ 8,
    /* jtype[2] */ 1,
    /* jtype[3] */ 1,
    /* jtype[4] */ 1,
    /* jtype[5] */ 1,
    /* inb[0] */ -1,
    /* inb[1] */ 0,
    /* inb[2] */ 1,
    /* inb[3] */ 2,
    /* inb[4] */ 3,
    /* inb[5] */ 4,
    /* outb[0] */ 0,
    /* outb[1] */ 1,
    /* outb[2] */ 2,
    /* outb[3] */ 3,
    /* outb[4] */ 4,
    /* outb[5] */ 5,
    /* njntdof[0] */ 1,
    /* njntdof[1] */ 3,
    /* njntdof[2] */ 1,
    /* njntdof[3] */ 1,
    /* njntdof[4] */ 1,
    /* njntdof[5] */ 1,
    /* njntc[0] */ 0,
    /* njntc[1] */ 0,
    /* njntc[2] */ 0,
    /* njntc[3] */ 0,
    /* njntc[4] */ 0,
    /* njntc[5] */ 0,
    /* njntp[0] */ 1,
    /* njntp[1] */ 0,
    /* njntp[2] */ 0,
    /* njntp[3] */ 0,
    /* njntp[4] */ 0,
    /* njntp[5] */ 0,
    /* firstq[0] */ 0,
    /* firstq[1] */ 1,
    /* firstq[2] */ 4,
    /* firstq[3] */ 5,
    /* firstq[4] */ 6,
    /* firstq[5] */ 7,
    /* ballq[0] */ -104,
    /* ballq[1] */ -104,
    /* ballq[2] */ -104,
    /* ballq[3] */ -104,
    /* ballq[4] */ -104,
    /* ballq[5] */ -104,
    /* firstm[0] */ -1,
    /* firstm[1] */ -1,
    /* firstm[2] */ -1,
    /* firstm[3] */ -1,
    /* firstm[4] */ -1,
    /* firstm[5] */ -1,
    /* firstp[0] */ 0,
    /* firstp[1] */ -1,
    /* firstp[2] */ -1,
    /* firstp[3] */ -1,
    /* firstp[4] */ -1,
    /* firstp[5] */ -1,
    /* trans[0] */ 0,
    /* trans[1] */ 1,
    /* trans[2] */ 1,
    /* trans[3] */ 0,
    /* trans[4] */ 0,
    /* trans[5] */ 0,
    /* trans[6] */ 0,
    /* trans[7] */ 0,
};
sdginput_t sdginput = {
/* Model parameters from the input file */

/* gravity */
    /* grav[0] */ 0.,
    /* grav[1] */ 0.,
    /* grav[2] */ 0.,

/* mass */
    /* mk[0] */ 0.,
    /* mk[1] */ 1.,
    /* mk[2] */ 1.,
    /* mk[3] */ 1.,
    /* mk[4] */ 1.,
    /* mk[5] */ 1.,

/* inertia */
    /* ik[0][0][0] */ 1.,
    /* ik[0][0][1] */ 0.,
    /* ik[0][0][2] */ 0.,
    /* ik[0][1][0] */ 0.,
    /* ik[0][1][1] */ 1.,
    /* ik[0][1][2] */ 0.,
    /* ik[0][2][0] */ 0.,
    /* ik[0][2][1] */ 0.,
    /* ik[0][2][2] */ 1.,
    /* ik[1][0][0] */ 10.,
    /* ik[1][0][1] */ 0.,
    /* ik[1][0][2] */ 0.,
    /* ik[1][1][0] */ 0.,
    /* ik[1][1][1] */ 50.,
    /* ik[1][1][2] */ 0.,
    /* ik[1][2][0] */ 0.,
    /* ik[1][2][1] */ 0.,
    /* ik[1][2][2] */ 50.,
    /* ik[2][0][0] */ 10.,
    /* ik[2][0][1] */ 0.,
    /* ik[2][0][2] */ 0.,
    /* ik[2][1][0] */ 0.,
    /* ik[2][1][1] */ 50.,
    /* ik[2][1][2] */ 0.,
    /* ik[2][2][0] */ 0.,
    /* ik[2][2][1] */ 0.,
    /* ik[2][2][2] */ 50.,
    /* ik[3][0][0] */ 10.,
    /* ik[3][0][1] */ 0.,
    /* ik[3][0][2] */ 0.,
    /* ik[3][1][0] */ 0.,
    /* ik[3][1][1] */ 50.,
    /* ik[3][1][2] */ 0.,
    /* ik[3][2][0] */ 0.,
    /* ik[3][2][1] */ 0.,
    /* ik[3][2][2] */ 50.,
    /* ik[4][0][0] */ 10.,
    /* ik[4][0][1] */ 0.,
    /* ik[4][0][2] */ 0.,
    /* ik[4][1][0] */ 0.,
    /* ik[4][1][1] */ 50.,
    /* ik[4][1][2] */ 0.,
    /* ik[4][2][0] */ 0.,
    /* ik[4][2][1] */ 0.,
    /* ik[4][2][2] */ 50.,
    /* ik[5][0][0] */ 10.,
    /* ik[5][0][1] */ 0.,
    /* ik[5][0][2] */ 0.,
    /* ik[5][1][0] */ 0.,
    /* ik[5][1][1] */ 50.,
    /* ik[5][1][2] */ 0.,
    /* ik[5][2][0] */ 0.,
    /* ik[5][2][1] */ 0.,
    /* ik[5][2][2] */ 50.,

/* tree hinge axis vectors */
    /* pin[0][0] */ 0.,
    /* pin[0][1] */ 0.,
    /* pin[0][2] */ 1.,
    /* pin[1][0] */ 1.,
    /* pin[1][1] */ 0.,
    /* pin[1][2] */ 0.,
    /* pin[2][0] */ 0.,
    /* pin[2][1] */ 1.,
    /* pin[2][2] */ 0.,
    /* pin[3][0] */ 0.,
    /* pin[3][1] */ 0.,
    /* pin[3][2] */ 1.,
    /* pin[4][0] */ 0.,
    /* pin[4][1] */ 0.,
    /* pin[4][2] */ 1.,
    /* pin[5][0] */ 0.,
    /* pin[5][1] */ 0.,
    /* pin[5][2] */ 1.,
    /* pin[6][0] */ 0.,
    /* pin[6][1] */ 0.,
    /* pin[6][2] */ 1.,
    /* pin[7][0] */ 0.,
    /* pin[7][1] */ 0.,
    /* pin[7][2] */ 1.,

/* tree bodytojoint vectors */
    /* rk[0][0] */ 0.,
    /* rk[0][1] */ 0.,
    /* rk[0][2] */ 0.,
    /* rk[1][0] */ 0.,
    /* rk[1][1] */ -1.,
    /* rk[1][2] */ 0.,
    /* rk[2][0] */ 0.,
    /* rk[2][1] */ -1.,
    /* rk[2][2] */ 0.,
    /* rk[3][0] */ 0.,
    /* rk[3][1] */ -1.,
    /* rk[3][2] */ 0.,
    /* rk[4][0] */ 0.,
    /* rk[4][1] */ -1.,
    /* rk[4][2] */ 0.,
    /* rk[5][0] */ 0.,
    /* rk[5][1] */ -1.,
    /* rk[5][2] */ 0.,

/* tree inbtojoint vectors */
    /* ri[0][0] */ 0.,
    /* ri[0][1] */ 0.,
    /* ri[0][2] */ 0.,
    /* ri[1][0] */ 0.,
    /* ri[1][1] */ 0.,
    /* ri[1][2] */ 0.,
    /* ri[2][0] */ 0.,
    /* ri[2][1] */ 1.,
    /* ri[2][2] */ 0.,
    /* ri[3][0] */ 0.,
    /* ri[3][1] */ 1.,
    /* ri[3][2] */ 0.,
    /* ri[4][0] */ 0.,
    /* ri[4][1] */ 1.,
    /* ri[4][2] */ 0.,
    /* ri[5][0] */ 0.,
    /* ri[5][1] */ 1.,
    /* ri[5][2] */ 0.,

/* tree prescribed motion */
    /* pres[0] */ 1.,
    /* pres[1] */ 0.,
    /* pres[2] */ 0.,
    /* pres[3] */ 0.,
    /* pres[4] */ 0.,
    /* pres[5] */ 0.,
    /* pres[6] */ 0.,
    /* pres[7] */ 0.,

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

/* Which parameters were "?" (1) or "<nominal>?" (3) */
    /* gravq[0] */ 0,
    /* gravq[1] */ 0,
    /* gravq[2] */ 0,
    /* mkq[0] */ 0,
    /* mkq[1] */ 0,
    /* mkq[2] */ 0,
    /* mkq[3] */ 0,
    /* mkq[4] */ 0,
    /* mkq[5] */ 0,
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
    /* ikq[5][0][0] */ 0,
    /* ikq[5][0][1] */ 0,
    /* ikq[5][0][2] */ 0,
    /* ikq[5][1][0] */ 0,
    /* ikq[5][1][1] */ 0,
    /* ikq[5][1][2] */ 0,
    /* ikq[5][2][0] */ 0,
    /* ikq[5][2][1] */ 0,
    /* ikq[5][2][2] */ 0,
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
    /* rkq[5][0] */ 0,
    /* rkq[5][1] */ 0,
    /* rkq[5][2] */ 0,
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
    /* riq[4][1] */ 0,
    /* riq[4][2] */ 0,
    /* riq[5][0] */ 0,
    /* riq[5][1] */ 0,
    /* riq[5][2] */ 0,
    /* presq[0] */ 0,
    /* presq[1] */ 0,
    /* presq[2] */ 0,
    /* presq[3] */ 0,
    /* presq[4] */ 0,
    /* presq[5] */ 0,
    /* presq[6] */ 0,
    /* presq[7] */ 0,
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
    for (k = 0; k < 6; k++) {
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
    for (k = 0; k < 8; k++) {
        for (i = 0; i < 3; i++) {
            if (pinq[k][i] == 1) {
                sdseterr(7,28);
            }
        }
    }

/* Normalize pin vectors if necessary */


/* Zero out Vpk and Wpk */

    for (i = 0; i < 8; i++) {
        for (j = i; j <= 7; j++) {
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
    rcom[5][0] = 0.;
    rcom[5][1] = 0.;
    rcom[5][2] = 0.;

/* Compute mass properties-related constants */

    mtot = 5.;
    sdserialno(&i);
    if (i != 10001) {
        sdseterr(7,41);
    }
    roustate = 1;
}

/* Convert state to form using 1-2-3 Euler angles for ball joints. */

void sdst2ang(double st[8],
    double stang[8])
{
    int i;

    for (i = 0; i < 8; i++) {
        stang[i] = st[i];
    }
}

/* Convert 1-2-3 form of state back to Euler parameters for ball joints. */

void sdang2st(double stang[8],
    double st[8])
{
    int i;

    for (i = 0; i < 8; i++) {
        st[i] = stang[i];
    }
}

/* Normalize Euler parameters in state. */

void sdnrmsterr(double st[8],
    double normst[8],
    int routine)
{
    int i;

    for (i = 0; i < 8; i++) {
        normst[i] = st[i];
    }
}

void sdnormst(double st[8],
    double normst[8])
{

    sdnrmsterr(st,normst,0);
}

void sdstate(double timein,
    double qin[8],
    double uin[8])
{
/*
Compute kinematic information and store it in sdgstate.

Generated TIMESTAMP by SD/FAST, Kane's formulation
(sdfast 2.0 #10001)
*/
    int i,j,qchg,uchg;

    if ((roustate != 1) && (roustate != 2) && (roustate != 3)) {
        sdseterr(8,22);
        return;
    }
    if (roustate == 1) {
        for (i = 0; i < 8; i++) {
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
        for (i = 0; i < 8; i++) {
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
        for (i = 0; i < 8; i++) {
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
    for (i = 0; i < 8; i++) {
        q[i] = qin[i];
    }
/*
Compute sines and cosines of q
*/
    s0 = sin(q[0]);
    c0 = cos(q[0]);
    s3 = sin(q[3]);
    c3 = cos(q[3]);
    s4 = sin(q[4]);
    c4 = cos(q[4]);
    s5 = sin(q[5]);
    c5 = cos(q[5]);
    s6 = sin(q[6]);
    c6 = cos(q[6]);
    s7 = sin(q[7]);
    c7 = cos(q[7]);
/*
Compute across-axis direction cosines Cik
*/
/*
Compute across-joint direction cosines Cib
*/
/*
Compute gravity
*/
/*
Compute cnk & cnb (direction cosines in N)
*/
    cnk[3][0][0] = ((c0*c3)-(s0*s3));
    cnk[3][0][1] = -((s0*c3)+(s3*c0));
    cnk[3][1][0] = ((s0*c3)+(s3*c0));
    cnk[3][1][1] = ((c0*c3)-(s0*s3));
    cnk[4][0][0] = ((cnk[3][0][0]*c4)+(cnk[3][0][1]*s4));
    cnk[4][0][1] = ((cnk[3][0][1]*c4)-(cnk[3][0][0]*s4));
    cnk[4][1][0] = ((cnk[3][1][0]*c4)+(cnk[3][1][1]*s4));
    cnk[4][1][1] = ((cnk[3][1][1]*c4)-(cnk[3][1][0]*s4));
    cnk[5][0][0] = ((cnk[4][0][0]*c5)+(cnk[4][0][1]*s5));
    cnk[5][0][1] = ((cnk[4][0][1]*c5)-(cnk[4][0][0]*s5));
    cnk[5][1][0] = ((cnk[4][1][0]*c5)+(cnk[4][1][1]*s5));
    cnk[5][1][1] = ((cnk[4][1][1]*c5)-(cnk[4][1][0]*s5));
    cnk[6][0][0] = ((cnk[5][0][0]*c6)+(cnk[5][0][1]*s6));
    cnk[6][0][1] = ((cnk[5][0][1]*c6)-(cnk[5][0][0]*s6));
    cnk[6][1][0] = ((cnk[5][1][0]*c6)+(cnk[5][1][1]*s6));
    cnk[6][1][1] = ((cnk[5][1][1]*c6)-(cnk[5][1][0]*s6));
    cnk[7][0][0] = ((cnk[6][0][0]*c7)+(cnk[6][0][1]*s7));
    cnk[7][0][1] = ((cnk[6][0][1]*c7)-(cnk[6][0][0]*s7));
    cnk[7][1][0] = ((cnk[6][1][0]*c7)+(cnk[6][1][1]*s7));
    cnk[7][1][1] = ((cnk[6][1][1]*c7)-(cnk[6][1][0]*s7));
    cnb[0][0][0] = c0;
    cnb[0][0][1] = -s0;
    cnb[0][0][2] = 0.;
    cnb[0][1][0] = s0;
    cnb[0][1][1] = c0;
    cnb[0][1][2] = 0.;
    cnb[0][2][0] = 0.;
    cnb[0][2][1] = 0.;
    cnb[0][2][2] = 1.;
    cnb[1][0][0] = cnk[3][0][0];
    cnb[1][0][1] = cnk[3][0][1];
    cnb[1][0][2] = 0.;
    cnb[1][1][0] = cnk[3][1][0];
    cnb[1][1][1] = cnk[3][1][1];
    cnb[1][1][2] = 0.;
    cnb[1][2][0] = 0.;
    cnb[1][2][1] = 0.;
    cnb[1][2][2] = 1.;
    cnb[2][0][0] = cnk[4][0][0];
    cnb[2][0][1] = cnk[4][0][1];
    cnb[2][0][2] = 0.;
    cnb[2][1][0] = cnk[4][1][0];
    cnb[2][1][1] = cnk[4][1][1];
    cnb[2][1][2] = 0.;
    cnb[2][2][0] = 0.;
    cnb[2][2][1] = 0.;
    cnb[2][2][2] = 1.;
    cnb[3][0][0] = cnk[5][0][0];
    cnb[3][0][1] = cnk[5][0][1];
    cnb[3][0][2] = 0.;
    cnb[3][1][0] = cnk[5][1][0];
    cnb[3][1][1] = cnk[5][1][1];
    cnb[3][1][2] = 0.;
    cnb[3][2][0] = 0.;
    cnb[3][2][1] = 0.;
    cnb[3][2][2] = 1.;
    cnb[4][0][0] = cnk[6][0][0];
    cnb[4][0][1] = cnk[6][0][1];
    cnb[4][0][2] = 0.;
    cnb[4][1][0] = cnk[6][1][0];
    cnb[4][1][1] = cnk[6][1][1];
    cnb[4][1][2] = 0.;
    cnb[4][2][0] = 0.;
    cnb[4][2][1] = 0.;
    cnb[4][2][2] = 1.;
    cnb[5][0][0] = cnk[7][0][0];
    cnb[5][0][1] = cnk[7][0][1];
    cnb[5][0][2] = 0.;
    cnb[5][1][0] = cnk[7][1][0];
    cnb[5][1][1] = cnk[7][1][1];
    cnb[5][1][2] = 0.;
    cnb[5][2][0] = 0.;
    cnb[5][2][1] = 0.;
    cnb[5][2][2] = 1.;
/*
Compute q-related auxiliary variables
*/
    rik[4][1] = (1.+c4);
    rik[5][1] = (1.+c5);
    rik[6][1] = (1.+c6);
    rik[7][1] = (1.+c7);
/*
Compute rnk & rnb (mass center locations in N)
*/
    rnk[1][0] = (q[1]*c0);
    rnk[1][1] = (q[1]*s0);
    rnk[2][0] = (rnk[1][0]-(q[2]*s0));
    rnk[2][1] = (rnk[1][1]+(q[2]*c0));
    rnk[3][0] = (cnk[3][0][1]+rnk[2][0]);
    rnk[3][1] = (cnk[3][1][1]+rnk[2][1]);
    rnk[4][0] = (cnk[4][0][1]+(cnk[3][0][1]+rnk[3][0]));
    rnk[4][1] = (cnk[4][1][1]+(cnk[3][1][1]+rnk[3][1]));
    rnk[5][0] = (cnk[5][0][1]+(cnk[4][0][1]+rnk[4][0]));
    rnk[5][1] = (cnk[5][1][1]+(cnk[4][1][1]+rnk[4][1]));
    rnk[6][0] = (cnk[6][0][1]+(cnk[5][0][1]+rnk[5][0]));
    rnk[6][1] = (cnk[6][1][1]+(cnk[5][1][1]+rnk[5][1]));
    rnk[7][0] = (cnk[7][0][1]+(cnk[6][0][1]+rnk[6][0]));
    rnk[7][1] = (cnk[7][1][1]+(cnk[6][1][1]+rnk[6][1]));
    rnb[0][0] = 0.;
    rnb[0][1] = 0.;
    rnb[0][2] = 0.;
    rnb[1][0] = rnk[3][0];
    rnb[1][1] = rnk[3][1];
    rnb[1][2] = 0.;
    rnb[2][0] = rnk[4][0];
    rnb[2][1] = rnk[4][1];
    rnb[2][2] = 0.;
    rnb[3][0] = rnk[5][0];
    rnb[3][1] = rnk[5][1];
    rnb[3][2] = 0.;
    rnb[4][0] = rnk[6][0];
    rnb[4][1] = rnk[6][1];
    rnb[4][2] = 0.;
    rnb[5][0] = rnk[7][0];
    rnb[5][1] = rnk[7][1];
    rnb[5][2] = 0.;
/*
Compute com (system mass center location in N)
*/
    com[0] = (.2*(rnk[7][0]+(rnk[6][0]+(rnk[5][0]+(rnk[3][0]+rnk[4][0])))));
    com[1] = (.2*(rnk[7][1]+(rnk[6][1]+(rnk[5][1]+(rnk[3][1]+rnk[4][1])))));
    com[2] = 0.;
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
    for (i = 0; i < 8; i++) {
        u[i] = uin[i];
    }
/*
Compute u-related auxiliary variables
*/
/*
Compute wk & wb (angular velocities)
*/
    wk[3][2] = (u[0]+u[3]);
    wk[4][2] = (u[4]+wk[3][2]);
    wk[5][2] = (u[5]+wk[4][2]);
    wk[6][2] = (u[6]+wk[5][2]);
    wk[7][2] = (u[7]+wk[6][2]);
    wb[0][0] = 0.;
    wb[0][1] = 0.;
    wb[0][2] = u[0];
    wb[1][0] = 0.;
    wb[1][1] = 0.;
    wb[1][2] = wk[3][2];
    wb[2][0] = 0.;
    wb[2][1] = 0.;
    wb[2][2] = wk[4][2];
    wb[3][0] = 0.;
    wb[3][1] = 0.;
    wb[3][2] = wk[5][2];
    wb[4][0] = 0.;
    wb[4][1] = 0.;
    wb[4][2] = wk[6][2];
    wb[5][0] = 0.;
    wb[5][1] = 0.;
    wb[5][2] = wk[7][2];
/*
Compute auxiliary variables involving wk
*/
    Wkrpk[1][1] = (q[1]*u[0]);
    Wkrpk[2][0] = -(q[2]*u[0]);
/*
Compute temporaries for use in SDRHS
*/
    w2w2[0] = (u[0]*u[0]);
    w2w2[1] = (wk[3][2]*wk[3][2]);
    w2w2[2] = (wk[4][2]*wk[4][2]);
    w2w2[3] = (wk[5][2]*wk[5][2]);
    w2w2[4] = (wk[6][2]*wk[6][2]);
    w2w2[5] = (wk[7][2]*wk[7][2]);
/*
Compute vnk & vnb (mass center linear velocities in N)
*/
    vnk[1][0] = ((u[1]*c0)-(Wkrpk[1][1]*s0));
    vnk[1][1] = ((u[1]*s0)+(Wkrpk[1][1]*c0));
    vnk[2][0] = (vnk[1][0]+((Wkrpk[2][0]*c0)-(u[2]*s0)));
    vnk[2][1] = (vnk[1][1]+((u[2]*c0)+(Wkrpk[2][0]*s0)));
    vnk[3][0] = (vnk[2][0]-(cnk[3][0][0]*wk[3][2]));
    vnk[3][1] = (vnk[2][1]-(cnk[3][1][0]*wk[3][2]));
    vnk[4][0] = ((vnk[3][0]-(cnk[3][0][0]*wk[3][2]))-(cnk[4][0][0]*wk[4][2]));
    vnk[4][1] = ((vnk[3][1]-(cnk[3][1][0]*wk[3][2]))-(cnk[4][1][0]*wk[4][2]));
    vnk[5][0] = ((vnk[4][0]-(cnk[4][0][0]*wk[4][2]))-(cnk[5][0][0]*wk[5][2]));
    vnk[5][1] = ((vnk[4][1]-(cnk[4][1][0]*wk[4][2]))-(cnk[5][1][0]*wk[5][2]));
    vnk[6][0] = ((vnk[5][0]-(cnk[5][0][0]*wk[5][2]))-(cnk[6][0][0]*wk[6][2]));
    vnk[6][1] = ((vnk[5][1]-(cnk[5][1][0]*wk[5][2]))-(cnk[6][1][0]*wk[6][2]));
    vnk[7][0] = ((vnk[6][0]-(cnk[6][0][0]*wk[6][2]))-(cnk[7][0][0]*wk[7][2]));
    vnk[7][1] = ((vnk[6][1]-(cnk[6][1][0]*wk[6][2]))-(cnk[7][1][0]*wk[7][2]));
    vnb[0][0] = 0.;
    vnb[0][1] = 0.;
    vnb[0][2] = 0.;
    vnb[1][0] = vnk[3][0];
    vnb[1][1] = vnk[3][1];
    vnb[1][2] = 0.;
    vnb[2][0] = vnk[4][0];
    vnb[2][1] = vnk[4][1];
    vnb[2][2] = 0.;
    vnb[3][0] = vnk[5][0];
    vnb[3][1] = vnk[5][1];
    vnb[3][2] = 0.;
    vnb[4][0] = vnk[6][0];
    vnb[4][1] = vnk[6][1];
    vnb[4][2] = 0.;
    vnb[5][0] = vnk[7][0];
    vnb[5][1] = vnk[7][1];
    vnb[5][2] = 0.;
/*
Compute qdot (kinematical equations)
*/
    qdot[0] = u[0];
    qdot[1] = u[1];
    qdot[2] = u[2];
    qdot[3] = u[3];
    qdot[4] = u[4];
    qdot[5] = u[5];
    qdot[6] = u[6];
    qdot[7] = u[7];
/*
Compute constraint velocity errors
*/
    skipus: ;
/*
Initialize applied forces and torques to zero
*/
    for (i = 0; i < 6; i++) {
        for (j = 0; j < 3; j++) {
            ufk[i][j] = 0.;
            utk[i][j] = 0.;
        }
    }
    for (i = 0; i < 8; i++) {
        utau[i] = 0.;
    }
    ltauflg = 0;
    fs0flg = 0;
/*
Initialize prescribed motions
*/
    uacc[0] = 0.;
    uvel[0] = u[0];
    upos[0] = q[0];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   84 adds/subtracts/negates
                     80 multiplies
                      0 divides
                    247 assignments
*/
}

void sdqdot(double oqdot[8])
{
/*
Return position coordinate derivatives for tree joints.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(63,23);
        return;
    }
    for (i = 0; i <= 7; i++) {
        oqdot[i] = qdot[i];
    }
}

void sdu2qdot(double uin[8],
    double oqdot[8])
{
/*
Convert velocities to qdots.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(64,23);
        return;
    }
    for (i = 0; i <= 7; i++) {
        oqdot[i] = uin[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
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
        Wpk[0][0][2] = 1.;
        Wpk[0][1][2] = 1.;
        Wpk[0][2][2] = 1.;
        Wpk[0][3][2] = 1.;
        Wpk[0][4][2] = 1.;
        Wpk[0][5][2] = 1.;
        Wpk[0][6][2] = 1.;
        Wpk[0][7][2] = 1.;
        Wpk[3][3][2] = 1.;
        Wpk[3][4][2] = 1.;
        Wpk[3][5][2] = 1.;
        Wpk[3][6][2] = 1.;
        Wpk[3][7][2] = 1.;
        Wpk[4][4][2] = 1.;
        Wpk[4][5][2] = 1.;
        Wpk[4][6][2] = 1.;
        Wpk[4][7][2] = 1.;
        Wpk[5][5][2] = 1.;
        Wpk[5][6][2] = 1.;
        Wpk[5][7][2] = 1.;
        Wpk[6][6][2] = 1.;
        Wpk[6][7][2] = 1.;
        Wpk[7][7][2] = 1.;
/*
Compute Vpk (partial velocities)
*/
        Vpk[0][1][1] = q[1];
        Vpk[0][2][0] = -q[2];
        Vpk[0][2][1] = q[1];
        Vpk[0][3][0] = (((q[1]*s3)-(q[2]*c3))-1.);
        Vpk[0][3][1] = ((q[1]*c3)+(q[2]*s3));
        VWri[0][4][0] = (Vpk[0][3][0]-1.);
        Vpk[0][4][0] = (((Vpk[0][3][1]*s4)+(VWri[0][4][0]*c4))-1.);
        Vpk[0][4][1] = ((Vpk[0][3][1]*c4)-(VWri[0][4][0]*s4));
        VWri[0][5][0] = (Vpk[0][4][0]-1.);
        Vpk[0][5][0] = (((Vpk[0][4][1]*s5)+(VWri[0][5][0]*c5))-1.);
        Vpk[0][5][1] = ((Vpk[0][4][1]*c5)-(VWri[0][5][0]*s5));
        VWri[0][6][0] = (Vpk[0][5][0]-1.);
        Vpk[0][6][0] = (((Vpk[0][5][1]*s6)+(VWri[0][6][0]*c6))-1.);
        Vpk[0][6][1] = ((Vpk[0][5][1]*c6)-(VWri[0][6][0]*s6));
        VWri[0][7][0] = (Vpk[0][6][0]-1.);
        Vpk[0][7][0] = (((Vpk[0][6][1]*s7)+(VWri[0][7][0]*c7))-1.);
        Vpk[0][7][1] = ((Vpk[0][6][1]*c7)-(VWri[0][7][0]*s7));
        Vpk[1][1][0] = 1.;
        Vpk[1][2][0] = 1.;
        Vpk[1][3][0] = c3;
        Vpk[1][3][1] = -s3;
        Vpk[1][4][0] = ((c3*c4)-(s3*s4));
        Vpk[1][4][1] = -((s3*c4)+(s4*c3));
        Vpk[1][5][0] = ((Vpk[1][4][0]*c5)+(Vpk[1][4][1]*s5));
        Vpk[1][5][1] = ((Vpk[1][4][1]*c5)-(Vpk[1][4][0]*s5));
        Vpk[1][6][0] = ((Vpk[1][5][0]*c6)+(Vpk[1][5][1]*s6));
        Vpk[1][6][1] = ((Vpk[1][5][1]*c6)-(Vpk[1][5][0]*s6));
        Vpk[1][7][0] = ((Vpk[1][6][0]*c7)+(Vpk[1][6][1]*s7));
        Vpk[1][7][1] = ((Vpk[1][6][1]*c7)-(Vpk[1][6][0]*s7));
        Vpk[2][2][1] = 1.;
        Vpk[2][3][0] = s3;
        Vpk[2][3][1] = c3;
        Vpk[2][4][0] = ((s3*c4)+(s4*c3));
        Vpk[2][4][1] = ((c3*c4)-(s3*s4));
        Vpk[2][5][0] = ((Vpk[2][4][0]*c5)+(Vpk[2][4][1]*s5));
        Vpk[2][5][1] = ((Vpk[2][4][1]*c5)-(Vpk[2][4][0]*s5));
        Vpk[2][6][0] = ((Vpk[2][5][0]*c6)+(Vpk[2][5][1]*s6));
        Vpk[2][6][1] = ((Vpk[2][5][1]*c6)-(Vpk[2][5][0]*s6));
        Vpk[2][7][0] = ((Vpk[2][6][0]*c7)+(Vpk[2][6][1]*s7));
        Vpk[2][7][1] = ((Vpk[2][6][1]*c7)-(Vpk[2][6][0]*s7));
        Vpk[3][3][0] = -1.;
        Vpk[3][4][0] = -(1.+(2.*c4));
        Vpk[3][4][1] = (2.*s4);
        VWri[3][5][0] = (Vpk[3][4][0]-1.);
        Vpk[3][5][0] = (((2.*(s4*s5))+(VWri[3][5][0]*c5))-1.);
        Vpk[3][5][1] = ((2.*(s4*c5))-(VWri[3][5][0]*s5));
        VWri[3][6][0] = (Vpk[3][5][0]-1.);
        Vpk[3][6][0] = (((Vpk[3][5][1]*s6)+(VWri[3][6][0]*c6))-1.);
        Vpk[3][6][1] = ((Vpk[3][5][1]*c6)-(VWri[3][6][0]*s6));
        VWri[3][7][0] = (Vpk[3][6][0]-1.);
        Vpk[3][7][0] = (((Vpk[3][6][1]*s7)+(VWri[3][7][0]*c7))-1.);
        Vpk[3][7][1] = ((Vpk[3][6][1]*c7)-(VWri[3][7][0]*s7));
        Vpk[4][4][0] = -1.;
        Vpk[4][5][0] = -(1.+(2.*c5));
        Vpk[4][5][1] = (2.*s5);
        VWri[4][6][0] = (Vpk[4][5][0]-1.);
        Vpk[4][6][0] = (((2.*(s5*s6))+(VWri[4][6][0]*c6))-1.);
        Vpk[4][6][1] = ((2.*(s5*c6))-(VWri[4][6][0]*s6));
        VWri[4][7][0] = (Vpk[4][6][0]-1.);
        Vpk[4][7][0] = (((Vpk[4][6][1]*s7)+(VWri[4][7][0]*c7))-1.);
        Vpk[4][7][1] = ((Vpk[4][6][1]*c7)-(VWri[4][7][0]*s7));
        Vpk[5][5][0] = -1.;
        Vpk[5][6][0] = -(1.+(2.*c6));
        Vpk[5][6][1] = (2.*s6);
        VWri[5][7][0] = (Vpk[5][6][0]-1.);
        Vpk[5][7][0] = (((2.*(s6*s7))+(VWri[5][7][0]*c7))-1.);
        Vpk[5][7][1] = ((2.*(s6*c7))-(VWri[5][7][0]*s7));
        Vpk[6][6][0] = -1.;
        Vpk[6][7][0] = -(1.+(2.*c7));
        Vpk[6][7][1] = (2.*s7);
        Vpk[7][7][0] = -1.;
        vpkflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   70 adds/subtracts/negates
                     90 multiplies
                      0 divides
                     94 assignments
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
/*
Compute Atk (inertial linear acceleration)
*/
        Atk[1][0] = -(u[0]*Wkrpk[1][1]);
        Atk[1][1] = (2.*(u[0]*u[1]));
        Atk[2][0] = (Atk[1][0]-(2.*(u[0]*u[2])));
        Atk[2][1] = (Atk[1][1]+(u[0]*Wkrpk[2][0]));
        Atk[3][0] = ((Atk[2][0]*c3)+(Atk[2][1]*s3));
        Atk[3][1] = (((Atk[2][1]*c3)-(Atk[2][0]*s3))-(wk[3][2]*wk[3][2]));
        AiOiWi[4][1] = (Atk[3][1]-(wk[3][2]*wk[3][2]));
        Atk[4][0] = ((AiOiWi[4][1]*s4)+(Atk[3][0]*c4));
        Atk[4][1] = (((AiOiWi[4][1]*c4)-(Atk[3][0]*s4))-(wk[4][2]*wk[4][2]));
        AiOiWi[5][1] = (Atk[4][1]-(wk[4][2]*wk[4][2]));
        Atk[5][0] = ((AiOiWi[5][1]*s5)+(Atk[4][0]*c5));
        Atk[5][1] = (((AiOiWi[5][1]*c5)-(Atk[4][0]*s5))-(wk[5][2]*wk[5][2]));
        AiOiWi[6][1] = (Atk[5][1]-(wk[5][2]*wk[5][2]));
        Atk[6][0] = ((AiOiWi[6][1]*s6)+(Atk[5][0]*c6));
        Atk[6][1] = (((AiOiWi[6][1]*c6)-(Atk[5][0]*s6))-(wk[6][2]*wk[6][2]));
        AiOiWi[7][1] = (Atk[6][1]-(wk[6][2]*wk[6][2]));
        Atk[7][0] = ((AiOiWi[7][1]*s7)+(Atk[6][0]*c7));
        Atk[7][1] = (((AiOiWi[7][1]*c7)-(Atk[6][0]*s7))-(wk[7][2]*wk[7][2]));
        inerflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   22 adds/subtracts/negates
                     35 multiplies
                      0 divides
                     18 assignments
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
        Fstar[3][0] = (Atk[3][0]-ufk[1][0]);
        Fstar[3][1] = (Atk[3][1]-ufk[1][1]);
        Fstar[4][0] = (Atk[4][0]-ufk[2][0]);
        Fstar[4][1] = (Atk[4][1]-ufk[2][1]);
        Fstar[5][0] = (Atk[5][0]-ufk[3][0]);
        Fstar[5][1] = (Atk[5][1]-ufk[3][1]);
        Fstar[6][0] = (Atk[6][0]-ufk[4][0]);
        Fstar[6][1] = (Atk[6][1]-ufk[4][1]);
        Fstar[7][0] = (Atk[7][0]-ufk[5][0]);
        Fstar[7][1] = (Atk[7][1]-ufk[5][1]);
/*
Compute Tstar (torques)
*/
/*
Compute fs0 (RHS ignoring constraints)
*/
        sddovpk();
        fs0[0] = (utau[0]-((((Fstar[7][0]*Vpk[0][7][0])+(Fstar[7][1]*
          Vpk[0][7][1]))-utk[5][2])+((((Fstar[6][0]*Vpk[0][6][0])+(Fstar[6][1]*
          Vpk[0][6][1]))-utk[4][2])+((((Fstar[5][0]*Vpk[0][5][0])+(Fstar[5][1]*
          Vpk[0][5][1]))-utk[3][2])+((((Fstar[4][0]*Vpk[0][4][0])+(Fstar[4][1]*
          Vpk[0][4][1]))-utk[2][2])+((((Fstar[3][0]*Vpk[0][3][0])+(Fstar[3][1]*
          Vpk[0][3][1]))-utk[1][2])-utk[0][2]))))));
        fs0[1] = (utau[1]-(((Fstar[7][0]*Vpk[1][7][0])+(Fstar[7][1]*Vpk[1][7][1]
          ))+(((Fstar[6][0]*Vpk[1][6][0])+(Fstar[6][1]*Vpk[1][6][1]))+(((
          Fstar[5][0]*Vpk[1][5][0])+(Fstar[5][1]*Vpk[1][5][1]))+(((Fstar[3][0]*
          c3)-(Fstar[3][1]*s3))+((Fstar[4][0]*Vpk[1][4][0])+(Fstar[4][1]*
          Vpk[1][4][1])))))));
        fs0[2] = (utau[2]-(((Fstar[7][0]*Vpk[2][7][0])+(Fstar[7][1]*Vpk[2][7][1]
          ))+(((Fstar[6][0]*Vpk[2][6][0])+(Fstar[6][1]*Vpk[2][6][1]))+(((
          Fstar[5][0]*Vpk[2][5][0])+(Fstar[5][1]*Vpk[2][5][1]))+(((Fstar[3][0]*
          s3)+(Fstar[3][1]*c3))+((Fstar[4][0]*Vpk[2][4][0])+(Fstar[4][1]*
          Vpk[2][4][1])))))));
        fs0[3] = (utau[3]-((((Fstar[7][0]*Vpk[3][7][0])+(Fstar[7][1]*
          Vpk[3][7][1]))-utk[5][2])+((((Fstar[6][0]*Vpk[3][6][0])+(Fstar[6][1]*
          Vpk[3][6][1]))-utk[4][2])+((((Fstar[5][0]*Vpk[3][5][0])+(Fstar[5][1]*
          Vpk[3][5][1]))-utk[3][2])+((((2.*(Fstar[4][1]*s4))+(Fstar[4][0]*
          Vpk[3][4][0]))-utk[2][2])-(Fstar[3][0]+utk[1][2]))))));
        fs0[4] = (utau[4]-((((Fstar[7][0]*Vpk[4][7][0])+(Fstar[7][1]*
          Vpk[4][7][1]))-utk[5][2])+((((Fstar[6][0]*Vpk[4][6][0])+(Fstar[6][1]*
          Vpk[4][6][1]))-utk[4][2])+((((2.*(Fstar[5][1]*s5))+(Fstar[5][0]*
          Vpk[4][5][0]))-utk[3][2])-(Fstar[4][0]+utk[2][2])))));
        fs0[5] = (utau[5]-((((Fstar[7][0]*Vpk[5][7][0])+(Fstar[7][1]*
          Vpk[5][7][1]))-utk[5][2])+((((2.*(Fstar[6][1]*s6))+(Fstar[6][0]*
          Vpk[5][6][0]))-utk[4][2])-(Fstar[5][0]+utk[3][2]))));
        fs0[6] = (utau[6]-((((2.*(Fstar[7][1]*s7))+(Fstar[7][0]*Vpk[6][7][0]))-
          utk[5][2])-(Fstar[6][0]+utk[4][2])));
        fs0[7] = (utau[7]+(Fstar[7][0]+utk[5][2]));
        fs0flg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   86 adds/subtracts/negates
                     54 multiplies
                      0 divides
                     18 assignments
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
        mm[0][0] = (251.+(((Vpk[0][7][0]*Vpk[0][7][0])+(Vpk[0][7][1]*
          Vpk[0][7][1]))+(((Vpk[0][6][0]*Vpk[0][6][0])+(Vpk[0][6][1]*
          Vpk[0][6][1]))+(((Vpk[0][5][0]*Vpk[0][5][0])+(Vpk[0][5][1]*
          Vpk[0][5][1]))+(((Vpk[0][3][0]*Vpk[0][3][0])+(Vpk[0][3][1]*
          Vpk[0][3][1]))+((Vpk[0][4][0]*Vpk[0][4][0])+(Vpk[0][4][1]*Vpk[0][4][1]
          )))))));
        mm[0][1] = (((Vpk[0][7][0]*Vpk[1][7][0])+(Vpk[0][7][1]*Vpk[1][7][1]))+((
          (Vpk[0][6][0]*Vpk[1][6][0])+(Vpk[0][6][1]*Vpk[1][6][1]))+(((
          Vpk[0][5][0]*Vpk[1][5][0])+(Vpk[0][5][1]*Vpk[1][5][1]))+(((
          Vpk[0][3][0]*c3)-(Vpk[0][3][1]*s3))+((Vpk[0][4][0]*Vpk[1][4][0])+(
          Vpk[0][4][1]*Vpk[1][4][1]))))));
        mm[0][2] = (((Vpk[0][7][0]*Vpk[2][7][0])+(Vpk[0][7][1]*Vpk[2][7][1]))+((
          (Vpk[0][6][0]*Vpk[2][6][0])+(Vpk[0][6][1]*Vpk[2][6][1]))+(((
          Vpk[0][5][0]*Vpk[2][5][0])+(Vpk[0][5][1]*Vpk[2][5][1]))+(((
          Vpk[0][3][0]*s3)+(Vpk[0][3][1]*c3))+((Vpk[0][4][0]*Vpk[2][4][0])+(
          Vpk[0][4][1]*Vpk[2][4][1]))))));
        mm[0][3] = (250.+(((Vpk[0][7][0]*Vpk[3][7][0])+(Vpk[0][7][1]*
          Vpk[3][7][1]))+(((Vpk[0][6][0]*Vpk[3][6][0])+(Vpk[0][6][1]*
          Vpk[3][6][1]))+(((Vpk[0][5][0]*Vpk[3][5][0])+(Vpk[0][5][1]*
          Vpk[3][5][1]))+(((2.*(Vpk[0][4][1]*s4))+(Vpk[0][4][0]*Vpk[3][4][0]))-
          Vpk[0][3][0])))));
        mm[0][4] = (200.+(((Vpk[0][7][0]*Vpk[4][7][0])+(Vpk[0][7][1]*
          Vpk[4][7][1]))+(((Vpk[0][6][0]*Vpk[4][6][0])+(Vpk[0][6][1]*
          Vpk[4][6][1]))+(((2.*(Vpk[0][5][1]*s5))+(Vpk[0][5][0]*Vpk[4][5][0]))-
          Vpk[0][4][0]))));
        mm[0][5] = (150.+(((Vpk[0][7][0]*Vpk[5][7][0])+(Vpk[0][7][1]*
          Vpk[5][7][1]))+(((2.*(Vpk[0][6][1]*s6))+(Vpk[0][6][0]*Vpk[5][6][0]))-
          Vpk[0][5][0])));
        mm[0][6] = (100.+(((2.*(Vpk[0][7][1]*s7))+(Vpk[0][7][0]*Vpk[6][7][0]))-
          Vpk[0][6][0]));
        mm[0][7] = (50.-Vpk[0][7][0]);
        mm[1][1] = (1.+(((Vpk[1][7][0]*Vpk[1][7][0])+(Vpk[1][7][1]*Vpk[1][7][1])
          )+(((Vpk[1][6][0]*Vpk[1][6][0])+(Vpk[1][6][1]*Vpk[1][6][1]))+(((
          Vpk[1][4][0]*Vpk[1][4][0])+(Vpk[1][4][1]*Vpk[1][4][1]))+((Vpk[1][5][0]
          *Vpk[1][5][0])+(Vpk[1][5][1]*Vpk[1][5][1]))))));
        mm[1][2] = (((Vpk[1][7][0]*Vpk[2][7][0])+(Vpk[1][7][1]*Vpk[2][7][1]))+((
          (Vpk[1][6][0]*Vpk[2][6][0])+(Vpk[1][6][1]*Vpk[2][6][1]))+(((
          Vpk[1][4][0]*Vpk[2][4][0])+(Vpk[1][4][1]*Vpk[2][4][1]))+((Vpk[1][5][0]
          *Vpk[2][5][0])+(Vpk[1][5][1]*Vpk[2][5][1])))));
        mm[1][3] = (((Vpk[1][7][0]*Vpk[3][7][0])+(Vpk[1][7][1]*Vpk[3][7][1]))+((
          (Vpk[1][6][0]*Vpk[3][6][0])+(Vpk[1][6][1]*Vpk[3][6][1]))+(((
          Vpk[1][5][0]*Vpk[3][5][0])+(Vpk[1][5][1]*Vpk[3][5][1]))+(((2.*(
          Vpk[1][4][1]*s4))+(Vpk[1][4][0]*Vpk[3][4][0]))-c3))));
        mm[1][4] = (((Vpk[1][7][0]*Vpk[4][7][0])+(Vpk[1][7][1]*Vpk[4][7][1]))+((
          (Vpk[1][6][0]*Vpk[4][6][0])+(Vpk[1][6][1]*Vpk[4][6][1]))+(((2.*(
          Vpk[1][5][1]*s5))+(Vpk[1][5][0]*Vpk[4][5][0]))-Vpk[1][4][0])));
        mm[1][5] = (((Vpk[1][7][0]*Vpk[5][7][0])+(Vpk[1][7][1]*Vpk[5][7][1]))+((
          (2.*(Vpk[1][6][1]*s6))+(Vpk[1][6][0]*Vpk[5][6][0]))-Vpk[1][5][0]));
        mm[1][6] = (((2.*(Vpk[1][7][1]*s7))+(Vpk[1][7][0]*Vpk[6][7][0]))-
          Vpk[1][6][0]);
        mm[1][7] = -Vpk[1][7][0];
        mm[2][2] = (1.+(((Vpk[2][7][0]*Vpk[2][7][0])+(Vpk[2][7][1]*Vpk[2][7][1])
          )+(((Vpk[2][6][0]*Vpk[2][6][0])+(Vpk[2][6][1]*Vpk[2][6][1]))+(((
          Vpk[2][4][0]*Vpk[2][4][0])+(Vpk[2][4][1]*Vpk[2][4][1]))+((Vpk[2][5][0]
          *Vpk[2][5][0])+(Vpk[2][5][1]*Vpk[2][5][1]))))));
        mm[2][3] = (((Vpk[2][7][0]*Vpk[3][7][0])+(Vpk[2][7][1]*Vpk[3][7][1]))+((
          (Vpk[2][6][0]*Vpk[3][6][0])+(Vpk[2][6][1]*Vpk[3][6][1]))+(((
          Vpk[2][5][0]*Vpk[3][5][0])+(Vpk[2][5][1]*Vpk[3][5][1]))+(((2.*(
          Vpk[2][4][1]*s4))+(Vpk[2][4][0]*Vpk[3][4][0]))-s3))));
        mm[2][4] = (((Vpk[2][7][0]*Vpk[4][7][0])+(Vpk[2][7][1]*Vpk[4][7][1]))+((
          (Vpk[2][6][0]*Vpk[4][6][0])+(Vpk[2][6][1]*Vpk[4][6][1]))+(((2.*(
          Vpk[2][5][1]*s5))+(Vpk[2][5][0]*Vpk[4][5][0]))-Vpk[2][4][0])));
        mm[2][5] = (((Vpk[2][7][0]*Vpk[5][7][0])+(Vpk[2][7][1]*Vpk[5][7][1]))+((
          (2.*(Vpk[2][6][1]*s6))+(Vpk[2][6][0]*Vpk[5][6][0]))-Vpk[2][5][0]));
        mm[2][6] = (((2.*(Vpk[2][7][1]*s7))+(Vpk[2][7][0]*Vpk[6][7][0]))-
          Vpk[2][6][0]);
        mm[2][7] = -Vpk[2][7][0];
        mm[3][3] = (251.+(((Vpk[3][7][0]*Vpk[3][7][0])+(Vpk[3][7][1]*
          Vpk[3][7][1]))+(((Vpk[3][6][0]*Vpk[3][6][0])+(Vpk[3][6][1]*
          Vpk[3][6][1]))+(((4.*(s4*s4))+(Vpk[3][4][0]*Vpk[3][4][0]))+((
          Vpk[3][5][0]*Vpk[3][5][0])+(Vpk[3][5][1]*Vpk[3][5][1]))))));
        mm[3][4] = (200.+(((Vpk[3][7][0]*Vpk[4][7][0])+(Vpk[3][7][1]*
          Vpk[4][7][1]))+(((Vpk[3][6][0]*Vpk[4][6][0])+(Vpk[3][6][1]*
          Vpk[4][6][1]))+(((2.*(Vpk[3][5][1]*s5))+(Vpk[3][5][0]*Vpk[4][5][0]))-
          Vpk[3][4][0]))));
        mm[3][5] = (150.+(((Vpk[3][7][0]*Vpk[5][7][0])+(Vpk[3][7][1]*
          Vpk[5][7][1]))+(((2.*(Vpk[3][6][1]*s6))+(Vpk[3][6][0]*Vpk[5][6][0]))-
          Vpk[3][5][0])));
        mm[3][6] = (100.+(((2.*(Vpk[3][7][1]*s7))+(Vpk[3][7][0]*Vpk[6][7][0]))-
          Vpk[3][6][0]));
        mm[3][7] = (50.-Vpk[3][7][0]);
        mm[4][4] = (201.+(((Vpk[4][7][0]*Vpk[4][7][0])+(Vpk[4][7][1]*
          Vpk[4][7][1]))+(((4.*(s5*s5))+(Vpk[4][5][0]*Vpk[4][5][0]))+((
          Vpk[4][6][0]*Vpk[4][6][0])+(Vpk[4][6][1]*Vpk[4][6][1])))));
        mm[4][5] = (150.+(((Vpk[4][7][0]*Vpk[5][7][0])+(Vpk[4][7][1]*
          Vpk[5][7][1]))+(((2.*(Vpk[4][6][1]*s6))+(Vpk[4][6][0]*Vpk[5][6][0]))-
          Vpk[4][5][0])));
        mm[4][6] = (100.+(((2.*(Vpk[4][7][1]*s7))+(Vpk[4][7][0]*Vpk[6][7][0]))-
          Vpk[4][6][0]));
        mm[4][7] = (50.-Vpk[4][7][0]);
        mm[5][5] = (151.+(((4.*(s6*s6))+(Vpk[5][6][0]*Vpk[5][6][0]))+((
          Vpk[5][7][0]*Vpk[5][7][0])+(Vpk[5][7][1]*Vpk[5][7][1]))));
        mm[5][6] = (100.+(((2.*(Vpk[5][7][1]*s7))+(Vpk[5][7][0]*Vpk[6][7][0]))-
          Vpk[5][6][0]));
        mm[5][7] = (50.-Vpk[5][7][0]);
        mm[6][6] = (101.+((4.*(s7*s7))+(Vpk[6][7][0]*Vpk[6][7][0])));
        mm[6][7] = (50.-Vpk[6][7][0]);
        mm[7][7] = 51.;
/*
Check for singular mass matrix
*/
        for (i = 0; i < 8; i++) {
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
 Equations contain  168 adds/subtracts/negates
                    176 multiplies
                      0 divides
                     36 assignments
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
        sdldudcomp(8,8,mmap,1e-13,workss,works,mm,mlo,mdi);
/*
Check for singular mass matrix
*/
        for (i = 0; i < 8; i++) {
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

    mtau[0] = imult[0];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      1 assignments
*/
}

void sdequivht(double tau[8])
{
/* Compute tree hinge torques to match effect of applied loads
*/
    double fstareq[8][3],tstareq[8][3];

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
    tau[0] = (utau[0]+((utk[5][2]+((ufk[5][0]*Vpk[0][7][0])+(ufk[5][1]*
      Vpk[0][7][1])))+((utk[4][2]+((ufk[4][0]*Vpk[0][6][0])+(ufk[4][1]*
      Vpk[0][6][1])))+((utk[3][2]+((ufk[3][0]*Vpk[0][5][0])+(ufk[3][1]*
      Vpk[0][5][1])))+((utk[0][2]+(utk[1][2]+((ufk[1][0]*Vpk[0][3][0])+(
      ufk[1][1]*Vpk[0][3][1]))))+(utk[2][2]+((ufk[2][0]*Vpk[0][4][0])+(ufk[2][1]
      *Vpk[0][4][1]))))))));
    tau[1] = (utau[1]-((((((ufk[1][1]*s3)-(ufk[1][0]*c3))-((ufk[2][0]*
      Vpk[1][4][0])+(ufk[2][1]*Vpk[1][4][1])))-((ufk[3][0]*Vpk[1][5][0])+(
      ufk[3][1]*Vpk[1][5][1])))-((ufk[4][0]*Vpk[1][6][0])+(ufk[4][1]*
      Vpk[1][6][1])))-((ufk[5][0]*Vpk[1][7][0])+(ufk[5][1]*Vpk[1][7][1]))));
    tau[2] = (utau[2]+(((ufk[5][0]*Vpk[2][7][0])+(ufk[5][1]*Vpk[2][7][1]))+(((
      ufk[4][0]*Vpk[2][6][0])+(ufk[4][1]*Vpk[2][6][1]))+(((ufk[3][0]*
      Vpk[2][5][0])+(ufk[3][1]*Vpk[2][5][1]))+(((ufk[1][0]*s3)+(ufk[1][1]*c3))+(
      (ufk[2][0]*Vpk[2][4][0])+(ufk[2][1]*Vpk[2][4][1])))))));
    tau[3] = (utau[3]-(((((ufk[1][0]-utk[1][2])-(utk[2][2]+((2.*(ufk[2][1]*s4))+
      (ufk[2][0]*Vpk[3][4][0]))))-(utk[3][2]+((ufk[3][0]*Vpk[3][5][0])+(
      ufk[3][1]*Vpk[3][5][1]))))-(utk[4][2]+((ufk[4][0]*Vpk[3][6][0])+(ufk[4][1]
      *Vpk[3][6][1]))))-(utk[5][2]+((ufk[5][0]*Vpk[3][7][0])+(ufk[5][1]*
      Vpk[3][7][1])))));
    tau[4] = (utau[4]-((((ufk[2][0]-utk[2][2])-(utk[3][2]+((2.*(ufk[3][1]*s5))+(
      ufk[3][0]*Vpk[4][5][0]))))-(utk[4][2]+((ufk[4][0]*Vpk[4][6][0])+(ufk[4][1]
      *Vpk[4][6][1]))))-(utk[5][2]+((ufk[5][0]*Vpk[4][7][0])+(ufk[5][1]*
      Vpk[4][7][1])))));
    tau[5] = (utau[5]-(((ufk[3][0]-utk[3][2])-(utk[4][2]+((2.*(ufk[4][1]*s6))+(
      ufk[4][0]*Vpk[5][6][0]))))-(utk[5][2]+((ufk[5][0]*Vpk[5][7][0])+(ufk[5][1]
      *Vpk[5][7][1])))));
    tau[6] = (utau[6]-((ufk[4][0]-utk[4][2])-(utk[5][2]+((2.*(ufk[5][1]*s7))+(
      ufk[5][0]*Vpk[6][7][0])))));
    tau[7] = (utau[7]-(ufk[5][0]-utk[5][2]));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   76 adds/subtracts/negates
                     54 multiplies
                      0 divides
                      8 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
*/
}

void sdfsmult(void)
{

/*
Compute Fs (multiplier-generated forces only)
*/
    sddovpk();
    fs[0] = mtau[0];
    fs[1] = 0.;
    fs[2] = 0.;
    fs[3] = 0.;
    fs[4] = 0.;
    fs[5] = 0.;
    fs[6] = 0.;
    fs[7] = 0.;
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    8 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
*/
}

void sdfsgenmult(void)
{

/*
Compute Fs (generic multiplier-generated forces)
*/
    sddovpk();
    fs[0] = (mtau[0]+((mtk[5][2]+((mfk[5][0]*Vpk[0][7][0])+(mfk[5][1]*
      Vpk[0][7][1])))+((mtk[4][2]+((mfk[4][0]*Vpk[0][6][0])+(mfk[4][1]*
      Vpk[0][6][1])))+((mtk[3][2]+((mfk[3][0]*Vpk[0][5][0])+(mfk[3][1]*
      Vpk[0][5][1])))+((mtk[0][2]+(mtk[1][2]+((mfk[1][0]*Vpk[0][3][0])+(
      mfk[1][1]*Vpk[0][3][1]))))+(mtk[2][2]+((mfk[2][0]*Vpk[0][4][0])+(mfk[2][1]
      *Vpk[0][4][1]))))))));
    fs[1] = (mtau[1]-((((((mfk[1][1]*s3)-(mfk[1][0]*c3))-((mfk[2][0]*
      Vpk[1][4][0])+(mfk[2][1]*Vpk[1][4][1])))-((mfk[3][0]*Vpk[1][5][0])+(
      mfk[3][1]*Vpk[1][5][1])))-((mfk[4][0]*Vpk[1][6][0])+(mfk[4][1]*
      Vpk[1][6][1])))-((mfk[5][0]*Vpk[1][7][0])+(mfk[5][1]*Vpk[1][7][1]))));
    fs[2] = (mtau[2]+(((mfk[5][0]*Vpk[2][7][0])+(mfk[5][1]*Vpk[2][7][1]))+(((
      mfk[4][0]*Vpk[2][6][0])+(mfk[4][1]*Vpk[2][6][1]))+(((mfk[3][0]*
      Vpk[2][5][0])+(mfk[3][1]*Vpk[2][5][1]))+(((mfk[1][0]*s3)+(mfk[1][1]*c3))+(
      (mfk[2][0]*Vpk[2][4][0])+(mfk[2][1]*Vpk[2][4][1])))))));
    fs[3] = (mtau[3]-(((((mfk[1][0]-mtk[1][2])-(mtk[2][2]+((2.*(mfk[2][1]*s4))+(
      mfk[2][0]*Vpk[3][4][0]))))-(mtk[3][2]+((mfk[3][0]*Vpk[3][5][0])+(mfk[3][1]
      *Vpk[3][5][1]))))-(mtk[4][2]+((mfk[4][0]*Vpk[3][6][0])+(mfk[4][1]*
      Vpk[3][6][1]))))-(mtk[5][2]+((mfk[5][0]*Vpk[3][7][0])+(mfk[5][1]*
      Vpk[3][7][1])))));
    fs[4] = (mtau[4]-((((mfk[2][0]-mtk[2][2])-(mtk[3][2]+((2.*(mfk[3][1]*s5))+(
      mfk[3][0]*Vpk[4][5][0]))))-(mtk[4][2]+((mfk[4][0]*Vpk[4][6][0])+(mfk[4][1]
      *Vpk[4][6][1]))))-(mtk[5][2]+((mfk[5][0]*Vpk[4][7][0])+(mfk[5][1]*
      Vpk[4][7][1])))));
    fs[5] = (mtau[5]-(((mfk[3][0]-mtk[3][2])-(mtk[4][2]+((2.*(mfk[4][1]*s6))+(
      mfk[4][0]*Vpk[5][6][0]))))-(mtk[5][2]+((mfk[5][0]*Vpk[5][7][0])+(mfk[5][1]
      *Vpk[5][7][1])))));
    fs[6] = (mtau[6]-((mfk[4][0]-mtk[4][2])-(mtk[5][2]+((2.*(mfk[5][1]*s7))+(
      mfk[5][0]*Vpk[6][7][0])))));
    fs[7] = (mtau[7]-(mfk[5][0]-mtk[5][2]));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   76 adds/subtracts/negates
                     54 multiplies
                      0 divides
                      8 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    8 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
*/
}

void sdfulltrq(double udotin[8],
    double multin[1],
    double trqout[8])
{
/* Compute hinge torques which would produce indicated udots
*/
    double fstarr[8][3],tstarr[8][3],Otkr[8][3],Atir[8][3],Atkr[8][3];

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
    Otkr[3][2] = (udotin[0]+udotin[3]);
    Otkr[4][2] = (Otkr[3][2]+udotin[4]);
    Otkr[5][2] = (Otkr[4][2]+udotin[5]);
    Otkr[6][2] = (Otkr[5][2]+udotin[6]);
    Otkr[7][2] = (Otkr[6][2]+udotin[7]);
    Atkr[1][0] = (udotin[1]-(u[0]*Wkrpk[1][1]));
    Atkr[1][1] = ((2.*(u[0]*u[1]))+(q[1]*udotin[0]));
    Atkr[2][0] = (Atkr[1][0]-((2.*(u[0]*u[2]))+(q[2]*udotin[0])));
    Atkr[2][1] = (udotin[2]+(Atkr[1][1]+(u[0]*Wkrpk[2][0])));
    Atkr[3][0] = (((Atkr[2][0]*c3)+(Atkr[2][1]*s3))-Otkr[3][2]);
    Atkr[3][1] = (((Atkr[2][1]*c3)-(Atkr[2][0]*s3))-(wk[3][2]*wk[3][2]));
    Atir[4][0] = (Atkr[3][0]-Otkr[3][2]);
    Atir[4][1] = (Atkr[3][1]-(wk[3][2]*wk[3][2]));
    Atkr[4][0] = (((Atir[4][0]*c4)+(Atir[4][1]*s4))-Otkr[4][2]);
    Atkr[4][1] = (((Atir[4][1]*c4)-(Atir[4][0]*s4))-(wk[4][2]*wk[4][2]));
    Atir[5][0] = (Atkr[4][0]-Otkr[4][2]);
    Atir[5][1] = (Atkr[4][1]-(wk[4][2]*wk[4][2]));
    Atkr[5][0] = (((Atir[5][0]*c5)+(Atir[5][1]*s5))-Otkr[5][2]);
    Atkr[5][1] = (((Atir[5][1]*c5)-(Atir[5][0]*s5))-(wk[5][2]*wk[5][2]));
    Atir[6][0] = (Atkr[5][0]-Otkr[5][2]);
    Atir[6][1] = (Atkr[5][1]-(wk[5][2]*wk[5][2]));
    Atkr[6][0] = (((Atir[6][0]*c6)+(Atir[6][1]*s6))-Otkr[6][2]);
    Atkr[6][1] = (((Atir[6][1]*c6)-(Atir[6][0]*s6))-(wk[6][2]*wk[6][2]));
    Atir[7][0] = (Atkr[6][0]-Otkr[6][2]);
    Atir[7][1] = (Atkr[6][1]-(wk[6][2]*wk[6][2]));
    Atkr[7][0] = (((Atir[7][0]*c7)+(Atir[7][1]*s7))-Otkr[7][2]);
    Atkr[7][1] = (((Atir[7][1]*c7)-(Atir[7][0]*s7))-(wk[7][2]*wk[7][2]));
/*
Accumulate all forces and torques
*/
    fstarr[3][0] = (ufk[1][0]-Atkr[3][0]);
    fstarr[3][1] = (ufk[1][1]-Atkr[3][1]);
    fstarr[4][0] = (ufk[2][0]-Atkr[4][0]);
    fstarr[4][1] = (ufk[2][1]-Atkr[4][1]);
    fstarr[5][0] = (ufk[3][0]-Atkr[5][0]);
    fstarr[5][1] = (ufk[3][1]-Atkr[5][1]);
    fstarr[6][0] = (ufk[4][0]-Atkr[6][0]);
    fstarr[6][1] = (ufk[4][1]-Atkr[6][1]);
    fstarr[7][0] = (ufk[5][0]-Atkr[7][0]);
    fstarr[7][1] = (ufk[5][1]-Atkr[7][1]);
    tstarr[0][2] = (utk[0][2]-udotin[0]);
    tstarr[3][2] = (utk[1][2]-(50.*Otkr[3][2]));
    tstarr[4][2] = (utk[2][2]-(50.*Otkr[4][2]));
    tstarr[5][2] = (utk[3][2]-(50.*Otkr[5][2]));
    tstarr[6][2] = (utk[4][2]-(50.*Otkr[6][2]));
    tstarr[7][2] = (utk[5][2]-(50.*Otkr[7][2]));
/*
Now calculate the torques
*/
    sddovpk();
    trqout[0] = -((mtau[0]+utau[0])+((tstarr[7][2]+((fstarr[7][0]*Vpk[0][7][0])+
      (fstarr[7][1]*Vpk[0][7][1])))+((tstarr[6][2]+((fstarr[6][0]*Vpk[0][6][0])+
      (fstarr[6][1]*Vpk[0][6][1])))+((tstarr[5][2]+((fstarr[5][0]*Vpk[0][5][0])+
      (fstarr[5][1]*Vpk[0][5][1])))+((tstarr[0][2]+(tstarr[3][2]+((fstarr[3][0]*
      Vpk[0][3][0])+(fstarr[3][1]*Vpk[0][3][1]))))+(tstarr[4][2]+((fstarr[4][0]*
      Vpk[0][4][0])+(fstarr[4][1]*Vpk[0][4][1]))))))));
    trqout[1] = -(utau[1]+(((fstarr[7][0]*Vpk[1][7][0])+(fstarr[7][1]*
      Vpk[1][7][1]))+(((fstarr[6][0]*Vpk[1][6][0])+(fstarr[6][1]*Vpk[1][6][1]))+
      (((fstarr[5][0]*Vpk[1][5][0])+(fstarr[5][1]*Vpk[1][5][1]))+(((fstarr[3][0]
      *c3)-(fstarr[3][1]*s3))+((fstarr[4][0]*Vpk[1][4][0])+(fstarr[4][1]*
      Vpk[1][4][1])))))));
    trqout[2] = -(utau[2]+(((fstarr[7][0]*Vpk[2][7][0])+(fstarr[7][1]*
      Vpk[2][7][1]))+(((fstarr[6][0]*Vpk[2][6][0])+(fstarr[6][1]*Vpk[2][6][1]))+
      (((fstarr[5][0]*Vpk[2][5][0])+(fstarr[5][1]*Vpk[2][5][1]))+(((fstarr[3][0]
      *s3)+(fstarr[3][1]*c3))+((fstarr[4][0]*Vpk[2][4][0])+(fstarr[4][1]*
      Vpk[2][4][1])))))));
    trqout[3] = -(utau[3]+((tstarr[7][2]+((fstarr[7][0]*Vpk[3][7][0])+(
      fstarr[7][1]*Vpk[3][7][1])))+((tstarr[6][2]+((fstarr[6][0]*Vpk[3][6][0])+(
      fstarr[6][1]*Vpk[3][6][1])))+((tstarr[5][2]+((fstarr[5][0]*Vpk[3][5][0])+(
      fstarr[5][1]*Vpk[3][5][1])))+((tstarr[3][2]-fstarr[3][0])+(tstarr[4][2]+((
      2.*(fstarr[4][1]*s4))+(fstarr[4][0]*Vpk[3][4][0]))))))));
    trqout[4] = -(utau[4]+((tstarr[7][2]+((fstarr[7][0]*Vpk[4][7][0])+(
      fstarr[7][1]*Vpk[4][7][1])))+((tstarr[6][2]+((fstarr[6][0]*Vpk[4][6][0])+(
      fstarr[6][1]*Vpk[4][6][1])))+((tstarr[4][2]-fstarr[4][0])+(tstarr[5][2]+((
      2.*(fstarr[5][1]*s5))+(fstarr[5][0]*Vpk[4][5][0])))))));
    trqout[5] = -(utau[5]+((tstarr[7][2]+((fstarr[7][0]*Vpk[5][7][0])+(
      fstarr[7][1]*Vpk[5][7][1])))+((tstarr[5][2]-fstarr[5][0])+(tstarr[6][2]+((
      2.*(fstarr[6][1]*s6))+(fstarr[6][0]*Vpk[5][6][0]))))));
    trqout[6] = -(utau[6]+((tstarr[6][2]-fstarr[6][0])+(tstarr[7][2]+((2.*(
      fstarr[7][1]*s7))+(fstarr[7][0]*Vpk[6][7][0])))));
    trqout[7] = -(utau[7]+(tstarr[7][2]-fstarr[7][0]));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  140 adds/subtracts/negates
                     96 multiplies
                      0 divides
                     51 assignments
*/
}

void sdcomptrq(double udotin[8],
    double trqout[8])
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
    double trqout[8])
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
    for (i = 0; i < 8; i++) {
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
    tauc[0] = (mtau[0]+utau[0]);
    tauc[1] = utau[1];
    tauc[2] = utau[2];
    tauc[3] = utau[3];
    tauc[4] = utau[4];
    tauc[5] = utau[5];
    tauc[6] = utau[6];
    tauc[7] = utau[7];
    sddoiner();
/*
Compute onk & onb (angular accels in N)
*/
    Onkb[3][2] = (udot[0]+udot[3]);
    Onkb[4][2] = (Onkb[3][2]+udot[4]);
    Onkb[5][2] = (Onkb[4][2]+udot[5]);
    Onkb[6][2] = (Onkb[5][2]+udot[6]);
    Onkb[7][2] = (Onkb[6][2]+udot[7]);
    onb[0][0] = 0.;
    onb[0][1] = 0.;
    onb[0][2] = udot[0];
    onb[1][0] = 0.;
    onb[1][1] = 0.;
    onb[1][2] = Onkb[3][2];
    onb[2][0] = 0.;
    onb[2][1] = 0.;
    onb[2][2] = Onkb[4][2];
    onb[3][0] = 0.;
    onb[3][1] = 0.;
    onb[3][2] = Onkb[5][2];
    onb[4][0] = 0.;
    onb[4][1] = 0.;
    onb[4][2] = Onkb[6][2];
    onb[5][0] = 0.;
    onb[5][1] = 0.;
    onb[5][2] = Onkb[7][2];
/*
Compute acceleration dyadics
*/
    dyad[0][0][0] = -w2w2[0];
    dyad[0][0][1] = -udot[0];
    dyad[0][0][2] = 0.;
    dyad[0][1][0] = udot[0];
    dyad[0][1][1] = -w2w2[0];
    dyad[0][1][2] = 0.;
    dyad[0][2][0] = 0.;
    dyad[0][2][1] = 0.;
    dyad[0][2][2] = 0.;
    dyad[1][0][0] = -w2w2[1];
    dyad[1][0][1] = -Onkb[3][2];
    dyad[1][0][2] = 0.;
    dyad[1][1][0] = Onkb[3][2];
    dyad[1][1][1] = -w2w2[1];
    dyad[1][1][2] = 0.;
    dyad[1][2][0] = 0.;
    dyad[1][2][1] = 0.;
    dyad[1][2][2] = 0.;
    dyad[2][0][0] = -w2w2[2];
    dyad[2][0][1] = -Onkb[4][2];
    dyad[2][0][2] = 0.;
    dyad[2][1][0] = Onkb[4][2];
    dyad[2][1][1] = -w2w2[2];
    dyad[2][1][2] = 0.;
    dyad[2][2][0] = 0.;
    dyad[2][2][1] = 0.;
    dyad[2][2][2] = 0.;
    dyad[3][0][0] = -w2w2[3];
    dyad[3][0][1] = -Onkb[5][2];
    dyad[3][0][2] = 0.;
    dyad[3][1][0] = Onkb[5][2];
    dyad[3][1][1] = -w2w2[3];
    dyad[3][1][2] = 0.;
    dyad[3][2][0] = 0.;
    dyad[3][2][1] = 0.;
    dyad[3][2][2] = 0.;
    dyad[4][0][0] = -w2w2[4];
    dyad[4][0][1] = -Onkb[6][2];
    dyad[4][0][2] = 0.;
    dyad[4][1][0] = Onkb[6][2];
    dyad[4][1][1] = -w2w2[4];
    dyad[4][1][2] = 0.;
    dyad[4][2][0] = 0.;
    dyad[4][2][1] = 0.;
    dyad[4][2][2] = 0.;
    dyad[5][0][0] = -w2w2[5];
    dyad[5][0][1] = -Onkb[7][2];
    dyad[5][0][2] = 0.;
    dyad[5][1][0] = Onkb[7][2];
    dyad[5][1][1] = -w2w2[5];
    dyad[5][1][2] = 0.;
    dyad[5][2][0] = 0.;
    dyad[5][2][1] = 0.;
    dyad[5][2][2] = 0.;
/*
Compute ank & anb (mass center linear accels in N)
*/
    Ankb[1][1] = (q[1]*udot[0]);
    Ankb[2][0] = (udot[1]-(q[2]*udot[0]));
    Ankb[2][1] = (Ankb[1][1]+udot[2]);
    Ankb[3][0] = (((Ankb[2][0]*c3)+(Ankb[2][1]*s3))-Onkb[3][2]);
    Ankb[3][1] = ((Ankb[2][1]*c3)-(Ankb[2][0]*s3));
    AOnkri[4][0] = (Ankb[3][0]-Onkb[3][2]);
    Ankb[4][0] = (((Ankb[3][1]*s4)+(AOnkri[4][0]*c4))-Onkb[4][2]);
    Ankb[4][1] = ((Ankb[3][1]*c4)-(AOnkri[4][0]*s4));
    AOnkri[5][0] = (Ankb[4][0]-Onkb[4][2]);
    Ankb[5][0] = (((Ankb[4][1]*s5)+(AOnkri[5][0]*c5))-Onkb[5][2]);
    Ankb[5][1] = ((Ankb[4][1]*c5)-(AOnkri[5][0]*s5));
    AOnkri[6][0] = (Ankb[5][0]-Onkb[5][2]);
    Ankb[6][0] = (((Ankb[5][1]*s6)+(AOnkri[6][0]*c6))-Onkb[6][2]);
    Ankb[6][1] = ((Ankb[5][1]*c6)-(AOnkri[6][0]*s6));
    AOnkri[7][0] = (Ankb[6][0]-Onkb[6][2]);
    Ankb[7][0] = (((Ankb[6][1]*s7)+(AOnkri[7][0]*c7))-Onkb[7][2]);
    Ankb[7][1] = ((Ankb[6][1]*c7)-(AOnkri[7][0]*s7));
    AnkAtk[3][0] = (Ankb[3][0]+Atk[3][0]);
    AnkAtk[3][1] = (Ankb[3][1]+Atk[3][1]);
    ank[3][0] = ((AnkAtk[3][0]*cnk[3][0][0])+(AnkAtk[3][1]*cnk[3][0][1]));
    ank[3][1] = ((AnkAtk[3][0]*cnk[3][1][0])+(AnkAtk[3][1]*cnk[3][1][1]));
    AnkAtk[4][0] = (Ankb[4][0]+Atk[4][0]);
    AnkAtk[4][1] = (Ankb[4][1]+Atk[4][1]);
    ank[4][0] = ((AnkAtk[4][0]*cnk[4][0][0])+(AnkAtk[4][1]*cnk[4][0][1]));
    ank[4][1] = ((AnkAtk[4][0]*cnk[4][1][0])+(AnkAtk[4][1]*cnk[4][1][1]));
    AnkAtk[5][0] = (Ankb[5][0]+Atk[5][0]);
    AnkAtk[5][1] = (Ankb[5][1]+Atk[5][1]);
    ank[5][0] = ((AnkAtk[5][0]*cnk[5][0][0])+(AnkAtk[5][1]*cnk[5][0][1]));
    ank[5][1] = ((AnkAtk[5][0]*cnk[5][1][0])+(AnkAtk[5][1]*cnk[5][1][1]));
    AnkAtk[6][0] = (Ankb[6][0]+Atk[6][0]);
    AnkAtk[6][1] = (Ankb[6][1]+Atk[6][1]);
    ank[6][0] = ((AnkAtk[6][0]*cnk[6][0][0])+(AnkAtk[6][1]*cnk[6][0][1]));
    ank[6][1] = ((AnkAtk[6][0]*cnk[6][1][0])+(AnkAtk[6][1]*cnk[6][1][1]));
    AnkAtk[7][0] = (Ankb[7][0]+Atk[7][0]);
    AnkAtk[7][1] = (Ankb[7][1]+Atk[7][1]);
    ank[7][0] = ((AnkAtk[7][0]*cnk[7][0][0])+(AnkAtk[7][1]*cnk[7][0][1]));
    ank[7][1] = ((AnkAtk[7][0]*cnk[7][1][0])+(AnkAtk[7][1]*cnk[7][1][1]));
    anb[0][0] = 0.;
    anb[0][1] = 0.;
    anb[0][2] = 0.;
    anb[1][0] = ank[3][0];
    anb[1][1] = ank[3][1];
    anb[1][2] = 0.;
    anb[2][0] = ank[4][0];
    anb[2][1] = ank[4][1];
    anb[2][2] = 0.;
    anb[3][0] = ank[5][0];
    anb[3][1] = ank[5][1];
    anb[3][2] = 0.;
    anb[4][0] = ank[6][0];
    anb[4][1] = ank[6][1];
    anb[4][2] = 0.;
    anb[5][0] = ank[7][0];
    anb[5][1] = ank[7][1];
    anb[5][2] = 0.;
/*
Compute constraint acceleration errors
*/
    roustate = 3;
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   65 adds/subtracts/negates
                     42 multiplies
                      0 divides
                    140 assignments
*/
}

void sdmassmat(double mmat[8][8])
{
/* Return the system mass matrix (LHS)
*/
    int i,j;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(57,23);
        return;
    }
    sddomm(57);
    for (i = 0; i < 8; i++) {
        for (j = i; j <= 7; j++) {
            mmat[i][j] = mm[i][j];
            mmat[j][i] = mm[i][j];
        }
    }
}

void sdfrcmat(double fmat[8])
{
/* Return the system force matrix (RHS), excluding constraints
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(58,23);
        return;
    }
    sddofs0();
    for (i = 0; i < 8; i++) {
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
    perr[0] = (q[0]-upos[0]);
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
    verr[0] = (u[0]-uvel[0]);
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
    aerr[0] = (udot[0]-uacc[0]);
    errs[0] = aerr[0];
}
int 
sdchkbnum(int routine,
    int bnum)
{

    if ((bnum < -1) || (bnum > 5)) {
        sdseterr(routine,15);
        return 1;
    }
    return 0;
}
int 
sdchkjnum(int routine,
    int jnum)
{

    if ((jnum < 0) || (jnum > 5)) {
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
    double pp[1][8],dpp[8][1];
    int i,j,c;
    double sum;
    double dfk[6][3],dtk[6][3],dtau[8],dltci[1][3],dltc[1][3],dlfci[1][3],dlfc[1
      ][3];
    double dTinb[1][3],dToutb[1][3],dltaufi[1][3],dltaufo[1][3],dltauti[1][3],
      dltauto[1][3];
    double dfs[8],row[8],dinvrow[8];

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
        dfs[0] = 1.;
        dfs[1] = 0.;
        dfs[2] = 0.;
        dfs[3] = 0.;
        dfs[4] = 0.;
        dfs[5] = 0.;
        dfs[6] = 0.;
        dfs[7] = 0.;
        sdldubsl(8,8,mmap,mlo,dfs,row);
        sdldubsd(8,8,mmap,mdi,row,dinvrow);
        for (i = 0; i <= 7; i++) {
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
                for (j = 0; j <= 7; j++) {
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
 Equations contain    8 adds/subtracts/negates
                      8 multiplies
                      0 divides
                     35 assignments
*/
}

void sdxudot0(int routine,
    double oudot0[8])
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
    sdldubslv(8,8,mmap,works,mlo,mdi,fs,udot);
    for (i = 0; i <= 7; i++) {
        oudot0[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
*/
}

void sdudot0(double oudot0[8])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(66,23);
        return;
    }
    sdxudot0(66,oudot0);
}

void sdsetudot(double iudot[8])
{
/*
Assign udots and advance to stage Dynamics Ready
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(68,23);
        return;
    }
    for (i = 0; i <= 7; i++) {
        udot[i] = iudot[i];
    }
    sdrhs();
}

void sdxudotm(int routine,
    double imult[1],
    double oudotm[8])
{
/*
Compute udots due only to multipliers
*/
    int i;

    sdlhs(routine);
    sdmfrc(imult);
    sdfsmult();
    sdldubslv(8,8,mmap,works,mlo,mdi,fs,udot);
    for (i = 0; i <= 7; i++) {
        oudotm[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      8 assignments
*/
}

void sdudotm(double imult[1],
    double oudotm[8])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(67,23);
        return;
    }
    sdxudotm(67,imult,oudotm);
}

void sdderiv(double oqdot[8],
    double oudot[8])
{
/*
This is the derivative section for a 6-body ground-based
system with 8 hinge degree(s) of freedom.
1 of the degrees of freedom follow(s) prescribed motion.
There is 1 constraint.
*/
    double workr[1],bb[1],b0[1],v0[1],p0[1];
    int iwork[1];
    int i,j;
    double udot0[8],udot1[8];

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
    for (i = 0; i <= 7; i++) {
        udot[i] = udot0[i]+udot1[i];
    }
    sdrhs();
    for (i = 0; i <= 7; i++) {
        oqdot[i] = qdot[i];
    }
    for (i = 0; i <= 7; i++) {
        oudot[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   11 adds/subtracts/negates
                      2 multiplies
                      0 divides
                     27 assignments
*/
}
/*
Compute residuals for use with DAE integrator
*/

void sdresid(double eqdot[8],
    double eudot[8],
    double emults[1],
    double resid[17])
{
    int i;
    double uderrs[8],p0[1];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(16,23);
        return;
    }
    if (stabposq == 1) {
        sdseterr(16,33);
    }
    sdfulltrq(eudot,emults,uderrs);
    for (i = 0; i < 8; i++) {
        resid[i] = eqdot[i]-qdot[i];
    }
    for (i = 0; i < 8; i++) {
        resid[8+i] = uderrs[i];
    }
    sdverr(&resid[16]);
    if (stabpos  !=  0.) {
        sdperr(p0);
        for (i = 0; i < 1; i++) {
            resid[16+i] = resid[16+i]+stabpos*p0[i];
        }
    }
    for (i = 0; i < 8; i++) {
        udot[i] = eudot[i];
    }
    for (i = 0; i < 1; i++) {
        mult[i] = emults[i];
    }
    sdrhs();
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    9 adds/subtracts/negates
                      1 multiplies
                      0 divides
                     26 assignments
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

void sdreac(double force[6][3],
    double torque[6][3])
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
    fc[7][0] = (AnkAtk[7][0]-ufk[5][0]);
    fc[7][1] = (AnkAtk[7][1]-ufk[5][1]);
    fc[7][2] = -ufk[5][2];
    tc[7][0] = -(utk[5][0]-fc[7][2]);
    tc[7][1] = -utk[5][1];
    tc[7][2] = ((50.*Onkb[7][2])-(fc[7][0]+utk[5][2]));
    fccikt[7][0] = ((fc[7][0]*c7)-(fc[7][1]*s7));
    fccikt[7][1] = ((fc[7][0]*s7)+(fc[7][1]*c7));
    fccikt[7][2] = fc[7][2];
    ffk[6][0] = (ufk[4][0]-fccikt[7][0]);
    ffk[6][1] = (ufk[4][1]-fccikt[7][1]);
    ffk[6][2] = (ufk[4][2]-fccikt[7][2]);
    ttk[6][0] = (utk[4][0]-(fccikt[7][2]+((tc[7][0]*c7)-(tc[7][1]*s7))));
    ttk[6][1] = (utk[4][1]-((tc[7][0]*s7)+(tc[7][1]*c7)));
    ttk[6][2] = (utk[4][2]-(tc[7][2]-fccikt[7][0]));
    fc[6][0] = (AnkAtk[6][0]-ffk[6][0]);
    fc[6][1] = (AnkAtk[6][1]-ffk[6][1]);
    fc[6][2] = -ffk[6][2];
    tc[6][0] = -(ttk[6][0]-fc[6][2]);
    tc[6][1] = -ttk[6][1];
    tc[6][2] = ((50.*Onkb[6][2])-(fc[6][0]+ttk[6][2]));
    fccikt[6][0] = ((fc[6][0]*c6)-(fc[6][1]*s6));
    fccikt[6][1] = ((fc[6][0]*s6)+(fc[6][1]*c6));
    fccikt[6][2] = fc[6][2];
    ffk[5][0] = (ufk[3][0]-fccikt[6][0]);
    ffk[5][1] = (ufk[3][1]-fccikt[6][1]);
    ffk[5][2] = (ufk[3][2]-fccikt[6][2]);
    ttk[5][0] = (utk[3][0]-(fccikt[6][2]+((tc[6][0]*c6)-(tc[6][1]*s6))));
    ttk[5][1] = (utk[3][1]-((tc[6][0]*s6)+(tc[6][1]*c6)));
    ttk[5][2] = (utk[3][2]-(tc[6][2]-fccikt[6][0]));
    fc[5][0] = (AnkAtk[5][0]-ffk[5][0]);
    fc[5][1] = (AnkAtk[5][1]-ffk[5][1]);
    fc[5][2] = -ffk[5][2];
    tc[5][0] = -(ttk[5][0]-fc[5][2]);
    tc[5][1] = -ttk[5][1];
    tc[5][2] = ((50.*Onkb[5][2])-(fc[5][0]+ttk[5][2]));
    fccikt[5][0] = ((fc[5][0]*c5)-(fc[5][1]*s5));
    fccikt[5][1] = ((fc[5][0]*s5)+(fc[5][1]*c5));
    fccikt[5][2] = fc[5][2];
    ffk[4][0] = (ufk[2][0]-fccikt[5][0]);
    ffk[4][1] = (ufk[2][1]-fccikt[5][1]);
    ffk[4][2] = (ufk[2][2]-fccikt[5][2]);
    ttk[4][0] = (utk[2][0]-(fccikt[5][2]+((tc[5][0]*c5)-(tc[5][1]*s5))));
    ttk[4][1] = (utk[2][1]-((tc[5][0]*s5)+(tc[5][1]*c5)));
    ttk[4][2] = (utk[2][2]-(tc[5][2]-fccikt[5][0]));
    fc[4][0] = (AnkAtk[4][0]-ffk[4][0]);
    fc[4][1] = (AnkAtk[4][1]-ffk[4][1]);
    fc[4][2] = -ffk[4][2];
    tc[4][0] = -(ttk[4][0]-fc[4][2]);
    tc[4][1] = -ttk[4][1];
    tc[4][2] = ((50.*Onkb[4][2])-(fc[4][0]+ttk[4][2]));
    fccikt[4][0] = ((fc[4][0]*c4)-(fc[4][1]*s4));
    fccikt[4][1] = ((fc[4][0]*s4)+(fc[4][1]*c4));
    fccikt[4][2] = fc[4][2];
    ffk[3][0] = (ufk[1][0]-fccikt[4][0]);
    ffk[3][1] = (ufk[1][1]-fccikt[4][1]);
    ffk[3][2] = (ufk[1][2]-fccikt[4][2]);
    ttk[3][0] = (utk[1][0]-(fccikt[4][2]+((tc[4][0]*c4)-(tc[4][1]*s4))));
    ttk[3][1] = (utk[1][1]-((tc[4][0]*s4)+(tc[4][1]*c4)));
    ttk[3][2] = (utk[1][2]-(tc[4][2]-fccikt[4][0]));
    fc[3][0] = (AnkAtk[3][0]-ffk[3][0]);
    fc[3][1] = (AnkAtk[3][1]-ffk[3][1]);
    fc[3][2] = -ffk[3][2];
    tc[3][0] = -(ttk[3][0]-fc[3][2]);
    tc[3][1] = -ttk[3][1];
    tc[3][2] = ((50.*Onkb[3][2])-(fc[3][0]+ttk[3][2]));
    fccikt[3][0] = ((fc[3][0]*c3)-(fc[3][1]*s3));
    fccikt[3][1] = ((fc[3][0]*s3)+(fc[3][1]*c3));
    fccikt[3][2] = fc[3][2];
    ffk[2][0] = -fccikt[3][0];
    ffk[2][1] = -fccikt[3][1];
    ffk[2][2] = -fccikt[3][2];
    ttk[2][0] = -((tc[3][0]*c3)-(tc[3][1]*s3));
    ttk[2][1] = -((tc[3][0]*s3)+(tc[3][1]*c3));
    ttk[2][2] = -tc[3][2];
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
    ttk[1][0] = -(tc[2][0]+(fccikt[2][2]*q[2]));
    ttk[1][1] = -tc[2][1];
    ttk[1][2] = -(tc[2][2]-(fccikt[2][0]*q[2]));
    fc[1][0] = -ffk[1][0];
    fc[1][1] = -ffk[1][1];
    fc[1][2] = -ffk[1][2];
    tc[1][0] = -ttk[1][0];
    tc[1][1] = -ttk[1][1];
    tc[1][2] = -ttk[1][2];
    fccikt[1][0] = fc[1][0];
    fccikt[1][1] = fc[1][1];
    fccikt[1][2] = fc[1][2];
    ffk[0][0] = (ufk[0][0]-fccikt[1][0]);
    ffk[0][1] = (ufk[0][1]-fccikt[1][1]);
    ffk[0][2] = (ufk[0][2]-fccikt[1][2]);
    ttk[0][0] = (utk[0][0]-tc[1][0]);
    ttk[0][1] = (utk[0][1]-(tc[1][1]-(fccikt[1][2]*q[1])));
    ttk[0][2] = (utk[0][2]-(tc[1][2]+(fccikt[1][1]*q[1])));
    fc[0][0] = -ffk[0][0];
    fc[0][1] = -ffk[0][1];
    fc[0][2] = -ffk[0][2];
    tc[0][0] = -ttk[0][0];
    tc[0][1] = -ttk[0][1];
    tc[0][2] = (udot[0]-ttk[0][2]);
    force[0][0] = fc[0][0];
    torque[0][0] = tc[0][0];
    force[0][1] = fc[0][1];
    torque[0][1] = tc[0][1];
    force[0][2] = fc[0][2];
    torque[0][2] = tc[0][2];
    force[1][0] = fc[3][0];
    torque[1][0] = tc[3][0];
    force[1][1] = fc[3][1];
    torque[1][1] = tc[3][1];
    force[1][2] = fc[3][2];
    torque[1][2] = tc[3][2];
    force[2][0] = fc[4][0];
    torque[2][0] = tc[4][0];
    force[2][1] = fc[4][1];
    torque[2][1] = tc[4][1];
    force[2][2] = fc[4][2];
    torque[2][2] = tc[4][2];
    force[3][0] = fc[5][0];
    torque[3][0] = tc[5][0];
    force[3][1] = fc[5][1];
    torque[3][1] = tc[5][1];
    force[3][2] = fc[5][2];
    torque[3][2] = tc[5][2];
    force[4][0] = fc[6][0];
    torque[4][0] = tc[6][0];
    force[4][1] = fc[6][1];
    torque[4][1] = tc[6][1];
    force[4][2] = fc[6][2];
    torque[4][2] = tc[6][2];
    force[5][0] = fc[7][0];
    torque[5][0] = tc[7][0];
    force[5][1] = fc[7][1];
    torque[5][1] = tc[7][1];
    force[5][2] = fc[7][2];
    torque[5][2] = tc[7][2];
/*
Compute reaction forces for tree weld joints
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain  132 adds/subtracts/negates
                     49 multiplies
                      0 divides
                    147 assignments
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
    double lk[6][3],hnk[6][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(19,23);
        return;
    }
    lm[0] = (vnk[7][0]+(vnk[6][0]+(vnk[5][0]+(vnk[3][0]+vnk[4][0]))));
    lm[1] = (vnk[7][1]+(vnk[6][1]+(vnk[5][1]+(vnk[3][1]+vnk[4][1]))));
    lm[2] = 0.;
    am[0] = 0.;
    am[1] = 0.;
    temp[0] = (((50.*wk[6][2])+((vnk[6][1]*(rnk[6][0]-com[0]))-(vnk[6][0]*(
      rnk[6][1]-com[1]))))+(((50.*wk[5][2])+((vnk[5][1]*(rnk[5][0]-com[0]))-(
      vnk[5][0]*(rnk[5][1]-com[1]))))+((u[0]+((50.*wk[3][2])+((vnk[3][1]*(
      rnk[3][0]-com[0]))-(vnk[3][0]*(rnk[3][1]-com[1])))))+((50.*wk[4][2])+((
      vnk[4][1]*(rnk[4][0]-com[0]))-(vnk[4][0]*(rnk[4][1]-com[1])))))));
    am[2] = (((50.*wk[7][2])+((vnk[7][1]*(rnk[7][0]-com[0]))-(vnk[7][0]*(
      rnk[7][1]-com[1]))))+temp[0]);
    temp[0] = (((50.*(wk[6][2]*wk[6][2]))+((vnk[6][0]*vnk[6][0])+(vnk[6][1]*
      vnk[6][1])))+(((50.*(wk[5][2]*wk[5][2]))+((vnk[5][0]*vnk[5][0])+(vnk[5][1]
      *vnk[5][1])))+(((50.*(wk[4][2]*wk[4][2]))+((vnk[4][0]*vnk[4][0])+(
      vnk[4][1]*vnk[4][1])))+((u[0]*u[0])+((50.*(wk[3][2]*wk[3][2]))+((vnk[3][0]
      *vnk[3][0])+(vnk[3][1]*vnk[3][1])))))));
    *ke = (.5*(((50.*(wk[7][2]*wk[7][2]))+((vnk[7][0]*vnk[7][0])+(vnk[7][1]*
      vnk[7][1])))+temp[0]));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   48 adds/subtracts/negates
                     37 multiplies
                      0 divides
                      9 assignments
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
    double ikcnkt[8][3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(20,23);
        return;
    }
    *mtoto = 5.;
    cm[0] = com[0];
    cm[1] = com[1];
    cm[2] = 0.;
    temp[0] = (((rnk[5][1]*rnk[5][1])+((10.*(cnk[5][0][0]*cnk[5][0][0]))+(50.*(
      cnk[5][0][1]*cnk[5][0][1]))))+(((rnk[3][1]*rnk[3][1])+((10.*(cnk[3][0][0]*
      cnk[3][0][0]))+(50.*(cnk[3][0][1]*cnk[3][0][1]))))+((rnk[4][1]*rnk[4][1])+
      ((10.*(cnk[4][0][0]*cnk[4][0][0]))+(50.*(cnk[4][0][1]*cnk[4][0][1]))))));
    icm[0][0] = (1.+((((rnk[7][1]*rnk[7][1])+((10.*(cnk[7][0][0]*cnk[7][0][0]))+
      (50.*(cnk[7][0][1]*cnk[7][0][1]))))+(((rnk[6][1]*rnk[6][1])+((10.*(
      cnk[6][0][0]*cnk[6][0][0]))+(50.*(cnk[6][0][1]*cnk[6][0][1]))))+temp[0]))-
      (5.*(com[1]*com[1]))));
    temp[0] = ((((10.*(cnk[5][0][0]*cnk[5][1][0]))+(50.*(cnk[5][0][1]*
      cnk[5][1][1])))-(rnk[5][0]*rnk[5][1]))+((((10.*(cnk[3][0][0]*cnk[3][1][0])
      )+(50.*(cnk[3][0][1]*cnk[3][1][1])))-(rnk[3][0]*rnk[3][1]))+(((10.*(
      cnk[4][0][0]*cnk[4][1][0]))+(50.*(cnk[4][0][1]*cnk[4][1][1])))-(rnk[4][0]*
      rnk[4][1]))));
    icm[0][1] = ((5.*(com[0]*com[1]))+((((10.*(cnk[7][0][0]*cnk[7][1][0]))+(50.*
      (cnk[7][0][1]*cnk[7][1][1])))-(rnk[7][0]*rnk[7][1]))+((((10.*(cnk[6][0][0]
      *cnk[6][1][0]))+(50.*(cnk[6][0][1]*cnk[6][1][1])))-(rnk[6][0]*rnk[6][1]))+
      temp[0])));
    icm[0][2] = 0.;
    icm[1][0] = icm[0][1];
    temp[0] = (((rnk[5][0]*rnk[5][0])+((10.*(cnk[5][1][0]*cnk[5][1][0]))+(50.*(
      cnk[5][1][1]*cnk[5][1][1]))))+(((rnk[3][0]*rnk[3][0])+((10.*(cnk[3][1][0]*
      cnk[3][1][0]))+(50.*(cnk[3][1][1]*cnk[3][1][1]))))+((rnk[4][0]*rnk[4][0])+
      ((10.*(cnk[4][1][0]*cnk[4][1][0]))+(50.*(cnk[4][1][1]*cnk[4][1][1]))))));
    icm[1][1] = (1.+((((rnk[7][0]*rnk[7][0])+((10.*(cnk[7][1][0]*cnk[7][1][0]))+
      (50.*(cnk[7][1][1]*cnk[7][1][1]))))+(((rnk[6][0]*rnk[6][0])+((10.*(
      cnk[6][1][0]*cnk[6][1][0]))+(50.*(cnk[6][1][1]*cnk[6][1][1]))))+temp[0]))-
      (5.*(com[0]*com[0]))));
    icm[1][2] = 0.;
    icm[2][0] = icm[0][2];
    icm[2][1] = icm[1][2];
    icm[2][2] = (251.+((((rnk[7][0]*rnk[7][0])+(rnk[7][1]*rnk[7][1]))+(((
      rnk[6][0]*rnk[6][0])+(rnk[6][1]*rnk[6][1]))+(((rnk[5][0]*rnk[5][0])+(
      rnk[5][1]*rnk[5][1]))+(((rnk[3][0]*rnk[3][0])+(rnk[3][1]*rnk[3][1]))+((
      rnk[4][0]*rnk[4][0])+(rnk[4][1]*rnk[4][1]))))))-(5.*((com[0]*com[0])+(
      com[1]*com[1])))));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   59 adds/subtracts/negates
                     94 multiplies
                      0 divides
                     16 assignments
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

    if ((coord < 0) || (coord > 7)) {
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
  sdstate:    84 adds    80 multiplies     0 divides   247 assignments
  sdderiv:   729 adds   735 multiplies    10 divides  1083 assignments
  sdresid:   284 adds   229 multiplies     0 divides   312 assignments
  sdreac:    132 adds    49 multiplies     0 divides   147 assignments
  sdmom:      48 adds    37 multiplies     0 divides     9 assignments
  sdsys:      59 adds    94 multiplies     0 divides    16 assignments
*/
