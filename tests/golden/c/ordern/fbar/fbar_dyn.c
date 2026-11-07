/*
Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)


ROADMAP (fbar.sd)

Bodies        Inb
No  Name      body Joint type  Coords q         Multipliers
--- --------- ---- ----------- ---------------- -----------------------
 -1 $ground                                    |
  0 crank      -1  Pin           0             |
  1 rocker     -1  Pin           1             |
  2 connect     1  Pin           2             |

Loop Joints                    Pseudo Coords lq

  3 connect     0  Pin           0             |  0   1   2   3   4 

*/
#include <math.h>
#include <stdio.h>

typedef struct {
    int ground_,nbod_,ndof_,ncons_,nloop_,nldof_,nloopc_,nball_,nlball_,npres_,
      nuser_;
    int jtype_[4],inb_[4],outb_[4],njntdof_[4],njntc_[4],njntp_[4],firstq_[4],
      ballq_[4],firstm_[4],firstp_[4];
    int trans_[4];
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
    double grav_[3],mk_[3],ik_[3][3][3],pin_[3][3];
    double rk_[3][3],ri_[3][3],pres_[3],stabvel_,stabpos_;
    double inbpin1_[1][3],inbpin2_[1][3],inbpin3_[1][3],inbref_[1][3];
    double bodypin_[1][3],bodyref_[1][3],lbtj_[1][3],litj_[1][3],lpres_[1];
    int mfrcflg_,roustate_,vpkflg_,inerflg_,mmflg_,mmlduflg_,wwflg_,ltauflg_,
      fs0flg_,mmap_[3];
    int gravq_[3],mkq_[3],ikq_[3][3][3],pinq_[3][3],rkq_[3][3],riq_[3][3],presq_
      [3],stabvelq_,stabposq_;
    int inbpin1q_[1][3],inbpin2q_[1][3],inbpin3q_[1][3],inbrefq_[1][3],bodypinq_
      [1][3],bodyrefq_[1][3],lbtjq_[1][3],litjq_[1][3],lpresq_[1];
    double mtot_,psmkg_,rhead_[3][3],rcom_[3][3],mkrcomt_[3][3][3],psikg_[3][3],
      psrcomg_[3],psrkg_[3],psrig_[3],psmk_[3],psik_[3][3][3],psrcom_[3][3],
      psrk_[3][3],psri_[3][3];
    double ipin_[1][3],ipin2_[1][3],iref_[1][3],iperp_[1][3],opin_[1][3],oref_[1
      ][3],operp_[1][3],ghand_[1];
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
#define inbpin1 (sdginput.inbpin1_)
#define inbpin2 (sdginput.inbpin2_)
#define inbpin3 (sdginput.inbpin3_)
#define inbref (sdginput.inbref_)
#define bodypin (sdginput.bodypin_)
#define bodyref (sdginput.bodyref_)
#define lbtj (sdginput.lbtj_)
#define litj (sdginput.litj_)
#define lpres (sdginput.lpres_)
#define ipin (sdginput.ipin_)
#define ipin2 (sdginput.ipin2_)
#define iref (sdginput.iref_)
#define iperp (sdginput.iperp_)
#define opin (sdginput.opin_)
#define oref (sdginput.oref_)
#define operp (sdginput.operp_)
#define ghand (sdginput.ghand_)
#define mfrcflg (sdginput.mfrcflg_)
#define roustate (sdginput.roustate_)
#define vpkflg (sdginput.vpkflg_)
#define inerflg (sdginput.inerflg_)
#define mmflg (sdginput.mmflg_)
#define mmlduflg (sdginput.mmlduflg_)
#define wwflg (sdginput.wwflg_)
#define ltauflg (sdginput.ltauflg_)
#define fs0flg (sdginput.fs0flg_)
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
#define lbtjq (sdginput.lbtjq_)
#define litjq (sdginput.litjq_)
#define lpresq (sdginput.lpresq_)
#define inbpin1q (sdginput.inbpin1q_)
#define inbpin2q (sdginput.inbpin2q_)
#define inbpin3q (sdginput.inbpin3q_)
#define inbrefq (sdginput.inbrefq_)
#define bodypinq (sdginput.bodypinq_)
#define bodyrefq (sdginput.bodyrefq_)

typedef struct {
    double curtim_,q_[3],qn_[3],u_[3],cnk_[3][3][3],cnb_[3][3][3];
    double rnk_[3][3],vnk_[3][3],wk_[3][3],rnb_[3][3],vnb_[3][3],wb_[3][3],
      wbrcom_[3][3],com_[3],rnkg_[3];
    double Cik_[3][3][3],rikt_[3][3][3],Iko_[3][3][3],mkrk_[3][3][3],Cib_[3][3][
      3];
    double Wkk_[3][3],Vkk_[3][3],dik_[3][3],rpp_[3][3],rpk_[3][3],rik_[3][3],
      rik2_[3][3];
    double rpri_[3][3],Wik_[3][3],Vik_[3][3],Wirk_[3][3],rkWkk_[3][3],Wkrpk_[3][
      3],VikWkr_[3][3];
    double perr_[5],verr_[5],aerr_[5],mult_[5],ufk_[3][3],utk_[3][3],mfk_[3][3],
      mtk_[3][3];
    double utau_[3],mtau_[3],uacc_[3],uvel_[3],upos_[3];
    double Cio_[1][3][3],Cibo_[1][3][3],Cibob_[1][3][3],eul1_[1],eul2_[1],eul3_[
      1],eul4_[1];
    double eul1dot_[1],eul2dot_[1],eul3dot_[1],eul4dot_[1],eul1a_[1],eul2a_[1],
      eul3a_[1];
    double ceul1_[1],ceul2_[1],ceul3_[1],seul1_[1],seul2_[1],seul3_[1],sli1_[1],
      sli2_[1],sli3_[1];
    double sli1v_[1],sli2v_[1],sli3v_[1],sli1a_[1],sli2a_[1],sli3a_[1],ipin2x_[1
      ][3];
    double ltau_[1],mltau_[1],lacc_[1],lvel_[1],lpos_[1],lq_[1];
    double mltaufi_[1][3],mltaufo_[1][3],mltauti_[1][3],mltauto_[1][3],Woio_[1][
      3],Woiob_[1][3],Ooiob_[1][3];
    double vt1_[1][3],vt2_[1][3],vt2a_[1][3],vt3_[1][3],vt4_[1][3],vt5_[1][3],
      vt6_[1][3];
    double vt7_[1][3],vt8_[1][3],vt9_[1][3],vt10_[1][3],vt10a_[1][3],vt10b_[1][3
      ],vt10c_[1][3];
    double vt11_[1][3],vt12_[1][3],vt13_[1][3],vt13a_[1][3],vt14_[1][3],vt15_[1
      ][3];
    double vt16_[1][3],vt17_[1][3],vt18_[1][3],vt19_[1][3],vt20_[1][3],vt21_[1][
      3];
    double vt22_[1][3],vt23_[1][3],vt24_[1][3],vt25_[1][3],vt26_[1][3],vt26a_[1
      ][3],vt27_[1][3];
    double s0_,c0_,s1_,c1_,s2_,c2_;
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
#define Cio (sdgstate.Cio_)
#define Cibo (sdgstate.Cibo_)
#define Cibob (sdgstate.Cibob_)
#define ltau (sdgstate.ltau_)
#define mltau (sdgstate.mltau_)
#define lacc (sdgstate.lacc_)
#define lvel (sdgstate.lvel_)
#define lpos (sdgstate.lpos_)
#define lq (sdgstate.lq_)
#define mltaufi (sdgstate.mltaufi_)
#define mltaufo (sdgstate.mltaufo_)
#define mltauti (sdgstate.mltauti_)
#define mltauto (sdgstate.mltauto_)
#define Woio (sdgstate.Woio_)
#define Woiob (sdgstate.Woiob_)
#define Ooiob (sdgstate.Ooiob_)
#define eul1 (sdgstate.eul1_)
#define eul2 (sdgstate.eul2_)
#define eul3 (sdgstate.eul3_)
#define eul4 (sdgstate.eul4_)
#define eul1dot (sdgstate.eul1dot_)
#define eul2dot (sdgstate.eul2dot_)
#define eul3dot (sdgstate.eul3dot_)
#define eul4dot (sdgstate.eul4dot_)
#define eul1a (sdgstate.eul1a_)
#define eul2a (sdgstate.eul2a_)
#define eul3a (sdgstate.eul3a_)
#define ceul1 (sdgstate.ceul1_)
#define ceul2 (sdgstate.ceul2_)
#define ceul3 (sdgstate.ceul3_)
#define seul1 (sdgstate.seul1_)
#define seul2 (sdgstate.seul2_)
#define seul3 (sdgstate.seul3_)
#define sli1 (sdgstate.sli1_)
#define sli2 (sdgstate.sli2_)
#define sli3 (sdgstate.sli3_)
#define sli1v (sdgstate.sli1v_)
#define sli2v (sdgstate.sli2v_)
#define sli3v (sdgstate.sli3v_)
#define sli1a (sdgstate.sli1a_)
#define sli2a (sdgstate.sli2a_)
#define sli3a (sdgstate.sli3a_)
#define ipin2x (sdgstate.ipin2x_)
#define vt1 (sdgstate.vt1_)
#define vt2 (sdgstate.vt2_)
#define vt2a (sdgstate.vt2a_)
#define vt3 (sdgstate.vt3_)
#define vt4 (sdgstate.vt4_)
#define vt5 (sdgstate.vt5_)
#define vt6 (sdgstate.vt6_)
#define vt7 (sdgstate.vt7_)
#define vt8 (sdgstate.vt8_)
#define vt9 (sdgstate.vt9_)
#define vt10 (sdgstate.vt10_)
#define vt10a (sdgstate.vt10a_)
#define vt10b (sdgstate.vt10b_)
#define vt10c (sdgstate.vt10c_)
#define vt11 (sdgstate.vt11_)
#define vt12 (sdgstate.vt12_)
#define vt13 (sdgstate.vt13_)
#define vt13a (sdgstate.vt13a_)
#define vt14 (sdgstate.vt14_)
#define vt15 (sdgstate.vt15_)
#define vt16 (sdgstate.vt16_)
#define vt17 (sdgstate.vt17_)
#define vt18 (sdgstate.vt18_)
#define vt19 (sdgstate.vt19_)
#define vt20 (sdgstate.vt20_)
#define vt21 (sdgstate.vt21_)
#define vt22 (sdgstate.vt22_)
#define vt23 (sdgstate.vt23_)
#define vt24 (sdgstate.vt24_)
#define vt25 (sdgstate.vt25_)
#define vt26 (sdgstate.vt26_)
#define vt26a (sdgstate.vt26a_)
#define vt27 (sdgstate.vt27_)
#define s0 (sdgstate.s0_)
#define c0 (sdgstate.c0_)
#define s1 (sdgstate.s1_)
#define c1 (sdgstate.c1_)
#define s2 (sdgstate.s2_)
#define c2 (sdgstate.c2_)

typedef struct {
    double fs0_[3],qdot_[3],Otk_[3][3],Atk_[3][3],AiOiWi_[3][3],Fstar_[3][3];
    double Tstar_[3][3],Fstark_[3][3],Tstark_[3][3],IkWk_[3][3],WkIkWk_[3][3],
      gk_[3][3],IkbWk_[3][3],WkIkbWk_[3][3];
    double w0w0_[3],w1w1_[3],w2w2_[3],w0w1_[3],w0w2_[3],w1w2_[3];
    double w00w11_[3],w00w22_[3],w11w22_[3],ww_[5][5],qraux_[5];
    double DD_[3],PH1_[3][3],PH2_[3][3],HL1_[3][3],HL2_[3][3],G1_[3][3],G2_[3][3
      ];
    double P11_[3][3][3],Pd_[3][3][3],P22_[3][3][3],L11_[3][3][3],L21_[3][3][3],
      L22_[3][3][3],D11_[3][3][3],D22_[3][3][3];
    double N11_[3][3][3],N21_[3][3][3],N22_[3][3][3],psi11_[3][3][3],psi12_[3][3
      ][3],psi21_[3][3][3],psi22_[3][3][3];
    double psiD11_[3][3][3],psiD12_[3][3][3],psiD21_[3][3][3],psiD22_[3][3][3];
    double sL11_[3][3],sL21_[3][3],sL22_[3][3],sD1_[3][3],sD2_[3][3];
    double sD1INV_[3][3],sD2INV_[3][3],sL11D1_[3][3],sL22D2_[3][3],sD1L21_[3][3]
      ;
    double ping_[3][3],hngpt_[3][3];
    double ltaufk_[3][3],ltautk_[3][3],ltauc_[1],ltci_[1][3],ltc_[1][3],lfci_[1
      ][3],lfc_[1][3];
    double Tinb_[1][3],Toutb_[1][3],ltaufi_[1][3],ltaufo_[1][3],ltauti_[1][3],
      ltauto_[1][3];
    int wmap_[5],multmap_[5],jpvt_[5],wsiz_,wrank_;
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
#define ltaufk (sdglhs.ltaufk_)
#define ltautk (sdglhs.ltautk_)
#define ltci (sdglhs.ltci_)
#define ltc (sdglhs.ltc_)
#define lfci (sdglhs.lfci_)
#define lfc (sdglhs.lfc_)
#define Tinb (sdglhs.Tinb_)
#define Toutb (sdglhs.Toutb_)
#define ltauc (sdglhs.ltauc_)
#define ltaufi (sdglhs.ltaufi_)
#define ltaufo (sdglhs.ltaufo_)
#define ltauti (sdglhs.ltauti_)
#define ltauto (sdglhs.ltauto_)
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
    double fs_[3],udot_[3],tauc_[3],dyad_[3][3][3],fc_[3][3],tc_[3][3];
    double ank_[3][3],onk_[3][3],Onkb_[3][3],AOnkri_[3][3],Ankb_[3][3],AnkAtk_[3
      ][3],anb_[3][3],onb_[3][3],dyrcom_[3][3];
    double ffk_[3][3],ttk_[3][3],fccikt_[3][3],ffkb_[3][3],ttkb_[3][3];
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
    /* ndof */ 3,
    /* ncons */ 5,
    /* nloop */ 1,
    /* nldof */ 1,
    /* nloopc */ 5,
    /* nball */ 0,
    /* nlball */ 0,
    /* npres */ 0,
    /* nuser */ 0,
    /* jtype[0] */ 1,
    /* jtype[1] */ 1,
    /* jtype[2] */ 1,
    /* jtype[3] */ 1,
    /* inb[0] */ -1,
    /* inb[1] */ -1,
    /* inb[2] */ 1,
    /* inb[3] */ 0,
    /* outb[0] */ 0,
    /* outb[1] */ 1,
    /* outb[2] */ 2,
    /* outb[3] */ 2,
    /* njntdof[0] */ 1,
    /* njntdof[1] */ 1,
    /* njntdof[2] */ 1,
    /* njntdof[3] */ 1,
    /* njntc[0] */ 0,
    /* njntc[1] */ 0,
    /* njntc[2] */ 0,
    /* njntc[3] */ 5,
    /* njntp[0] */ 0,
    /* njntp[1] */ 0,
    /* njntp[2] */ 0,
    /* njntp[3] */ 0,
    /* firstq[0] */ 0,
    /* firstq[1] */ 1,
    /* firstq[2] */ 2,
    /* firstq[3] */ 0,
    /* ballq[0] */ -104,
    /* ballq[1] */ -104,
    /* ballq[2] */ -104,
    /* ballq[3] */ -104,
    /* firstm[0] */ -1,
    /* firstm[1] */ -1,
    /* firstm[2] */ -1,
    /* firstm[3] */ 0,
    /* firstp[0] */ -1,
    /* firstp[1] */ -1,
    /* firstp[2] */ -1,
    /* firstp[3] */ -1,
    /* trans[0] */ 0,
    /* trans[1] */ 0,
    /* trans[2] */ 0,
    /* trans[3] */ 0,
};
sdginput_t sdginput = {
/* Model parameters from the input file */

/* gravity */
    /* grav[0] */ 0.,
    /* grav[1] */ -9.8,
    /* grav[2] */ 0.,

/* mass */
    /* mk[0] */ 1.,
    /* mk[1] */ 2.,
    /* mk[2] */ 3.,

/* inertia */
    /* ik[0][0][0] */ 1.,
    /* ik[0][0][1] */ 0.,
    /* ik[0][0][2] */ 0.,
    /* ik[0][1][0] */ 0.,
    /* ik[0][1][1] */ 0.,
    /* ik[0][1][2] */ 0.,
    /* ik[0][2][0] */ 0.,
    /* ik[0][2][1] */ 0.,
    /* ik[0][2][2] */ 1.,
    /* ik[1][0][0] */ 2.,
    /* ik[1][0][1] */ 0.,
    /* ik[1][0][2] */ 0.,
    /* ik[1][1][0] */ 0.,
    /* ik[1][1][1] */ 0.,
    /* ik[1][1][2] */ 0.,
    /* ik[1][2][0] */ 0.,
    /* ik[1][2][1] */ 0.,
    /* ik[1][2][2] */ 2.,
    /* ik[2][0][0] */ 0.,
    /* ik[2][0][1] */ 0.,
    /* ik[2][0][2] */ 0.,
    /* ik[2][1][0] */ 0.,
    /* ik[2][1][1] */ 3.,
    /* ik[2][1][2] */ 0.,
    /* ik[2][2][0] */ 0.,
    /* ik[2][2][1] */ 0.,
    /* ik[2][2][2] */ 3.,

/* tree hinge axis vectors */
    /* pin[0][0] */ 0.,
    /* pin[0][1] */ 0.,
    /* pin[0][2] */ 1.,
    /* pin[1][0] */ 0.,
    /* pin[1][1] */ 0.,
    /* pin[1][2] */ 1.,
    /* pin[2][0] */ 0.,
    /* pin[2][1] */ 0.,
    /* pin[2][2] */ 1.,

/* tree bodytojoint vectors */
    /* rk[0][0] */ 0.,
    /* rk[0][1] */ -.5,
    /* rk[0][2] */ 0.,
    /* rk[1][0] */ 0.,
    /* rk[1][1] */ -1.,
    /* rk[1][2] */ 0.,
    /* rk[2][0] */ 1.5,
    /* rk[2][1] */ 0.,
    /* rk[2][2] */ 0.,

/* tree inbtojoint vectors */
    /* ri[0][0] */ 0.,
    /* ri[0][1] */ 0.,
    /* ri[0][2] */ 0.,
    /* ri[1][0] */ 3.,
    /* ri[1][1] */ 0.,
    /* ri[1][2] */ 0.,
    /* ri[2][0] */ 0.,
    /* ri[2][1] */ 1.,
    /* ri[2][2] */ 0.,

/* tree prescribed motion */
    /* pres[0] */ 0.,
    /* pres[1] */ 0.,
    /* pres[2] */ 0.,

/* stabilization parameters */
    /* stabvel */ 0.,
    /* stabpos */ 0.,

/* loop first inboard pin vectors */
    /* inbpin1[0][0] */ 0.,
    /* inbpin1[0][1] */ 0.,
    /* inbpin1[0][2] */ 1.,

/* loop second inboard pin vectors */
    /* inbpin2[0][0] */ 0.,
    /* inbpin2[0][1] */ 0.,
    /* inbpin2[0][2] */ 0.,

/* loop third inboard pin vectors */
    /* inbpin3[0][0] */ 0.,
    /* inbpin3[0][1] */ 0.,
    /* inbpin3[0][2] */ 0.,

/* loop inboard reference vectors */
    /* inbref[0][0] */ 0.,
    /* inbref[0][1] */ 0.,
    /* inbref[0][2] */ 0.,

/* loop body pin vectors */
    /* bodypin[0][0] */ 0.,
    /* bodypin[0][1] */ 0.,
    /* bodypin[0][2] */ 0.,

/* loop body reference vectors */
    /* bodyref[0][0] */ 0.,
    /* bodyref[0][1] */ 0.,
    /* bodyref[0][2] */ 0.,

/* loop bodytojoint vectors */
    /* lbtj[0][0] */ -1.5,
    /* lbtj[0][1] */ 0.,
    /* lbtj[0][2] */ 0.,

/* loop inbtojoint vectors */
    /* litj[0][0] */ 0.,
    /* litj[0][1] */ .5,
    /* litj[0][2] */ 0.,

/* loop prescribed motion */
    /* lpres[0] */ 0.,

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
    /* mmap[0] */ 0,
    /* mmap[1] */ 1,
    /* mmap[2] */ 2,

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
    /* presq[2] */ 0,
    /* stabvelq */ 3,
    /* stabposq */ 3,
    /* inbpin1q[0][0] */ 0,
    /* inbpin1q[0][1] */ 0,
    /* inbpin1q[0][2] */ 0,
    /* inbpin2q[0][0] */ 0,
    /* inbpin2q[0][1] */ 0,
    /* inbpin2q[0][2] */ 0,
    /* inbpin3q[0][0] */ 0,
    /* inbpin3q[0][1] */ 0,
    /* inbpin3q[0][2] */ 0,
    /* inbrefq[0][0] */ 0,
    /* inbrefq[0][1] */ 0,
    /* inbrefq[0][2] */ 0,
    /* bodypinq[0][0] */ 0,
    /* bodypinq[0][1] */ 0,
    /* bodypinq[0][2] */ 0,
    /* bodyrefq[0][0] */ 0,
    /* bodyrefq[0][1] */ 0,
    /* bodyrefq[0][2] */ 0,
    /* lbtjq[0][0] */ 0,
    /* lbtjq[0][1] */ 0,
    /* lbtjq[0][2] */ 0,
    /* litjq[0][0] */ 0,
    /* litjq[0][1] */ 0,
    /* litjq[0][2] */ 0,
    /* lpresq[0] */ 0,

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
    for (k = 0; k < 3; k++) {
        for (i = 0; i < 3; i++) {
            if (pinq[k][i] == 1) {
                sdseterr(7,28);
            }
        }
    }
    for (k = 0; k < 1; k++) {
        for (i = 0; i < 3; i++) {
            if (inbpin1q[k][i] == 1) {
                sdseterr(7,34);
            }
            if (inbpin2q[k][i] == 1) {
                sdseterr(7,34);
            }
            if (inbpin3q[k][i] == 1) {
                sdseterr(7,34);
            }
            if (inbrefq[k][i] == 1) {
                sdseterr(7,35);
            }
            if (bodypinq[k][i] == 1) {
                sdseterr(7,36);
            }
            if (bodyrefq[k][i] == 1) {
                sdseterr(7,37);
            }
            if (lbtjq[k][i] == 1) {
                sdseterr(7,38);
            }
            if (litjq[k][i] == 1) {
                sdseterr(7,39);
            }
        }
    }

/* Normalize pin vectors if necessary */


/* Zero out ping and hngpt */

    for (i = 0; i < 3; i++) {
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

    mtot = 6.;
    sdserialno(&i);
    if (i != 10001) {
        sdseterr(7,41);
    }
    roustate = 1;
}

/* Convert state to form using 1-2-3 Euler angles for ball joints. */

void sdst2ang(double st[3],
    double stang[3])
{
    int i;

    for (i = 0; i < 3; i++) {
        stang[i] = st[i];
    }
}

/* Convert 1-2-3 form of state back to Euler parameters for ball joints. */

void sdang2st(double stang[3],
    double st[3])
{
    int i;

    for (i = 0; i < 3; i++) {
        st[i] = stang[i];
    }
}

/* Normalize Euler parameters in state. */

void sdnrmsterr(double st[3],
    double normst[3],
    int routine)
{
    int i;

    for (i = 0; i < 3; i++) {
        normst[i] = st[i];
    }
}

void sdnormst(double st[3],
    double normst[3])
{

    sdnrmsterr(st,normst,0);
}

void sdstate(double timein,
    double qin[3],
    double uin[3])
{
/*
Compute kinematic information and store it in sdgstate.

Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)
*/
    int i,j,qchg,uchg;
    double e1,e2,e3,e4;

    if ((roustate != 1) && (roustate != 2) && (roustate != 3)) {
        sdseterr(8,22);
        return;
    }
    if (roustate == 1) {
        for (i = 0; i < 3; i++) {
            if (presq[i] == 1) {
                sdseterr(8,31);
            }
        }
        for (i = 0; i < 1; i++) {
            if (lpresq[i] == 1) {
                sdseterr(8,40);
            }
        }
    }
/*
See if time or any qs have changed since last call
*/
    if ((roustate != 1) && (timein == curtim)) {
        qchg = 0;
        for (i = 0; i < 3; i++) {
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
        for (i = 0; i < 3; i++) {
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
    for (i = 0; i < 3; i++) {
        q[i] = qin[i];
    }
/*
Compute sines and cosines of q
*/
    s0 = sin(q[0]);
    c0 = cos(q[0]);
    s1 = sin(q[1]);
    c1 = cos(q[1]);
    s2 = sin(q[2]);
    c2 = cos(q[2]);
/*
Compute across-axis direction cosines Cik
*/
/*
Compute across-joint direction cosines Cib
*/
/*
Compute gravity
*/
    gk[2][0] = -(9.8*((s1*c2)+(s2*c1)));
    gk[2][1] = (9.8*((s1*s2)-(c1*c2)));
/*
Compute cnk & cnb (direction cosines in N)
*/
    cnk[2][0][0] = ((c1*c2)-(s1*s2));
    cnk[2][0][1] = -((s1*c2)+(s2*c1));
    cnk[2][1][0] = ((s1*c2)+(s2*c1));
    cnk[2][1][1] = ((c1*c2)-(s1*s2));
    cnb[0][0][0] = c0;
    cnb[0][0][1] = -s0;
    cnb[0][0][2] = 0.;
    cnb[0][1][0] = s0;
    cnb[0][1][1] = c0;
    cnb[0][1][2] = 0.;
    cnb[0][2][0] = 0.;
    cnb[0][2][1] = 0.;
    cnb[0][2][2] = 1.;
    cnb[1][0][0] = c1;
    cnb[1][0][1] = -s1;
    cnb[1][0][2] = 0.;
    cnb[1][1][0] = s1;
    cnb[1][1][1] = c1;
    cnb[1][1][2] = 0.;
    cnb[1][2][0] = 0.;
    cnb[1][2][1] = 0.;
    cnb[1][2][2] = 1.;
    cnb[2][0][0] = cnk[2][0][0];
    cnb[2][0][1] = cnk[2][0][1];
    cnb[2][0][2] = 0.;
    cnb[2][1][0] = cnk[2][1][0];
    cnb[2][1][1] = cnk[2][1][1];
    cnb[2][1][2] = 0.;
    cnb[2][2][0] = 0.;
    cnb[2][2][1] = 0.;
    cnb[2][2][2] = 1.;
/*
Compute Cio (dircos btw loop jt connected bodies)
*/
    Cio[0][0][0] = ((cnk[2][0][0]*c0)+(cnk[2][1][0]*s0));
    Cio[0][0][1] = ((cnk[2][0][1]*c0)+(cnk[2][1][1]*s0));
    Cio[0][1][0] = ((cnk[2][1][0]*c0)-(cnk[2][0][0]*s0));
    Cio[0][1][1] = ((cnk[2][1][1]*c0)-(cnk[2][0][1]*s0));
/*
Compute q-related auxiliary variables
*/
    rik[1][1] = (1.-(3.*s1));
    rik[2][0] = (s2-1.5);
/*
Compute rnk & rnb (mass center locations in N)
*/
    rnk[1][0] = (3.-s1);
    rnk[2][0] = ((rnk[1][0]-s1)-(1.5*cnk[2][0][0]));
    rnk[2][1] = ((2.*c1)-(1.5*cnk[2][1][0]));
    rnb[0][0] = -(.5*s0);
    rnb[0][1] = (.5*c0);
    rnb[0][2] = 0.;
    rnb[1][0] = rnk[1][0];
    rnb[1][1] = c1;
    rnb[1][2] = 0.;
    rnb[2][0] = rnk[2][0];
    rnb[2][1] = rnk[2][1];
    rnb[2][2] = 0.;
/*
Compute com (system mass center location in N)
*/
    com[0] = (.166666666666667*((3.*rnk[2][0])+((2.*rnk[1][0])-(.5*s0))));
    com[1] = (.166666666666667*((3.*rnk[2][1])+((.5*c0)+(2.*c1))));
    com[2] = 0.;
/*
Compute loop joint q-related temps
*/
    vt6[0][0] = -(rnk[2][0]+(.5*s0));
    vt6[0][1] = ((.5*c0)-rnk[2][1]);
    vt10c[0][0] = -((1.5*Cio[0][0][0])+((vt6[0][0]*c0)+(vt6[0][1]*s0)));
    vt10c[0][1] = -((1.5*Cio[0][1][0])+((vt6[0][1]*c0)-(vt6[0][0]*s0)));
    vt11[0][1] = (.5-vt10c[0][1]);
/*
Compute dircos inside loop joints
*/
/*
Decompose loop joint dircos into Euler angles or parameters
*/
    e1 = atan2(Cio[0][1][0],Cio[0][0][0]);
    eul1[0] = e1;
    ceul1[0] = cos(eul1[0]);
    seul1[0] = sin(eul1[0]);
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
    for (i = 0; i < 3; i++) {
        u[i] = uin[i];
    }
/*
Compute u-related auxiliary variables
*/
/*
Compute wk & wb (angular velocities)
*/
    wk[2][2] = (u[1]+u[2]);
    wb[0][0] = 0.;
    wb[0][1] = 0.;
    wb[0][2] = u[0];
    wb[1][0] = 0.;
    wb[1][1] = 0.;
    wb[1][2] = u[1];
    wb[2][0] = 0.;
    wb[2][1] = 0.;
    wb[2][2] = wk[2][2];
/*
Compute auxiliary variables involving wk
*/
/*
Compute temporaries for use in SDRHS
*/
    w2w2[0] = (u[0]*u[0]);
    w2w2[1] = (u[1]*u[1]);
    w2w2[2] = (wk[2][2]*wk[2][2]);
/*
Compute vnk & vnb (mass center linear velocities in N)
*/
    vnk[0][0] = -(.5*(u[0]*c0));
    vnk[0][1] = -(.5*(u[0]*s0));
    vnk[1][0] = -(u[1]*c1);
    vnk[1][1] = -(u[1]*s1);
    vnk[2][0] = ((vnk[1][0]-(u[1]*c1))-(1.5*(cnk[2][0][1]*wk[2][2])));
    vnk[2][1] = ((vnk[1][1]-(u[1]*s1))-(1.5*(cnk[2][1][1]*wk[2][2])));
    vnb[0][0] = vnk[0][0];
    vnb[0][1] = vnk[0][1];
    vnb[0][2] = 0.;
    vnb[1][0] = vnk[1][0];
    vnb[1][1] = vnk[1][1];
    vnb[1][2] = 0.;
    vnb[2][0] = vnk[2][0];
    vnb[2][1] = vnk[2][1];
    vnb[2][2] = 0.;
/*
Compute loop joint u-related temps
*/
    vt9[0][0] = (vnk[0][0]-vnk[2][0]);
    vt9[0][1] = (vnk[0][1]-vnk[2][1]);
    vt12[0][0] = (((1.5*(Cio[0][0][1]*wk[2][2]))-(.5*u[0]))+((vt9[0][0]*c0)+(
      vt9[0][1]*s0)));
    vt12[0][1] = ((1.5*(Cio[0][1][1]*wk[2][2]))+((vt9[0][1]*c0)-(vt9[0][0]*s0)))
      ;
/*
Compute angvel inside loop joints
*/
    Woio[0][2] = (wk[2][2]-u[0]);
/*
Compute derivatives of loop joint pseudo-coordinates
*/
/*
Compute qdot (kinematical equations)
*/
    qdot[0] = u[0];
    qdot[1] = u[1];
    qdot[2] = u[2];
/*
Compute constraint velocity errors
*/
    verr[2] = (vt12[0][0]+(u[0]*vt11[0][1]));
    verr[3] = (vt12[0][1]+(u[0]*vt10c[0][0]));
    skipus: ;
/*
Initialize applied forces and torques to zero
*/
    for (i = 0; i < 3; i++) {
        for (j = 0; j < 3; j++) {
            ufk[i][j] = 0.;
            utk[i][j] = 0.;
        }
    }
    for (i = 0; i < 3; i++) {
        utau[i] = 0.;
    }
    for (i = 0; i < 1; i++) {
        ltau[i] = 0.;
    }
    ltauflg = 0;
    fs0flg = 0;
/*
Set lqs -- sdpsstate may override some
*/
    lq[0] = eul1[0];
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   54 adds/subtracts/negates
                     70 multiplies
                      0 divides
                    130 assignments
*/
}

void sdqdot(double oqdot[3])
{
/*
Return position coordinate derivatives for tree joints.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(63,23);
        return;
    }
    for (i = 0; i <= 2; i++) {
        oqdot[i] = qdot[i];
    }
}

void sdu2qdot(double uin[3],
    double oqdot[3])
{
/*
Convert velocities to qdots.
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(64,23);
        return;
    }
    for (i = 0; i <= 2; i++) {
        oqdot[i] = uin[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      3 assignments
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
        ping[0][2] = 1.;
        ping[1][2] = 1.;
        ping[2][2] = 1.;
/*
Compute hngpt (hinge pts in ground frame)
*/
        hngpt[1][0] = (rnk[1][0]+s1);
        hngpt[2][0] = (rnk[2][0]+(1.5*cnk[2][0][0]));
        hngpt[2][1] = (rnk[2][1]+(1.5*cnk[2][1][0]));
        vpkflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    3 adds/subtracts/negates
                      2 multiplies
                      0 divides
                      6 assignments
*/
}

void sddoltau(void)
{

/*
Compute effect of loop hinge torques
*/
    if (ltauflg == 0) {
/*
Compute forces due to loop taus
*/
        ltauflg = 1;
    }
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
        Atk[0][1] = -(.5*(u[0]*u[0]));
        Atk[1][1] = -(u[1]*u[1]);
        AiOiWi[2][1] = (Atk[1][1]-(u[1]*u[1]));
        Atk[2][0] = ((1.5*(wk[2][2]*wk[2][2]))+(AiOiWi[2][1]*s2));
        Atk[2][1] = (AiOiWi[2][1]*c2);
        inerflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    4 adds/subtracts/negates
                      8 multiplies
                      0 divides
                      5 assignments
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
        Fstar[0][0] = ((9.8*s0)-ufk[0][0]);
        Fstar[0][1] = ((Atk[0][1]+(9.8*c0))-ufk[0][1]);
        Fstar[1][0] = ((19.6*s1)-ufk[1][0]);
        Fstar[1][1] = ((2.*(Atk[1][1]+(9.8*c1)))-ufk[1][1]);
        Fstar[2][0] = ((3.*(Atk[2][0]-gk[2][0]))-ufk[2][0]);
        Fstar[2][1] = ((3.*(Atk[2][1]-gk[2][1]))-ufk[2][1]);
/*
Compute Tstar (torques)
*/
        Tstar[0][2] = -(utk[0][2]-ltau[0]);
        Tstar[2][2] = -(ltau[0]+utk[2][2]);
/*
Compute fs0 (RHS ignoring constraints)
*/
        fs0[2] = (utau[2]-(Tstar[2][2]-(1.5*Fstar[2][1])));
        Fstark[1][0] = (Fstar[1][0]+((Fstar[2][0]*c2)-(Fstar[2][1]*s2)));
        Fstark[1][1] = (Fstar[1][1]+((Fstar[2][0]*s2)+(Fstar[2][1]*c2)));
        Fstark[1][2] = -(ufk[1][2]+ufk[2][2]);
        tvec1[0] = -(utk[2][0]+(ufk[2][2]*c2));
        tvec1[1] = ((rik[2][0]*ufk[2][2])-utk[2][1]);
        tvec1[2] = (Tstar[2][2]+((Fstar[2][1]*rik[2][0])-(Fstar[2][0]*c2)));
        Tstark[1][0] = (((tvec1[0]*c2)-(tvec1[1]*s2))-utk[1][0]);
        Tstark[1][1] = (((tvec1[0]*s2)+(tvec1[1]*c2))-utk[1][1]);
        Tstark[1][2] = (tvec1[2]-utk[1][2]);
        fs0[1] = (utau[1]-(Tstark[1][2]-Fstark[1][0]));
        fs0[0] = (utau[0]-(Tstar[0][2]-(.5*Fstar[0][0])));
        fs0flg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   36 adds/subtracts/negates
                     21 multiplies
                      0 divides
                     20 assignments
*/
}

void sddomm(int routine)
{
    int dumroutine,errnum;

    if (mmflg == 0) {
/*
Compute gains (DD, G)
*/
        DD[2] = .102564102564103;
        N21[2][0][2] = (2.-(1.5*s2));
        P11[1][0][0] = (2.92307692307692+(2.07692307692308*(c2*c2)));
        P11[1][0][1] = (2.07692307692308*(s2*c2));
        P11[1][1][0] = P11[1][0][1];
        P11[1][1][1] = (2.92307692307692+(2.07692307692308*(s2*s2)));
        Pd[1][0][2] = -(3.84615384615385+(4.15384615384615*(c2*c2)));
        Pd[1][1][2] = -(4.15384615384615*(s2*c2));
        Pd[1][2][0] = (2.+(3.*N21[2][0][2]));
        Pd[1][2][1] = (4.5*c2);
        P22[1][0][0] = (4.+(3.*((N21[2][0][2]*N21[2][0][2])+(s2*s2))));
        P22[1][0][1] = ((4.5*(N21[2][0][2]*c2))-(3.*(s2*c2)));
        P22[1][1][0] = P22[1][0][1];
        P22[1][1][1] = (9.75*(c2*c2));
        P22[1][2][2] = (7.69230769230769+(8.30769230769231*(c2*c2)));
        PH1[1][0] = Pd[1][0][2];
        PH1[1][1] = Pd[1][1][2];
        PH2[1][2] = P22[1][2][2];
        if (PH2[1][2] < 1e-13) {
            sdseterr(routine,47);
        } else {
            DD[1] = (1./PH2[1][2]);
        }
        DD[0] = .8;
        sderror(&dumroutine,&errnum);
        if (errnum == 0) {
            mmflg = 1;
        }
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   11 adds/subtracts/negates
                     24 multiplies
                      1 divides
                     20 assignments
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

void sdmfrc(double imult[5])
{
/*
Calculate forces due to constraint multipliers.

*/

    lfci[0][0] = imult[2];
    lfci[0][1] = imult[3];
    lfci[0][2] = imult[4];
    lfc[0][0] = -((Cio[0][0][0]*lfci[0][0])+(Cio[0][1][0]*lfci[0][1]));
    lfc[0][1] = -((Cio[0][0][1]*lfci[0][0])+(Cio[0][1][1]*lfci[0][1]));
    lfc[0][2] = -lfci[0][2];
    ltci[0][0] = imult[0];
    ltci[0][1] = imult[1];
    ltc[0][0] = -((Cio[0][0][0]*ltci[0][0])+(Cio[0][1][0]*ltci[0][1]));
    ltc[0][1] = -((Cio[0][0][1]*ltci[0][0])+(Cio[0][1][1]*ltci[0][1]));
    Tinb[0][0] = (ltci[0][0]+(lfci[0][2]*vt10c[0][1]));
    Tinb[0][1] = (ltci[0][1]-(lfci[0][2]*vt10c[0][0]));
    Tinb[0][2] = ((lfci[0][1]*vt10c[0][0])-(lfci[0][0]*vt10c[0][1]));
    Toutb[0][1] = (ltc[0][1]+(1.5*lfc[0][2]));
    mfk[0][0] = lfci[0][0];
    mfk[0][1] = lfci[0][1];
    mfk[0][2] = lfci[0][2];
    mfk[2][0] = lfc[0][0];
    mfk[2][1] = lfc[0][1];
    mfk[2][2] = lfc[0][2];
    mtk[0][0] = Tinb[0][0];
    mtk[0][1] = Tinb[0][1];
    mtk[0][2] = Tinb[0][2];
    mtk[2][0] = ltc[0][0];
    mtk[2][1] = Toutb[0][1];
    mtk[2][2] = -(1.5*lfc[0][1]);
    Toutb[0][0] = ltc[0][0];
    Toutb[0][2] = -(1.5*lfc[0][1]);
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   15 adds/subtracts/negates
                     15 multiplies
                      0 divides
                     28 assignments
*/
}

void sdequivht(double tau[3])
{
/* Compute tree hinge torques to match effect of applied loads
*/
    double fstareq[3][3],tstareq[3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(56,23);
        return;
    }
/*
Compute forces due to loop-joint hinge torques
*/
    sddoltau();
/*
Compute fstareq (forces)
*/
    fstareq[0][0] = ((9.8*s0)-ufk[0][0]);
    fstareq[0][1] = ((9.8*c0)-ufk[0][1]);
    fstareq[1][0] = ((19.6*s1)-ufk[1][0]);
    fstareq[1][1] = ((19.6*c1)-ufk[1][1]);
    fstareq[2][0] = -(ufk[2][0]+(3.*gk[2][0]));
    fstareq[2][1] = -(ufk[2][1]+(3.*gk[2][1]));
/*
Compute tstareq (torques)
*/
    tstareq[0][2] = -(utk[0][2]-ltau[0]);
    tstareq[2][2] = -(ltau[0]+utk[2][2]);
/*
Compute taus (RHS ignoring constraints and inertial forces)
*/
    tau[2] = (utau[2]-(tstareq[2][2]-(1.5*fstareq[2][1])));
    Fstark[1][0] = (fstareq[1][0]+((fstareq[2][0]*c2)-(fstareq[2][1]*s2)));
    Fstark[1][1] = (fstareq[1][1]+((fstareq[2][0]*s2)+(fstareq[2][1]*c2)));
    Fstark[1][2] = -(ufk[1][2]+ufk[2][2]);
    tvec1[0] = -(utk[2][0]+(ufk[2][2]*c2));
    tvec1[1] = ((rik[2][0]*ufk[2][2])-utk[2][1]);
    tvec1[2] = (tstareq[2][2]+((fstareq[2][1]*rik[2][0])-(fstareq[2][0]*c2)));
    Tstark[1][0] = (((tvec1[0]*c2)-(tvec1[1]*s2))-utk[1][0]);
    Tstark[1][1] = (((tvec1[0]*s2)+(tvec1[1]*c2))-utk[1][1]);
    Tstark[1][2] = (tvec1[2]-utk[1][2]);
    tau[1] = (utau[1]-(Tstark[1][2]-Fstark[1][0]));
    tau[0] = (utau[0]-(tstareq[0][2]-(.5*fstareq[0][0])));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   34 adds/subtracts/negates
                     20 multiplies
                      0 divides
                     20 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    0 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      3 assignments
*/
}

void sdfsmult(void)
{

/*
Compute Fs (multiplier-generated forces only)
*/
    fs[2] = -((1.5*mfk[2][1])-mtk[2][2]);
    Fstark[1][0] = ((mfk[2][1]*s2)-(mfk[2][0]*c2));
    Fstark[1][1] = -((mfk[2][0]*s2)+(mfk[2][1]*c2));
    Fstark[1][2] = -mfk[2][2];
    tvec1[0] = -(mtk[2][0]+(mfk[2][2]*c2));
    tvec1[1] = ((mfk[2][2]*rik[2][0])-mtk[2][1]);
    tvec1[2] = (((mfk[2][0]*c2)-(mfk[2][1]*rik[2][0]))-mtk[2][2]);
    Tstark[1][0] = ((tvec1[0]*c2)-(tvec1[1]*s2));
    Tstark[1][1] = ((tvec1[0]*s2)+(tvec1[1]*c2));
    Tstark[1][2] = tvec1[2];
    fs[1] = -(Tstark[1][2]-Fstark[1][0]);
    fs[0] = -((.5*mfk[0][0])-mtk[0][2]);
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   17 adds/subtracts/negates
                     14 multiplies
                      0 divides
                     12 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    3 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      3 assignments
*/
}

void sdfsgenmult(void)
{

/*
Compute Fs (generic multiplier-generated forces)
*/
    fs[2] = (mtau[2]-((1.5*mfk[2][1])-mtk[2][2]));
    Fstark[1][0] = (((mfk[2][1]*s2)-(mfk[2][0]*c2))-mfk[1][0]);
    Fstark[1][1] = -(mfk[1][1]+((mfk[2][0]*s2)+(mfk[2][1]*c2)));
    Fstark[1][2] = -(mfk[1][2]+mfk[2][2]);
    tvec1[0] = -(mtk[2][0]+(mfk[2][2]*c2));
    tvec1[1] = ((mfk[2][2]*rik[2][0])-mtk[2][1]);
    tvec1[2] = (((mfk[2][0]*c2)-(mfk[2][1]*rik[2][0]))-mtk[2][2]);
    Tstark[1][0] = (((tvec1[0]*c2)-(tvec1[1]*s2))-mtk[1][0]);
    Tstark[1][1] = (((tvec1[0]*s2)+(tvec1[1]*c2))-mtk[1][1]);
    Tstark[1][2] = (tvec1[2]-mtk[1][2]);
    fs[1] = (mtau[1]-(Tstark[1][2]-Fstark[1][0]));
    fs[0] = (mtau[0]-((.5*mfk[0][0])-mtk[0][2]));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   23 adds/subtracts/negates
                     14 multiplies
                      0 divides
                     12 assignments
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
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    3 adds/subtracts/negates
                      0 multiplies
                      0 divides
                      3 assignments
*/
}

void sdfulltrq(double udotin[3],
    double multin[5],
    double trqout[3])
{
/* Compute hinge torques which would produce indicated udots
*/
    double fstarr[3][3],tstarr[3][3],Otkr[3][3],Atir[3][3],Atkr[3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(61,23);
        return;
    }
/*
Compute forces due to loop-joint hinge torques
*/
    sddoltau();
/*
Compute multiplier-generated forces
*/
    sdmfrc(multin);
/*
Account for inertial accelerations and supplied udots
*/
    Otkr[2][2] = (udotin[1]+udotin[2]);
    Atkr[0][1] = -(.5*(u[0]*u[0]));
    Atkr[1][1] = -(u[1]*u[1]);
    Atir[2][1] = (Atkr[1][1]-(u[1]*u[1]));
    Atkr[2][0] = ((1.5*(wk[2][2]*wk[2][2]))+((Atir[2][1]*s2)-(2.*(udotin[1]*c2))
      ));
    Atkr[2][1] = (((2.*(udotin[1]*s2))+(Atir[2][1]*c2))-(1.5*Otkr[2][2]));
/*
Accumulate all forces and torques
*/
    fstarr[0][0] = ((mfk[0][0]+ufk[0][0])+((.5*udotin[0])-(9.8*s0)));
    fstarr[0][1] = ((mfk[0][1]+ufk[0][1])-(Atkr[0][1]+(9.8*c0)));
    fstarr[0][2] = (mfk[0][2]+ufk[0][2]);
    fstarr[1][0] = (ufk[1][0]+(2.*(udotin[1]-(9.8*s1))));
    fstarr[1][1] = (ufk[1][1]-(2.*(Atkr[1][1]+(9.8*c1))));
    fstarr[2][0] = ((3.*(gk[2][0]-Atkr[2][0]))+(mfk[2][0]+ufk[2][0]));
    fstarr[2][1] = ((3.*(gk[2][1]-Atkr[2][1]))+(mfk[2][1]+ufk[2][1]));
    fstarr[2][2] = (mfk[2][2]+ufk[2][2]);
    tstarr[0][0] = (mtk[0][0]+utk[0][0]);
    tstarr[0][1] = (mtk[0][1]+utk[0][1]);
    tstarr[0][2] = ((utk[0][2]+(mtk[0][2]-ltau[0]))-udotin[0]);
    tstarr[1][2] = (utk[1][2]-(2.*udotin[1]));
    tstarr[2][0] = (mtk[2][0]+utk[2][0]);
    tstarr[2][1] = (mtk[2][1]+utk[2][1]);
    tstarr[2][2] = ((utk[2][2]+(ltau[0]+mtk[2][2]))-(3.*Otkr[2][2]));
/*
Now calculate the torques
*/
    trqout[2] = -(utau[2]+(tstarr[2][2]-(1.5*fstarr[2][1])));
    Fstark[1][0] = (fstarr[1][0]+((fstarr[2][0]*c2)-(fstarr[2][1]*s2)));
    Fstark[1][1] = (fstarr[1][1]+((fstarr[2][0]*s2)+(fstarr[2][1]*c2)));
    Fstark[1][2] = (fstarr[2][2]+ufk[1][2]);
    tvec1[0] = (tstarr[2][0]+(fstarr[2][2]*c2));
    tvec1[1] = (tstarr[2][1]-(fstarr[2][2]*rik[2][0]));
    tvec1[2] = (tstarr[2][2]+((fstarr[2][1]*rik[2][0])-(fstarr[2][0]*c2)));
    Tstark[1][0] = (utk[1][0]+((tvec1[0]*c2)-(tvec1[1]*s2)));
    Tstark[1][1] = (utk[1][1]+((tvec1[0]*s2)+(tvec1[1]*c2)));
    Tstark[1][2] = (tstarr[1][2]+tvec1[2]);
    trqout[1] = -(utau[1]+(Tstark[1][2]-Fstark[1][0]));
    trqout[0] = -(utau[0]+(tstarr[0][2]-(.5*fstarr[0][0])));
/*
Op counts below do not include called subroutines
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   60 adds/subtracts/negates
                     38 multiplies
                      0 divides
                     33 assignments
*/
}

void sdcomptrq(double udotin[3],
    double trqout[3])
{
/* Compute hinge torques to produce these udots, ignoring constraints
*/
    int i;
    double multin[5];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(60,23);
        return;
    }
    for (i = 0; i < 5; i++) {
        multin[i] = 0.;
    }
    sdfulltrq(udotin,multin,trqout);
}

void sdmulttrq(double multin[5],
    double trqout[3])
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
    for (i = 0; i < 3; i++) {
        trqout[i] = fs[i];
    }
}

void sdrhs(void)
{
/*
Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)
*/

/*
Compute hinge torques for tree hinges
*/
    tauc[0] = utau[0];
    tauc[1] = utau[1];
    tauc[2] = utau[2];
/*
Compute hinge torques for loop joints
*/
    ltauc[0] = ltau[0];
    sddoiner();
/*
Compute onk & onb (angular accels in N)
*/
    Onkb[2][2] = (udot[1]+udot[2]);
    onb[0][0] = 0.;
    onb[0][1] = 0.;
    onb[0][2] = udot[0];
    onb[1][0] = 0.;
    onb[1][1] = 0.;
    onb[1][2] = udot[1];
    onb[2][0] = 0.;
    onb[2][1] = 0.;
    onb[2][2] = Onkb[2][2];
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
    dyad[1][0][1] = -udot[1];
    dyad[1][0][2] = 0.;
    dyad[1][1][0] = udot[1];
    dyad[1][1][1] = -w2w2[1];
    dyad[1][1][2] = 0.;
    dyad[1][2][0] = 0.;
    dyad[1][2][1] = 0.;
    dyad[1][2][2] = 0.;
    dyad[2][0][0] = -w2w2[2];
    dyad[2][0][1] = -Onkb[2][2];
    dyad[2][0][2] = 0.;
    dyad[2][1][0] = Onkb[2][2];
    dyad[2][1][1] = -w2w2[2];
    dyad[2][1][2] = 0.;
    dyad[2][2][0] = 0.;
    dyad[2][2][1] = 0.;
    dyad[2][2][2] = 0.;
/*
Compute ank & anb (mass center linear accels in N)
*/
    Ankb[2][0] = -(2.*(udot[1]*c2));
    Ankb[2][1] = ((2.*(udot[1]*s2))-(1.5*Onkb[2][2]));
    ank[0][0] = -((.5*(udot[0]*c0))+(Atk[0][1]*s0));
    ank[0][1] = ((Atk[0][1]*c0)-(.5*(udot[0]*s0)));
    ank[1][0] = -((Atk[1][1]*s1)+(udot[1]*c1));
    ank[1][1] = ((Atk[1][1]*c1)-(udot[1]*s1));
    AnkAtk[2][0] = (Ankb[2][0]+Atk[2][0]);
    AnkAtk[2][1] = (Ankb[2][1]+Atk[2][1]);
    ank[2][0] = ((AnkAtk[2][0]*cnk[2][0][0])+(AnkAtk[2][1]*cnk[2][0][1]));
    ank[2][1] = ((AnkAtk[2][0]*cnk[2][1][0])+(AnkAtk[2][1]*cnk[2][1][1]));
    anb[0][0] = ank[0][0];
    anb[0][1] = ank[0][1];
    anb[0][2] = 0.;
    anb[1][0] = ank[1][0];
    anb[1][1] = ank[1][1];
    anb[1][2] = 0.;
    anb[2][0] = ank[2][0];
    anb[2][1] = ank[2][1];
    anb[2][2] = 0.;
/*
Compute loop joint temps
*/
    vt17[0][0] = (ank[0][0]-ank[2][0]);
    vt17[0][1] = (ank[0][1]-ank[2][1]);
    vt19[0][0] = (((vt17[0][0]*c0)+(vt17[0][1]*s0))-((.5*udot[0])+(1.5*((
      Cio[0][0][0]*w2w2[2])-(Cio[0][0][1]*Onkb[2][2])))));
    vt19[0][1] = (((vt17[0][1]*c0)-(vt17[0][0]*s0))-((.5*w2w2[0])+(1.5*((
      Cio[0][1][0]*w2w2[2])-(Cio[0][1][1]*Onkb[2][2])))));
    vt24[0][2] = (Onkb[2][2]-udot[0]);
/*
Compute loop joint accelerations
*/
/*
Compute constraint acceleration errors
*/
    roustate = 3;
    aerr[2] = ((vt19[0][0]+(2.*(u[0]*vt12[0][1])))+((udot[0]*vt11[0][1])+(
      vt10c[0][0]*w2w2[0])));
    aerr[3] = ((vt19[0][1]-(2.*(u[0]*vt12[0][0])))+((udot[0]*vt10c[0][0])-(
      vt11[0][1]*w2w2[0])));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   39 adds/subtracts/negates
                     39 multiplies
                      0 divides
                     67 assignments
*/
}

void sdmassmat(double mmat[3][3])
{
/* Calculate the system mass matrix
*/
    int i,j;
    double udotin[3],mmrow[3],biastrq[3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(57,23);
        return;
    }
    for (i = 0; i < 3; i++) {
        udotin[i] = 0.;
    }
    sdcomptrq(udotin,biastrq);
    for (i = 0; i < 3; i++) {
        udotin[i] = 1.;
        sdcomptrq(udotin,mmrow);
        udotin[i] = 0.;
        for (j = i; j <= 2; j++) {
            mmat[i][j] = mmrow[j]-biastrq[j];
            mmat[j][i] = mmat[i][j];
        }
    }
/*
Check for singular mass matrix
*/
    for (i = 0; i < 3; i++) {
        if (mmat[i][i] <= 1e-13) {
            sdseterr(57,47);
        }
    }
}

void sdfrcmat(double fmat[3])
{
/* Return the system force matrix (RHS), excluding constraints
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(58,23);
        return;
    }
    sddofs0();
    for (i = 0; i < 3; i++) {
        fmat[i] = fs0[i];
    }
}

void sdpseudo(double lqout[1],
    double luout[1])
{
/*
Return pseudo-coordinates for loop joints.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(18,23);
        return;
    }
    lqout[0] = eul1[0];
    luout[0] = Woio[0][2];
}

void sdpsqdot(double lqdout[1])
{
/*
Return pseudo-coordinate derivatives for loop joints.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(28,23);
        return;
    }
    lqdout[0] = Woio[0][2];
}

void sdpsudot(double ludout[1])
{
/*
Return pseudo-coordinate accelerations for loop joints.

*/

    if (roustate != 3) {
        sdseterr(29,24);
        return;
    }
    ludout[0] = vt24[0][2];
}

void sdperr(double errs[5])
{
/*
Return position constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(26,23);
        return;
    }
    errs[0] = 0.;
    errs[1] = 0.;
    errs[2] = -vt10c[0][0];
    errs[3] = vt11[0][1];
    errs[4] = 0.;
}

void sdverr(double errs[5])
{
/*
Return velocity constraint errors.

*/

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(27,23);
        return;
    }
    errs[0] = 0.;
    errs[1] = 0.;
    errs[2] = verr[2];
    errs[3] = verr[3];
    errs[4] = 0.;
}

void sdaerr(double errs[5])
{
/*
Return acceleration constraint errors.

*/

    if (roustate != 3) {
        sdseterr(35,24);
        return;
    }
    errs[0] = 0.;
    errs[1] = 0.;
    errs[2] = aerr[2];
    errs[3] = aerr[3];
    errs[4] = 0.;
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

    if ((jnum < 0) || (jnum > 3)) {
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
    if (jnum > 2) {
        if (((pinno == 0) && ((jtype[jnum] == 9)))) {
            pinok = 1;
        }
        if ((pinno == 3) && ((jtype[jnum] == 1) || (jtype[jnum] == 5) || (jtype[
          jnum] == 7) || (jtype[jnum] == 9))) {
            pinok = 1;
        }
        if (((pinno == 4) || (pinno == 5)) && ((jtype[jnum] != 4) && (jtype[jnum
          ] != 6))) {
            pinok = 1;
        }
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

}

void sdpresvel(int joint,
    int axis,
    double prval)
{

}

void sdprespos(int joint,
    int axis,
    double prval)
{

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
    if (joint  >  2) {
        *torque = ltauc[sdindx(joint,axis)];
    } else {
        *torque = tauc[sdindx(joint,axis)];
    }
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
        if (joint > 2) {
            mltau[sdindx(joint,axis)] = mltau[sdindx(joint,axis)]+torque;
        } else {
            mtau[sdindx(joint,axis)] = mtau[sdindx(joint,axis)]+torque;
        }
    } else {
        fs0flg = 0;
        if (joint > 2) {
            ltau[sdindx(joint,axis)] = ltau[sdindx(joint,axis)]+torque;
            ltauflg = 0;
        } else {
            utau[sdindx(joint,axis)] = utau[sdindx(joint,axis)]+torque;
        }
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
    double pp[5][3],dpp[3][5];
    int i,j,c;
    double sum;
    double dfk[3][3],dtk[3][3],dtau[3],dltci[1][3],dltc[1][3],dlfci[1][3],dlfc[1
      ][3];
    double dTinb[1][3],dToutb[1][3],dltaufi[1][3],dltaufo[1][3],dltauti[1][3],
      dltauto[1][3];
    double deps[3],dZ1[3][3],dZ2[3][3];

    roustate = 2;
    if (wwflg == 0) {
/*
Compute constraint effects
*/
        sddomm(routine);
/*
Constraint 0 (loop constraint)
*/
        dltc[0][0] = -Cio[0][0][0];
        dltc[0][1] = -Cio[0][0][1];
        deps[2] = 0.;
        dZ2[1][0] = ((dltc[0][1]*s2)-(dltc[0][0]*c2));
        dZ2[1][1] = -((dltc[0][0]*s2)+(dltc[0][1]*c2));
        deps[1] = 0.;
        deps[0] = 0.;
/* ... redundant */
/*
Constraint 1 (loop constraint)
*/
        dltc[0][0] = -Cio[0][1][0];
        dltc[0][1] = -Cio[0][1][1];
        deps[2] = 0.;
        dZ2[1][0] = ((dltc[0][1]*s2)-(dltc[0][0]*c2));
        dZ2[1][1] = -((dltc[0][0]*s2)+(dltc[0][1]*c2));
        deps[1] = 0.;
        deps[0] = 0.;
/* ... redundant */
/*
Constraint 2 (loop constraint)
*/
        dlfc[0][0] = -Cio[0][0][0];
        dlfc[0][1] = -Cio[0][0][1];
        deps[2] = -(3.*dlfc[0][1]);
        dZ1[2][1] = -(dlfc[0][1]+(.461538461538462*deps[2]));
        dZ2[2][2] = (deps[2]+(3.*dlfc[0][1]));
        dZ1[1][0] = -((dlfc[0][0]*c2)+(dZ1[2][1]*s2));
        dZ1[1][1] = ((dZ1[2][1]*c2)-(dlfc[0][0]*s2));
        dZ2[1][2] = (dZ2[2][2]+(2.*((dlfc[0][0]*c2)+(dZ1[2][1]*s2))));
        deps[1] = -dZ2[1][2];
        deps[0] = -(.5+vt10c[0][1]);
        for (i = 0; i <= 2; i++) {
            pp[2][i] = deps[i];
            dpp[i][2] = DD[i]*deps[i];
        }
        wmap[0] = 2;
/*
Constraint 3 (loop constraint)
*/
        dlfc[0][0] = -Cio[0][1][0];
        dlfc[0][1] = -Cio[0][1][1];
        deps[2] = -(3.*dlfc[0][1]);
        dZ1[2][1] = -(dlfc[0][1]+(.461538461538462*deps[2]));
        dZ2[2][2] = (deps[2]+(3.*dlfc[0][1]));
        dZ1[1][0] = -((dlfc[0][0]*c2)+(dZ1[2][1]*s2));
        dZ1[1][1] = ((dZ1[2][1]*c2)-(dlfc[0][0]*s2));
        dZ2[1][2] = (dZ2[2][2]+(2.*((dlfc[0][0]*c2)+(dZ1[2][1]*s2))));
        deps[1] = -dZ2[1][2];
        deps[0] = vt10c[0][0];
        for (i = 0; i <= 2; i++) {
            pp[3][i] = deps[i];
            dpp[i][3] = DD[i]*deps[i];
        }
        wmap[1] = 3;
/*
Constraint 4 (loop constraint)
*/
        deps[2] = 0.;
        dZ2[1][0] = (2.-(3.*s2));
        dZ2[1][1] = (3.*c2);
        deps[1] = 0.;
        deps[0] = 0.;
/* ... redundant */
/*
Produce constraint coefficient matrix WW
*/
        for (c = 0; c <= 1; c++) {
            for (i = c; i <= 1; i++) {
                sum = 0.;
                for (j = 0; j <= 2; j++) {
                    sum = sum+pp[wmap[c]][j]*dpp[j][wmap[i]];
                }
                ww[wmap[c]][wmap[i]] = sum;
                ww[wmap[i]][wmap[c]] = sum;
            }
        }
/*
Form QR decomposition of WW
*/
        sdqrdcomp(5,5,2,2,wmap,wmap,ww,qraux,jpvt);
        wwflg = 1;
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   46 adds/subtracts/negates
                     45 multiplies
                      0 divides
                     69 assignments
*/
}

void sdxudot0(int routine,
    double oudot0[3])
{
/*
Compute unconstrained equations
*/
    int i;
    double eps[3],Z1[3][3],Z2[3][3],A1[3][3],A2[3][3],K1[3][3],K2[3][3];

    sdlhs(routine);
/*
Solve equations ignoring constraints
*/
    eps[2] = fs0[2];
    Z1[1][0] = (.461538461538462*(eps[2]*s2));
    Z1[1][1] = -(.461538461538462*(eps[2]*c2));
    Z2[1][2] = (eps[2]-(.923076923076923*(eps[2]*s2)));
    eps[1] = (fs0[1]-Z2[1][2]);
    eps[0] = fs0[0];
    udot[0] = (.8*eps[0]);
    udot[1] = (DD[1]*eps[1]);
    K1[2][0] = -(2.*(udot[1]*c2));
    K1[2][1] = (2.*(udot[1]*s2));
    udot[2] = ((.102564102564103*eps[2])-(udot[1]-(.461538461538462*K1[2][1])));
    A2[2][2] = (udot[1]+udot[2]);
    for (i = 0; i <= 2; i++) {
        oudot0[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    7 adds/subtracts/negates
                     14 multiplies
                      0 divides
                     15 assignments
*/
}

void sdudot0(double oudot0[3])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(66,23);
        return;
    }
    sdxudot0(66,oudot0);
}

void sdsetudot(double iudot[3])
{
/*
Assign udots and advance to stage Dynamics Ready
*/
    int i;

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(68,23);
        return;
    }
    for (i = 0; i <= 2; i++) {
        udot[i] = iudot[i];
    }
    sdrhs();
}

void sdxudotm(int routine,
    double imult[5],
    double oudotm[3])
{
/*
Compute udots due only to multipliers
*/
    int i;
    double eps[3],Z1[3][3],Z2[3][3],A1[3][3],A2[3][3],K1[3][3],K2[3][3];

    sdlhs(routine);
    sdmfrc(imult);
    eps[2] = (mtk[2][2]-(1.5*mfk[2][1]));
    Z1[2][1] = -(mfk[2][1]+(.461538461538462*eps[2]));
    Z2[2][1] = -(mtk[2][1]+(1.5*mfk[2][2]));
    Z2[2][2] = (eps[2]-(mtk[2][2]-(1.5*mfk[2][1])));
    Z1[1][0] = -((mfk[2][0]*c2)+(Z1[2][1]*s2));
    Z1[1][1] = ((Z1[2][1]*c2)-(mfk[2][0]*s2));
    Z1[1][2] = -mfk[2][2];
    Z2[1][0] = -((2.*mfk[2][2])+((mtk[2][0]*c2)+(Z2[2][1]*s2)));
    Z2[1][1] = ((Z2[2][1]*c2)-(mtk[2][0]*s2));
    Z2[1][2] = (Z2[2][2]+(2.*((mfk[2][0]*c2)+(Z1[2][1]*s2))));
    eps[1] = -Z2[1][2];
    eps[0] = (mtk[0][2]-(.5*mfk[0][0]));
    udot[0] = (.8*eps[0]);
    udot[1] = (DD[1]*eps[1]);
    K1[2][0] = -(2.*(udot[1]*c2));
    K1[2][1] = (2.*(udot[1]*s2));
    udot[2] = ((.102564102564103*eps[2])-(udot[1]-(.461538461538462*K1[2][1])));
    A2[2][2] = (udot[1]+udot[2]);
    for (i = 0; i <= 2; i++) {
        oudotm[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   23 adds/subtracts/negates
                     25 multiplies
                      0 divides
                     21 assignments
*/
}

void sdudotm(double imult[5],
    double oudotm[3])
{

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(67,23);
        return;
    }
    sdxudotm(67,imult,oudotm);
}

void sdderiv(double oqdot[3],
    double oudot[3])
{
/*
This is the derivative section for a 3-body ground-based
system with 3 hinge degree(s) of freedom.
There are 5 constraints.
*/
    double workr[5],bb[5],b0[5],v0[5],p0[5];
    int iwork[5];
    int i,j;
    double udot0[3],udot1[3];
    double eps[3],Z1[3][3],Z2[3][3],A1[3][3],A2[3][3],K1[3][3],K2[3][3];

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
    wsiz = 2;
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
    for (i = 0; i <= 4; i++) {
        bb[i] = -b0[i];
    }
    if (stabvel  !=  0.) {
        for (i = 0; i <= 4; i++) {
            bb[i] = bb[i]-stabvel*v0[i];
        }
    }
    if (stabpos  !=  0.) {
        for (i = 0; i <= 4; i++) {
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
    for (i = 0; i <= 4; i++) {
        mult[i] = 0.;
    }
    sdqrbslv(5,5,2,2,wmap,wmap,1e-13,workr,iwork,ww,qraux,jpvt,bb,mult,&wrank);
    for (i = 0; i <= 1; i++) {
        multmap[i] = 0;
    }
    for (i = 0; i < wrank; i++) {
        multmap[jpvt[i]] = 1;
    }
    j = 0;
    for (i = 0; i <= 1; i++) {
        if (multmap[i] != 0) {
            multmap[j] = wmap[i];
            j = j+1;
        }
    }
/*
Compute final udots
*/
    sdxudotm(17,mult,udot1);
    for (i = 0; i <= 2; i++) {
        udot[i] = udot0[i]+udot1[i];
    }
    sdrhs();
    for (i = 0; i <= 2; i++) {
        oqdot[i] = qdot[i];
    }
    for (i = 0; i <= 2; i++) {
        oudot[i] = udot[i];
    }
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   18 adds/subtracts/negates
                     10 multiplies
                      0 divides
                     29 assignments
*/
}
/*
Compute residuals for use with DAE integrator
*/

void sdresid(double eqdot[3],
    double eudot[3],
    double emults[5],
    double resid[11])
{
    int i;
    double uderrs[3],p0[5];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(16,23);
        return;
    }
    if (stabposq == 1) {
        sdseterr(16,33);
    }
    sdfulltrq(eudot,emults,uderrs);
    for (i = 0; i < 3; i++) {
        resid[i] = eqdot[i]-qdot[i];
    }
    for (i = 0; i < 3; i++) {
        resid[3+i] = uderrs[i];
    }
    sdverr(&resid[6]);
    if (stabpos  !=  0.) {
        sdperr(p0);
        for (i = 0; i < 5; i++) {
            resid[6+i] = resid[6+i]+stabpos*p0[i];
        }
    }
    for (i = 0; i < 3; i++) {
        udot[i] = eudot[i];
    }
    for (i = 0; i < 5; i++) {
        mult[i] = emults[i];
    }
    sdrhs();
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain    8 adds/subtracts/negates
                      5 multiplies
                      0 divides
                     19 assignments
*/
}

void sdmult(double omults[5],
    int *owrank,
    int omultmap[5])
{
    int i;

    if (roustate != 3) {
        sdseterr(34,24);
        return;
    }
    for (i = 0; i < 5; i++) {
        omults[i] = mult[i];
        if (i <= wrank-1) {
            omultmap[i] = multmap[i];
        } else {
            omultmap[i] = -1;
        }
    }
    *owrank = wrank;
}

void sdreac(double force[4][3],
    double torque[4][3])
{
/*
Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)
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
    ffkb[2][0] = (mfk[2][0]+ufk[2][0]);
    ffkb[2][1] = (mfk[2][1]+ufk[2][1]);
    ffkb[2][2] = (mfk[2][2]+ufk[2][2]);
    ttkb[0][0] = (mtk[0][0]+utk[0][0]);
    ttkb[0][1] = (mtk[0][1]+utk[0][1]);
    ttkb[0][2] = (utk[0][2]+(mtk[0][2]-ltau[0]));
    ttkb[2][0] = (mtk[2][0]+utk[2][0]);
    ttkb[2][1] = (mtk[2][1]+utk[2][1]);
    ttkb[2][2] = (utk[2][2]+(ltau[0]+mtk[2][2]));
    fc[2][0] = ((3.*(AnkAtk[2][0]-gk[2][0]))-ffkb[2][0]);
    fc[2][1] = ((3.*(AnkAtk[2][1]-gk[2][1]))-ffkb[2][1]);
    fc[2][2] = -ffkb[2][2];
    tc[2][0] = -ttkb[2][0];
    tc[2][1] = -(ttkb[2][1]-(1.5*fc[2][2]));
    tc[2][2] = ((3.*Onkb[2][2])-(ttkb[2][2]+(1.5*fc[2][1])));
    fccikt[2][0] = ((fc[2][0]*c2)-(fc[2][1]*s2));
    fccikt[2][1] = ((fc[2][0]*s2)+(fc[2][1]*c2));
    fccikt[2][2] = fc[2][2];
    ffk[1][0] = (ufk[1][0]-fccikt[2][0]);
    ffk[1][1] = (ufk[1][1]-fccikt[2][1]);
    ffk[1][2] = (ufk[1][2]-fccikt[2][2]);
    ttk[1][0] = (utk[1][0]-(fccikt[2][2]+((tc[2][0]*c2)-(tc[2][1]*s2))));
    ttk[1][1] = (utk[1][1]-((tc[2][0]*s2)+(tc[2][1]*c2)));
    ttk[1][2] = (utk[1][2]-(tc[2][2]-fccikt[2][0]));
    fc[1][0] = ((2.*((9.8*s1)-udot[1]))-ffk[1][0]);
    fc[1][1] = ((2.*(Atk[1][1]+(9.8*c1)))-ffk[1][1]);
    fc[1][2] = -ffk[1][2];
    tc[1][0] = -(ttk[1][0]-fc[1][2]);
    tc[1][1] = -ttk[1][1];
    tc[1][2] = ((2.*udot[1])-(fc[1][0]+ttk[1][2]));
    fc[0][0] = (((9.8*s0)-(.5*udot[0]))-ffkb[0][0]);
    fc[0][1] = ((Atk[0][1]+(9.8*c0))-ffkb[0][1]);
    fc[0][2] = -ffkb[0][2];
    tc[0][0] = -(ttkb[0][0]-(.5*fc[0][2]));
    tc[0][1] = -ttkb[0][1];
    tc[0][2] = (udot[0]-(ttkb[0][2]+(.5*fc[0][0])));
    force[0][0] = fc[0][0];
    torque[0][0] = tc[0][0];
    force[0][1] = fc[0][1];
    torque[0][1] = tc[0][1];
    force[0][2] = fc[0][2];
    torque[0][2] = tc[0][2];
    force[1][0] = fc[1][0];
    torque[1][0] = tc[1][0];
    force[1][1] = fc[1][1];
    torque[1][1] = tc[1][1];
    force[1][2] = fc[1][2];
    torque[1][2] = tc[1][2];
    force[2][0] = fc[2][0];
    torque[2][0] = tc[2][0];
    force[2][1] = fc[2][1];
    torque[2][1] = tc[2][1];
    force[2][2] = fc[2][2];
    torque[2][2] = tc[2][2];
/*
Compute reaction forces for loop joints
*/
    force[3][0] = lfc[0][0];
    torque[3][0] = ltc[0][0];
    force[3][1] = lfc[0][1];
    torque[3][1] = ltc[0][1];
    force[3][2] = lfc[0][2];
    torque[3][2] = ltau[0];
/*
Compute reaction forces for tree weld joints
*/
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   56 adds/subtracts/negates
                     23 multiplies
                      0 divides
                     63 assignments
*/
}

void sdmom(double lm[3],
    double am[3],
    double *ke)
{
/*
Compute system linear and angular momentum, and kinetic energy.

Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)
*/
    double lk[3][3],hnk[3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(19,23);
        return;
    }
    lm[0] = ((3.*vnk[2][0])+(vnk[0][0]+(2.*vnk[1][0])));
    lm[1] = ((3.*vnk[2][1])+(vnk[0][1]+(2.*vnk[1][1])));
    lm[2] = 0.;
    am[0] = 0.;
    am[1] = 0.;
    am[2] = ((3.*(wk[2][2]+((vnk[2][1]*(rnk[2][0]-com[0]))-(vnk[2][0]*(rnk[2][1]
      -com[1])))))+((2.*(u[1]+((vnk[1][1]*(rnk[1][0]-com[0]))-(vnk[1][0]*(c1-
      com[1])))))+(u[0]-((vnk[0][0]*((.5*c0)-com[1]))+(vnk[0][1]*(com[0]+(.5*s0)
      ))))));
    *ke = (.5*((3.*((wk[2][2]*wk[2][2])+((vnk[2][0]*vnk[2][0])+(vnk[2][1]*
      vnk[2][1]))))+((2.*((u[1]*u[1])+((vnk[1][0]*vnk[1][0])+(vnk[1][1]*
      vnk[1][1]))))+((u[0]*u[0])+((vnk[0][0]*vnk[0][0])+(vnk[0][1]*vnk[0][1]))))
      ));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   26 adds/subtracts/negates
                     26 multiplies
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

Generated TIMESTAMP by SD/FAST, Order(N) formulation
(sdfast 2.0 #10001)
*/
    double ikcnkt[3][3][3];

    if ((roustate != 2) && (roustate != 3)) {
        sdseterr(20,23);
        return;
    }
    *mtoto = 6.;
    cm[0] = com[0];
    cm[1] = com[1];
    cm[2] = 0.;
    icm[0][0] = (((3.*((cnk[2][0][1]*cnk[2][0][1])+(rnk[2][1]*rnk[2][1])))+((
      1.25*(c0*c0))+(4.*(c1*c1))))-(6.*(com[1]*com[1])));
    icm[0][1] = ((6.*(com[0]*com[1]))+((3.*((cnk[2][0][1]*cnk[2][1][1])-(
      rnk[2][0]*rnk[2][1])))+((1.25*(s0*c0))+(2.*((s1*c1)-(rnk[1][0]*c1))))));
    icm[0][2] = 0.;
    icm[1][0] = icm[0][1];
    icm[1][1] = (((3.*((cnk[2][1][1]*cnk[2][1][1])+(rnk[2][0]*rnk[2][0])))+((
      1.25*(s0*s0))+(2.*((rnk[1][0]*rnk[1][0])+(s1*s1)))))-(6.*(com[0]*com[0])))
      ;
    icm[1][2] = 0.;
    icm[2][0] = icm[0][2];
    icm[2][1] = icm[1][2];
    icm[2][2] = (6.25+(((2.*((rnk[1][0]*rnk[1][0])+(c1*c1)))+(3.*((rnk[2][0]*
      rnk[2][0])+(rnk[2][1]*rnk[2][1]))))-(6.*((com[0]*com[0])+(com[1]*com[1])))
      ));
/*
 Used TIMESTAMP seconds CPU time,
 TIMESTAMP additional bytes of memory.
 Equations contain   20 adds/subtracts/negates
                     38 multiplies
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

    if ((coord < 0) || (coord > 2)) {
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
    if (joint  >  2) {
        if (lbtjq[joint-3][0]  !=  0) {
            lbtj[joint-3][0] = btjin[0];
            lbtjq[joint-3][0] = 3;
            anyques = 1;
        }
        if (lbtjq[joint-3][1]  !=  0) {
            lbtj[joint-3][1] = btjin[1];
            lbtjq[joint-3][1] = 3;
            anyques = 1;
        }
        if (lbtjq[joint-3][2]  !=  0) {
            lbtj[joint-3][2] = btjin[2];
            lbtjq[joint-3][2] = 3;
            anyques = 1;
        }
    } else {
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
    if (joint  >  2) {
        if (litjq[joint-3][0]  !=  0) {
            litj[joint-3][0] = itjin[0];
            litjq[joint-3][0] = 3;
            anyques = 1;
        }
        if (litjq[joint-3][1]  !=  0) {
            litj[joint-3][1] = itjin[1];
            litjq[joint-3][1] = 3;
            anyques = 1;
        }
        if (litjq[joint-3][2]  !=  0) {
            litj[joint-3][2] = itjin[2];
            litjq[joint-3][2] = 3;
            anyques = 1;
        }
    } else {
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
    int ljnum,anyques,offs;

    if (sdchkjpin(6,joint,pinno) != 0) {
        return;
    }
    anyques = 0;
    if (joint  >  2) {
        ljnum = joint-3;
        if (pinno  ==  0) {
            if (inbpin1q[ljnum][0]  !=  0) {
                inbpin1[ljnum][0] = pinin[0];
                inbpin1q[ljnum][0] = 3;
                anyques = 1;
            }
            if (inbpin1q[ljnum][1]  !=  0) {
                inbpin1[ljnum][1] = pinin[1];
                inbpin1q[ljnum][1] = 3;
                anyques = 1;
            }
            if (inbpin1q[ljnum][2]  !=  0) {
                inbpin1[ljnum][2] = pinin[2];
                inbpin1q[ljnum][2] = 3;
                anyques = 1;
            }
        }
        if (pinno  ==  1) {
            if (inbpin2q[ljnum][0]  !=  0) {
                inbpin2[ljnum][0] = pinin[0];
                inbpin2q[ljnum][0] = 3;
                anyques = 1;
            }
            if (inbpin2q[ljnum][1]  !=  0) {
                inbpin2[ljnum][1] = pinin[1];
                inbpin2q[ljnum][1] = 3;
                anyques = 1;
            }
            if (inbpin2q[ljnum][2]  !=  0) {
                inbpin2[ljnum][2] = pinin[2];
                inbpin2q[ljnum][2] = 3;
                anyques = 1;
            }
        }
        if (pinno  ==  2) {
            if (inbpin3q[ljnum][0]  !=  0) {
                inbpin3[ljnum][0] = pinin[0];
                inbpin3q[ljnum][0] = 3;
                anyques = 1;
            }
            if (inbpin3q[ljnum][1]  !=  0) {
                inbpin3[ljnum][1] = pinin[1];
                inbpin3q[ljnum][1] = 3;
                anyques = 1;
            }
            if (inbpin3q[ljnum][2]  !=  0) {
                inbpin3[ljnum][2] = pinin[2];
                inbpin3q[ljnum][2] = 3;
                anyques = 1;
            }
        }
        if (pinno  ==  3) {
            if (inbrefq[ljnum][0]  !=  0) {
                inbref[ljnum][0] = pinin[0];
                inbrefq[ljnum][0] = 3;
                anyques = 1;
            }
            if (inbrefq[ljnum][1]  !=  0) {
                inbref[ljnum][1] = pinin[1];
                inbrefq[ljnum][1] = 3;
                anyques = 1;
            }
            if (inbrefq[ljnum][2]  !=  0) {
                inbref[ljnum][2] = pinin[2];
                inbrefq[ljnum][2] = 3;
                anyques = 1;
            }
        }
        if (pinno  ==  4) {
            if (bodypinq[ljnum][0]  !=  0) {
                bodypin[ljnum][0] = pinin[0];
                bodypinq[ljnum][0] = 3;
                anyques = 1;
            }
            if (bodypinq[ljnum][1]  !=  0) {
                bodypin[ljnum][1] = pinin[1];
                bodypinq[ljnum][1] = 3;
                anyques = 1;
            }
            if (bodypinq[ljnum][2]  !=  0) {
                bodypin[ljnum][2] = pinin[2];
                bodypinq[ljnum][2] = 3;
                anyques = 1;
            }
        }
        if (pinno  ==  5) {
            if (bodyrefq[ljnum][0]  !=  0) {
                bodyref[ljnum][0] = pinin[0];
                bodyrefq[ljnum][0] = 3;
                anyques = 1;
            }
            if (bodyrefq[ljnum][1]  !=  0) {
                bodyref[ljnum][1] = pinin[1];
                bodyrefq[ljnum][1] = 3;
                anyques = 1;
            }
            if (bodyrefq[ljnum][2]  !=  0) {
                bodyref[ljnum][2] = pinin[2];
                bodyrefq[ljnum][2] = 3;
                anyques = 1;
            }
        }
    } else {
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
    if (joint  >  2) {
        if (lpresq[sdindx(joint,axis)]  !=  0) {
            if (presin  !=  0) {
                lpres[sdindx(joint,axis)] = 1.;
            } else {
                lpres[sdindx(joint,axis)] = 0.;
            }
            lpresq[sdindx(joint,axis)] = 3;
            anyques = 1;
        }
    } else {
        if (presq[sdindx(joint,axis)]  !=  0) {
            if (presin  !=  0) {
                pres[sdindx(joint,axis)] = 1.;
            } else {
                pres[sdindx(joint,axis)] = 0.;
            }
            presq[sdindx(joint,axis)] = 3;
            anyques = 1;
        }
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
    if (joint  >  2) {
        btjout[0] = lbtj[joint-3][0];
        btjout[1] = lbtj[joint-3][1];
        btjout[2] = lbtj[joint-3][2];
    } else {
        btjout[0] = rk[joint][0];
        btjout[1] = rk[joint][1];
        btjout[2] = rk[joint][2];
    }
}

void sdgetitj(int joint,
    double itjout[3])
{

    if (sdchkjnum(43,joint) != 0) {
        return;
    }
    if (joint  >  2) {
        itjout[0] = litj[joint-3][0];
        itjout[1] = litj[joint-3][1];
        itjout[2] = litj[joint-3][2];
    } else {
        itjout[0] = ri[joint][0];
        itjout[1] = ri[joint][1];
        itjout[2] = ri[joint][2];
    }
}

void sdgetpin(int joint,
    int pinno,
    double pinout[3])
{
    int ljnum,offs;

    if (sdchkjpin(44,joint,pinno) != 0) {
        return;
    }
    if (joint  >  2) {
        ljnum = joint-3;
        if (pinno  ==  0) {
            pinout[0] = inbpin1[ljnum][0];
            pinout[1] = inbpin1[ljnum][1];
            pinout[2] = inbpin1[ljnum][2];
        }
        if (pinno  ==  1) {
            pinout[0] = inbpin2[ljnum][0];
            pinout[1] = inbpin2[ljnum][1];
            pinout[2] = inbpin2[ljnum][2];
        }
        if (pinno  ==  2) {
            pinout[0] = inbpin3[ljnum][0];
            pinout[1] = inbpin3[ljnum][1];
            pinout[2] = inbpin3[ljnum][2];
        }
        if (pinno  ==  3) {
            pinout[0] = inbref[ljnum][0];
            pinout[1] = inbref[ljnum][1];
            pinout[2] = inbref[ljnum][2];
        }
        if (pinno  ==  4) {
            pinout[0] = bodypin[ljnum][0];
            pinout[1] = bodypin[ljnum][1];
            pinout[2] = bodypin[ljnum][2];
        }
        if (pinno  ==  5) {
            pinout[0] = bodyref[ljnum][0];
            pinout[1] = bodyref[ljnum][1];
            pinout[2] = bodyref[ljnum][2];
        }
    } else {
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
}

void sdgetpres(int joint,
    int axis,
    int *presout)
{

    if (sdchkjaxis(45,joint,axis) != 0) {
        return;
    }
    if (joint  >  2) {
        if (lpres[sdindx(joint,axis)]  !=  0.) {
            *presout = 1;
        } else {
            *presout = 0;
        }
    } else {
        if (pres[sdindx(joint,axis)]  !=  0.) {
            *presout = 1;
        } else {
            *presout = 0;
        }
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
    info[11] = 2;
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
    if (joint  <=  2) {
        info[1] = 0;
        offs = 0;
    } else {
        info[1] = 1;
        offs = 3;
    }
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
  sdstate:    54 adds    70 multiplies     0 divides   130 assignments
  sdderiv:   264 adds   263 multiplies    10 divides   397 assignments
  sdresid:   122 adds    97 multiplies     0 divides   147 assignments
  sdreac:     56 adds    23 multiplies     0 divides    63 assignments
  sdmom:      26 adds    26 multiplies     0 divides     7 assignments
  sdsys:      20 adds    38 multiplies     0 divides    13 assignments
*/
