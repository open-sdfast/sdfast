C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
C
C ROADMAP (slew.sd)
C
C Bodies        Inb
C No  Name      body Joint type  Coords q         Multipliers
C --- --------- ---- ----------- ---------------- -----------------------
C   0 $ground                                    |
C   1 bus         0  Sixdof        1   2   3     |
C                    ...           4   5   6  12 |
C   2 clock       1  Pin           7             |
C   3 camera      2  Pin           8             |
C   4 boom        1  U-joint       9  10         |
C   5 scanner     3  Pin          11p            |  1p
C
C
      BLOCK DATA SDBLKDATA
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C  Topological information
C
      DATA GROUND/0/
      DATA NBOD/5/
      DATA NDOF/11/
      DATA NCONS/1/
      DATA NLOOP/0/
      DATA NLDOF/0/
      DATA NLOOPC/0/
      DATA NBALL/1/
      DATA NLBALL/0/
      DATA NPRES/1/
      DATA NUSER/0/
      DATA JTYPE(1)/6/
      DATA JTYPE(2)/1/
      DATA JTYPE(3)/1/
      DATA JTYPE(4)/2/
      DATA JTYPE(5)/1/
      DATA INB(1)/0/
      DATA INB(2)/1/
      DATA INB(3)/2/
      DATA INB(4)/1/
      DATA INB(5)/3/
      DATA OUTB(1)/1/
      DATA OUTB(2)/2/
      DATA OUTB(3)/3/
      DATA OUTB(4)/4/
      DATA OUTB(5)/5/
      DATA NJNTDOF(1)/6/
      DATA NJNTDOF(2)/1/
      DATA NJNTDOF(3)/1/
      DATA NJNTDOF(4)/2/
      DATA NJNTDOF(5)/1/
      DATA NJNTC(1)/0/
      DATA NJNTC(2)/0/
      DATA NJNTC(3)/0/
      DATA NJNTC(4)/0/
      DATA NJNTC(5)/0/
      DATA NJNTP(1)/0/
      DATA NJNTP(2)/0/
      DATA NJNTP(3)/0/
      DATA NJNTP(4)/0/
      DATA NJNTP(5)/1/
      DATA FIRSTQ(1)/1/
      DATA FIRSTQ(2)/7/
      DATA FIRSTQ(3)/8/
      DATA FIRSTQ(4)/9/
      DATA FIRSTQ(5)/11/
      DATA BALLQ(1)/12/
      DATA BALLQ(2)/-103/
      DATA BALLQ(3)/-103/
      DATA BALLQ(4)/-103/
      DATA BALLQ(5)/-103/
      DATA FIRSTM(1)/0/
      DATA FIRSTM(2)/0/
      DATA FIRSTM(3)/0/
      DATA FIRSTM(4)/0/
      DATA FIRSTM(5)/0/
      DATA FIRSTP(1)/0/
      DATA FIRSTP(2)/0/
      DATA FIRSTP(3)/0/
      DATA FIRSTP(4)/0/
      DATA FIRSTP(5)/1/
      DATA TRANS(1)/1/
      DATA TRANS(2)/1/
      DATA TRANS(3)/1/
      DATA TRANS(4)/0/
      DATA TRANS(5)/0/
      DATA TRANS(6)/0/
      DATA TRANS(7)/0/
      DATA TRANS(8)/0/
      DATA TRANS(9)/0/
      DATA TRANS(10)/0/
      DATA TRANS(11)/0/
C Model parameters from the input file
C
C gravity
      DATA GRAV(1)/0D0/
      DATA GRAV(2)/0D0/
      DATA GRAV(3)/0D0/
C
C mass
      DATA MK(1)/410D0/
      DATA MK(2)/6.8D0/
      DATA MK(3)/57.5D0/
      DATA MK(4)/10.7D0/
      DATA MK(5)/1D0/
C
C inertia
      DATA IK(1,1,1)/115D0/
      DATA IK(1,1,2)/-14D0/
      DATA IK(1,1,3)/14D0/
      DATA IK(1,2,1)/-14D0/
      DATA IK(1,2,2)/316D0/
      DATA IK(1,2,3)/-34.6D0/
      DATA IK(1,3,1)/14D0/
      DATA IK(1,3,2)/-34.6D0/
      DATA IK(1,3,3)/440D0/
      DATA IK(2,1,1)/.35D0/
      DATA IK(2,1,2)/0D0/
      DATA IK(2,1,3)/0D0/
      DATA IK(2,2,1)/0D0/
      DATA IK(2,2,2)/.35D0/
      DATA IK(2,2,3)/0D0/
      DATA IK(2,3,1)/0D0/
      DATA IK(2,3,2)/0D0/
      DATA IK(2,3,3)/0D0/
      DATA IK(3,1,1)/4.85D0/
      DATA IK(3,1,2)/.41D0/
      DATA IK(3,1,3)/-.07D0/
      DATA IK(3,2,1)/.41D0/
      DATA IK(3,2,2)/2.2D0/
      DATA IK(3,2,3)/.54D0/
      DATA IK(3,3,1)/-.07D0/
      DATA IK(3,3,2)/.54D0/
      DATA IK(3,3,3)/5.5D0/
      DATA IK(4,1,1)/27.2D0/
      DATA IK(4,1,2)/0D0/
      DATA IK(4,1,3)/0D0/
      DATA IK(4,2,1)/0D0/
      DATA IK(4,2,2)/.2D0/
      DATA IK(4,2,3)/0D0/
      DATA IK(4,3,1)/0D0/
      DATA IK(4,3,2)/0D0/
      DATA IK(4,3,3)/27.2D0/
      DATA IK(5,1,1)/.1D0/
      DATA IK(5,1,2)/0D0/
      DATA IK(5,1,3)/0D0/
      DATA IK(5,2,1)/0D0/
      DATA IK(5,2,2)/.1D0/
      DATA IK(5,2,3)/0D0/
      DATA IK(5,3,1)/0D0/
      DATA IK(5,3,2)/0D0/
      DATA IK(5,3,3)/.5D0/
C
C tree hinge axis vectors
      DATA PIN(1,1)/1D0/
      DATA PIN(1,2)/0D0/
      DATA PIN(1,3)/0D0/
      DATA PIN(2,1)/0D0/
      DATA PIN(2,2)/1D0/
      DATA PIN(2,3)/0D0/
      DATA PIN(3,1)/0D0/
      DATA PIN(3,2)/0D0/
      DATA PIN(3,3)/1D0/
      DATA PIN(4,1)/0D0/
      DATA PIN(4,2)/0D0/
      DATA PIN(4,3)/0D0/
      DATA PIN(5,1)/0D0/
      DATA PIN(5,2)/0D0/
      DATA PIN(5,3)/0D0/
      DATA PIN(6,1)/0D0/
      DATA PIN(6,2)/0D0/
      DATA PIN(6,3)/0D0/
      DATA PIN(7,1)/0D0/
      DATA PIN(7,2)/0D0/
      DATA PIN(7,3)/1D0/
      DATA PIN(8,1)/-1D0/
      DATA PIN(8,2)/0D0/
      DATA PIN(8,3)/0D0/
      DATA PIN(9,1)/0D0/
      DATA PIN(9,2)/0D0/
      DATA PIN(9,3)/1D0/
      DATA PIN(10,1)/1D0/
      DATA PIN(10,2)/0D0/
      DATA PIN(10,3)/0D0/
      DATA PIN(11,1)/0D0/
      DATA PIN(11,2)/0D0/
      DATA PIN(11,3)/1D0/
C
C tree bodytojoint vectors
      DATA RK(1,1)/0D0/
      DATA RK(1,2)/0D0/
      DATA RK(1,3)/0D0/
      DATA RK(2,1)/0D0/
      DATA RK(2,2)/0D0/
      DATA RK(2,3)/-.75D0/
      DATA RK(3,1)/0D0/
      DATA RK(3,2)/-.22D0/
      DATA RK(3,3)/.2D0/
      DATA RK(4,1)/0D0/
      DATA RK(4,2)/3.3D0/
      DATA RK(4,3)/0D0/
      DATA RK(5,1)/0D0/
      DATA RK(5,2)/.01D0/
      DATA RK(5,3)/0D0/
C
C tree inbtojoint vectors
      DATA RI(1,1)/0D0/
      DATA RI(1,2)/0D0/
      DATA RI(1,3)/0D0/
      DATA RI(2,1)/0D0/
      DATA RI(2,2)/0D0/
      DATA RI(2,3)/-1.5D0/
      DATA RI(3,1)/0D0/
      DATA RI(3,2)/-.1D0/
      DATA RI(3,3)/-.75D0/
      DATA RI(4,1)/0D0/
      DATA RI(4,2)/-1.2D0/
      DATA RI(4,3)/0D0/
      DATA RI(5,1)/0D0/
      DATA RI(5,2)/0D0/
      DATA RI(5,3)/0D0/
C
C tree prescribed motion
      DATA PRES(1)/0D0/
      DATA PRES(2)/0D0/
      DATA PRES(3)/0D0/
      DATA PRES(4)/0D0/
      DATA PRES(5)/0D0/
      DATA PRES(6)/0D0/
      DATA PRES(7)/0D0/
      DATA PRES(8)/0D0/
      DATA PRES(9)/0D0/
      DATA PRES(10)/0D0/
      DATA PRES(11)/1D0/
C
C stabilization parameters
      DATA STABVEL/0D0/
      DATA STABPOS/0D0/
C
C miscellaneous
      DATA MFRCFLG/0/
      DATA ROUSTATE/0/
      DATA VPKFLG/0/
      DATA INERFLG/0/
      DATA MMFLG/0/
      DATA MMLDUFLG/0/
      DATA WWFLG/0/
      DATA LTAUFLG/0/
      DATA FS0FLG/0/
      DATA II/0/
      DATA MMAP(1)/1/
      DATA MMAP(2)/2/
      DATA MMAP(3)/3/
      DATA MMAP(4)/4/
      DATA MMAP(5)/5/
      DATA MMAP(6)/6/
      DATA MMAP(7)/7/
      DATA MMAP(8)/8/
      DATA MMAP(9)/9/
      DATA MMAP(10)/10/
      DATA MMAP(11)/11/
C
C Which parameters were "?" (1) or "<nominal>?" (3)
      DATA GRAVQ(1)/0/
      DATA GRAVQ(2)/0/
      DATA GRAVQ(3)/0/
      DATA MKQ(1)/0/
      DATA MKQ(2)/0/
      DATA MKQ(3)/0/
      DATA MKQ(4)/0/
      DATA MKQ(5)/0/
      DATA IKQ(1,1,1)/0/
      DATA IKQ(1,1,2)/0/
      DATA IKQ(1,1,3)/0/
      DATA IKQ(1,2,1)/0/
      DATA IKQ(1,2,2)/0/
      DATA IKQ(1,2,3)/0/
      DATA IKQ(1,3,1)/0/
      DATA IKQ(1,3,2)/0/
      DATA IKQ(1,3,3)/0/
      DATA IKQ(2,1,1)/0/
      DATA IKQ(2,1,2)/0/
      DATA IKQ(2,1,3)/0/
      DATA IKQ(2,2,1)/0/
      DATA IKQ(2,2,2)/0/
      DATA IKQ(2,2,3)/0/
      DATA IKQ(2,3,1)/0/
      DATA IKQ(2,3,2)/0/
      DATA IKQ(2,3,3)/0/
      DATA IKQ(3,1,1)/0/
      DATA IKQ(3,1,2)/0/
      DATA IKQ(3,1,3)/0/
      DATA IKQ(3,2,1)/0/
      DATA IKQ(3,2,2)/0/
      DATA IKQ(3,2,3)/0/
      DATA IKQ(3,3,1)/0/
      DATA IKQ(3,3,2)/0/
      DATA IKQ(3,3,3)/0/
      DATA IKQ(4,1,1)/0/
      DATA IKQ(4,1,2)/0/
      DATA IKQ(4,1,3)/0/
      DATA IKQ(4,2,1)/0/
      DATA IKQ(4,2,2)/0/
      DATA IKQ(4,2,3)/0/
      DATA IKQ(4,3,1)/0/
      DATA IKQ(4,3,2)/0/
      DATA IKQ(4,3,3)/0/
      DATA IKQ(5,1,1)/0/
      DATA IKQ(5,1,2)/0/
      DATA IKQ(5,1,3)/0/
      DATA IKQ(5,2,1)/0/
      DATA IKQ(5,2,2)/0/
      DATA IKQ(5,2,3)/0/
      DATA IKQ(5,3,1)/0/
      DATA IKQ(5,3,2)/0/
      DATA IKQ(5,3,3)/0/
      DATA PINQ(1,1)/0/
      DATA PINQ(1,2)/0/
      DATA PINQ(1,3)/0/
      DATA PINQ(2,1)/0/
      DATA PINQ(2,2)/0/
      DATA PINQ(2,3)/0/
      DATA PINQ(3,1)/0/
      DATA PINQ(3,2)/0/
      DATA PINQ(3,3)/0/
      DATA PINQ(4,1)/0/
      DATA PINQ(4,2)/0/
      DATA PINQ(4,3)/0/
      DATA PINQ(5,1)/0/
      DATA PINQ(5,2)/0/
      DATA PINQ(5,3)/0/
      DATA PINQ(6,1)/0/
      DATA PINQ(6,2)/0/
      DATA PINQ(6,3)/0/
      DATA PINQ(7,1)/0/
      DATA PINQ(7,2)/0/
      DATA PINQ(7,3)/0/
      DATA PINQ(8,1)/0/
      DATA PINQ(8,2)/0/
      DATA PINQ(8,3)/0/
      DATA PINQ(9,1)/0/
      DATA PINQ(9,2)/0/
      DATA PINQ(9,3)/0/
      DATA PINQ(10,1)/0/
      DATA PINQ(10,2)/0/
      DATA PINQ(10,3)/0/
      DATA PINQ(11,1)/0/
      DATA PINQ(11,2)/0/
      DATA PINQ(11,3)/0/
      DATA RKQ(1,1)/0/
      DATA RKQ(1,2)/0/
      DATA RKQ(1,3)/0/
      DATA RKQ(2,1)/0/
      DATA RKQ(2,2)/0/
      DATA RKQ(2,3)/0/
      DATA RKQ(3,1)/0/
      DATA RKQ(3,2)/0/
      DATA RKQ(3,3)/0/
      DATA RKQ(4,1)/0/
      DATA RKQ(4,2)/0/
      DATA RKQ(4,3)/0/
      DATA RKQ(5,1)/0/
      DATA RKQ(5,2)/0/
      DATA RKQ(5,3)/0/
      DATA RIQ(1,1)/0/
      DATA RIQ(1,2)/0/
      DATA RIQ(1,3)/0/
      DATA RIQ(2,1)/0/
      DATA RIQ(2,2)/0/
      DATA RIQ(2,3)/0/
      DATA RIQ(3,1)/0/
      DATA RIQ(3,2)/0/
      DATA RIQ(3,3)/0/
      DATA RIQ(4,1)/0/
      DATA RIQ(4,2)/0/
      DATA RIQ(4,3)/0/
      DATA RIQ(5,1)/0/
      DATA RIQ(5,2)/3/
      DATA RIQ(5,3)/0/
      DATA PRESQ(1)/0/
      DATA PRESQ(2)/0/
      DATA PRESQ(3)/0/
      DATA PRESQ(4)/0/
      DATA PRESQ(5)/0/
      DATA PRESQ(6)/0/
      DATA PRESQ(7)/0/
      DATA PRESQ(8)/0/
      DATA PRESQ(9)/0/
      DATA PRESQ(10)/0/
      DATA PRESQ(11)/0/
      DATA STABVELQ/3/
      DATA STABPOSQ/3/
C
C End of values from input file
C
      END
C
      SUBROUTINE SDINIT
C
C
C Initialization routine
C
C
C  This routine must be called before the first call to sdstate(), after
C  supplying values for any `?' parameters in the input.
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION SUMSQ,NORMINV
      INTEGER I,J,K
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      EXTERNAL SDBLKDATA
C
C
C Check that all `?' parameters have been assigned values
C
      DO 100 K=1,3
        IF (GRAVQ(K) .EQ. 1) THEN
          CALL SDSETERR(7,25)
        ENDIF
100   CONTINUE
      DO 140 K=1,5
        IF (MKQ(K) .EQ. 1) THEN
          CALL SDSETERR(7,26)
        ENDIF
        DO 130 I=1,3
          IF (RKQ(K,I) .EQ. 1) THEN
            CALL SDSETERR(7,29)
          ENDIF
          IF (RIQ(K,I) .EQ. 1) THEN
            CALL SDSETERR(7,30)
          ENDIF
          DO 120 J=1,3
            IF (IKQ(K,I,J) .EQ. 1) THEN
              CALL SDSETERR(7,27)
            ENDIF
120       CONTINUE
130     CONTINUE
140   CONTINUE
      DO 160 K=1,11
        DO 150 I=1,3
          IF (PINQ(K,I) .EQ. 1) THEN
            CALL SDSETERR(7,28)
          ENDIF
150     CONTINUE
160   CONTINUE
C
C Normalize pin vectors if necessary
C
C
C Zero out Vpk and Wpk
C
      DO 520 I=1,11
        DO 510 J=I,11
          DO 500 K=1,3
            VPK(I,J,K)= 0D0
            WPK(I,J,K)= 0D0
500       CONTINUE
510     CONTINUE
520   CONTINUE
C
C Compute pseudobody-related constants
C
      RCOM(1,1)= 0D0
      RCOM(1,2)= 0D0
      RCOM(1,3)= 0D0
      RCOM(2,1)= 0D0
      RCOM(2,2)= 0D0
      RCOM(2,3)= 0D0
      RCOM(3,1)= 0D0
      RCOM(3,2)= 0D0
      RCOM(3,3)= 0D0
      RCOM(4,1)= 0D0
      RCOM(4,2)= 0D0
      RCOM(4,3)= 0D0
      RCOM(5,1)= 0D0
      RCOM(5,2)= 0D0
      RCOM(5,3)= 0D0
      DIK(11,2)= (.22D0+RI(5,2))
C
C Compute mass properties-related constants
C
      MTOT= 486D0
      CALL SDSERIALNO(I)
      IF (I .NE. 10001) THEN
        CALL SDSETERR(7,41)
      ENDIF
      ROUSTATE= 1
C
      RETURN
      END
C
C Convert state to form using 1-2-3 Euler angles for ball joints.
C
      SUBROUTINE SDST2ANG(ST,STANG)
      DOUBLE PRECISION ST(12),STANG(11)
C
      INTEGER I
      DOUBLE PRECISION DC(3,3)
C
      DO 100 I=1,11
        STANG(I)= ST(I)
100   CONTINUE
      CALL SDQUAT2DC(ST(4),ST(5),ST(6),ST(12),DC)
      CALL SDDC2ANG(DC,STANG(4),STANG(5),STANG(6))
C
      RETURN
      END
C
C Convert 1-2-3 form of state back to Euler parameters for ball joints.
C
      SUBROUTINE SDANG2ST(STANG,ST)
      DOUBLE PRECISION STANG(11),ST(12)
C
      INTEGER I
      DOUBLE PRECISION DC(3,3)
C
      DO 100 I=1,11
        ST(I)= STANG(I)
100   CONTINUE
      CALL SDANG2DC(STANG(4),STANG(5),STANG(6),DC)
      CALL SDDC2QUAT(DC,ST(4),ST(5),ST(6),ST(12))
C
      RETURN
      END
C
C Normalize Euler parameters in state.
C
      SUBROUTINE SDNRMSTERR(ST,NORMST,ROUTINE)
      DOUBLE PRECISION ST(12),NORMST(12)
      INTEGER ROUTINE
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER I
      DOUBLE PRECISION NORM
C
      DO 100 I=1,12
        NORMST(I)= ST(I)
100   CONTINUE
      NORM= DSQRT(ST(4)*ST(4)+ST(5)*ST(5)+ST(6)*ST(6)+ST(12)*ST(12))
      IF (ROUTINE .NE. 0) THEN
        IF ((NORM .LT. .9D0) .OR. (NORM .GT. 1.1D0)) THEN
          CALL SDSETERR(ROUTINE,14)
        ENDIF
      ENDIF
      IF (NORM .EQ. 0D0) THEN
        NORMST(12)= 1D0
        NORM= 1D0
      ENDIF
      NORM= 1D0/NORM
      NORMST(4)= NORMST(4)*NORM
      NORMST(5)= NORMST(5)*NORM
      NORMST(6)= NORMST(6)*NORM
      NORMST(12)= NORMST(12)*NORM
C
      RETURN
      END
C
      SUBROUTINE SDNORMST(ST,NORMST)
      DOUBLE PRECISION ST(12),NORMST(12)
C
C
      CALL SDNRMSTERR(ST,NORMST,0)
C
      RETURN
      END
C
      SUBROUTINE SDSTATE(TIMEIN,QIN,UIN)
      DOUBLE PRECISION TIMEIN,QIN(12),UIN(11)
C
C
C Compute kinematic information and store it in sdgstate.
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      INTEGER I,J,QCHG,UCHG
      DOUBLE PRECISION EE,STAB
C
      IF ((ROUSTATE .NE. 1) .AND. (ROUSTATE .NE. 2) .AND. (ROUSTATE
     &  .NE. 3)) THEN
        CALL SDSETERR(8,22)
        RETURN
      ENDIF
      IF (ROUSTATE .EQ. 1) THEN
        DO 50 I=1,11
          IF (PRESQ(I) .EQ. 1) THEN
            CALL SDSETERR(8,31)
          ENDIF
50      CONTINUE
      ENDIF
C
C See if time or any qs have changed since last call
C
      IF ((ROUSTATE .NE. 1) .AND. (TIMEIN .EQ. CURTIM)) THEN
        QCHG= 0
        DO 65 I=1,12
          IF (QIN(I) .NE. Q(I)) THEN
            QCHG= 1
            GOTO 70
          ENDIF
65      CONTINUE
70      CONTINUE
      ELSE
        QCHG= 1
      ENDIF
C
C If time and qs are unchanged, check us
C
      IF (QCHG .EQ. 0) THEN
        UCHG= 0
        DO 75 I=1,11
          IF (UIN(I) .NE. U(I)) THEN
            UCHG= 1
            GOTO 80
          ENDIF
75      CONTINUE
80      CONTINUE
      ELSE
        UCHG= 1
      ENDIF
      CURTIM= TIMEIN
      ROUSTATE= 2
      IF (QCHG .EQ. 0) THEN
C goto skipqs
        GOTO 190
      ENDIF
C
C Position-related variables need to be computed
C
      VPKFLG= 0
      MMFLG= 0
      MMLDUFLG= 0
      WWFLG= 0
      DO 100 I=1,12
        Q(I)= QIN(I)
100   CONTINUE
C
C Normalize Euler parameters in state
C
      CALL SDNRMSTERR(Q,QN,8)
C
C Compute sines and cosines of q
C
      S7= DSIN(Q(7))
      C7= DCOS(Q(7))
      S8= DSIN(Q(8))
      C8= DCOS(Q(8))
      S9= DSIN(Q(9))
      C9= DCOS(Q(9))
      S10= DSIN(Q(10))
      C10= DCOS(Q(10))
      S11= DSIN(Q(11))
      C11= DCOS(Q(11))
C
C Compute across-axis direction cosines Cik
C
      CIK(4,1,1)= (1D0-(2D0*((QN(5)*QN(5))+(QN(6)*QN(6)))))
      CIK(4,1,2)= (2D0*((QN(4)*QN(5))-(QN(6)*QN(12))))
      CIK(4,1,3)= (2D0*((QN(4)*QN(6))+(QN(5)*QN(12))))
      CIK(4,2,1)= (2D0*((QN(4)*QN(5))+(QN(6)*QN(12))))
      CIK(4,2,2)= (1D0-(2D0*((QN(4)*QN(4))+(QN(6)*QN(6)))))
      CIK(4,2,3)= (2D0*((QN(5)*QN(6))-(QN(4)*QN(12))))
      CIK(4,3,1)= (2D0*((QN(4)*QN(6))-(QN(5)*QN(12))))
      CIK(4,3,2)= (2D0*((QN(4)*QN(12))+(QN(5)*QN(6))))
      CIK(4,3,3)= (1D0-(2D0*((QN(4)*QN(4))+(QN(5)*QN(5)))))
C
C Compute across-joint direction cosines Cib
C
      CIB(4,1,2)= -(S9*C10)
      CIB(4,1,3)= (S9*S10)
      CIB(4,2,2)= (C9*C10)
      CIB(4,2,3)= -(S10*C9)
C
C Compute gravity
C
C
C Compute cnk & cnb (direction cosines in N)
C
      CNK(7,1,1)= ((CIK(4,1,1)*C7)+(CIK(4,1,2)*S7))
      CNK(7,1,2)= ((CIK(4,1,2)*C7)-(CIK(4,1,1)*S7))
      CNK(7,2,1)= ((CIK(4,2,1)*C7)+(CIK(4,2,2)*S7))
      CNK(7,2,2)= ((CIK(4,2,2)*C7)-(CIK(4,2,1)*S7))
      CNK(7,3,1)= ((CIK(4,3,1)*C7)+(CIK(4,3,2)*S7))
      CNK(7,3,2)= ((CIK(4,3,2)*C7)-(CIK(4,3,1)*S7))
      CNK(8,1,2)= ((CNK(7,1,2)*C8)-(CIK(4,1,3)*S8))
      CNK(8,1,3)= ((CIK(4,1,3)*C8)+(CNK(7,1,2)*S8))
      CNK(8,2,2)= ((CNK(7,2,2)*C8)-(CIK(4,2,3)*S8))
      CNK(8,2,3)= ((CIK(4,2,3)*C8)+(CNK(7,2,2)*S8))
      CNK(8,3,2)= ((CNK(7,3,2)*C8)-(CIK(4,3,3)*S8))
      CNK(8,3,3)= ((CIK(4,3,3)*C8)+(CNK(7,3,2)*S8))
      CNK(9,1,1)= ((CIK(4,1,1)*C9)+(CIK(4,1,2)*S9))
      CNK(9,1,2)= ((CIK(4,1,2)*C9)-(CIK(4,1,1)*S9))
      CNK(9,2,1)= ((CIK(4,2,1)*C9)+(CIK(4,2,2)*S9))
      CNK(9,2,2)= ((CIK(4,2,2)*C9)-(CIK(4,2,1)*S9))
      CNK(9,3,1)= ((CIK(4,3,1)*C9)+(CIK(4,3,2)*S9))
      CNK(9,3,2)= ((CIK(4,3,2)*C9)-(CIK(4,3,1)*S9))
      CNK(10,1,2)= ((CIK(4,1,3)*S10)+(CNK(9,1,2)*C10))
      CNK(10,1,3)= ((CIK(4,1,3)*C10)-(CNK(9,1,2)*S10))
      CNK(10,2,2)= ((CIK(4,2,3)*S10)+(CNK(9,2,2)*C10))
      CNK(10,2,3)= ((CIK(4,2,3)*C10)-(CNK(9,2,2)*S10))
      CNK(10,3,2)= ((CIK(4,3,3)*S10)+(CNK(9,3,2)*C10))
      CNK(10,3,3)= ((CIK(4,3,3)*C10)-(CNK(9,3,2)*S10))
      CNK(11,1,1)= ((CNK(7,1,1)*C11)+(CNK(8,1,2)*S11))
      CNK(11,1,2)= ((CNK(8,1,2)*C11)-(CNK(7,1,1)*S11))
      CNK(11,2,1)= ((CNK(7,2,1)*C11)+(CNK(8,2,2)*S11))
      CNK(11,2,2)= ((CNK(8,2,2)*C11)-(CNK(7,2,1)*S11))
      CNK(11,3,1)= ((CNK(7,3,1)*C11)+(CNK(8,3,2)*S11))
      CNK(11,3,2)= ((CNK(8,3,2)*C11)-(CNK(7,3,1)*S11))
      CNB(1,1,1)= CIK(4,1,1)
      CNB(1,1,2)= CIK(4,1,2)
      CNB(1,1,3)= CIK(4,1,3)
      CNB(1,2,1)= CIK(4,2,1)
      CNB(1,2,2)= CIK(4,2,2)
      CNB(1,2,3)= CIK(4,2,3)
      CNB(1,3,1)= CIK(4,3,1)
      CNB(1,3,2)= CIK(4,3,2)
      CNB(1,3,3)= CIK(4,3,3)
      CNB(2,1,1)= CNK(7,1,1)
      CNB(2,1,2)= CNK(7,1,2)
      CNB(2,1,3)= CIK(4,1,3)
      CNB(2,2,1)= CNK(7,2,1)
      CNB(2,2,2)= CNK(7,2,2)
      CNB(2,2,3)= CIK(4,2,3)
      CNB(2,3,1)= CNK(7,3,1)
      CNB(2,3,2)= CNK(7,3,2)
      CNB(2,3,3)= CIK(4,3,3)
      CNB(3,1,1)= CNK(7,1,1)
      CNB(3,1,2)= CNK(8,1,2)
      CNB(3,1,3)= CNK(8,1,3)
      CNB(3,2,1)= CNK(7,2,1)
      CNB(3,2,2)= CNK(8,2,2)
      CNB(3,2,3)= CNK(8,2,3)
      CNB(3,3,1)= CNK(7,3,1)
      CNB(3,3,2)= CNK(8,3,2)
      CNB(3,3,3)= CNK(8,3,3)
      CNB(4,1,1)= CNK(9,1,1)
      CNB(4,1,2)= CNK(10,1,2)
      CNB(4,1,3)= CNK(10,1,3)
      CNB(4,2,1)= CNK(9,2,1)
      CNB(4,2,2)= CNK(10,2,2)
      CNB(4,2,3)= CNK(10,2,3)
      CNB(4,3,1)= CNK(9,3,1)
      CNB(4,3,2)= CNK(10,3,2)
      CNB(4,3,3)= CNK(10,3,3)
      CNB(5,1,1)= CNK(11,1,1)
      CNB(5,1,2)= CNK(11,1,2)
      CNB(5,1,3)= CNK(8,1,3)
      CNB(5,2,1)= CNK(11,2,1)
      CNB(5,2,2)= CNK(11,2,2)
      CNB(5,2,3)= CNK(8,2,3)
      CNB(5,3,1)= CNK(11,3,1)
      CNB(5,3,2)= CNK(11,3,2)
      CNB(5,3,3)= CNK(8,3,3)
C
C Compute q-related auxiliary variables
C
      RIK(8,2)= (.22D0+((.75D0*S8)-(.1D0*C8)))
      RIK(8,3)= -(.2D0+((.1D0*S8)+(.75D0*C8)))
      RIK(11,1)= (RI(5,2)*S11)
      RIK(11,2)= ((RI(5,2)*C11)-.01D0)
C
C Compute rnk & rnb (mass center locations in N)
C
      RNK(7,1)= ((.75D0*CIK(4,1,3))+(Q(1)-(1.5D0*CIK(4,1,3))))
      RNK(7,2)= ((.75D0*CIK(4,2,3))+(Q(2)-(1.5D0*CIK(4,2,3))))
      RNK(7,3)= ((.75D0*CIK(4,3,3))+(Q(3)-(1.5D0*CIK(4,3,3))))
      RNK(8,1)= ((RNK(7,1)-((.1D0*CNK(7,1,2))+(.75D0*CIK(4,1,3))))+((
     & .22D0*CNK(8,1,2))-(.2D0*CNK(8,1,3))))
      RNK(8,2)= ((RNK(7,2)-((.1D0*CNK(7,2,2))+(.75D0*CIK(4,2,3))))+((
     & .22D0*CNK(8,2,2))-(.2D0*CNK(8,2,3))))
      RNK(8,3)= ((RNK(7,3)-((.1D0*CNK(7,3,2))+(.75D0*CIK(4,3,3))))+((
     & .22D0*CNK(8,3,2))-(.2D0*CNK(8,3,3))))
      RNK(9,1)= (Q(1)-(1.2D0*CIK(4,1,2)))
      RNK(9,2)= (Q(2)-(1.2D0*CIK(4,2,2)))
      RNK(9,3)= (Q(3)-(1.2D0*CIK(4,3,2)))
      RNK(10,1)= (RNK(9,1)-(3.3D0*CNK(10,1,2)))
      RNK(10,2)= (RNK(9,2)-(3.3D0*CNK(10,2,2)))
      RNK(10,3)= (RNK(9,3)-(3.3D0*CNK(10,3,2)))
      RNK(11,1)= ((RNK(8,1)+(CNK(8,1,2)*RI(5,2)))-(.01D0*CNK(11,1,2)))
      RNK(11,2)= ((RNK(8,2)+(CNK(8,2,2)*RI(5,2)))-(.01D0*CNK(11,2,2)))
      RNK(11,3)= ((RNK(8,3)+(CNK(8,3,2)*RI(5,2)))-(.01D0*CNK(11,3,2)))
      RNB(1,1)= Q(1)
      RNB(1,2)= Q(2)
      RNB(1,3)= Q(3)
      RNB(2,1)= RNK(7,1)
      RNB(2,2)= RNK(7,2)
      RNB(2,3)= RNK(7,3)
      RNB(3,1)= RNK(8,1)
      RNB(3,2)= RNK(8,2)
      RNB(3,3)= RNK(8,3)
      RNB(4,1)= RNK(10,1)
      RNB(4,2)= RNK(10,2)
      RNB(4,3)= RNK(10,3)
      RNB(5,1)= RNK(11,1)
      RNB(5,2)= RNK(11,2)
      RNB(5,3)= RNK(11,3)
C
C Compute com (system mass center location in N)
C
      COM(1)= (.00205761316872428D0*(RNK(11,1)+((10.7D0*RNK(10,1))+((
     & 57.5D0*RNK(8,1))+((6.8D0*RNK(7,1))+(410D0*Q(1)))))))
      COM(2)= (.00205761316872428D0*(RNK(11,2)+((10.7D0*RNK(10,2))+((
     & 57.5D0*RNK(8,2))+((6.8D0*RNK(7,2))+(410D0*Q(2)))))))
      COM(3)= (.00205761316872428D0*(RNK(11,3)+((10.7D0*RNK(10,3))+((
     & 57.5D0*RNK(8,3))+((6.8D0*RNK(7,3))+(410D0*Q(3)))))))
C
C Compute constraint position errors
C
C skipqs
190   CONTINUE
      IF (UCHG .EQ. 0) THEN
C goto skipus
        GOTO 290
      ENDIF
C
C Velocity-related variables need to be computed
C
      INERFLG= 0
      DO 200 I=1,11
        U(I)= UIN(I)
200   CONTINUE
C
C Compute u-related auxiliary variables
C
C
C Compute wk & wb (angular velocities)
C
      WK(7,1)= ((U(4)*C7)+(U(5)*S7))
      WK(7,2)= ((U(5)*C7)-(U(4)*S7))
      WK(7,3)= (U(6)+U(7))
      WK(8,1)= (WK(7,1)-U(8))
      WK(8,2)= ((WK(7,2)*C8)-(WK(7,3)*S8))
      WK(8,3)= ((WK(7,2)*S8)+(WK(7,3)*C8))
      WK(9,1)= ((U(4)*C9)+(U(5)*S9))
      WK(9,2)= ((U(5)*C9)-(U(4)*S9))
      WK(9,3)= (U(6)+U(9))
      WK(10,1)= (U(10)+WK(9,1))
      WK(10,2)= ((WK(9,2)*C10)+(WK(9,3)*S10))
      WK(10,3)= ((WK(9,3)*C10)-(WK(9,2)*S10))
      WK(11,1)= ((WK(8,1)*C11)+(WK(8,2)*S11))
      WK(11,2)= ((WK(8,2)*C11)-(WK(8,1)*S11))
      WK(11,3)= (U(11)+WK(8,3))
      WB(1,1)= U(4)
      WB(1,2)= U(5)
      WB(1,3)= U(6)
      WB(2,1)= WK(7,1)
      WB(2,2)= WK(7,2)
      WB(2,3)= WK(7,3)
      WB(3,1)= WK(8,1)
      WB(3,2)= WK(8,2)
      WB(3,3)= WK(8,3)
      WB(4,1)= WK(10,1)
      WB(4,2)= WK(10,2)
      WB(4,3)= WK(10,3)
      WB(5,1)= WK(11,1)
      WB(5,2)= WK(11,2)
      WB(5,3)= WK(11,3)
C
C Compute auxiliary variables involving wk
C
      WIRK(8,1)= ((.1D0*WK(7,3))-(.75D0*WK(7,2)))
      WIRK(11,1)= -(RI(5,2)*WK(8,3))
      WIRK(11,3)= (RI(5,2)*WK(8,1))
      WKRPK(8,1)= -((.2D0*WK(8,2))+(.22D0*WK(8,3)))
      IKWK(6,1)= ((14D0*U(6))+((115D0*U(4))-(14D0*U(5))))
      IKWK(6,2)= (((316D0*U(5))-(14D0*U(4)))-(34.6D0*U(6)))
      IKWK(6,3)= ((440D0*U(6))+((14D0*U(4))-(34.6D0*U(5))))
      WKIKWK(6,1)= ((IKWK(6,3)*U(5))-(IKWK(6,2)*U(6)))
      WKIKWK(6,2)= ((IKWK(6,1)*U(6))-(IKWK(6,3)*U(4)))
      WKIKWK(6,3)= ((IKWK(6,2)*U(4))-(IKWK(6,1)*U(5)))
      WKIKWK(7,1)= -(.35D0*(WK(7,2)*WK(7,3)))
      WKIKWK(7,2)= (.35D0*(WK(7,1)*WK(7,3)))
      IKWK(8,1)= (((.41D0*WK(8,2))+(4.85D0*WK(8,1)))-(.07D0*WK(8,3)))
      IKWK(8,2)= ((.54D0*WK(8,3))+((.41D0*WK(8,1))+(2.2D0*WK(8,2))))
      IKWK(8,3)= ((5.5D0*WK(8,3))+((.54D0*WK(8,2))-(.07D0*WK(8,1))))
      WKIKWK(8,1)= ((IKWK(8,3)*WK(8,2))-(IKWK(8,2)*WK(8,3)))
      WKIKWK(8,2)= ((IKWK(8,1)*WK(8,3))-(IKWK(8,3)*WK(8,1)))
      WKIKWK(8,3)= ((IKWK(8,2)*WK(8,1))-(IKWK(8,1)*WK(8,2)))
      WKIKWK(10,1)= (27D0*(WK(10,2)*WK(10,3)))
      WKIKWK(10,3)= -(27D0*(WK(10,1)*WK(10,2)))
      WKIKWK(11,1)= (.4D0*(WK(11,2)*WK(11,3)))
      WKIKWK(11,2)= -(.4D0*(WK(11,1)*WK(11,3)))
C
C Compute temporaries for use in SDRHS
C
      W0W0(1)= (U(4)*U(4))
      W0W0(2)= (WK(7,1)*WK(7,1))
      W0W0(3)= (WK(8,1)*WK(8,1))
      W0W0(4)= (WK(10,1)*WK(10,1))
      W0W0(5)= (WK(11,1)*WK(11,1))
      W1W1(1)= (U(5)*U(5))
      W1W1(2)= (WK(7,2)*WK(7,2))
      W1W1(3)= (WK(8,2)*WK(8,2))
      W1W1(4)= (WK(10,2)*WK(10,2))
      W1W1(5)= (WK(11,2)*WK(11,2))
      W2W2(1)= (U(6)*U(6))
      W2W2(2)= (WK(7,3)*WK(7,3))
      W2W2(3)= (WK(8,3)*WK(8,3))
      W2W2(4)= (WK(10,3)*WK(10,3))
      W2W2(5)= (WK(11,3)*WK(11,3))
      W0W1(1)= (U(4)*U(5))
      W0W1(2)= (WK(7,1)*WK(7,2))
      W0W1(3)= (WK(8,1)*WK(8,2))
      W0W1(4)= (WK(10,1)*WK(10,2))
      W0W1(5)= (WK(11,1)*WK(11,2))
      W0W2(1)= (U(4)*U(6))
      W0W2(2)= (WK(7,1)*WK(7,3))
      W0W2(3)= (WK(8,1)*WK(8,3))
      W0W2(4)= (WK(10,1)*WK(10,3))
      W0W2(5)= (WK(11,1)*WK(11,3))
      W1W2(1)= (U(5)*U(6))
      W1W2(2)= (WK(7,2)*WK(7,3))
      W1W2(3)= (WK(8,2)*WK(8,3))
      W1W2(4)= (WK(10,2)*WK(10,3))
      W1W2(5)= (WK(11,2)*WK(11,3))
      W00W11(1)= -(W0W0(1)+W1W1(1))
      W00W11(2)= -(W0W0(2)+W1W1(2))
      W00W11(3)= -(W0W0(3)+W1W1(3))
      W00W11(4)= -(W0W0(4)+W1W1(4))
      W00W11(5)= -(W0W0(5)+W1W1(5))
      W00W22(1)= -(W0W0(1)+W2W2(1))
      W00W22(2)= -(W0W0(2)+W2W2(2))
      W00W22(3)= -(W0W0(3)+W2W2(3))
      W00W22(4)= -(W0W0(4)+W2W2(4))
      W00W22(5)= -(W0W0(5)+W2W2(5))
      W11W22(1)= -(W1W1(1)+W2W2(1))
      W11W22(2)= -(W1W1(2)+W2W2(2))
      W11W22(3)= -(W1W1(3)+W2W2(3))
      W11W22(4)= -(W1W1(4)+W2W2(4))
      W11W22(5)= -(W1W1(5)+W2W2(5))
C
C Compute vnk & vnb (mass center linear velocities in N)
C
      VNK(7,1)= ((.75D0*((CNK(7,1,1)*WK(7,2))-(CNK(7,1,2)*WK(7,1))))+(
     & U(1)+(1.5D0*((CIK(4,1,2)*U(4))-(CIK(4,1,1)*U(5))))))
      VNK(7,2)= ((.75D0*((CNK(7,2,1)*WK(7,2))-(CNK(7,2,2)*WK(7,1))))+(
     & U(2)+(1.5D0*((CIK(4,2,2)*U(4))-(CIK(4,2,1)*U(5))))))
      VNK(7,3)= ((.75D0*((CNK(7,3,1)*WK(7,2))-(CNK(7,3,2)*WK(7,1))))+(
     & U(3)+(1.5D0*((CIK(4,3,2)*U(4))-(CIK(4,3,1)*U(5))))))
      VNK(8,1)= ((VNK(7,1)+(((.75D0*(CNK(7,1,2)*WK(7,1)))+(CNK(7,1,1)*
     & WIRK(8,1)))-(.1D0*(CIK(4,1,3)*WK(7,1)))))+((.22D0*(CNK(8,1,3)*
     & WK(8,1)))+((.2D0*(CNK(8,1,2)*WK(8,1)))+(CNK(7,1,1)*WKRPK(8,1)))))
      VNK(8,2)= ((VNK(7,2)+(((.75D0*(CNK(7,2,2)*WK(7,1)))+(CNK(7,2,1)*
     & WIRK(8,1)))-(.1D0*(CIK(4,2,3)*WK(7,1)))))+((.22D0*(CNK(8,2,3)*
     & WK(8,1)))+((.2D0*(CNK(8,2,2)*WK(8,1)))+(CNK(7,2,1)*WKRPK(8,1)))))
      VNK(8,3)= ((VNK(7,3)+(((.75D0*(CNK(7,3,2)*WK(7,1)))+(CNK(7,3,1)*
     & WIRK(8,1)))-(.1D0*(CIK(4,3,3)*WK(7,1)))))+((.22D0*(CNK(8,3,3)*
     & WK(8,1)))+((.2D0*(CNK(8,3,2)*WK(8,1)))+(CNK(7,3,1)*WKRPK(8,1)))))
      VNK(9,1)= (U(1)+(1.2D0*((CIK(4,1,1)*U(6))-(CIK(4,1,3)*U(4)))))
      VNK(9,2)= (U(2)+(1.2D0*((CIK(4,2,1)*U(6))-(CIK(4,2,3)*U(4)))))
      VNK(9,3)= (U(3)+(1.2D0*((CIK(4,3,1)*U(6))-(CIK(4,3,3)*U(4)))))
      VNK(10,1)= (VNK(9,1)+(3.3D0*((CNK(9,1,1)*WK(10,3))-(CNK(10,1,3)*
     & WK(10,1)))))
      VNK(10,2)= (VNK(9,2)+(3.3D0*((CNK(9,2,1)*WK(10,3))-(CNK(10,2,3)*
     & WK(10,1)))))
      VNK(10,3)= (VNK(9,3)+(3.3D0*((CNK(9,3,1)*WK(10,3))-(CNK(10,3,3)*
     & WK(10,1)))))
      VNK(11,1)= ((.01D0*((CNK(11,1,1)*WK(11,3))-(CNK(8,1,3)*WK(11,1))))
     & +(VNK(8,1)+((CNK(7,1,1)*WIRK(11,1))+(CNK(8,1,3)*WIRK(11,3)))))
      VNK(11,2)= ((.01D0*((CNK(11,2,1)*WK(11,3))-(CNK(8,2,3)*WK(11,1))))
     & +(VNK(8,2)+((CNK(7,2,1)*WIRK(11,1))+(CNK(8,2,3)*WIRK(11,3)))))
      VNK(11,3)= ((.01D0*((CNK(11,3,1)*WK(11,3))-(CNK(8,3,3)*WK(11,1))))
     & +(VNK(8,3)+((CNK(7,3,1)*WIRK(11,1))+(CNK(8,3,3)*WIRK(11,3)))))
      VNB(1,1)= U(1)
      VNB(1,2)= U(2)
      VNB(1,3)= U(3)
      VNB(2,1)= VNK(7,1)
      VNB(2,2)= VNK(7,2)
      VNB(2,3)= VNK(7,3)
      VNB(3,1)= VNK(8,1)
      VNB(3,2)= VNK(8,2)
      VNB(3,3)= VNK(8,3)
      VNB(4,1)= VNK(10,1)
      VNB(4,2)= VNK(10,2)
      VNB(4,3)= VNK(10,3)
      VNB(5,1)= VNK(11,1)
      VNB(5,2)= VNK(11,2)
      VNB(5,3)= VNK(11,3)
C
C Compute qdot (kinematical equations)
C
      QDOT(1)= U(1)
      QDOT(2)= U(2)
      QDOT(3)= U(3)
      QDOT(4)= (.5D0*((Q(12)*U(4))+((Q(5)*U(6))-(Q(6)*U(5)))))
      QDOT(5)= (.5D0*((Q(6)*U(4))+((Q(12)*U(5))-(Q(4)*U(6)))))
      QDOT(6)= (.5D0*(((Q(4)*U(5))+(Q(12)*U(6)))-(Q(5)*U(4))))
      QDOT(12)= -(.5D0*((Q(4)*U(4))+((Q(5)*U(5))+(Q(6)*U(6)))))
      IF (STABVEL  .NE.  0D0) THEN
        EE= ((Q(12)*Q(12))+((Q(6)*Q(6))+((Q(4)*Q(4))+(Q(5)*Q(5)))))
        STAB= ((STABVEL*(1D0-EE))/EE)
        QDOT(4)= (QDOT(4)+(Q(4)*STAB))
        QDOT(5)= (QDOT(5)+(Q(5)*STAB))
        QDOT(6)= (QDOT(6)+(Q(6)*STAB))
        QDOT(12)= (QDOT(12)+(Q(12)*STAB))
      ENDIF
      QDOT(7)= U(7)
      QDOT(8)= U(8)
      QDOT(9)= U(9)
      QDOT(10)= U(10)
      QDOT(11)= U(11)
C
C Compute constraint velocity errors
C
C skipus
290   CONTINUE
C
C Initialize applied forces and torques to zero
C
      DO 310 I=1,5
        DO 300 J=1,3
          UFK(I,J)= 0D0
          UTK(I,J)= 0D0
300     CONTINUE
310   CONTINUE
      DO 320 I=1,11
        UTAU(I)= 0D0
320   CONTINUE
      LTAUFLG= 0
      FS0FLG= 0
C
C Initialize prescribed motions
C
      UACC(11)= 0D0
      UVEL(11)= U(11)
      UPOS(11)= Q(11)
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  233 adds/subtracts/negates
C                     346 multiplies
C                       1 divides
C                     337 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDQDOT(OQDOT)
      DOUBLE PRECISION OQDOT(12)
C
C
C Return position coordinate derivatives for tree joints.
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(63,23)
        RETURN
      ENDIF
      DO 100 I=1,12
        OQDOT(I)= QDOT(I)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDU2QDOT(UIN,OQDOT)
      DOUBLE PRECISION UIN(11),OQDOT(12)
C
C
C Convert velocities to qdots.
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(64,23)
        RETURN
      ENDIF
      DO 100 I=1,11
        OQDOT(I)= UIN(I)
100   CONTINUE
      OQDOT(4)= (.5D0*((Q(12)*UIN(4))+((Q(5)*UIN(6))-(Q(6)*UIN(5)))))
      OQDOT(5)= (.5D0*((Q(6)*UIN(4))+((Q(12)*UIN(5))-(Q(4)*UIN(6)))))
      OQDOT(6)= (.5D0*(((Q(4)*UIN(5))+(Q(12)*UIN(6)))-(Q(5)*UIN(4))))
      OQDOT(12)= -(.5D0*((Q(4)*UIN(4))+((Q(5)*UIN(5))+(Q(6)*UIN(6)))))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    9 adds/subtracts/negates
C                      16 multiplies
C                       0 divides
C                      15 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDPSSTATE(LQIN)
      DOUBLE PRECISION LQIN(1)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
C
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(9,23)
        RETURN
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDDOVPK
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
      IF (VPKFLG .EQ. 0) THEN
C
C Compute Wpk (partial angular velocities)
C
        WPK(4,4,1)= 1D0
        WPK(4,5,1)= 1D0
        WPK(4,6,1)= 1D0
        WPK(4,7,1)= C7
        WPK(4,7,2)= -S7
        WPK(4,8,1)= C7
        WPK(4,8,2)= -(S7*C8)
        WPK(4,8,3)= -(S7*S8)
        WPK(4,9,1)= C9
        WPK(4,9,2)= -S9
        WPK(4,10,1)= C9
        WPK(4,10,2)= -(S9*C10)
        WPK(4,10,3)= (S9*S10)
        WPK(4,11,1)= ((WPK(4,8,2)*S11)+(C7*C11))
        WPK(4,11,2)= ((WPK(4,8,2)*C11)-(S11*C7))
        WPK(4,11,3)= WPK(4,8,3)
        WPK(5,5,2)= 1D0
        WPK(5,6,2)= 1D0
        WPK(5,7,1)= S7
        WPK(5,7,2)= C7
        WPK(5,8,1)= S7
        WPK(5,8,2)= (C7*C8)
        WPK(5,8,3)= (S8*C7)
        WPK(5,9,1)= S9
        WPK(5,9,2)= C9
        WPK(5,10,1)= S9
        WPK(5,10,2)= (C9*C10)
        WPK(5,10,3)= -(S10*C9)
        WPK(5,11,1)= ((WPK(5,8,2)*S11)+(S7*C11))
        WPK(5,11,2)= ((WPK(5,8,2)*C11)-(S7*S11))
        WPK(5,11,3)= WPK(5,8,3)
        WPK(6,6,3)= 1D0
        WPK(6,7,3)= 1D0
        WPK(6,8,2)= -S8
        WPK(6,8,3)= C8
        WPK(6,9,3)= 1D0
        WPK(6,10,2)= S10
        WPK(6,10,3)= C10
        WPK(6,11,1)= -(S8*S11)
        WPK(6,11,2)= -(S8*C11)
        WPK(6,11,3)= C8
        WPK(7,7,3)= 1D0
        WPK(7,8,2)= -S8
        WPK(7,8,3)= C8
        WPK(7,11,1)= -(S8*S11)
        WPK(7,11,2)= -(S8*C11)
        WPK(7,11,3)= C8
        WPK(8,8,1)= -1D0
        WPK(8,11,1)= -C11
        WPK(8,11,2)= S11
        WPK(9,9,3)= 1D0
        WPK(9,10,2)= S10
        WPK(9,10,3)= C10
        WPK(10,10,1)= 1D0
        WPK(11,11,3)= 1D0
C
C Compute Vpk (partial velocities)
C
        VPK(1,1,1)= 1D0
        VPK(1,2,1)= 1D0
        VPK(1,3,1)= 1D0
        VPK(1,4,1)= CIK(4,1,1)
        VPK(1,4,2)= CIK(4,1,2)
        VPK(1,4,3)= CIK(4,1,3)
        VPK(1,5,1)= CIK(4,1,1)
        VPK(1,5,2)= CIK(4,1,2)
        VPK(1,5,3)= CIK(4,1,3)
        VPK(1,6,1)= CIK(4,1,1)
        VPK(1,6,2)= CIK(4,1,2)
        VPK(1,6,3)= CIK(4,1,3)
        VPK(1,7,1)= ((CIK(4,1,1)*C7)+(CIK(4,1,2)*S7))
        VPK(1,7,2)= ((CIK(4,1,2)*C7)-(CIK(4,1,1)*S7))
        VPK(1,7,3)= CIK(4,1,3)
        VPK(1,8,1)= VPK(1,7,1)
        VPK(1,8,2)= ((VPK(1,7,2)*C8)-(CIK(4,1,3)*S8))
        VPK(1,8,3)= ((CIK(4,1,3)*C8)+(VPK(1,7,2)*S8))
        VPK(1,9,1)= ((CIK(4,1,1)*C9)+(CIK(4,1,2)*S9))
        VPK(1,9,2)= ((CIK(4,1,2)*C9)-(CIK(4,1,1)*S9))
        VPK(1,9,3)= CIK(4,1,3)
        VPK(1,10,1)= VPK(1,9,1)
        VPK(1,10,2)= ((CIK(4,1,3)*S10)+(VPK(1,9,2)*C10))
        VPK(1,10,3)= ((CIK(4,1,3)*C10)-(VPK(1,9,2)*S10))
        VPK(1,11,1)= ((VPK(1,7,1)*C11)+(VPK(1,8,2)*S11))
        VPK(1,11,2)= ((VPK(1,8,2)*C11)-(VPK(1,7,1)*S11))
        VPK(1,11,3)= VPK(1,8,3)
        VPK(2,2,2)= 1D0
        VPK(2,3,2)= 1D0
        VPK(2,4,1)= CIK(4,2,1)
        VPK(2,4,2)= CIK(4,2,2)
        VPK(2,4,3)= CIK(4,2,3)
        VPK(2,5,1)= CIK(4,2,1)
        VPK(2,5,2)= CIK(4,2,2)
        VPK(2,5,3)= CIK(4,2,3)
        VPK(2,6,1)= CIK(4,2,1)
        VPK(2,6,2)= CIK(4,2,2)
        VPK(2,6,3)= CIK(4,2,3)
        VPK(2,7,1)= ((CIK(4,2,1)*C7)+(CIK(4,2,2)*S7))
        VPK(2,7,2)= ((CIK(4,2,2)*C7)-(CIK(4,2,1)*S7))
        VPK(2,7,3)= CIK(4,2,3)
        VPK(2,8,1)= VPK(2,7,1)
        VPK(2,8,2)= ((VPK(2,7,2)*C8)-(CIK(4,2,3)*S8))
        VPK(2,8,3)= ((CIK(4,2,3)*C8)+(VPK(2,7,2)*S8))
        VPK(2,9,1)= ((CIK(4,2,1)*C9)+(CIK(4,2,2)*S9))
        VPK(2,9,2)= ((CIK(4,2,2)*C9)-(CIK(4,2,1)*S9))
        VPK(2,9,3)= CIK(4,2,3)
        VPK(2,10,1)= VPK(2,9,1)
        VPK(2,10,2)= ((CIK(4,2,3)*S10)+(VPK(2,9,2)*C10))
        VPK(2,10,3)= ((CIK(4,2,3)*C10)-(VPK(2,9,2)*S10))
        VPK(2,11,1)= ((VPK(2,7,1)*C11)+(VPK(2,8,2)*S11))
        VPK(2,11,2)= ((VPK(2,8,2)*C11)-(VPK(2,7,1)*S11))
        VPK(2,11,3)= VPK(2,8,3)
        VPK(3,3,3)= 1D0
        VPK(3,4,1)= CIK(4,3,1)
        VPK(3,4,2)= CIK(4,3,2)
        VPK(3,4,3)= CIK(4,3,3)
        VPK(3,5,1)= CIK(4,3,1)
        VPK(3,5,2)= CIK(4,3,2)
        VPK(3,5,3)= CIK(4,3,3)
        VPK(3,6,1)= CIK(4,3,1)
        VPK(3,6,2)= CIK(4,3,2)
        VPK(3,6,3)= CIK(4,3,3)
        VPK(3,7,1)= ((CIK(4,3,1)*C7)+(CIK(4,3,2)*S7))
        VPK(3,7,2)= ((CIK(4,3,2)*C7)-(CIK(4,3,1)*S7))
        VPK(3,7,3)= CIK(4,3,3)
        VPK(3,8,1)= VPK(3,7,1)
        VPK(3,8,2)= ((VPK(3,7,2)*C8)-(CIK(4,3,3)*S8))
        VPK(3,8,3)= ((CIK(4,3,3)*C8)+(VPK(3,7,2)*S8))
        VPK(3,9,1)= ((CIK(4,3,1)*C9)+(CIK(4,3,2)*S9))
        VPK(3,9,2)= ((CIK(4,3,2)*C9)-(CIK(4,3,1)*S9))
        VPK(3,9,3)= CIK(4,3,3)
        VPK(3,10,1)= VPK(3,9,1)
        VPK(3,10,2)= ((CIK(4,3,3)*S10)+(VPK(3,9,2)*C10))
        VPK(3,10,3)= ((CIK(4,3,3)*C10)-(VPK(3,9,2)*S10))
        VPK(3,11,1)= ((VPK(3,7,1)*C11)+(VPK(3,8,2)*S11))
        VPK(3,11,2)= ((VPK(3,8,2)*C11)-(VPK(3,7,1)*S11))
        VPK(3,11,3)= VPK(3,8,3)
        VPK(4,7,1)= (.75D0*S7)
        VPK(4,7,2)= (.75D0*C7)
        VPK(4,8,1)= ((1.5D0*S7)-((.2D0*WPK(4,8,2))+(.22D0*WPK(4,8,3))))
        VPK(4,8,2)= ((.2D0*C7)+((.1D0*(S8*C7))+(1.5D0*(C7*C8))))
        VPK(4,8,3)= ((.22D0*C7)+((1.5D0*(S8*C7))-(.1D0*(C7*C8))))
        VPK(4,9,3)= -1.2D0
        VPK(4,10,1)= (3.3D0*WPK(4,10,3))
        VPK(4,10,2)= -(1.2D0*S10)
        VPK(4,10,3)= -((1.2D0*C10)+(3.3D0*C9))
        VWRI(4,11,1)= (VPK(4,8,1)-(RI(5,2)*WPK(4,8,3)))
        VWRI(4,11,3)= (VPK(4,8,3)+(RI(5,2)*C7))
        VPK(4,11,1)= ((.01D0*WPK(4,8,3))+((VPK(4,8,2)*S11)+(VWRI(4,11,1)
     &   *C11)))
        VPK(4,11,2)= ((VPK(4,8,2)*C11)-(VWRI(4,11,1)*S11))
        VPK(4,11,3)= (VWRI(4,11,3)-(.01D0*WPK(4,11,1)))
        VPK(5,7,1)= -(.75D0*C7)
        VPK(5,7,2)= (.75D0*S7)
        VPK(5,8,1)= -((1.5D0*C7)+((.2D0*WPK(5,8,2))+(.22D0*WPK(5,8,3))))
        VPK(5,8,2)= ((.2D0*S7)+((.1D0*(S7*S8))+(1.5D0*(S7*C8))))
        VPK(5,8,3)= ((.22D0*S7)+((1.5D0*(S7*S8))-(.1D0*(S7*C8))))
        VPK(5,10,1)= (3.3D0*WPK(5,10,3))
        VPK(5,10,3)= -(3.3D0*S9)
        VWRI(5,11,1)= (VPK(5,8,1)-(RI(5,2)*WPK(5,8,3)))
        VWRI(5,11,3)= (VPK(5,8,3)+(RI(5,2)*S7))
        VPK(5,11,1)= ((.01D0*WPK(5,8,3))+((VPK(5,8,2)*S11)+(VWRI(5,11,1)
     &   *C11)))
        VPK(5,11,2)= ((VPK(5,8,2)*C11)-(VWRI(5,11,1)*S11))
        VPK(5,11,3)= (VWRI(5,11,3)-(.01D0*WPK(5,11,1)))
        VPK(6,8,1)= (.1D0+((.2D0*S8)-(.22D0*C8)))
        VPK(6,9,1)= (1.2D0*C9)
        VPK(6,9,2)= -(1.2D0*S9)
        VPK(6,10,1)= ((1.2D0*C9)+(3.3D0*C10))
        VPK(6,10,2)= -(1.2D0*(S9*C10))
        VPK(6,10,3)= (1.2D0*(S9*S10))
        VWRI(6,11,1)= (VPK(6,8,1)-(RI(5,2)*C8))
        VPK(6,11,1)= ((.01D0*C8)+(VWRI(6,11,1)*C11))
        VPK(6,11,2)= -(VWRI(6,11,1)*S11)
        VPK(6,11,3)= -(.01D0*WPK(6,11,1))
        VPK(7,8,1)= (.1D0+((.2D0*S8)-(.22D0*C8)))
        VWRI(7,11,1)= (VPK(7,8,1)-(RI(5,2)*C8))
        VPK(7,11,1)= ((.01D0*C8)+(VWRI(7,11,1)*C11))
        VPK(7,11,2)= -(VWRI(7,11,1)*S11)
        VPK(7,11,3)= -(.01D0*WPK(7,11,1))
        VPK(8,8,2)= -.2D0
        VPK(8,8,3)= -.22D0
        VWRI(8,11,3)= -(.22D0+RI(5,2))
        VPK(8,11,1)= -(.2D0*S11)
        VPK(8,11,2)= -(.2D0*C11)
        VPK(8,11,3)= (VWRI(8,11,3)+(.01D0*C11))
        VPK(9,10,1)= (3.3D0*C10)
        VPK(10,10,3)= -3.3D0
        VPK(11,11,1)= .01D0
        VPKFLG= 1
      ENDIF
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   97 adds/subtracts/negates
C                     158 multiplies
C                       0 divides
C                     183 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOLTAU
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute effect of loop hinge torques
C
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       0 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOINER
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute inertial accelerations and related temps
C
      IF (INERFLG .EQ. 0) THEN
C
C Compute Otk (inertial angular acceleration)
C
        OTK(7,1)= (U(7)*WK(7,2))
        OTK(7,2)= -(U(7)*WK(7,1))
        OTK(8,2)= ((OTK(7,2)*C8)-(U(8)*WK(8,3)))
        OTK(8,3)= ((OTK(7,2)*S8)+(U(8)*WK(8,2)))
        OTK(9,1)= (U(9)*WK(9,2))
        OTK(9,2)= -(U(9)*WK(9,1))
        OTK(10,2)= ((OTK(9,2)*C10)+(U(10)*WK(10,3)))
        OTK(10,3)= -((OTK(9,2)*S10)+(U(10)*WK(10,2)))
        OTK(11,1)= ((U(11)*WK(11,2))+((OTK(7,1)*C11)+(OTK(8,2)*S11)))
        OTK(11,2)= (((OTK(8,2)*C11)-(OTK(7,1)*S11))-(U(11)*WK(11,1)))
C
C Compute Atk (inertial linear acceleration)
C
        AIOIWI(7,1)= -(1.5D0*(U(4)*U(6)))
        AIOIWI(7,2)= -(1.5D0*(U(5)*U(6)))
        AIOIWI(7,3)= (1.5D0*((U(4)*U(4))+(U(5)*U(5))))
        ATK(7,1)= ((.75D0*(OTK(7,2)+(WK(7,1)*WK(7,3))))+((AIOIWI(7,1)*C7
     &   )+(AIOIWI(7,2)*S7)))
        ATK(7,2)= ((.75D0*((WK(7,2)*WK(7,3))-OTK(7,1)))+((AIOIWI(7,2)*C7
     &   )-(AIOIWI(7,1)*S7)))
        ATK(7,3)= (AIOIWI(7,3)-(.75D0*((WK(7,1)*WK(7,1))+(WK(7,2)*
     &   WK(7,2)))))
        AIOIWI(8,1)= (ATK(7,1)-((.75D0*OTK(7,2))+((.1D0*(WK(7,1)*WK(7,2)
     &   ))+(.75D0*(WK(7,1)*WK(7,3))))))
        AIOIWI(8,2)= (ATK(7,2)+((.75D0*OTK(7,1))+((.1D0*(WK(7,1)*WK(7,1)
     &   ))+(WIRK(8,1)*WK(7,3)))))
        AIOIWI(8,3)= (ATK(7,3)+(((.75D0*(WK(7,1)*WK(7,1)))-(WIRK(8,1)*
     &   WK(7,2)))-(.1D0*OTK(7,1))))
        ATK(8,1)= (AIOIWI(8,1)+(((.22D0*(WK(8,1)*WK(8,2)))-(.2D0*(
     &   WK(8,1)*WK(8,3))))-((.2D0*OTK(8,2))+(.22D0*OTK(8,3)))))
        ATK(8,2)= (((.2D0*OTK(7,1))+((WK(8,3)*WKRPK(8,1))-(.22D0*(
     &   WK(8,1)*WK(8,1)))))+((AIOIWI(8,2)*C8)-(AIOIWI(8,3)*S8)))
        ATK(8,3)= (((.22D0*OTK(7,1))+((.2D0*(WK(8,1)*WK(8,1)))-(WK(8,2)*
     &   WKRPK(8,1))))+((AIOIWI(8,2)*S8)+(AIOIWI(8,3)*C8)))
        AIOIWI(9,1)= -(1.2D0*(U(4)*U(5)))
        AIOIWI(9,2)= (1.2D0*((U(4)*U(4))+(U(6)*U(6))))
        AIOIWI(9,3)= -(1.2D0*(U(5)*U(6)))
        ATK(9,1)= ((AIOIWI(9,1)*C9)+(AIOIWI(9,2)*S9))
        ATK(9,2)= ((AIOIWI(9,2)*C9)-(AIOIWI(9,1)*S9))
        ATK(10,1)= (ATK(9,1)+(3.3D0*(OTK(10,3)-(WK(10,1)*WK(10,2)))))
        ATK(10,2)= ((3.3D0*((WK(10,1)*WK(10,1))+(WK(10,3)*WK(10,3))))+((
     &   AIOIWI(9,3)*S10)+(ATK(9,2)*C10)))
        ATK(10,3)= (((AIOIWI(9,3)*C10)-(ATK(9,2)*S10))-(3.3D0*(OTK(9,1)+
     &   (WK(10,2)*WK(10,3)))))
        AIOIWI(11,1)= (ATK(8,1)+((WIRK(11,3)*WK(8,2))-(OTK(8,3)*RI(5,2))
     &   ))
        AIOIWI(11,2)= (ATK(8,2)+((WIRK(11,1)*WK(8,3))-(WIRK(11,3)*
     &   WK(8,1))))
        AIOIWI(11,3)= (ATK(8,3)+((OTK(7,1)*RI(5,2))-(WIRK(11,1)*WK(8,2))
     &   ))
        ATK(11,1)= ((.01D0*(OTK(8,3)-(WK(11,1)*WK(11,2))))+((
     &   AIOIWI(11,1)*C11)+(AIOIWI(11,2)*S11)))
        ATK(11,2)= ((.01D0*((WK(11,1)*WK(11,1))+(WK(11,3)*WK(11,3))))+((
     &   AIOIWI(11,2)*C11)-(AIOIWI(11,1)*S11)))
        ATK(11,3)= (AIOIWI(11,3)-(.01D0*(OTK(11,1)+(WK(11,2)*WK(11,3))))
     &   )
        INERFLG= 1
      ENDIF
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   70 adds/subtracts/negates
C                     106 multiplies
C                       0 divides
C                      36 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOFS0
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute effect of all applied loads
C
      IF (FS0FLG .EQ. 0) THEN
        CALL SDDOLTAU
        CALL SDDOINER
C
C Compute Fstar (forces)
C
        FSTAR(7,1)= ((6.8D0*ATK(7,1))-UFK(2,1))
        FSTAR(7,2)= ((6.8D0*ATK(7,2))-UFK(2,2))
        FSTAR(7,3)= ((6.8D0*ATK(7,3))-UFK(2,3))
        FSTAR(8,1)= ((57.5D0*ATK(8,1))-UFK(3,1))
        FSTAR(8,2)= ((57.5D0*ATK(8,2))-UFK(3,2))
        FSTAR(8,3)= ((57.5D0*ATK(8,3))-UFK(3,3))
        FSTAR(10,1)= ((10.7D0*ATK(10,1))-UFK(4,1))
        FSTAR(10,2)= ((10.7D0*ATK(10,2))-UFK(4,2))
        FSTAR(10,3)= ((10.7D0*ATK(10,3))-UFK(4,3))
        FSTAR(11,1)= (ATK(11,1)-UFK(5,1))
        FSTAR(11,2)= (ATK(11,2)-UFK(5,2))
        FSTAR(11,3)= (ATK(11,3)-UFK(5,3))
C
C Compute Tstar (torques)
C
        TSTAR(6,1)= (WKIKWK(6,1)-UTK(1,1))
        TSTAR(6,2)= (WKIKWK(6,2)-UTK(1,2))
        TSTAR(6,3)= (WKIKWK(6,3)-UTK(1,3))
        TSTAR(7,1)= ((WKIKWK(7,1)+(.35D0*OTK(7,1)))-UTK(2,1))
        TSTAR(7,2)= ((WKIKWK(7,2)+(.35D0*OTK(7,2)))-UTK(2,2))
        TSTAR(8,1)= ((WKIKWK(8,1)+(((.41D0*OTK(8,2))+(4.85D0*OTK(7,1)))-
     &   (.07D0*OTK(8,3))))-UTK(3,1))
        TSTAR(8,2)= ((WKIKWK(8,2)+((.54D0*OTK(8,3))+((.41D0*OTK(7,1))+(
     &   2.2D0*OTK(8,2)))))-UTK(3,2))
        TSTAR(8,3)= ((WKIKWK(8,3)+((5.5D0*OTK(8,3))+((.54D0*OTK(8,2))-(
     &   .07D0*OTK(7,1)))))-UTK(3,3))
        TSTAR(10,1)= ((WKIKWK(10,1)+(27.2D0*OTK(9,1)))-UTK(4,1))
        TSTAR(10,2)= ((.2D0*OTK(10,2))-UTK(4,2))
        TSTAR(10,3)= ((WKIKWK(10,3)+(27.2D0*OTK(10,3)))-UTK(4,3))
        TSTAR(11,1)= ((WKIKWK(11,1)+(.1D0*OTK(11,1)))-UTK(5,1))
        TSTAR(11,2)= ((WKIKWK(11,2)+(.1D0*OTK(11,2)))-UTK(5,2))
        TSTAR(11,3)= ((.5D0*OTK(8,3))-UTK(5,3))
C
C Compute fs0 (RHS ignoring constraints)
C
        CALL SDDOVPK
        TEMP(1)= (((FSTAR(10,3)*VPK(1,10,3))+((FSTAR(10,1)*VPK(1,9,1))+(
     &   FSTAR(10,2)*VPK(1,10,2))))+(((FSTAR(8,3)*VPK(1,8,3))+((
     &   FSTAR(8,1)*VPK(1,7,1))+(FSTAR(8,2)*VPK(1,8,2))))+(((CIK(4,1,3)*
     &   FSTAR(7,3))+((FSTAR(7,1)*VPK(1,7,1))+(FSTAR(7,2)*VPK(1,7,2))))-
     &   ((CIK(4,1,3)*UFK(1,3))+((CIK(4,1,1)*UFK(1,1))+(CIK(4,1,2)*
     &   UFK(1,2)))))))
        FS0(1)= (UTAU(1)-(((FSTAR(11,3)*VPK(1,8,3))+((FSTAR(11,1)*
     &   VPK(1,11,1))+(FSTAR(11,2)*VPK(1,11,2))))+TEMP(1)))
        TEMP(1)= (((FSTAR(10,3)*VPK(2,10,3))+((FSTAR(10,1)*VPK(2,9,1))+(
     &   FSTAR(10,2)*VPK(2,10,2))))+(((FSTAR(8,3)*VPK(2,8,3))+((
     &   FSTAR(8,1)*VPK(2,7,1))+(FSTAR(8,2)*VPK(2,8,2))))+(((CIK(4,2,3)*
     &   FSTAR(7,3))+((FSTAR(7,1)*VPK(2,7,1))+(FSTAR(7,2)*VPK(2,7,2))))-
     &   ((CIK(4,2,3)*UFK(1,3))+((CIK(4,2,1)*UFK(1,1))+(CIK(4,2,2)*
     &   UFK(1,2)))))))
        FS0(2)= (UTAU(2)-(((FSTAR(11,3)*VPK(2,8,3))+((FSTAR(11,1)*
     &   VPK(2,11,1))+(FSTAR(11,2)*VPK(2,11,2))))+TEMP(1)))
        TEMP(1)= (((FSTAR(10,3)*VPK(3,10,3))+((FSTAR(10,1)*VPK(3,9,1))+(
     &   FSTAR(10,2)*VPK(3,10,2))))+(((FSTAR(8,3)*VPK(3,8,3))+((
     &   FSTAR(8,1)*VPK(3,7,1))+(FSTAR(8,2)*VPK(3,8,2))))+(((CIK(4,3,3)*
     &   FSTAR(7,3))+((FSTAR(7,1)*VPK(3,7,1))+(FSTAR(7,2)*VPK(3,7,2))))-
     &   ((CIK(4,3,3)*UFK(1,3))+((CIK(4,3,1)*UFK(1,1))+(CIK(4,3,2)*
     &   UFK(1,2)))))))
        FS0(3)= (UTAU(3)-(((FSTAR(11,3)*VPK(3,8,3))+((FSTAR(11,1)*
     &   VPK(3,11,1))+(FSTAR(11,2)*VPK(3,11,2))))+TEMP(1)))
        TEMP(1)= (((TSTAR(6,1)+((.75D0*((FSTAR(7,1)*S7)+(FSTAR(7,2)*C7))
     &   )+((TSTAR(7,1)*C7)-(TSTAR(7,2)*S7))))+(((FSTAR(8,3)*VPK(4,8,3))
     &   +((FSTAR(8,1)*VPK(4,8,1))+(FSTAR(8,2)*VPK(4,8,2))))+((
     &   TSTAR(8,3)*WPK(4,8,3))+((TSTAR(8,1)*C7)+(TSTAR(8,2)*WPK(4,8,2))
     &   ))))+(((FSTAR(10,3)*VPK(4,10,3))+((3.3D0*(FSTAR(10,1)*
     &   WPK(4,10,3)))-(1.2D0*(FSTAR(10,2)*S10))))+((TSTAR(10,3)*
     &   WPK(4,10,3))+((TSTAR(10,1)*C9)+(TSTAR(10,2)*WPK(4,10,2))))))
        FS0(4)= (UTAU(4)-((((FSTAR(11,3)*VPK(4,11,3))+((FSTAR(11,1)*
     &   VPK(4,11,1))+(FSTAR(11,2)*VPK(4,11,2))))+((TSTAR(11,3)*
     &   WPK(4,8,3))+((TSTAR(11,1)*WPK(4,11,1))+(TSTAR(11,2)*WPK(4,11,2)
     &   ))))+TEMP(1)))
        TEMP(1)= (((3.3D0*((FSTAR(10,1)*WPK(5,10,3))-(FSTAR(10,3)*S9)))+
     &   ((TSTAR(10,3)*WPK(5,10,3))+((TSTAR(10,1)*S9)+(TSTAR(10,2)*
     &   WPK(5,10,2)))))+((TSTAR(6,2)+((.75D0*((FSTAR(7,2)*S7)-(
     &   FSTAR(7,1)*C7)))+((TSTAR(7,1)*S7)+(TSTAR(7,2)*C7))))+(((
     &   FSTAR(8,3)*VPK(5,8,3))+((FSTAR(8,1)*VPK(5,8,1))+(FSTAR(8,2)*
     &   VPK(5,8,2))))+((TSTAR(8,3)*WPK(5,8,3))+((TSTAR(8,1)*S7)+(
     &   TSTAR(8,2)*WPK(5,8,2)))))))
        FS0(5)= (UTAU(5)-(TEMP(1)+(((FSTAR(11,3)*VPK(5,11,3))+((
     &   FSTAR(11,1)*VPK(5,11,1))+(FSTAR(11,2)*VPK(5,11,2))))+((
     &   TSTAR(11,3)*WPK(5,8,3))+((TSTAR(11,1)*WPK(5,11,1))+(TSTAR(11,2)
     &   *WPK(5,11,2)))))))
        FS0(6)= (UTAU(6)-((((TSTAR(6,3)-UTK(2,3))+((FSTAR(8,1)*
     &   VPK(6,8,1))+((TSTAR(8,3)*C8)-(TSTAR(8,2)*S8))))+(((FSTAR(10,3)*
     &   VPK(6,10,3))+((FSTAR(10,1)*VPK(6,10,1))+(FSTAR(10,2)*
     &   VPK(6,10,2))))+((TSTAR(10,2)*S10)+(TSTAR(10,3)*C10))))+(((
     &   TSTAR(11,3)*C8)+((TSTAR(11,1)*WPK(6,11,1))+(TSTAR(11,2)*
     &   WPK(6,11,2))))+(((FSTAR(11,1)*VPK(6,11,1))+(FSTAR(11,2)*
     &   VPK(6,11,2)))-(.01D0*(FSTAR(11,3)*WPK(6,11,1)))))))
        FS0(7)= (UTAU(7)-((((FSTAR(8,1)*VPK(7,8,1))+((TSTAR(8,3)*C8)-(
     &   TSTAR(8,2)*S8)))-UTK(2,3))+(((TSTAR(11,3)*C8)+((TSTAR(11,1)*
     &   WPK(7,11,1))+(TSTAR(11,2)*WPK(7,11,2))))+(((FSTAR(11,1)*
     &   VPK(7,11,1))+(FSTAR(11,2)*VPK(7,11,2)))-(.01D0*(FSTAR(11,3)*
     &   WPK(7,11,1)))))))
        FS0(8)= (UTAU(8)-((((FSTAR(11,3)*VPK(8,11,3))-(.2D0*((
     &   FSTAR(11,1)*S11)+(FSTAR(11,2)*C11))))+((TSTAR(11,2)*S11)-(
     &   TSTAR(11,1)*C11)))-(TSTAR(8,1)+((.2D0*FSTAR(8,2))+(.22D0*
     &   FSTAR(8,3))))))
        FS0(9)= (UTAU(9)-((3.3D0*(FSTAR(10,1)*C10))+((TSTAR(10,2)*S10)+(
     &   TSTAR(10,3)*C10))))
        FS0(10)= (UTAU(10)-(TSTAR(10,1)-(3.3D0*FSTAR(10,3))))
        FS0(11)= (UTAU(11)-(TSTAR(11,3)+(.01D0*FSTAR(11,1))))
        FS0FLG= 1
      ENDIF
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  172 adds/subtracts/negates
C                     158 multiplies
C                       0 divides
C                      42 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOMM(ROUTINE)
      INTEGER ROUTINE
C
      INTEGER DUMROUTINE,ERRNUM
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      INTEGER I
C
      IF (MMFLG .EQ. 0) THEN
C
C Compute mass matrix (MM)
C
        CALL SDDOVPK
        IKWPK(4,8,1)= (((.41D0*WPK(4,8,2))+(4.85D0*C7))-(.07D0*
     &   WPK(4,8,3)))
        IKWPK(4,8,2)= ((.54D0*WPK(4,8,3))+((.41D0*C7)+(2.2D0*WPK(4,8,2))
     &   ))
        IKWPK(4,8,3)= ((5.5D0*WPK(4,8,3))+((.54D0*WPK(4,8,2))-(.07D0*C7)
     &   ))
        IKWPK(5,8,1)= (((.41D0*WPK(5,8,2))+(4.85D0*S7))-(.07D0*
     &   WPK(5,8,3)))
        IKWPK(5,8,2)= ((.54D0*WPK(5,8,3))+((.41D0*S7)+(2.2D0*WPK(5,8,2))
     &   ))
        IKWPK(5,8,3)= ((5.5D0*WPK(5,8,3))+((.54D0*WPK(5,8,2))-(.07D0*S7)
     &   ))
        IKWPK(6,8,1)= -((.07D0*C8)+(.41D0*S8))
        IKWPK(6,8,2)= ((.54D0*C8)-(2.2D0*S8))
        IKWPK(6,8,3)= ((5.5D0*C8)-(.54D0*S8))
        IKWPK(7,8,1)= -((.07D0*C8)+(.41D0*S8))
        IKWPK(7,8,2)= ((.54D0*C8)-(2.2D0*S8))
        IKWPK(7,8,3)= ((5.5D0*C8)-(.54D0*S8))
        TEMP(1)= ((57.5D0*((VPK(1,8,3)*VPK(1,8,3))+((VPK(1,7,1)*
     &   VPK(1,7,1))+(VPK(1,8,2)*VPK(1,8,2)))))+((6.8D0*((CIK(4,1,3)*
     &   CIK(4,1,3))+((VPK(1,7,1)*VPK(1,7,1))+(VPK(1,7,2)*VPK(1,7,2)))))
     &   +(410D0*((CIK(4,1,3)*CIK(4,1,3))+((CIK(4,1,1)*CIK(4,1,1))+(
     &   CIK(4,1,2)*CIK(4,1,2)))))))
        MM(1,1)= (((10.7D0*((VPK(1,10,3)*VPK(1,10,3))+((VPK(1,9,1)*
     &   VPK(1,9,1))+(VPK(1,10,2)*VPK(1,10,2)))))+TEMP(1))+((VPK(1,8,3)*
     &   VPK(1,8,3))+((VPK(1,11,1)*VPK(1,11,1))+(VPK(1,11,2)*VPK(1,11,2)
     &   ))))
        TEMP(1)= ((57.5D0*((VPK(1,8,3)*VPK(2,8,3))+((VPK(1,7,1)*
     &   VPK(2,7,1))+(VPK(1,8,2)*VPK(2,8,2)))))+((6.8D0*((CIK(4,1,3)*
     &   CIK(4,2,3))+((VPK(1,7,1)*VPK(2,7,1))+(VPK(1,7,2)*VPK(2,7,2)))))
     &   +(410D0*((CIK(4,1,3)*CIK(4,2,3))+((CIK(4,1,1)*CIK(4,2,1))+(
     &   CIK(4,1,2)*CIK(4,2,2)))))))
        MM(1,2)= (((10.7D0*((VPK(1,10,3)*VPK(2,10,3))+((VPK(1,9,1)*
     &   VPK(2,9,1))+(VPK(1,10,2)*VPK(2,10,2)))))+TEMP(1))+((VPK(1,8,3)*
     &   VPK(2,8,3))+((VPK(1,11,1)*VPK(2,11,1))+(VPK(1,11,2)*VPK(2,11,2)
     &   ))))
        TEMP(1)= ((57.5D0*((VPK(1,8,3)*VPK(3,8,3))+((VPK(1,7,1)*
     &   VPK(3,7,1))+(VPK(1,8,2)*VPK(3,8,2)))))+((6.8D0*((CIK(4,1,3)*
     &   CIK(4,3,3))+((VPK(1,7,1)*VPK(3,7,1))+(VPK(1,7,2)*VPK(3,7,2)))))
     &   +(410D0*((CIK(4,1,3)*CIK(4,3,3))+((CIK(4,1,1)*CIK(4,3,1))+(
     &   CIK(4,1,2)*CIK(4,3,2)))))))
        MM(1,3)= (((10.7D0*((VPK(1,10,3)*VPK(3,10,3))+((VPK(1,9,1)*
     &   VPK(3,9,1))+(VPK(1,10,2)*VPK(3,10,2)))))+TEMP(1))+((VPK(1,8,3)*
     &   VPK(3,8,3))+((VPK(1,11,1)*VPK(3,11,1))+(VPK(1,11,2)*VPK(3,11,2)
     &   ))))
        MM(1,4)= (((10.7D0*((VPK(1,10,3)*VPK(4,10,3))+((3.3D0*(
     &   VPK(1,9,1)*WPK(4,10,3)))-(1.2D0*(VPK(1,10,2)*S10)))))+((5.1D0*(
     &   (VPK(1,7,1)*S7)+(VPK(1,7,2)*C7)))+(57.5D0*((VPK(1,8,3)*
     &   VPK(4,8,3))+((VPK(1,7,1)*VPK(4,8,1))+(VPK(1,8,2)*VPK(4,8,2)))))
     &   ))+((VPK(1,8,3)*VPK(4,11,3))+((VPK(1,11,1)*VPK(4,11,1))+(
     &   VPK(1,11,2)*VPK(4,11,2)))))
        MM(1,5)= (((35.31D0*((VPK(1,9,1)*WPK(5,10,3))-(VPK(1,10,3)*S9)))
     &   +((5.1D0*((VPK(1,7,2)*S7)-(VPK(1,7,1)*C7)))+(57.5D0*((
     &   VPK(1,8,3)*VPK(5,8,3))+((VPK(1,7,1)*VPK(5,8,1))+(VPK(1,8,2)*
     &   VPK(5,8,2)))))))+((VPK(1,8,3)*VPK(5,11,3))+((VPK(1,11,1)*
     &   VPK(5,11,1))+(VPK(1,11,2)*VPK(5,11,2)))))
        MM(1,6)= (((10.7D0*((VPK(1,10,3)*VPK(6,10,3))+((VPK(1,9,1)*
     &   VPK(6,10,1))+(VPK(1,10,2)*VPK(6,10,2)))))+(57.5D0*(VPK(1,7,1)*
     &   VPK(6,8,1))))+(((VPK(1,11,1)*VPK(6,11,1))+(VPK(1,11,2)*
     &   VPK(6,11,2)))-(.01D0*(VPK(1,8,3)*WPK(6,11,1)))))
        MM(1,7)= ((57.5D0*(VPK(1,7,1)*VPK(7,8,1)))+(((VPK(1,11,1)*
     &   VPK(7,11,1))+(VPK(1,11,2)*VPK(7,11,2)))-(.01D0*(VPK(1,8,3)*
     &   WPK(7,11,1)))))
        MM(1,8)= (((VPK(1,8,3)*VPK(8,11,3))-(.2D0*((VPK(1,11,1)*S11)+(
     &   VPK(1,11,2)*C11))))-(57.5D0*((.2D0*VPK(1,8,2))+(.22D0*
     &   VPK(1,8,3)))))
        MM(1,9)= (35.31D0*(VPK(1,9,1)*C10))
        MM(1,10)= -(35.31D0*VPK(1,10,3))
        MM(1,11)= (.01D0*VPK(1,11,1))
        TEMP(1)= ((57.5D0*((VPK(2,8,3)*VPK(2,8,3))+((VPK(2,7,1)*
     &   VPK(2,7,1))+(VPK(2,8,2)*VPK(2,8,2)))))+((6.8D0*((CIK(4,2,3)*
     &   CIK(4,2,3))+((VPK(2,7,1)*VPK(2,7,1))+(VPK(2,7,2)*VPK(2,7,2)))))
     &   +(410D0*((CIK(4,2,3)*CIK(4,2,3))+((CIK(4,2,1)*CIK(4,2,1))+(
     &   CIK(4,2,2)*CIK(4,2,2)))))))
        MM(2,2)= (((10.7D0*((VPK(2,10,3)*VPK(2,10,3))+((VPK(2,9,1)*
     &   VPK(2,9,1))+(VPK(2,10,2)*VPK(2,10,2)))))+TEMP(1))+((VPK(2,8,3)*
     &   VPK(2,8,3))+((VPK(2,11,1)*VPK(2,11,1))+(VPK(2,11,2)*VPK(2,11,2)
     &   ))))
        TEMP(1)= ((57.5D0*((VPK(2,8,3)*VPK(3,8,3))+((VPK(2,7,1)*
     &   VPK(3,7,1))+(VPK(2,8,2)*VPK(3,8,2)))))+((6.8D0*((CIK(4,2,3)*
     &   CIK(4,3,3))+((VPK(2,7,1)*VPK(3,7,1))+(VPK(2,7,2)*VPK(3,7,2)))))
     &   +(410D0*((CIK(4,2,3)*CIK(4,3,3))+((CIK(4,2,1)*CIK(4,3,1))+(
     &   CIK(4,2,2)*CIK(4,3,2)))))))
        MM(2,3)= (((10.7D0*((VPK(2,10,3)*VPK(3,10,3))+((VPK(2,9,1)*
     &   VPK(3,9,1))+(VPK(2,10,2)*VPK(3,10,2)))))+TEMP(1))+((VPK(2,8,3)*
     &   VPK(3,8,3))+((VPK(2,11,1)*VPK(3,11,1))+(VPK(2,11,2)*VPK(3,11,2)
     &   ))))
        MM(2,4)= (((10.7D0*((VPK(2,10,3)*VPK(4,10,3))+((3.3D0*(
     &   VPK(2,9,1)*WPK(4,10,3)))-(1.2D0*(VPK(2,10,2)*S10)))))+((5.1D0*(
     &   (VPK(2,7,1)*S7)+(VPK(2,7,2)*C7)))+(57.5D0*((VPK(2,8,3)*
     &   VPK(4,8,3))+((VPK(2,7,1)*VPK(4,8,1))+(VPK(2,8,2)*VPK(4,8,2)))))
     &   ))+((VPK(2,8,3)*VPK(4,11,3))+((VPK(2,11,1)*VPK(4,11,1))+(
     &   VPK(2,11,2)*VPK(4,11,2)))))
        MM(2,5)= (((35.31D0*((VPK(2,9,1)*WPK(5,10,3))-(VPK(2,10,3)*S9)))
     &   +((5.1D0*((VPK(2,7,2)*S7)-(VPK(2,7,1)*C7)))+(57.5D0*((
     &   VPK(2,8,3)*VPK(5,8,3))+((VPK(2,7,1)*VPK(5,8,1))+(VPK(2,8,2)*
     &   VPK(5,8,2)))))))+((VPK(2,8,3)*VPK(5,11,3))+((VPK(2,11,1)*
     &   VPK(5,11,1))+(VPK(2,11,2)*VPK(5,11,2)))))
        MM(2,6)= (((10.7D0*((VPK(2,10,3)*VPK(6,10,3))+((VPK(2,9,1)*
     &   VPK(6,10,1))+(VPK(2,10,2)*VPK(6,10,2)))))+(57.5D0*(VPK(2,7,1)*
     &   VPK(6,8,1))))+(((VPK(2,11,1)*VPK(6,11,1))+(VPK(2,11,2)*
     &   VPK(6,11,2)))-(.01D0*(VPK(2,8,3)*WPK(6,11,1)))))
        MM(2,7)= ((57.5D0*(VPK(2,7,1)*VPK(7,8,1)))+(((VPK(2,11,1)*
     &   VPK(7,11,1))+(VPK(2,11,2)*VPK(7,11,2)))-(.01D0*(VPK(2,8,3)*
     &   WPK(7,11,1)))))
        MM(2,8)= (((VPK(2,8,3)*VPK(8,11,3))-(.2D0*((VPK(2,11,1)*S11)+(
     &   VPK(2,11,2)*C11))))-(57.5D0*((.2D0*VPK(2,8,2))+(.22D0*
     &   VPK(2,8,3)))))
        MM(2,9)= (35.31D0*(VPK(2,9,1)*C10))
        MM(2,10)= -(35.31D0*VPK(2,10,3))
        MM(2,11)= (.01D0*VPK(2,11,1))
        TEMP(1)= ((57.5D0*((VPK(3,8,3)*VPK(3,8,3))+((VPK(3,7,1)*
     &   VPK(3,7,1))+(VPK(3,8,2)*VPK(3,8,2)))))+((6.8D0*((CIK(4,3,3)*
     &   CIK(4,3,3))+((VPK(3,7,1)*VPK(3,7,1))+(VPK(3,7,2)*VPK(3,7,2)))))
     &   +(410D0*((CIK(4,3,3)*CIK(4,3,3))+((CIK(4,3,1)*CIK(4,3,1))+(
     &   CIK(4,3,2)*CIK(4,3,2)))))))
        MM(3,3)= (((10.7D0*((VPK(3,10,3)*VPK(3,10,3))+((VPK(3,9,1)*
     &   VPK(3,9,1))+(VPK(3,10,2)*VPK(3,10,2)))))+TEMP(1))+((VPK(3,8,3)*
     &   VPK(3,8,3))+((VPK(3,11,1)*VPK(3,11,1))+(VPK(3,11,2)*VPK(3,11,2)
     &   ))))
        MM(3,4)= (((10.7D0*((VPK(3,10,3)*VPK(4,10,3))+((3.3D0*(
     &   VPK(3,9,1)*WPK(4,10,3)))-(1.2D0*(VPK(3,10,2)*S10)))))+((5.1D0*(
     &   (VPK(3,7,1)*S7)+(VPK(3,7,2)*C7)))+(57.5D0*((VPK(3,8,3)*
     &   VPK(4,8,3))+((VPK(3,7,1)*VPK(4,8,1))+(VPK(3,8,2)*VPK(4,8,2)))))
     &   ))+((VPK(3,8,3)*VPK(4,11,3))+((VPK(3,11,1)*VPK(4,11,1))+(
     &   VPK(3,11,2)*VPK(4,11,2)))))
        MM(3,5)= (((35.31D0*((VPK(3,9,1)*WPK(5,10,3))-(VPK(3,10,3)*S9)))
     &   +((5.1D0*((VPK(3,7,2)*S7)-(VPK(3,7,1)*C7)))+(57.5D0*((
     &   VPK(3,8,3)*VPK(5,8,3))+((VPK(3,7,1)*VPK(5,8,1))+(VPK(3,8,2)*
     &   VPK(5,8,2)))))))+((VPK(3,8,3)*VPK(5,11,3))+((VPK(3,11,1)*
     &   VPK(5,11,1))+(VPK(3,11,2)*VPK(5,11,2)))))
        MM(3,6)= (((10.7D0*((VPK(3,10,3)*VPK(6,10,3))+((VPK(3,9,1)*
     &   VPK(6,10,1))+(VPK(3,10,2)*VPK(6,10,2)))))+(57.5D0*(VPK(3,7,1)*
     &   VPK(6,8,1))))+(((VPK(3,11,1)*VPK(6,11,1))+(VPK(3,11,2)*
     &   VPK(6,11,2)))-(.01D0*(VPK(3,8,3)*WPK(6,11,1)))))
        MM(3,7)= ((57.5D0*(VPK(3,7,1)*VPK(7,8,1)))+(((VPK(3,11,1)*
     &   VPK(7,11,1))+(VPK(3,11,2)*VPK(7,11,2)))-(.01D0*(VPK(3,8,3)*
     &   WPK(7,11,1)))))
        MM(3,8)= (((VPK(3,8,3)*VPK(8,11,3))-(.2D0*((VPK(3,11,1)*S11)+(
     &   VPK(3,11,2)*C11))))-(57.5D0*((.2D0*VPK(3,8,2))+(.22D0*
     &   VPK(3,8,3)))))
        MM(3,9)= (35.31D0*(VPK(3,9,1)*C10))
        MM(3,10)= -(35.31D0*VPK(3,10,3))
        MM(3,11)= (.01D0*VPK(3,11,1))
        TEMP(1)= (((10.7D0*((VPK(4,10,3)*VPK(4,10,3))+((1.44D0*(S10*S10)
     &   )+(10.89D0*(WPK(4,10,3)*WPK(4,10,3))))))+((27.2D0*(WPK(4,10,3)*
     &   WPK(4,10,3)))+((.2D0*(WPK(4,10,2)*WPK(4,10,2)))+(27.2D0*(C9*C9)
     &   ))))+((57.5D0*((VPK(4,8,3)*VPK(4,8,3))+((VPK(4,8,1)*VPK(4,8,1))
     &   +(VPK(4,8,2)*VPK(4,8,2)))))+((IKWPK(4,8,3)*WPK(4,8,3))+((
     &   IKWPK(4,8,1)*C7)+(IKWPK(4,8,2)*WPK(4,8,2))))))
        MM(4,4)= (119.175D0+((((.1D0*((WPK(4,11,1)*WPK(4,11,1))+(
     &   WPK(4,11,2)*WPK(4,11,2))))+(.5D0*(WPK(4,8,3)*WPK(4,8,3))))+((
     &   VPK(4,11,3)*VPK(4,11,3))+((VPK(4,11,1)*VPK(4,11,1))+(
     &   VPK(4,11,2)*VPK(4,11,2)))))+TEMP(1)))
        TEMP(1)= (((10.7D0*((10.89D0*(WPK(4,10,3)*WPK(5,10,3)))-(3.3D0*(
     &   VPK(4,10,3)*S9))))+((27.2D0*(WPK(4,10,3)*WPK(5,10,3)))+((.2D0*(
     &   WPK(4,10,2)*WPK(5,10,2)))+(27.2D0*(S9*C9)))))+(((57.5D0*((
     &   VPK(4,8,3)*VPK(5,8,3))+((VPK(4,8,1)*VPK(5,8,1))+(VPK(4,8,2)*
     &   VPK(5,8,2)))))+((IKWPK(5,8,3)*WPK(4,8,3))+((IKWPK(5,8,1)*C7)+(
     &   IKWPK(5,8,2)*WPK(4,8,2)))))-14D0))
        MM(4,5)= ((((.1D0*((WPK(4,11,1)*WPK(5,11,1))+(WPK(4,11,2)*
     &   WPK(5,11,2))))+(.5D0*(WPK(4,8,3)*WPK(5,8,3))))+((VPK(4,11,3)*
     &   VPK(5,11,3))+((VPK(4,11,1)*VPK(5,11,1))+(VPK(4,11,2)*
     &   VPK(5,11,2)))))+TEMP(1))
        TEMP(1)= (((10.7D0*((VPK(4,10,3)*VPK(6,10,3))+((3.3D0*(
     &   VPK(6,10,1)*WPK(4,10,3)))-(1.2D0*(VPK(6,10,2)*S10)))))+((.2D0*(
     &   WPK(4,10,2)*S10))+(27.2D0*(WPK(4,10,3)*C10))))+((57.5D0*(
     &   VPK(4,8,1)*VPK(6,8,1)))+((IKWPK(6,8,3)*WPK(4,8,3))+((
     &   IKWPK(6,8,1)*C7)+(IKWPK(6,8,2)*WPK(4,8,2))))))
        MM(4,6)= (14D0+((((.1D0*((WPK(4,11,1)*WPK(6,11,1))+(WPK(4,11,2)*
     &   WPK(6,11,2))))+(.5D0*(WPK(4,8,3)*C8)))+(((VPK(4,11,1)*
     &   VPK(6,11,1))+(VPK(4,11,2)*VPK(6,11,2)))-(.01D0*(VPK(4,11,3)*
     &   WPK(6,11,1)))))+TEMP(1)))
        MM(4,7)= (((57.5D0*(VPK(4,8,1)*VPK(7,8,1)))+((IKWPK(7,8,3)*
     &   WPK(4,8,3))+((IKWPK(7,8,1)*C7)+(IKWPK(7,8,2)*WPK(4,8,2)))))+(((
     &   .1D0*((WPK(4,11,1)*WPK(7,11,1))+(WPK(4,11,2)*WPK(7,11,2))))+(
     &   .5D0*(WPK(4,8,3)*C8)))+(((VPK(4,11,1)*VPK(7,11,1))+(VPK(4,11,2)
     &   *VPK(7,11,2)))-(.01D0*(VPK(4,11,3)*WPK(7,11,1))))))
        MM(4,8)= (((.1D0*((WPK(4,11,2)*S11)-(WPK(4,11,1)*C11)))+((
     &   VPK(4,11,3)*VPK(8,11,3))-(.2D0*((VPK(4,11,1)*S11)+(VPK(4,11,2)*
     &   C11)))))+(((.07D0*WPK(4,8,3))-((.41D0*WPK(4,8,2))+(4.85D0*C7)))
     &   -(57.5D0*((.2D0*VPK(4,8,2))+(.22D0*VPK(4,8,3))))))
        MM(4,9)= ((116.523D0*(WPK(4,10,3)*C10))+((.2D0*(WPK(4,10,2)*S10)
     &   )+(27.2D0*(WPK(4,10,3)*C10))))
        MM(4,10)= ((27.2D0*C9)-(35.31D0*VPK(4,10,3)))
        MM(4,11)= ((.01D0*VPK(4,11,1))+(.5D0*WPK(4,8,3)))
        TEMP(1)= (((57.5D0*((VPK(5,8,3)*VPK(5,8,3))+((VPK(5,8,1)*
     &   VPK(5,8,1))+(VPK(5,8,2)*VPK(5,8,2)))))+((IKWPK(5,8,3)*
     &   WPK(5,8,3))+((IKWPK(5,8,1)*S7)+(IKWPK(5,8,2)*WPK(5,8,2)))))+((
     &   116.523D0*((WPK(5,10,3)*WPK(5,10,3))+(S9*S9)))+((27.2D0*(
     &   WPK(5,10,3)*WPK(5,10,3)))+((.2D0*(WPK(5,10,2)*WPK(5,10,2)))+(
     &   27.2D0*(S9*S9))))))
        MM(5,5)= (320.175D0+((((.1D0*((WPK(5,11,1)*WPK(5,11,1))+(
     &   WPK(5,11,2)*WPK(5,11,2))))+(.5D0*(WPK(5,8,3)*WPK(5,8,3))))+((
     &   VPK(5,11,3)*VPK(5,11,3))+((VPK(5,11,1)*VPK(5,11,1))+(
     &   VPK(5,11,2)*VPK(5,11,2)))))+TEMP(1)))
        TEMP(1)= (((35.31D0*((VPK(6,10,1)*WPK(5,10,3))-(VPK(6,10,3)*S9))
     &   )+((.2D0*(WPK(5,10,2)*S10))+(27.2D0*(WPK(5,10,3)*C10))))+(((
     &   57.5D0*(VPK(5,8,1)*VPK(6,8,1)))+((IKWPK(6,8,3)*WPK(5,8,3))+((
     &   IKWPK(6,8,1)*S7)+(IKWPK(6,8,2)*WPK(5,8,2)))))-34.6D0))
        MM(5,6)= ((((.1D0*((WPK(5,11,1)*WPK(6,11,1))+(WPK(5,11,2)*
     &   WPK(6,11,2))))+(.5D0*(WPK(5,8,3)*C8)))+(((VPK(5,11,1)*
     &   VPK(6,11,1))+(VPK(5,11,2)*VPK(6,11,2)))-(.01D0*(VPK(5,11,3)*
     &   WPK(6,11,1)))))+TEMP(1))
        MM(5,7)= (((57.5D0*(VPK(5,8,1)*VPK(7,8,1)))+((IKWPK(7,8,3)*
     &   WPK(5,8,3))+((IKWPK(7,8,1)*S7)+(IKWPK(7,8,2)*WPK(5,8,2)))))+(((
     &   .1D0*((WPK(5,11,1)*WPK(7,11,1))+(WPK(5,11,2)*WPK(7,11,2))))+(
     &   .5D0*(WPK(5,8,3)*C8)))+(((VPK(5,11,1)*VPK(7,11,1))+(VPK(5,11,2)
     &   *VPK(7,11,2)))-(.01D0*(VPK(5,11,3)*WPK(7,11,1))))))
        MM(5,8)= (((.1D0*((WPK(5,11,2)*S11)-(WPK(5,11,1)*C11)))+((
     &   VPK(5,11,3)*VPK(8,11,3))-(.2D0*((VPK(5,11,1)*S11)+(VPK(5,11,2)*
     &   C11)))))+(((.07D0*WPK(5,8,3))-((.41D0*WPK(5,8,2))+(4.85D0*S7)))
     &   -(57.5D0*((.2D0*VPK(5,8,2))+(.22D0*VPK(5,8,3))))))
        MM(5,9)= ((116.523D0*(WPK(5,10,3)*C10))+((.2D0*(WPK(5,10,2)*S10)
     &   )+(27.2D0*(WPK(5,10,3)*C10))))
        MM(5,10)= (143.723D0*S9)
        MM(5,11)= ((.01D0*VPK(5,11,1))+(.5D0*WPK(5,8,3)))
        MM(6,6)= (440.2D0+((((.0001D0*(WPK(6,11,1)*WPK(6,11,1)))+((
     &   VPK(6,11,1)*VPK(6,11,1))+(VPK(6,11,2)*VPK(6,11,2))))+((.1D0*((
     &   WPK(6,11,1)*WPK(6,11,1))+(WPK(6,11,2)*WPK(6,11,2))))+(.5D0*(C8*
     &   C8))))+(((10.7D0*((VPK(6,10,3)*VPK(6,10,3))+((VPK(6,10,1)*
     &   VPK(6,10,1))+(VPK(6,10,2)*VPK(6,10,2)))))+(27D0*(C10*C10)))+((
     &   57.5D0*(VPK(6,8,1)*VPK(6,8,1)))+((IKWPK(6,8,3)*C8)-(
     &   IKWPK(6,8,2)*S8))))))
        MM(6,7)= (((57.5D0*(VPK(6,8,1)*VPK(7,8,1)))+((IKWPK(7,8,3)*C8)-(
     &   IKWPK(7,8,2)*S8)))+(((.0001D0*(WPK(6,11,1)*WPK(7,11,1)))+((
     &   VPK(6,11,1)*VPK(7,11,1))+(VPK(6,11,2)*VPK(7,11,2))))+((.1D0*((
     &   WPK(6,11,1)*WPK(7,11,1))+(WPK(6,11,2)*WPK(7,11,2))))+(.5D0*(C8*
     &   C8)))))
        MM(6,8)= (((.07D0*C8)+(.41D0*S8))+((.1D0*((WPK(6,11,2)*S11)-(
     &   WPK(6,11,1)*C11)))-((.01D0*(VPK(8,11,3)*WPK(6,11,1)))+(.2D0*((
     &   VPK(6,11,1)*S11)+(VPK(6,11,2)*C11))))))
        MM(6,9)= (.2D0+((27D0*(C10*C10))+(35.31D0*(VPK(6,10,1)*C10))))
        MM(6,10)= -(35.31D0*VPK(6,10,3))
        MM(6,11)= ((.01D0*VPK(6,11,1))+(.5D0*C8))
        MM(7,7)= (((57.5D0*(VPK(7,8,1)*VPK(7,8,1)))+((IKWPK(7,8,3)*C8)-(
     &   IKWPK(7,8,2)*S8)))+(((.0001D0*(WPK(7,11,1)*WPK(7,11,1)))+((
     &   VPK(7,11,1)*VPK(7,11,1))+(VPK(7,11,2)*VPK(7,11,2))))+((.1D0*((
     &   WPK(7,11,1)*WPK(7,11,1))+(WPK(7,11,2)*WPK(7,11,2))))+(.5D0*(C8*
     &   C8)))))
        MM(7,8)= (((.07D0*C8)+(.41D0*S8))+((.1D0*((WPK(7,11,2)*S11)-(
     &   WPK(7,11,1)*C11)))-((.01D0*(VPK(8,11,3)*WPK(7,11,1)))+(.2D0*((
     &   VPK(7,11,1)*S11)+(VPK(7,11,2)*C11))))))
        MM(7,9)= 0D0
        MM(7,10)= 0D0
        MM(7,11)= ((.01D0*VPK(7,11,1))+(.5D0*C8))
        MM(8,8)= (10.073D0+(VPK(8,11,3)*VPK(8,11,3)))
        MM(8,9)= 0D0
        MM(8,10)= 0D0
        MM(8,11)= -(.002D0*S11)
        MM(9,9)= (.2D0+(143.523D0*(C10*C10)))
        MM(9,10)= 0D0
        MM(9,11)= 0D0
        MM(10,10)= 143.723D0
        MM(10,11)= 0D0
        MM(11,11)= .5001D0
C
C Check for singular mass matrix
C
        DO 100 I=1,11
          IF (MM(I,I) .LT. 1D-13) THEN
            CALL SDSETERR(ROUTINE,47)
          ENDIF
100     CONTINUE
        CALL SDERROR(DUMROUTINE,ERRNUM)
        IF (ERRNUM .EQ. 0) THEN
          MMFLG= 1
        ENDIF
      ENDIF
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  376 adds/subtracts/negates
C                     585 multiplies
C                       0 divides
C                      89 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOMMLDU(ROUTINE)
      INTEGER ROUTINE
C
      INTEGER I
      INTEGER DUMROUTINE,ERRNUM
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
C
      IF (MMLDUFLG .EQ. 0) THEN
        CALL SDDOMM(ROUTINE)
C
C Numerically decompose the mass matrix
C
        CALL SDLDUDCOMP(11,11,MMAP,1D-13,WORKSS,WORKS,MM,MLO,MDI)
C
C Check for singular mass matrix
C
        DO 100 I=1,11
          IF (MDI(I) .EQ. 0D0) THEN
            CALL SDSETERR(ROUTINE,47)
          ENDIF
100     CONTINUE
        CALL SDERROR(DUMROUTINE,ERRNUM)
        IF (ERRNUM .EQ. 0) THEN
          MMLDUFLG= 1
        ENDIF
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDLHS(ROUTINE)
      INTEGER ROUTINE
C
C Compute all remaining state- and force-dependent quantities
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
C
      ROUSTATE= 2
      CALL SDDOMMLDU(ROUTINE)
      CALL SDDOFS0
C
      RETURN
      END
C
      SUBROUTINE SDMFRC(IMULT)
      DOUBLE PRECISION IMULT(1)
C
C
C Calculate forces due to constraint multipliers.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
      MTAU(11)= IMULT(1)
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       1 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDEQUIVHT(TAU)
      DOUBLE PRECISION TAU(11)
C
C Compute tree hinge torques to match effect of applied loads
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION FSTAREQ(11,3),TSTAREQ(11,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(56,23)
        RETURN
      ENDIF
C
C Compute fstareq (forces)
C
C
C Compute tstareq (torques)
C
C
C Compute taus (RHS ignoring constraints and inertial forces)
C
      CALL SDDOVPK
      TEMP(1)= (((UFK(4,3)*VPK(1,10,3))+((UFK(4,1)*VPK(1,9,1))+(UFK(4,2)
     & *VPK(1,10,2))))+(((UFK(3,3)*VPK(1,8,3))+((UFK(3,1)*VPK(1,7,1))+(
     & UFK(3,2)*VPK(1,8,2))))+(((CIK(4,1,3)*UFK(1,3))+((CIK(4,1,1)*
     & UFK(1,1))+(CIK(4,1,2)*UFK(1,2))))+((CIK(4,1,3)*UFK(2,3))+((
     & UFK(2,1)*VPK(1,7,1))+(UFK(2,2)*VPK(1,7,2)))))))
      TAU(1)= (UTAU(1)+(((UFK(5,3)*VPK(1,8,3))+((UFK(5,1)*VPK(1,11,1))+(
     & UFK(5,2)*VPK(1,11,2))))+TEMP(1)))
      TEMP(1)= (((UFK(4,3)*VPK(2,10,3))+((UFK(4,1)*VPK(2,9,1))+(UFK(4,2)
     & *VPK(2,10,2))))+(((UFK(3,3)*VPK(2,8,3))+((UFK(3,1)*VPK(2,7,1))+(
     & UFK(3,2)*VPK(2,8,2))))+(((CIK(4,2,3)*UFK(1,3))+((CIK(4,2,1)*
     & UFK(1,1))+(CIK(4,2,2)*UFK(1,2))))+((CIK(4,2,3)*UFK(2,3))+((
     & UFK(2,1)*VPK(2,7,1))+(UFK(2,2)*VPK(2,7,2)))))))
      TAU(2)= (UTAU(2)+(((UFK(5,3)*VPK(2,8,3))+((UFK(5,1)*VPK(2,11,1))+(
     & UFK(5,2)*VPK(2,11,2))))+TEMP(1)))
      TEMP(1)= (((UFK(4,3)*VPK(3,10,3))+((UFK(4,1)*VPK(3,9,1))+(UFK(4,2)
     & *VPK(3,10,2))))+(((UFK(3,3)*VPK(3,8,3))+((UFK(3,1)*VPK(3,7,1))+(
     & UFK(3,2)*VPK(3,8,2))))+(((CIK(4,3,3)*UFK(1,3))+((CIK(4,3,1)*
     & UFK(1,1))+(CIK(4,3,2)*UFK(1,2))))+((CIK(4,3,3)*UFK(2,3))+((
     & UFK(2,1)*VPK(3,7,1))+(UFK(2,2)*VPK(3,7,2)))))))
      TAU(3)= (UTAU(3)+(((UFK(5,3)*VPK(3,8,3))+((UFK(5,1)*VPK(3,11,1))+(
     & UFK(5,2)*VPK(3,11,2))))+TEMP(1)))
      TEMP(1)= (((((1.2D0*(UFK(4,2)*S10))-(3.3D0*(UFK(4,1)*WPK(4,10,3)))
     & )-(UFK(4,3)*VPK(4,10,3)))-((UTK(4,3)*WPK(4,10,3))+((UTK(4,1)*C9)+
     & (UTK(4,2)*WPK(4,10,2)))))+(((((UTK(2,2)*S7)-(UTK(2,1)*C7))-(.75D0
     & *((UFK(2,1)*S7)+(UFK(2,2)*C7))))-UTK(1,1))-(((UFK(3,3)*VPK(4,8,3)
     & )+((UFK(3,1)*VPK(4,8,1))+(UFK(3,2)*VPK(4,8,2))))+((UTK(3,3)*
     & WPK(4,8,3))+((UTK(3,1)*C7)+(UTK(3,2)*WPK(4,8,2)))))))
      TAU(4)= (UTAU(4)-(TEMP(1)-(((UFK(5,3)*VPK(4,11,3))+((UFK(5,1)*
     & VPK(4,11,1))+(UFK(5,2)*VPK(4,11,2))))+((UTK(5,3)*WPK(4,8,3))+((
     & UTK(5,1)*WPK(4,11,1))+(UTK(5,2)*WPK(4,11,2)))))))
      TEMP(1)= (((3.3D0*((UFK(4,3)*S9)-(UFK(4,1)*WPK(5,10,3))))-((
     & UTK(4,3)*WPK(5,10,3))+((UTK(4,1)*S9)+(UTK(4,2)*WPK(5,10,2)))))+((
     & ((.75D0*((UFK(2,1)*C7)-(UFK(2,2)*S7)))-((UTK(2,1)*S7)+(UTK(2,2)*
     & C7)))-UTK(1,2))-(((UFK(3,3)*VPK(5,8,3))+((UFK(3,1)*VPK(5,8,1))+(
     & UFK(3,2)*VPK(5,8,2))))+((UTK(3,3)*WPK(5,8,3))+((UTK(3,1)*S7)+(
     & UTK(3,2)*WPK(5,8,2)))))))
      TAU(5)= (UTAU(5)-(TEMP(1)-(((UFK(5,3)*VPK(5,11,3))+((UFK(5,1)*
     & VPK(5,11,1))+(UFK(5,2)*VPK(5,11,2))))+((UTK(5,3)*WPK(5,8,3))+((
     & UTK(5,1)*WPK(5,11,1))+(UTK(5,2)*WPK(5,11,2)))))))
      TAU(6)= (UTAU(6)-((((.01D0*(UFK(5,3)*WPK(6,11,1)))-((UFK(5,1)*
     & VPK(6,11,1))+(UFK(5,2)*VPK(6,11,2))))-((UTK(5,3)*C8)+((UTK(5,1)*
     & WPK(6,11,1))+(UTK(5,2)*WPK(6,11,2)))))+(((((UTK(3,2)*S8)-(
     & UTK(3,3)*C8))-(UFK(3,1)*VPK(6,8,1)))-(UTK(1,3)+UTK(2,3)))-(((
     & UFK(4,3)*VPK(6,10,3))+((UFK(4,1)*VPK(6,10,1))+(UFK(4,2)*
     & VPK(6,10,2))))+((UTK(4,2)*S10)+(UTK(4,3)*C10))))))
      TAU(7)= (UTAU(7)-((((.01D0*(UFK(5,3)*WPK(7,11,1)))-((UFK(5,1)*
     & VPK(7,11,1))+(UFK(5,2)*VPK(7,11,2))))-((UTK(5,3)*C8)+((UTK(5,1)*
     & WPK(7,11,1))+(UTK(5,2)*WPK(7,11,2)))))+((((UTK(3,2)*S8)-(UTK(3,3)
     & *C8))-(UFK(3,1)*VPK(7,8,1)))-UTK(2,3))))
      TAU(8)= (UTAU(8)-((UTK(3,1)+((.2D0*UFK(3,2))+(.22D0*UFK(3,3))))+((
     & (.2D0*((UFK(5,1)*S11)+(UFK(5,2)*C11)))-(UFK(5,3)*VPK(8,11,3)))+((
     & UTK(5,1)*C11)-(UTK(5,2)*S11)))))
      TAU(9)= (UTAU(9)+((3.3D0*(UFK(4,1)*C10))+((UTK(4,2)*S10)+(UTK(4,3)
     & *C10))))
      TAU(10)= (UTAU(10)-((3.3D0*UFK(4,3))-UTK(4,1)))
      TAU(11)= (UTAU(11)+(UTK(5,3)+(.01D0*UFK(5,1))))
C
C Op counts below do not include called subroutines
C
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  131 adds/subtracts/negates
C                     132 multiplies
C                       0 divides
C                      16 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFS0
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute Fs (ignoring multiplier forces)
C
      FS(1)= FS0(1)
      FS(2)= FS0(2)
      FS(3)= FS0(3)
      FS(4)= FS0(4)
      FS(5)= FS0(5)
      FS(6)= FS0(6)
      FS(7)= FS0(7)
      FS(8)= FS0(8)
      FS(9)= FS0(9)
      FS(10)= FS0(10)
      FS(11)= FS0(11)
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSMULT
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute Fs (multiplier-generated forces only)
C
      CALL SDDOVPK
      FS(11)= MTAU(11)
      FS(1)= 0D0
      FS(2)= 0D0
      FS(3)= 0D0
      FS(4)= 0D0
      FS(5)= 0D0
      FS(6)= 0D0
      FS(7)= 0D0
      FS(8)= 0D0
      FS(9)= 0D0
      FS(10)= 0D0
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSFULL
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute Fs (including all forces)
C
      CALL SDFSMULT
      FS(1)= (FS(1)+FS0(1))
      FS(2)= (FS(2)+FS0(2))
      FS(3)= (FS(3)+FS0(3))
      FS(4)= (FS(4)+FS0(4))
      FS(5)= (FS(5)+FS0(5))
      FS(6)= (FS(6)+FS0(6))
      FS(7)= (FS(7)+FS0(7))
      FS(8)= (FS(8)+FS0(8))
      FS(9)= (FS(9)+FS0(9))
      FS(10)= (FS(10)+FS0(10))
      FS(11)= (FS(11)+FS0(11))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   11 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSGENMULT
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute Fs (generic multiplier-generated forces)
C
      CALL SDDOVPK
      TEMP(1)= (((MFK(4,3)*VPK(1,10,3))+((MFK(4,1)*VPK(1,9,1))+(MFK(4,2)
     & *VPK(1,10,2))))+(((MFK(3,3)*VPK(1,8,3))+((MFK(3,1)*VPK(1,7,1))+(
     & MFK(3,2)*VPK(1,8,2))))+(((CIK(4,1,3)*MFK(1,3))+((CIK(4,1,1)*
     & MFK(1,1))+(CIK(4,1,2)*MFK(1,2))))+((CIK(4,1,3)*MFK(2,3))+((
     & MFK(2,1)*VPK(1,7,1))+(MFK(2,2)*VPK(1,7,2)))))))
      FS(1)= (MTAU(1)+(((MFK(5,3)*VPK(1,8,3))+((MFK(5,1)*VPK(1,11,1))+(
     & MFK(5,2)*VPK(1,11,2))))+TEMP(1)))
      TEMP(1)= (((MFK(4,3)*VPK(2,10,3))+((MFK(4,1)*VPK(2,9,1))+(MFK(4,2)
     & *VPK(2,10,2))))+(((MFK(3,3)*VPK(2,8,3))+((MFK(3,1)*VPK(2,7,1))+(
     & MFK(3,2)*VPK(2,8,2))))+(((CIK(4,2,3)*MFK(1,3))+((CIK(4,2,1)*
     & MFK(1,1))+(CIK(4,2,2)*MFK(1,2))))+((CIK(4,2,3)*MFK(2,3))+((
     & MFK(2,1)*VPK(2,7,1))+(MFK(2,2)*VPK(2,7,2)))))))
      FS(2)= (MTAU(2)+(((MFK(5,3)*VPK(2,8,3))+((MFK(5,1)*VPK(2,11,1))+(
     & MFK(5,2)*VPK(2,11,2))))+TEMP(1)))
      TEMP(1)= (((MFK(4,3)*VPK(3,10,3))+((MFK(4,1)*VPK(3,9,1))+(MFK(4,2)
     & *VPK(3,10,2))))+(((MFK(3,3)*VPK(3,8,3))+((MFK(3,1)*VPK(3,7,1))+(
     & MFK(3,2)*VPK(3,8,2))))+(((CIK(4,3,3)*MFK(1,3))+((CIK(4,3,1)*
     & MFK(1,1))+(CIK(4,3,2)*MFK(1,2))))+((CIK(4,3,3)*MFK(2,3))+((
     & MFK(2,1)*VPK(3,7,1))+(MFK(2,2)*VPK(3,7,2)))))))
      FS(3)= (MTAU(3)+(((MFK(5,3)*VPK(3,8,3))+((MFK(5,1)*VPK(3,11,1))+(
     & MFK(5,2)*VPK(3,11,2))))+TEMP(1)))
      TEMP(1)= (((((1.2D0*(MFK(4,2)*S10))-(3.3D0*(MFK(4,1)*WPK(4,10,3)))
     & )-(MFK(4,3)*VPK(4,10,3)))-((MTK(4,3)*WPK(4,10,3))+((MTK(4,1)*C9)+
     & (MTK(4,2)*WPK(4,10,2)))))+(((((MTK(2,2)*S7)-(MTK(2,1)*C7))-(.75D0
     & *((MFK(2,1)*S7)+(MFK(2,2)*C7))))-MTK(1,1))-(((MFK(3,3)*VPK(4,8,3)
     & )+((MFK(3,1)*VPK(4,8,1))+(MFK(3,2)*VPK(4,8,2))))+((MTK(3,3)*
     & WPK(4,8,3))+((MTK(3,1)*C7)+(MTK(3,2)*WPK(4,8,2)))))))
      FS(4)= (MTAU(4)-(TEMP(1)-(((MFK(5,3)*VPK(4,11,3))+((MFK(5,1)*
     & VPK(4,11,1))+(MFK(5,2)*VPK(4,11,2))))+((MTK(5,3)*WPK(4,8,3))+((
     & MTK(5,1)*WPK(4,11,1))+(MTK(5,2)*WPK(4,11,2)))))))
      TEMP(1)= (((3.3D0*((MFK(4,3)*S9)-(MFK(4,1)*WPK(5,10,3))))-((
     & MTK(4,3)*WPK(5,10,3))+((MTK(4,1)*S9)+(MTK(4,2)*WPK(5,10,2)))))+((
     & ((.75D0*((MFK(2,1)*C7)-(MFK(2,2)*S7)))-((MTK(2,1)*S7)+(MTK(2,2)*
     & C7)))-MTK(1,2))-(((MFK(3,3)*VPK(5,8,3))+((MFK(3,1)*VPK(5,8,1))+(
     & MFK(3,2)*VPK(5,8,2))))+((MTK(3,3)*WPK(5,8,3))+((MTK(3,1)*S7)+(
     & MTK(3,2)*WPK(5,8,2)))))))
      FS(5)= (MTAU(5)-(TEMP(1)-(((MFK(5,3)*VPK(5,11,3))+((MFK(5,1)*
     & VPK(5,11,1))+(MFK(5,2)*VPK(5,11,2))))+((MTK(5,3)*WPK(5,8,3))+((
     & MTK(5,1)*WPK(5,11,1))+(MTK(5,2)*WPK(5,11,2)))))))
      FS(6)= (MTAU(6)-((((.01D0*(MFK(5,3)*WPK(6,11,1)))-((MFK(5,1)*
     & VPK(6,11,1))+(MFK(5,2)*VPK(6,11,2))))-((MTK(5,3)*C8)+((MTK(5,1)*
     & WPK(6,11,1))+(MTK(5,2)*WPK(6,11,2)))))+(((((MTK(3,2)*S8)-(
     & MTK(3,3)*C8))-(MFK(3,1)*VPK(6,8,1)))-(MTK(1,3)+MTK(2,3)))-(((
     & MFK(4,3)*VPK(6,10,3))+((MFK(4,1)*VPK(6,10,1))+(MFK(4,2)*
     & VPK(6,10,2))))+((MTK(4,2)*S10)+(MTK(4,3)*C10))))))
      FS(7)= (MTAU(7)-((((.01D0*(MFK(5,3)*WPK(7,11,1)))-((MFK(5,1)*
     & VPK(7,11,1))+(MFK(5,2)*VPK(7,11,2))))-((MTK(5,3)*C8)+((MTK(5,1)*
     & WPK(7,11,1))+(MTK(5,2)*WPK(7,11,2)))))+((((MTK(3,2)*S8)-(MTK(3,3)
     & *C8))-(MFK(3,1)*VPK(7,8,1)))-MTK(2,3))))
      FS(8)= (MTAU(8)-((MTK(3,1)+((.2D0*MFK(3,2))+(.22D0*MFK(3,3))))+(((
     & .2D0*((MFK(5,1)*S11)+(MFK(5,2)*C11)))-(MFK(5,3)*VPK(8,11,3)))+((
     & MTK(5,1)*C11)-(MTK(5,2)*S11)))))
      FS(9)= (MTAU(9)+((3.3D0*(MFK(4,1)*C10))+((MTK(4,2)*S10)+(MTK(4,3)*
     & C10))))
      FS(10)= (MTAU(10)-((3.3D0*MFK(4,3))-MTK(4,1)))
      FS(11)= (MTAU(11)+(MTK(5,3)+(.01D0*MFK(5,1))))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  131 adds/subtracts/negates
C                     132 multiplies
C                       0 divides
C                      16 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSGENFULL
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute Fs (incl generic mult & other forces)
C
      CALL SDFSGENMULT
      FS(1)= (FS(1)+FS0(1))
      FS(2)= (FS(2)+FS0(2))
      FS(3)= (FS(3)+FS0(3))
      FS(4)= (FS(4)+FS0(4))
      FS(5)= (FS(5)+FS0(5))
      FS(6)= (FS(6)+FS0(6))
      FS(7)= (FS(7)+FS0(7))
      FS(8)= (FS(8)+FS0(8))
      FS(9)= (FS(9)+FS0(9))
      FS(10)= (FS(10)+FS0(10))
      FS(11)= (FS(11)+FS0(11))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   11 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFULLTRQ(UDOTIN,MULTIN,TRQOUT)
      DOUBLE PRECISION UDOTIN(11),MULTIN(1),TRQOUT(11)
C
C Compute hinge torques which would produce indicated udots
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION FSTARR(11,3),TSTARR(11,3),OTKR(11,3),ATIR(11,3),
     & ATKR(11,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(61,23)
        RETURN
      ENDIF
C
C Compute multiplier-generated forces
C
      CALL SDMFRC(MULTIN)
C
C Account for inertial accelerations and supplied udots
C
      OTKR(7,1)= ((U(7)*WK(7,2))+((UDOTIN(4)*C7)+(UDOTIN(5)*S7)))
      OTKR(7,2)= (((UDOTIN(5)*C7)-(UDOTIN(4)*S7))-(U(7)*WK(7,1)))
      OTKR(7,3)= (UDOTIN(6)+UDOTIN(7))
      OTKR(8,1)= (OTKR(7,1)-UDOTIN(8))
      OTKR(8,2)= (((OTKR(7,2)*C8)-(OTKR(7,3)*S8))-(U(8)*WK(8,3)))
      OTKR(8,3)= ((U(8)*WK(8,2))+((OTKR(7,2)*S8)+(OTKR(7,3)*C8)))
      OTKR(9,1)= ((U(9)*WK(9,2))+((UDOTIN(4)*C9)+(UDOTIN(5)*S9)))
      OTKR(9,2)= (((UDOTIN(5)*C9)-(UDOTIN(4)*S9))-(U(9)*WK(9,1)))
      OTKR(9,3)= (UDOTIN(6)+UDOTIN(9))
      OTKR(10,1)= (OTKR(9,1)+UDOTIN(10))
      OTKR(10,2)= ((U(10)*WK(10,3))+((OTKR(9,2)*C10)+(OTKR(9,3)*S10)))
      OTKR(10,3)= (((OTKR(9,3)*C10)-(OTKR(9,2)*S10))-(U(10)*WK(10,2)))
      OTKR(11,1)= ((U(11)*WK(11,2))+((OTKR(8,1)*C11)+(OTKR(8,2)*S11)))
      OTKR(11,2)= (((OTKR(8,2)*C11)-(OTKR(8,1)*S11))-(U(11)*WK(11,1)))
      OTKR(11,3)= (OTKR(8,3)+UDOTIN(11))
      ATKR(4,1)= ((CIK(4,3,1)*UDOTIN(3))+((CIK(4,1,1)*UDOTIN(1))+(
     & CIK(4,2,1)*UDOTIN(2))))
      ATKR(4,2)= ((CIK(4,3,2)*UDOTIN(3))+((CIK(4,1,2)*UDOTIN(1))+(
     & CIK(4,2,2)*UDOTIN(2))))
      ATKR(4,3)= ((CIK(4,3,3)*UDOTIN(3))+((CIK(4,1,3)*UDOTIN(1))+(
     & CIK(4,2,3)*UDOTIN(2))))
      ATIR(7,1)= (ATKR(4,1)-(1.5D0*(UDOTIN(5)+(U(4)*U(6)))))
      ATIR(7,2)= (ATKR(4,2)+(1.5D0*(UDOTIN(4)-(U(5)*U(6)))))
      ATIR(7,3)= (ATKR(4,3)+(1.5D0*((U(4)*U(4))+(U(5)*U(5)))))
      ATKR(7,1)= ((.75D0*(OTKR(7,2)+(WK(7,1)*WK(7,3))))+((ATIR(7,1)*C7)+
     & (ATIR(7,2)*S7)))
      ATKR(7,2)= ((.75D0*((WK(7,2)*WK(7,3))-OTKR(7,1)))+((ATIR(7,2)*C7)-
     & (ATIR(7,1)*S7)))
      ATKR(7,3)= (ATIR(7,3)-(.75D0*((WK(7,1)*WK(7,1))+(WK(7,2)*WK(7,2)))
     & ))
      ATIR(8,1)= (ATKR(7,1)+(((.1D0*OTKR(7,3))-(.75D0*OTKR(7,2)))-((.1D0
     & *(WK(7,1)*WK(7,2)))+(.75D0*(WK(7,1)*WK(7,3))))))
      ATIR(8,2)= (ATKR(7,2)+((.75D0*OTKR(7,1))+((.1D0*(WK(7,1)*WK(7,1)))
     & +(WIRK(8,1)*WK(7,3)))))
      ATIR(8,3)= (ATKR(7,3)+(((.75D0*(WK(7,1)*WK(7,1)))-(WIRK(8,1)*
     & WK(7,2)))-(.1D0*OTKR(7,1))))
      ATKR(8,1)= (ATIR(8,1)+(((.22D0*(WK(8,1)*WK(8,2)))-(.2D0*(WK(8,1)*
     & WK(8,3))))-((.2D0*OTKR(8,2))+(.22D0*OTKR(8,3)))))
      ATKR(8,2)= (((.2D0*OTKR(8,1))+((WK(8,3)*WKRPK(8,1))-(.22D0*(
     & WK(8,1)*WK(8,1)))))+((ATIR(8,2)*C8)-(ATIR(8,3)*S8)))
      ATKR(8,3)= (((.22D0*OTKR(8,1))+((.2D0*(WK(8,1)*WK(8,1)))-(WK(8,2)*
     & WKRPK(8,1))))+((ATIR(8,2)*S8)+(ATIR(8,3)*C8)))
      ATIR(9,1)= (ATKR(4,1)+(1.2D0*(UDOTIN(6)-(U(4)*U(5)))))
      ATIR(9,2)= (ATKR(4,2)+(1.2D0*((U(4)*U(4))+(U(6)*U(6)))))
      ATIR(9,3)= (ATKR(4,3)-(1.2D0*(UDOTIN(4)+(U(5)*U(6)))))
      ATKR(9,1)= ((ATIR(9,1)*C9)+(ATIR(9,2)*S9))
      ATKR(9,2)= ((ATIR(9,2)*C9)-(ATIR(9,1)*S9))
      ATKR(10,1)= (ATKR(9,1)+(3.3D0*(OTKR(10,3)-(WK(10,1)*WK(10,2)))))
      ATKR(10,2)= ((3.3D0*((WK(10,1)*WK(10,1))+(WK(10,3)*WK(10,3))))+((
     & ATIR(9,3)*S10)+(ATKR(9,2)*C10)))
      ATKR(10,3)= (((ATIR(9,3)*C10)-(ATKR(9,2)*S10))-(3.3D0*(OTKR(10,1)+
     & (WK(10,2)*WK(10,3)))))
      ATIR(11,1)= (ATKR(8,1)+((WIRK(11,3)*WK(8,2))-(OTKR(8,3)*RI(5,2))))
      ATIR(11,2)= (ATKR(8,2)+((WIRK(11,1)*WK(8,3))-(WIRK(11,3)*WK(8,1)))
     & )
      ATIR(11,3)= (ATKR(8,3)+((OTKR(8,1)*RI(5,2))-(WIRK(11,1)*WK(8,2))))
      ATKR(11,1)= ((.01D0*(OTKR(11,3)-(WK(11,1)*WK(11,2))))+((ATIR(11,1)
     & *C11)+(ATIR(11,2)*S11)))
      ATKR(11,2)= ((.01D0*((WK(11,1)*WK(11,1))+(WK(11,3)*WK(11,3))))+((
     & ATIR(11,2)*C11)-(ATIR(11,1)*S11)))
      ATKR(11,3)= (ATIR(11,3)-(.01D0*(OTKR(11,1)+(WK(11,2)*WK(11,3)))))
C
C Accumulate all forces and torques
C
      FSTARR(6,1)= (UFK(1,1)-(410D0*ATKR(4,1)))
      FSTARR(6,2)= (UFK(1,2)-(410D0*ATKR(4,2)))
      FSTARR(6,3)= (UFK(1,3)-(410D0*ATKR(4,3)))
      FSTARR(7,1)= (UFK(2,1)-(6.8D0*ATKR(7,1)))
      FSTARR(7,2)= (UFK(2,2)-(6.8D0*ATKR(7,2)))
      FSTARR(7,3)= (UFK(2,3)-(6.8D0*ATKR(7,3)))
      FSTARR(8,1)= (UFK(3,1)-(57.5D0*ATKR(8,1)))
      FSTARR(8,2)= (UFK(3,2)-(57.5D0*ATKR(8,2)))
      FSTARR(8,3)= (UFK(3,3)-(57.5D0*ATKR(8,3)))
      FSTARR(10,1)= (UFK(4,1)-(10.7D0*ATKR(10,1)))
      FSTARR(10,2)= (UFK(4,2)-(10.7D0*ATKR(10,2)))
      FSTARR(10,3)= (UFK(4,3)-(10.7D0*ATKR(10,3)))
      FSTARR(11,1)= (UFK(5,1)-ATKR(11,1))
      FSTARR(11,2)= (UFK(5,2)-ATKR(11,2))
      FSTARR(11,3)= (UFK(5,3)-ATKR(11,3))
      TSTARR(6,1)= (UTK(1,1)-(WKIKWK(6,1)+((14D0*UDOTIN(6))+((115D0*
     & UDOTIN(4))-(14D0*UDOTIN(5))))))
      TSTARR(6,2)= (UTK(1,2)-(WKIKWK(6,2)+(((316D0*UDOTIN(5))-(14D0*
     & UDOTIN(4)))-(34.6D0*UDOTIN(6)))))
      TSTARR(6,3)= (UTK(1,3)-(WKIKWK(6,3)+((440D0*UDOTIN(6))+((14D0*
     & UDOTIN(4))-(34.6D0*UDOTIN(5))))))
      TSTARR(7,1)= (UTK(2,1)-(WKIKWK(7,1)+(.35D0*OTKR(7,1))))
      TSTARR(7,2)= (UTK(2,2)-(WKIKWK(7,2)+(.35D0*OTKR(7,2))))
      TSTARR(8,1)= (UTK(3,1)-(WKIKWK(8,1)+(((.41D0*OTKR(8,2))+(4.85D0*
     & OTKR(8,1)))-(.07D0*OTKR(8,3)))))
      TSTARR(8,2)= (UTK(3,2)-(WKIKWK(8,2)+((.54D0*OTKR(8,3))+((.41D0*
     & OTKR(8,1))+(2.2D0*OTKR(8,2))))))
      TSTARR(8,3)= (UTK(3,3)-(WKIKWK(8,3)+((5.5D0*OTKR(8,3))+((.54D0*
     & OTKR(8,2))-(.07D0*OTKR(8,1))))))
      TSTARR(10,1)= (UTK(4,1)-(WKIKWK(10,1)+(27.2D0*OTKR(10,1))))
      TSTARR(10,2)= (UTK(4,2)-(.2D0*OTKR(10,2)))
      TSTARR(10,3)= (UTK(4,3)-(WKIKWK(10,3)+(27.2D0*OTKR(10,3))))
      TSTARR(11,1)= (UTK(5,1)-(WKIKWK(11,1)+(.1D0*OTKR(11,1))))
      TSTARR(11,2)= (UTK(5,2)-(WKIKWK(11,2)+(.1D0*OTKR(11,2))))
      TSTARR(11,3)= (UTK(5,3)-(.5D0*OTKR(11,3)))
C
C Now calculate the torques
C
      CALL SDDOVPK
      TEMP(1)= (((FSTARR(10,3)*VPK(1,10,3))+((FSTARR(10,1)*VPK(1,9,1))+(
     & FSTARR(10,2)*VPK(1,10,2))))+(((FSTARR(8,3)*VPK(1,8,3))+((
     & FSTARR(8,1)*VPK(1,7,1))+(FSTARR(8,2)*VPK(1,8,2))))+(((CIK(4,1,3)*
     & FSTARR(6,3))+((CIK(4,1,1)*FSTARR(6,1))+(CIK(4,1,2)*FSTARR(6,2))))
     & +((CIK(4,1,3)*FSTARR(7,3))+((FSTARR(7,1)*VPK(1,7,1))+(FSTARR(7,2)
     & *VPK(1,7,2)))))))
      TRQOUT(1)= -(UTAU(1)+(((FSTARR(11,3)*VPK(1,8,3))+((FSTARR(11,1)*
     & VPK(1,11,1))+(FSTARR(11,2)*VPK(1,11,2))))+TEMP(1)))
      TEMP(1)= (((FSTARR(10,3)*VPK(2,10,3))+((FSTARR(10,1)*VPK(2,9,1))+(
     & FSTARR(10,2)*VPK(2,10,2))))+(((FSTARR(8,3)*VPK(2,8,3))+((
     & FSTARR(8,1)*VPK(2,7,1))+(FSTARR(8,2)*VPK(2,8,2))))+(((CIK(4,2,3)*
     & FSTARR(6,3))+((CIK(4,2,1)*FSTARR(6,1))+(CIK(4,2,2)*FSTARR(6,2))))
     & +((CIK(4,2,3)*FSTARR(7,3))+((FSTARR(7,1)*VPK(2,7,1))+(FSTARR(7,2)
     & *VPK(2,7,2)))))))
      TRQOUT(2)= -(UTAU(2)+(((FSTARR(11,3)*VPK(2,8,3))+((FSTARR(11,1)*
     & VPK(2,11,1))+(FSTARR(11,2)*VPK(2,11,2))))+TEMP(1)))
      TEMP(1)= (((FSTARR(10,3)*VPK(3,10,3))+((FSTARR(10,1)*VPK(3,9,1))+(
     & FSTARR(10,2)*VPK(3,10,2))))+(((FSTARR(8,3)*VPK(3,8,3))+((
     & FSTARR(8,1)*VPK(3,7,1))+(FSTARR(8,2)*VPK(3,8,2))))+(((CIK(4,3,3)*
     & FSTARR(6,3))+((CIK(4,3,1)*FSTARR(6,1))+(CIK(4,3,2)*FSTARR(6,2))))
     & +((CIK(4,3,3)*FSTARR(7,3))+((FSTARR(7,1)*VPK(3,7,1))+(FSTARR(7,2)
     & *VPK(3,7,2)))))))
      TRQOUT(3)= -(UTAU(3)+(((FSTARR(11,3)*VPK(3,8,3))+((FSTARR(11,1)*
     & VPK(3,11,1))+(FSTARR(11,2)*VPK(3,11,2))))+TEMP(1)))
      TEMP(1)= (((TSTARR(6,1)+((.75D0*((FSTARR(7,1)*S7)+(FSTARR(7,2)*C7)
     & ))+((TSTARR(7,1)*C7)-(TSTARR(7,2)*S7))))+(((FSTARR(8,3)*
     & VPK(4,8,3))+((FSTARR(8,1)*VPK(4,8,1))+(FSTARR(8,2)*VPK(4,8,2))))+
     & ((TSTARR(8,3)*WPK(4,8,3))+((TSTARR(8,1)*C7)+(TSTARR(8,2)*
     & WPK(4,8,2))))))+(((FSTARR(10,3)*VPK(4,10,3))+((3.3D0*(
     & FSTARR(10,1)*WPK(4,10,3)))-(1.2D0*(FSTARR(10,2)*S10))))+((
     & TSTARR(10,3)*WPK(4,10,3))+((TSTARR(10,1)*C9)+(TSTARR(10,2)*
     & WPK(4,10,2))))))
      TRQOUT(4)= -(UTAU(4)+((((FSTARR(11,3)*VPK(4,11,3))+((FSTARR(11,1)*
     & VPK(4,11,1))+(FSTARR(11,2)*VPK(4,11,2))))+((TSTARR(11,3)*
     & WPK(4,8,3))+((TSTARR(11,1)*WPK(4,11,1))+(TSTARR(11,2)*WPK(4,11,2)
     & ))))+TEMP(1)))
      TEMP(1)= (((3.3D0*((FSTARR(10,1)*WPK(5,10,3))-(FSTARR(10,3)*S9)))+
     & ((TSTARR(10,3)*WPK(5,10,3))+((TSTARR(10,1)*S9)+(TSTARR(10,2)*
     & WPK(5,10,2)))))+((TSTARR(6,2)+((.75D0*((FSTARR(7,2)*S7)-(
     & FSTARR(7,1)*C7)))+((TSTARR(7,1)*S7)+(TSTARR(7,2)*C7))))+(((
     & FSTARR(8,3)*VPK(5,8,3))+((FSTARR(8,1)*VPK(5,8,1))+(FSTARR(8,2)*
     & VPK(5,8,2))))+((TSTARR(8,3)*WPK(5,8,3))+((TSTARR(8,1)*S7)+(
     & TSTARR(8,2)*WPK(5,8,2)))))))
      TRQOUT(5)= -(UTAU(5)+(TEMP(1)+(((FSTARR(11,3)*VPK(5,11,3))+((
     & FSTARR(11,1)*VPK(5,11,1))+(FSTARR(11,2)*VPK(5,11,2))))+((
     & TSTARR(11,3)*WPK(5,8,3))+((TSTARR(11,1)*WPK(5,11,1))+(
     & TSTARR(11,2)*WPK(5,11,2)))))))
      TRQOUT(6)= -(UTAU(6)+((((TSTARR(6,3)+UTK(2,3))+((FSTARR(8,1)*
     & VPK(6,8,1))+((TSTARR(8,3)*C8)-(TSTARR(8,2)*S8))))+(((FSTARR(10,3)
     & *VPK(6,10,3))+((FSTARR(10,1)*VPK(6,10,1))+(FSTARR(10,2)*
     & VPK(6,10,2))))+((TSTARR(10,2)*S10)+(TSTARR(10,3)*C10))))+(((
     & TSTARR(11,3)*C8)+((TSTARR(11,1)*WPK(6,11,1))+(TSTARR(11,2)*
     & WPK(6,11,2))))+(((FSTARR(11,1)*VPK(6,11,1))+(FSTARR(11,2)*
     & VPK(6,11,2)))-(.01D0*(FSTARR(11,3)*WPK(6,11,1)))))))
      TRQOUT(7)= -(UTAU(7)+((UTK(2,3)+((FSTARR(8,1)*VPK(7,8,1))+((
     & TSTARR(8,3)*C8)-(TSTARR(8,2)*S8))))+(((TSTARR(11,3)*C8)+((
     & TSTARR(11,1)*WPK(7,11,1))+(TSTARR(11,2)*WPK(7,11,2))))+(((
     & FSTARR(11,1)*VPK(7,11,1))+(FSTARR(11,2)*VPK(7,11,2)))-(.01D0*(
     & FSTARR(11,3)*WPK(7,11,1)))))))
      TRQOUT(8)= -(UTAU(8)+((((FSTARR(11,3)*VPK(8,11,3))-(.2D0*((
     & FSTARR(11,1)*S11)+(FSTARR(11,2)*C11))))+((TSTARR(11,2)*S11)-(
     & TSTARR(11,1)*C11)))-(TSTARR(8,1)+((.2D0*FSTARR(8,2))+(.22D0*
     & FSTARR(8,3))))))
      TRQOUT(9)= -(UTAU(9)+((3.3D0*(FSTARR(10,1)*C10))+((TSTARR(10,2)*
     & S10)+(TSTARR(10,3)*C10))))
      TRQOUT(10)= -(UTAU(10)+(TSTARR(10,1)-(3.3D0*FSTARR(10,3))))
      TRQOUT(11)= -((MTAU(11)+UTAU(11))+(TSTARR(11,3)+(.01D0*
     & FSTARR(11,1))))
C
C Op counts below do not include called subroutines
C
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  293 adds/subtracts/negates
C                     298 multiplies
C                       0 divides
C                      89 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDCOMPTRQ(UDOTIN,TRQOUT)
      DOUBLE PRECISION UDOTIN(11),TRQOUT(11)
C
C Compute hinge torques to produce these udots, ignoring constraints
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER I
      DOUBLE PRECISION MULTIN(1)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(60,23)
        RETURN
      ENDIF
      DO 100 I=1,1
        MULTIN(I)= 0D0
100   CONTINUE
      CALL SDFULLTRQ(UDOTIN,MULTIN,TRQOUT)
C
      RETURN
      END
C
      SUBROUTINE SDMULTTRQ(MULTIN,TRQOUT)
      DOUBLE PRECISION MULTIN(1),TRQOUT(11)
C
C Compute hinge trqs which would be produced by these mults.
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(65,23)
        RETURN
      ENDIF
      CALL SDMFRC(MULTIN)
      CALL SDFSMULT
      DO 100 I=1,11
        TRQOUT(I)= FS(I)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDRHS
C
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
C
C Compute hinge torques for tree hinges
C
      TAUC(1)= UTAU(1)
      TAUC(2)= UTAU(2)
      TAUC(3)= UTAU(3)
      TAUC(4)= UTAU(4)
      TAUC(5)= UTAU(5)
      TAUC(6)= UTAU(6)
      TAUC(7)= UTAU(7)
      TAUC(8)= UTAU(8)
      TAUC(9)= UTAU(9)
      TAUC(10)= UTAU(10)
      TAUC(11)= (MTAU(11)+UTAU(11))
      CALL SDDOINER
C
C Compute onk & onb (angular accels in N)
C
      ONKB(7,1)= ((UDOT(4)*C7)+(UDOT(5)*S7))
      ONKB(7,2)= ((UDOT(5)*C7)-(UDOT(4)*S7))
      ONKB(7,3)= (UDOT(6)+UDOT(7))
      ONKB(8,1)= (ONKB(7,1)-UDOT(8))
      ONKB(8,2)= ((ONKB(7,2)*C8)-(ONKB(7,3)*S8))
      ONKB(8,3)= ((ONKB(7,2)*S8)+(ONKB(7,3)*C8))
      ONKB(9,1)= ((UDOT(4)*C9)+(UDOT(5)*S9))
      ONKB(9,2)= ((UDOT(5)*C9)-(UDOT(4)*S9))
      ONKB(9,3)= (UDOT(6)+UDOT(9))
      ONKB(10,1)= (ONKB(9,1)+UDOT(10))
      ONKB(10,2)= ((ONKB(9,2)*C10)+(ONKB(9,3)*S10))
      ONKB(10,3)= ((ONKB(9,3)*C10)-(ONKB(9,2)*S10))
      ONKB(11,1)= ((ONKB(8,1)*C11)+(ONKB(8,2)*S11))
      ONKB(11,2)= ((ONKB(8,2)*C11)-(ONKB(8,1)*S11))
      ONKB(11,3)= (ONKB(8,3)+UDOT(11))
      ONK(7,1)= (ONKB(7,1)+OTK(7,1))
      ONK(7,2)= (ONKB(7,2)+OTK(7,2))
      ONK(8,1)= (ONKB(8,1)+OTK(7,1))
      ONK(8,2)= (ONKB(8,2)+OTK(8,2))
      ONK(8,3)= (ONKB(8,3)+OTK(8,3))
      ONK(10,1)= (ONKB(10,1)+OTK(9,1))
      ONK(10,2)= (ONKB(10,2)+OTK(10,2))
      ONK(10,3)= (ONKB(10,3)+OTK(10,3))
      ONK(11,1)= (ONKB(11,1)+OTK(11,1))
      ONK(11,2)= (ONKB(11,2)+OTK(11,2))
      ONK(11,3)= (ONKB(11,3)+OTK(8,3))
      ONB(1,1)= UDOT(4)
      ONB(1,2)= UDOT(5)
      ONB(1,3)= UDOT(6)
      ONB(2,1)= ONK(7,1)
      ONB(2,2)= ONK(7,2)
      ONB(2,3)= ONKB(7,3)
      ONB(3,1)= ONK(8,1)
      ONB(3,2)= ONK(8,2)
      ONB(3,3)= ONK(8,3)
      ONB(4,1)= ONK(10,1)
      ONB(4,2)= ONK(10,2)
      ONB(4,3)= ONK(10,3)
      ONB(5,1)= ONK(11,1)
      ONB(5,2)= ONK(11,2)
      ONB(5,3)= ONK(11,3)
C
C Compute acceleration dyadics
C
      DYAD(1,1,1)= W11W22(1)
      DYAD(1,1,2)= (W0W1(1)-UDOT(6))
      DYAD(1,1,3)= (UDOT(5)+W0W2(1))
      DYAD(1,2,1)= (UDOT(6)+W0W1(1))
      DYAD(1,2,2)= W00W22(1)
      DYAD(1,2,3)= (W1W2(1)-UDOT(4))
      DYAD(1,3,1)= (W0W2(1)-UDOT(5))
      DYAD(1,3,2)= (UDOT(4)+W1W2(1))
      DYAD(1,3,3)= W00W11(1)
      DYAD(2,1,1)= W11W22(2)
      DYAD(2,1,2)= (W0W1(2)-ONKB(7,3))
      DYAD(2,1,3)= (ONK(7,2)+W0W2(2))
      DYAD(2,2,1)= (ONKB(7,3)+W0W1(2))
      DYAD(2,2,2)= W00W22(2)
      DYAD(2,2,3)= (W1W2(2)-ONK(7,1))
      DYAD(2,3,1)= (W0W2(2)-ONK(7,2))
      DYAD(2,3,2)= (ONK(7,1)+W1W2(2))
      DYAD(2,3,3)= W00W11(2)
      DYAD(3,1,1)= W11W22(3)
      DYAD(3,1,2)= (W0W1(3)-ONK(8,3))
      DYAD(3,1,3)= (ONK(8,2)+W0W2(3))
      DYAD(3,2,1)= (ONK(8,3)+W0W1(3))
      DYAD(3,2,2)= W00W22(3)
      DYAD(3,2,3)= (W1W2(3)-ONK(8,1))
      DYAD(3,3,1)= (W0W2(3)-ONK(8,2))
      DYAD(3,3,2)= (ONK(8,1)+W1W2(3))
      DYAD(3,3,3)= W00W11(3)
      DYAD(4,1,1)= W11W22(4)
      DYAD(4,1,2)= (W0W1(4)-ONK(10,3))
      DYAD(4,1,3)= (ONK(10,2)+W0W2(4))
      DYAD(4,2,1)= (ONK(10,3)+W0W1(4))
      DYAD(4,2,2)= W00W22(4)
      DYAD(4,2,3)= (W1W2(4)-ONK(10,1))
      DYAD(4,3,1)= (W0W2(4)-ONK(10,2))
      DYAD(4,3,2)= (ONK(10,1)+W1W2(4))
      DYAD(4,3,3)= W00W11(4)
      DYAD(5,1,1)= W11W22(5)
      DYAD(5,1,2)= (W0W1(5)-ONK(11,3))
      DYAD(5,1,3)= (ONK(11,2)+W0W2(5))
      DYAD(5,2,1)= (ONK(11,3)+W0W1(5))
      DYAD(5,2,2)= W00W22(5)
      DYAD(5,2,3)= (W1W2(5)-ONK(11,1))
      DYAD(5,3,1)= (W0W2(5)-ONK(11,2))
      DYAD(5,3,2)= (ONK(11,1)+W1W2(5))
      DYAD(5,3,3)= W00W11(5)
C
C Compute ank & anb (mass center linear accels in N)
C
      ANKB(4,1)= ((CIK(4,3,1)*UDOT(3))+((CIK(4,1,1)*UDOT(1))+(CIK(4,2,1)
     & *UDOT(2))))
      ANKB(4,2)= ((CIK(4,3,2)*UDOT(3))+((CIK(4,1,2)*UDOT(1))+(CIK(4,2,2)
     & *UDOT(2))))
      ANKB(4,3)= ((CIK(4,3,3)*UDOT(3))+((CIK(4,1,3)*UDOT(1))+(CIK(4,2,3)
     & *UDOT(2))))
      AONKRI(7,1)= (ANKB(4,1)-(1.5D0*UDOT(5)))
      AONKRI(7,2)= (ANKB(4,2)+(1.5D0*UDOT(4)))
      ANKB(7,1)= ((.75D0*ONKB(7,2))+((AONKRI(7,1)*C7)+(AONKRI(7,2)*S7)))
      ANKB(7,2)= (((AONKRI(7,2)*C7)-(AONKRI(7,1)*S7))-(.75D0*ONKB(7,1)))
      AONKRI(8,1)= (ANKB(7,1)+((.1D0*ONKB(7,3))-(.75D0*ONKB(7,2))))
      AONKRI(8,2)= (ANKB(7,2)+(.75D0*ONKB(7,1)))
      AONKRI(8,3)= (ANKB(4,3)-(.1D0*ONKB(7,1)))
      ANKB(8,1)= (AONKRI(8,1)-((.2D0*ONKB(8,2))+(.22D0*ONKB(8,3))))
      ANKB(8,2)= ((.2D0*ONKB(8,1))+((AONKRI(8,2)*C8)-(AONKRI(8,3)*S8)))
      ANKB(8,3)= ((.22D0*ONKB(8,1))+((AONKRI(8,2)*S8)+(AONKRI(8,3)*C8)))
      AONKRI(9,1)= (ANKB(4,1)+(1.2D0*UDOT(6)))
      AONKRI(9,3)= (ANKB(4,3)-(1.2D0*UDOT(4)))
      ANKB(9,1)= ((ANKB(4,2)*S9)+(AONKRI(9,1)*C9))
      ANKB(9,2)= ((ANKB(4,2)*C9)-(AONKRI(9,1)*S9))
      ANKB(10,1)= (ANKB(9,1)+(3.3D0*ONKB(10,3)))
      ANKB(10,2)= ((ANKB(9,2)*C10)+(AONKRI(9,3)*S10))
      ANKB(10,3)= (((AONKRI(9,3)*C10)-(ANKB(9,2)*S10))-(3.3D0*ONKB(10,1)
     & ))
      AONKRI(11,1)= (ANKB(8,1)-(ONKB(8,3)*RI(5,2)))
      AONKRI(11,3)= (ANKB(8,3)+(ONKB(8,1)*RI(5,2)))
      ANKB(11,1)= ((.01D0*ONKB(11,3))+((ANKB(8,2)*S11)+(AONKRI(11,1)*C11
     & )))
      ANKB(11,2)= ((ANKB(8,2)*C11)-(AONKRI(11,1)*S11))
      ANKB(11,3)= (AONKRI(11,3)-(.01D0*ONKB(11,1)))
      ANK(6,1)= ((ANKB(4,3)*CIK(4,1,3))+((ANKB(4,1)*CIK(4,1,1))+(
     & ANKB(4,2)*CIK(4,1,2))))
      ANK(6,2)= ((ANKB(4,3)*CIK(4,2,3))+((ANKB(4,1)*CIK(4,2,1))+(
     & ANKB(4,2)*CIK(4,2,2))))
      ANK(6,3)= ((ANKB(4,3)*CIK(4,3,3))+((ANKB(4,1)*CIK(4,3,1))+(
     & ANKB(4,2)*CIK(4,3,2))))
      ANKATK(7,1)= (ANKB(7,1)+ATK(7,1))
      ANKATK(7,2)= (ANKB(7,2)+ATK(7,2))
      ANKATK(7,3)= (ANKB(4,3)+ATK(7,3))
      ANK(7,1)= ((ANKATK(7,3)*CIK(4,1,3))+((ANKATK(7,1)*CNK(7,1,1))+(
     & ANKATK(7,2)*CNK(7,1,2))))
      ANK(7,2)= ((ANKATK(7,3)*CIK(4,2,3))+((ANKATK(7,1)*CNK(7,2,1))+(
     & ANKATK(7,2)*CNK(7,2,2))))
      ANK(7,3)= ((ANKATK(7,3)*CIK(4,3,3))+((ANKATK(7,1)*CNK(7,3,1))+(
     & ANKATK(7,2)*CNK(7,3,2))))
      ANKATK(8,1)= (ANKB(8,1)+ATK(8,1))
      ANKATK(8,2)= (ANKB(8,2)+ATK(8,2))
      ANKATK(8,3)= (ANKB(8,3)+ATK(8,3))
      ANK(8,1)= ((ANKATK(8,3)*CNK(8,1,3))+((ANKATK(8,1)*CNK(7,1,1))+(
     & ANKATK(8,2)*CNK(8,1,2))))
      ANK(8,2)= ((ANKATK(8,3)*CNK(8,2,3))+((ANKATK(8,1)*CNK(7,2,1))+(
     & ANKATK(8,2)*CNK(8,2,2))))
      ANK(8,3)= ((ANKATK(8,3)*CNK(8,3,3))+((ANKATK(8,1)*CNK(7,3,1))+(
     & ANKATK(8,2)*CNK(8,3,2))))
      ANKATK(10,1)= (ANKB(10,1)+ATK(10,1))
      ANKATK(10,2)= (ANKB(10,2)+ATK(10,2))
      ANKATK(10,3)= (ANKB(10,3)+ATK(10,3))
      ANK(10,1)= ((ANKATK(10,3)*CNK(10,1,3))+((ANKATK(10,1)*CNK(9,1,1))+
     & (ANKATK(10,2)*CNK(10,1,2))))
      ANK(10,2)= ((ANKATK(10,3)*CNK(10,2,3))+((ANKATK(10,1)*CNK(9,2,1))+
     & (ANKATK(10,2)*CNK(10,2,2))))
      ANK(10,3)= ((ANKATK(10,3)*CNK(10,3,3))+((ANKATK(10,1)*CNK(9,3,1))+
     & (ANKATK(10,2)*CNK(10,3,2))))
      ANKATK(11,1)= (ANKB(11,1)+ATK(11,1))
      ANKATK(11,2)= (ANKB(11,2)+ATK(11,2))
      ANKATK(11,3)= (ANKB(11,3)+ATK(11,3))
      ANK(11,1)= ((ANKATK(11,3)*CNK(8,1,3))+((ANKATK(11,1)*CNK(11,1,1))+
     & (ANKATK(11,2)*CNK(11,1,2))))
      ANK(11,2)= ((ANKATK(11,3)*CNK(8,2,3))+((ANKATK(11,1)*CNK(11,2,1))+
     & (ANKATK(11,2)*CNK(11,2,2))))
      ANK(11,3)= ((ANKATK(11,3)*CNK(8,3,3))+((ANKATK(11,1)*CNK(11,3,1))+
     & (ANKATK(11,2)*CNK(11,3,2))))
      ANB(1,1)= ANK(6,1)
      ANB(1,2)= ANK(6,2)
      ANB(1,3)= ANK(6,3)
      ANB(2,1)= ANK(7,1)
      ANB(2,2)= ANK(7,2)
      ANB(2,3)= ANK(7,3)
      ANB(3,1)= ANK(8,1)
      ANB(3,2)= ANK(8,2)
      ANB(3,3)= ANK(8,3)
      ANB(4,1)= ANK(10,1)
      ANB(4,2)= ANK(10,2)
      ANB(4,3)= ANK(10,3)
      ANB(5,1)= ANK(11,1)
      ANB(5,2)= ANK(11,2)
      ANB(5,3)= ANK(11,3)
C
C Compute constraint acceleration errors
C
      ROUSTATE= 3
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  135 adds/subtracts/negates
C                     114 multiplies
C                       0 divides
C                     164 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDMASSMAT(MMAT)
      DOUBLE PRECISION MMAT(11,11)
C
C Return the system mass matrix (LHS)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER I,J
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(57,23)
        RETURN
      ENDIF
      CALL SDDOMM(57)
      DO 110 I=1,11
        DO 100 J=I,11
          MMAT(I,J)= MM(I,J)
          MMAT(J,I)= MM(I,J)
100     CONTINUE
110   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDFRCMAT(FMAT)
      DOUBLE PRECISION FMAT(11)
C
C Return the system force matrix (RHS), excluding constraints
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(58,23)
        RETURN
      ENDIF
      CALL SDDOFS0
      DO 100 I=1,11
        FMAT(I)= FS0(I)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDPSEUDO(LQOUT,LUOUT)
      DOUBLE PRECISION LQOUT(1),LUOUT(1)
C
C
C Return pseudo-coordinates for loop joints.
C
C
C
C There are no loop joints in this system.
C
C
C
      RETURN
      END
C
      SUBROUTINE SDPSQDOT(LQDOUT)
      DOUBLE PRECISION LQDOUT(1)
C
C
C Return pseudo-coordinate derivatives for loop joints.
C
C
C
C There are no loop joints in this system.
C
C
C
      RETURN
      END
C
      SUBROUTINE SDPSUDOT(LUDOUT)
      DOUBLE PRECISION LUDOUT(1)
C
C
C Return pseudo-coordinate accelerations for loop joints.
C
C
C
C There are no loop joints in this system.
C
C
C
      RETURN
      END
C
      SUBROUTINE SDPERR(ERRS)
      DOUBLE PRECISION ERRS(1)
C
C
C Return position constraint errors.
C
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(26,23)
        RETURN
      ENDIF
      PERR(1)= (Q(11)-UPOS(11))
      ERRS(1)= PERR(1)
C
      RETURN
      END
C
      SUBROUTINE SDVERR(ERRS)
      DOUBLE PRECISION ERRS(1)
C
C
C Return velocity constraint errors.
C
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(27,23)
        RETURN
      ENDIF
      VERR(1)= (U(11)-UVEL(11))
      ERRS(1)= VERR(1)
C
      RETURN
      END
C
      SUBROUTINE SDAERR(ERRS)
      DOUBLE PRECISION ERRS(1)
C
C
C Return acceleration constraint errors.
C
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
C
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(35,24)
        RETURN
      ENDIF
      AERR(1)= (UDOT(11)-UACC(11))
      ERRS(1)= AERR(1)
C
      RETURN
      END
C
      FUNCTION SDCHKBNUM(ROUTINE,BNUM)
      INTEGER ROUTINE,BNUM
      INTEGER SDCHKBNUM
C
C
      IF ((BNUM .LT. 0) .OR. (BNUM .GT. 5)) THEN
        CALL SDSETERR(ROUTINE,15)
        SDCHKBNUM= 1
        RETURN
      ENDIF
      SDCHKBNUM= 0
      RETURN
C
      END
C
      FUNCTION SDCHKJNUM(ROUTINE,JNUM)
      INTEGER ROUTINE,JNUM
      INTEGER SDCHKJNUM
C
C
      IF ((JNUM .LT. 1) .OR. (JNUM .GT. 5)) THEN
        CALL SDSETERR(ROUTINE,16)
        SDCHKJNUM= 1
        RETURN
      ENDIF
      SDCHKJNUM= 0
      RETURN
C
      END
C
      FUNCTION SDCHKUCNUM(ROUTINE,UCNUM)
      INTEGER ROUTINE,UCNUM
      INTEGER SDCHKUCNUM
C
C
      IF ((UCNUM .LT. 1) .OR. (UCNUM .GT. 0)) THEN
        CALL SDSETERR(ROUTINE,21)
        SDCHKUCNUM= 1
        RETURN
      ENDIF
      SDCHKUCNUM= 0
      RETURN
C
      END
C
      FUNCTION SDCHKJAXIS(ROUTINE,JNUM,AXNUM)
      INTEGER ROUTINE,JNUM,AXNUM
      INTEGER SDCHKJAXIS
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      INTEGER MAXAX,SDCHKJNUM
C
      IF (SDCHKJNUM(ROUTINE,JNUM) .NE. 0) THEN
        SDCHKJAXIS= 1
        RETURN
      ENDIF
      IF ((AXNUM .LT. 1) .OR. (AXNUM .GT. 7)) THEN
        CALL SDSETERR(ROUTINE,17)
        SDCHKJAXIS= 1
        RETURN
      ENDIF
      MAXAX= NJNTDOF(JNUM)
      IF ((JTYPE(JNUM) .EQ. 4) .OR. (JTYPE(JNUM) .EQ. 6) .OR. (JTYPE(
     & JNUM) .EQ. 21)) THEN
        MAXAX= MAXAX+1
      ENDIF
      IF (AXNUM .GT. MAXAX) THEN
        CALL SDSETERR(ROUTINE,18)
        SDCHKJAXIS= 1
        RETURN
      ENDIF
      SDCHKJAXIS= 0
      RETURN
C
      END
C
      FUNCTION SDCHKJPIN(ROUTINE,JNUM,PINNO)
      INTEGER ROUTINE,JNUM,PINNO
      INTEGER SDCHKJPIN
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      INTEGER MAXAX,PINOK,SDCHKJNUM
C
      IF (SDCHKJNUM(ROUTINE,JNUM) .NE. 0) THEN
        SDCHKJPIN= 1
        RETURN
      ENDIF
      IF ((PINNO .LT. 1) .OR. (PINNO .GT. 6)) THEN
        CALL SDSETERR(ROUTINE,17)
        SDCHKJPIN= 1
        RETURN
      ENDIF
      IF (NJNTDOF(JNUM) .GE. 3) THEN
        MAXAX= 3
      ELSE
        MAXAX= NJNTDOF(JNUM)
      ENDIF
      IF (JTYPE(JNUM) .EQ. 4) THEN
        MAXAX= 0
      ENDIF
      IF (JTYPE(JNUM) .EQ. 7) THEN
        MAXAX= 1
      ENDIF
      PINOK= 0
      IF (PINNO .LE. MAXAX) THEN
        PINOK= 1
      ENDIF
      IF (PINOK .EQ. 0) THEN
        CALL SDSETERR(ROUTINE,18)
        SDCHKJPIN= 1
        RETURN
      ENDIF
      SDCHKJPIN= 0
      RETURN
C
      END
C
      FUNCTION SDINDX(JOINT,AXIS)
      INTEGER JOINT,AXIS
      INTEGER SDINDX
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      INTEGER OFFS,GOTIT
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(36,JOINT,AXIS) .NE. 0) THEN
        SDINDX= 1
        RETURN
      ENDIF
      GOTIT= 0
      IF (JTYPE(JOINT) .EQ. 4) THEN
        IF (AXIS .EQ. 4) THEN
          OFFS= BALLQ(JOINT)
          GOTIT= 1
        ENDIF
      ELSE
        IF ((JTYPE(JOINT) .EQ. 6) .OR. (JTYPE(JOINT) .EQ. 21)) THEN
          IF (AXIS .EQ. 7) THEN
            OFFS= BALLQ(JOINT)
            GOTIT= 1
          ENDIF
        ENDIF
      ENDIF
      IF (GOTIT .EQ. 0) THEN
        OFFS= FIRSTQ(JOINT)+AXIS-1
      ENDIF
      SDINDX= OFFS
      RETURN
C
      END
C
      SUBROUTINE SDPRESACC(JOINT,AXIS,PRVAL)
      INTEGER JOINT,AXIS
      DOUBLE PRECISION PRVAL
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(13,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(13,23)
        RETURN
      ENDIF
      IF (PRES(SDINDX(JOINT,AXIS))  .NE.  0D0) THEN
        UACC(SDINDX(JOINT,AXIS))= PRVAL
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDPRESVEL(JOINT,AXIS,PRVAL)
      INTEGER JOINT,AXIS
      DOUBLE PRECISION PRVAL
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(14,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(14,23)
        RETURN
      ENDIF
      IF (PRES(SDINDX(JOINT,AXIS))  .NE.  0D0) THEN
        UVEL(SDINDX(JOINT,AXIS))= PRVAL
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDPRESPOS(JOINT,AXIS,PRVAL)
      INTEGER JOINT,AXIS
      DOUBLE PRECISION PRVAL
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(15,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(15,23)
        RETURN
      ENDIF
      IF (PRES(SDINDX(JOINT,AXIS))  .NE.  0D0) THEN
        UPOS(SDINDX(JOINT,AXIS))= PRVAL
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDGETHT(JOINT,AXIS,TORQUE)
      INTEGER JOINT,AXIS
      DOUBLE PRECISION TORQUE
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(30,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(30,24)
        RETURN
      ENDIF
      TORQUE= TAUC(SDINDX(JOINT,AXIS))
C
      RETURN
      END
C
      SUBROUTINE SDHINGET(JOINT,AXIS,TORQUE)
      INTEGER JOINT,AXIS
      DOUBLE PRECISION TORQUE
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(10,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(10,23)
        RETURN
      ENDIF
      IF (MFRCFLG .NE. 0) THEN
        MTAU(SDINDX(JOINT,AXIS))= MTAU(SDINDX(JOINT,AXIS))+TORQUE
      ELSE
        FS0FLG= 0
        UTAU(SDINDX(JOINT,AXIS))= UTAU(SDINDX(JOINT,AXIS))+TORQUE
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDPOINTF(BODY,POINT,FORCE)
      INTEGER BODY
      DOUBLE PRECISION POINT(3),FORCE(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION TORQUE(3)
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(11,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(11,23)
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        RETURN
      ENDIF
      TORQUE(1)= POINT(2)*FORCE(3)-POINT(3)*FORCE(2)
      TORQUE(2)= POINT(3)*FORCE(1)-POINT(1)*FORCE(3)
      TORQUE(3)= POINT(1)*FORCE(2)-POINT(2)*FORCE(1)
      IF (MFRCFLG .NE. 0) THEN
        MFK(BODY,1)= MFK(BODY,1)+FORCE(1)
        MTK(BODY,1)= MTK(BODY,1)+TORQUE(1)
        MFK(BODY,2)= MFK(BODY,2)+FORCE(2)
        MTK(BODY,2)= MTK(BODY,2)+TORQUE(2)
        MFK(BODY,3)= MFK(BODY,3)+FORCE(3)
        MTK(BODY,3)= MTK(BODY,3)+TORQUE(3)
      ELSE
        FS0FLG= 0
        UFK(BODY,1)= UFK(BODY,1)+FORCE(1)
        UTK(BODY,1)= UTK(BODY,1)+TORQUE(1)
        UFK(BODY,2)= UFK(BODY,2)+FORCE(2)
        UTK(BODY,2)= UTK(BODY,2)+TORQUE(2)
        UFK(BODY,3)= UFK(BODY,3)+FORCE(3)
        UTK(BODY,3)= UTK(BODY,3)+TORQUE(3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDBODYT(BODY,TORQUE)
      INTEGER BODY
      DOUBLE PRECISION TORQUE(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(12,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 2) THEN
        CALL SDSETERR(12,23)
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        RETURN
      ENDIF
      IF (MFRCFLG .NE. 0) THEN
        MTK(BODY,1)= MTK(BODY,1)+TORQUE(1)
        MTK(BODY,2)= MTK(BODY,2)+TORQUE(2)
        MTK(BODY,3)= MTK(BODY,3)+TORQUE(3)
      ELSE
        FS0FLG= 0
        UTK(BODY,1)= UTK(BODY,1)+TORQUE(1)
        UTK(BODY,2)= UTK(BODY,2)+TORQUE(2)
        UTK(BODY,3)= UTK(BODY,3)+TORQUE(3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDDOWW(ROUTINE)
      INTEGER ROUTINE
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION PP(1,11),DPP(11,1)
      INTEGER I,J,C
      DOUBLE PRECISION SUM
      DOUBLE PRECISION DFK(5,3),DTK(5,3),DTAU(11),DLTCI(1,3),DLTC(1,3),
     & DLFCI(1,3),DLFC(1,3)
      DOUBLE PRECISION DTINB(1,3),DTOUTB(1,3),DLTAUFI(1,3),DLTAUFO(1,3),
     & DLTAUTI(1,3),DLTAUTO(1,3)
      DOUBLE PRECISION DFS(11),ROW(11),DINVROW(11)
C
      ROUSTATE= 2
      IF (WWFLG .EQ. 0) THEN
C
C Compute constraint effects
C
        CALL SDDOVPK
        CALL SDDOMMLDU(ROUTINE)
C
C Constraint 1 (prescribed motion)
C
        DFS(1)= 0D0
        DFS(2)= 0D0
        DFS(3)= 0D0
        DFS(4)= 0D0
        DFS(5)= 0D0
        DFS(6)= 0D0
        DFS(7)= 0D0
        DFS(8)= 0D0
        DFS(9)= 0D0
        DFS(10)= 0D0
        DFS(11)= 1D0
        CALL SDLDUBSL(11,11,MMAP,MLO,DFS,ROW)
        CALL SDLDUBSD(11,11,MMAP,MDI,ROW,DINVROW)
        DO 100 I=1,11
          PP(1,I)= ROW(I)
          DPP(I,1)= DINVROW(I)
100     CONTINUE
        WMAP(1)= 1
C
C Produce constraint coefficient matrix WW
C
        DO 103 C=1,1
          DO 102 I=C,1
            SUM= 0D0
            DO 101 J=1,11
              SUM= SUM+PP(WMAP(C),J)*DPP(J,WMAP(I))
101         CONTINUE
            WW(WMAP(C),WMAP(I))= SUM
            WW(WMAP(I),WMAP(C))= SUM
102       CONTINUE
103     CONTINUE
C
C Form QR decomposition of WW
C
        CALL SDQRDCOMP(1,1,1,1,WMAP,WMAP,WW,QRAUX,JPVT)
        WWFLG= 1
      ENDIF
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   11 adds/subtracts/negates
C                      11 multiplies
C                       0 divides
C                      47 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDXUDOT0(ROUTINE,OUDOT0)
      INTEGER ROUTINE
      DOUBLE PRECISION OUDOT0(11)
C
C
C Compute unconstrained equations
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      CALL SDLHS(ROUTINE)
C
C Solve equations ignoring constraints
C
      CALL SDFS0
      CALL SDLDUBSLV(11,11,MMAP,WORKS,MLO,MDI,FS,UDOT)
      DO 5 I=1,11
        OUDOT0(I)= UDOT(I)
5     CONTINUE
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDUDOT0(OUDOT0)
      DOUBLE PRECISION OUDOT0(11)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(66,23)
        RETURN
      ENDIF
      CALL SDXUDOT0(66,OUDOT0)
C
      RETURN
      END
C
      SUBROUTINE SDSETUDOT(IUDOT)
      DOUBLE PRECISION IUDOT(11)
C
C
C Assign udots and advance to stage Dynamics Ready
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(68,23)
        RETURN
      ENDIF
      DO 5 I=1,11
        UDOT(I)= IUDOT(I)
5     CONTINUE
      CALL SDRHS
C
      RETURN
      END
C
      SUBROUTINE SDXUDOTM(ROUTINE,IMULT,OUDOTM)
      INTEGER ROUTINE
      DOUBLE PRECISION IMULT(1),OUDOTM(11)
C
C
C Compute udots due only to multipliers
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      CALL SDLHS(ROUTINE)
      CALL SDMFRC(IMULT)
      CALL SDFSMULT
      CALL SDLDUBSLV(11,11,MMAP,WORKS,MLO,MDI,FS,UDOT)
      DO 10 I=1,11
        OUDOTM(I)= UDOT(I)
10    CONTINUE
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                      11 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDUDOTM(IMULT,OUDOTM)
      DOUBLE PRECISION IMULT(1),OUDOTM(11)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(67,23)
        RETURN
      ENDIF
      CALL SDXUDOTM(67,IMULT,OUDOTM)
C
      RETURN
      END
C
      SUBROUTINE SDDERIV(OQDOT,OUDOT)
      DOUBLE PRECISION OQDOT(12),OUDOT(11)
C
C
C This is the derivative section for a 5-body free-flying
C system with 11 degree(s) of freedom, including 6 degrees
C of freedom for attitude and translation of the base body.
C 1 of the degrees of freedom follow(s) prescribed motion.
C There is 1 constraint.
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION WORKR(1),BB(1),B0(1),V0(1),P0(1)
      INTEGER IWORK(1)
      INTEGER I,J
      DOUBLE PRECISION UDOT0(11),UDOT1(11)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(17,23)
        RETURN
      ENDIF
      IF (STABVELQ .EQ. 1) THEN
        CALL SDSETERR(17,32)
      ENDIF
      IF (STABPOSQ .EQ. 1) THEN
        CALL SDSETERR(17,33)
      ENDIF
      WSIZ= 1
C
C Compute unconstrained equations
C
      CALL SDXUDOT0(17,UDOT0)
      CALL SDRHS
      CALL SDAERR(B0)
      IF (STABVEL  .NE.  0D0) THEN
        CALL SDVERR(V0)
      ENDIF
      IF (STABPOS  .NE.  0D0) THEN
        CALL SDPERR(P0)
      ENDIF
C
C Stabilize constraints using Baumgarte's method
C
      DO 10 I=1,1
        BB(I)= -B0(I)
10    CONTINUE
      IF (STABVEL  .NE.  0D0) THEN
        DO 20 I=1,1
          BB(I)= BB(I)-STABVEL*V0(I)
20      CONTINUE
      ENDIF
      IF (STABPOS  .NE.  0D0) THEN
        DO 30 I=1,1
          BB(I)= BB(I)-STABPOS*P0(I)
30      CONTINUE
      ENDIF
C
C Compute and decompose constraint matrix WW
C
      CALL SDDOWW(17)
C
C Numerically solve for constraint multipliers
C
      CALL SDQRBSLV(1,1,1,1,WMAP,WMAP,1D-13,WORKR,IWORK,
     & WW,QRAUX,JPVT,BB,MULT,WRANK)
      DO 80 I=1,1
        MULTMAP(I)= 0
80    CONTINUE
      DO 85 I=1,WRANK
        MULTMAP(JPVT(I))= 1
85    CONTINUE
      J= 1
      DO 90 I=1,1
        IF (MULTMAP(I) .NE. 0) THEN
          MULTMAP(J)= WMAP(I)
          J= J+1
        ENDIF
90    CONTINUE
C
C Compute final udots
C
      CALL SDXUDOTM(17,MULT,UDOT1)
      DO 95 I=1,11
        UDOT(I)= UDOT0(I)+UDOT1(I)
95    CONTINUE
      CALL SDRHS
      DO 100 I=1,12
        OQDOT(I)= QDOT(I)
100   CONTINUE
      DO 110 I=1,11
        OUDOT(I)= UDOT(I)
110   CONTINUE
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   14 adds/subtracts/negates
C                       2 multiplies
C                       0 divides
C                      37 assignments
C
C
      RETURN
      END
C
C Compute residuals for use with DAE integrator
C
C
      SUBROUTINE SDRESID(EQDOT,EUDOT,EMULTS,RESID)
      DOUBLE PRECISION EQDOT(12),EUDOT(11),EMULTS(1),RESID(24)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      INTEGER I
      DOUBLE PRECISION UDERRS(11),P0(1)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(16,23)
        RETURN
      ENDIF
      IF (STABPOSQ .EQ. 1) THEN
        CALL SDSETERR(16,33)
      ENDIF
      CALL SDFULLTRQ(EUDOT,EMULTS,UDERRS)
      DO 100 I=1,12
        RESID(I)= EQDOT(I)-QDOT(I)
100   CONTINUE
      DO 200 I=1,11
        RESID(12+I)= UDERRS(I)
200   CONTINUE
      CALL SDVERR(RESID(24))
      IF (STABPOS  .NE.  0D0) THEN
        CALL SDPERR(P0)
        DO 250 I=1,1
          RESID(23+I)= RESID(23+I)+STABPOS*P0(I)
250     CONTINUE
      ENDIF
      DO 350 I=1,11
        UDOT(I)= EUDOT(I)
350   CONTINUE
      DO 375 I=1,1
        MULT(I)= EMULTS(I)
375   CONTINUE
      CALL SDRHS
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain   13 adds/subtracts/negates
C                       1 multiplies
C                       0 divides
C                      36 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDMULT(OMULTS,OWRANK,OMULTMAP)
      DOUBLE PRECISION OMULTS(1)
      INTEGER OWRANK,OMULTMAP(1)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER I
C
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(34,24)
        RETURN
      ENDIF
      DO 100 I=1,1
        OMULTS(I)= MULT(I)
        IF (I .LE. WRANK) THEN
          OMULTMAP(I)= MULTMAP(I)
        ELSE
          OMULTMAP(I)= 0
        ENDIF
100   CONTINUE
      OWRANK= WRANK
C
      RETURN
      END
C
      SUBROUTINE SDREAC(FORCE,TORQUE)
      DOUBLE PRECISION FORCE(5,3),TORQUE(5,3)
C
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
C
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(31,24)
        RETURN
      ENDIF
C
C Compute reaction forces for non-weld tree joints
C
      FC(11,1)= (ANKATK(11,1)-UFK(5,1))
      FC(11,2)= (ANKATK(11,2)-UFK(5,2))
      FC(11,3)= (ANKATK(11,3)-UFK(5,3))
      TC(11,1)= ((WKIKWK(11,1)+(.1D0*ONK(11,1)))-(UTK(5,1)+(.01D0*
     & FC(11,3))))
      TC(11,2)= ((WKIKWK(11,2)+(.1D0*ONK(11,2)))-UTK(5,2))
      TC(11,3)= ((.5D0*ONK(11,3))-(UTK(5,3)-(.01D0*FC(11,1))))
      FCCIKT(11,1)= ((FC(11,1)*C11)-(FC(11,2)*S11))
      FCCIKT(11,2)= ((FC(11,1)*S11)+(FC(11,2)*C11))
      FCCIKT(11,3)= FC(11,3)
      FFK(8,1)= (UFK(3,1)-FCCIKT(11,1))
      FFK(8,2)= (UFK(3,2)-FCCIKT(11,2))
      FFK(8,3)= (UFK(3,3)-FCCIKT(11,3))
      TTK(8,1)= (UTK(3,1)-((FCCIKT(11,3)*RI(5,2))+((TC(11,1)*C11)-(
     & TC(11,2)*S11))))
      TTK(8,2)= (UTK(3,2)-((TC(11,1)*S11)+(TC(11,2)*C11)))
      TTK(8,3)= (UTK(3,3)-(TC(11,3)-(FCCIKT(11,1)*RI(5,2))))
      FC(10,1)= ((10.7D0*ANKATK(10,1))-UFK(4,1))
      FC(10,2)= ((10.7D0*ANKATK(10,2))-UFK(4,2))
      FC(10,3)= ((10.7D0*ANKATK(10,3))-UFK(4,3))
      TC(10,1)= ((WKIKWK(10,1)+(27.2D0*ONK(10,1)))-(UTK(4,1)+(3.3D0*
     & FC(10,3))))
      TC(10,2)= ((.2D0*ONK(10,2))-UTK(4,2))
      TC(10,3)= ((WKIKWK(10,3)+(27.2D0*ONK(10,3)))-(UTK(4,3)-(3.3D0*
     & FC(10,1))))
      FCCIKT(10,1)= FC(10,1)
      FCCIKT(10,2)= ((FC(10,2)*C10)-(FC(10,3)*S10))
      FCCIKT(10,3)= ((FC(10,2)*S10)+(FC(10,3)*C10))
      FFK(9,1)= -FCCIKT(10,1)
      FFK(9,2)= -FCCIKT(10,2)
      FFK(9,3)= -FCCIKT(10,3)
      TTK(9,1)= -TC(10,1)
      TTK(9,2)= -((TC(10,2)*C10)-(TC(10,3)*S10))
      TTK(9,3)= -((TC(10,2)*S10)+(TC(10,3)*C10))
      FC(9,1)= -FFK(9,1)
      FC(9,2)= -FFK(9,2)
      FC(9,3)= -FFK(9,3)
      TC(9,1)= -TTK(9,1)
      TC(9,2)= -TTK(9,2)
      TC(9,3)= -TTK(9,3)
      FCCIKT(9,1)= ((FC(9,1)*C9)-(FC(9,2)*S9))
      FCCIKT(9,2)= ((FC(9,1)*S9)+(FC(9,2)*C9))
      FCCIKT(9,3)= FC(9,3)
      FFK(6,1)= (UFK(1,1)-FCCIKT(9,1))
      FFK(6,2)= (UFK(1,2)-FCCIKT(9,2))
      FFK(6,3)= (UFK(1,3)-FCCIKT(9,3))
      TTK(6,1)= (UTK(1,1)-(((TC(9,1)*C9)-(TC(9,2)*S9))-(1.2D0*
     & FCCIKT(9,3))))
      TTK(6,2)= (UTK(1,2)-((TC(9,1)*S9)+(TC(9,2)*C9)))
      TTK(6,3)= (UTK(1,3)-(TC(9,3)+(1.2D0*FCCIKT(9,1))))
      FC(8,1)= ((57.5D0*ANKATK(8,1))-FFK(8,1))
      FC(8,2)= ((57.5D0*ANKATK(8,2))-FFK(8,2))
      FC(8,3)= ((57.5D0*ANKATK(8,3))-FFK(8,3))
      TC(8,1)= ((WKIKWK(8,1)+(((.41D0*ONK(8,2))+(4.85D0*ONK(8,1)))-(
     & .07D0*ONK(8,3))))-(TTK(8,1)-((.2D0*FC(8,2))+(.22D0*FC(8,3)))))
      TC(8,2)= ((WKIKWK(8,2)+((.54D0*ONK(8,3))+((.41D0*ONK(8,1))+(2.2D0*
     & ONK(8,2)))))-(TTK(8,2)+(.2D0*FC(8,1))))
      TC(8,3)= ((WKIKWK(8,3)+((5.5D0*ONK(8,3))+((.54D0*ONK(8,2))-(.07D0*
     & ONK(8,1)))))-(TTK(8,3)+(.22D0*FC(8,1))))
      FCCIKT(8,1)= FC(8,1)
      FCCIKT(8,2)= ((FC(8,2)*C8)+(FC(8,3)*S8))
      FCCIKT(8,3)= ((FC(8,3)*C8)-(FC(8,2)*S8))
      FFK(7,1)= (UFK(2,1)-FCCIKT(8,1))
      FFK(7,2)= (UFK(2,2)-FCCIKT(8,2))
      FFK(7,3)= (UFK(2,3)-FCCIKT(8,3))
      TTK(7,1)= (UTK(2,1)-(TC(8,1)+((.75D0*FCCIKT(8,2))-(.1D0*
     & FCCIKT(8,3)))))
      TTK(7,2)= (UTK(2,2)-(((TC(8,2)*C8)+(TC(8,3)*S8))-(.75D0*
     & FCCIKT(8,1))))
      TTK(7,3)= (UTK(2,3)-((.1D0*FCCIKT(8,1))+((TC(8,3)*C8)-(TC(8,2)*S8)
     & )))
      FC(7,1)= ((6.8D0*ANKATK(7,1))-FFK(7,1))
      FC(7,2)= ((6.8D0*ANKATK(7,2))-FFK(7,2))
      FC(7,3)= ((6.8D0*ANKATK(7,3))-FFK(7,3))
      TC(7,1)= ((WKIKWK(7,1)+(.35D0*ONK(7,1)))-(TTK(7,1)+(.75D0*FC(7,2))
     & ))
      TC(7,2)= ((WKIKWK(7,2)+(.35D0*ONK(7,2)))-(TTK(7,2)-(.75D0*FC(7,1))
     & ))
      TC(7,3)= -TTK(7,3)
      FCCIKT(7,1)= ((FC(7,1)*C7)-(FC(7,2)*S7))
      FCCIKT(7,2)= ((FC(7,1)*S7)+(FC(7,2)*C7))
      FCCIKT(7,3)= FC(7,3)
      FFK(6,1)= (FFK(6,1)-FCCIKT(7,1))
      FFK(6,2)= (FFK(6,2)-FCCIKT(7,2))
      FFK(6,3)= (FFK(6,3)-FCCIKT(7,3))
      TTK(6,1)= (TTK(6,1)-((1.5D0*FCCIKT(7,2))+((TC(7,1)*C7)-(TC(7,2)*S7
     & ))))
      TTK(6,2)= (TTK(6,2)-(((TC(7,1)*S7)+(TC(7,2)*C7))-(1.5D0*
     & FCCIKT(7,1))))
      TTK(6,3)= (TTK(6,3)-TC(7,3))
      FC(6,1)= ((410D0*ANKB(4,1))-FFK(6,1))
      FC(6,2)= ((410D0*ANKB(4,2))-FFK(6,2))
      FC(6,3)= ((410D0*ANKB(4,3))-FFK(6,3))
      TC(6,1)= ((WKIKWK(6,1)+((14D0*UDOT(6))+((115D0*UDOT(4))-(14D0*
     & UDOT(5)))))-TTK(6,1))
      TC(6,2)= ((WKIKWK(6,2)+(((316D0*UDOT(5))-(14D0*UDOT(4)))-(34.6D0*
     & UDOT(6))))-TTK(6,2))
      TC(6,3)= ((WKIKWK(6,3)+((440D0*UDOT(6))+((14D0*UDOT(4))-(34.6D0*
     & UDOT(5)))))-TTK(6,3))
      FCCIKT(6,1)= FC(6,1)
      FCCIKT(6,2)= FC(6,2)
      FCCIKT(6,3)= FC(6,3)
      FFK(5,1)= -FCCIKT(6,1)
      FFK(5,2)= -FCCIKT(6,2)
      FFK(5,3)= -FCCIKT(6,3)
      TTK(5,1)= -TC(6,1)
      TTK(5,2)= -TC(6,2)
      TTK(5,3)= -TC(6,3)
      FC(5,1)= -FFK(5,1)
      FC(5,2)= -FFK(5,2)
      FC(5,3)= -FFK(5,3)
      TC(5,1)= -TTK(5,1)
      TC(5,2)= -TTK(5,2)
      TC(5,3)= -TTK(5,3)
      FCCIKT(5,1)= FC(5,1)
      FCCIKT(5,2)= FC(5,2)
      FCCIKT(5,3)= FC(5,3)
      FFK(4,1)= -FCCIKT(5,1)
      FFK(4,2)= -FCCIKT(5,2)
      FFK(4,3)= -FCCIKT(5,3)
      TTK(4,1)= -TC(5,1)
      TTK(4,2)= -TC(5,2)
      TTK(4,3)= -TC(5,3)
      FC(4,1)= -FFK(4,1)
      FC(4,2)= -FFK(4,2)
      FC(4,3)= -FFK(4,3)
      TC(4,1)= -TTK(4,1)
      TC(4,2)= -TTK(4,2)
      TC(4,3)= -TTK(4,3)
      FCCIKT(4,1)= ((CIK(4,1,3)*FC(4,3))+((CIK(4,1,1)*FC(4,1))+(
     & CIK(4,1,2)*FC(4,2))))
      FCCIKT(4,2)= ((CIK(4,2,3)*FC(4,3))+((CIK(4,2,1)*FC(4,1))+(
     & CIK(4,2,2)*FC(4,2))))
      FCCIKT(4,3)= ((CIK(4,3,3)*FC(4,3))+((CIK(4,3,1)*FC(4,1))+(
     & CIK(4,3,2)*FC(4,2))))
      FFK(3,1)= -FCCIKT(4,1)
      FFK(3,2)= -FCCIKT(4,2)
      FFK(3,3)= -FCCIKT(4,3)
      TTK(3,1)= -((CIK(4,1,3)*TC(4,3))+((CIK(4,1,1)*TC(4,1))+(CIK(4,1,2)
     & *TC(4,2))))
      TTK(3,2)= -((CIK(4,2,3)*TC(4,3))+((CIK(4,2,1)*TC(4,1))+(CIK(4,2,2)
     & *TC(4,2))))
      TTK(3,3)= -((CIK(4,3,3)*TC(4,3))+((CIK(4,3,1)*TC(4,1))+(CIK(4,3,2)
     & *TC(4,2))))
      FC(3,1)= -FFK(3,1)
      FC(3,2)= -FFK(3,2)
      FC(3,3)= -FFK(3,3)
      TC(3,1)= -TTK(3,1)
      TC(3,2)= -TTK(3,2)
      TC(3,3)= -TTK(3,3)
      FCCIKT(3,1)= FC(3,1)
      FCCIKT(3,2)= FC(3,2)
      FCCIKT(3,3)= FC(3,3)
      FFK(2,1)= -FCCIKT(3,1)
      FFK(2,2)= -FCCIKT(3,2)
      FFK(2,3)= -FCCIKT(3,3)
      TTK(2,1)= -(TC(3,1)-(FCCIKT(3,2)*Q(3)))
      TTK(2,2)= -(TC(3,2)+(FCCIKT(3,1)*Q(3)))
      TTK(2,3)= -TC(3,3)
      FC(2,1)= -FFK(2,1)
      FC(2,2)= -FFK(2,2)
      FC(2,3)= -FFK(2,3)
      TC(2,1)= -TTK(2,1)
      TC(2,2)= -TTK(2,2)
      TC(2,3)= -TTK(2,3)
      FCCIKT(2,1)= FC(2,1)
      FCCIKT(2,2)= FC(2,2)
      FCCIKT(2,3)= FC(2,3)
      FFK(1,1)= -FCCIKT(2,1)
      FFK(1,2)= -FCCIKT(2,2)
      FFK(1,3)= -FCCIKT(2,3)
      TTK(1,1)= -(TC(2,1)+(FCCIKT(2,3)*Q(2)))
      TTK(1,2)= -TC(2,2)
      TTK(1,3)= -(TC(2,3)-(FCCIKT(2,1)*Q(2)))
      FC(1,1)= -FFK(1,1)
      FC(1,2)= -FFK(1,2)
      FC(1,3)= -FFK(1,3)
      TC(1,1)= -TTK(1,1)
      TC(1,2)= -TTK(1,2)
      TC(1,3)= -TTK(1,3)
      FORCE(1,1)= FC(6,1)
      TORQUE(1,1)= TC(6,1)
      FORCE(1,2)= FC(6,2)
      TORQUE(1,2)= TC(6,2)
      FORCE(1,3)= FC(6,3)
      TORQUE(1,3)= TC(6,3)
      FORCE(2,1)= FC(7,1)
      TORQUE(2,1)= TC(7,1)
      FORCE(2,2)= FC(7,2)
      TORQUE(2,2)= TC(7,2)
      FORCE(2,3)= FC(7,3)
      TORQUE(2,3)= TC(7,3)
      FORCE(3,1)= FC(8,1)
      TORQUE(3,1)= TC(8,1)
      FORCE(3,2)= FC(8,2)
      TORQUE(3,2)= TC(8,2)
      FORCE(3,3)= FC(8,3)
      TORQUE(3,3)= TC(8,3)
      FORCE(4,1)= FC(10,1)
      TORQUE(4,1)= TC(10,1)
      FORCE(4,2)= FC(10,2)
      TORQUE(4,2)= TC(10,2)
      FORCE(4,3)= FC(10,3)
      TORQUE(4,3)= TC(10,3)
      FORCE(5,1)= FC(11,1)
      TORQUE(5,1)= TC(11,1)
      FORCE(5,2)= FC(11,2)
      TORQUE(5,2)= TC(11,2)
      FORCE(5,3)= FC(11,3)
      TORQUE(5,3)= TC(11,3)
C
C Compute reaction forces for tree weld joints
C
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  206 adds/subtracts/negates
C                     120 multiplies
C                       0 divides
C                     186 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDMOM(LM,AM,KE)
      DOUBLE PRECISION LM(3),AM(3),KE
C
C
C Compute system linear and angular momentum, and kinetic energy.
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION LK(5,3),HNK(5,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(19,23)
        RETURN
      ENDIF
      HNK(1,1)= ((14D0*U(6))+((115D0*U(4))-(14D0*U(5))))
      HNK(1,2)= (((316D0*U(5))-(14D0*U(4)))-(34.6D0*U(6)))
      HNK(1,3)= ((440D0*U(6))+((14D0*U(4))-(34.6D0*U(5))))
      HNK(3,1)= (((.41D0*WK(8,2))+(4.85D0*WK(8,1)))-(.07D0*WK(8,3)))
      HNK(3,2)= ((.54D0*WK(8,3))+((.41D0*WK(8,1))+(2.2D0*WK(8,2))))
      HNK(3,3)= ((5.5D0*WK(8,3))+((.54D0*WK(8,2))-(.07D0*WK(8,1))))
      LM(1)= (VNK(11,1)+((10.7D0*VNK(10,1))+((57.5D0*VNK(8,1))+((6.8D0*
     & VNK(7,1))+(410D0*U(1))))))
      LM(2)= (VNK(11,2)+((10.7D0*VNK(10,2))+((57.5D0*VNK(8,2))+((6.8D0*
     & VNK(7,2))+(410D0*U(2))))))
      LM(3)= (VNK(11,3)+((10.7D0*VNK(10,3))+((57.5D0*VNK(8,3))+((6.8D0*
     & VNK(7,3))+(410D0*U(3))))))
      TEMP(1)= (((.35D0*((CNK(7,1,1)*WK(7,1))+(CNK(7,1,2)*WK(7,2))))+(
     & 6.8D0*((VNK(7,3)*(RNK(7,2)-COM(2)))-(VNK(7,2)*(RNK(7,3)-COM(3))))
     & ))+((410D0*((U(3)*(Q(2)-COM(2)))-(U(2)*(Q(3)-COM(3)))))+((
     & CIK(4,1,3)*HNK(1,3))+((CIK(4,1,1)*HNK(1,1))+(CIK(4,1,2)*HNK(1,2))
     & ))))
      TEMP(2)= (((10.7D0*((VNK(10,3)*(RNK(10,2)-COM(2)))-(VNK(10,2)*(
     & RNK(10,3)-COM(3)))))+((27.2D0*(CNK(10,1,3)*WK(10,3)))+((.2D0*(
     & CNK(10,1,2)*WK(10,2)))+(27.2D0*(CNK(9,1,1)*WK(10,1))))))+(((
     & 57.5D0*((VNK(8,3)*(RNK(8,2)-COM(2)))-(VNK(8,2)*(RNK(8,3)-COM(3)))
     & ))+((CNK(8,1,3)*HNK(3,3))+((CNK(7,1,1)*HNK(3,1))+(CNK(8,1,2)*
     & HNK(3,2)))))+TEMP(1)))
      AM(1)= ((((.1D0*((CNK(11,1,1)*WK(11,1))+(CNK(11,1,2)*WK(11,2))))+(
     & .5D0*(CNK(8,1,3)*WK(11,3))))+((VNK(11,3)*(RNK(11,2)-COM(2)))-(
     & VNK(11,2)*(RNK(11,3)-COM(3)))))+TEMP(2))
      TEMP(1)= (((.35D0*((CNK(7,2,1)*WK(7,1))+(CNK(7,2,2)*WK(7,2))))+(
     & 6.8D0*((VNK(7,1)*(RNK(7,3)-COM(3)))-(VNK(7,3)*(RNK(7,1)-COM(1))))
     & ))+((410D0*((U(1)*(Q(3)-COM(3)))-(U(3)*(Q(1)-COM(1)))))+((
     & CIK(4,2,3)*HNK(1,3))+((CIK(4,2,1)*HNK(1,1))+(CIK(4,2,2)*HNK(1,2))
     & ))))
      TEMP(2)= (((10.7D0*((VNK(10,1)*(RNK(10,3)-COM(3)))-(VNK(10,3)*(
     & RNK(10,1)-COM(1)))))+((27.2D0*(CNK(10,2,3)*WK(10,3)))+((.2D0*(
     & CNK(10,2,2)*WK(10,2)))+(27.2D0*(CNK(9,2,1)*WK(10,1))))))+(((
     & 57.5D0*((VNK(8,1)*(RNK(8,3)-COM(3)))-(VNK(8,3)*(RNK(8,1)-COM(1)))
     & ))+((CNK(8,2,3)*HNK(3,3))+((CNK(7,2,1)*HNK(3,1))+(CNK(8,2,2)*
     & HNK(3,2)))))+TEMP(1)))
      AM(2)= ((((.1D0*((CNK(11,2,1)*WK(11,1))+(CNK(11,2,2)*WK(11,2))))+(
     & .5D0*(CNK(8,2,3)*WK(11,3))))+((VNK(11,1)*(RNK(11,3)-COM(3)))-(
     & VNK(11,3)*(RNK(11,1)-COM(1)))))+TEMP(2))
      TEMP(1)= (((.35D0*((CNK(7,3,1)*WK(7,1))+(CNK(7,3,2)*WK(7,2))))+(
     & 6.8D0*((VNK(7,2)*(RNK(7,1)-COM(1)))-(VNK(7,1)*(RNK(7,2)-COM(2))))
     & ))+((410D0*((U(2)*(Q(1)-COM(1)))-(U(1)*(Q(2)-COM(2)))))+((
     & CIK(4,3,3)*HNK(1,3))+((CIK(4,3,1)*HNK(1,1))+(CIK(4,3,2)*HNK(1,2))
     & ))))
      TEMP(2)= (((10.7D0*((VNK(10,2)*(RNK(10,1)-COM(1)))-(VNK(10,1)*(
     & RNK(10,2)-COM(2)))))+((27.2D0*(CNK(10,3,3)*WK(10,3)))+((.2D0*(
     & CNK(10,3,2)*WK(10,2)))+(27.2D0*(CNK(9,3,1)*WK(10,1))))))+(((
     & 57.5D0*((VNK(8,2)*(RNK(8,1)-COM(1)))-(VNK(8,1)*(RNK(8,2)-COM(2)))
     & ))+((CNK(8,3,3)*HNK(3,3))+((CNK(7,3,1)*HNK(3,1))+(CNK(8,3,2)*
     & HNK(3,2)))))+TEMP(1)))
      AM(3)= ((((.1D0*((CNK(11,3,1)*WK(11,1))+(CNK(11,3,2)*WK(11,2))))+(
     & .5D0*(CNK(8,3,3)*WK(11,3))))+((VNK(11,2)*(RNK(11,1)-COM(1)))-(
     & VNK(11,1)*(RNK(11,2)-COM(2)))))+TEMP(2))
      TEMP(1)= (((.35D0*((WK(7,1)*WK(7,1))+(WK(7,2)*WK(7,2))))+(6.8D0*((
     & VNK(7,3)*VNK(7,3))+((VNK(7,1)*VNK(7,1))+(VNK(7,2)*VNK(7,2))))))+(
     & (410D0*((U(3)*U(3))+((U(1)*U(1))+(U(2)*U(2)))))+((HNK(1,3)*U(6))+
     & ((HNK(1,1)*U(4))+(HNK(1,2)*U(5))))))
      TEMP(2)= (((10.7D0*((VNK(10,3)*VNK(10,3))+((VNK(10,1)*VNK(10,1))+(
     & VNK(10,2)*VNK(10,2)))))+((27.2D0*(WK(10,3)*WK(10,3)))+((.2D0*(
     & WK(10,2)*WK(10,2)))+(27.2D0*(WK(10,1)*WK(10,1))))))+(((57.5D0*((
     & VNK(8,3)*VNK(8,3))+((VNK(8,1)*VNK(8,1))+(VNK(8,2)*VNK(8,2)))))+((
     & HNK(3,3)*WK(8,3))+((HNK(3,1)*WK(8,1))+(HNK(3,2)*WK(8,2)))))+
     & TEMP(1)))
      KE= (.5D0*((((.1D0*((WK(11,1)*WK(11,1))+(WK(11,2)*WK(11,2))))+(
     & .5D0*(WK(11,3)*WK(11,3))))+((VNK(11,3)*VNK(11,3))+((VNK(11,1)*
     & VNK(11,1))+(VNK(11,2)*VNK(11,2)))))+TEMP(2)))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  151 adds/subtracts/negates
C                     172 multiplies
C                       0 divides
C                      21 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDSYS(MTOTO,CM,ICM)
      DOUBLE PRECISION MTOTO,CM(3),ICM(3,3)
C
C
C Compute system total mass, and instantaneous center of mass and
C inertia matrix.
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION IKCNKT(11,3,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(20,23)
        RETURN
      ENDIF
      MTOTO= 486D0
      CM(1)= COM(1)
      CM(2)= COM(2)
      CM(3)= COM(3)
      IKCNKT(6,1,1)= ((14D0*CIK(4,1,3))+((115D0*CIK(4,1,1))-(14D0*
     & CIK(4,1,2))))
      IKCNKT(6,1,2)= ((14D0*CIK(4,2,3))+((115D0*CIK(4,2,1))-(14D0*
     & CIK(4,2,2))))
      IKCNKT(6,1,3)= ((14D0*CIK(4,3,3))+((115D0*CIK(4,3,1))-(14D0*
     & CIK(4,3,2))))
      IKCNKT(6,2,1)= (((316D0*CIK(4,1,2))-(14D0*CIK(4,1,1)))-(34.6D0*
     & CIK(4,1,3)))
      IKCNKT(6,2,2)= (((316D0*CIK(4,2,2))-(14D0*CIK(4,2,1)))-(34.6D0*
     & CIK(4,2,3)))
      IKCNKT(6,2,3)= (((316D0*CIK(4,3,2))-(14D0*CIK(4,3,1)))-(34.6D0*
     & CIK(4,3,3)))
      IKCNKT(6,3,1)= ((440D0*CIK(4,1,3))+((14D0*CIK(4,1,1))-(34.6D0*
     & CIK(4,1,2))))
      IKCNKT(6,3,2)= ((440D0*CIK(4,2,3))+((14D0*CIK(4,2,1))-(34.6D0*
     & CIK(4,2,2))))
      IKCNKT(6,3,3)= ((440D0*CIK(4,3,3))+((14D0*CIK(4,3,1))-(34.6D0*
     & CIK(4,3,2))))
      IKCNKT(8,1,1)= (((.41D0*CNK(8,1,2))+(4.85D0*CNK(7,1,1)))-(.07D0*
     & CNK(8,1,3)))
      IKCNKT(8,1,2)= (((.41D0*CNK(8,2,2))+(4.85D0*CNK(7,2,1)))-(.07D0*
     & CNK(8,2,3)))
      IKCNKT(8,1,3)= (((.41D0*CNK(8,3,2))+(4.85D0*CNK(7,3,1)))-(.07D0*
     & CNK(8,3,3)))
      IKCNKT(8,2,1)= ((.54D0*CNK(8,1,3))+((.41D0*CNK(7,1,1))+(2.2D0*
     & CNK(8,1,2))))
      IKCNKT(8,2,2)= ((.54D0*CNK(8,2,3))+((.41D0*CNK(7,2,1))+(2.2D0*
     & CNK(8,2,2))))
      IKCNKT(8,2,3)= ((.54D0*CNK(8,3,3))+((.41D0*CNK(7,3,1))+(2.2D0*
     & CNK(8,3,2))))
      IKCNKT(8,3,1)= ((5.5D0*CNK(8,1,3))+((.54D0*CNK(8,1,2))-(.07D0*
     & CNK(7,1,1))))
      IKCNKT(8,3,2)= ((5.5D0*CNK(8,2,3))+((.54D0*CNK(8,2,2))-(.07D0*
     & CNK(7,2,1))))
      IKCNKT(8,3,3)= ((5.5D0*CNK(8,3,3))+((.54D0*CNK(8,3,2))-(.07D0*
     & CNK(7,3,1))))
      TEMP(1)= (((.35D0*((CNK(7,1,1)*CNK(7,1,1))+(CNK(7,1,2)*CNK(7,1,2))
     & ))+(6.8D0*((RNK(7,2)*RNK(7,2))+(RNK(7,3)*RNK(7,3)))))+((410D0*((
     & Q(2)*Q(2))+(Q(3)*Q(3))))+((CIK(4,1,3)*IKCNKT(6,3,1))+((CIK(4,1,1)
     & *IKCNKT(6,1,1))+(CIK(4,1,2)*IKCNKT(6,2,1))))))
      TEMP(2)= (((10.7D0*((RNK(10,2)*RNK(10,2))+(RNK(10,3)*RNK(10,3))))+
     & ((27.2D0*(CNK(10,1,3)*CNK(10,1,3)))+((.2D0*(CNK(10,1,2)*
     & CNK(10,1,2)))+(27.2D0*(CNK(9,1,1)*CNK(9,1,1))))))+(((57.5D0*((
     & RNK(8,2)*RNK(8,2))+(RNK(8,3)*RNK(8,3))))+((CNK(8,1,3)*
     & IKCNKT(8,3,1))+((CNK(7,1,1)*IKCNKT(8,1,1))+(CNK(8,1,2)*
     & IKCNKT(8,2,1)))))+TEMP(1)))
      ICM(1,1)= (((((.1D0*((CNK(11,1,1)*CNK(11,1,1))+(CNK(11,1,2)*
     & CNK(11,1,2))))+(.5D0*(CNK(8,1,3)*CNK(8,1,3))))+((RNK(11,2)*
     & RNK(11,2))+(RNK(11,3)*RNK(11,3))))+TEMP(2))-(486D0*((COM(2)*
     & COM(2))+(COM(3)*COM(3)))))
      TEMP(1)= ((((.35D0*((CNK(7,1,1)*CNK(7,2,1))+(CNK(7,1,2)*CNK(7,2,2)
     & )))-(6.8D0*(RNK(7,1)*RNK(7,2))))+(((CIK(4,1,3)*IKCNKT(6,3,2))+((
     & CIK(4,1,1)*IKCNKT(6,1,2))+(CIK(4,1,2)*IKCNKT(6,2,2))))-(410D0*(
     & Q(1)*Q(2)))))+(((CNK(8,1,3)*IKCNKT(8,3,2))+((CNK(7,1,1)*
     & IKCNKT(8,1,2))+(CNK(8,1,2)*IKCNKT(8,2,2))))-(57.5D0*(RNK(8,1)*
     & RNK(8,2)))))
      ICM(1,2)= ((486D0*(COM(1)*COM(2)))+((((.1D0*((CNK(11,1,1)*
     & CNK(11,2,1))+(CNK(11,1,2)*CNK(11,2,2))))+(.5D0*(CNK(8,1,3)*
     & CNK(8,2,3))))-(RNK(11,1)*RNK(11,2)))+((((27.2D0*(CNK(10,1,3)*
     & CNK(10,2,3)))+((.2D0*(CNK(10,1,2)*CNK(10,2,2)))+(27.2D0*(
     & CNK(9,1,1)*CNK(9,2,1)))))-(10.7D0*(RNK(10,1)*RNK(10,2))))+TEMP(1)
     & )))
      TEMP(1)= ((((.35D0*((CNK(7,1,1)*CNK(7,3,1))+(CNK(7,1,2)*CNK(7,3,2)
     & )))-(6.8D0*(RNK(7,1)*RNK(7,3))))+(((CIK(4,1,3)*IKCNKT(6,3,3))+((
     & CIK(4,1,1)*IKCNKT(6,1,3))+(CIK(4,1,2)*IKCNKT(6,2,3))))-(410D0*(
     & Q(1)*Q(3)))))+(((CNK(8,1,3)*IKCNKT(8,3,3))+((CNK(7,1,1)*
     & IKCNKT(8,1,3))+(CNK(8,1,2)*IKCNKT(8,2,3))))-(57.5D0*(RNK(8,1)*
     & RNK(8,3)))))
      ICM(1,3)= ((486D0*(COM(1)*COM(3)))+((((.1D0*((CNK(11,1,1)*
     & CNK(11,3,1))+(CNK(11,1,2)*CNK(11,3,2))))+(.5D0*(CNK(8,1,3)*
     & CNK(8,3,3))))-(RNK(11,1)*RNK(11,3)))+((((27.2D0*(CNK(10,1,3)*
     & CNK(10,3,3)))+((.2D0*(CNK(10,1,2)*CNK(10,3,2)))+(27.2D0*(
     & CNK(9,1,1)*CNK(9,3,1)))))-(10.7D0*(RNK(10,1)*RNK(10,3))))+TEMP(1)
     & )))
      ICM(2,1)= ICM(1,2)
      TEMP(1)= (((.35D0*((CNK(7,2,1)*CNK(7,2,1))+(CNK(7,2,2)*CNK(7,2,2))
     & ))+(6.8D0*((RNK(7,1)*RNK(7,1))+(RNK(7,3)*RNK(7,3)))))+((410D0*((
     & Q(1)*Q(1))+(Q(3)*Q(3))))+((CIK(4,2,3)*IKCNKT(6,3,2))+((CIK(4,2,1)
     & *IKCNKT(6,1,2))+(CIK(4,2,2)*IKCNKT(6,2,2))))))
      TEMP(2)= (((10.7D0*((RNK(10,1)*RNK(10,1))+(RNK(10,3)*RNK(10,3))))+
     & ((27.2D0*(CNK(10,2,3)*CNK(10,2,3)))+((.2D0*(CNK(10,2,2)*
     & CNK(10,2,2)))+(27.2D0*(CNK(9,2,1)*CNK(9,2,1))))))+(((57.5D0*((
     & RNK(8,1)*RNK(8,1))+(RNK(8,3)*RNK(8,3))))+((CNK(8,2,3)*
     & IKCNKT(8,3,2))+((CNK(7,2,1)*IKCNKT(8,1,2))+(CNK(8,2,2)*
     & IKCNKT(8,2,2)))))+TEMP(1)))
      ICM(2,2)= (((((.1D0*((CNK(11,2,1)*CNK(11,2,1))+(CNK(11,2,2)*
     & CNK(11,2,2))))+(.5D0*(CNK(8,2,3)*CNK(8,2,3))))+((RNK(11,1)*
     & RNK(11,1))+(RNK(11,3)*RNK(11,3))))+TEMP(2))-(486D0*((COM(1)*
     & COM(1))+(COM(3)*COM(3)))))
      TEMP(1)= ((((.35D0*((CNK(7,2,1)*CNK(7,3,1))+(CNK(7,2,2)*CNK(7,3,2)
     & )))-(6.8D0*(RNK(7,2)*RNK(7,3))))+(((CIK(4,2,3)*IKCNKT(6,3,3))+((
     & CIK(4,2,1)*IKCNKT(6,1,3))+(CIK(4,2,2)*IKCNKT(6,2,3))))-(410D0*(
     & Q(2)*Q(3)))))+(((CNK(8,2,3)*IKCNKT(8,3,3))+((CNK(7,2,1)*
     & IKCNKT(8,1,3))+(CNK(8,2,2)*IKCNKT(8,2,3))))-(57.5D0*(RNK(8,2)*
     & RNK(8,3)))))
      ICM(2,3)= ((486D0*(COM(2)*COM(3)))+((((.1D0*((CNK(11,2,1)*
     & CNK(11,3,1))+(CNK(11,2,2)*CNK(11,3,2))))+(.5D0*(CNK(8,2,3)*
     & CNK(8,3,3))))-(RNK(11,2)*RNK(11,3)))+((((27.2D0*(CNK(10,2,3)*
     & CNK(10,3,3)))+((.2D0*(CNK(10,2,2)*CNK(10,3,2)))+(27.2D0*(
     & CNK(9,2,1)*CNK(9,3,1)))))-(10.7D0*(RNK(10,2)*RNK(10,3))))+TEMP(1)
     & )))
      ICM(3,1)= ICM(1,3)
      ICM(3,2)= ICM(2,3)
      TEMP(1)= (((.35D0*((CNK(7,3,1)*CNK(7,3,1))+(CNK(7,3,2)*CNK(7,3,2))
     & ))+(6.8D0*((RNK(7,1)*RNK(7,1))+(RNK(7,2)*RNK(7,2)))))+((410D0*((
     & Q(1)*Q(1))+(Q(2)*Q(2))))+((CIK(4,3,3)*IKCNKT(6,3,3))+((CIK(4,3,1)
     & *IKCNKT(6,1,3))+(CIK(4,3,2)*IKCNKT(6,2,3))))))
      TEMP(2)= (((10.7D0*((RNK(10,1)*RNK(10,1))+(RNK(10,2)*RNK(10,2))))+
     & ((27.2D0*(CNK(10,3,3)*CNK(10,3,3)))+((.2D0*(CNK(10,3,2)*
     & CNK(10,3,2)))+(27.2D0*(CNK(9,3,1)*CNK(9,3,1))))))+(((57.5D0*((
     & RNK(8,1)*RNK(8,1))+(RNK(8,2)*RNK(8,2))))+((CNK(8,3,3)*
     & IKCNKT(8,3,3))+((CNK(7,3,1)*IKCNKT(8,1,3))+(CNK(8,3,2)*
     & IKCNKT(8,2,3)))))+TEMP(1)))
      ICM(3,3)= (((((.1D0*((CNK(11,3,1)*CNK(11,3,1))+(CNK(11,3,2)*
     & CNK(11,3,2))))+(.5D0*(CNK(8,3,3)*CNK(8,3,3))))+((RNK(11,1)*
     & RNK(11,1))+(RNK(11,2)*RNK(11,2))))+TEMP(2))-(486D0*((COM(1)*
     & COM(1))+(COM(2)*COM(2)))))
C
C  Used TIMESTAMP seconds CPU time,
C  TIMESTAMP additional bytes of memory.
C  Equations contain  168 adds/subtracts/negates
C                     258 multiplies
C                       0 divides
C                      40 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDPOS(BODY,PT,LOC)
      INTEGER BODY
      DOUBLE PRECISION PT(3),LOC(3)
C
C
C Return inertial frame location of a point on a body.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION PV(3)
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(21,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(21,23)
        RETURN
      ENDIF
      IF (BODY  .EQ.  0) THEN
        LOC(1)= PT(1)
        LOC(2)= PT(2)
        LOC(3)= PT(3)
      ELSE
        PV(1)= RNB(BODY,1)+PT(1)*CNB(BODY,1,1)+PT(2)*CNB(BODY,1,2)+PT(3)
     &   *CNB(BODY,1,3)
        PV(2)= RNB(BODY,2)+PT(1)*CNB(BODY,2,1)+PT(2)*CNB(BODY,2,2)+PT(3)
     &   *CNB(BODY,2,3)
        PV(3)= RNB(BODY,3)+PT(1)*CNB(BODY,3,1)+PT(2)*CNB(BODY,3,2)+PT(3)
     &   *CNB(BODY,3,3)
        LOC(1)= PV(1)
        LOC(2)= PV(2)
        LOC(3)= PV(3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDVEL(BODY,PT,VELO)
      INTEGER BODY
      DOUBLE PRECISION PT(3),VELO(3)
C
C
C Return inertial frame velocity of a point on a body.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION PV(3)
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(22,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(22,23)
        RETURN
      ENDIF
      IF (BODY  .EQ.  0) THEN
        VELO(1)= 0D0
        VELO(2)= 0D0
        VELO(3)= 0D0
      ELSE
        PV(1)= WB(BODY,2)*PT(3)-WB(BODY,3)*PT(2)
        PV(2)= WB(BODY,3)*PT(1)-WB(BODY,1)*PT(3)
        PV(3)= WB(BODY,1)*PT(2)-WB(BODY,2)*PT(1)
        VELO(1)= VNB(BODY,1)+PV(1)*CNB(BODY,1,1)+PV(2)*CNB(BODY,1,2)+PV(
     &   3)*CNB(BODY,1,3)
        VELO(2)= VNB(BODY,2)+PV(1)*CNB(BODY,2,1)+PV(2)*CNB(BODY,2,2)+PV(
     &   3)*CNB(BODY,2,3)
        VELO(3)= VNB(BODY,3)+PV(1)*CNB(BODY,3,1)+PV(2)*CNB(BODY,3,2)+PV(
     &   3)*CNB(BODY,3,3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDORIENT(BODY,DIRCOS)
      INTEGER BODY
      DOUBLE PRECISION DIRCOS(3,3)
C
C
C Return orientation of body w.r.t. ground frame.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(23,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(23,23)
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        DIRCOS(1,1)= 1D0
        DIRCOS(1,2)= 0D0
        DIRCOS(1,3)= 0D0
        DIRCOS(2,1)= 0D0
        DIRCOS(2,2)= 1D0
        DIRCOS(2,3)= 0D0
        DIRCOS(3,1)= 0D0
        DIRCOS(3,2)= 0D0
        DIRCOS(3,3)= 1D0
      ELSE
        DIRCOS(1,1)= CNB(BODY,1,1)
        DIRCOS(1,2)= CNB(BODY,1,2)
        DIRCOS(1,3)= CNB(BODY,1,3)
        DIRCOS(2,1)= CNB(BODY,2,1)
        DIRCOS(2,2)= CNB(BODY,2,2)
        DIRCOS(2,3)= CNB(BODY,2,3)
        DIRCOS(3,1)= CNB(BODY,3,1)
        DIRCOS(3,2)= CNB(BODY,3,2)
        DIRCOS(3,3)= CNB(BODY,3,3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDANGVEL(BODY,AVEL)
      INTEGER BODY
      DOUBLE PRECISION AVEL(3)
C
C
C Return angular velocity of the body.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(24,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(24,23)
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        AVEL(1)= 0D0
        AVEL(2)= 0D0
        AVEL(3)= 0D0
      ELSE
        AVEL(1)= WB(BODY,1)
        AVEL(2)= WB(BODY,2)
        AVEL(3)= WB(BODY,3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDTRANS(FRBOD,IVEC,TOBOD,OVEC)
      INTEGER FRBOD
      DOUBLE PRECISION IVEC(3)
      INTEGER TOBOD
      DOUBLE PRECISION OVEC(3)
C
C
C Transform ivec from frbod frame to tobod frame.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION PV(3)
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(25,FRBOD) .NE. 0) THEN
        RETURN
      ENDIF
      IF (SDCHKBNUM(25,TOBOD) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(25,23)
        RETURN
      ENDIF
      IF (FRBOD .EQ. TOBOD) THEN
        CALL SDVCOPY(IVEC,OVEC)
        RETURN
      ENDIF
      IF (FRBOD .EQ. 0) THEN
        CALL SDVCOPY(IVEC,PV)
        OVEC(1)= PV(1)*CNB(TOBOD,1,1)+PV(2)*CNB(TOBOD,2,1)+PV(3)*CNB(
     &   TOBOD,3,1)
        OVEC(2)= PV(1)*CNB(TOBOD,1,2)+PV(2)*CNB(TOBOD,2,2)+PV(3)*CNB(
     &   TOBOD,3,2)
        OVEC(3)= PV(1)*CNB(TOBOD,1,3)+PV(2)*CNB(TOBOD,2,3)+PV(3)*CNB(
     &   TOBOD,3,3)
        RETURN
      ENDIF
      IF (TOBOD .EQ. 0) THEN
        CALL SDVCOPY(IVEC,PV)
        OVEC(1)= PV(1)*CNB(FRBOD,1,1)+PV(2)*CNB(FRBOD,1,2)+PV(3)*CNB(
     &   FRBOD,1,3)
        OVEC(2)= PV(1)*CNB(FRBOD,2,1)+PV(2)*CNB(FRBOD,2,2)+PV(3)*CNB(
     &   FRBOD,2,3)
        OVEC(3)= PV(1)*CNB(FRBOD,3,1)+PV(2)*CNB(FRBOD,3,2)+PV(3)*CNB(
     &   FRBOD,3,3)
        RETURN
      ENDIF
      PV(1)= IVEC(1)*CNB(FRBOD,1,1)+IVEC(2)*CNB(FRBOD,1,2)+IVEC(3)*CNB(
     & FRBOD,1,3)
      PV(2)= IVEC(1)*CNB(FRBOD,2,1)+IVEC(2)*CNB(FRBOD,2,2)+IVEC(3)*CNB(
     & FRBOD,2,3)
      PV(3)= IVEC(1)*CNB(FRBOD,3,1)+IVEC(2)*CNB(FRBOD,3,2)+IVEC(3)*CNB(
     & FRBOD,3,3)
      OVEC(1)= PV(1)*CNB(TOBOD,1,1)+PV(2)*CNB(TOBOD,2,1)+PV(3)*CNB(TOBOD
     & ,3,1)
      OVEC(2)= PV(1)*CNB(TOBOD,1,2)+PV(2)*CNB(TOBOD,2,2)+PV(3)*CNB(TOBOD
     & ,3,2)
      OVEC(3)= PV(1)*CNB(TOBOD,1,3)+PV(2)*CNB(TOBOD,2,3)+PV(3)*CNB(TOBOD
     & ,3,3)
C
      RETURN
      END
C
      SUBROUTINE SDREL2CART(COORD,BODY,POINT,LINCHG,ROTCHG)
      INTEGER COORD,BODY
      DOUBLE PRECISION POINT(3),LINCHG(3),ROTCHG(3)
C
C Return derivative of pt loc and body orient w.r.t. hinge rate
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER X,I,GND
      DOUBLE PRECISION LIN(3),PV(3)
      INTEGER SDCHKBNUM
C
      IF ((COORD .LT. 1) .OR. (COORD .GT. 11)) THEN
        CALL SDSETERR(59,45)
        RETURN
      ENDIF
      IF (SDCHKBNUM(59,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(59,23)
        RETURN
      ENDIF
      GND= 0
      IF (BODY .EQ. GND) THEN
        X= 0
      ELSE
        X= FIRSTQ(BODY)+NJNTDOF(BODY)-1
      ENDIF
      IF (X .LT. COORD) THEN
        CALL SDVSET(0D0,0D0,0D0,LINCHG)
        CALL SDVSET(0D0,0D0,0D0,ROTCHG)
        RETURN
      ENDIF
      CALL SDDOVPK
      DO 100 I=1,3
        ROTCHG(I)= WPK(COORD,X,I)
        LIN(I)= VPK(COORD,X,I)
100   CONTINUE
      IF (BODY .EQ. GND) THEN
        CALL SDVCOPY(POINT,PV)
      ELSE
        PV(1)= RCOM(BODY,1)+POINT(1)
        PV(2)= RCOM(BODY,2)+POINT(2)
        PV(3)= RCOM(BODY,3)+POINT(3)
      ENDIF
      CALL SDVCROSS(ROTCHG,PV,LINCHG)
      CALL SDVADD(LINCHG,LIN,LINCHG)
C
      RETURN
      END
C
      SUBROUTINE SDACC(BODY,PT,ACCEL)
      INTEGER BODY
      DOUBLE PRECISION PT(3),ACCEL(3)
C
C
C Return linear acceleration a point of the specified body.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION PV(3)
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(32,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(32,24)
        RETURN
      ENDIF
      IF (BODY  .EQ.  0) THEN
        ACCEL(1)= 0D0
        ACCEL(2)= 0D0
        ACCEL(3)= 0D0
      ELSE
        PV(1)= PT(1)*DYAD(BODY,1,1)+PT(2)*DYAD(BODY,1,2)+PT(3)*DYAD(BODY
     &   ,1,3)
        PV(2)= PT(1)*DYAD(BODY,2,1)+PT(2)*DYAD(BODY,2,2)+PT(3)*DYAD(BODY
     &   ,2,3)
        PV(3)= PT(1)*DYAD(BODY,3,1)+PT(2)*DYAD(BODY,3,2)+PT(3)*DYAD(BODY
     &   ,3,3)
        ACCEL(1)= ANB(BODY,1)+PV(1)*CNB(BODY,1,1)+PV(2)*CNB(BODY,1,2)+PV
     &   (3)*CNB(BODY,1,3)
        ACCEL(2)= ANB(BODY,2)+PV(1)*CNB(BODY,2,1)+PV(2)*CNB(BODY,2,2)+PV
     &   (3)*CNB(BODY,2,3)
        ACCEL(3)= ANB(BODY,3)+PV(1)*CNB(BODY,3,1)+PV(2)*CNB(BODY,3,2)+PV
     &   (3)*CNB(BODY,3,3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDANGACC(BODY,AACC)
      INTEGER BODY
      DOUBLE PRECISION AACC(3)
C
C
C Return angular acceleration of the body.
C
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(12),QN(12),U(11),CNK(11,3,3),CNB(5,3,3)
      DOUBLE PRECISION RNK(11,3),VNK(11,3),WK(11,3),RNB(5,3),VNB(5,3),WB
     & (5,3),WBRCOM(5,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(11,3,3),RIKT(11,3,3),IKO(11,3,3),MKRK(11,3,3)
     & ,CIB(5,3,3)
      DOUBLE PRECISION WKK(11,3),VKK(11,3),DIK(11,3),RPP(11,3),RPK(11,3)
     & ,RIK(11,3),RIK2(11,3)
      DOUBLE PRECISION RPRI(11,3),WIK(11,3),VIK(11,3),WIRK(11,3),RKWKK(
     & 11,3),WKRPK(11,3),VIKWKR(11,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(5,3),UTK(5,3)
     & ,MFK(5,3),MTK(5,3)
      DOUBLE PRECISION UTAU(11),MTAU(11),UACC(11),UVEL(11),UPOS(12)
      DOUBLE PRECISION S7,C7,S8,C8,S9,C9,S10,C10,S11,C11
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S7,C7,S8,C8,S9,C9,S10,C10,S11,C
     & 11
      DOUBLE PRECISION FS0(11),QDOT(12),OTK(11,3),ATK(11,3),AIOIWI(11,3)
     & ,FSTAR(11,3)
      DOUBLE PRECISION TSTAR(11,3),FSTARK(11,3),TSTARK(11,3),IKWK(11,3),
     & WKIKWK(11,3),GK(11,3),IKBWK(5,3),WKIKBWK(5,3)
      DOUBLE PRECISION W0W0(5),W1W1(5),W2W2(5),W0W1(5),W0W2(5),W1W2(5)
      DOUBLE PRECISION W00W11(5),W00W22(5),W11W22(5),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(11,11),MLO(11,11),MDI(11),IKWPK(11,11,3),WORKS
     & (11),WORKSS(11,11)
      DOUBLE PRECISION WPK(11,11,3),VPK(11,11,3),VWRI(11,11,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(11),UDOT(11),TAUC(11),DYAD(5,3,3),FC(11,3),TC(
     & 11,3)
      DOUBLE PRECISION ANK(11,3),ONK(11,3),ONKB(11,3),AONKRI(11,3),ANKB(
     & 11,3),ANKATK(11,3),ANB(5,3),ONB(5,3),DYRCOM(5,3)
      DOUBLE PRECISION FFK(11,3),TTK(11,3),FCCIKT(11,3),FFKB(5,3),TTKB(5
     & ,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(33,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(33,24)
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        AACC(1)= 0D0
        AACC(2)= 0D0
        AACC(3)= 0D0
      ELSE
        AACC(1)= ONB(BODY,1)
        AACC(2)= ONB(BODY,2)
        AACC(3)= ONB(BODY,3)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDGRAV(GRAVIN)
      DOUBLE PRECISION GRAVIN(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      CALL SDSETERR(1,19)
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDMASS(BODY,MASSIN)
      INTEGER BODY
      DOUBLE PRECISION MASSIN
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(2,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        CALL SDSETERR(2,15)
        RETURN
      ENDIF
      IF (MKQ(BODY) .NE. 0) THEN
        MK(BODY)= MASSIN
        MKQ(BODY)= 3
      ELSE
        CALL SDSETERR(2,19)
      ENDIF
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDINER(BODY,INERIN)
      INTEGER BODY
      DOUBLE PRECISION INERIN(3,3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER ANYQUES
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(3,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        CALL SDSETERR(3,15)
        RETURN
      ENDIF
      ANYQUES= 0
      IF (IKQ(BODY,1,1)  .NE.  0) THEN
        IK(BODY,1,1)= INERIN(1,1)
        IKQ(BODY,1,1)= 3
        ANYQUES= 1
      ENDIF
      IF (IKQ(BODY,1,2)  .NE.  0) THEN
        IK(BODY,1,2)= INERIN(1,2)
        IKQ(BODY,1,2)= 3
        IK(BODY,2,1)= INERIN(1,2)
        IKQ(BODY,2,1)= 3
        ANYQUES= 1
      ENDIF
      IF (IKQ(BODY,1,3)  .NE.  0) THEN
        IK(BODY,1,3)= INERIN(1,3)
        IKQ(BODY,1,3)= 3
        IK(BODY,3,1)= INERIN(1,3)
        IKQ(BODY,3,1)= 3
        ANYQUES= 1
      ENDIF
      IF (IKQ(BODY,2,2)  .NE.  0) THEN
        IK(BODY,2,2)= INERIN(2,2)
        IKQ(BODY,2,2)= 3
        ANYQUES= 1
      ENDIF
      IF (IKQ(BODY,2,3)  .NE.  0) THEN
        IK(BODY,2,3)= INERIN(2,3)
        IKQ(BODY,2,3)= 3
        IK(BODY,3,2)= INERIN(2,3)
        IKQ(BODY,3,2)= 3
        ANYQUES= 1
      ENDIF
      IF (IKQ(BODY,3,3)  .NE.  0) THEN
        IK(BODY,3,3)= INERIN(3,3)
        IKQ(BODY,3,3)= 3
        ANYQUES= 1
      ENDIF
      IF (ANYQUES .EQ. 0) THEN
        CALL SDSETERR(3,19)
      ENDIF
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDBTJ(JOINT,BTJIN)
      INTEGER JOINT
      DOUBLE PRECISION BTJIN(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER ANYQUES
      INTEGER SDCHKJNUM
C
      IF (SDCHKJNUM(4,JOINT) .NE. 0) THEN
        RETURN
      ENDIF
      ANYQUES= 0
      IF (RKQ(JOINT,1)  .NE.  0) THEN
        RK(JOINT,1)= BTJIN(1)
        RKQ(JOINT,1)= 3
        ANYQUES= 1
      ENDIF
      IF (RKQ(JOINT,2)  .NE.  0) THEN
        RK(JOINT,2)= BTJIN(2)
        RKQ(JOINT,2)= 3
        ANYQUES= 1
      ENDIF
      IF (RKQ(JOINT,3)  .NE.  0) THEN
        RK(JOINT,3)= BTJIN(3)
        RKQ(JOINT,3)= 3
        ANYQUES= 1
      ENDIF
      IF (ANYQUES .EQ. 0) THEN
        CALL SDSETERR(4,19)
      ENDIF
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDITJ(JOINT,ITJIN)
      INTEGER JOINT
      DOUBLE PRECISION ITJIN(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER ANYQUES
      INTEGER SDCHKJNUM
C
      IF (SDCHKJNUM(5,JOINT) .NE. 0) THEN
        RETURN
      ENDIF
      ANYQUES= 0
      IF (RIQ(JOINT,1)  .NE.  0) THEN
        RI(JOINT,1)= ITJIN(1)
        RIQ(JOINT,1)= 3
        ANYQUES= 1
      ENDIF
      IF (RIQ(JOINT,2)  .NE.  0) THEN
        RI(JOINT,2)= ITJIN(2)
        RIQ(JOINT,2)= 3
        ANYQUES= 1
      ENDIF
      IF (RIQ(JOINT,3)  .NE.  0) THEN
        RI(JOINT,3)= ITJIN(3)
        RIQ(JOINT,3)= 3
        ANYQUES= 1
      ENDIF
      IF (ANYQUES .EQ. 0) THEN
        CALL SDSETERR(5,19)
      ENDIF
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDPIN(JOINT,PINNO,PININ)
      INTEGER JOINT,PINNO
      DOUBLE PRECISION PININ(3)
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER ANYQUES,OFFS
      INTEGER SDCHKJPIN
C
      IF (SDCHKJPIN(6,JOINT,PINNO) .NE. 0) THEN
        RETURN
      ENDIF
      ANYQUES= 0
      OFFS= FIRSTQ(JOINT)+PINNO-1
      IF (JTYPE(JOINT) .EQ. 21) THEN
        OFFS= OFFS+3
      ENDIF
      IF (JTYPE(JOINT) .EQ. 11) THEN
        OFFS= OFFS+1
      ENDIF
      IF (PINQ(OFFS,1)  .NE.  0) THEN
        PIN(OFFS,1)= PININ(1)
        PINQ(OFFS,1)= 3
        ANYQUES= 1
      ENDIF
      IF (PINQ(OFFS,2)  .NE.  0) THEN
        PIN(OFFS,2)= PININ(2)
        PINQ(OFFS,2)= 3
        ANYQUES= 1
      ENDIF
      IF (PINQ(OFFS,3)  .NE.  0) THEN
        PIN(OFFS,3)= PININ(3)
        PINQ(OFFS,3)= 3
        ANYQUES= 1
      ENDIF
      IF (ANYQUES .EQ. 0) THEN
        CALL SDSETERR(6,19)
      ENDIF
      ROUSTATE= 0
C
      RETURN
      END
C
      SUBROUTINE SDPRES(JOINT,AXIS,PRESIN)
      INTEGER JOINT,AXIS,PRESIN
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDINDX,ANYQUES
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(37,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF ((PRESIN .NE. 0) .AND. (PRESIN .NE. 1)) THEN
        CALL SDSETERR(37,20)
      ENDIF
      ANYQUES= 0
      IF (PRESQ(SDINDX(JOINT,AXIS))  .NE.  0) THEN
        IF (PRESIN  .NE.  0) THEN
          PRES(SDINDX(JOINT,AXIS))= 1D0
        ELSE
          PRES(SDINDX(JOINT,AXIS))= 0D0
        ENDIF
        PRESQ(SDINDX(JOINT,AXIS))= 3
        ANYQUES= 1
      ENDIF
      IF (ANYQUES .EQ. 0) THEN
        CALL SDSETERR(37,19)
      ENDIF
      WWFLG= 0
C
      RETURN
      END
C
      SUBROUTINE SDCONSCHG
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      WWFLG= 0
C
      RETURN
      END
C
      SUBROUTINE SDSTAB(VELIN,POSIN)
      DOUBLE PRECISION VELIN,POSIN
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      STABVEL= VELIN
      STABVELQ= 3
      STABPOS= POSIN
      STABPOSQ= 3
C
      RETURN
      END
C
      SUBROUTINE SDGETGRAV(GRAVOUT)
      DOUBLE PRECISION GRAVOUT(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      GRAVOUT(1)= GRAV(1)
      GRAVOUT(2)= GRAV(2)
      GRAVOUT(3)= GRAV(3)
C
      RETURN
      END
C
      SUBROUTINE SDGETMASS(BODY,MASSOUT)
      INTEGER BODY
      DOUBLE PRECISION MASSOUT
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(40,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        CALL SDSETERR(40,15)
        RETURN
      ENDIF
      MASSOUT= MK(BODY)
C
      RETURN
      END
C
      SUBROUTINE SDGETINER(BODY,INEROUT)
      INTEGER BODY
      DOUBLE PRECISION INEROUT(3,3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDCHKBNUM
C
      IF (SDCHKBNUM(41,BODY) .NE. 0) THEN
        RETURN
      ENDIF
      IF (BODY .EQ. 0) THEN
        CALL SDSETERR(41,15)
        RETURN
      ENDIF
      INEROUT(1,1)= IK(BODY,1,1)
      INEROUT(1,2)= IK(BODY,1,2)
      INEROUT(1,3)= IK(BODY,1,3)
      INEROUT(2,1)= IK(BODY,2,1)
      INEROUT(2,2)= IK(BODY,2,2)
      INEROUT(2,3)= IK(BODY,2,3)
      INEROUT(3,1)= IK(BODY,3,1)
      INEROUT(3,2)= IK(BODY,3,2)
      INEROUT(3,3)= IK(BODY,3,3)
C
      RETURN
      END
C
      SUBROUTINE SDGETBTJ(JOINT,BTJOUT)
      INTEGER JOINT
      DOUBLE PRECISION BTJOUT(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDCHKJNUM
C
      IF (SDCHKJNUM(42,JOINT) .NE. 0) THEN
        RETURN
      ENDIF
      BTJOUT(1)= RK(JOINT,1)
      BTJOUT(2)= RK(JOINT,2)
      BTJOUT(3)= RK(JOINT,3)
C
      RETURN
      END
C
      SUBROUTINE SDGETITJ(JOINT,ITJOUT)
      INTEGER JOINT
      DOUBLE PRECISION ITJOUT(3)
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDCHKJNUM
C
      IF (SDCHKJNUM(43,JOINT) .NE. 0) THEN
        RETURN
      ENDIF
      ITJOUT(1)= RI(JOINT,1)
      ITJOUT(2)= RI(JOINT,2)
      ITJOUT(3)= RI(JOINT,3)
C
      RETURN
      END
C
      SUBROUTINE SDGETPIN(JOINT,PINNO,PINOUT)
      INTEGER JOINT,PINNO
      DOUBLE PRECISION PINOUT(3)
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER OFFS
      INTEGER SDCHKJPIN
C
      IF (SDCHKJPIN(44,JOINT,PINNO) .NE. 0) THEN
        RETURN
      ENDIF
      OFFS= FIRSTQ(JOINT)+PINNO-1
      IF (JTYPE(JOINT) .EQ. 21) THEN
        OFFS= OFFS+3
      ENDIF
      IF (JTYPE(JOINT) .EQ. 11) THEN
        OFFS= OFFS+1
      ENDIF
      PINOUT(1)= PIN(OFFS,1)
      PINOUT(2)= PIN(OFFS,2)
      PINOUT(3)= PIN(OFFS,3)
C
      RETURN
      END
C
      SUBROUTINE SDGETPRES(JOINT,AXIS,PRESOUT)
      INTEGER JOINT,AXIS,PRESOUT
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER SDINDX
      INTEGER SDCHKJAXIS
C
      IF (SDCHKJAXIS(45,JOINT,AXIS) .NE. 0) THEN
        RETURN
      ENDIF
      IF (PRES(SDINDX(JOINT,AXIS))  .NE.  0D0) THEN
        PRESOUT= 1
      ELSE
        PRESOUT= 0
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDGETSTAB(VELOUT,POSOUT)
      DOUBLE PRECISION VELOUT,POSOUT
C
      DOUBLE PRECISION GRAV(3),MK(5),IK(5,3,3),PIN(11,3)
      DOUBLE PRECISION RK(5,3),RI(5,3),PRES(11),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(11)
      INTEGER GRAVQ(3),MKQ(5),IKQ(5,3,3),PINQ(11,3),RKQ(5,3),RIQ(5,3),
     & PRESQ(11),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(5,3),RCOM(5,3),MKRCOMT(5,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(11),PSIK(11,3,3),
     & PSRCOM(11,3),PSRK(11,3),PSRI(11,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C
      VELOUT= STABVEL
      POSOUT= STABPOS
C
      RETURN
      END
C
      SUBROUTINE SDINFO(INFO)
      INTEGER INFO(50)
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
C
      INFO(1)= GROUND
      INFO(2)= NBOD
      INFO(3)= NDOF
      INFO(4)= NCONS
      INFO(5)= NLOOP
      INFO(6)= NLDOF
      INFO(7)= NLOOPC
      INFO(8)= NBALL
      INFO(9)= NLBALL
      INFO(10)= NPRES
      INFO(11)= NUSER
      INFO(12)= 1
C info entries from 13-50 are reserved
C
      RETURN
      END
C
      SUBROUTINE SDJNT(JOINT,INFO,TRAN)
      INTEGER JOINT,INFO(50),TRAN(6)
C
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(5),INB(5),OUTB(5),NJNTDOF(5),NJNTC(5),NJNTP(5),
     & FIRSTQ(5),BALLQ(5),FIRSTM(5),FIRSTP(5)
      INTEGER TRANS(11)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      INTEGER I,OFFS
      INTEGER SDCHKJNUM
C
      IF (SDCHKJNUM(48,JOINT) .NE. 0) THEN
        RETURN
      ENDIF
      INFO(1)= JTYPE(JOINT)
      INFO(2)= 0
      OFFS= 0
      INFO(3)= INB(JOINT)
      INFO(4)= OUTB(JOINT)
      INFO(5)= NJNTDOF(JOINT)
      INFO(6)= NJNTC(JOINT)
      INFO(7)= NJNTP(JOINT)
      INFO(8)= FIRSTQ(JOINT)
      INFO(9)= BALLQ(JOINT)
      INFO(10)= FIRSTM(JOINT)
      INFO(11)= FIRSTP(JOINT)
C info entries from 12-50 are reserved
C
      DO 100 I=0,5
        IF (I  .LT.  NJNTDOF(JOINT)) THEN
          TRAN(I+1)= TRANS(OFFS+FIRSTQ(JOINT)+I)
        ELSE
          TRAN(I+1)= -1
        ENDIF
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDCONS(CONSNO,INFO)
      INTEGER CONSNO,INFO(50)
C
C
      INTEGER SDCHKUCNUM
      IF (SDCHKUCNUM(49,CONSNO) .NE. 0) THEN
        RETURN
      ENDIF
C There are no user constraints in this problem.
C
      RETURN
      END
C
      SUBROUTINE SDGENTIME(GENTM)
      INTEGER GENTM
C
C
      GENTM= TIMESTAMP
C
      RETURN
      END
C
C Done. CPU seconds used: TIMESTAMP  Memory used: TIMESTAMP bytes.
C Equation complexity:
C   sdstate:   233 adds   346 multiplies     1 divides   337 assignments
C   sdderiv:  1518 adds  1844 multiplies    13 divides  1821 assignments
C   sdresid:   538 adds   571 multiplies     0 divides   473 assignments
C   sdreac:    206 adds   120 multiplies     0 divides   186 assignments
C   sdmom:     151 adds   172 multiplies     0 divides    21 assignments
C   sdsys:     168 adds   258 multiplies     0 divides    40 assignments
C
