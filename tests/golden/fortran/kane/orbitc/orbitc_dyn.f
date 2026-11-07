C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
C
C ROADMAP (orbitc.sd)
C
C Bodies        Inb
C No  Name      body Joint type  Coords q         Multipliers
C --- --------- ---- ----------- ---------------- -----------------------
C   0 $ground                                    |
C   1 A           0  Pin           1p            |  1p
C   2 B           1  Planar        2   3   4     |
C   3 C           2  Pin           5             |
C   4 D           3  Pin           6             |
C   5 E           4  Pin           7             |
C   6 F           5  Pin           8             |
C
C
      BLOCK DATA SDBLKDATA
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
C  Topological information
C
      DATA GROUND/1/
      DATA NBOD/6/
      DATA NDOF/8/
      DATA NCONS/1/
      DATA NLOOP/0/
      DATA NLDOF/0/
      DATA NLOOPC/0/
      DATA NBALL/0/
      DATA NLBALL/0/
      DATA NPRES/1/
      DATA NUSER/0/
      DATA JTYPE(1)/1/
      DATA JTYPE(2)/8/
      DATA JTYPE(3)/1/
      DATA JTYPE(4)/1/
      DATA JTYPE(5)/1/
      DATA JTYPE(6)/1/
      DATA INB(1)/0/
      DATA INB(2)/1/
      DATA INB(3)/2/
      DATA INB(4)/3/
      DATA INB(5)/4/
      DATA INB(6)/5/
      DATA OUTB(1)/1/
      DATA OUTB(2)/2/
      DATA OUTB(3)/3/
      DATA OUTB(4)/4/
      DATA OUTB(5)/5/
      DATA OUTB(6)/6/
      DATA NJNTDOF(1)/1/
      DATA NJNTDOF(2)/3/
      DATA NJNTDOF(3)/1/
      DATA NJNTDOF(4)/1/
      DATA NJNTDOF(5)/1/
      DATA NJNTDOF(6)/1/
      DATA NJNTC(1)/0/
      DATA NJNTC(2)/0/
      DATA NJNTC(3)/0/
      DATA NJNTC(4)/0/
      DATA NJNTC(5)/0/
      DATA NJNTC(6)/0/
      DATA NJNTP(1)/1/
      DATA NJNTP(2)/0/
      DATA NJNTP(3)/0/
      DATA NJNTP(4)/0/
      DATA NJNTP(5)/0/
      DATA NJNTP(6)/0/
      DATA FIRSTQ(1)/1/
      DATA FIRSTQ(2)/2/
      DATA FIRSTQ(3)/5/
      DATA FIRSTQ(4)/6/
      DATA FIRSTQ(5)/7/
      DATA FIRSTQ(6)/8/
      DATA BALLQ(1)/-103/
      DATA BALLQ(2)/-103/
      DATA BALLQ(3)/-103/
      DATA BALLQ(4)/-103/
      DATA BALLQ(5)/-103/
      DATA BALLQ(6)/-103/
      DATA FIRSTM(1)/0/
      DATA FIRSTM(2)/0/
      DATA FIRSTM(3)/0/
      DATA FIRSTM(4)/0/
      DATA FIRSTM(5)/0/
      DATA FIRSTM(6)/0/
      DATA FIRSTP(1)/1/
      DATA FIRSTP(2)/0/
      DATA FIRSTP(3)/0/
      DATA FIRSTP(4)/0/
      DATA FIRSTP(5)/0/
      DATA FIRSTP(6)/0/
      DATA TRANS(1)/0/
      DATA TRANS(2)/1/
      DATA TRANS(3)/1/
      DATA TRANS(4)/0/
      DATA TRANS(5)/0/
      DATA TRANS(6)/0/
      DATA TRANS(7)/0/
      DATA TRANS(8)/0/
C Model parameters from the input file
C
C gravity
      DATA GRAV(1)/0D0/
      DATA GRAV(2)/0D0/
      DATA GRAV(3)/0D0/
C
C mass
      DATA MK(1)/0D0/
      DATA MK(2)/1D0/
      DATA MK(3)/1D0/
      DATA MK(4)/1D0/
      DATA MK(5)/1D0/
      DATA MK(6)/1D0/
C
C inertia
      DATA IK(1,1,1)/1D0/
      DATA IK(1,1,2)/0D0/
      DATA IK(1,1,3)/0D0/
      DATA IK(1,2,1)/0D0/
      DATA IK(1,2,2)/1D0/
      DATA IK(1,2,3)/0D0/
      DATA IK(1,3,1)/0D0/
      DATA IK(1,3,2)/0D0/
      DATA IK(1,3,3)/1D0/
      DATA IK(2,1,1)/10D0/
      DATA IK(2,1,2)/0D0/
      DATA IK(2,1,3)/0D0/
      DATA IK(2,2,1)/0D0/
      DATA IK(2,2,2)/50D0/
      DATA IK(2,2,3)/0D0/
      DATA IK(2,3,1)/0D0/
      DATA IK(2,3,2)/0D0/
      DATA IK(2,3,3)/50D0/
      DATA IK(3,1,1)/10D0/
      DATA IK(3,1,2)/0D0/
      DATA IK(3,1,3)/0D0/
      DATA IK(3,2,1)/0D0/
      DATA IK(3,2,2)/50D0/
      DATA IK(3,2,3)/0D0/
      DATA IK(3,3,1)/0D0/
      DATA IK(3,3,2)/0D0/
      DATA IK(3,3,3)/50D0/
      DATA IK(4,1,1)/10D0/
      DATA IK(4,1,2)/0D0/
      DATA IK(4,1,3)/0D0/
      DATA IK(4,2,1)/0D0/
      DATA IK(4,2,2)/50D0/
      DATA IK(4,2,3)/0D0/
      DATA IK(4,3,1)/0D0/
      DATA IK(4,3,2)/0D0/
      DATA IK(4,3,3)/50D0/
      DATA IK(5,1,1)/10D0/
      DATA IK(5,1,2)/0D0/
      DATA IK(5,1,3)/0D0/
      DATA IK(5,2,1)/0D0/
      DATA IK(5,2,2)/50D0/
      DATA IK(5,2,3)/0D0/
      DATA IK(5,3,1)/0D0/
      DATA IK(5,3,2)/0D0/
      DATA IK(5,3,3)/50D0/
      DATA IK(6,1,1)/10D0/
      DATA IK(6,1,2)/0D0/
      DATA IK(6,1,3)/0D0/
      DATA IK(6,2,1)/0D0/
      DATA IK(6,2,2)/50D0/
      DATA IK(6,2,3)/0D0/
      DATA IK(6,3,1)/0D0/
      DATA IK(6,3,2)/0D0/
      DATA IK(6,3,3)/50D0/
C
C tree hinge axis vectors
      DATA PIN(1,1)/0D0/
      DATA PIN(1,2)/0D0/
      DATA PIN(1,3)/1D0/
      DATA PIN(2,1)/1D0/
      DATA PIN(2,2)/0D0/
      DATA PIN(2,3)/0D0/
      DATA PIN(3,1)/0D0/
      DATA PIN(3,2)/1D0/
      DATA PIN(3,3)/0D0/
      DATA PIN(4,1)/0D0/
      DATA PIN(4,2)/0D0/
      DATA PIN(4,3)/1D0/
      DATA PIN(5,1)/0D0/
      DATA PIN(5,2)/0D0/
      DATA PIN(5,3)/1D0/
      DATA PIN(6,1)/0D0/
      DATA PIN(6,2)/0D0/
      DATA PIN(6,3)/1D0/
      DATA PIN(7,1)/0D0/
      DATA PIN(7,2)/0D0/
      DATA PIN(7,3)/1D0/
      DATA PIN(8,1)/0D0/
      DATA PIN(8,2)/0D0/
      DATA PIN(8,3)/1D0/
C
C tree bodytojoint vectors
      DATA RK(1,1)/0D0/
      DATA RK(1,2)/0D0/
      DATA RK(1,3)/0D0/
      DATA RK(2,1)/0D0/
      DATA RK(2,2)/-1D0/
      DATA RK(2,3)/0D0/
      DATA RK(3,1)/0D0/
      DATA RK(3,2)/-1D0/
      DATA RK(3,3)/0D0/
      DATA RK(4,1)/0D0/
      DATA RK(4,2)/-1D0/
      DATA RK(4,3)/0D0/
      DATA RK(5,1)/0D0/
      DATA RK(5,2)/-1D0/
      DATA RK(5,3)/0D0/
      DATA RK(6,1)/0D0/
      DATA RK(6,2)/-1D0/
      DATA RK(6,3)/0D0/
C
C tree inbtojoint vectors
      DATA RI(1,1)/0D0/
      DATA RI(1,2)/0D0/
      DATA RI(1,3)/0D0/
      DATA RI(2,1)/0D0/
      DATA RI(2,2)/0D0/
      DATA RI(2,3)/0D0/
      DATA RI(3,1)/0D0/
      DATA RI(3,2)/1D0/
      DATA RI(3,3)/0D0/
      DATA RI(4,1)/0D0/
      DATA RI(4,2)/1D0/
      DATA RI(4,3)/0D0/
      DATA RI(5,1)/0D0/
      DATA RI(5,2)/1D0/
      DATA RI(5,3)/0D0/
      DATA RI(6,1)/0D0/
      DATA RI(6,2)/1D0/
      DATA RI(6,3)/0D0/
C
C tree prescribed motion
      DATA PRES(1)/1D0/
      DATA PRES(2)/0D0/
      DATA PRES(3)/0D0/
      DATA PRES(4)/0D0/
      DATA PRES(5)/0D0/
      DATA PRES(6)/0D0/
      DATA PRES(7)/0D0/
      DATA PRES(8)/0D0/
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
      DATA MKQ(6)/0/
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
      DATA IKQ(6,1,1)/0/
      DATA IKQ(6,1,2)/0/
      DATA IKQ(6,1,3)/0/
      DATA IKQ(6,2,1)/0/
      DATA IKQ(6,2,2)/0/
      DATA IKQ(6,2,3)/0/
      DATA IKQ(6,3,1)/0/
      DATA IKQ(6,3,2)/0/
      DATA IKQ(6,3,3)/0/
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
      DATA RKQ(6,1)/0/
      DATA RKQ(6,2)/0/
      DATA RKQ(6,3)/0/
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
      DATA RIQ(5,2)/0/
      DATA RIQ(5,3)/0/
      DATA RIQ(6,1)/0/
      DATA RIQ(6,2)/0/
      DATA RIQ(6,3)/0/
      DATA PRESQ(1)/0/
      DATA PRESQ(2)/0/
      DATA PRESQ(3)/0/
      DATA PRESQ(4)/0/
      DATA PRESQ(5)/0/
      DATA PRESQ(6)/0/
      DATA PRESQ(7)/0/
      DATA PRESQ(8)/0/
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION SUMSQ,NORMINV
      INTEGER I,J,K
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      DO 140 K=1,6
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
      DO 160 K=1,8
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
      DO 520 I=1,8
        DO 510 J=I,8
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
      RCOM(6,1)= 0D0
      RCOM(6,2)= 0D0
      RCOM(6,3)= 0D0
C
C Compute mass properties-related constants
C
      MTOT= 5D0
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
      DOUBLE PRECISION ST(8),STANG(8)
C
      INTEGER I
C
      DO 100 I=1,8
        STANG(I)= ST(I)
100   CONTINUE
C
      RETURN
      END
C
C Convert 1-2-3 form of state back to Euler parameters for ball joints.
C
      SUBROUTINE SDANG2ST(STANG,ST)
      DOUBLE PRECISION STANG(8),ST(8)
C
      INTEGER I
C
      DO 100 I=1,8
        ST(I)= STANG(I)
100   CONTINUE
C
      RETURN
      END
C
C Normalize Euler parameters in state.
C
      SUBROUTINE SDNRMSTERR(ST,NORMST,ROUTINE)
      DOUBLE PRECISION ST(8),NORMST(8)
      INTEGER ROUTINE
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER I
C
      DO 100 I=1,8
        NORMST(I)= ST(I)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDNORMST(ST,NORMST)
      DOUBLE PRECISION ST(8),NORMST(8)
C
C
      CALL SDNRMSTERR(ST,NORMST,0)
C
      RETURN
      END
C
      SUBROUTINE SDSTATE(TIMEIN,QIN,UIN)
      DOUBLE PRECISION TIMEIN,QIN(8),UIN(8)
C
C
C Compute kinematic information and store it in sdgstate.
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
C
      IF ((ROUSTATE .NE. 1) .AND. (ROUSTATE .NE. 2) .AND. (ROUSTATE
     &  .NE. 3)) THEN
        CALL SDSETERR(8,22)
        RETURN
      ENDIF
      IF (ROUSTATE .EQ. 1) THEN
        DO 50 I=1,8
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
        DO 65 I=1,8
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
        DO 75 I=1,8
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
      DO 100 I=1,8
        Q(I)= QIN(I)
100   CONTINUE
C
C Compute sines and cosines of q
C
      S1= DSIN(Q(1))
      C1= DCOS(Q(1))
      S4= DSIN(Q(4))
      C4= DCOS(Q(4))
      S5= DSIN(Q(5))
      C5= DCOS(Q(5))
      S6= DSIN(Q(6))
      C6= DCOS(Q(6))
      S7= DSIN(Q(7))
      C7= DCOS(Q(7))
      S8= DSIN(Q(8))
      C8= DCOS(Q(8))
C
C Compute across-axis direction cosines Cik
C
C
C Compute across-joint direction cosines Cib
C
C
C Compute gravity
C
C
C Compute cnk & cnb (direction cosines in N)
C
      CNK(4,1,1)= ((C1*C4)-(S1*S4))
      CNK(4,1,2)= -((S1*C4)+(S4*C1))
      CNK(4,2,1)= ((S1*C4)+(S4*C1))
      CNK(4,2,2)= ((C1*C4)-(S1*S4))
      CNK(5,1,1)= ((CNK(4,1,1)*C5)+(CNK(4,1,2)*S5))
      CNK(5,1,2)= ((CNK(4,1,2)*C5)-(CNK(4,1,1)*S5))
      CNK(5,2,1)= ((CNK(4,2,1)*C5)+(CNK(4,2,2)*S5))
      CNK(5,2,2)= ((CNK(4,2,2)*C5)-(CNK(4,2,1)*S5))
      CNK(6,1,1)= ((CNK(5,1,1)*C6)+(CNK(5,1,2)*S6))
      CNK(6,1,2)= ((CNK(5,1,2)*C6)-(CNK(5,1,1)*S6))
      CNK(6,2,1)= ((CNK(5,2,1)*C6)+(CNK(5,2,2)*S6))
      CNK(6,2,2)= ((CNK(5,2,2)*C6)-(CNK(5,2,1)*S6))
      CNK(7,1,1)= ((CNK(6,1,1)*C7)+(CNK(6,1,2)*S7))
      CNK(7,1,2)= ((CNK(6,1,2)*C7)-(CNK(6,1,1)*S7))
      CNK(7,2,1)= ((CNK(6,2,1)*C7)+(CNK(6,2,2)*S7))
      CNK(7,2,2)= ((CNK(6,2,2)*C7)-(CNK(6,2,1)*S7))
      CNK(8,1,1)= ((CNK(7,1,1)*C8)+(CNK(7,1,2)*S8))
      CNK(8,1,2)= ((CNK(7,1,2)*C8)-(CNK(7,1,1)*S8))
      CNK(8,2,1)= ((CNK(7,2,1)*C8)+(CNK(7,2,2)*S8))
      CNK(8,2,2)= ((CNK(7,2,2)*C8)-(CNK(7,2,1)*S8))
      CNB(1,1,1)= C1
      CNB(1,1,2)= -S1
      CNB(1,1,3)= 0D0
      CNB(1,2,1)= S1
      CNB(1,2,2)= C1
      CNB(1,2,3)= 0D0
      CNB(1,3,1)= 0D0
      CNB(1,3,2)= 0D0
      CNB(1,3,3)= 1D0
      CNB(2,1,1)= CNK(4,1,1)
      CNB(2,1,2)= CNK(4,1,2)
      CNB(2,1,3)= 0D0
      CNB(2,2,1)= CNK(4,2,1)
      CNB(2,2,2)= CNK(4,2,2)
      CNB(2,2,3)= 0D0
      CNB(2,3,1)= 0D0
      CNB(2,3,2)= 0D0
      CNB(2,3,3)= 1D0
      CNB(3,1,1)= CNK(5,1,1)
      CNB(3,1,2)= CNK(5,1,2)
      CNB(3,1,3)= 0D0
      CNB(3,2,1)= CNK(5,2,1)
      CNB(3,2,2)= CNK(5,2,2)
      CNB(3,2,3)= 0D0
      CNB(3,3,1)= 0D0
      CNB(3,3,2)= 0D0
      CNB(3,3,3)= 1D0
      CNB(4,1,1)= CNK(6,1,1)
      CNB(4,1,2)= CNK(6,1,2)
      CNB(4,1,3)= 0D0
      CNB(4,2,1)= CNK(6,2,1)
      CNB(4,2,2)= CNK(6,2,2)
      CNB(4,2,3)= 0D0
      CNB(4,3,1)= 0D0
      CNB(4,3,2)= 0D0
      CNB(4,3,3)= 1D0
      CNB(5,1,1)= CNK(7,1,1)
      CNB(5,1,2)= CNK(7,1,2)
      CNB(5,1,3)= 0D0
      CNB(5,2,1)= CNK(7,2,1)
      CNB(5,2,2)= CNK(7,2,2)
      CNB(5,2,3)= 0D0
      CNB(5,3,1)= 0D0
      CNB(5,3,2)= 0D0
      CNB(5,3,3)= 1D0
      CNB(6,1,1)= CNK(8,1,1)
      CNB(6,1,2)= CNK(8,1,2)
      CNB(6,1,3)= 0D0
      CNB(6,2,1)= CNK(8,2,1)
      CNB(6,2,2)= CNK(8,2,2)
      CNB(6,2,3)= 0D0
      CNB(6,3,1)= 0D0
      CNB(6,3,2)= 0D0
      CNB(6,3,3)= 1D0
C
C Compute q-related auxiliary variables
C
      RIK(5,2)= (1D0+C5)
      RIK(6,2)= (1D0+C6)
      RIK(7,2)= (1D0+C7)
      RIK(8,2)= (1D0+C8)
C
C Compute rnk & rnb (mass center locations in N)
C
      RNK(2,1)= (Q(2)*C1)
      RNK(2,2)= (Q(2)*S1)
      RNK(3,1)= (RNK(2,1)-(Q(3)*S1))
      RNK(3,2)= (RNK(2,2)+(Q(3)*C1))
      RNK(4,1)= (CNK(4,1,2)+RNK(3,1))
      RNK(4,2)= (CNK(4,2,2)+RNK(3,2))
      RNK(5,1)= (CNK(5,1,2)+(CNK(4,1,2)+RNK(4,1)))
      RNK(5,2)= (CNK(5,2,2)+(CNK(4,2,2)+RNK(4,2)))
      RNK(6,1)= (CNK(6,1,2)+(CNK(5,1,2)+RNK(5,1)))
      RNK(6,2)= (CNK(6,2,2)+(CNK(5,2,2)+RNK(5,2)))
      RNK(7,1)= (CNK(7,1,2)+(CNK(6,1,2)+RNK(6,1)))
      RNK(7,2)= (CNK(7,2,2)+(CNK(6,2,2)+RNK(6,2)))
      RNK(8,1)= (CNK(8,1,2)+(CNK(7,1,2)+RNK(7,1)))
      RNK(8,2)= (CNK(8,2,2)+(CNK(7,2,2)+RNK(7,2)))
      RNB(1,1)= 0D0
      RNB(1,2)= 0D0
      RNB(1,3)= 0D0
      RNB(2,1)= RNK(4,1)
      RNB(2,2)= RNK(4,2)
      RNB(2,3)= 0D0
      RNB(3,1)= RNK(5,1)
      RNB(3,2)= RNK(5,2)
      RNB(3,3)= 0D0
      RNB(4,1)= RNK(6,1)
      RNB(4,2)= RNK(6,2)
      RNB(4,3)= 0D0
      RNB(5,1)= RNK(7,1)
      RNB(5,2)= RNK(7,2)
      RNB(5,3)= 0D0
      RNB(6,1)= RNK(8,1)
      RNB(6,2)= RNK(8,2)
      RNB(6,3)= 0D0
C
C Compute com (system mass center location in N)
C
      COM(1)= (.2D0*(RNK(8,1)+(RNK(7,1)+(RNK(6,1)+(RNK(4,1)+RNK(5,1)))))
     & )
      COM(2)= (.2D0*(RNK(8,2)+(RNK(7,2)+(RNK(6,2)+(RNK(4,2)+RNK(5,2)))))
     & )
      COM(3)= 0D0
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
      DO 200 I=1,8
        U(I)= UIN(I)
200   CONTINUE
C
C Compute u-related auxiliary variables
C
C
C Compute wk & wb (angular velocities)
C
      WK(4,3)= (U(1)+U(4))
      WK(5,3)= (U(5)+WK(4,3))
      WK(6,3)= (U(6)+WK(5,3))
      WK(7,3)= (U(7)+WK(6,3))
      WK(8,3)= (U(8)+WK(7,3))
      WB(1,1)= 0D0
      WB(1,2)= 0D0
      WB(1,3)= U(1)
      WB(2,1)= 0D0
      WB(2,2)= 0D0
      WB(2,3)= WK(4,3)
      WB(3,1)= 0D0
      WB(3,2)= 0D0
      WB(3,3)= WK(5,3)
      WB(4,1)= 0D0
      WB(4,2)= 0D0
      WB(4,3)= WK(6,3)
      WB(5,1)= 0D0
      WB(5,2)= 0D0
      WB(5,3)= WK(7,3)
      WB(6,1)= 0D0
      WB(6,2)= 0D0
      WB(6,3)= WK(8,3)
C
C Compute auxiliary variables involving wk
C
      WKRPK(2,2)= (Q(2)*U(1))
      WKRPK(3,1)= -(Q(3)*U(1))
C
C Compute temporaries for use in SDRHS
C
      W2W2(1)= (U(1)*U(1))
      W2W2(2)= (WK(4,3)*WK(4,3))
      W2W2(3)= (WK(5,3)*WK(5,3))
      W2W2(4)= (WK(6,3)*WK(6,3))
      W2W2(5)= (WK(7,3)*WK(7,3))
      W2W2(6)= (WK(8,3)*WK(8,3))
C
C Compute vnk & vnb (mass center linear velocities in N)
C
      VNK(2,1)= ((U(2)*C1)-(WKRPK(2,2)*S1))
      VNK(2,2)= ((U(2)*S1)+(WKRPK(2,2)*C1))
      VNK(3,1)= (VNK(2,1)+((WKRPK(3,1)*C1)-(U(3)*S1)))
      VNK(3,2)= (VNK(2,2)+((U(3)*C1)+(WKRPK(3,1)*S1)))
      VNK(4,1)= (VNK(3,1)-(CNK(4,1,1)*WK(4,3)))
      VNK(4,2)= (VNK(3,2)-(CNK(4,2,1)*WK(4,3)))
      VNK(5,1)= ((VNK(4,1)-(CNK(4,1,1)*WK(4,3)))-(CNK(5,1,1)*WK(5,3)))
      VNK(5,2)= ((VNK(4,2)-(CNK(4,2,1)*WK(4,3)))-(CNK(5,2,1)*WK(5,3)))
      VNK(6,1)= ((VNK(5,1)-(CNK(5,1,1)*WK(5,3)))-(CNK(6,1,1)*WK(6,3)))
      VNK(6,2)= ((VNK(5,2)-(CNK(5,2,1)*WK(5,3)))-(CNK(6,2,1)*WK(6,3)))
      VNK(7,1)= ((VNK(6,1)-(CNK(6,1,1)*WK(6,3)))-(CNK(7,1,1)*WK(7,3)))
      VNK(7,2)= ((VNK(6,2)-(CNK(6,2,1)*WK(6,3)))-(CNK(7,2,1)*WK(7,3)))
      VNK(8,1)= ((VNK(7,1)-(CNK(7,1,1)*WK(7,3)))-(CNK(8,1,1)*WK(8,3)))
      VNK(8,2)= ((VNK(7,2)-(CNK(7,2,1)*WK(7,3)))-(CNK(8,2,1)*WK(8,3)))
      VNB(1,1)= 0D0
      VNB(1,2)= 0D0
      VNB(1,3)= 0D0
      VNB(2,1)= VNK(4,1)
      VNB(2,2)= VNK(4,2)
      VNB(2,3)= 0D0
      VNB(3,1)= VNK(5,1)
      VNB(3,2)= VNK(5,2)
      VNB(3,3)= 0D0
      VNB(4,1)= VNK(6,1)
      VNB(4,2)= VNK(6,2)
      VNB(4,3)= 0D0
      VNB(5,1)= VNK(7,1)
      VNB(5,2)= VNK(7,2)
      VNB(5,3)= 0D0
      VNB(6,1)= VNK(8,1)
      VNB(6,2)= VNK(8,2)
      VNB(6,3)= 0D0
C
C Compute qdot (kinematical equations)
C
      QDOT(1)= U(1)
      QDOT(2)= U(2)
      QDOT(3)= U(3)
      QDOT(4)= U(4)
      QDOT(5)= U(5)
      QDOT(6)= U(6)
      QDOT(7)= U(7)
      QDOT(8)= U(8)
C
C Compute constraint velocity errors
C
C skipus
290   CONTINUE
C
C Initialize applied forces and torques to zero
C
      DO 310 I=1,6
        DO 300 J=1,3
          UFK(I,J)= 0D0
          UTK(I,J)= 0D0
300     CONTINUE
310   CONTINUE
      DO 320 I=1,8
        UTAU(I)= 0D0
320   CONTINUE
      LTAUFLG= 0
      FS0FLG= 0
C
C Initialize prescribed motions
C
      UACC(1)= 0D0
      UVEL(1)= U(1)
      UPOS(1)= Q(1)
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   84 adds/subtracts/negates
C                      80 multiplies
C                       0 divides
C                     247 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDQDOT(OQDOT)
      DOUBLE PRECISION OQDOT(8)
C
C
C Return position coordinate derivatives for tree joints.
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DO 100 I=1,8
        OQDOT(I)= QDOT(I)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDU2QDOT(UIN,OQDOT)
      DOUBLE PRECISION UIN(8),OQDOT(8)
C
C
C Convert velocities to qdots.
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(64,23)
        RETURN
      ENDIF
      DO 100 I=1,8
        OQDOT(I)= UIN(I)
100   CONTINUE
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDPSSTATE(LQIN)
      DOUBLE PRECISION LQIN(1)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
        WPK(1,1,3)= 1D0
        WPK(1,2,3)= 1D0
        WPK(1,3,3)= 1D0
        WPK(1,4,3)= 1D0
        WPK(1,5,3)= 1D0
        WPK(1,6,3)= 1D0
        WPK(1,7,3)= 1D0
        WPK(1,8,3)= 1D0
        WPK(4,4,3)= 1D0
        WPK(4,5,3)= 1D0
        WPK(4,6,3)= 1D0
        WPK(4,7,3)= 1D0
        WPK(4,8,3)= 1D0
        WPK(5,5,3)= 1D0
        WPK(5,6,3)= 1D0
        WPK(5,7,3)= 1D0
        WPK(5,8,3)= 1D0
        WPK(6,6,3)= 1D0
        WPK(6,7,3)= 1D0
        WPK(6,8,3)= 1D0
        WPK(7,7,3)= 1D0
        WPK(7,8,3)= 1D0
        WPK(8,8,3)= 1D0
C
C Compute Vpk (partial velocities)
C
        VPK(1,2,2)= Q(2)
        VPK(1,3,1)= -Q(3)
        VPK(1,3,2)= Q(2)
        VPK(1,4,1)= (((Q(2)*S4)-(Q(3)*C4))-1D0)
        VPK(1,4,2)= ((Q(2)*C4)+(Q(3)*S4))
        VWRI(1,5,1)= (VPK(1,4,1)-1D0)
        VPK(1,5,1)= (((VPK(1,4,2)*S5)+(VWRI(1,5,1)*C5))-1D0)
        VPK(1,5,2)= ((VPK(1,4,2)*C5)-(VWRI(1,5,1)*S5))
        VWRI(1,6,1)= (VPK(1,5,1)-1D0)
        VPK(1,6,1)= (((VPK(1,5,2)*S6)+(VWRI(1,6,1)*C6))-1D0)
        VPK(1,6,2)= ((VPK(1,5,2)*C6)-(VWRI(1,6,1)*S6))
        VWRI(1,7,1)= (VPK(1,6,1)-1D0)
        VPK(1,7,1)= (((VPK(1,6,2)*S7)+(VWRI(1,7,1)*C7))-1D0)
        VPK(1,7,2)= ((VPK(1,6,2)*C7)-(VWRI(1,7,1)*S7))
        VWRI(1,8,1)= (VPK(1,7,1)-1D0)
        VPK(1,8,1)= (((VPK(1,7,2)*S8)+(VWRI(1,8,1)*C8))-1D0)
        VPK(1,8,2)= ((VPK(1,7,2)*C8)-(VWRI(1,8,1)*S8))
        VPK(2,2,1)= 1D0
        VPK(2,3,1)= 1D0
        VPK(2,4,1)= C4
        VPK(2,4,2)= -S4
        VPK(2,5,1)= ((C4*C5)-(S4*S5))
        VPK(2,5,2)= -((S4*C5)+(S5*C4))
        VPK(2,6,1)= ((VPK(2,5,1)*C6)+(VPK(2,5,2)*S6))
        VPK(2,6,2)= ((VPK(2,5,2)*C6)-(VPK(2,5,1)*S6))
        VPK(2,7,1)= ((VPK(2,6,1)*C7)+(VPK(2,6,2)*S7))
        VPK(2,7,2)= ((VPK(2,6,2)*C7)-(VPK(2,6,1)*S7))
        VPK(2,8,1)= ((VPK(2,7,1)*C8)+(VPK(2,7,2)*S8))
        VPK(2,8,2)= ((VPK(2,7,2)*C8)-(VPK(2,7,1)*S8))
        VPK(3,3,2)= 1D0
        VPK(3,4,1)= S4
        VPK(3,4,2)= C4
        VPK(3,5,1)= ((S4*C5)+(S5*C4))
        VPK(3,5,2)= ((C4*C5)-(S4*S5))
        VPK(3,6,1)= ((VPK(3,5,1)*C6)+(VPK(3,5,2)*S6))
        VPK(3,6,2)= ((VPK(3,5,2)*C6)-(VPK(3,5,1)*S6))
        VPK(3,7,1)= ((VPK(3,6,1)*C7)+(VPK(3,6,2)*S7))
        VPK(3,7,2)= ((VPK(3,6,2)*C7)-(VPK(3,6,1)*S7))
        VPK(3,8,1)= ((VPK(3,7,1)*C8)+(VPK(3,7,2)*S8))
        VPK(3,8,2)= ((VPK(3,7,2)*C8)-(VPK(3,7,1)*S8))
        VPK(4,4,1)= -1D0
        VPK(4,5,1)= -(1D0+(2D0*C5))
        VPK(4,5,2)= (2D0*S5)
        VWRI(4,6,1)= (VPK(4,5,1)-1D0)
        VPK(4,6,1)= (((2D0*(S5*S6))+(VWRI(4,6,1)*C6))-1D0)
        VPK(4,6,2)= ((2D0*(S5*C6))-(VWRI(4,6,1)*S6))
        VWRI(4,7,1)= (VPK(4,6,1)-1D0)
        VPK(4,7,1)= (((VPK(4,6,2)*S7)+(VWRI(4,7,1)*C7))-1D0)
        VPK(4,7,2)= ((VPK(4,6,2)*C7)-(VWRI(4,7,1)*S7))
        VWRI(4,8,1)= (VPK(4,7,1)-1D0)
        VPK(4,8,1)= (((VPK(4,7,2)*S8)+(VWRI(4,8,1)*C8))-1D0)
        VPK(4,8,2)= ((VPK(4,7,2)*C8)-(VWRI(4,8,1)*S8))
        VPK(5,5,1)= -1D0
        VPK(5,6,1)= -(1D0+(2D0*C6))
        VPK(5,6,2)= (2D0*S6)
        VWRI(5,7,1)= (VPK(5,6,1)-1D0)
        VPK(5,7,1)= (((2D0*(S6*S7))+(VWRI(5,7,1)*C7))-1D0)
        VPK(5,7,2)= ((2D0*(S6*C7))-(VWRI(5,7,1)*S7))
        VWRI(5,8,1)= (VPK(5,7,1)-1D0)
        VPK(5,8,1)= (((VPK(5,7,2)*S8)+(VWRI(5,8,1)*C8))-1D0)
        VPK(5,8,2)= ((VPK(5,7,2)*C8)-(VWRI(5,8,1)*S8))
        VPK(6,6,1)= -1D0
        VPK(6,7,1)= -(1D0+(2D0*C7))
        VPK(6,7,2)= (2D0*S7)
        VWRI(6,8,1)= (VPK(6,7,1)-1D0)
        VPK(6,8,1)= (((2D0*(S7*S8))+(VWRI(6,8,1)*C8))-1D0)
        VPK(6,8,2)= ((2D0*(S7*C8))-(VWRI(6,8,1)*S8))
        VPK(7,7,1)= -1D0
        VPK(7,8,1)= -(1D0+(2D0*C8))
        VPK(7,8,2)= (2D0*S8)
        VPK(8,8,1)= -1D0
        VPKFLG= 1
      ENDIF
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   70 adds/subtracts/negates
C                      90 multiplies
C                       0 divides
C                      94 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOLTAU
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
C
C Compute Atk (inertial linear acceleration)
C
        ATK(2,1)= -(U(1)*WKRPK(2,2))
        ATK(2,2)= (2D0*(U(1)*U(2)))
        ATK(3,1)= (ATK(2,1)-(2D0*(U(1)*U(3))))
        ATK(3,2)= (ATK(2,2)+(U(1)*WKRPK(3,1)))
        ATK(4,1)= ((ATK(3,1)*C4)+(ATK(3,2)*S4))
        ATK(4,2)= (((ATK(3,2)*C4)-(ATK(3,1)*S4))-(WK(4,3)*WK(4,3)))
        AIOIWI(5,2)= (ATK(4,2)-(WK(4,3)*WK(4,3)))
        ATK(5,1)= ((AIOIWI(5,2)*S5)+(ATK(4,1)*C5))
        ATK(5,2)= (((AIOIWI(5,2)*C5)-(ATK(4,1)*S5))-(WK(5,3)*WK(5,3)))
        AIOIWI(6,2)= (ATK(5,2)-(WK(5,3)*WK(5,3)))
        ATK(6,1)= ((AIOIWI(6,2)*S6)+(ATK(5,1)*C6))
        ATK(6,2)= (((AIOIWI(6,2)*C6)-(ATK(5,1)*S6))-(WK(6,3)*WK(6,3)))
        AIOIWI(7,2)= (ATK(6,2)-(WK(6,3)*WK(6,3)))
        ATK(7,1)= ((AIOIWI(7,2)*S7)+(ATK(6,1)*C7))
        ATK(7,2)= (((AIOIWI(7,2)*C7)-(ATK(6,1)*S7))-(WK(7,3)*WK(7,3)))
        AIOIWI(8,2)= (ATK(7,2)-(WK(7,3)*WK(7,3)))
        ATK(8,1)= ((AIOIWI(8,2)*S8)+(ATK(7,1)*C8))
        ATK(8,2)= (((AIOIWI(8,2)*C8)-(ATK(7,1)*S8))-(WK(8,3)*WK(8,3)))
        INERFLG= 1
      ENDIF
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   22 adds/subtracts/negates
C                      35 multiplies
C                       0 divides
C                      18 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOFS0
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
        FSTAR(4,1)= (ATK(4,1)-UFK(2,1))
        FSTAR(4,2)= (ATK(4,2)-UFK(2,2))
        FSTAR(5,1)= (ATK(5,1)-UFK(3,1))
        FSTAR(5,2)= (ATK(5,2)-UFK(3,2))
        FSTAR(6,1)= (ATK(6,1)-UFK(4,1))
        FSTAR(6,2)= (ATK(6,2)-UFK(4,2))
        FSTAR(7,1)= (ATK(7,1)-UFK(5,1))
        FSTAR(7,2)= (ATK(7,2)-UFK(5,2))
        FSTAR(8,1)= (ATK(8,1)-UFK(6,1))
        FSTAR(8,2)= (ATK(8,2)-UFK(6,2))
C
C Compute Tstar (torques)
C
C
C Compute fs0 (RHS ignoring constraints)
C
        CALL SDDOVPK
        FS0(1)= (UTAU(1)-((((FSTAR(8,1)*VPK(1,8,1))+(FSTAR(8,2)*
     &   VPK(1,8,2)))-UTK(6,3))+((((FSTAR(7,1)*VPK(1,7,1))+(FSTAR(7,2)*
     &   VPK(1,7,2)))-UTK(5,3))+((((FSTAR(6,1)*VPK(1,6,1))+(FSTAR(6,2)*
     &   VPK(1,6,2)))-UTK(4,3))+((((FSTAR(5,1)*VPK(1,5,1))+(FSTAR(5,2)*
     &   VPK(1,5,2)))-UTK(3,3))+((((FSTAR(4,1)*VPK(1,4,1))+(FSTAR(4,2)*
     &   VPK(1,4,2)))-UTK(2,3))-UTK(1,3)))))))
        FS0(2)= (UTAU(2)-(((FSTAR(8,1)*VPK(2,8,1))+(FSTAR(8,2)*
     &   VPK(2,8,2)))+(((FSTAR(7,1)*VPK(2,7,1))+(FSTAR(7,2)*VPK(2,7,2)))
     &   +(((FSTAR(6,1)*VPK(2,6,1))+(FSTAR(6,2)*VPK(2,6,2)))+(((
     &   FSTAR(4,1)*C4)-(FSTAR(4,2)*S4))+((FSTAR(5,1)*VPK(2,5,1))+(
     &   FSTAR(5,2)*VPK(2,5,2))))))))
        FS0(3)= (UTAU(3)-(((FSTAR(8,1)*VPK(3,8,1))+(FSTAR(8,2)*
     &   VPK(3,8,2)))+(((FSTAR(7,1)*VPK(3,7,1))+(FSTAR(7,2)*VPK(3,7,2)))
     &   +(((FSTAR(6,1)*VPK(3,6,1))+(FSTAR(6,2)*VPK(3,6,2)))+(((
     &   FSTAR(4,1)*S4)+(FSTAR(4,2)*C4))+((FSTAR(5,1)*VPK(3,5,1))+(
     &   FSTAR(5,2)*VPK(3,5,2))))))))
        FS0(4)= (UTAU(4)-((((FSTAR(8,1)*VPK(4,8,1))+(FSTAR(8,2)*
     &   VPK(4,8,2)))-UTK(6,3))+((((FSTAR(7,1)*VPK(4,7,1))+(FSTAR(7,2)*
     &   VPK(4,7,2)))-UTK(5,3))+((((FSTAR(6,1)*VPK(4,6,1))+(FSTAR(6,2)*
     &   VPK(4,6,2)))-UTK(4,3))+((((2D0*(FSTAR(5,2)*S5))+(FSTAR(5,1)*
     &   VPK(4,5,1)))-UTK(3,3))-(FSTAR(4,1)+UTK(2,3)))))))
        FS0(5)= (UTAU(5)-((((FSTAR(8,1)*VPK(5,8,1))+(FSTAR(8,2)*
     &   VPK(5,8,2)))-UTK(6,3))+((((FSTAR(7,1)*VPK(5,7,1))+(FSTAR(7,2)*
     &   VPK(5,7,2)))-UTK(5,3))+((((2D0*(FSTAR(6,2)*S6))+(FSTAR(6,1)*
     &   VPK(5,6,1)))-UTK(4,3))-(FSTAR(5,1)+UTK(3,3))))))
        FS0(6)= (UTAU(6)-((((FSTAR(8,1)*VPK(6,8,1))+(FSTAR(8,2)*
     &   VPK(6,8,2)))-UTK(6,3))+((((2D0*(FSTAR(7,2)*S7))+(FSTAR(7,1)*
     &   VPK(6,7,1)))-UTK(5,3))-(FSTAR(6,1)+UTK(4,3)))))
        FS0(7)= (UTAU(7)-((((2D0*(FSTAR(8,2)*S8))+(FSTAR(8,1)*VPK(7,8,1)
     &   ))-UTK(6,3))-(FSTAR(7,1)+UTK(5,3))))
        FS0(8)= (UTAU(8)+(FSTAR(8,1)+UTK(6,3)))
        FS0FLG= 1
      ENDIF
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   86 adds/subtracts/negates
C                      54 multiplies
C                       0 divides
C                      18 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDDOMM(ROUTINE)
      INTEGER ROUTINE
C
      INTEGER DUMROUTINE,ERRNUM
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
        MM(1,1)= (251D0+(((VPK(1,8,1)*VPK(1,8,1))+(VPK(1,8,2)*VPK(1,8,2)
     &   ))+(((VPK(1,7,1)*VPK(1,7,1))+(VPK(1,7,2)*VPK(1,7,2)))+(((
     &   VPK(1,6,1)*VPK(1,6,1))+(VPK(1,6,2)*VPK(1,6,2)))+(((VPK(1,4,1)*
     &   VPK(1,4,1))+(VPK(1,4,2)*VPK(1,4,2)))+((VPK(1,5,1)*VPK(1,5,1))+(
     &   VPK(1,5,2)*VPK(1,5,2))))))))
        MM(1,2)= (((VPK(1,8,1)*VPK(2,8,1))+(VPK(1,8,2)*VPK(2,8,2)))+(((
     &   VPK(1,7,1)*VPK(2,7,1))+(VPK(1,7,2)*VPK(2,7,2)))+(((VPK(1,6,1)*
     &   VPK(2,6,1))+(VPK(1,6,2)*VPK(2,6,2)))+(((VPK(1,4,1)*C4)-(
     &   VPK(1,4,2)*S4))+((VPK(1,5,1)*VPK(2,5,1))+(VPK(1,5,2)*VPK(2,5,2)
     &   ))))))
        MM(1,3)= (((VPK(1,8,1)*VPK(3,8,1))+(VPK(1,8,2)*VPK(3,8,2)))+(((
     &   VPK(1,7,1)*VPK(3,7,1))+(VPK(1,7,2)*VPK(3,7,2)))+(((VPK(1,6,1)*
     &   VPK(3,6,1))+(VPK(1,6,2)*VPK(3,6,2)))+(((VPK(1,4,1)*S4)+(
     &   VPK(1,4,2)*C4))+((VPK(1,5,1)*VPK(3,5,1))+(VPK(1,5,2)*VPK(3,5,2)
     &   ))))))
        MM(1,4)= (250D0+(((VPK(1,8,1)*VPK(4,8,1))+(VPK(1,8,2)*VPK(4,8,2)
     &   ))+(((VPK(1,7,1)*VPK(4,7,1))+(VPK(1,7,2)*VPK(4,7,2)))+(((
     &   VPK(1,6,1)*VPK(4,6,1))+(VPK(1,6,2)*VPK(4,6,2)))+(((2D0*(
     &   VPK(1,5,2)*S5))+(VPK(1,5,1)*VPK(4,5,1)))-VPK(1,4,1))))))
        MM(1,5)= (200D0+(((VPK(1,8,1)*VPK(5,8,1))+(VPK(1,8,2)*VPK(5,8,2)
     &   ))+(((VPK(1,7,1)*VPK(5,7,1))+(VPK(1,7,2)*VPK(5,7,2)))+(((2D0*(
     &   VPK(1,6,2)*S6))+(VPK(1,6,1)*VPK(5,6,1)))-VPK(1,5,1)))))
        MM(1,6)= (150D0+(((VPK(1,8,1)*VPK(6,8,1))+(VPK(1,8,2)*VPK(6,8,2)
     &   ))+(((2D0*(VPK(1,7,2)*S7))+(VPK(1,7,1)*VPK(6,7,1)))-VPK(1,6,1))
     &   ))
        MM(1,7)= (100D0+(((2D0*(VPK(1,8,2)*S8))+(VPK(1,8,1)*VPK(7,8,1)))
     &   -VPK(1,7,1)))
        MM(1,8)= (50D0-VPK(1,8,1))
        MM(2,2)= (1D0+(((VPK(2,8,1)*VPK(2,8,1))+(VPK(2,8,2)*VPK(2,8,2)))
     &   +(((VPK(2,7,1)*VPK(2,7,1))+(VPK(2,7,2)*VPK(2,7,2)))+(((
     &   VPK(2,5,1)*VPK(2,5,1))+(VPK(2,5,2)*VPK(2,5,2)))+((VPK(2,6,1)*
     &   VPK(2,6,1))+(VPK(2,6,2)*VPK(2,6,2)))))))
        MM(2,3)= (((VPK(2,8,1)*VPK(3,8,1))+(VPK(2,8,2)*VPK(3,8,2)))+(((
     &   VPK(2,7,1)*VPK(3,7,1))+(VPK(2,7,2)*VPK(3,7,2)))+(((VPK(2,5,1)*
     &   VPK(3,5,1))+(VPK(2,5,2)*VPK(3,5,2)))+((VPK(2,6,1)*VPK(3,6,1))+(
     &   VPK(2,6,2)*VPK(3,6,2))))))
        MM(2,4)= (((VPK(2,8,1)*VPK(4,8,1))+(VPK(2,8,2)*VPK(4,8,2)))+(((
     &   VPK(2,7,1)*VPK(4,7,1))+(VPK(2,7,2)*VPK(4,7,2)))+(((VPK(2,6,1)*
     &   VPK(4,6,1))+(VPK(2,6,2)*VPK(4,6,2)))+(((2D0*(VPK(2,5,2)*S5))+(
     &   VPK(2,5,1)*VPK(4,5,1)))-C4))))
        MM(2,5)= (((VPK(2,8,1)*VPK(5,8,1))+(VPK(2,8,2)*VPK(5,8,2)))+(((
     &   VPK(2,7,1)*VPK(5,7,1))+(VPK(2,7,2)*VPK(5,7,2)))+(((2D0*(
     &   VPK(2,6,2)*S6))+(VPK(2,6,1)*VPK(5,6,1)))-VPK(2,5,1))))
        MM(2,6)= (((VPK(2,8,1)*VPK(6,8,1))+(VPK(2,8,2)*VPK(6,8,2)))+(((
     &   2D0*(VPK(2,7,2)*S7))+(VPK(2,7,1)*VPK(6,7,1)))-VPK(2,6,1)))
        MM(2,7)= (((2D0*(VPK(2,8,2)*S8))+(VPK(2,8,1)*VPK(7,8,1)))-
     &   VPK(2,7,1))
        MM(2,8)= -VPK(2,8,1)
        MM(3,3)= (1D0+(((VPK(3,8,1)*VPK(3,8,1))+(VPK(3,8,2)*VPK(3,8,2)))
     &   +(((VPK(3,7,1)*VPK(3,7,1))+(VPK(3,7,2)*VPK(3,7,2)))+(((
     &   VPK(3,5,1)*VPK(3,5,1))+(VPK(3,5,2)*VPK(3,5,2)))+((VPK(3,6,1)*
     &   VPK(3,6,1))+(VPK(3,6,2)*VPK(3,6,2)))))))
        MM(3,4)= (((VPK(3,8,1)*VPK(4,8,1))+(VPK(3,8,2)*VPK(4,8,2)))+(((
     &   VPK(3,7,1)*VPK(4,7,1))+(VPK(3,7,2)*VPK(4,7,2)))+(((VPK(3,6,1)*
     &   VPK(4,6,1))+(VPK(3,6,2)*VPK(4,6,2)))+(((2D0*(VPK(3,5,2)*S5))+(
     &   VPK(3,5,1)*VPK(4,5,1)))-S4))))
        MM(3,5)= (((VPK(3,8,1)*VPK(5,8,1))+(VPK(3,8,2)*VPK(5,8,2)))+(((
     &   VPK(3,7,1)*VPK(5,7,1))+(VPK(3,7,2)*VPK(5,7,2)))+(((2D0*(
     &   VPK(3,6,2)*S6))+(VPK(3,6,1)*VPK(5,6,1)))-VPK(3,5,1))))
        MM(3,6)= (((VPK(3,8,1)*VPK(6,8,1))+(VPK(3,8,2)*VPK(6,8,2)))+(((
     &   2D0*(VPK(3,7,2)*S7))+(VPK(3,7,1)*VPK(6,7,1)))-VPK(3,6,1)))
        MM(3,7)= (((2D0*(VPK(3,8,2)*S8))+(VPK(3,8,1)*VPK(7,8,1)))-
     &   VPK(3,7,1))
        MM(3,8)= -VPK(3,8,1)
        MM(4,4)= (251D0+(((VPK(4,8,1)*VPK(4,8,1))+(VPK(4,8,2)*VPK(4,8,2)
     &   ))+(((VPK(4,7,1)*VPK(4,7,1))+(VPK(4,7,2)*VPK(4,7,2)))+(((4D0*(
     &   S5*S5))+(VPK(4,5,1)*VPK(4,5,1)))+((VPK(4,6,1)*VPK(4,6,1))+(
     &   VPK(4,6,2)*VPK(4,6,2)))))))
        MM(4,5)= (200D0+(((VPK(4,8,1)*VPK(5,8,1))+(VPK(4,8,2)*VPK(5,8,2)
     &   ))+(((VPK(4,7,1)*VPK(5,7,1))+(VPK(4,7,2)*VPK(5,7,2)))+(((2D0*(
     &   VPK(4,6,2)*S6))+(VPK(4,6,1)*VPK(5,6,1)))-VPK(4,5,1)))))
        MM(4,6)= (150D0+(((VPK(4,8,1)*VPK(6,8,1))+(VPK(4,8,2)*VPK(6,8,2)
     &   ))+(((2D0*(VPK(4,7,2)*S7))+(VPK(4,7,1)*VPK(6,7,1)))-VPK(4,6,1))
     &   ))
        MM(4,7)= (100D0+(((2D0*(VPK(4,8,2)*S8))+(VPK(4,8,1)*VPK(7,8,1)))
     &   -VPK(4,7,1)))
        MM(4,8)= (50D0-VPK(4,8,1))
        MM(5,5)= (201D0+(((VPK(5,8,1)*VPK(5,8,1))+(VPK(5,8,2)*VPK(5,8,2)
     &   ))+(((4D0*(S6*S6))+(VPK(5,6,1)*VPK(5,6,1)))+((VPK(5,7,1)*
     &   VPK(5,7,1))+(VPK(5,7,2)*VPK(5,7,2))))))
        MM(5,6)= (150D0+(((VPK(5,8,1)*VPK(6,8,1))+(VPK(5,8,2)*VPK(6,8,2)
     &   ))+(((2D0*(VPK(5,7,2)*S7))+(VPK(5,7,1)*VPK(6,7,1)))-VPK(5,6,1))
     &   ))
        MM(5,7)= (100D0+(((2D0*(VPK(5,8,2)*S8))+(VPK(5,8,1)*VPK(7,8,1)))
     &   -VPK(5,7,1)))
        MM(5,8)= (50D0-VPK(5,8,1))
        MM(6,6)= (151D0+(((4D0*(S7*S7))+(VPK(6,7,1)*VPK(6,7,1)))+((
     &   VPK(6,8,1)*VPK(6,8,1))+(VPK(6,8,2)*VPK(6,8,2)))))
        MM(6,7)= (100D0+(((2D0*(VPK(6,8,2)*S8))+(VPK(6,8,1)*VPK(7,8,1)))
     &   -VPK(6,7,1)))
        MM(6,8)= (50D0-VPK(6,8,1))
        MM(7,7)= (101D0+((4D0*(S8*S8))+(VPK(7,8,1)*VPK(7,8,1))))
        MM(7,8)= (50D0-VPK(7,8,1))
        MM(8,8)= 51D0
C
C Check for singular mass matrix
C
        DO 100 I=1,8
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
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain  168 adds/subtracts/negates
C                     176 multiplies
C                       0 divides
C                      36 assignments
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
        CALL SDLDUDCOMP(8,8,MMAP,1D-13,WORKSS,WORKS,MM,MLO,MDI)
C
C Check for singular mass matrix
C
        DO 100 I=1,8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      MTAU(1)= IMULT(1)
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
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
      DOUBLE PRECISION TAU(8)
C
C Compute tree hinge torques to match effect of applied loads
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DOUBLE PRECISION FSTAREQ(8,3),TSTAREQ(8,3)
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
      TAU(1)= (UTAU(1)+((UTK(6,3)+((UFK(6,1)*VPK(1,8,1))+(UFK(6,2)*
     & VPK(1,8,2))))+((UTK(5,3)+((UFK(5,1)*VPK(1,7,1))+(UFK(5,2)*
     & VPK(1,7,2))))+((UTK(4,3)+((UFK(4,1)*VPK(1,6,1))+(UFK(4,2)*
     & VPK(1,6,2))))+((UTK(1,3)+(UTK(2,3)+((UFK(2,1)*VPK(1,4,1))+(
     & UFK(2,2)*VPK(1,4,2)))))+(UTK(3,3)+((UFK(3,1)*VPK(1,5,1))+(
     & UFK(3,2)*VPK(1,5,2)))))))))
      TAU(2)= (UTAU(2)-((((((UFK(2,2)*S4)-(UFK(2,1)*C4))-((UFK(3,1)*
     & VPK(2,5,1))+(UFK(3,2)*VPK(2,5,2))))-((UFK(4,1)*VPK(2,6,1))+(
     & UFK(4,2)*VPK(2,6,2))))-((UFK(5,1)*VPK(2,7,1))+(UFK(5,2)*
     & VPK(2,7,2))))-((UFK(6,1)*VPK(2,8,1))+(UFK(6,2)*VPK(2,8,2)))))
      TAU(3)= (UTAU(3)+(((UFK(6,1)*VPK(3,8,1))+(UFK(6,2)*VPK(3,8,2)))+((
     & (UFK(5,1)*VPK(3,7,1))+(UFK(5,2)*VPK(3,7,2)))+(((UFK(4,1)*
     & VPK(3,6,1))+(UFK(4,2)*VPK(3,6,2)))+(((UFK(2,1)*S4)+(UFK(2,2)*C4))
     & +((UFK(3,1)*VPK(3,5,1))+(UFK(3,2)*VPK(3,5,2))))))))
      TAU(4)= (UTAU(4)-(((((UFK(2,1)-UTK(2,3))-(UTK(3,3)+((2D0*(UFK(3,2)
     & *S5))+(UFK(3,1)*VPK(4,5,1)))))-(UTK(4,3)+((UFK(4,1)*VPK(4,6,1))+(
     & UFK(4,2)*VPK(4,6,2)))))-(UTK(5,3)+((UFK(5,1)*VPK(4,7,1))+(
     & UFK(5,2)*VPK(4,7,2)))))-(UTK(6,3)+((UFK(6,1)*VPK(4,8,1))+(
     & UFK(6,2)*VPK(4,8,2))))))
      TAU(5)= (UTAU(5)-((((UFK(3,1)-UTK(3,3))-(UTK(4,3)+((2D0*(UFK(4,2)*
     & S6))+(UFK(4,1)*VPK(5,6,1)))))-(UTK(5,3)+((UFK(5,1)*VPK(5,7,1))+(
     & UFK(5,2)*VPK(5,7,2)))))-(UTK(6,3)+((UFK(6,1)*VPK(5,8,1))+(
     & UFK(6,2)*VPK(5,8,2))))))
      TAU(6)= (UTAU(6)-(((UFK(4,1)-UTK(4,3))-(UTK(5,3)+((2D0*(UFK(5,2)*
     & S7))+(UFK(5,1)*VPK(6,7,1)))))-(UTK(6,3)+((UFK(6,1)*VPK(6,8,1))+(
     & UFK(6,2)*VPK(6,8,2))))))
      TAU(7)= (UTAU(7)-((UFK(5,1)-UTK(5,3))-(UTK(6,3)+((2D0*(UFK(6,2)*S8
     & ))+(UFK(6,1)*VPK(7,8,1))))))
      TAU(8)= (UTAU(8)-(UFK(6,1)-UTK(6,3)))
C
C Op counts below do not include called subroutines
C
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   76 adds/subtracts/negates
C                      54 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFS0
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSMULT
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      FS(1)= MTAU(1)
      FS(2)= 0D0
      FS(3)= 0D0
      FS(4)= 0D0
      FS(5)= 0D0
      FS(6)= 0D0
      FS(7)= 0D0
      FS(8)= 0D0
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSFULL
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    8 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSGENMULT
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      FS(1)= (MTAU(1)+((MTK(6,3)+((MFK(6,1)*VPK(1,8,1))+(MFK(6,2)*
     & VPK(1,8,2))))+((MTK(5,3)+((MFK(5,1)*VPK(1,7,1))+(MFK(5,2)*
     & VPK(1,7,2))))+((MTK(4,3)+((MFK(4,1)*VPK(1,6,1))+(MFK(4,2)*
     & VPK(1,6,2))))+((MTK(1,3)+(MTK(2,3)+((MFK(2,1)*VPK(1,4,1))+(
     & MFK(2,2)*VPK(1,4,2)))))+(MTK(3,3)+((MFK(3,1)*VPK(1,5,1))+(
     & MFK(3,2)*VPK(1,5,2)))))))))
      FS(2)= (MTAU(2)-((((((MFK(2,2)*S4)-(MFK(2,1)*C4))-((MFK(3,1)*
     & VPK(2,5,1))+(MFK(3,2)*VPK(2,5,2))))-((MFK(4,1)*VPK(2,6,1))+(
     & MFK(4,2)*VPK(2,6,2))))-((MFK(5,1)*VPK(2,7,1))+(MFK(5,2)*
     & VPK(2,7,2))))-((MFK(6,1)*VPK(2,8,1))+(MFK(6,2)*VPK(2,8,2)))))
      FS(3)= (MTAU(3)+(((MFK(6,1)*VPK(3,8,1))+(MFK(6,2)*VPK(3,8,2)))+(((
     & MFK(5,1)*VPK(3,7,1))+(MFK(5,2)*VPK(3,7,2)))+(((MFK(4,1)*
     & VPK(3,6,1))+(MFK(4,2)*VPK(3,6,2)))+(((MFK(2,1)*S4)+(MFK(2,2)*C4))
     & +((MFK(3,1)*VPK(3,5,1))+(MFK(3,2)*VPK(3,5,2))))))))
      FS(4)= (MTAU(4)-(((((MFK(2,1)-MTK(2,3))-(MTK(3,3)+((2D0*(MFK(3,2)*
     & S5))+(MFK(3,1)*VPK(4,5,1)))))-(MTK(4,3)+((MFK(4,1)*VPK(4,6,1))+(
     & MFK(4,2)*VPK(4,6,2)))))-(MTK(5,3)+((MFK(5,1)*VPK(4,7,1))+(
     & MFK(5,2)*VPK(4,7,2)))))-(MTK(6,3)+((MFK(6,1)*VPK(4,8,1))+(
     & MFK(6,2)*VPK(4,8,2))))))
      FS(5)= (MTAU(5)-((((MFK(3,1)-MTK(3,3))-(MTK(4,3)+((2D0*(MFK(4,2)*
     & S6))+(MFK(4,1)*VPK(5,6,1)))))-(MTK(5,3)+((MFK(5,1)*VPK(5,7,1))+(
     & MFK(5,2)*VPK(5,7,2)))))-(MTK(6,3)+((MFK(6,1)*VPK(5,8,1))+(
     & MFK(6,2)*VPK(5,8,2))))))
      FS(6)= (MTAU(6)-(((MFK(4,1)-MTK(4,3))-(MTK(5,3)+((2D0*(MFK(5,2)*S7
     & ))+(MFK(5,1)*VPK(6,7,1)))))-(MTK(6,3)+((MFK(6,1)*VPK(6,8,1))+(
     & MFK(6,2)*VPK(6,8,2))))))
      FS(7)= (MTAU(7)-((MFK(5,1)-MTK(5,3))-(MTK(6,3)+((2D0*(MFK(6,2)*S8)
     & )+(MFK(6,1)*VPK(7,8,1))))))
      FS(8)= (MTAU(8)-(MFK(6,1)-MTK(6,3)))
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   76 adds/subtracts/negates
C                      54 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFSGENFULL
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    8 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDFULLTRQ(UDOTIN,MULTIN,TRQOUT)
      DOUBLE PRECISION UDOTIN(8),MULTIN(1),TRQOUT(8)
C
C Compute hinge torques which would produce indicated udots
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION FSTARR(8,3),TSTARR(8,3),OTKR(8,3),ATIR(8,3),ATKR(
     & 8,3)
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
      OTKR(4,3)= (UDOTIN(1)+UDOTIN(4))
      OTKR(5,3)= (OTKR(4,3)+UDOTIN(5))
      OTKR(6,3)= (OTKR(5,3)+UDOTIN(6))
      OTKR(7,3)= (OTKR(6,3)+UDOTIN(7))
      OTKR(8,3)= (OTKR(7,3)+UDOTIN(8))
      ATKR(2,1)= (UDOTIN(2)-(U(1)*WKRPK(2,2)))
      ATKR(2,2)= ((2D0*(U(1)*U(2)))+(Q(2)*UDOTIN(1)))
      ATKR(3,1)= (ATKR(2,1)-((2D0*(U(1)*U(3)))+(Q(3)*UDOTIN(1))))
      ATKR(3,2)= (UDOTIN(3)+(ATKR(2,2)+(U(1)*WKRPK(3,1))))
      ATKR(4,1)= (((ATKR(3,1)*C4)+(ATKR(3,2)*S4))-OTKR(4,3))
      ATKR(4,2)= (((ATKR(3,2)*C4)-(ATKR(3,1)*S4))-(WK(4,3)*WK(4,3)))
      ATIR(5,1)= (ATKR(4,1)-OTKR(4,3))
      ATIR(5,2)= (ATKR(4,2)-(WK(4,3)*WK(4,3)))
      ATKR(5,1)= (((ATIR(5,1)*C5)+(ATIR(5,2)*S5))-OTKR(5,3))
      ATKR(5,2)= (((ATIR(5,2)*C5)-(ATIR(5,1)*S5))-(WK(5,3)*WK(5,3)))
      ATIR(6,1)= (ATKR(5,1)-OTKR(5,3))
      ATIR(6,2)= (ATKR(5,2)-(WK(5,3)*WK(5,3)))
      ATKR(6,1)= (((ATIR(6,1)*C6)+(ATIR(6,2)*S6))-OTKR(6,3))
      ATKR(6,2)= (((ATIR(6,2)*C6)-(ATIR(6,1)*S6))-(WK(6,3)*WK(6,3)))
      ATIR(7,1)= (ATKR(6,1)-OTKR(6,3))
      ATIR(7,2)= (ATKR(6,2)-(WK(6,3)*WK(6,3)))
      ATKR(7,1)= (((ATIR(7,1)*C7)+(ATIR(7,2)*S7))-OTKR(7,3))
      ATKR(7,2)= (((ATIR(7,2)*C7)-(ATIR(7,1)*S7))-(WK(7,3)*WK(7,3)))
      ATIR(8,1)= (ATKR(7,1)-OTKR(7,3))
      ATIR(8,2)= (ATKR(7,2)-(WK(7,3)*WK(7,3)))
      ATKR(8,1)= (((ATIR(8,1)*C8)+(ATIR(8,2)*S8))-OTKR(8,3))
      ATKR(8,2)= (((ATIR(8,2)*C8)-(ATIR(8,1)*S8))-(WK(8,3)*WK(8,3)))
C
C Accumulate all forces and torques
C
      FSTARR(4,1)= (UFK(2,1)-ATKR(4,1))
      FSTARR(4,2)= (UFK(2,2)-ATKR(4,2))
      FSTARR(5,1)= (UFK(3,1)-ATKR(5,1))
      FSTARR(5,2)= (UFK(3,2)-ATKR(5,2))
      FSTARR(6,1)= (UFK(4,1)-ATKR(6,1))
      FSTARR(6,2)= (UFK(4,2)-ATKR(6,2))
      FSTARR(7,1)= (UFK(5,1)-ATKR(7,1))
      FSTARR(7,2)= (UFK(5,2)-ATKR(7,2))
      FSTARR(8,1)= (UFK(6,1)-ATKR(8,1))
      FSTARR(8,2)= (UFK(6,2)-ATKR(8,2))
      TSTARR(1,3)= (UTK(1,3)-UDOTIN(1))
      TSTARR(4,3)= (UTK(2,3)-(50D0*OTKR(4,3)))
      TSTARR(5,3)= (UTK(3,3)-(50D0*OTKR(5,3)))
      TSTARR(6,3)= (UTK(4,3)-(50D0*OTKR(6,3)))
      TSTARR(7,3)= (UTK(5,3)-(50D0*OTKR(7,3)))
      TSTARR(8,3)= (UTK(6,3)-(50D0*OTKR(8,3)))
C
C Now calculate the torques
C
      CALL SDDOVPK
      TRQOUT(1)= -((MTAU(1)+UTAU(1))+((TSTARR(8,3)+((FSTARR(8,1)*
     & VPK(1,8,1))+(FSTARR(8,2)*VPK(1,8,2))))+((TSTARR(7,3)+((
     & FSTARR(7,1)*VPK(1,7,1))+(FSTARR(7,2)*VPK(1,7,2))))+((TSTARR(6,3)+
     & ((FSTARR(6,1)*VPK(1,6,1))+(FSTARR(6,2)*VPK(1,6,2))))+((
     & TSTARR(1,3)+(TSTARR(4,3)+((FSTARR(4,1)*VPK(1,4,1))+(FSTARR(4,2)*
     & VPK(1,4,2)))))+(TSTARR(5,3)+((FSTARR(5,1)*VPK(1,5,1))+(
     & FSTARR(5,2)*VPK(1,5,2)))))))))
      TRQOUT(2)= -(UTAU(2)+(((FSTARR(8,1)*VPK(2,8,1))+(FSTARR(8,2)*
     & VPK(2,8,2)))+(((FSTARR(7,1)*VPK(2,7,1))+(FSTARR(7,2)*VPK(2,7,2)))
     & +(((FSTARR(6,1)*VPK(2,6,1))+(FSTARR(6,2)*VPK(2,6,2)))+(((
     & FSTARR(4,1)*C4)-(FSTARR(4,2)*S4))+((FSTARR(5,1)*VPK(2,5,1))+(
     & FSTARR(5,2)*VPK(2,5,2))))))))
      TRQOUT(3)= -(UTAU(3)+(((FSTARR(8,1)*VPK(3,8,1))+(FSTARR(8,2)*
     & VPK(3,8,2)))+(((FSTARR(7,1)*VPK(3,7,1))+(FSTARR(7,2)*VPK(3,7,2)))
     & +(((FSTARR(6,1)*VPK(3,6,1))+(FSTARR(6,2)*VPK(3,6,2)))+(((
     & FSTARR(4,1)*S4)+(FSTARR(4,2)*C4))+((FSTARR(5,1)*VPK(3,5,1))+(
     & FSTARR(5,2)*VPK(3,5,2))))))))
      TRQOUT(4)= -(UTAU(4)+((TSTARR(8,3)+((FSTARR(8,1)*VPK(4,8,1))+(
     & FSTARR(8,2)*VPK(4,8,2))))+((TSTARR(7,3)+((FSTARR(7,1)*VPK(4,7,1))
     & +(FSTARR(7,2)*VPK(4,7,2))))+((TSTARR(6,3)+((FSTARR(6,1)*
     & VPK(4,6,1))+(FSTARR(6,2)*VPK(4,6,2))))+((TSTARR(4,3)-FSTARR(4,1))
     & +(TSTARR(5,3)+((2D0*(FSTARR(5,2)*S5))+(FSTARR(5,1)*VPK(4,5,1)))))
     & ))))
      TRQOUT(5)= -(UTAU(5)+((TSTARR(8,3)+((FSTARR(8,1)*VPK(5,8,1))+(
     & FSTARR(8,2)*VPK(5,8,2))))+((TSTARR(7,3)+((FSTARR(7,1)*VPK(5,7,1))
     & +(FSTARR(7,2)*VPK(5,7,2))))+((TSTARR(5,3)-FSTARR(5,1))+(
     & TSTARR(6,3)+((2D0*(FSTARR(6,2)*S6))+(FSTARR(6,1)*VPK(5,6,1)))))))
     & )
      TRQOUT(6)= -(UTAU(6)+((TSTARR(8,3)+((FSTARR(8,1)*VPK(6,8,1))+(
     & FSTARR(8,2)*VPK(6,8,2))))+((TSTARR(6,3)-FSTARR(6,1))+(TSTARR(7,3)
     & +((2D0*(FSTARR(7,2)*S7))+(FSTARR(7,1)*VPK(6,7,1)))))))
      TRQOUT(7)= -(UTAU(7)+((TSTARR(7,3)-FSTARR(7,1))+(TSTARR(8,3)+((2D0
     & *(FSTARR(8,2)*S8))+(FSTARR(8,1)*VPK(7,8,1))))))
      TRQOUT(8)= -(UTAU(8)+(TSTARR(8,3)-FSTARR(8,1)))
C
C Op counts below do not include called subroutines
C
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain  140 adds/subtracts/negates
C                      96 multiplies
C                       0 divides
C                      51 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDCOMPTRQ(UDOTIN,TRQOUT)
      DOUBLE PRECISION UDOTIN(8),TRQOUT(8)
C
C Compute hinge torques to produce these udots, ignoring constraints
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION MULTIN(1),TRQOUT(8)
C
C Compute hinge trqs which would be produced by these mults.
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      DO 100 I=1,8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      TAUC(1)= (MTAU(1)+UTAU(1))
      TAUC(2)= UTAU(2)
      TAUC(3)= UTAU(3)
      TAUC(4)= UTAU(4)
      TAUC(5)= UTAU(5)
      TAUC(6)= UTAU(6)
      TAUC(7)= UTAU(7)
      TAUC(8)= UTAU(8)
      CALL SDDOINER
C
C Compute onk & onb (angular accels in N)
C
      ONKB(4,3)= (UDOT(1)+UDOT(4))
      ONKB(5,3)= (ONKB(4,3)+UDOT(5))
      ONKB(6,3)= (ONKB(5,3)+UDOT(6))
      ONKB(7,3)= (ONKB(6,3)+UDOT(7))
      ONKB(8,3)= (ONKB(7,3)+UDOT(8))
      ONB(1,1)= 0D0
      ONB(1,2)= 0D0
      ONB(1,3)= UDOT(1)
      ONB(2,1)= 0D0
      ONB(2,2)= 0D0
      ONB(2,3)= ONKB(4,3)
      ONB(3,1)= 0D0
      ONB(3,2)= 0D0
      ONB(3,3)= ONKB(5,3)
      ONB(4,1)= 0D0
      ONB(4,2)= 0D0
      ONB(4,3)= ONKB(6,3)
      ONB(5,1)= 0D0
      ONB(5,2)= 0D0
      ONB(5,3)= ONKB(7,3)
      ONB(6,1)= 0D0
      ONB(6,2)= 0D0
      ONB(6,3)= ONKB(8,3)
C
C Compute acceleration dyadics
C
      DYAD(1,1,1)= -W2W2(1)
      DYAD(1,1,2)= -UDOT(1)
      DYAD(1,1,3)= 0D0
      DYAD(1,2,1)= UDOT(1)
      DYAD(1,2,2)= -W2W2(1)
      DYAD(1,2,3)= 0D0
      DYAD(1,3,1)= 0D0
      DYAD(1,3,2)= 0D0
      DYAD(1,3,3)= 0D0
      DYAD(2,1,1)= -W2W2(2)
      DYAD(2,1,2)= -ONKB(4,3)
      DYAD(2,1,3)= 0D0
      DYAD(2,2,1)= ONKB(4,3)
      DYAD(2,2,2)= -W2W2(2)
      DYAD(2,2,3)= 0D0
      DYAD(2,3,1)= 0D0
      DYAD(2,3,2)= 0D0
      DYAD(2,3,3)= 0D0
      DYAD(3,1,1)= -W2W2(3)
      DYAD(3,1,2)= -ONKB(5,3)
      DYAD(3,1,3)= 0D0
      DYAD(3,2,1)= ONKB(5,3)
      DYAD(3,2,2)= -W2W2(3)
      DYAD(3,2,3)= 0D0
      DYAD(3,3,1)= 0D0
      DYAD(3,3,2)= 0D0
      DYAD(3,3,3)= 0D0
      DYAD(4,1,1)= -W2W2(4)
      DYAD(4,1,2)= -ONKB(6,3)
      DYAD(4,1,3)= 0D0
      DYAD(4,2,1)= ONKB(6,3)
      DYAD(4,2,2)= -W2W2(4)
      DYAD(4,2,3)= 0D0
      DYAD(4,3,1)= 0D0
      DYAD(4,3,2)= 0D0
      DYAD(4,3,3)= 0D0
      DYAD(5,1,1)= -W2W2(5)
      DYAD(5,1,2)= -ONKB(7,3)
      DYAD(5,1,3)= 0D0
      DYAD(5,2,1)= ONKB(7,3)
      DYAD(5,2,2)= -W2W2(5)
      DYAD(5,2,3)= 0D0
      DYAD(5,3,1)= 0D0
      DYAD(5,3,2)= 0D0
      DYAD(5,3,3)= 0D0
      DYAD(6,1,1)= -W2W2(6)
      DYAD(6,1,2)= -ONKB(8,3)
      DYAD(6,1,3)= 0D0
      DYAD(6,2,1)= ONKB(8,3)
      DYAD(6,2,2)= -W2W2(6)
      DYAD(6,2,3)= 0D0
      DYAD(6,3,1)= 0D0
      DYAD(6,3,2)= 0D0
      DYAD(6,3,3)= 0D0
C
C Compute ank & anb (mass center linear accels in N)
C
      ANKB(2,2)= (Q(2)*UDOT(1))
      ANKB(3,1)= (UDOT(2)-(Q(3)*UDOT(1)))
      ANKB(3,2)= (ANKB(2,2)+UDOT(3))
      ANKB(4,1)= (((ANKB(3,1)*C4)+(ANKB(3,2)*S4))-ONKB(4,3))
      ANKB(4,2)= ((ANKB(3,2)*C4)-(ANKB(3,1)*S4))
      AONKRI(5,1)= (ANKB(4,1)-ONKB(4,3))
      ANKB(5,1)= (((ANKB(4,2)*S5)+(AONKRI(5,1)*C5))-ONKB(5,3))
      ANKB(5,2)= ((ANKB(4,2)*C5)-(AONKRI(5,1)*S5))
      AONKRI(6,1)= (ANKB(5,1)-ONKB(5,3))
      ANKB(6,1)= (((ANKB(5,2)*S6)+(AONKRI(6,1)*C6))-ONKB(6,3))
      ANKB(6,2)= ((ANKB(5,2)*C6)-(AONKRI(6,1)*S6))
      AONKRI(7,1)= (ANKB(6,1)-ONKB(6,3))
      ANKB(7,1)= (((ANKB(6,2)*S7)+(AONKRI(7,1)*C7))-ONKB(7,3))
      ANKB(7,2)= ((ANKB(6,2)*C7)-(AONKRI(7,1)*S7))
      AONKRI(8,1)= (ANKB(7,1)-ONKB(7,3))
      ANKB(8,1)= (((ANKB(7,2)*S8)+(AONKRI(8,1)*C8))-ONKB(8,3))
      ANKB(8,2)= ((ANKB(7,2)*C8)-(AONKRI(8,1)*S8))
      ANKATK(4,1)= (ANKB(4,1)+ATK(4,1))
      ANKATK(4,2)= (ANKB(4,2)+ATK(4,2))
      ANK(4,1)= ((ANKATK(4,1)*CNK(4,1,1))+(ANKATK(4,2)*CNK(4,1,2)))
      ANK(4,2)= ((ANKATK(4,1)*CNK(4,2,1))+(ANKATK(4,2)*CNK(4,2,2)))
      ANKATK(5,1)= (ANKB(5,1)+ATK(5,1))
      ANKATK(5,2)= (ANKB(5,2)+ATK(5,2))
      ANK(5,1)= ((ANKATK(5,1)*CNK(5,1,1))+(ANKATK(5,2)*CNK(5,1,2)))
      ANK(5,2)= ((ANKATK(5,1)*CNK(5,2,1))+(ANKATK(5,2)*CNK(5,2,2)))
      ANKATK(6,1)= (ANKB(6,1)+ATK(6,1))
      ANKATK(6,2)= (ANKB(6,2)+ATK(6,2))
      ANK(6,1)= ((ANKATK(6,1)*CNK(6,1,1))+(ANKATK(6,2)*CNK(6,1,2)))
      ANK(6,2)= ((ANKATK(6,1)*CNK(6,2,1))+(ANKATK(6,2)*CNK(6,2,2)))
      ANKATK(7,1)= (ANKB(7,1)+ATK(7,1))
      ANKATK(7,2)= (ANKB(7,2)+ATK(7,2))
      ANK(7,1)= ((ANKATK(7,1)*CNK(7,1,1))+(ANKATK(7,2)*CNK(7,1,2)))
      ANK(7,2)= ((ANKATK(7,1)*CNK(7,2,1))+(ANKATK(7,2)*CNK(7,2,2)))
      ANKATK(8,1)= (ANKB(8,1)+ATK(8,1))
      ANKATK(8,2)= (ANKB(8,2)+ATK(8,2))
      ANK(8,1)= ((ANKATK(8,1)*CNK(8,1,1))+(ANKATK(8,2)*CNK(8,1,2)))
      ANK(8,2)= ((ANKATK(8,1)*CNK(8,2,1))+(ANKATK(8,2)*CNK(8,2,2)))
      ANB(1,1)= 0D0
      ANB(1,2)= 0D0
      ANB(1,3)= 0D0
      ANB(2,1)= ANK(4,1)
      ANB(2,2)= ANK(4,2)
      ANB(2,3)= 0D0
      ANB(3,1)= ANK(5,1)
      ANB(3,2)= ANK(5,2)
      ANB(3,3)= 0D0
      ANB(4,1)= ANK(6,1)
      ANB(4,2)= ANK(6,2)
      ANB(4,3)= 0D0
      ANB(5,1)= ANK(7,1)
      ANB(5,2)= ANK(7,2)
      ANB(5,3)= 0D0
      ANB(6,1)= ANK(8,1)
      ANB(6,2)= ANK(8,2)
      ANB(6,3)= 0D0
C
C Compute constraint acceleration errors
C
      ROUSTATE= 3
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   65 adds/subtracts/negates
C                      42 multiplies
C                       0 divides
C                     140 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDMASSMAT(MMAT)
      DOUBLE PRECISION MMAT(8,8)
C
C Return the system mass matrix (LHS)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DO 110 I=1,8
        DO 100 J=I,8
          MMAT(I,J)= MM(I,J)
          MMAT(J,I)= MM(I,J)
100     CONTINUE
110   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDFRCMAT(FMAT)
      DOUBLE PRECISION FMAT(8)
C
C Return the system force matrix (RHS), excluding constraints
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DO 100 I=1,8
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(26,23)
        RETURN
      ENDIF
      PERR(1)= (Q(1)-UPOS(1))
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(27,23)
        RETURN
      ENDIF
      VERR(1)= (U(1)-UVEL(1))
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
C
      IF (ROUSTATE .NE. 3) THEN
        CALL SDSETERR(35,24)
        RETURN
      ENDIF
      AERR(1)= (UDOT(1)-UACC(1))
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
      IF ((BNUM .LT. 0) .OR. (BNUM .GT. 6)) THEN
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
      IF ((JNUM .LT. 1) .OR. (JNUM .GT. 6)) THEN
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION PP(1,8),DPP(8,1)
      INTEGER I,J,C
      DOUBLE PRECISION SUM
      DOUBLE PRECISION DFK(6,3),DTK(6,3),DTAU(8),DLTCI(1,3),DLTC(1,3),
     & DLFCI(1,3),DLFC(1,3)
      DOUBLE PRECISION DTINB(1,3),DTOUTB(1,3),DLTAUFI(1,3),DLTAUFO(1,3),
     & DLTAUTI(1,3),DLTAUTO(1,3)
      DOUBLE PRECISION DFS(8),ROW(8),DINVROW(8)
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
        DFS(1)= 1D0
        DFS(2)= 0D0
        DFS(3)= 0D0
        DFS(4)= 0D0
        DFS(5)= 0D0
        DFS(6)= 0D0
        DFS(7)= 0D0
        DFS(8)= 0D0
        CALL SDLDUBSL(8,8,MMAP,MLO,DFS,ROW)
        CALL SDLDUBSD(8,8,MMAP,MDI,ROW,DINVROW)
        DO 100 I=1,8
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
            DO 101 J=1,8
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
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    8 adds/subtracts/negates
C                       8 multiplies
C                       0 divides
C                      35 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDXUDOT0(ROUTINE,OUDOT0)
      INTEGER ROUTINE
      DOUBLE PRECISION OUDOT0(8)
C
C
C Compute unconstrained equations
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      CALL SDLHS(ROUTINE)
C
C Solve equations ignoring constraints
C
      CALL SDFS0
      CALL SDLDUBSLV(8,8,MMAP,WORKS,MLO,MDI,FS,UDOT)
      DO 5 I=1,8
        OUDOT0(I)= UDOT(I)
5     CONTINUE
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDUDOT0(OUDOT0)
      DOUBLE PRECISION OUDOT0(8)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION IUDOT(8)
C
C
C Assign udots and advance to stage Dynamics Ready
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(68,23)
        RETURN
      ENDIF
      DO 5 I=1,8
        UDOT(I)= IUDOT(I)
5     CONTINUE
      CALL SDRHS
C
      RETURN
      END
C
      SUBROUTINE SDXUDOTM(ROUTINE,IMULT,OUDOTM)
      INTEGER ROUTINE
      DOUBLE PRECISION IMULT(1),OUDOTM(8)
C
C
C Compute udots due only to multipliers
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      INTEGER I
C
      CALL SDLHS(ROUTINE)
      CALL SDMFRC(IMULT)
      CALL SDFSMULT
      CALL SDLDUBSLV(8,8,MMAP,WORKS,MLO,MDI,FS,UDOT)
      DO 10 I=1,8
        OUDOTM(I)= UDOT(I)
10    CONTINUE
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    0 adds/subtracts/negates
C                       0 multiplies
C                       0 divides
C                       8 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDUDOTM(IMULT,OUDOTM)
      DOUBLE PRECISION IMULT(1),OUDOTM(8)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION OQDOT(8),OUDOT(8)
C
C
C This is the derivative section for a 6-body ground-based
C system with 8 hinge degree(s) of freedom.
C 1 of the degrees of freedom follow(s) prescribed motion.
C There is 1 constraint.
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION WORKR(1),BB(1),B0(1),V0(1),P0(1)
      INTEGER IWORK(1)
      INTEGER I,J
      DOUBLE PRECISION UDOT0(8),UDOT1(8)
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
      DO 95 I=1,8
        UDOT(I)= UDOT0(I)+UDOT1(I)
95    CONTINUE
      CALL SDRHS
      DO 100 I=1,8
        OQDOT(I)= QDOT(I)
100   CONTINUE
      DO 110 I=1,8
        OUDOT(I)= UDOT(I)
110   CONTINUE
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   11 adds/subtracts/negates
C                       2 multiplies
C                       0 divides
C                      27 assignments
C
C
      RETURN
      END
C
C Compute residuals for use with DAE integrator
C
C
      SUBROUTINE SDRESID(EQDOT,EUDOT,EMULTS,RESID)
      DOUBLE PRECISION EQDOT(8),EUDOT(8),EMULTS(1),RESID(17)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
      COMMON/SDGRHS/ FS,UDOT,ANK,ANB,ONK,ONB,ONKB,AONKRI,ANKB,ANKATK,
     & DYRCOM,FFK,TTK,FCCIKT,FFKB,TTKB,DYAD,FC,TC,TAUC
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      INTEGER I
      DOUBLE PRECISION UDERRS(8),P0(1)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(16,23)
        RETURN
      ENDIF
      IF (STABPOSQ .EQ. 1) THEN
        CALL SDSETERR(16,33)
      ENDIF
      CALL SDFULLTRQ(EUDOT,EMULTS,UDERRS)
      DO 100 I=1,8
        RESID(I)= EQDOT(I)-QDOT(I)
100   CONTINUE
      DO 200 I=1,8
        RESID(8+I)= UDERRS(I)
200   CONTINUE
      CALL SDVERR(RESID(17))
      IF (STABPOS  .NE.  0D0) THEN
        CALL SDPERR(P0)
        DO 250 I=1,1
          RESID(16+I)= RESID(16+I)+STABPOS*P0(I)
250     CONTINUE
      ENDIF
      DO 350 I=1,8
        UDOT(I)= EUDOT(I)
350   CONTINUE
      DO 375 I=1,1
        MULT(I)= EMULTS(I)
375   CONTINUE
      CALL SDRHS
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain    9 adds/subtracts/negates
C                       1 multiplies
C                       0 divides
C                      26 assignments
C
C
      RETURN
      END
C
      SUBROUTINE SDMULT(OMULTS,OWRANK,OMULTMAP)
      DOUBLE PRECISION OMULTS(1)
      INTEGER OWRANK,OMULTMAP(1)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
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
      DOUBLE PRECISION FORCE(6,3),TORQUE(6,3)
C
C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      FC(8,1)= (ANKATK(8,1)-UFK(6,1))
      FC(8,2)= (ANKATK(8,2)-UFK(6,2))
      FC(8,3)= -UFK(6,3)
      TC(8,1)= -(UTK(6,1)-FC(8,3))
      TC(8,2)= -UTK(6,2)
      TC(8,3)= ((50D0*ONKB(8,3))-(FC(8,1)+UTK(6,3)))
      FCCIKT(8,1)= ((FC(8,1)*C8)-(FC(8,2)*S8))
      FCCIKT(8,2)= ((FC(8,1)*S8)+(FC(8,2)*C8))
      FCCIKT(8,3)= FC(8,3)
      FFK(7,1)= (UFK(5,1)-FCCIKT(8,1))
      FFK(7,2)= (UFK(5,2)-FCCIKT(8,2))
      FFK(7,3)= (UFK(5,3)-FCCIKT(8,3))
      TTK(7,1)= (UTK(5,1)-(FCCIKT(8,3)+((TC(8,1)*C8)-(TC(8,2)*S8))))
      TTK(7,2)= (UTK(5,2)-((TC(8,1)*S8)+(TC(8,2)*C8)))
      TTK(7,3)= (UTK(5,3)-(TC(8,3)-FCCIKT(8,1)))
      FC(7,1)= (ANKATK(7,1)-FFK(7,1))
      FC(7,2)= (ANKATK(7,2)-FFK(7,2))
      FC(7,3)= -FFK(7,3)
      TC(7,1)= -(TTK(7,1)-FC(7,3))
      TC(7,2)= -TTK(7,2)
      TC(7,3)= ((50D0*ONKB(7,3))-(FC(7,1)+TTK(7,3)))
      FCCIKT(7,1)= ((FC(7,1)*C7)-(FC(7,2)*S7))
      FCCIKT(7,2)= ((FC(7,1)*S7)+(FC(7,2)*C7))
      FCCIKT(7,3)= FC(7,3)
      FFK(6,1)= (UFK(4,1)-FCCIKT(7,1))
      FFK(6,2)= (UFK(4,2)-FCCIKT(7,2))
      FFK(6,3)= (UFK(4,3)-FCCIKT(7,3))
      TTK(6,1)= (UTK(4,1)-(FCCIKT(7,3)+((TC(7,1)*C7)-(TC(7,2)*S7))))
      TTK(6,2)= (UTK(4,2)-((TC(7,1)*S7)+(TC(7,2)*C7)))
      TTK(6,3)= (UTK(4,3)-(TC(7,3)-FCCIKT(7,1)))
      FC(6,1)= (ANKATK(6,1)-FFK(6,1))
      FC(6,2)= (ANKATK(6,2)-FFK(6,2))
      FC(6,3)= -FFK(6,3)
      TC(6,1)= -(TTK(6,1)-FC(6,3))
      TC(6,2)= -TTK(6,2)
      TC(6,3)= ((50D0*ONKB(6,3))-(FC(6,1)+TTK(6,3)))
      FCCIKT(6,1)= ((FC(6,1)*C6)-(FC(6,2)*S6))
      FCCIKT(6,2)= ((FC(6,1)*S6)+(FC(6,2)*C6))
      FCCIKT(6,3)= FC(6,3)
      FFK(5,1)= (UFK(3,1)-FCCIKT(6,1))
      FFK(5,2)= (UFK(3,2)-FCCIKT(6,2))
      FFK(5,3)= (UFK(3,3)-FCCIKT(6,3))
      TTK(5,1)= (UTK(3,1)-(FCCIKT(6,3)+((TC(6,1)*C6)-(TC(6,2)*S6))))
      TTK(5,2)= (UTK(3,2)-((TC(6,1)*S6)+(TC(6,2)*C6)))
      TTK(5,3)= (UTK(3,3)-(TC(6,3)-FCCIKT(6,1)))
      FC(5,1)= (ANKATK(5,1)-FFK(5,1))
      FC(5,2)= (ANKATK(5,2)-FFK(5,2))
      FC(5,3)= -FFK(5,3)
      TC(5,1)= -(TTK(5,1)-FC(5,3))
      TC(5,2)= -TTK(5,2)
      TC(5,3)= ((50D0*ONKB(5,3))-(FC(5,1)+TTK(5,3)))
      FCCIKT(5,1)= ((FC(5,1)*C5)-(FC(5,2)*S5))
      FCCIKT(5,2)= ((FC(5,1)*S5)+(FC(5,2)*C5))
      FCCIKT(5,3)= FC(5,3)
      FFK(4,1)= (UFK(2,1)-FCCIKT(5,1))
      FFK(4,2)= (UFK(2,2)-FCCIKT(5,2))
      FFK(4,3)= (UFK(2,3)-FCCIKT(5,3))
      TTK(4,1)= (UTK(2,1)-(FCCIKT(5,3)+((TC(5,1)*C5)-(TC(5,2)*S5))))
      TTK(4,2)= (UTK(2,2)-((TC(5,1)*S5)+(TC(5,2)*C5)))
      TTK(4,3)= (UTK(2,3)-(TC(5,3)-FCCIKT(5,1)))
      FC(4,1)= (ANKATK(4,1)-FFK(4,1))
      FC(4,2)= (ANKATK(4,2)-FFK(4,2))
      FC(4,3)= -FFK(4,3)
      TC(4,1)= -(TTK(4,1)-FC(4,3))
      TC(4,2)= -TTK(4,2)
      TC(4,3)= ((50D0*ONKB(4,3))-(FC(4,1)+TTK(4,3)))
      FCCIKT(4,1)= ((FC(4,1)*C4)-(FC(4,2)*S4))
      FCCIKT(4,2)= ((FC(4,1)*S4)+(FC(4,2)*C4))
      FCCIKT(4,3)= FC(4,3)
      FFK(3,1)= -FCCIKT(4,1)
      FFK(3,2)= -FCCIKT(4,2)
      FFK(3,3)= -FCCIKT(4,3)
      TTK(3,1)= -((TC(4,1)*C4)-(TC(4,2)*S4))
      TTK(3,2)= -((TC(4,1)*S4)+(TC(4,2)*C4))
      TTK(3,3)= -TC(4,3)
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
      TTK(2,1)= -(TC(3,1)+(FCCIKT(3,3)*Q(3)))
      TTK(2,2)= -TC(3,2)
      TTK(2,3)= -(TC(3,3)-(FCCIKT(3,1)*Q(3)))
      FC(2,1)= -FFK(2,1)
      FC(2,2)= -FFK(2,2)
      FC(2,3)= -FFK(2,3)
      TC(2,1)= -TTK(2,1)
      TC(2,2)= -TTK(2,2)
      TC(2,3)= -TTK(2,3)
      FCCIKT(2,1)= FC(2,1)
      FCCIKT(2,2)= FC(2,2)
      FCCIKT(2,3)= FC(2,3)
      FFK(1,1)= (UFK(1,1)-FCCIKT(2,1))
      FFK(1,2)= (UFK(1,2)-FCCIKT(2,2))
      FFK(1,3)= (UFK(1,3)-FCCIKT(2,3))
      TTK(1,1)= (UTK(1,1)-TC(2,1))
      TTK(1,2)= (UTK(1,2)-(TC(2,2)-(FCCIKT(2,3)*Q(2))))
      TTK(1,3)= (UTK(1,3)-(TC(2,3)+(FCCIKT(2,2)*Q(2))))
      FC(1,1)= -FFK(1,1)
      FC(1,2)= -FFK(1,2)
      FC(1,3)= -FFK(1,3)
      TC(1,1)= -TTK(1,1)
      TC(1,2)= -TTK(1,2)
      TC(1,3)= (UDOT(1)-TTK(1,3))
      FORCE(1,1)= FC(1,1)
      TORQUE(1,1)= TC(1,1)
      FORCE(1,2)= FC(1,2)
      TORQUE(1,2)= TC(1,2)
      FORCE(1,3)= FC(1,3)
      TORQUE(1,3)= TC(1,3)
      FORCE(2,1)= FC(4,1)
      TORQUE(2,1)= TC(4,1)
      FORCE(2,2)= FC(4,2)
      TORQUE(2,2)= TC(4,2)
      FORCE(2,3)= FC(4,3)
      TORQUE(2,3)= TC(4,3)
      FORCE(3,1)= FC(5,1)
      TORQUE(3,1)= TC(5,1)
      FORCE(3,2)= FC(5,2)
      TORQUE(3,2)= TC(5,2)
      FORCE(3,3)= FC(5,3)
      TORQUE(3,3)= TC(5,3)
      FORCE(4,1)= FC(6,1)
      TORQUE(4,1)= TC(6,1)
      FORCE(4,2)= FC(6,2)
      TORQUE(4,2)= TC(6,2)
      FORCE(4,3)= FC(6,3)
      TORQUE(4,3)= TC(6,3)
      FORCE(5,1)= FC(7,1)
      TORQUE(5,1)= TC(7,1)
      FORCE(5,2)= FC(7,2)
      TORQUE(5,2)= TC(7,2)
      FORCE(5,3)= FC(7,3)
      TORQUE(5,3)= TC(7,3)
      FORCE(6,1)= FC(8,1)
      TORQUE(6,1)= TC(8,1)
      FORCE(6,2)= FC(8,2)
      TORQUE(6,2)= TC(8,2)
      FORCE(6,3)= FC(8,3)
      TORQUE(6,3)= TC(8,3)
C
C Compute reaction forces for tree weld joints
C
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain  132 adds/subtracts/negates
C                      49 multiplies
C                       0 divides
C                     147 assignments
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION LK(6,3),HNK(6,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(19,23)
        RETURN
      ENDIF
      LM(1)= (VNK(8,1)+(VNK(7,1)+(VNK(6,1)+(VNK(4,1)+VNK(5,1)))))
      LM(2)= (VNK(8,2)+(VNK(7,2)+(VNK(6,2)+(VNK(4,2)+VNK(5,2)))))
      LM(3)= 0D0
      AM(1)= 0D0
      AM(2)= 0D0
      TEMP(1)= (((50D0*WK(7,3))+((VNK(7,2)*(RNK(7,1)-COM(1)))-(VNK(7,1)*
     & (RNK(7,2)-COM(2)))))+(((50D0*WK(6,3))+((VNK(6,2)*(RNK(6,1)-COM(1)
     & ))-(VNK(6,1)*(RNK(6,2)-COM(2)))))+((U(1)+((50D0*WK(4,3))+((
     & VNK(4,2)*(RNK(4,1)-COM(1)))-(VNK(4,1)*(RNK(4,2)-COM(2))))))+((
     & 50D0*WK(5,3))+((VNK(5,2)*(RNK(5,1)-COM(1)))-(VNK(5,1)*(RNK(5,2)-
     & COM(2))))))))
      AM(3)= (((50D0*WK(8,3))+((VNK(8,2)*(RNK(8,1)-COM(1)))-(VNK(8,1)*(
     & RNK(8,2)-COM(2)))))+TEMP(1))
      TEMP(1)= (((50D0*(WK(7,3)*WK(7,3)))+((VNK(7,1)*VNK(7,1))+(VNK(7,2)
     & *VNK(7,2))))+(((50D0*(WK(6,3)*WK(6,3)))+((VNK(6,1)*VNK(6,1))+(
     & VNK(6,2)*VNK(6,2))))+(((50D0*(WK(5,3)*WK(5,3)))+((VNK(5,1)*
     & VNK(5,1))+(VNK(5,2)*VNK(5,2))))+((U(1)*U(1))+((50D0*(WK(4,3)*
     & WK(4,3)))+((VNK(4,1)*VNK(4,1))+(VNK(4,2)*VNK(4,2))))))))
      KE= (.5D0*(((50D0*(WK(8,3)*WK(8,3)))+((VNK(8,1)*VNK(8,1))+(
     & VNK(8,2)*VNK(8,2))))+TEMP(1)))
C
C  Used 0.00 seconds CPU time,
C  0 additional bytes of memory.
C  Equations contain   48 adds/subtracts/negates
C                      37 multiplies
C                       0 divides
C                       9 assignments
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION TEMP(3000),TMAT1(3,3),TMAT2(3,3),TVEC1(3),TVEC2(3
     & ),TVEC3(3),TVEC4(3),TVEC5(3)
      DOUBLE PRECISION TSC1,TSC2,TSC3
      COMMON/SDGTEMP/ TEMP,TMAT1,TMAT2,TVEC1,TVEC2,TVEC3,TVEC4,TVEC5,
     & TSC1,TSC2,TSC3
      DOUBLE PRECISION IKCNKT(8,3,3)
C
      IF ((ROUSTATE .NE. 2) .AND. (ROUSTATE .NE. 3)) THEN
        CALL SDSETERR(20,23)
        RETURN
      ENDIF
      MTOTO= 5D0
      CM(1)= COM(1)
      CM(2)= COM(2)
      CM(3)= 0D0
      TEMP(1)= (((RNK(6,2)*RNK(6,2))+((10D0*(CNK(6,1,1)*CNK(6,1,1)))+(
     & 50D0*(CNK(6,1,2)*CNK(6,1,2)))))+(((RNK(4,2)*RNK(4,2))+((10D0*(
     & CNK(4,1,1)*CNK(4,1,1)))+(50D0*(CNK(4,1,2)*CNK(4,1,2)))))+((
     & RNK(5,2)*RNK(5,2))+((10D0*(CNK(5,1,1)*CNK(5,1,1)))+(50D0*(
     & CNK(5,1,2)*CNK(5,1,2)))))))
      ICM(1,1)= (1D0+((((RNK(8,2)*RNK(8,2))+((10D0*(CNK(8,1,1)*
     & CNK(8,1,1)))+(50D0*(CNK(8,1,2)*CNK(8,1,2)))))+(((RNK(7,2)*
     & RNK(7,2))+((10D0*(CNK(7,1,1)*CNK(7,1,1)))+(50D0*(CNK(7,1,2)*
     & CNK(7,1,2)))))+TEMP(1)))-(5D0*(COM(2)*COM(2)))))
      TEMP(1)= ((((10D0*(CNK(6,1,1)*CNK(6,2,1)))+(50D0*(CNK(6,1,2)*
     & CNK(6,2,2))))-(RNK(6,1)*RNK(6,2)))+((((10D0*(CNK(4,1,1)*
     & CNK(4,2,1)))+(50D0*(CNK(4,1,2)*CNK(4,2,2))))-(RNK(4,1)*RNK(4,2)))
     & +(((10D0*(CNK(5,1,1)*CNK(5,2,1)))+(50D0*(CNK(5,1,2)*CNK(5,2,2))))
     & -(RNK(5,1)*RNK(5,2)))))
      ICM(1,2)= ((5D0*(COM(1)*COM(2)))+((((10D0*(CNK(8,1,1)*CNK(8,2,1)))
     & +(50D0*(CNK(8,1,2)*CNK(8,2,2))))-(RNK(8,1)*RNK(8,2)))+((((10D0*(
     & CNK(7,1,1)*CNK(7,2,1)))+(50D0*(CNK(7,1,2)*CNK(7,2,2))))-(RNK(7,1)
     & *RNK(7,2)))+TEMP(1))))
      ICM(1,3)= 0D0
      ICM(2,1)= ICM(1,2)
      TEMP(1)= (((RNK(6,1)*RNK(6,1))+((10D0*(CNK(6,2,1)*CNK(6,2,1)))+(
     & 50D0*(CNK(6,2,2)*CNK(6,2,2)))))+(((RNK(4,1)*RNK(4,1))+((10D0*(
     & CNK(4,2,1)*CNK(4,2,1)))+(50D0*(CNK(4,2,2)*CNK(4,2,2)))))+((
     & RNK(5,1)*RNK(5,1))+((10D0*(CNK(5,2,1)*CNK(5,2,1)))+(50D0*(
     & CNK(5,2,2)*CNK(5,2,2)))))))
      ICM(2,2)= (1D0+((((RNK(8,1)*RNK(8,1))+((10D0*(CNK(8,2,1)*
     & CNK(8,2,1)))+(50D0*(CNK(8,2,2)*CNK(8,2,2)))))+(((RNK(7,1)*
     & RNK(7,1))+((10D0*(CNK(7,2,1)*CNK(7,2,1)))+(50D0*(CNK(7,2,2)*
     & CNK(7,2,2)))))+TEMP(1)))-(5D0*(COM(1)*COM(1)))))
      ICM(2,3)= 0D0
      ICM(3,1)= ICM(1,3)
      ICM(3,2)= ICM(2,3)
      ICM(3,3)= (251D0+((((RNK(8,1)*RNK(8,1))+(RNK(8,2)*RNK(8,2)))+(((
     & RNK(7,1)*RNK(7,1))+(RNK(7,2)*RNK(7,2)))+(((RNK(6,1)*RNK(6,1))+(
     & RNK(6,2)*RNK(6,2)))+(((RNK(4,1)*RNK(4,1))+(RNK(4,2)*RNK(4,2)))+((
     & RNK(5,1)*RNK(5,1))+(RNK(5,2)*RNK(5,2)))))))-(5D0*((COM(1)*COM(1))
     & +(COM(2)*COM(2))))))
C
C  Used 0.00 seconds CPU time,
C  135168 additional bytes of memory.
C  Equations contain   59 adds/subtracts/negates
C                      94 multiplies
C                       0 divides
C                      16 assignments
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      INTEGER X,I,GND
      DOUBLE PRECISION LIN(3),PV(3)
      INTEGER SDCHKBNUM
C
      IF ((COORD .LT. 1) .OR. (COORD .GT. 8)) THEN
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
      COMMON/SDGINPUT/ GRAV,MK,IK,PIN,RK,RI,PRES,STABVEL,STABPOS,RHEAD,
     & RCOM,PSRCOMG,PSRCOM,MKRCOMT,PSMK,PSIK,PSRK,PSRI,PSMKG,PSIKG,PSRKG
     & ,PSRIG,MTOT,MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP,GRAVQ,MKQ,IKQ,PINQ,RKQ,RIQ,PRESQ,STABVELQ,
     & STABPOSQ
      INTEGER GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,NLBALL,
     & NPRES,NUSER
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION CURTIM,Q(8),QN(8),U(8),CNK(8,3,3),CNB(6,3,3)
      DOUBLE PRECISION RNK(8,3),VNK(8,3),WK(8,3),RNB(6,3),VNB(6,3),WB(6,
     & 3),WBRCOM(6,3),COM(3),RNKG(3)
      DOUBLE PRECISION CIK(8,3,3),RIKT(8,3,3),IKO(8,3,3),MKRK(8,3,3),CIB
     & (6,3,3)
      DOUBLE PRECISION WKK(8,3),VKK(8,3),DIK(8,3),RPP(8,3),RPK(8,3),RIK(
     & 8,3),RIK2(8,3)
      DOUBLE PRECISION RPRI(8,3),WIK(8,3),VIK(8,3),WIRK(8,3),RKWKK(8,3),
     & WKRPK(8,3),VIKWKR(8,3)
      DOUBLE PRECISION PERR(1),VERR(1),AERR(1),MULT(1),UFK(6,3),UTK(6,3)
     & ,MFK(6,3),MTK(6,3)
      DOUBLE PRECISION UTAU(8),MTAU(8),UACC(8),UVEL(8),UPOS(8)
      DOUBLE PRECISION S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8,C8
      COMMON/SDGSTATE/ CURTIM,Q,QN,U,CNK,CNB,RNKG,RNK,RNB,VNK,VNB,WK,WB,
     & COM,CIK,CIB,RIKT,IKO,MKRK,WKK,VKK,DIK,RPP,RPK,RIK,RIK2,RPRI,WIK,
     & VIK,WIRK,RKWKK,WKRPK,VIKWKR,WBRCOM,PERR,VERR,AERR,MULT,UFK,UTK,
     & UTAU,MFK,MTK,MTAU,UACC,UVEL,UPOS,S1,C1,S4,C4,S5,C5,S6,C6,S7,C7,S8
     & ,C8
      DOUBLE PRECISION FS0(8),QDOT(8),OTK(8,3),ATK(8,3),AIOIWI(8,3),
     & FSTAR(8,3)
      DOUBLE PRECISION TSTAR(8,3),FSTARK(8,3),TSTARK(8,3),IKWK(8,3),
     & WKIKWK(8,3),GK(8,3),IKBWK(6,3),WKIKBWK(6,3)
      DOUBLE PRECISION W0W0(6),W1W1(6),W2W2(6),W0W1(6),W0W2(6),W1W2(6)
      DOUBLE PRECISION W00W11(6),W00W22(6),W11W22(6),WW(1,1),QRAUX(1)
      DOUBLE PRECISION MM(8,8),MLO(8,8),MDI(8),IKWPK(8,8,3),WORKS(8),
     & WORKSS(8,8)
      DOUBLE PRECISION WPK(8,8,3),VPK(8,8,3),VWRI(8,8,3)
      INTEGER WMAP(1),MULTMAP(1),JPVT(1),WSIZ,WRANK
      COMMON/SDGLHS/ QDOT,OTK,ATK,AIOIWI,FSTAR,TSTAR,FS0,FSTARK,TSTARK,
     & IKWK,IKBWK,WKIKWK,WKIKBWK,GK,W0W0,W1W1,W2W2,W0W1,W0W2,W1W2,W00W11
     & ,W00W22,W11W22,WW,QRAUX,MM,MLO,MDI,IKWPK,WORKS,WORKSS,WPK,VPK,
     & VWRI,WMAP,MULTMAP,JPVT,WSIZ,WRANK
      DOUBLE PRECISION FS(8),UDOT(8),TAUC(8),DYAD(6,3,3),FC(8,3),TC(8,3)
      DOUBLE PRECISION ANK(8,3),ONK(8,3),ONKB(8,3),AONKRI(8,3),ANKB(8,3)
     & ,ANKATK(8,3),ANB(6,3),ONB(6,3),DYRCOM(6,3)
      DOUBLE PRECISION FFK(8,3),TTK(8,3),FCCIKT(8,3),FFKB(6,3),TTKB(6,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
      COMMON/SDGTOPO/ GROUND,NBOD,NDOF,NCONS,NLOOP,NLDOF,NLOOPC,NBALL,
     & NLBALL,NPRES,NUSER,JTYPE,INB,OUTB,NJNTDOF,NJNTC,NJNTP,FIRSTQ,
     & BALLQ,FIRSTM,FIRSTP,TRANS
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      DOUBLE PRECISION GRAV(3),MK(6),IK(6,3,3),PIN(8,3)
      DOUBLE PRECISION RK(6,3),RI(6,3),PRES(8),STABVEL,STABPOS
      INTEGER MFRCFLG,ROUSTATE,VPKFLG,INERFLG,MMFLG,MMLDUFLG,WWFLG,
     & LTAUFLG,FS0FLG,II,MMAP(8)
      INTEGER GRAVQ(3),MKQ(6),IKQ(6,3,3),PINQ(8,3),RKQ(6,3),RIQ(6,3),
     & PRESQ(8),STABVELQ,STABPOSQ
      DOUBLE PRECISION MTOT,PSMKG,RHEAD(6,3),RCOM(6,3),MKRCOMT(6,3,3),
     & PSIKG(3,3),PSRCOMG(3),PSRKG(3),PSRIG(3),PSMK(8),PSIK(8,3,3),
     & PSRCOM(8,3),PSRK(8,3),PSRI(8,3)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
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
      INTEGER JTYPE(6),INB(6),OUTB(6),NJNTDOF(6),NJNTC(6),NJNTP(6),
     & FIRSTQ(6),BALLQ(6),FIRSTM(6),FIRSTP(6)
      INTEGER TRANS(8)
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
C   sdstate:    84 adds    80 multiplies     0 divides   247 assignments
C   sdderiv:   729 adds   735 multiplies    10 divides  1083 assignments
C   sdresid:   284 adds   229 multiplies     0 divides   312 assignments
C   sdreac:    132 adds    49 multiplies     0 divides   147 assignments
C   sdmom:      48 adds    37 multiplies     0 divides     9 assignments
C   sdsys:      59 adds    94 multiplies     0 divides    16 assignments
C
