C
C Generated TIMESTAMP by SD/FAST, Kane's formulation
C (sdfast 2.0 #10001)
C
      BLOCK DATA SDLIBDATA
      INTEGER LASTERR,LASTROU
      COMMON/SDGERROR/ LASTERR,LASTROU
C Error parameters
C
      DATA LASTERR/0/
      DATA LASTROU/0/
      END
C
      SUBROUTINE SDPRERRMSG(FNUM,ROUTINE,ERRNUM)
      INTEGER FNUM,ROUTINE,ERRNUM
C
C
      IF (ERRNUM .EQ. 0) THEN
        RETURN
      ENDIF
      WRITE(FNUM,*)' '
      IF (ROUTINE .EQ. 1) THEN
        WRITE(FNUM,*)'SDGRAV:'
      ENDIF
      IF (ROUTINE .EQ. 2) THEN
        WRITE(FNUM,*)'SDMASS:'
      ENDIF
      IF (ROUTINE .EQ. 3) THEN
        WRITE(FNUM,*)'SDINER:'
      ENDIF
      IF (ROUTINE .EQ. 4) THEN
        WRITE(FNUM,*)'SDBTJ:'
      ENDIF
      IF (ROUTINE .EQ. 5) THEN
        WRITE(FNUM,*)'SDITJ:'
      ENDIF
      IF (ROUTINE .EQ. 6) THEN
        WRITE(FNUM,*)'SDPIN:'
      ENDIF
      IF (ROUTINE .EQ. 7) THEN
        WRITE(FNUM,*)'SDINIT:'
      ENDIF
      IF (ROUTINE .EQ. 8) THEN
        WRITE(FNUM,*)'SDSTATE:'
      ENDIF
      IF (ROUTINE .EQ. 9) THEN
        WRITE(FNUM,*)'SDPSSTATE:'
      ENDIF
      IF (ROUTINE .EQ. 13) THEN
        WRITE(FNUM,*)'SDPRESACC:'
      ENDIF
      IF (ROUTINE .EQ. 14) THEN
        WRITE(FNUM,*)'SDPRESVEL:'
      ENDIF
      IF (ROUTINE .EQ. 15) THEN
        WRITE(FNUM,*)'SDPRESPOS:'
      ENDIF
      IF (ROUTINE .EQ. 10) THEN
        WRITE(FNUM,*)'SDHINGET:'
      ENDIF
      IF (ROUTINE .EQ. 11) THEN
        WRITE(FNUM,*)'SDPOINTF:'
      ENDIF
      IF (ROUTINE .EQ. 12) THEN
        WRITE(FNUM,*)'SDBODYT:'
      ENDIF
      IF (ROUTINE .EQ. 17) THEN
        WRITE(FNUM,*)'SDDERIV:'
      ENDIF
      IF (ROUTINE .EQ. 16) THEN
        WRITE(FNUM,*)'SDRESID:'
      ENDIF
      IF (ROUTINE .EQ. 18) THEN
        WRITE(FNUM,*)'SDPSEUDO:'
      ENDIF
      IF (ROUTINE .EQ. 19) THEN
        WRITE(FNUM,*)'SDMOM:'
      ENDIF
      IF (ROUTINE .EQ. 20) THEN
        WRITE(FNUM,*)'SDSYS:'
      ENDIF
      IF (ROUTINE .EQ. 21) THEN
        WRITE(FNUM,*)'SDPOS:'
      ENDIF
      IF (ROUTINE .EQ. 22) THEN
        WRITE(FNUM,*)'SDVEL:'
      ENDIF
      IF (ROUTINE .EQ. 23) THEN
        WRITE(FNUM,*)'SDORIENT:'
      ENDIF
      IF (ROUTINE .EQ. 24) THEN
        WRITE(FNUM,*)'SDANGVEL:'
      ENDIF
      IF (ROUTINE .EQ. 25) THEN
        WRITE(FNUM,*)'SDTRANS:'
      ENDIF
      IF (ROUTINE .EQ. 26) THEN
        WRITE(FNUM,*)'SDPERR:'
      ENDIF
      IF (ROUTINE .EQ. 27) THEN
        WRITE(FNUM,*)'SDVERR:'
      ENDIF
      IF (ROUTINE .EQ. 28) THEN
        WRITE(FNUM,*)'SDPSQDOT:'
      ENDIF
      IF (ROUTINE .EQ. 29) THEN
        WRITE(FNUM,*)'SDPSUDOT:'
      ENDIF
      IF (ROUTINE .EQ. 30) THEN
        WRITE(FNUM,*)'SDGETHT:'
      ENDIF
      IF (ROUTINE .EQ. 31) THEN
        WRITE(FNUM,*)'SDREAC:'
      ENDIF
      IF (ROUTINE .EQ. 32) THEN
        WRITE(FNUM,*)'SDACC:'
      ENDIF
      IF (ROUTINE .EQ. 33) THEN
        WRITE(FNUM,*)'SDANGACC:'
      ENDIF
      IF (ROUTINE .EQ. 34) THEN
        WRITE(FNUM,*)'SDMULT:'
      ENDIF
      IF (ROUTINE .EQ. 35) THEN
        WRITE(FNUM,*)'SDAERR:'
      ENDIF
      IF (ROUTINE .EQ. 36) THEN
        WRITE(FNUM,*)'SDINDX:'
      ENDIF
      IF (ROUTINE .EQ. 37) THEN
        WRITE(FNUM,*)'SDPRES:'
      ENDIF
      IF (ROUTINE .EQ. 38) THEN
        WRITE(FNUM,*)'SDSTAB:'
      ENDIF
      IF (ROUTINE .EQ. 39) THEN
        WRITE(FNUM,*)'SDGETGRAV:'
      ENDIF
      IF (ROUTINE .EQ. 40) THEN
        WRITE(FNUM,*)'SDGETMASS:'
      ENDIF
      IF (ROUTINE .EQ. 41) THEN
        WRITE(FNUM,*)'SDGETINER:'
      ENDIF
      IF (ROUTINE .EQ. 42) THEN
        WRITE(FNUM,*)'SDGETBTJ:'
      ENDIF
      IF (ROUTINE .EQ. 43) THEN
        WRITE(FNUM,*)'SDGETITJ:'
      ENDIF
      IF (ROUTINE .EQ. 44) THEN
        WRITE(FNUM,*)'SDGETPIN:'
      ENDIF
      IF (ROUTINE .EQ. 45) THEN
        WRITE(FNUM,*)'SDGETPRES:'
      ENDIF
      IF (ROUTINE .EQ. 46) THEN
        WRITE(FNUM,*)'SDGETSTAB:'
      ENDIF
      IF (ROUTINE .EQ. 47) THEN
        WRITE(FNUM,*)'SDINFO:'
      ENDIF
      IF (ROUTINE .EQ. 48) THEN
        WRITE(FNUM,*)'SDJNT:'
      ENDIF
      IF (ROUTINE .EQ. 49) THEN
        WRITE(FNUM,*)'SDCONS:'
      ENDIF
      IF (ROUTINE .EQ. 50) THEN
        WRITE(FNUM,*)'SDASSEMBLE:'
      ENDIF
      IF (ROUTINE .EQ. 51) THEN
        WRITE(FNUM,*)'SDINITVEL:'
      ENDIF
      IF (ROUTINE .EQ. 52) THEN
        WRITE(FNUM,*)'SDSTATIC:'
      ENDIF
      IF (ROUTINE .EQ. 53) THEN
        WRITE(FNUM,*)'SDSTEADY:'
      ENDIF
      IF (ROUTINE .EQ. 54) THEN
        WRITE(FNUM,*)'SDMOTION:'
      ENDIF
      IF (ROUTINE .EQ. 55) THEN
        WRITE(FNUM,*)'SDFMOTION:'
      ENDIF
      IF (ROUTINE .EQ. 56) THEN
        WRITE(FNUM,*)'SDEQUIVHT:'
      ENDIF
      IF (ROUTINE .EQ. 57) THEN
        WRITE(FNUM,*)'SDMASSMAT:'
      ENDIF
      IF (ROUTINE .EQ. 58) THEN
        WRITE(FNUM,*)'SDFRCMAT:'
      ENDIF
      IF (ROUTINE .EQ. 59) THEN
        WRITE(FNUM,*)'SDREL2CART:'
      ENDIF
      IF (ROUTINE .EQ. 60) THEN
        WRITE(FNUM,*)'SDCOMPTRQ:'
      ENDIF
      IF (ROUTINE .EQ. 61) THEN
        WRITE(FNUM,*)'SDFULLTRQ:'
      ENDIF
      IF (ROUTINE .EQ. 62) THEN
        WRITE(FNUM,*)'SDVROT:'
      ENDIF
      IF (ERRNUM .EQ. 1) THEN
        WRITE(FNUM,*)'a tree joint pin axis was zero'
      ENDIF
      IF (ERRNUM .EQ. 2) THEN
        WRITE(FNUM,*)'the 1st inboard pin for a loop joint was zero'
      ENDIF
      IF (ERRNUM .EQ. 3) THEN
        WRITE(FNUM,*)'the 2nd inboard pin for a loop joint was zero'
      ENDIF
      IF (ERRNUM .EQ. 4) THEN
        WRITE(FNUM,*)'the 3rd inboard pin for a loop joint was zero'
      ENDIF
      IF (ERRNUM .EQ. 5) THEN
        WRITE(FNUM,*)'an inboard reference line was zero'
      ENDIF
      IF (ERRNUM .EQ. 6) THEN
        WRITE(FNUM,*)'a set of loop joint axes was not right handed'
      ENDIF
      IF (ERRNUM .EQ. 7) THEN
        WRITE(FNUM,*)'a loop joint bodypin was zero'
      ENDIF
      IF (ERRNUM .EQ. 8) THEN
        WRITE(FNUM,*)'a loop joint body reference line was zero'
      ENDIF
      IF (ERRNUM .EQ. 9) THEN
        WRITE(FNUM,*)'1st/2nd pins in a loop joint not perpendicular'
      ENDIF
      IF (ERRNUM .EQ. 10) THEN
        WRITE(FNUM,*)'2nd/3rd pins in a loop joint not perpendicular'
      ENDIF
      IF (ERRNUM .EQ. 11) THEN
        WRITE(FNUM,*)'1st/3rd pins in a loop joint not perpendicular'
      ENDIF
      IF (ERRNUM .EQ. 12) THEN
        WRITE(FNUM,*)'a loop jt pin and inbref were not perpendicular'
      ENDIF
      IF (ERRNUM .EQ. 13) THEN
        WRITE(FNUM,*)'a bodypin and bodyref were not perpendicular'
      ENDIF
      IF (ERRNUM .EQ. 14) THEN
        WRITE(FNUM,*)'Euler parameters were far from normalized'
      ENDIF
      IF (ERRNUM .EQ. 15) THEN
        WRITE(FNUM,*)'illegal body number'
      ENDIF
      IF (ERRNUM .EQ. 16) THEN
        WRITE(FNUM,*)'illegal joint number'
      ENDIF
      IF (ERRNUM .EQ. 17) THEN
        WRITE(FNUM,*)'illegal axis number'
      ENDIF
      IF (ERRNUM .EQ. 18) THEN
        WRITE(FNUM,*)'illegal axis number for this joint'
      ENDIF
      IF (ERRNUM .EQ. 19) THEN
        WRITE(FNUM,*)'tried to set non-variable (i.e., non-?) parameter'
      ENDIF
      IF (ERRNUM .EQ. 20) THEN
        WRITE(FNUM,*)'prescribed motion was neither 0 (off) or 1 (on)'
      ENDIF
      IF (ERRNUM .EQ. 21) THEN
        WRITE(FNUM,*)'illegal user constraint number'
      ENDIF
      IF (ERRNUM .EQ. 22) THEN
        WRITE(FNUM,*)'SDINIT must be called first'
      ENDIF
      IF (ERRNUM .EQ. 23) THEN
        WRITE(FNUM,*)'SDSTATE must be called first'
      ENDIF
      IF (ERRNUM .EQ. 24) THEN
        WRITE(FNUM,*)'SDDERIV must be called first'
      ENDIF
      IF (ERRNUM .EQ. 25) THEN
        WRITE(FNUM,*)'a gravity ? parameter is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 26) THEN
        WRITE(FNUM,*)'a ? mass is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 27) THEN
        WRITE(FNUM,*)'a ? inertia is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 28) THEN
        WRITE(FNUM,*)'a ? tree jt pin is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 29) THEN
        WRITE(FNUM,*)'a ? tree bodyToJoint vector is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 30) THEN
        WRITE(FNUM,*)'a ? tree inbToJoint vector is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 31) THEN
        WRITE(FNUM,*)'a ? prescribed tree jt axis is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 32) THEN
        WRITE(FNUM,*)'the stabvel ? parameter is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 33) THEN
        WRITE(FNUM,*)'the stabpos ? parameter is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 34) THEN
        WRITE(FNUM,*)'a ? loop jt inboard pin is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 35) THEN
        WRITE(FNUM,*)'a ? loop jt inbref is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 36) THEN
        WRITE(FNUM,*)'a ? loop jt bodypin is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 37) THEN
        WRITE(FNUM,*)'a ? loop jt bodyref is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 38) THEN
        WRITE(FNUM,*)'a ? loop jt bodyToJoint vector is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 39) THEN
        WRITE(FNUM,*)'a ? loop jt inbToJoint vector is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 40) THEN
        WRITE(FNUM,*)'a ? prescribed loop jt axis is unspecified'
      ENDIF
      IF (ERRNUM .EQ. 41) THEN
        WRITE(FNUM,*)'Dynamics & Library File serial nos. differ'
      ENDIF
      IF (ERRNUM .EQ. 42) THEN
        WRITE(FNUM,*)'Dynamics & Analysis File gen. times differ'
      ENDIF
      IF (ERRNUM .EQ. 43) THEN
        WRITE(FNUM,*)'A tree gimbal joint is in gimbal lock'
      ENDIF
      IF (ERRNUM .EQ. 44) THEN
        WRITE(FNUM,*)'A loop gimbal joint is in gimbal lock'
      ENDIF
      IF (ERRNUM .EQ. 45) THEN
        WRITE(FNUM,*)'Bad relative coordinate number'
      ENDIF
      IF (ERRNUM .EQ. 46) THEN
        WRITE(FNUM,*)'The vector about which to rotate was zero'
      ENDIF
      IF (ERRNUM .EQ. 47) THEN
        WRITE(FNUM,*)'Singular mass matrix - bad inertialess body?'
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDERROR(ROUTINE,ERRNUM)
      INTEGER ROUTINE,ERRNUM
C
      INTEGER LASTERR,LASTROU
      COMMON/SDGERROR/ LASTERR,LASTROU
C
      ERRNUM= LASTERR
      ROUTINE= LASTROU
C
      RETURN
      END
C
      SUBROUTINE SDPRINTERR(FNUM)
      INTEGER FNUM
C
      INTEGER LASTERR,LASTROU
      COMMON/SDGERROR/ LASTERR,LASTROU
C
      CALL SDPRERRMSG(FNUM,LASTROU,LASTERR)
C
      RETURN
      END
C
      SUBROUTINE SDCLEARERR
C
      INTEGER LASTERR,LASTROU
      COMMON/SDGERROR/ LASTERR,LASTROU
C
      LASTERR= 0
      LASTROU= 0
C
      RETURN
      END
C
      SUBROUTINE SDSETERR(ROUTINE,ERRNUM)
      INTEGER ROUTINE,ERRNUM
C
      INTEGER LASTERR,LASTROU
      COMMON/SDGERROR/ LASTERR,LASTROU
C
      IF (LASTERR .EQ. 0) THEN
        LASTERR= ERRNUM
        LASTROU= ROUTINE
      ENDIF
C
      RETURN
      END
C
C
C ===========================================================
C LDU decomposition scheme for solving Mx=b
C   M: na X na symmetric, pos. definite, upper rt triangle filled
C   b: na X 1
C   x: na X 1 (returned)
C Actual dimensions (n) may be larger, map says where to find
C the na interesting elements.
C ===========================================================
C
C
      SUBROUTINE SDLDUDCOMP(N,NA,MAP,TOL,LD,SUM,M,L,D)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION TOL,LD(NA,NA),SUM(NA),M(N,N),L(N,N),D(N)
C
      INTEGER R,C,ROW,COL,CM1,J,MJ
      DOUBLE PRECISION DTEMP,DTEMPI,LDTEMP,TSUM
C
      DO 400 C=1,NA
        COL= MAP(C)
        CM1= C-1
C
        DO 300 R=C,NA
          ROW= MAP(R)
          TSUM= M(COL,ROW)
          DO 200 J=1,CM1
            MJ= MAP(J)
            TSUM= TSUM-LD(R,J)*L(COL,MJ)
200       CONTINUE
          SUM(R)= TSUM
300     CONTINUE
        DTEMP= SUM(C)
        DTEMPI= 0D0
        IF (DABS(DTEMP) .GT. TOL) DTEMPI= 1D0/DTEMP
        D(COL)= DTEMPI
        DO 350 R=C,NA
          ROW= MAP(R)
          LDTEMP= SUM(R)
          LD(R,C)= LDTEMP
          L(ROW,COL)= LDTEMP*DTEMPI
350     CONTINUE
400   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDLDUBSL(N,NA,MAP,L,B,X)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION L(N,N),B(N),X(N)
C
      INTEGER I,J,IM1,MI,MJ
      DOUBLE PRECISION SUM
C
      MI= MAP(1)
      X(MI)= B(MI)
      DO 200 I=2,NA
        MI= MAP(I)
        IM1= I-1
        SUM= B(MI)
        DO 100 J=1,IM1
          MJ= MAP(J)
          SUM= SUM-L(MI,MJ)*X(MJ)
100     CONTINUE
        X(MI)= SUM
200   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDLDUBSD(N,NA,MAP,D,B,X)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION D(N),B(N),X(N)
C
      INTEGER I,MI
C
      DO 100 I=1,NA
        MI= MAP(I)
        X(MI)= B(MI)*D(MI)
100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDLDUBSU(N,NA,MAP,L,B,X)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION L(N,N),B(N),X(N)
C
      INTEGER I,J,IP1,MI,MJ
      DOUBLE PRECISION SUM
C
      MI= MAP(NA)
      X(MI)= B(MI)
      I= NA-1
100   IF (I  .GE.  1) THEN
        MI= MAP(I)
        IP1= I+1
        SUM= B(MI)
        DO 200 J=IP1,NA
          MJ= MAP(J)
          SUM= SUM-L(MJ,MI)*X(MJ)
200     CONTINUE
        X(MI)= SUM
        I= I-1
        GOTO 100
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDLDUBSLV(N,NA,MAP,WORK,L,D,B,X)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION WORK(N),L(N,N),D(N),B(N),X(N)
C
C
      CALL SDLDUBSL(N,NA,MAP,L,B,WORK)
      CALL SDLDUBSD(N,NA,MAP,D,WORK,WORK)
      CALL SDLDUBSU(N,NA,MAP,L,WORK,X)
C
      RETURN
      END
C
      SUBROUTINE SDLDUSLV(N,NA,MAP,TOL,WORK1,WORK2,M,B,L,D,X)
      INTEGER N,NA,MAP(NA)
      DOUBLE PRECISION TOL,WORK1(NA,NA),WORK2(N),M(N,N),B(N),L(N,N),D(N)
     & ,X(N)
C
C
      CALL SDLDUDCOMP(N,NA,MAP,TOL,WORK1,WORK2,M,L,D)
      CALL SDLDUBSLV(N,NA,MAP,WORK2,L,D,B,X)
C
      RETURN
      END
C
C
C ===========================================================
C QR decomposition scheme for solving Wx=b
C   W is nra by nca
C   b is nra by 1
C   x (returned) is nca by 1
C Actual dimensions (nr,nc) may be larger, mapr (mapc) says where
C to find the nra (nca) interesting rows (columns).  On return, W is
C overwritten by the Q and R matrices in compact form.  Solution yields
C least squares residual for overdetermined systems.  Underdetermined
C systems give a solution in which elements corresponding to redundant
C or missing equations are set to 0. (Not necessarily the LS solution.)
C ===========================================================
C
C
      SUBROUTINE SDQRDCOMP(NR,NC,NRA,NCA,MAPR,MAPC,W,QRAUX,JPVT)
      INTEGER NR,NC,NRA,NCA,MAPR(NRA),MAPC(NCA)
      DOUBLE PRECISION W(NR,NC),QRAUX(NCA)
      INTEGER JPVT(NCA)
C
      INTEGER I,J,L,LP1,MAXJ,MI,MJ,MLR,MLC,MMAXJ,MINRCA
      DOUBLE PRECISION MAXNRM,NRMXL,T
C
      DO 70 J=1,NCA
        MJ= MAPC(J)
        JPVT(J)= J
        T= 0D0
        DO 60 L=1,NRA
          MLR= MAPR(L)
          T= T+W(MLR,MJ)*W(MLR,MJ)
60      CONTINUE
        QRAUX(J)= DSQRT(T)
70    CONTINUE
      IF (NCA  .LT.  NRA) THEN
        MINRCA= NCA
      ELSE
        MINRCA= NRA
      ENDIF
      DO 200 L=1,MINRCA
        MLR= MAPR(L)
        MLC= MAPC(L)
        MAXNRM= 0D0
        MAXJ= L
        DO 100 J=L,NCA
          IF (QRAUX(J)  .GT.  MAXNRM) THEN
            MAXNRM= QRAUX(J)
            MAXJ= J
          ENDIF
100     CONTINUE
        IF (MAXJ  .NE.  L) THEN
          MMAXJ= MAPC(MAXJ)
          DO 105 I=1,NRA
            MI= MAPR(I)
            T= W(MI,MLC)
            W(MI,MLC)= W(MI,MMAXJ)
            W(MI,MMAXJ)= T
105       CONTINUE
          QRAUX(MAXJ)= QRAUX(L)
          I= JPVT(MAXJ)
          JPVT(MAXJ)= JPVT(L)
          JPVT(L)= I
        ENDIF
        QRAUX(L)= 0D0
        IF (L  .NE.  NRA) THEN
          T= 0D0
          DO 110 I=L,NRA
            MI= MAPR(I)
            T= T+W(MI,MLC)*W(MI,MLC)
110       CONTINUE
          NRMXL= DSQRT(T)
          IF (NRMXL  .NE.  0D0) THEN
            IF (W(MLR,MLC)  .LT.  0D0) THEN
              NRMXL=  -NRMXL
            ENDIF
            T= 1D0/NRMXL
            DO 120 I=L,NRA
              MI= MAPR(I)
              W(MI,MLC)= T*W(MI,MLC)
120         CONTINUE
            W(MLR,MLC)= 1D0+W(MLR,MLC)
            LP1= L+1
            DO 160 J=LP1,NCA
              MJ= MAPC(J)
              T= 0D0
              DO 130 I=L,NRA
                MI= MAPR(I)
                T= T-W(MI,MLC)*W(MI,MJ)
130           CONTINUE
              T= T/W(MLR,MLC)
              DO 140 I=L,NRA
                MI= MAPR(I)
                W(MI,MJ)= W(MI,MJ)+T*W(MI,MLC)
140           CONTINUE
              T= 0D0
              DO 150 I=LP1,NRA
                MI= MAPR(I)
                T= T+W(MI,MJ)*W(MI,MJ)
150           CONTINUE
              QRAUX(J)= DSQRT(T)
160         CONTINUE
            QRAUX(L)= W(MLR,MLC)
            W(MLR,MLC)=  -NRMXL
          ENDIF
        ENDIF
200   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDQRSL(NR,NC,NRA,NCA,MAPR,MAPC,K,WORK,W,QRAUX,B,X)
      INTEGER NR,NC,NRA,NCA,MAPR(NRA),MAPC(NCA),K
      DOUBLE PRECISION WORK(NRA),W(NR,NC),QRAUX(NCA),B(NR),X(NC)
C
      INTEGER I,J,JJ,JM1,MIR,MIC,MJR,MJC,MINRK
      DOUBLE PRECISION T,TT
C
      DO 50 I=1,NRA
        MIR= MAPR(I)
        WORK(I)= B(MIR)
50    CONTINUE
      MINRK= NRA-1
      IF (K  .LT.  MINRK) THEN
        MINRK= K
      ENDIF
      DO 90 J=1,MINRK
        MJR= MAPR(J)
        MJC= MAPC(J)
        IF (QRAUX(J)  .NE.  0D0) THEN
          TT= W(MJR,MJC)
          W(MJR,MJC)= QRAUX(J)
          T= 0D0
          DO 60 I=J,NRA
            MIR= MAPR(I)
            T= T-W(MIR,MJC)*WORK(I)
60        CONTINUE
          T= T/W(MJR,MJC)
          DO 70 I=J,NRA
            MIR= MAPR(I)
            WORK(I)= WORK(I)+T*W(MIR,MJC)
70        CONTINUE
          W(MJR,MJC)= TT
        ENDIF
90    CONTINUE
      DO 100 J=1,K
        MJC= MAPC(J)
        X(MJC)= WORK(J)
100   CONTINUE
      DO 170 JJ=1,K
        J= K-JJ+1
        JM1= J-1
        MJR= MAPR(J)
        MJC= MAPC(J)
        X(MJC)= X(MJC)/W(MJR,MJC)
        T=  -X(MJC)
        DO 160 I=1,JM1
          MIR= MAPR(I)
          MIC= MAPC(I)
          X(MIC)= X(MIC)+T*W(MIR,MJC)
160     CONTINUE
170   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SDQRBSLV(NR,NC,NRA,NCA,MAPR,MAPC,TOL,WORK,IWORK,W,QRAUX
     & ,JPVT,B,X,RANK)
      INTEGER NR,NC,NRA,NCA,MAPR(NRA),MAPC(NCA)
      DOUBLE PRECISION TOL,WORK(NRA)
      INTEGER IWORK(NCA)
      DOUBLE PRECISION W(NR,NC),QRAUX(NCA)
      INTEGER JPVT(NCA)
      DOUBLE PRECISION B(NR),X(NC)
      INTEGER RANK
C
      INTEGER J,K,KK,MJC,MKKR,MKKC,MINRCA
      DOUBLE PRECISION T
C
      DO 10 J=1,NCA
        IWORK(J)= JPVT(J)
10    CONTINUE
      IF (NCA  .LT.  NRA) THEN
        MINRCA= NCA
      ELSE
        MINRCA= NRA
      ENDIF
      K= 0
      MKKR= MAPR(1)
      MKKC= MAPC(1)
      T= TOL*DABS(W(MKKR,MKKC))
      DO 20 KK=1,MINRCA
        MKKR= MAPR(KK)
        MKKC= MAPC(KK)
        IF (DABS(W(MKKR,MKKC))  .LE.  T) THEN
          GOTO 30
        ELSE
          K= K+1
        ENDIF
20    CONTINUE
30    CONTINUE
      IF (K  .NE.  0) THEN
        CALL SDQRSL(NR,NC,NRA,NCA,MAPR,MAPC,K,WORK,W,QRAUX,B,X)
      ENDIF
      KK= K+1
      DO 35 J=KK,NCA
        X(MAPC(J))= 0D0
35    CONTINUE
      DO 50 J=1,NCA
        KK= IWORK(J)
40      IF (KK  .NE.  J) THEN
          MJC= MAPC(J)
          MKKC= MAPC(KK)
          T= X(MJC)
          X(MJC)= X(MKKC)
          X(MKKC)= T
          IWORK(J)= IWORK(KK)
          IWORK(KK)= KK
          KK= IWORK(J)
          GOTO 40
        ENDIF
50    CONTINUE
      RANK= K
C
      RETURN
      END
C
      SUBROUTINE SDQRSLV(NR,NC,NRA,NCA,MAPR,MAPC,TOL,JPVT,QRAUX,WORK,
     & IWORK,W,B,X,RANK)
      INTEGER NR,NC,NRA,NCA,MAPR(NRA),MAPC(NCA)
      DOUBLE PRECISION TOL
      INTEGER JPVT(NCA)
      DOUBLE PRECISION QRAUX(NCA),WORK(NRA)
      INTEGER IWORK(NCA)
      DOUBLE PRECISION W(NR,NC),B(NR),X(NC)
      INTEGER RANK
C
C
      CALL SDQRDCOMP(NR,NC,NRA,NCA,MAPR,MAPC,W,QRAUX,JPVT)
      CALL SDQRBSLV(NR,NC,NRA,NCA,MAPR,MAPC,TOL,WORK,IWORK,W,QRAUX,
     & JPVT,B,X,RANK)
C
      RETURN
      END
C
C
C ===========================================================
C Linear equation solver for Wx=b
C   W is nra by nca
C   b is nra by 1
C   x (returned) is nca by 1
C Actual dimensions (nr,nc) may be larger, mapr (mapc) says where
C to find the nra (nca) interesting rows (columns).  On return, W and
C b are unchanged.  Solution yields least squares residual for
C overdetermined systems and a least squares solution vector for
C underdetermined systems.  If ndes > 0, the first nra-ndes rows
C are `required' while the remaining ndes rows are `desired'.  The
C returned solution minimizes the residual for the required rows,
C then does the best it can on the desired rows.
C
C The performance and numerical properties of this routine are
C considerably worse than those of the qrslv routine.  However, it
C works very well as a guide to the root finding routine.
C
C Work arrays should be dimensioned as follows:
C   dw is 2*(nra+nca)**2
C   rw is 4*(nra+nca)
C   iw is 3*(nra+nca)
C ===========================================================
C
C
      SUBROUTINE SDLSSLV(NR,NC,NRA,NCA,NDES,MAPR,MAPC,TOL,DW,RW,IW,W,B,X
     & )
      INTEGER NR,NC,NRA,NCA,NDES,MAPR(NRA),MAPC(NCA)
      DOUBLE PRECISION TOL,DW(*),RW(*)
      INTEGER IW(*)
      DOUBLE PRECISION W(NR,NC),B(NR),X(NC)
C
      INTEGER I,J,K,NREQ,DSIZ,RANK,MAPI,MAPJ,MAPK
      INTEGER IX,WWT,DLOC,DDT,QRAUX,WORK,RHS,SOLN,MAP,JPVT,IWORK
C
      NREQ= NRA-NDES
      DSIZ= NCA+NREQ
      WWT= 1
      DLOC= 1
      DDT= DLOC+DSIZ*DSIZ
      QRAUX= 1
      WORK= QRAUX+NCA+NRA
      RHS= WORK+NCA+NRA
      SOLN= RHS+NCA+NRA
      MAP= 1
      JPVT= MAP+NCA+NRA
      IWORK= JPVT+NCA+NRA
      IF (NDES  .EQ.  0) THEN
        DO 130 I=1,NRA
          MAPI= MAPR(I)
          RW(RHS-1+I)= B(MAPI)
          DO 120 J=I,NRA
            MAPJ= MAPR(J)
            IX= WWT-1+I+(J-1)*NRA
            DW(IX)= 0D0
            DO 110 K=1,NCA
              MAPK= MAPC(K)
              DW(IX)= DW(IX)+W(MAPI,MAPK)*W(MAPJ,MAPK)
110         CONTINUE
            DW(WWT-1+J+(I-1)*NRA)= DW(IX)
120       CONTINUE
          IW(MAP-1+I)= I
130     CONTINUE
        CALL SDQRSLV(NRA,NRA,NRA,NRA,IW(MAP),IW(MAP),TOL,IW(JPVT),RW(
     &   QRAUX),RW(WORK),IW(IWORK),DW(WWT),RW(RHS),RW(SOLN),RANK)
        DO 220 I=1,NCA
          MAPI= MAPC(I)
          X(MAPI)= 0D0
          DO 210 J=1,NRA
            MAPJ= MAPR(J)
            X(MAPI)= X(MAPI)+W(MAPJ,MAPI)*RW(SOLN-1+J)
210       CONTINUE
220     CONTINUE
      ELSE
        DO 350 I=1,NCA
          MAPI= MAPC(I)
          DO 320 J=I,NCA
            MAPJ= MAPC(J)
            IX= DLOC-1+I+(J-1)*DSIZ
            DW(IX)= 0D0
            DO 310 K=1,NDES
              MAPK= MAPR(NREQ+K)
              DW(IX)= DW(IX)+W(MAPK,MAPI)*W(MAPK,MAPJ)
310         CONTINUE
            DW(DLOC-1+J+(I-1)*DSIZ)= DW(IX)
320       CONTINUE
          DO 330 J=1,NREQ
            MAPJ= MAPR(J)
            IX= DLOC-1+I+((J+NCA)-1)*DSIZ
            DW(IX)= W(MAPJ,MAPI)
            DW(DLOC-1+(J+NCA)+(I-1)*DSIZ)= DW(IX)
330       CONTINUE
          RW(RHS-1+I)= 0D0
          DO 340 J=1,NDES
            MAPJ= MAPR(NREQ+J)
            RW(RHS-1+I)= RW(RHS-1+I)+W(MAPJ,MAPI)*B(MAPJ)
340       CONTINUE
350     CONTINUE
        DO 420 I=1,NREQ
          DO 410 J=1,NREQ
            DW(DLOC-1+(I+NCA)+((J+NCA)-1)*DSIZ)= 0D0
410       CONTINUE
          RW(RHS-1+(I+NCA))= B(MAPR(I))
420     CONTINUE
        DO 530 I=1,DSIZ
          IW(MAP-1+I)= I
          DO 520 J=I,DSIZ
            IX= DDT-1+I+(J-1)*DSIZ
            DW(IX)= 0D0
            DO 510 K=1,DSIZ
              DW(IX)= DW(IX)+DW(DLOC-1+I+(K-1)*DSIZ)*DW(DLOC-1+J+(K-1)*
     &         DSIZ)
510         CONTINUE
            DW(DDT-1+J+(I-1)*DSIZ)= DW(IX)
520       CONTINUE
530     CONTINUE
        CALL SDQRSLV(DSIZ,DSIZ,DSIZ,DSIZ,IW(MAP),IW(MAP),TOL,IW(JPVT),RW
     &   (QRAUX),RW(WORK),IW(IWORK),DW(DDT),RW(RHS),RW(SOLN),RANK)
        DO 620 I=1,NCA
          MAPI= MAPC(I)
          X(MAPI)= 0D0
          DO 610 J=1,DSIZ
            X(MAPI)= X(MAPI)+DW(DLOC-1+J+(I-1)*DSIZ)*RW(SOLN-1+J)
610       CONTINUE
620     CONTINUE
      ENDIF
C
      RETURN
      END
C
C Utility routines for use with sdroot.
C
C
      SUBROUTINE SDCALCERRS(FVAL,NFUNC,NDES,DNORM,MAXDERR,MAXRERR,
     & DERRNORM)
      DOUBLE PRECISION FVAL(NFUNC)
      INTEGER NFUNC,NDES,DNORM
      DOUBLE PRECISION MAXDERR,MAXRERR,DERRNORM
C
      INTEGER I,NREQ
      DOUBLE PRECISION TMP
C
      NREQ= NFUNC-NDES
      MAXDERR= 0D0
      MAXRERR= 0D0
      DERRNORM= 0D0
      DO 100 I=1,NREQ
        TMP= DABS(FVAL(I))
        IF (TMP .GT. MAXRERR) THEN
          MAXRERR= TMP
        ENDIF
100   CONTINUE
      DO 200 I=1,NDES
        TMP= DABS(FVAL(NREQ+I))
        IF (TMP .GT. MAXDERR) THEN
          MAXDERR= TMP
        ENDIF
        DERRNORM= DERRNORM+TMP
200   CONTINUE
      IF (DNORM .EQ. 1) THEN
        DERRNORM= MAXDERR
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDADJVARS(FUNC,VARS,PARAM,NFUNC,NDES,DNORM,NVAR,DELTAS,
     & STEP,RERR,TDERR,RTOL,FCNT,NEWVARS,NEWERRS)
      EXTERNAL FUNC
      DOUBLE PRECISION VARS(NVAR),PARAM(*)
      INTEGER NFUNC,NDES,DNORM,NVAR
      DOUBLE PRECISION DELTAS(NVAR),STEP,RERR,TDERR,RTOL
      INTEGER FCNT
      DOUBLE PRECISION NEWVARS(NVAR),NEWERRS(NFUNC)
C
      INTEGER I,CNT,ALLDONE
      DOUBLE PRECISION IMPR,MAXCHG,PMAXFACT,MAXFACT,PMAXRERR,PDERRNORM,
     & MAXDERR,MAXRERR,DERRNORM,MAG
C
      MAXFACT= 1D0
      DO 100 I=1,NVAR
        IMPR= DABS(DELTAS(I)*STEP)
        MAXCHG= .1D0*DABS(VARS(I))+1D0
        IF (IMPR/MAXCHG .GT. MAXFACT) THEN
          MAXFACT= IMPR/MAXCHG
        ENDIF
100   CONTINUE
      MAXFACT= 1D0/MAXFACT
      PMAXRERR= RERR
      PDERRNORM= TDERR
      PMAXFACT= MAXFACT
      CNT= 0
      ALLDONE= 0
C retry
150   CONTINUE
      CNT= CNT+1
      DO 200 I=1,NVAR
        IMPR= DELTAS(I)*STEP
        NEWVARS(I)= VARS(I) - IMPR*MAXFACT
200   CONTINUE
      CALL FUNC(NEWVARS,PARAM,NEWERRS)
      FCNT= FCNT+1
      IF (ALLDONE .NE. 0) THEN
        RETURN
      ENDIF
      CALL SDCALCERRS(NEWERRS,NFUNC,NDES,DNORM,MAXDERR,MAXRERR,DERRNORM)
      IF ((PMAXRERR .LE. RTOL) .AND. (MAXRERR .LE. RTOL)) THEN
        IMPR= PDERRNORM-DERRNORM
      ELSE
        IMPR= PMAXRERR-MAXRERR
      ENDIF
      PMAXRERR= MAXRERR
      PDERRNORM= DERRNORM
      IF (IMPR .LE. 0D0) THEN
        IF (MAXFACT .NE. PMAXFACT) THEN
          MAXFACT= PMAXFACT
          ALLDONE= 1
C goto retry
          GOTO 150
        ENDIF
      ELSE
        IF (MAXFACT .LT. .5D0) THEN
          IF (CNT .LT. 5) THEN
            MAG= 1.25D0
          ELSE
            IF (CNT .LT. 10) THEN
              MAG= 2D0
            ELSE
              MAG= 10D0
            ENDIF
          ENDIF
          PMAXFACT= MAXFACT
          MAXFACT= MAG*MAXFACT
          IF (MAXFACT .GT. 1D0) THEN
            MAXFACT= 1D0
          ENDIF
C goto retry
          GOTO 150
        ENDIF
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE SDCALCJAC(FUNC,VARS,PARAM,NFUNC,NVAR,LOCK,DELTA,FVAL,
     & FTMP,JW,FCNT,SCALE)
      EXTERNAL FUNC
      DOUBLE PRECISION VARS(NVAR),PARAM(*)
      INTEGER NFUNC,NVAR,LOCK(NVAR)
      DOUBLE PRECISION DELTA,FVAL(*),FTMP(*),JW(NFUNC,NVAR)
      INTEGER FCNT
      DOUBLE PRECISION SCALE(NFUNC)
C
      INTEGER I,J
      DOUBLE PRECISION SAVE,CHG,VCHG,MAXELT
C
      DO 120 J=1,NVAR
        IF (LOCK(J) .NE. 0) THEN
          DO 100 I=1,NFUNC
            JW(I,J)= 0D0
100       CONTINUE
        ELSE
          SAVE= VARS(J)
          VCHG= DELTA*(DABS(SAVE)+1D0)
          VARS(J)= VARS(J)+VCHG
          CALL FUNC(VARS,PARAM,FTMP)
          FCNT= FCNT+1
          VARS(J)= SAVE
          DO 110 I=1,NFUNC
            CHG= FTMP(I)-FVAL(I)
            IF (DABS(CHG) .LT. 1D-13) THEN
              JW(I,J)= 0D0
            ELSE
              JW(I,J)= CHG/VCHG
            ENDIF
110       CONTINUE
        ENDIF
120   CONTINUE
      DO 220 I=1,NFUNC
        MAXELT= 0D0
        DO 200 J=1,NVAR
          IF (DABS(JW(I,J)) .GT. MAXELT) THEN
            MAXELT= DABS(JW(I,J))
          ENDIF
200     CONTINUE
        IF (MAXELT .GT. 0D0) THEN
          SCALE(I)= 1D0/MAXELT
          DO 210 J=1,NVAR
            JW(I,J)= JW(I,J)*SCALE(I)
210       CONTINUE
        ELSE
          SCALE(I)= 1D0
        ENDIF
220   CONTINUE
C
      RETURN
      END
C
C
C ====================================================================
C Root-finding scheme for solving a set of nfunc=nreq+ndes nonlinear
C equations in nvar unknowns:
C   r (v) = r (v) = ... = r (v) = 0    (actually |r | <= rtol)
C    1       2             nreq                    i
C
C   d (v) = d (v) = ... = d (v) = 0    (actually |d | <= dtol)
C    1       2             ndes                    i
C The r's are "required" functions while the d's are only "desired".
C That is, we adjust the nvar variables v to find a solution which keeps
C each of the r's below rtol, and keeps the d's as small as possible
C without violating any of the r's.  Least squares residuals are attempted
C if the equations cannot be solved.  No guarantees can be made that
C this routine will find a solution even if there is one.  The better
C the initial values for the v's, the more likely it is to succeed and
C the more quickly it will converge.
C
C A single function func is provided which calculates all of the
C r's and d's for the current v and returns the residuals.  A parameter
C param is passed through unchanged to the function. 
C
C The array lock has an element corresponding to each variable.  Any
C variable which may be modified should have its corresponding lock
C element set to 0.  If non-zero, the variable will not be altered here.
C
C Maxeval sets an upper limit on the number of calls to func which may
C be made.  The root finder only returns after complete iterations, so
C it may make somewhat more than maxeval calls.  On return, the actual
C number of calls made is returned in fcnt.  Err is returned 0 if we
C successfully reduce all the r's to below rtol and all the d's to below
C dtol.  If the r's are met but not the d's, we return with err=1, and if
C the r's are not met we return err=2.
C
C Work arrays should be dimensioned as follows:
C   jw is nf*nv
C   dw is 2*(nf+nv)**2
C   rw is 7*nf+9*nv
C   iw is 4*(nf+nv)
C ====================================================================
C
C
      SUBROUTINE SDROOT(FUNC,VARS,PARAM,NFUNC,NVAR,NDESIN,LOCK,RTOL,DTOL
     & ,MAXEVAL,JW,DW,RW,IW,FRET,FCNT,ERR)
      EXTERNAL FUNC
      DOUBLE PRECISION VARS(NVAR),PARAM(*)
      INTEGER NFUNC,NVAR,NDESIN,LOCK(NVAR)
      DOUBLE PRECISION RTOL,DTOL
      INTEGER MAXEVAL
      DOUBLE PRECISION JW(*),DW(*),RW(*)
      INTEGER IW(*)
      DOUBLE PRECISION FRET(NFUNC)
      INTEGER FCNT,ERR
C
      INTEGER I,SLOWCNT,TOOSLOW,NTRYS,NREQ,FIXITS,NDES,DNORM
      INTEGER F1,F2,SCALE,DELTAV,GUESS,RVARS,RDELTAV,RGUESS,MORERW,MAPF,
     & MAPV,MOREIW
      DOUBLE PRECISION QRTOL,MAXDERR,MAXRERR,DERRNORM,PMAXDERR,PMAXRERR,
     & PDERRNORM,STEP,CRUDE,IMPR,RSTEP,PREVERR
C
      IF (NDESIN .LT. 0) THEN
        NDES= -NDESIN
        DNORM= 1
      ELSE
        NDES= NDESIN
        DNORM= 0
      ENDIF
      NREQ= NFUNC-NDES
      QRTOL= 1D-13
      FCNT= 0
      ERR= 0
      F1= 1
      F2= F1+NFUNC
      SCALE= F2+NFUNC
      DELTAV= SCALE+NFUNC
      GUESS= DELTAV+NVAR
      RVARS= GUESS+NVAR
      RDELTAV= RVARS+NVAR
      RGUESS= RDELTAV+NVAR
      MORERW= RGUESS+NVAR
      MAPF= 1
      MAPV= MAPF+NFUNC
      MOREIW= MAPV+NVAR
      DO 100 I=1,NFUNC
        IW(MAPF-1+I)= I
100   CONTINUE
      DO 110 I=1,NVAR
        IW(MAPV-1+I)= I
110   CONTINUE
      CALL FUNC(VARS,PARAM,FRET)
      FCNT= FCNT+1
      CALL SDCALCERRS(FRET,NFUNC,NDES,DNORM,PMAXDERR,PMAXRERR,PDERRNORM)
      CRUDE= 1D-6
      DO 590 NTRYS=1,2
        SLOWCNT= 0
        STEP= 1D0
490     CONTINUE
          IF ((PMAXRERR .LE. RTOL) .AND. (PMAXDERR .LE. DTOL)) THEN
            RETURN
          ENDIF
          IF (FCNT .GE. MAXEVAL) THEN
C goto givingUp
            GOTO 600
          ENDIF
          CALL SDCALCJAC(FUNC,VARS,PARAM,NFUNC,NVAR,LOCK,CRUDE,FRET,RW(
     &     F1),JW,FCNT,RW(SCALE))
          DO 210 I=1,NFUNC
            RW(F1-1+I)= FRET(I)*RW(SCALE-1+I)
210       CONTINUE
          CALL SDLSSLV(NFUNC,NVAR,NFUNC,NVAR,NDES,IW(MAPF),IW(MAPV)
     &     ,QRTOL,DW,RW(MORERW),IW(MOREIW),JW,RW(F1),RW(DELTAV))
390       CONTINUE
            CALL SDADJVARS(FUNC,VARS,PARAM,NFUNC,NDES,DNORM,NVAR,RW(
     &       DELTAV),STEP,PMAXRERR,PDERRNORM,RTOL,FCNT,RW(GUESS),RW(F1))
            CALL SDCALCERRS(RW(F1),NFUNC,NDES,DNORM,MAXDERR,MAXRERR,
     &       DERRNORM)
            IF ((PDERRNORM .GT. DERRNORM) .AND. (PMAXRERR .LE. RTOL)
     &        .AND. (MAXRERR .GT. RTOL) .AND. (MAXRERR .LE. .05D0)) THEN
              DO 220 I=1,NVAR
                RW(RVARS-1+I)= RW(GUESS-1+I)
220           CONTINUE
              RSTEP= 1D0
              PREVERR= MAXRERR
              DO 290 FIXITS=1,5
                CALL SDCALCJAC(FUNC,RW(RVARS)
     &           ,PARAM,NREQ,NVAR,LOCK,CRUDE,RW(F1),RW(F2),JW,FCNT,RW(
     &           SCALE))
                DO 230 I=1,NREQ
                  RW(F2-1+I)= RW(F1-1+I)*RW(SCALE-1+I)
230             CONTINUE
                CALL SDLSSLV(NREQ,NVAR,NREQ,NVAR,0,IW(MAPF),IW(MAPV)
     &           ,QRTOL,DW,RW(MORERW),IW(MOREIW),JW,RW(F2),RW(RDELTAV))
240             CONTINUE
                  CALL SDADJVARS(FUNC,RW(RVARS)
     &             ,PARAM,NFUNC,NDES,DNORM,NVAR,RW(RDELTAV)
     &             ,RSTEP,PREVERR,PDERRNORM,RTOL,FCNT,RW(RGUESS),RW(F2))
                  CALL SDCALCERRS(RW(F2),NFUNC,NDES,DNORM,MAXDERR,
     &             MAXRERR,DERRNORM)
                  IF (PREVERR-MAXRERR .GE. RTOL) THEN
                    GOTO 245
                  ELSE
                    RSTEP= RSTEP*.5D0
                    IF (RSTEP .LT. .01D0) THEN
C goto couldntFixReqs
                      GOTO 380
                    ENDIF
                  ENDIF
                GOTO 240
245             CONTINUE
                IF (DERRNORM .GT. PDERRNORM) THEN
C goto couldntFixReqs
                  GOTO 380
                ENDIF
                DO 250 I=1,NFUNC
                  RW(F1-1+I)= RW(F2-1+I)
250             CONTINUE
                DO 260 I=1,NVAR
                  RW(RVARS-1+I)= RW(RGUESS-1+I)
260             CONTINUE
                PREVERR= MAXRERR
                IF (MAXRERR .LE. RTOL) THEN
C goto fixUpSucceeded
                  GOTO 300
                ENDIF
                RSTEP= RSTEP*2D0
                IF (RSTEP .GT. 1D0) THEN
                  RSTEP= 1D0
                ENDIF
290           CONTINUE
C goto couldntFixReqs
              GOTO 380
C fixUpSucceeded
300           CONTINUE
              DO 310 I=1,NVAR
                RW(GUESS-1+I)= RW(RVARS-1+I)
310           CONTINUE
            ENDIF
            IF ((PMAXRERR .LE. RTOL) .AND. (MAXRERR .LE. RTOL)) THEN
              IMPR= PDERRNORM-DERRNORM
              IF (IMPR .LT. .1D0*DTOL+.01D0*DERRNORM) THEN
                TOOSLOW= 1
              ELSE
                TOOSLOW= 0
              ENDIF
            ELSE
              IMPR= PMAXRERR-MAXRERR
              IF (IMPR .LT. .01D0*RTOL+.01D0*MAXRERR) THEN
                TOOSLOW= 1
              ELSE
                TOOSLOW= 0
              ENDIF
            ENDIF
            IF (IMPR .GE. 0D0) THEN
              GOTO 395
            ENDIF
C couldntFixReqs
380         CONTINUE
            STEP= STEP*.5D0
            IF (NTRYS .EQ. 2) THEN
              IF (STEP .LT. 1D-5) THEN
C goto nextcrude
                GOTO 580
              ENDIF
            ELSE
              IF (STEP .LT. .01D0) THEN
C goto nextcrude
                GOTO 580
              ENDIF
            ENDIF
          GOTO 390
395       CONTINUE
          DO 400 I=1,NFUNC
            FRET(I)= RW(F1-1+I)
400       CONTINUE
          DO 410 I=1,NVAR
            VARS(I)= RW(GUESS-1+I)
410       CONTINUE
          PMAXDERR= MAXDERR
          PMAXRERR= MAXRERR
          PDERRNORM= DERRNORM
          IF (TOOSLOW .NE. 0) THEN
            SLOWCNT= SLOWCNT+1
            IF (SLOWCNT .GE. 3) THEN
C goto nextcrude
              GOTO 580
            ENDIF
          ELSE
            SLOWCNT= 0
          ENDIF
          STEP= STEP*2D0
          IF (STEP .GT. 1D0) THEN
            STEP= 1D0
          ENDIF
        GOTO 490
C nextcrude
580     CONTINUE
        CRUDE= CRUDE*.001D0
590   CONTINUE
C givingUp
600   CONTINUE
      IF (PMAXRERR .GT. RTOL) THEN
        ERR= 2
      ELSE
        IF (PMAXDERR .GT. DTOL) THEN
          ERR= 1
        ENDIF
      ENDIF
C
      RETURN
      END
C
C Utility routine for use with sdfinteg and sdvinteg.  Work is 2*neq.
C
C
      SUBROUTINE SDRK4M(FUNC,TIME,ST,DST0,PARAM,STEP,NST,NEQ,WORK,ERRS,
     & MAXERR,WHICH)
      EXTERNAL FUNC
      DOUBLE PRECISION TIME,ST(NEQ),DST0(NEQ),PARAM(*),STEP,NST(NEQ)
      INTEGER NEQ
      DOUBLE PRECISION WORK(*),ERRS(NEQ),MAXERR
      INTEGER WHICH
C
      INTEGER I,DST1,DST2,ERRF
      DOUBLE PRECISION STEP2,STEP3,STEP6,STEP8,ERR,OLD,AST
C
      DST1= 1
      DST2= DST1+NEQ
      STEP2= STEP/2D0
      STEP3= STEP/3D0
      STEP6= STEP/6D0
      STEP8= STEP/8D0
      DO 100 I=1,NEQ
        NST(I)= ST(I)+DST0(I)*STEP3
100   CONTINUE
      CALL FUNC(TIME+STEP3,NST,WORK(DST1),PARAM,ERRF)
      DO 110 I=1,NEQ
        NST(I)= ST(I)+(DST0(I)+WORK(DST1-1+I))*STEP6
110   CONTINUE
      CALL FUNC(TIME+STEP3,NST,WORK(DST1),PARAM,ERRF)
      DO 120 I=1,NEQ
        NST(I)= ST(I)+(DST0(I)+3D0*WORK(DST1-1+I))*STEP8
120   CONTINUE
      CALL FUNC(TIME+STEP2,NST,WORK(DST2),PARAM,ERRF)
      DO 130 I=1,NEQ
        NST(I)= ST(I)+(DST0(I)-3D0*WORK(DST1-1+I)+4D0*WORK(DST2-1+I)
     &   )*STEP2
130   CONTINUE
      CALL FUNC(TIME+STEP,NST,WORK(DST1),PARAM,ERRF)
      MAXERR= 0D0
      WHICH= 1
      DO 140 I=1,NEQ
        OLD= NST(I)
        NST(I)= ST(I)+(DST0(I)+4D0*WORK(DST2-1+I)+WORK(DST1-1+I))*STEP6
        ERR= DABS(.2D0*(OLD-NST(I)))
        AST= DABS(NST(I))
        IF (AST .GT. .1D0) THEN
          ERRS(I)= ERR/(AST+1D-20)
        ELSE
          ERRS(I)= 10D0*ERR
        ENDIF
        IF (ERRS(I) .GT. MAXERR) THEN
          MAXERR= ERRS(I)
          WHICH= I
        ENDIF
140   CONTINUE
C
      RETURN
      END
C
C A fixed-step integrator.  Work should be dimensioned 4*neq.
C
C
      SUBROUTINE SDFINTEG(FUNC,TIME,ST,DST,PARAM,STEP,NEQ,WORK,ERREST,
     & STATUS)
      EXTERNAL FUNC
      DOUBLE PRECISION TIME,ST(NEQ),DST(NEQ),PARAM(*),STEP
      INTEGER NEQ
      DOUBLE PRECISION WORK(*),ERREST
      INTEGER STATUS
C
      INTEGER I,WHICH,NST,ERRS,MOREWORK
      DOUBLE PRECISION TTIME
C
      NST= 1
      ERRS= NST+NEQ
      MOREWORK= ERRS+NEQ
      TTIME= TIME
      IF (STEP .GT. 0D0) THEN
        CALL SDRK4M(FUNC,TTIME,ST,DST,PARAM,STEP,WORK(NST),NEQ,WORK(
     &   MOREWORK),WORK(ERRS),ERREST,WHICH)
        DO 100 I=1,NEQ
          ST(I)= WORK(NST-1+I)
100     CONTINUE
        TTIME= TTIME+STEP
      ELSE
        ERREST= 0D0
      ENDIF
      STATUS= 0
      CALL FUNC(TTIME,ST,DST,PARAM,STATUS)
      TIME= TTIME
C
      RETURN
      END
C
C A variable-step integrator.  Work should be dimensioned 6*neq.
C
      SUBROUTINE SDVINTEG(FUNC,TIME,ST,DST,PARAM,DT,STEP,NEQIN,TOL,WORK,
     & ERR,WHICH)
      EXTERNAL FUNC
      DOUBLE PRECISION TIME,ST(NEQIN),DST(NEQIN),PARAM(*),DT,STEP
      INTEGER NEQIN
      DOUBLE PRECISION TOL,WORK(*)
      INTEGER ERR,WHICH
C
      INTEGER I,WH,WH2,XST,XST2,XDST,ERRS,MOREWORK,ERRF,NEQ,EARLYRET
      DOUBLE PRECISION TTIME,NTIME,XTIME,TFIN,STP,STP2,NSTP,MAXERR,
     & MINSTEP
C
      IF (NEQIN .LT. 0) THEN
        NEQ= -NEQIN
        EARLYRET= 1
      ELSE
        NEQ= NEQIN
        EARLYRET= 0
      ENDIF
      XST= 1
      XST2= XST+NEQ
      XDST= XST2+NEQ
      ERRS= XDST+NEQ
      MOREWORK= ERRS+NEQ
      MINSTEP= 1D-10
      TTIME= TIME
      TFIN= TTIME+DT
      STP= STEP
      IF (STP .LT. MINSTEP) THEN
        STP= MINSTEP
      ENDIF
      ERR= 0
      IF (DT .LE. 0D0) THEN
        ERRF= 0
        CALL FUNC(TTIME,ST,DST,PARAM,ERRF)
        IF (ERRF .NE. 0) THEN
          ERR= 3
          WHICH= ERRF
        ENDIF
        RETURN
      ENDIF
200   IF (TTIME  .LT.  TFIN) THEN
        IF (TTIME+1.25D0*STP .GE. TFIN) THEN
          STP= TFIN-TTIME
        ENDIF
        NSTP= STP
100     CONTINUE
          CALL SDRK4M(FUNC,TTIME,ST,DST,PARAM,NSTP,WORK(XST),NEQ,WORK(
     &     MOREWORK),WORK(ERRS),MAXERR,WH)
          XTIME= TTIME+NSTP
          IF (MAXERR .LE. TOL) THEN
            ERRF= 0
            CALL FUNC(XTIME,WORK(XST),WORK(XDST),PARAM,ERRF)
            IF (ERRF .EQ. 0) THEN
              STP2= 2D0*NSTP
              IF ((MAXERR*64D0 .LE. TOL) .AND. (STP2 .LE. DT)) THEN
                STP= STP2
              ELSE
                STP= NSTP
              ENDIF
              GOTO 110
            ENDIF
          ENDIF
          STP2= NSTP/2D0
          IF (STP2 .LT. MINSTEP) THEN
            ERRF= 0
            CALL FUNC(XTIME,WORK(XST),WORK(XDST),PARAM,ERRF)
            IF (ERRF .NE. 0) THEN
              ERR= 3
              WHICH= ERRF
              TIME= TTIME
              STEP= NSTP
              CALL FUNC(TTIME,ST,DST,PARAM,ERRF)
              RETURN
            ENDIF
            IF (ERR .EQ. 0) THEN
              ERR= 1
              WHICH= WH
            ENDIF
            IF (XTIME+NSTP .GT. TFIN) THEN
              STP2= TFIN-XTIME
            ELSE
              STP2= NSTP
            ENDIF
            NTIME= XTIME+STP2
            IF (NTIME .EQ. XTIME) THEN
              STP= NSTP
              GOTO 110
            ENDIF
            CALL SDRK4M(FUNC,XTIME,WORK(XST),WORK(XDST),PARAM,STP2,WORK(
     &       XST2),NEQ,WORK(MOREWORK),WORK(ERRS),MAXERR,WH2)
            IF (WORK(ERRS-1+WH) .LE. TOL) THEN
              CALL FUNC(XTIME,WORK(XST),WORK(XDST),PARAM,ERRF)
              STP= NSTP
              GOTO 110
            ENDIF
            ERR= 2
            WHICH= WH
            TIME= TTIME
            STEP= NSTP
            CALL FUNC(TTIME,ST,DST,PARAM,ERRF)
            RETURN
          ELSE
            NSTP= STP2
          ENDIF
        GOTO 100
110     CONTINUE
        TTIME= XTIME
        DO 120 I=1,NEQ
          ST(I)= WORK(XST-1+I)
          DST(I)= WORK(XDST-1+I)
120     CONTINUE
        IF (STP .GE. MINSTEP) THEN
          STEP= STP
        ENDIF
        NTIME= TTIME+STP
        IF (NTIME .EQ. TTIME) THEN
          TTIME= TFIN
        ENDIF
        IF (EARLYRET .NE. 0) THEN
          TIME= TTIME
          RETURN
        ENDIF
        GOTO 200
      ENDIF
      TIME= TFIN
C
      RETURN
      END
C
      SUBROUTINE SDDC2ANG(DIRCOS,A1,A2,A3)
      DOUBLE PRECISION DIRCOS(3,3),A1,A2,A3
C
      DOUBLE PRECISION TH1,TH2,TH3,TEMP(10)
C
      IF (((DABS(DIRCOS(1,3))-1D0) .GE. -1D-15)  ) THEN
        TH1= DATAN2(DIRCOS(3,2),DIRCOS(2,2))
        IF ((DIRCOS(1,3) .GT. 0D0)  ) THEN
          TEMP(1)= 1.5707963267949D0
        ELSE
          TEMP(1)= -1.5707963267949D0
        ENDIF
        TH2= TEMP(1)
        TH3= 0D0
      ELSE
        TH1= DATAN2(-DIRCOS(2,3),DIRCOS(3,3))
        TH2= DASIN(DIRCOS(1,3))
        TH3= DATAN2(-DIRCOS(1,2),DIRCOS(1,1))
      ENDIF
      A1= TH1
      A2= TH2
      A3= TH3
C
      RETURN
      END
C
      SUBROUTINE SDDC2QUAT(DIRCOS,E1,E2,E3,E4)
      DOUBLE PRECISION DIRCOS(3,3),E1,E2,E3,E4
C
      DOUBLE PRECISION TMP,TMP1,TMP2,TMP3,TMP4,TEMP(10)
C
      TMP= (DIRCOS(1,1)+(DIRCOS(2,2)+DIRCOS(3,3)))
      IF (((TMP .GE. DIRCOS(1,1)) .AND. ((TMP .GE. DIRCOS(2,2)) .AND. (
     & TMP .GE. DIRCOS(3,3))))  ) THEN
        TMP1= (DIRCOS(3,2)-DIRCOS(2,3))
        TMP2= (DIRCOS(1,3)-DIRCOS(3,1))
        TMP3= (DIRCOS(2,1)-DIRCOS(1,2))
        TMP4= (1D0+TMP)
      ELSE
        IF (((DIRCOS(1,1) .GE. DIRCOS(2,2)) .AND. (DIRCOS(1,1) .GE. 
     &   DIRCOS(3,3)))  ) THEN
          TMP1= (1D0-(TMP-(2D0*DIRCOS(1,1))))
          TMP2= (DIRCOS(1,2)+DIRCOS(2,1))
          TMP3= (DIRCOS(1,3)+DIRCOS(3,1))
          TMP4= (DIRCOS(3,2)-DIRCOS(2,3))
        ELSE
          IF ((DIRCOS(2,2) .GE. DIRCOS(3,3))  ) THEN
            TMP1= (DIRCOS(1,2)+DIRCOS(2,1))
            TMP2= (1D0-(TMP-(2D0*DIRCOS(2,2))))
            TMP3= (DIRCOS(2,3)+DIRCOS(3,2))
            TMP4= (DIRCOS(1,3)-DIRCOS(3,1))
          ELSE
            TMP1= (DIRCOS(1,3)+DIRCOS(3,1))
            TMP2= (DIRCOS(2,3)+DIRCOS(3,2))
            TMP3= (1D0-(TMP-(2D0*DIRCOS(3,3))))
            TMP4= (DIRCOS(2,1)-DIRCOS(1,2))
          ENDIF
        ENDIF
      ENDIF
      TMP= (1D0/DSQRT(((TMP1*TMP1)+((TMP2*TMP2)+((TMP3*TMP3)+(TMP4*TMP4)
     & )))))
      E1= (TMP*TMP1)
      E2= (TMP*TMP2)
      E3= (TMP*TMP3)
      E4= (TMP*TMP4)
C
      RETURN
      END
C
      SUBROUTINE SDANG2DC(A1,A2,A3,DIRCOS)
      DOUBLE PRECISION A1,A2,A3,DIRCOS(3,3)
C
      DOUBLE PRECISION COS1,COS2,COS3,SIN1,SIN2,SIN3
C
      COS1= DCOS(A1)
      COS2= DCOS(A2)
      COS3= DCOS(A3)
      SIN1= DSIN(A1)
      SIN2= DSIN(A2)
      SIN3= DSIN(A3)
      DIRCOS(1,1)= (COS2*COS3)
      DIRCOS(1,2)= -(COS2*SIN3)
      DIRCOS(1,3)= SIN2
      DIRCOS(2,1)= ((COS1*SIN3)+(SIN1*(COS3*SIN2)))
      DIRCOS(2,2)= ((COS1*COS3)-(SIN1*(SIN2*SIN3)))
      DIRCOS(2,3)= -(COS2*SIN1)
      DIRCOS(3,1)= ((SIN1*SIN3)-(COS1*(COS3*SIN2)))
      DIRCOS(3,2)= ((COS1*(SIN2*SIN3))+(COS3*SIN1))
      DIRCOS(3,3)= (COS1*COS2)
C
      RETURN
      END
C
      SUBROUTINE SDQUAT2DC(IE1,IE2,IE3,IE4,DIRCOS)
      DOUBLE PRECISION IE1,IE2,IE3,IE4,DIRCOS(3,3)
C
      DOUBLE PRECISION E1,E2,E3,E4,E11,E22,E33,E44,NORM
C
      E11= IE1*IE1
      E22= IE2*IE2
      E33= IE3*IE3
      E44= IE4*IE4
      NORM= DSQRT(E11+E22+E33+E44)
      IF (NORM .EQ. 0D0) THEN
        E4= 1D0
        NORM= 1D0
      ELSE
        E4= IE4
      ENDIF
      NORM= 1D0/NORM
      E1= IE1*NORM
      E2= IE2*NORM
      E3= IE3*NORM
      E4= E4*NORM
      E11= E1*E1
      E22= E2*E2
      E33= E3*E3
      DIRCOS(1,1)= 1D0-(2D0*(E22+E33))
      DIRCOS(1,2)= 2D0*(E1*E2-E3*E4)
      DIRCOS(1,3)= 2D0*(E1*E3+E2*E4)
      DIRCOS(2,1)= 2D0*(E1*E2+E3*E4)
      DIRCOS(2,2)= 1D0-(2D0*(E11+E33))
      DIRCOS(2,3)= 2D0*(E2*E3-E1*E4)
      DIRCOS(3,1)= 2D0*(E1*E3-E2*E4)
      DIRCOS(3,2)= 2D0*(E2*E3+E1*E4)
      DIRCOS(3,3)= 1D0-(2D0*(E11+E22))
C
      RETURN
      END
C
      FUNCTION SDVDOT(IVEC1,IVEC2)
      DOUBLE PRECISION IVEC1(3),IVEC2(3)
      DOUBLE PRECISION SDVDOT
C
      DOUBLE PRECISION DPROD
C
C Compute the dot product of two vectors
C
      DPROD= IVEC1(1)*IVEC2(1)+IVEC1(2)*IVEC2(2)+IVEC1(3)*IVEC2(3)
      SDVDOT= DPROD
      RETURN
C
      END
C
      FUNCTION SDVNORM(IVEC)
      DOUBLE PRECISION IVEC(3)
      DOUBLE PRECISION SDVNORM
C
      DOUBLE PRECISION NORM,SDVDOT
C
C Compute the length (norm) of a vector
C
      NORM= DSQRT(SDVDOT(IVEC,IVEC))
      SDVNORM= NORM
      RETURN
C
      END
C
      SUBROUTINE SDVCOPY(IVEC,OVEC)
      DOUBLE PRECISION IVEC(3),OVEC(3)
C
C
C Copy vector ivec to vector ovec
C
      OVEC(1)= IVEC(1)
      OVEC(2)= IVEC(2)
      OVEC(3)= IVEC(3)
C
      RETURN
      END
C
      SUBROUTINE SDVSET(SCLR1,SCLR2,SCLR3,OVEC)
      DOUBLE PRECISION SCLR1,SCLR2,SCLR3,OVEC(3)
C
C
C Set ovec to [sclr1,sclr2,sclr3]
C
      OVEC(1)= SCLR1
      OVEC(2)= SCLR2
      OVEC(3)= SCLR3
C
      RETURN
      END
C
      SUBROUTINE SDVADD(IVEC1,IVEC2,OVEC)
      DOUBLE PRECISION IVEC1(3),IVEC2(3),OVEC(3)
C
C
C Add two vectors
C
      OVEC(1)= IVEC1(1)+IVEC2(1)
      OVEC(2)= IVEC1(2)+IVEC2(2)
      OVEC(3)= IVEC1(3)+IVEC2(3)
C
      RETURN
      END
C
      SUBROUTINE SDVSUB(IVEC1,IVEC2,OVEC)
      DOUBLE PRECISION IVEC1(3),IVEC2(3),OVEC(3)
C
C
C Subtract two vectors
C
      OVEC(1)= IVEC1(1)-IVEC2(1)
      OVEC(2)= IVEC1(2)-IVEC2(2)
      OVEC(3)= IVEC1(3)-IVEC2(3)
C
      RETURN
      END
C
      SUBROUTINE SDVMUL(SCLR,IVEC,OVEC)
      DOUBLE PRECISION SCLR,IVEC(3),OVEC(3)
C
C
C Multiply a vector by a scalar
C
      OVEC(1)= SCLR*IVEC(1)
      OVEC(2)= SCLR*IVEC(2)
      OVEC(3)= SCLR*IVEC(3)
C
      RETURN
      END
C
      SUBROUTINE SDVAXPY(SCLR,IVEC1,IVEC2,OVEC)
      DOUBLE PRECISION SCLR,IVEC1(3),IVEC2(3),OVEC(3)
C
C
C Multiply a vector by a scalar and add in another vector
C
      OVEC(1)= SCLR*IVEC1(1)+IVEC2(1)
      OVEC(2)= SCLR*IVEC1(2)+IVEC2(2)
      OVEC(3)= SCLR*IVEC1(3)+IVEC2(3)
C
      RETURN
      END
C
      SUBROUTINE SDVCROSS(IVEC1,IVEC2,OVEC)
      DOUBLE PRECISION IVEC1(3),IVEC2(3),OVEC(3)
C
      DOUBLE PRECISION TEMPOUT(3)
C
C Compute the cross product of two vectors
C
      TEMPOUT(1)= IVEC1(2)*IVEC2(3)-IVEC1(3)*IVEC2(2)
      TEMPOUT(2)= IVEC1(3)*IVEC2(1)-IVEC1(1)*IVEC2(3)
      TEMPOUT(3)= IVEC1(1)*IVEC2(2)-IVEC1(2)*IVEC2(1)
      OVEC(1)= TEMPOUT(1)
      OVEC(2)= TEMPOUT(2)
      OVEC(3)= TEMPOUT(3)
C
      RETURN
      END
C
      SUBROUTINE SDVROT(IVEC,RVEC,THETA,OVEC)
      DOUBLE PRECISION IVEC(3),RVEC(3),THETA,OVEC(3)
C
      DOUBLE PRECISION SDVDOT,SDVNORM,NORM,IDOTU,CTHETA,STHETA,DOTCOS,
     & UVEC(3),ICOSVEC(3),ISINVEC(3),UXISIN(3)
C
C Rotate a vector ivec around vector rvec by angle theta
C
      NORM= SDVNORM(RVEC)
      IF (NORM .EQ. 0D0) THEN
        OVEC(1)= IVEC(1)
        OVEC(2)= IVEC(2)
        OVEC(3)= IVEC(3)
        CALL SDSETERR(62,46)
        RETURN
      ENDIF
      STHETA= DSIN(THETA)
      CTHETA= DCOS(THETA)
      CALL SDVMUL(STHETA,IVEC,ISINVEC)
      CALL SDVMUL(CTHETA,IVEC,ICOSVEC)
      NORM= 1D0/NORM
      CALL SDVMUL(NORM,RVEC,UVEC)
      IDOTU= SDVDOT(IVEC,UVEC)
      CALL SDVCROSS(UVEC,ISINVEC,UXISIN)
      DOTCOS= IDOTU*(1D0-CTHETA)
      CALL SDVAXPY(DOTCOS,UVEC,ICOSVEC,OVEC)
      CALL SDVADD(OVEC,UXISIN,OVEC)
C
      RETURN
      END
C
      SUBROUTINE SDSERIALNO(SERNO)
      INTEGER SERNO
C
C
      SERNO= 10001
C
      RETURN
      END
