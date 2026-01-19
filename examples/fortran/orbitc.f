       integer NQ,NU,NEQ,GROUND,A,B,NF,NV,NVF,SDW
       parameter (NQ=8,NU=8,NEQ=NQ+NU,GROUND=0,A=1,B=2)
       parameter (NF=2,NV=8,NVF=NF+NV,SDW=2*NVF*NVF)
       real*8 y(NEQ),dy(NEQ),t,n,dt,tol,mtot,cm(3),icm(3,3)
       real*8 parm(50),x0,y0,xd0,yd0,period,pi
       integer i,j,flag,maxeval,nfunc
       integer iw(4*NVF),lock(NV),err,fcnt,SDINDX
       real*8 jw(NV*NF),dw(SDW),rw(9*NVF),fret(NF)
       common /orbrate/n

       external setq
       external setu

       call SDINIT

       t = 0d0
       pi = dacos(-1d0)
       period = 6000d0
       n = 2d0*pi/period

       y(SDINDX(B,3)) = 0d0
       do i = 3,6
           y(SDINDX(i,1)) = 0.01d0
       end do

       x0 =0d0
       y0 =0d0
       xd0 =0.01d0
       yd0 =0d0

       parm(1) = x0
       parm(2) = y0 
       parm(3) = NQ
       parm(4) = t

       do i = 1,NV
          lock(i) = 1
       end do
       lock(SDINDX(B,1)) = 0
       lock(SDINDX(B,2)) = 0
       nfunc =NF 
       maxeval = 500

       call SDROOT(setq,y,parm,nfunc,NQ,0,lock,1d-8,0d0,
     1             maxeval,jw,dw,rw,iw,fret,fcnt,err)

       y(NQ+SDINDX(A,1)) = n

       parm(1) = t
       parm(2) = xd0
       parm(3) = yd0
       do i = 1,NQ
           parm(3+i) = y(i)
       end do

       call SDROOT(setu,y(NQ+1),parm,nfunc,NU,0,lock,1d-8,0d0,
     1             maxeval,jw,dw,rw,iw,fret,fcnt,err)

       pause '** INITIAL CONDITIONS SET'

       dt = 60d0
       tol = 1d-6
       flag = 1

       do i = 1,101

         call SDSTATE(t,y,y(NQ+1))
         call SDSYS(mtot,cm,icm)
         call SDTRANS(GROUND,cm,A,cm)

         write(40,101) t,cm(1),cm(2)
         do j = 2,NQ
            write(40+j,101) t,y(j)
         end do

         call SDMOTION(t,y,dy,dt,10d0*tol,tol,flag,err)

       end do

       pause '** DONE WITH SIMULATION'

101    format(3(e12.5,2x))
       call SDPRINTERR(6)
       end

       subroutine sdumotion(t,q,u)
       real*8 t,q(*),u(*)
       real*8 n
       integer A
       parameter (A=1)
       common /orbrate/n
       call SDPRESPOS(A,1,n*t)
       call SDPRESVEL(A,1,n)
       call SDPRESACC(A,1,0d0)
       return
       end

       subroutine sduforce(t,q,u)
       integer i,nbod,GROUND,A
       parameter (NBOD=6,GROUND=0,A=1)
       real*8 t,q(*),u(*)
       real*8 n,fb(3),com(3),a1(3),pos(3),tb(3)
       real*8 mb,ib(3,3),dot,vec(3)
       common /orbrate/n
       data com/3*0d0/

       do i = 2,NBOD

         call SDGETMASS(i,mb)
         call SDGETINER(i,ib)
  
         a1(1) = 1d0
         a1(2) = 0d0
         a1(3) = 0d0
       
         call SDPOS(i,com,pos)
         call SDTRANS(GROUND,pos,i,pos)
         call SDTRANS(A,a1,i,a1)

         dot = a1(1)*pos(1) + a1(2)*pos(2) + a1(3)*pos(3)

         fb(1) = -mb*n*n*(pos(1) -3d0*dot*a1(1))
         fb(2) = -mb*n*n*(pos(2) -3d0*dot*a1(2))
         fb(3) = -mb*n*n*(pos(3) -3d0*dot*a1(3))

         call SDPOINTF(i,com,fb)

         vec(1) = ib(1,1)*a1(1) + ib(1,2)*a1(2) + ib(1,3)*a1(3)
         vec(2) = ib(2,1)*a1(1) + ib(2,2)*a1(2) + ib(2,3)*a1(3)
         vec(3) = ib(3,1)*a1(1) + ib(3,2)*a1(2) + ib(3,3)*a1(3)

         tb(1) = 3d0*n*n*(a1(2)*vec(3) - a1(3)*vec(2))
         tb(2) = 3d0*n*n*(a1(3)*vec(1) - a1(1)*vec(3))
         tb(3) = 3d0*n*n*(a1(1)*vec(2) - a1(2)*vec(1))

         call SDBODYT(i,tb)

       end do

       return
       end

       subroutine setq(y,parm,resid)
       real*8 y(*),parm(*),resid(2)
       real*8 mtot,cm(3),icm(3,3)
       real*8 t,x0,y0
       integer NQ,GROUND,A
       parameter(GROUND=0,A=1)

       x0 = parm(1)
       y0 = parm(2)
       NQ = parm(3)
       t  = parm(4)

       call SDSTATE(t,y,y(NQ+1))
       call SDSYS(mtot,cm,icm)
       call SDTRANS(GROUND,cm,A,cm)

       resid(1) = cm(1) - x0
       resid(2) = cm(2) - y0

       return
       end

       subroutine setu(u,parm,resid)
       real*8 u(*),parm(*),resid(2)
       real*8 lm(3),am(3),ke,mtot,cm(3),icm(3,3)
       real*8 wa(3),t,xd0,yd0,vs(3)
       integer GROUND,A
       parameter (GROUND=0,A=1)

       t   = parm(1)
       xd0 = parm(2)
       yd0 = parm(3)

       call SDSTATE(t,parm(4),u)
       call SDMOM(lm,am,ke)
       call SDSYS(mtot,cm,icm)

       vs(1) = lm(1)/mtot
       vs(2) = lm(2)/mtot
       vs(3) = lm(3)/mtot

       call SDTRANS(GROUND,vs,A,vs)
       call SDTRANS(GROUND,cm,A,cm)
       call SDANGVEL(A,wa)

       resid(1)  = vs(1) -(wa(2)*cm(3) -wa(3)*cm(2)) - xd0
       resid(2)  = vs(2) -(wa(3)*cm(1) -wa(1)*cm(3)) - yd0

       return
       end
