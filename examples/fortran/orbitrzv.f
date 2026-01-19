       integer NV,NF,NVF,SDW
       parameter (NV=2,NF=2,NVF=NF+NV,SDW=2*NVF*NVF)
       real*8 res(NF),miss(5,5),stab,qdot0(NV)
       real*8 parm(3),jw(NF*NV),dw(SDW),rw(9*NVF),fret(NF)
       integer lock(NV),iw(4*NVF),fcnt,err,i,j
       external resid

       call SDINIT

c enable constraint stabilization for the prescribed motion
       stab = .01d0
       call SDSTAB(2d0*stab, stab*stab)

c first bracket region where solution lies.

       parm(1) = 1d-4
       parm(2) = 1
       parm(3) = 0d0

       do i = 1,5
          qdot0(1) = 100d0*i -300d0
          do j = 1,5
            qdot0(2) = 100d0*j - 300d0
            call resid(qdot0,parm,res)
            miss(i,j) = dsqrt(res(1)*res(1) + res(2)*res(2))
          end do
        end do
 
        do i = 1,5
           write(6,100) (miss(i,j),j=1,5)
           write(10,100) (miss(i,j),j=1,5)
        end do
 
 100    format(5(e12.5,2x))
        pause 'DONE WITH BRACKETING TABLE'

c now use the root-finder

       qdot0(1) = 0d0
       qdot0(2) = -100d0
       parm(1) = 1d-7
       parm(2) = 1
       lock(1) = 1

       call SDROOT(resid,qdot0,parm,2,2,0,lock,1d-3,1d0,500,
     1             jw,dw,rw,iw,fret,fcnt,err)

       write(6,*) 'solution for initial velocities:'
       write(6,*) qdot0
       write(6,*) 'final miss distance:',fret

c now compute final trajectory to check results

       parm(2) = 25
       parm(3) = 1d0
       call resid(qdot0,parm,res)

       call SDPRINTERR(6)
       end

       subroutine resid(qdot,parm,res)
       integer NQ,NU,NEQ,NVARS,NFUNCS,A,B
       integer flag,err,npts,i,SDINDX
       parameter (NQ=4,NU=4,NEQ=NQ+NU)
       parameter (NVARS=2,NFUNCS=2,A=1,B=2)
       real*8 t,qdot(NVARS),y(NEQ),dy(NEQ),n,res(NFUNCS)
       real*8 period,pi,dt,ctol,tol,parm(3)
       common /orbrate/ n

       npts = parm(2)

       period = 6000d0
       pi = acos(-1d0)
       n = 2d0*pi/period

c initial conditions are set here. The velocities are passed-in.

       t = 0d0
       do i=1,NEQ
          y(i) = 0d0
       end do

       y(SDINDX(B,1)) = 50d3
       y(SDINDX(B,2)) = 100d3
       y(NQ+SDINDX(A,1)) = n
       y(NQ+SDINDX(B,1)) = qdot(1)
       y(NQ+SDINDX(B,2)) = qdot(2)

       dt = period/4d0/npts
       ctol = 0.1*parm(1)
       tol  = parm(1)
       flag = 1

       do i = 1,npts
          if(parm(3) .ne. 0d0)  write(11,100) t,y(2),y(3)

          call SDMOTION(t,y,dy,dt,ctol,tol,flag,err)
          if(err .ne. 0) write(6,*) 'err from SDMOTION:',err
       end do

       if(parm(3) .ne. 0d0)  write(11,100) t,y(2),y(3)

c the value of x and y after a quarter period is returned as the residual.

       res(1) = y(SDINDX(B,1))
       res(2) = y(SDINDX(B,2))

100    format(5(e12.5,2x))
       return
       end

       subroutine sdumotion(t,q,u)
       integer NQ,NU,A
       parameter (NQ=4, NU=4, A=1)
       real*8 t,q(NQ),u(NU),n
       common /orbrate/ n
       call SDPRESPOS(A,1,n*t)
       call SDPRESVEL(A,1,n)
       call SDPRESACC(A,1,0d0)
       return
       end

       subroutine sduforce(t,q,u)
       integer NQ,NU,A,B
       parameter (NQ=4, NU=4, A=1, B=2)
       real*8 t,q(NQ),u(NU),n,mb
       real*8 x,y,fb(3),com(3)
       integer SDINDX
       common /orbrate/ n
       data com/3*0d0/

       call SDGETMASS(B,mb)
       x = q(SDINDX(B,1))
       y = q(SDINDX(B,2))
       fb(1) = -mb*n*n*(-2d0*x)
       fb(2) = -mb*n*n*(y)
       fb(3) = 0d0
       call SDTRANS(A,fb,B,fb)
       call SDPOINTF(B,com,fb)
       return 
       end
