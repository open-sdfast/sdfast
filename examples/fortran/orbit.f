       integer NQ,NU,NEQ,A,B
       parameter (NQ=4, NU=4, NEQ=NQ+NU, A=1, B=2)
       real*8 t,y(NEQ),dy(NEQ),n,period,pi,dt,ctol,tol
       integer flag,err,i,SDINDX
       common /orbrate/ n

       call SDINIT
       t = 0d0
       period = 6000
       pi = acos(-1d0)
       n = 2d0*pi/period

       y(NQ+SDINDX(A,1)) = n
       y(NQ+SDINDX(B,1)) = 1d-2
       call SDSTATE(t,y,y(NQ+1))

       dt = period/100d0
       ctol = 1d-5
       tol  = 1d-6
       flag = 1
       do i = 1,101
         write(6,100)  t,y(SDINDX(B,1)),y(SDINDX(B,2)),y(SDINDX(B,3))
         write(9,100)  t,y(SDINDX(B,1)),y(SDINDX(B,2)),y(SDINDX(B,3))
         call SDMOTION(t,y,dy,dt,ctol,tol,flag,err)
         if (err .ne. 0) write(6,*) 'error after sdmotion = ',err
       end do
100    format(4(f15.5,2x))
       call SDPRINTERR(6)
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
