       program humpage
       integer NQ,NU,NEQ,NC,GROUND,B,E,F,D
       parameter (NQ=4,NU=4,NEQ=NQ+NU,NC=3,GROUND=0,B=1,E=2,F=3,D=4)
       real*8 y(NEQ),dy(NEQ),t,ctol,torque,vel(3),pa(3),pb(3),power
       real*8 frc1(3),frc2(3),frc3(3),n(3),fp1(3),fp2(3),fp3(3)
       integer lock(NU),fcnt,err,maxeval,i,SDINDX
       common /vectors/ n,fp1,fp2,fp3
       common /mesh/ frc1,frc2,frc3
       common /load/ torque
       call SDINIT
       t = 0d0

c******************************************************
c  Exercise 1: Compute input-output velocity ratio
c******************************************************

c Set input shaft velocity to 1 rad/sec. Then perform
c velocity analysis to find velocities of remaining gears.

       y(NQ+SDINDX(B,1)) = 1d0
       lock(SDINDX(B,1)) = 1
       ctol = 1d-10
       maxeval = 500

       call SDINITVEL(t,y,lock,ctol,maxeval,fcnt,err)
       if (err .ne. 0) then
           write(6,*) 'SDINITVEL returned err=',err,' fcnt=',fcnt
           stop
       endif

       write(6,*)   'BODY     ANG. VEL.'
       write(6,*)   ' '
       write(6,100) '  B   ',  y(NQ+SDINDX(B,1))
       write(6,100) '  F   ',  y(NQ+SDINDX(F,1))
       write(6,100) '  E   ',  y(NQ+SDINDX(E,1))
       write(6,*)   ' '
       write(6,*)   '  Planet rate relative to arm'
       write(6,100) '  D   ',  y(NQ+SDINDX(D,1))
100    format(a6, f14.8)

c******************************************************
c  Exercise 2: Test mechanical advantage
c******************************************************

c If we apply a unit torque to the input shaft, the gear train 
c will be static if we apply the correct load to the output shaft. 
c The right torque is the inverse of the velocity ratio.  
       torque = - y(NQ+SDINDX(B,1)) / y(NQ+SDINDX(E,1))

       call SDSTATE(t,y,y(NQ+1))
       call sduforce(t,y,y(NQ+1))
       call SDDERIV(dy,dy(NQ+1))

       write(6,*) ' '
       write(6,*) 'STATIC TEST'
       write(6,*) ' '
       write(6,*) '   GEAR ANGULAR ACCELERATIONS:'
       write(6,*) ' '
       write(6,101) (dy(NQ+i),i=1,NU)
101    format (3x,e12.5)

c******************************************************
c  Exercise 3: Power Flow through the gear train
c******************************************************

c The power flow is computed by summing the gear tooth force multiplied
c by the tooth velocity. The input power is one unit, since the input
c torque and shaft velocity were set to 1.0. Also, we know that no
c power flows to the ground, since it is not moving. Thus we can
c exclude the ground contact at gear D.

       write(6,*) ' '
       write(6,*) 'POWER FLOW'
       write(6,*) ' '

c Start with the gear between B and D

       call getcoord(F,B,D,fp3,pa,pb)
       call SDVEL(D,pb,vel)
       call SDTRANS(F,frc3,GROUND,frc3)
       write(6,102) 'gear pair','gear velocity','tooth force','power'
       write(6,*)   ' '
       power = -vel(3)*frc3(3)
       write(6,103) 'B_D',vel(3),-frc3(3),power
       write(6,*)   ' '

c Compute power flow into E from D

       call getcoord(F,E,D,fp2,pa,pb)
       call SDVEL(D,pb,vel)
       call SDTRANS(F,frc2,GROUND,frc2)
       power = vel(3)*frc2(3)
       write(6,103) 'E_D',vel(3),frc2(3),power
       
102    format(3x,a9,a20,a20,a20)
103    format(a9,10x,f12.5,10x,e12.5,10x,f12.5)




c******************************************************
c  Done.
c******************************************************
       call SDPRINTERR(6)
       stop
       end

       subroutine getcoord(case,A,B,fp,pa,pb)
       integer GROUND,case,A,B,i
       parameter (GROUND=0)
       real*8 fp(3),com(3),ra(3),rb(3),pa(3),pb(3),phat(3)
       data com/3*0d0/

c Get coordinates of contact point in case frame the vector fp is 
c the vector fixed in the case from the case mass center to the 
c contact point.

       call SDPOS(case,fp,phat)

c get coordinates of gear1 and gear2 mass center

       call SDPOS(A,com,ra)
       call SDPOS(B,com,rb)

c compute vectors from body mass centers to contact point

       do 10 i = 1,3
           pa(i) = phat(i) - ra(i)
           pb(i) = phat(i) - rb(i)
 10    continue

c transform the vectors into the body-local frames

       call SDTRANS(GROUND,pa,A,pa)
       call SDTRANS(GROUND,pb,B,pb)

       return
       end





       subroutine sduforce(t,q,u)
       integer GROUND,B,E,F,D
       parameter (GROUND=0,B=1,E=2,F=3,D=4)
       real*8 t,q(*),u(*),torque
       common /load/ torque

c Apply torques to the input and output shafts.

       call SDHINGET(B,1,1d0)
       call SDHINGET(E,1,torque)
       return
       end




c These are the data for the user constraints, followed by the required
c user constraint routines.  There are no position constraints since
c we're modeling the gears as velocity constraints.

       block data
       real*8 n(3),fp1(3),fp2(3),fp3(3)
       common /vectors/ n,fp1,fp2,fp3
       data n  /0d0, 0d0, 1d0/
       data fp1/-43.74600596901954d0, 60d0,   0d0/
       data fp2/ 71.51023030477174d0, 20.0d0, 0d0/
       data fp3/-21.34374745810949d0, 30.0d0, 0d0/
       end

       subroutine sduperr(t,q,perr)
       real*8 t,q(*),perr(*)
       perr(1) = 0d0
       perr(2) = 0d0
       perr(3) = 0d0
       return
       end

       subroutine sduverr(t,q,u,verr)
       integer GROUND,B,E,F,D
       parameter (GROUND=0,B=1,E=2,F=3,D=4)
       real*8 t,q(*),u(*),verr(*),n(3),fp1(3),fp2(3),fp3(3)
       common /vectors/ n,fp1,fp2,fp3

       call gearverr(F,GROUND,D, fp1,n,verr(1))
       call gearverr(F,     E,D, fp2,n,verr(2))
       call gearverr(F,     B,D, fp3,n,verr(3))

       return
       end

       subroutine sduaerr(t,q,u,udot,aerr)
       integer GROUND,B,E,F,D
       parameter (GROUND=0,B=1,E=2,F=3,D=4)
       real*8 t,q(*),u(*),udot(*),aerr(*),n(3),fp1(3),fp2(3),fp3(3)
       common /vectors/ n,fp1,fp2,fp3

       call gearaerr(F,GROUND,D, fp1,n,aerr(1))
       call gearaerr(F,     E,D, fp2,n,aerr(2))
       call gearaerr(F,     B,D, fp3,n,aerr(3))

       return
       end

       subroutine sduconsfrc(t,q,u,mult)
       integer GROUND,B,E,F,D
       parameter (GROUND=0,B=1,E=2,F=3,D=4)
       real*8 t,q(*),u(*),mult(*)
       real*8 n(3),fp1(3),fp2(3),fp3(3),frc1(3),frc2(3),frc3(3)
       common /vectors/ n,fp1,fp2,fp3
       common /mesh/ frc1,frc2,frc3

       call gearfrc(F,GROUND,D, fp1,n,mult(1),frc1)
       call gearfrc(F,     E,D, fp2,n,mult(2),frc2)
       call gearfrc(F,     B,D, fp3,n,mult(3),frc3)

       return
       end

       subroutine gearfrc(case,A,B,fp,n,mult,mesh)
       integer GROUND,case,A,B,i
       parameter (GROUND=0)
       real*8 fp(3),n(3),mult
       real*8 pa(3),pb(3)
       real*8 mesh(3),mesha(3),meshb(3)

c This routine applies a force mult * n to the contact
c point in gear1 and a force  -mult * n to the contact
c point in gear2 where
c                      mult is the Lagrange multiplier
c for the gear contact force.
c                      n is the gear contact surface
c normal (fixed in the case frame.)

c get coordinates of contact point in the gears' body-local frame.

       call getcoord(case,A,B,fp,pa,pb)

c compute the gear tooth force as the multiplier times the
c tooth normal. This is expressed in the case frame.

       do 10 i = 1,3
 10        mesh(i) = mult*n(i)

c transform the tooth force into the gear1 frame and apply at
c the contact point

       call SDTRANS(case,mesh,A,mesha)
       call SDPOINTF(A,pa,mesha)

c negate the force (Newton's Third Law) and apply to the second gear

       do 20 i = 1,3
 20        mesha(i) = -mesha(i)

c transform tooth force into gear2 frame and apply at contact point

       call SDTRANS(A,mesha,B,meshb)
       call SDPOINTF(B,pb,meshb)
       
       return 
       end

       subroutine gearverr(case,A,B,fp,n,verr)
       integer GROUND,case,A,B,i
       parameter (GROUND=0)
       real*8 fp(3),n(3),verr
       real*8 pa(3),pb(3)
       real*8 va(3),vb(3),vab(3)

c This routine computes the the velocity constraint error:
c
c  verr = ( Va - Vb ) . n
c
c  where Va is the velocity of the gear1 contact point.
c        Vb is the velocity of the gear2 contact point.
c        n  is the surface normal unit vector.

c get coordinates of contact point in the gears' body-
c local frame.

       call getcoord(case,A,B,fp,pa,pb)

c get the velocity of the contacting points.

       call SDVEL(A,pa,va)
       call SDVEL(B,pb,vb)

c subtract the velocities.

       do 10 i = 1,3
 10        vab(i) = va(i) - vb(i)

c transform the velocity difference into the case frame
c and dot multiply with the surface normal vector.

       call SDTRANS(GROUND,vab,case,vab)
       verr = vab(1)*n(1) + vab(2)*n(2) + vab(3)*n(3)

       return 
       end

       subroutine gearaerr(case,A,B,fp,n,aerr)
       integer GROUND,case,A,B,i
       parameter (GROUND=0)
       real*8 fp(3),n(3),aerr
       real*8 pa(3),pb(3)
       real*8 va(3),vb(3),vab(3)
       real*8 aa(3),ab(3),aab(3)
       real*8 wc(3),vec(3)

c This routine computes the the acceleration constraint error:
c Note: aerr is the derivative of verr as computed by the
c gearverr subroutine.
c
c  aerr = [ Aa - Ab + ( Va - Vb ) X Wc ] . n
c
c  where Aa is the acceleration of the gear1 contact point.
c        Ab is the acceleration of the gear2 contact point.
c        Va is the velocity     of the gear1 contact point.
c        Vb is the velocity     of the gear2 contact point.
c        Wc is the angular velocity of the case.
c        n  is the surface normal unit vector.

c get coordinates of contact point in the gears' body-local frame.

       call getcoord(case,A,B,fp,pa,pb)

c get the velocity and acceleration of the contacting points.

       call SDVEL(A,pa,va)
       call SDVEL(B,pb,vb)

       call SDACC(A,pa,aa)
       call SDACC(B,pb,ab)

c subtract the velocities and accelerations.

       do 10 i = 1,3
           vab(i) = va(i) - vb(i)
           aab(i) = aa(i) - ab(i)
 10    continue

c transform the differences into the case frame.

       call SDTRANS(GROUND,aab,case,aab)
       call SDTRANS(GROUND,vab,case,vab)

c get angular velocity of the case

       call SDANGVEL(case,wc)

c combine the acceleration term plus the cross product.

       vec(1) = aab(1) + vab(2)*wc(3) - vab(3)*wc(2)
       vec(2) = aab(2) + vab(3)*wc(1) - vab(1)*wc(3)
       vec(3) = aab(3) + vab(1)*wc(2) - vab(2)*wc(1)

       aerr = vec(1)*n(1) + vec(2)*n(2) + vec(3)*n(3)

       return 
       end
