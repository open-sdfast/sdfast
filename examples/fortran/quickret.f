c  quickret
c
c  This is a set of analyses performed on a quick-return mechanism
c  used as a cutter in a machine tool discussed in Tutorial 4 of the
c  SD/FAST User's Manual, version B.1.0.  See quickret.sd for a description
c  of the mechanism.
c
       program quickret
 
c Parameters of the quick return model, from the quickret_info file.
       integer NQ,NU,NEQ,NJNT,NC
       parameter(NQ=5,NU=5,NEQ=NQ+NU,NJNT=7,NC=11)

c Parameters for use with the root finder. NF=number of functions,
c NV=number of variables, the rest are sizes of work arrays.
       integer NF,NV,JWSZ,DWSZ,RWSZ,IWSZ
       parameter(NF=1,NV=1,JWSZ=NF*NV, DWSZ=2*(NF+NV)*(NF+NV),
     1           RWSZ=7*NF+9*NV, IWSZ=4*(NF+NV))

c Body numbers (also the joint number of the associated inboard joint).
       integer GROUND,TOOL,CRANK,SLEEVE,COUPLER,ROCKER
       parameter(GROUND=0,TOOL=1,CRANK=2,SLEEVE=3,COUPLER=4,ROCKER=5)

       real*8 t,pi,dtr,y(NEQ),dy(NEQ),yinit(NEQ)
       real*8 rps,ctol,tol,qdot(NQ),udot(NU),vec(3)
       integer fcnt,err,i,i1,flag,SDINDX
       real*8 r1,r3,theta2,theta4,theta5
       real*8 w(3),r1d,r3d,theta4d,theta5d
       real*8 alpha(3),r1dd,r3dd,theta4dd,theta5dd
       real*8 force(NJNT,3),torque(NJNT,3)
       real*8 fx,fy,torq,crx,cry
       real*8 theta2d,theta2dd,dt,delta,parm(3)
       real*8 com(3),pos(3),vel(3)
       real*8 jw(JWSZ),dw(DWSZ),rw(RWSZ),fret(NF)
       integer ilock,iw(IWSZ)
       integer rank,multmap(NC)
       real*8 mults(NC),perr(NC),verr(NC)
       common /radps/rps,theta2
       common /pidtr/pi,dtr
       common /icond/ yinit
       external resid
       data com/3*0d0/

       pi = acos(-1d0)
       dtr = pi/180d0

       call SDINIT

c This is an initial guess at the assembled configuration, to make sure
c we get into the right branch.

       do 10 i=1,NEQ
 10        yinit(i) = 0d0
       yinit(SDINDX(CRANK,1))  =  90d0*dtr
       yinit(SDINDX(SLEEVE,1)) = -60d0*dtr
       yinit(SDINDX(ROCKER,1)) =  135d0*dtr

c EXERCISE #1: ASSEMBLY and INITIAL VELOCITY ANALYSIS

c Start the system out with crank angle at ninety degrees, crank
c angular velocity of 20 rad/sec.

c Start with prescribed motion ON at crank.
       call SDPRES(CRANK,1,1)

       do 15 i = 1,NEQ
 15        y(i) = yinit(i)

c       write(6,*) ' '
c       write(6,*) 'EXERCISE 1: ASSEMBLY and INITIAL VELOCITY ANALYSIS'
c       write(6,*) ' '

       t = 0d0
       rps = 20d0
       theta2 = 90d0

c do assembly and velocity analysis
           call initconds(t,theta2,rps,y)

c check if position and velocity errors are below ctol
       call sdperr(perr)
       write(6,*) 'Position errors for the 11 constraints:'
       write(6,*) perr
       call sdverr(verr)
       write(6,*) 'Velocity errors for the 11 constraints:'
       write(6,*) verr
       write(6,*) ' '

c verify that the mechanism assembled into the correct branch.

       write(6,*) 'Mechanism generalized coodinates (relative q''s)'
       write(6,*) ' '
       write(6,100) 'TOOL DISPLACEMENT:',y(SDINDX(TOOL,1))
       write(6,100) 'CRANK ANGLE      :',y(SDINDX(CRANK,1))/dtr
       write(6,100) 'SLEEVE ANGLE     :',y(SDINDX(SLEEVE,1))/dtr
       write(6,100) 'COUPLER EXTENSION:',y(SDINDX(COUPLER,1))
       write(6,100) 'ROCKER ANGLE     :',y(SDINDX(ROCKER,1))/dtr
       write(6,*) ' '

       call SDPOS(TOOL,com,pos)
       write(6,100) 'TOOL  LOCATION:',pos
       call SDPOS(CRANK,com,pos)
       write(6,100) 'CRANK LOCATION:',pos
       call SDPOS(SLEEVE,com,pos)
       write(6,100) 'SLEEVE LOCATION:',pos
       call SDPOS(COUPLER,com,pos)
       write(6,100) 'COUPLER LOCATION:',pos
       call SDPOS(ROCKER ,com,pos)
       write(6,100) 'ROCKER LOCATION:',pos

       write(6,*) ' '
       call SDVEL(TOOL,com,vel)
       write(6,100) 'TOOL  VELOCITY:',vel
       call SDVEL(CRANK,com,vel)
       write(6,100) 'CRANK VELOCITY:',vel
       call SDVEL(SLEEVE,com,vel)
       write(6,100) 'SLEEVE VELOCITY:',vel
       call SDVEL(COUPLER,com,vel)
       write(6,100) 'COUPLER VELOCITY:',vel
       call SDVEL(ROCKER ,com,vel)
       write(6,100) 'ROCKER VELOCITY:',vel

100    format(a20,3(f14.4))
       write(6,*) ' '
      pause 'DONE WITH EXERCISE #1'

c EXERCISE #2: INVERSE DYNAMICS

c Rotate crank at 20 rad per sec and compute crank torque required 
c to maintain this speed while load is applied to cutter.

c Start with prescribed motion ON at crank.
       call SDPRES(CRANK,1,1)

       do 20 i = 1,NEQ
 20           y(i) = yinit(i)

       write(6,*) ' '
       write(6,*) 'EXERCISE 2: INVERSE DYNAMICS ANALYSIS'
       write(6,*) ' '

       t = 0d0
       rps = 20d0
       theta2 = 90d0

c Move theta2 (crank angle) 360 degrees, 10 degrees per step.
       do 30 i1 = 1,37

c  do assembly and velocity analysis
           call initconds(t,theta2,rps,y)

c  apply forces and motions and compute resulting derivatives 
           call sduforce(t,y,y(NQ+1))
           call sdumotion(t,y,y(NQ+1))
           call SDDERIV(qdot,udot)

c calculate outputs for inverse dynamics analysis
c theta2  - crank angle (input) (degrees)
c theta4  - angle between COUPLER and global Y (degrees)
c theta5  - angle between ROCKER and global Y (degrees)
c theta2d,theta4d,theta5d - angular rates in radians/s
c theta2dd,theta4dd,theta5dd - angular accelerations in rad/s**2
c r1      - TOOL displacement from O
c r3      - distance between pts A and B
c torq    - driving torque required on crank
c crx,cry - x,y components of crank-ground reaction (ground frame)
c fx,fy   - x,y components of rocker-ground reaction (gnd frame)
c rank    - rank of the constraint matrix. if rank < NC, then redundancies

          theta2d  = y(NQ+SDINDX(CRANK,1))
          theta2dd = udot(SDINDX(CRANK,1))

          theta4 = theta2 + y(SDINDX(SLEEVE,1))/dtr
          call SDANGVEL(COUPLER,w)
          theta4d = w(3)
          call SDANGACC(COUPLER,alpha)
          theta4dd = alpha(3)

          theta5 = y(SDINDX(ROCKER,1))/dtr
          call SDANGVEL(ROCKER,w)
          theta5d = w(3)
          call SDANGACC(ROCKER,alpha)
          theta5dd = alpha(3)

          r1   = y(   SDINDX(TOOL,1))
          r1d  = y(NQ+SDINDX(TOOL,1))
          r1dd = udot(SDINDX(TOOL,1))

          r3   = y(   SDINDX(COUPLER,1))
          r3d  = y(NQ+SDINDX(COUPLER,1))
          r3dd = udot(SDINDX(COUPLER,1))

          call SDGETHT(CRANK,1,torq)
          call SDREAC(force,torque)

          vec(1) = force(CRANK,1)
          vec(2) = force(CRANK,2)
          vec(3) = force(CRANK,3)
          call SDTRANS(CRANK,vec,GROUND,vec)
          crx = vec(1)
          cry = vec(2)

          vec(1) = force(ROCKER,1)
          vec(2) = force(ROCKER,2)
          vec(3) = force(ROCKER,3)
          call SDTRANS(ROCKER,vec,GROUND,vec)
          fx = vec(1)
          fy = vec(2)

          call SDMULT(mults,rank,multmap)

          write(6,105)  theta2,theta4,theta4d,theta4dd
          write(61,105) theta2,theta4,theta4d,theta4dd
          write(6,105)  theta2,theta5,theta5d,theta5dd
          write(62,105) theta2,theta5,theta5d,theta5dd
          write(6,105)  theta2,r1,r1d,r1dd
          write(63,105) theta2,r1,r1d,r1dd
          write(6,105)  theta2,r3,r3d,r3dd
          write(64,105) theta2,r3,r3d,r3dd
          write(6,105)  theta2,torq,crx,cry
          write(65,105) theta2,torq,crx,cry
          write(6,105)  theta2,fx,fy
          write(66,105) theta2,fx,fy
          write(6,106)  rank
          write(99,106) rank
          write(6,*) ' '
105       format(5(f14.3))
106          format(i10)

          theta2 =  theta2 + 10d0
 30   continue
          pause 'DONE WITH EXERCISE #2'

c EXERCISE #3: MECHANICAL ADVANTAGE

c Step crank from 90 deg through 450 deg and compute crank torque required 
c to resist load applied to cutter.

c start with prescribed motion ON at crank

       call SDPRES(CRANK,1,1)

       do 40 i = 1,NEQ
 40           y(i) = yinit(i)

       write(6,*) ' '
       write(6,*) 'EXERCISE 3: MECHANICAL ADVANTAGE'
       write(6,*) ' '

       rps = 0d0
       theta2 = 90d0
       write(6,108)'theta2','torque','mech. advan.'
       do 50 i1 = 1,361

c  do assembly and velocity analysis
           t = 0d0
           call initconds(t,theta2,rps,y)

c  now compute derivatives 
           call sduforce(t,y,y(NQ+1))
           call sdumotion(t,y,y(NQ+1))
           call SDDERIV(qdot,udot)

c calculate outputs for mechanical advantage analysis
c torq = driving torque required on crank
c m.a. = (cutter force) / (crank torque)

           call SDGETHT(CRANK,1,torq)

           write(6,105)  theta2,torq,-450d0/torq
           write(67,105) theta2,torq,-450d0/torq

           theta2 =  theta2 + 1d0
 50    continue
       pause 'DONE WITH EXERCISE #3'

c EXERCISE #4: DYNAMIC ANALYSIS

c Start crank at 20 rad per sec and compute time history, using 
c motor torque vs. rpm curve.

       write(6,*) ' '
       write(6,*) 'EXERCISE 4: DYNAMIC ANALYSIS'
       write(6,*) ' '
       write(6,*) 'initial crank angular velocity: 20 rad/sec'
       write(6,108) 't', 'theta2', 'theta2d','theta2dd'
108    format(5(a14))

c turn prescribed motion  OFF  at crank
       call SDPRES(CRANK,1,0)

c  do assembly and velocity analysis
       do 60 i = 1,NEQ
 60        y(i) = yinit(i)
       t = 0d0
       rps = 20d0
       theta2 = 90d0
       call initconds(t,theta2,rps,y)

c  now perform a motion analysis

       flag = 1
       dt   = 0.002d0
       ctol = 1d-4
       tol  = 1d-5

c  call once with dt=0 just to evaluate derivatives at t=0

       call SDMOTION(t,y,dy,0d0,ctol,tol,flag,err)
       if (err .ne. 0) write(6,*) 'at t = ',t,'error=',err

c  simulate for long enough to cover a full cycle
       do 70 i1 = 1,151

c calculate outputs for dynamic analysis 
c theta2 - crank angle
c theta2d - crank angular velocity
c theta2dd - crank angular acceleration

           theta2   = y(   SDINDX(CRANK,1))/dtr
           theta2d  = y(NQ+SDINDX(CRANK,1))
           theta2dd = dy(NQ+SDINDX(CRANK,1))
           write(6,105)  t,theta2,theta2d,theta2dd
           write(70,105) t,theta2,theta2d,theta2dd

           call SDMOTION(t,y,dy,dt,ctol,tol,flag,err)
           if (err .ne. 0) write(6,*) 'at t = ',t,'error=',err
 70    continue
       pause 'DONE WITH EXERCISE #4'

c EXERCISE #5: DESIGN STUDY

c We want the mechanism to operate in a steady cycle. This
c can only happen for a particular initial crank velocity.
c We will compute the crank terminal angular velocity (after one
c revolution) for various initial crank angular velocities.  We're
c looking for the initial crank velocity which produces an identical
c terminal crank velocity.

       write(6,*) ' '
       write(6,*) 'EXERCISE 5: DESIGN STUDY'
       write(6,*) ' '

c turn prescribed motion  OFF  at crank
       call SDPRES(CRANK,1,0)

c start with velocity at 10 and increase by 1 each trial
       rps = 10d0
       do 90 i1 = 1,11

c  do assembly and velocity analysis

           do 80 i = 1,NEQ
 80               y(i) = yinit(i)
           t = 0d0
           theta2 = 90d0
           call initconds(t,theta2,rps,y)

c Now evaluate the amount by which the terminal angular velocity differs
c from the initial angular velocity.  This requires perfoming a simulation
c of one cycle.  `resid' calculates and prints out the values as we go.

           ctol = 1d-2
           tol  = 1d-3
           dt   = 0.01d0
           parm(1) = ctol
           parm(2) = tol
           parm(3) = dt
           call resid(rps,parm,delta)

           rps = rps + 1d0
 90    continue
       pause 'DONE WITH EXERCISE #5'

c EXERCISE #6: FIND LIMIT CYCLE INITIAL CONDITIONS

c We will repeat previous study, but this time we will use the
c SD/FAST root-finder to find the initial crank angular velocity which 
c will initiate a steady period.  

       write(6,*) ' '
       write(6,*) 'EXERCISE 6: FIND LIMIT CYCLE INITIAL CONDITIONS'
       write(6,*) ' '

c turn prescribed motion  OFF  at crank
       call SDPRES(CRANK,1,0)

       rps = 10d0
       ctol = 1d-2
       tol  = 1d-3
       dt   = 0.01d0
       parm(1) = ctol
       parm(2) = tol
       parm(3) = dt
       ilock = 0
       call SD2ROOT(resid,rps,parm,NF,NV,0,ilock,tol,0d0,50,
     1              jw,dw,rw,iw,fret,fcnt,err)
       if (err .ne. 0) then
           write(6,*) 'failed to find solution, err=', err
       else
           write(6,*) 'correct initial speed:',rps
       endif
       pause 'DONE WITH EXERCISE #6'

c END OF EXERCISES

c check for SD/FAST routine usage error

       call SDPRINTERR(6)

       end

c resid
c
c Perform a single cycle of the mechanism, and then return the amount
c by which the initial angular velocity of the crank (rps) differs
c from the final angular velocity.  Accuracy is controlled by parm.

       subroutine resid(rps,parm,delta)
       integer NQ,NU,NEQ,CRANK,err,i,flag,SDINDX
       parameter(NQ=5,NU=5,NEQ=NQ+NU,CRANK=2)
       real*8 rps,parm(3),delta,y(NEQ),yinit(NEQ),dy(NEQ)
       real*8 t,dt,pi,dtr,th,thd,w,theta2,theta2d,ctol,tol
       common /pidtr/pi,dtr
       common /icond/yinit

c Get constraint and integration tols, and maximum step size dt.
       ctol = parm(1)
       tol  = parm(2)
       dt   = parm(3)

c  do assembly and velocity analysis
       do 10 i = 1,NEQ
 10             y(i) = yinit(i)
       t = 0d0
       theta2 = 90d0
       theta2d = rps
       call initconds(t,theta2,theta2d,y)
       write(6,*) 'initial conditions:',t,theta2d

c now integrate until we complete one revolution
       flag = 1
       th = theta2
100    continue
           call SDMOTION(t,y,dy,dt,ctol,tol,flag,err)
           if (err .ne. 0) write(6,*) 'at t = ',t,' error=',err
           theta2  = y(   SDINDX(CRANK,1))/dtr
           theta2d = y(NQ+SDINDX(CRANK,1))

c          detect stalled condition
           if (th .ge. theta2) then
               write(6,*) 'not going around'
               stop
           end if
           
           if (theta2 .lt. 450d0) then
               th = theta2
               thd = theta2d
               goto 100
           end if
c      end integration loop

c the crank made a full revolution. Interpolate to compute
c crank angular velocity corresponding to one rev.

       w = (theta2d - thd)*(450d0 - th)/(theta2 - th) + thd
       delta = w - rps
       write(6,*) 'terminal velocity:', w, '   delta:', delta
       write(6,*) ' '
       write(71,*) rps,delta
      
       return
       end

c sduforce
c
c This routine computes and applies the forces acting in the system.

       subroutine sduforce(t,q,u)
       real*8 t,q(*),u(*),cutter(3),fk(3),w,torq
       integer presval,TOOL,CRANK,SDINDX
       parameter(TOOL=1,CRANK=2)
       data cutter/0d0,0.025d0,0d0/

c Compute and apply the cutter force. If the tool is moving forward, 
c the force is applied.  Otherwise no force is applied.  `cutter'
c is the location of the point on body TOOL at which the cutter force is 
c to be applied.

       fk(1) = 0d0
       if (u(SDINDX(TOOL,1)) .ge. 0d0) fk(1) = -450d0
       call SDPOINTF(TOOL,cutter,fk)

c If the crank motion is not prescribed, we'll use motor torque-rpm 
c line and apply a torque to crank.

       call SDGETPRES(CRANK,1,presval)
       if (presval .eq. 0) then
           w = u(SDINDX(CRANK,1))
           torq = 90d0 - 3d0 * w
           if ( w .gt. 30d0) torq = 0d0
           if ( w .lt. 0d0 ) torq = 90d0
           call SDHINGET(CRANK,1,torq)
       end if

       return
       end




c sdumotion
c
c If enabled, prescribe crank motion to a constant velocity rps plus an 
c initial crank rotation theta2, both passed in common.

       subroutine sdumotion(t,q,u)
       real*8 t,q(*),u(*),rps,theta2,pos,vel,acc,pi,dtr
       integer presval,CRANK
       parameter(CRANK=2)
       common /radps/rps,theta2
       common /pidtr/pi,dtr

       acc = 0d0
       vel = rps
       pos = theta2*dtr + t*vel

       call SDGETPRES(CRANK,1,presval)
       if (presval .ne. 0) then
           call SDPRESPOS (CRANK,1,pos)
           call SDPRESVEL (CRANK,1,vel)
           call SDPRESACC (CRANK,1,acc)
       end if

       return
       end

c  initconds
c
c  Set up initial conditions.  Initializes crank angle to passed-in
c  theta2 (degrees), and crank velocity to passed-in rps (rad/s).  Pass 
c  in y with an initial guess to control assembled branch and improve 
c  convergence speed.  On return, y is a fully compatible state vector 
c  unless an error message has been printed out.

       subroutine initconds(t,theta2,rps,y)
       integer NQ,NU,NEQ,CRANK
       parameter(NQ=5,NU=5,NEQ=NQ+NU,CRANK=2)
       real*8 t,theta2,rps,y(NEQ),ctol,pi,dtr
       common /pidtr/pi,dtr
       integer lock(NU),fcnt,err,SDINDX

c lock the crank angle and rate to the passed-in values
       data lock/0,1,0,0,0/
       ctol = 1e-7

       y(SDINDX(CRANK,1)) =  theta2*dtr
       call SDASSEMBLE (t,y,lock,ctol,500,fcnt,err)
       if( err .ne. 0) write(6,*) 'assembly failed'

       y(NQ+SDINDX(CRANK,1)) = rps 
       call SDINITVEL(t,y,lock,ctol,500,fcnt,err)
       if( err .ne. 0) write(6,*) 'velocity analysis failed'

       return
       end
