c  Spacecraft simulation example.  This is the example from Tutorial 3 of
c  the SD/FAST manual.  Time histories for each of the four analyses
c  go into fort.11, fort.12, fort.13, and fort.14, resp.

c  NQ = # of q's (generalized coordinates)
c  NU = # of u's (generalized speeds)
c  NEQ = # of equations to integrate (q's, u's and command rates)
c  AZCMD, ELCMD = location of command values in system state array
        
        integer NQ,NU,NEQ,AZCMD,ELCMD
        parameter (NQ=12,NU=11,AZCMD=NQ+NU+1,ELCMD=AZCMD+1,NEQ=NQ+NU+2)

c  The body numbers.  Note that these are also the numbers for these
c  bodies' inboard joints.

        integer GROUND,BUS,CLOCK,BOOM,CAMERA,SCANNER
        parameter (GROUND=0,BUS=1,CLOCK=2,CAMERA=3,BOOM=4,SCANNER=5)

        real*8 y(NEQ),t,dt,tol,camscan(3)
        real*8 savey(NEQ),tscan,slwstrt,slwstp,azrate,elrate,scanrt
        integer nout,nbtw,i,scan,SDINDX

        common /scanner/tscan,scanrt,scan
        common /slewparm/slwstrt,slwstp,azrate,elrate

c  The slew is set up to begin at the camera current position, to slew 
c  for 10s at the indicated rates, after waiting for 1s.
        slwstrt = 1d0
        slwstp  = 11d0
        azrate  = -.025d0
        elrate  = .01d0

c  During integration, report any state or constraint errors gt tol.
        tol = 1d-3

c  When the scanner is enabled, it scans at this rate (rad/s).
        scanrt = 1d0

c  We'll start out using the default parameters for the system, as 
c  provided in the System Description File.
        call SDINIT

c==================================================================
c  ANALYSIS #1: simulate the slew maneuver with the scanner locked.
c==================================================================

c  Set up initial conditions.  Start with everything at 0 (reference
c  configuration).  We'll fill in the base body orientation coordinates
c  as though they were 1-2-3 Euler angles, rather than quaternions.  
c  (Then the reference configuration is 0,0,0.)  Then we can use 
c  SDANG2ST to convert to quaternions.

        t = 0d0
        do 10 i=1,NEQ
 10            y(i) = 0d0
        call SDANG2ST(y,y)

c  Command the camera's initial azimuth and elevation, and set the
c  initial conditions for these angles to the commanded values.
        y(AZCMD) = 4d0
        y(ELCMD) = -0.5d0
        y(SDINDX(CLOCK,1))  = y(AZCMD)
        y(SDINDX(CAMERA,1)) = y(ELCMD)

c  Lock the scanner.
        scan = 0

c  We run long enough to let the system settle down after the slew.
        dt = .05
        nout = 1000
        nbtw = 4
        call simulate(1,nout,nbtw,dt,tol,scan,t,y)

c  Save the state at the end of the above simulation, so we can run
c  several analyses each beginning at this state.

        do 40 i=1,NEQ
 40            savey(i) = y(i)
        tscan = t

c===================================================================
c  ANALYSIS #2: continue run with scanner locked as a control study.
c===================================================================
c After finishing the slew, the camera is supposed to remain still in 
c inertial space, with its bore sight at AZCMD and ELCMD in the 
c 'celestial sphere'.  Now as a control, we'll observe the pointing 
c accuracy with the scanner off, continuing the above simulation.

        nout = 1000
        nbtw = 1
        call simulate(2,nout,nbtw,dt,tol,scan,t,y)

c===================================================================
c  ANALYSIS #3: simulate effect of scanner on camera pointing error.
c===================================================================
c We will enable the scanner and observe its effect on camera pointing 
c accuracy.  (Scanner motion is defined in motions() as a sinusoid 
c beginning at tscan.)
        scan = 1

c  Put state back to the end of the first analysis.
        do 44 i=1, NEQ
 44             y(i) = savey(i)
        y(   SDINDX(SCANNER,1)) = 0d0
        y(NQ+SDINDX(SCANNER,1)) = scanrt
        t = tscan

        call simulate(3,nout,nbtw,dt,tol,scan,t,y)

c=================================================================
c  ANALYSIS #4: repeat #3 but with different location for scanner.
c=================================================================
        
        write(6,*) 'Changing geometry.'
         camscan(2) = -.2d0
        call SDITJ(SCANNER,camscan)
        call SDINIT

c  Put state back to the end of the first analysis.
        do 60 i=1, NEQ
 60             y(i) = savey(i)
        y(   SDINDX(SCANNER,1)) = 0d0
        y(NQ+SDINDX(SCANNER,1)) = scanrt
        t = tscan

        call simulate(4,nout,nbtw,dt,tol,scan,t,y)

        call SDPRINTERR(6)
        end

c simulate
c
c Run a simulation of the spacecraft to produce nout datapoints beginning
c at time t and separated by (nbtw*dt) seconds.  Complain if the integration
c error or a constraint error exceeds tol.  Output all the interesting
c data.  Set scan nonzero to enable the scanner.  Output goes to file
c 10+<simulation number>, i.e., fort.11, fort.12, fort.13, or fort.14.
c
        subroutine simulate(n,nout,nbtw,dt,tol,scan,t,y)
        integer NQ,NU,NEQ,AZCMD,ELCMD,BUS,BOOM,SCANNER
        parameter (NQ=12,NU=11,AZCMD=NQ+NU+1,ELCMD=AZCMD+1,NEQ=NQ+NU+2)
        parameter (BUS=1,BOOM=4,SCANNER=5)
        integer i,j,n,nout,nbtw,scan,err,SDINDX
        real*8 dt,tol,t,y(NEQ),param(1),dy(NEQ),cone,ori(3),dc(3,3)
        real*8 est,work(4*NEQ),fire(3)
        common /firing/ fire
        external deriv

        write(6,*)'Simulation #',n,' for ',dt*nout*nbtw,
     &            ' seconds from t=',t
        if (scan .ne. 0) write(6,*)'Scanner is on.'
        if (scan .eq. 0) write(6,*)'Scanner is off.'

c  Make sure the thrusters are off to start.
        fire(1) = 0d0
        fire(2) = 0d0
        fire(3) = 0d0

c  Supply the tolerance for the prescribed motion constraint.
        param(1) = tol

c  Integrator requires correct derivatives passed in dy -- evaluate now.
        call deriv(t,y,dy,param,err)

c  Integrate and write out data.  Note that bus orientation is 1-2-3 
c  Euler angles (millirad) rather than Euler parameters.

        do 20 i = 1,nout+1
          call SDORIENT(BUS,dc)
          call SDDC2ANG(dc, ori(1), ori(2), ori(3))
          call pointerr(y(AZCMD),y(ELCMD),cone)

          write(10+n,30) t, ori(1)*1000., ori(2)*1000., ori(3)*1000., 
     &      y(SDINDX(SCANNER,1)), y(SDINDX(BOOM,1))*1000., 
     &      y(SDINDX(BOOM,2))*1000., fire(1), fire(2), fire(3), cone

          if (i .le. nout) then
              do 10 j = 1,nbtw
                call SDFINTEG(deriv,t,y,dy,param,dt,NEQ,work,est,err)
                if ((err .ne. 0) .or. (est .gt. tol)) then
                    print *,'at time=',t,' err=',err,' errest=',est
                end if
 10           continue
          end if
 20     continue
 30     format(10f10.5)
        end

c  deriv
c
c  Compute state derivatives.  We'll return with non-zero status if 
c  a constraint error is larger than a particular tolerance.  The 
c  tolerance is passed in param(1).  (In this problem, the only 
c  constraint is the prescribed motion on the scanner.)

        subroutine deriv(time,state,dstate,param,status)
        integer NQ,NU,NC,AZCMD,ELCMD
        parameter (NQ=12,NU=11,NC=1,AZCMD=NQ+NU+1,ELCMD=AZCMD+1)
        real*8 time,state(*),dstate(*),param(1),errs(NC)
        real*8 slwstrt,slwstp,azrate,elrate
        common /slewparm/slwstrt,slwstp,azrate,elrate
        integer status,i

        call SDSTATE(time,state,state(NQ+1))
        call forces(time,state,state(NQ+1),state(AZCMD),state(ELCMD))
        call motions(time,state,state(NQ+1))
        call SDDERIV(dstate,dstate(NQ+1))

C  Determine the currently commanded rate for the clock and camera
C  motors.  This rate is a constant during the slewing maneuver, and
C  zero otherwise. 

        if ((time .ge. slwstrt) .and. (time .lt. slwstp)) then
            dstate(AZCMD) = azrate
            dstate(ELCMD) = elrate
        else
            dstate(AZCMD) = 0.0D0
            dstate(ELCMD) = 0.0D0
        end if

c  Check that constraint errors are below tol.

        status = 1

        call SDVERR(errs)
        do 10 i=1,NC
 10            if (abs(errs(i)) .gt. param(1)) return
        call SDPERR(errs)
        do 20 i=1,NC
 20            if (abs(errs(i)) .gt. param(1)) return

        status = 0

        return
        end

c  forces
c
c  This subroutine takes time, the system state, and the camera
c  azimuth and elevation commands as inputs. These are used to
c  generate sensor outputs which are then passed to the actuator
c  routine.  Bending of the flexible boom is handled in a separate
c  subroutine.

        subroutine forces(t,q,u,az,el)
        real*8 t,q(*),u(*),az,el
        real*8 azerr,azerrt,elerr,elerrt,buserr(3)

c  Sense errors in camera and base body orientations and rates.
        call sensor(q,u,az,el,azerr,azerrt,elerr,elerrt,buserr)

c  Apply control forces to reduce the sensed errors.
        call actuator(t,azerr,azerrt,elerr,elerrt,buserr)

c  Apply forces to model boom flexibility.
        call boomflex(q,u)

        return
        end




c  motions
c
c  Prescribe the scanner motion.  If `scan' is zero, set the motion to
c  0 (i.e., locked).  Otherwise, prescribe it to follow a sinusoidal 
c  motion with frequency w=scanrt radian per second.

        subroutine motions(t,q,u)
        integer NQ,NU,SCANNER
        parameter (NQ=12, NU=11, SCANNER=5)
        real*8 t,q(NQ),u(NU),w,tscan,scanrt
        integer scan
        common /scanner/tscan,scanrt,scan

        w = scanrt
        if (scan .eq. 0) then
          call SDPRESPOS(SCANNER,1,0d0)
          call SDPRESVEL(SCANNER,1,0d0)
          call SDPRESACC(SCANNER,1,0d0)
        else
          call SDPRESPOS(SCANNER,1,      sin(w*(t-tscan)))
          call SDPRESVEL(SCANNER,1,    w*cos(w*(t-tscan)))
          call SDPRESACC(SCANNER,1, -w*w*sin(w*(t-tscan)))
        end if

        return
        end

c  sensor
c
c  The sensed quantities are camera az and el positions and
c  rates, base body attitude error and error rate (123 Euler Angles).

        subroutine sensor(q,u,azcmd,elcmd,
     &                          azerr,azerrt,elerr,elerrt,err)
        real*8 q(*),u(*),azcmd,elcmd,azerr,azerrt,elerr,elerrt
        real*8 atterr(3),raterr(3),dc(3,3),err(3),gyro
        integer BUS,CLOCK,CAMERA,SDINDX,i
        parameter (BUS=1,CLOCK=2,CAMERA=3)
        data gyro/2d0/

        azerr  = azcmd - q(SDINDX(CLOCK,1))
        elerr  = elcmd - q(SDINDX(CAMERA,1))
        azerrt = -u(SDINDX(CLOCK,1))
        elerrt = -u(SDINDX(CAMERA,1))

c  Convert bus attitude to 1-2-3 Euler angles.
        call SDORIENT(BUS, dc)
        call SDDC2ANG(dc,atterr(1),atterr(2),atterr(3))

c  Get bus inertial angular velocity (in the bus frame).
        call SDANGVEL(BUS,raterr)

c  The deadband logic mixes pos and vel errors for each axis.
        do 10 i = 1,3
 10            err(i) = gyro*raterr(i) + atterr(i)

        return
        end




c  actuator
c
c  This routine applies the forces and torques acting on the spacecraft,
c  as a function of the passed-in errors.

        subroutine actuator(t,azerr,azerrt,elerr,elerrt,err)
        real*8 t,azerr,azerrt,elerr,elerrt,err(3)
        real*8 torq(3),thrust,k1,k2,b1,b2,l(3)
        integer BUS,CLOCK,CAMERA,axis
        parameter (BUS=1,CLOCK=2,CAMERA=3)
        data k1,k2,b1,b2/2*3500d0,2*20d0/
        data l/0.23d0,0.21d0,0.31d0/

c  The camera controller just uses rate and position error feedback.
        call SDHINGET(CLOCK,1,  k1*azerr + b1*azerrt)
        call SDHINGET(CAMERA,1, k2*elerr + b2*elerrt)

c  Compute base body torques from model of thrusters and their 
c  controller, expressed in base body frame.

        do 10 axis = 1,3
 10            torq(axis) = l(axis)*thrust(t,axis,err(axis))
        call SDBODYT(BUS,torq)

        return
        end

c  boomflex
c
c  This routine models the first cantilever mode of the boom, using
c  empirically derived stiffness and damping constants.

        subroutine boomflex(q,u)
        real*8 q(*),u(*),k,b,torq
        integer BOOM,SDINDX
        parameter (BOOM=4)
        data k,b/2000d0,10d0/

        torq = - (k*q(SDINDX(BOOM,1)) + b*u(SDINDX(BOOM,1)))
        call SDHINGET(BOOM,1,torq)
        torq = - (k*q(SDINDX(BOOM,2)) + b*u(SDINDX(BOOM,2)))
        call SDHINGET(BOOM,2,torq)

        return
        end




c  thrust
c
c  Thruster model. Each thruster can fire in the positive or negative
c  direction.  If the error is large enough to trigger a thruster
c  firing, the thruster must remain on for a minimum amount of time.
c  This routine can cause problems for variable step integrators because
c  of the 'memory' built into the routine.

        real*8 function thrust(t,axis,error)
        real*8 t,error,dband,tmin,fire(3),toff(3)
        integer axis
        common /firing/ fire
        data dband,tmin /0.0025d0, 0.02d0/
        data toff /3*0d0/

        if (error .lt. -dband) then
            fire(axis) = 1d0
            toff(axis) = t + tmin
        else if (error .gt. dband) then
            fire(axis) = -1d0
            toff(axis) = t + tmin
        else if (t .ge. toff(axis)) then
            fire(axis) = 0d0
        end if

        thrust = fire(axis)
        end

c To compute pointing accuracy, just reading the gimbal angle errors 
c is insufficient since it does not take into account base body pointing 
c error.  If the system is in its reference configuration, the correct 
c camera pointing vector v is given by first rotating the camera
c about the clock z-axis, followed by a rotation about the camera -x axis. 
c Expressed in the inertial frame this is:
c
c      v = [ -sin(az)*cos(el)   cos(az)*cos(el)   -sin(el) ]
c
c The pointing error can be found by transforming the actual camera
c bore sight into the inertial frame, dot multiplying with v to
c obtain the cosine of the cone angle, and then using acos.  The cone
c angle is reported in milliradians.

        subroutine pointerr(az,el,cone)
        real*8 az,el,cone,v(3),bore(3),boreg(3),c
        integer CAMERA,GROUND
        parameter (GROUND=0,CAMERA=3)

c  This is the orientation of the bore sight in the camera's local frame.
        data bore/0d0,1d0,0d0/

        v(1) = -sin(az)*cos(el)
        v(2) =  cos(az)*cos(el)
        v(3) = -sin(el)

        call SDTRANS(CAMERA,bore,GROUND,boreg)
        c = boreg(1)*v(1) + boreg(2)*v(2) + boreg(3)*v(3)

c  Avoid tiny numerical errors which might cause acos to complain.
        if (c .gt. 1d0) c = 1d0
        if (c .lt. -1d0) c = -1d0
        cone = acos(c)*1000d0
        end
