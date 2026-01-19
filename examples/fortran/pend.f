c  pend
c
c  This is a set of analyses performed on the pendulum example in
c  Tutorial 2 of the SD/FAST User's Manual, version B.1.0.
c  See pend.sd for a description of the pendulum.
c
        PROGRAM PEND

c  Set up variables, common block to SDUFORCE, and all known parameters
        REAL*8        Y(2),DY(2),T,DT,CTOL,TOL,BIAS,FORCE(1,3)
        REAL*8        TORQUE(1,3),COMASS(3),ACC(3),M1,GRAV(3),FVEC(3)
        INTEGER        NSTEP,ILOOP,LOCK(1),MAXEVALS,FCNT,FLAG,ERR

        COMMON / USER1 / BIAS, FVEC

        DATA COMASS /3*0.0D0/
        DATA Y / 0.0D0, 0.0D0 /
        T    = 0.0D0
        DT   = 0.05D0
        CTOL = 0.0D0
        TOL  = 1.0D-4
        LOCK(1) = 0
        MAXEVALS = 1000

c  Initialize all SD subroutines before making any calls to analysis routines.
        CALL SDINIT

c  EXERCISE #1: STATIC ANALYSIS FOR 10kg PENDULUM

c  Perform static analysis for mass = 10 kg (default) and spring bias as shown.
        BIAS = 0.3D0
        CALL SDSTATIC(T,Y,LOCK,CTOL,TOL,MAXEVALS,FCNT,ERR)
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 1: Static equilibrium for mass = 10 kg '
        WRITE (6,*) ' '
        WRITE (6,*) ' Y = ',Y(1),'   FCNT = ',FCNT,'   ERR = ',ERR
        PAUSE 'DONE WITH EXERCISE #1'

c  EXERCISE #2: PERTURBATION ANALYSIS TO VERIFY STABLE EQUILIBRIUM

c  Perturb solution slightly and run a motion analysis at static configuration 
c  to verify no unstable motion occurs.
c  Note that FLAG = 1 must be set each time SDMOTION is first called or
c  the state is changed discontinuously, such as after an impulsive load.
        FLAG = 1
        NSTEP = 10
        Y(1) = Y(1) + 0.0001D0
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 2: Perturbation Analysis'
        WRITE (6,*) ' '
        WRITE (6,*) 'Check subsequent motion to verify static condition:'
        WRITE (6,*) 
     +'      Time (sec)             Angle(rad)           Rate(rad/sec)'
        DO 10 ILOOP = 1, NSTEP
          CALL SDMOTION(T,Y,DY,DT,CTOL,TOL,FLAG,ERR)
          WRITE (6,1000) T,Y
1000          FORMAT (1X,3(D18.10,5X))
10        CONTINUE
        PAUSE 'DONE WITH EXERCISE #2'

c  EXERCISE #3: STATIC ANALYSIS FOR 5kg PENDULUM

c  Redo static analysis for mass set to 5 kg, bias unchanged.
c  Note that all velocities, Y(2), must be set to zero for SDSTATIC() to work
        CALL SDMASS(1,5.0D0)
        CALL SDINIT
        Y(2) = 0.0D0
        CALL SDSTATIC(T,Y,LOCK,CTOL,TOL,MAXEVALS,FCNT,ERR)
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 3: Static equilibrium for mass = 5 kg'
        WRITE (6,*) ' '
        WRITE (6,*) ' Y = ',Y(1),'   FCNT = ',FCNT,'   ERR = ',ERR
        PAUSE 'DONE WITH EXERCISE #3'

c  EXERCISE #4: CHECK F=MA

c  Set spring bias to zero to allow the pendulum to fall.
c  At the instant of release, check that F = ma for all three axes.
c  Note that setting DT = 0 causes the derivatives to be evaluated, but 
c  not propagated.
        BIAS = 0.0D0
        DT = 0.0D0
        CALL SDMOTION(T,Y,DY,DT,CTOL,TOL,FLAG,ERR)
c  Get reaction forces and torques acting on the pendulum
        CALL SDREAC(FORCE,TORQUE)
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 4: Check F=ma on Released Pendulum'
        WRITE (6,*) ' '
        WRITE (6,*) 'Force = '
        WRITE (6,1000) FORCE
        WRITE (6,*) 'Torque= '
        WRITE (6,1000) TORQUE
c  Transform the force vector from the pendulum frame to the inertial frame
        CALL SDTRANS(1,FORCE,0,FORCE)
c  Find the acceleration of the center of mass of the pendulum
        CALL SDACC(1,COMASS,ACC)
c  Transform the external force vector from SDUFORCE to inertial frame
        CALL SDTRANS(1,FVEC,0,FVEC)
c  Get the current values of the pendulum mass and gravity for F = ma eqn.
        CALL SDGETMASS(1,M1)
        CALL SDGETGRAV(GRAV)
        WRITE (6,*)
        WRITE (6,*) 'Set torsion spring bias to zero'
        WRITE (6,*) 'Compute external forces, accelerations, and position'
        WRITE (6,*) 'at the time of release:'
        WRITE (6,*) 'Force = '
        WRITE (6,1000) FORCE
        WRITE (6,*) 'Acceleration = '
        WRITE (6,1000) ACC
        WRITE (6,*)
        WRITE (6,*) 
     +'Check if sum of external forces equals mass*acceleration:'
        WRITE (6,*) '(along three inertial unit vectors, n1, n2, and n3)'
        WRITE (6,*) '                F        =     m       a       ?'
        WRITE (6,*) ' n1: ',FORCE(1,1)+M1*GRAV(1)+FVEC(1), 
     +' = ',M1*ACC(1),' ?'
        WRITE (6,*) ' n2: ',FORCE(1,2)+M1*GRAV(2)+FVEC(2), 
     +' = ',M1*ACC(2),' ?'
        WRITE (6,*) ' n3: ',FORCE(1,3)+M1*GRAV(3)+FVEC(3), 
     +' = ',M1*ACC(3),' ?'
        PAUSE 'DONE WITH EXERCISE #4'

c  EXERCISE #5: MOTION ANALYSIS OF RELEASED PENDULUM

c  Set DT = 0.05 to allow motion to propagate for a while
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 5: Motion Analysis After Release'
        WRITE (6,*) ' '
        WRITE (6,*) 
     +'      Time (sec)             Angle(rad)           Rate(rad/sec)'
        DT = 0.05D0
        NSTEP = 200
        DO 20 ILOOP = 1, NSTEP
          CALL SDMOTION(T,Y,DY,DT,CTOL,TOL,FLAG,ERR)
          WRITE (6,1000) T,Y
20        CONTINUE
        PAUSE 'DONE WITH EXERCISE #5'

c  EXERCISE #6: STATIC ANALYSIS OF FINAL RESTING POSITION

c  Redo static analysis for spring bias set to 0.
c  Long term motion of damped pendulum should lead to hanging vertically.
c  Note that all velocities, Y(2), must be set to zero.
        Y(2) = 0.0D0
        CALL SDSTATIC(T,Y,LOCK,CTOL,TOL,MAXEVALS,FCNT,ERR)
        WRITE (6,*) ' '
        WRITE (6,*) 'Exercise 6: Static equilibrium for bias = 0 '
        WRITE (6,*) ' '
        WRITE (6,*) ' Y = ',Y(1),'   FCNT = ',FCNT,'      ERR = ',ERR
        PAUSE 'DONE WITH EXERCISE #6'

c  END OF EXERCISES

c  ALWAYS include a call to SDPRINTERR() at the end of your program
c  to report any problems found by SD/FAST.
        CALL SDPRINTERR(6)
        STOP
        END

c  SDUFORCE must be written for use with SDMOTION or SDSTATIC

        SUBROUTINE SDUFORCE(T,Q,U)

c  Set up variables, common block to PEND, and all known parameters.

        REAL*8        T,Q(1),U(1),FFRICT,KROTATE,TAU,BIAS,FREELENGTH,KLINEAR
        REAL*8        PNTP(3),VVEC(3),FVEC(3),STRETCH,MAG,POSP(3),GNDPT(3)

        COMMON / USER1 / BIAS, FVEC

        DATA PNTP /0.0D0, 0.5D0, 0.0D0/
        DATA GNDPT /-1.0D0, -1.0D0, 0.0D0/
        FFRICT = 10.0D0
        KROTATE = 100.0D0
        KLINEAR = 10.0D0
        FREELENGTH = 1.0D0

c  Compute spring and damper hinge torque TAU and apply to axis 1 of joint 1.
        TAU = -FFRICT*U(1) - KROTATE*(Q(1)-BIAS)
        CALL SDHINGET(1,1,TAU)

c  Compute inertial location of linear spring attach point pntp, put into posp
        CALL SDPOS(1,PNTP,POSP)
c  Find a unit vector in the direction of the spring force
        VVEC(1) = GNDPT(1) - POSP(1)
        VVEC(2) = GNDPT(2) - POSP(2)
        VVEC(3) = GNDPT(3) - POSP(3)
        MAG = SQRT( VVEC(1)**2 + VVEC(2)**2 + VVEC(3)**2 )
        IF ( MAG .GT. 1.0D-15 ) THEN
          VVEC(1) = VVEC(1) / MAG
          VVEC(2) = VVEC(2) / MAG
          VVEC(3) = VVEC(3) / MAG
        ELSE
          WRITE(6,*) ' Error in SDUFORCE: Zero length unit vector'
          STOP
        END IF
c  Find the stretch of the spring beyond its free length
        STRETCH = MAG - FREELENGTH
c  Find the resultant components of the linear spring force
        FVEC(1) = KLINEAR * STRETCH * VVEC(1)
        FVEC(2) = KLINEAR * STRETCH * VVEC(2)
        FVEC(3) = KLINEAR * STRETCH * VVEC(3)
c  Transform the spring force back to the pendulum frame
        CALL SDTRANS(0,FVEC,1,FVEC)
c  Apply the spring force as a point force acting at point P, PNTP, on pendulum.
        CALL SDPOINTF(1,PNTP,FVEC)

        RETURN
        END
