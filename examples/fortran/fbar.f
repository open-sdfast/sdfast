       real*8 state(6),dstate(6),t,dt,ctol,tol,frc(4,3),trq(4,3)
       integer lock(3),fcnt,err,i,flag
       data dt,ctol,tol/0.05d0,1d-5,1d-6/
       data lock,flag/1,0,0,1/

c Set initial conditions to a starting guess and initialize sd model.
       data t,state/0d0,0d0,0d0,0d0,10d0,1d0,-1d0/
       call sdinit
       call sdstab(2d0,1d0)

c Do assembly and velocity analysis.  Only q(1) and u(1) are locked.
       call sdassemble(t,state,lock,1d-7,1000,fcnt,err)
       call sdinitvel(t,state,lock,1d-7,1000,fcnt,err)
       print 5, state
    5  format(' ',6f12.5)

c Do motion analysis. Constraints maintained to `ctol'; integration to `tol'.
       do 10 i=1,200
           call sdmotion(t,state,dstate,dt,ctol,tol,flag,err)
           call sdreac(frc,trq)
           print 5, t,state(1),state(4),frc(1,1),frc(1,2)
   10  continue
       call sdprinterr(6)
       end

c Apply viscous damping torque to the crank joint.
       subroutine sduforce(t,q,u)
       double precision t,q(3),u(3)
       call sdhinget(1,1,-3d0*u(1))
       return
       end
