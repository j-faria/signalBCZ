! Author        : Joao Faria
! Date          : Oct 2012
! Last changed  : 

subroutine minimize( n, p, x, resid, resd )
!**********************************************************
! This subroutine tries to determine the global minimum of
! the function RESID by using the NL2SOL algorithm [1]. 
! RESID returns a residual function R(X) = R1(X),...,Rn(X) 
! of P parameters X = X1,...,XP. The subroutine attempts to
! find a parameter vector X* that minimizes the sum of 
! squares function F(X) = sum(i,1,N) Ri(X)^2 
!
!   [input, integer] N: the number of components in the
!                    residual vector R
!   [input, integer] P: the number of parameters on which 
!                    R(X) depends
!   [i/o,real,len=P] X: on input is an initial guess at the
!                    desired solutiion X*. When returning,
!                    X contains the best solution found
!   [i/o, external ] RESID: residual function that returns
!                    R(X), declared as external
!   [out,real,len=N] RESD: on output contains the value of
!                    the sum of squares function
!**********************************************************
	implicit none
	
	integer, intent(in) :: n
	integer, intent(in) :: p
	real, intent(inout) :: x(:)
	real, intent(inout) :: resd
	
	real, dimension(p)  :: x0
	
	! NL2SOL control variables:
	integer :: uiparm(1)
	real    :: urparm(1)
	integer, dimension(60+p)                    :: iv
	real, dimension( 93+n*(p+3)+p*(3*p+33)/2 )  :: v
		
    external resid
    external ufparm     

    ! dimension check
!    write(*,*) size(x)
!    write(*,*) p
    if( size(x).ne.p ) then
        write(*,*) 'Wrong dimensions in MINIMIZE(): size(X) must equal P.'
        return
    endif

    ! initial solution estimate
    x0 = x
    ! set to 0 to provide default values to IV and V
    iv(1) = 12
	
	call nl2sno ( n, p, x, resid, iv, v, uiparm, urparm, ufparm )
	
	resd = v(10)
	
	return
end subroutine minimize




subroutine ufparm ( meqn, nvar, x )
!**********************************************************
! Dummy routine to be passed by NL2SOL to the residual and
! jacobian routines. Currently it is not used.
!**********************************************************
    implicit none

    integer meqn
    integer nvar
    real x(nvar)

    return
end subroutine ufparm
