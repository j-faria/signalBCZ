subroutine resid ( meqn, nvar, x, nf, r, uiparm, urparm, ufparm )
!    Input, integer MEQN, the number of functions.
!
!    Input, integer NVAR, the number of variables.
!
!    Input, real X(NVAR), the current value of the variables.
!
!    Input, integer NF, the number of times the residual routine
!    has been called so far.
!
!    Output, real R(MEQN), the residual vector, that is, the
!    value of the functions for the given input value of the variables.
!
!    Input, integer UIPARM(*), a user array.
!
!    Input, real URPARM(*), a user array.
!
!    Input, external UFPARM, an external reference to a user subroutine
!    or function.
!
    use commonvar, only : nconst, include_errors
    ! contains npt, n, l, sd, sig, xn, w:
    use commonarray
    
    implicit none

    integer, intent(in) :: meqn
    integer, intent(in) :: nvar
    integer, intent(in) :: nf
    
    real, intent(in), dimension(nvar)       :: x
    real, intent(inout), dimension(meqn)    :: r

    integer :: i, ll
    real    :: ww
    real    :: sf

    real :: fun2
    external ufparm
    integer uiparm(*)
    real urparm(*)
		
            
    ! if not using errors -
    if (include_errors == 'no' .or. include_errors == 'n') then
	    do i=1,n
		    ww = w(i)
		    ll = l(i)
		    sf = fun2(r,ww,ll)
		    r(i) = sd(i)-sf
	    end do
    ! if using errors -
    else if (include_errors == 'yes' .or. include_errors == 'y') then
	    do i=1,n
		    ww = w(i)
		    ll = l(i)
		    sf = fun2(r,ww,ll)
		    r(i) = (sd(i)-sf)/sig(i)
	    end do
    endif

    return
    
end subroutine resid 
