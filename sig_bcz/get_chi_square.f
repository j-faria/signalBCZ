!**********************************************************
! 
!
!
!**********************************************************
  subroutine get_chi_square (chi2, chi2norm, c)
   
    ! contains npt, n, l, sd, sig, xn, w -
    use commonarray
    use commonvar
    
    implicit double precision (b-h,o-z)
    
    real, intent(inout)         :: chi2, chi2norm
    real                        :: chi2term, chi2sum
    real(kind=8)                :: fit, ww
    real                        :: signal_err
    integer                     :: i, chi2N
    integer                     :: ll
    integer, parameter          :: ncp = 20
    dimension c(ncp)
    
    chi2sum = 0.0
    chi2N = 0
    
    do i=1,n
        ! the smooth component must be removed from the sum of
        ! frequency+error in each point
        ww = w(i)
        ll = l(i)
        signal_err = (w0*ww + sig(i)) - (w0*ww - sd(i))
        fit = fun(c,ww,ll)
        chi2term = ((sd(i) - sngl(fit)) / signal_err)**2
	    chi2sum = chi2sum + chi2term
	    chi2N = chi2N + 1
	enddo
	
	chi2 = chi2sum
	! normalized chi^2:
	chi2norm = chi2sum / (chi2N - nconst)
	
	return
	
  end subroutine
