!--------------------------------------------------------------------
  subroutine fitlamb (xinitm,c,resd,amess)
!	 this subroutine iterates in XLAMB from the inital 
!	 value XINIT until the value of XLAMB is smaller than 
!	 FTOL, with maximum iterations allowed being IMAX. For
!	 each value of XLAMB it also iterates until the 
! 	 relative variation of the residuals is smaller than 
!	 TOLFIT. The final value of the parameters is returned
!	 in C and the residuals in RESD

		use commonvar, only : nconst, iprint, iterinit,iterfit,ftol,tolfit
		use commonarray, only : n
		use interfaces
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		
		character(len=1)                 :: aerror
		character(len=*), intent(inout)  :: amess

		
		integer, parameter               :: ncp = 20
		integer, parameter               :: imax = 5000
		integer, parameter               :: npt = 2000
		real, parameter                  :: tiny = 1.0d0
		
		integer                          :: iamoeba	! forced jumps in AMOEBA
		integer                          :: iter	! iteration number

		real, dimension(ncp)             :: c0
		real(kind=8), intent(inout)  :: c(*)
		real, dimension(nconst)  :: xin
		real(kind=8)            :: resd
		
		external resid


		if (iprint.ge.1) call writeout (2,c)
		iamoeba=0
		iter=0
		iterloc=0
		resd0=0.0d0
		
		do il = 1,iterinit
			! reduce the smoothing condition by a factor of 2 -
			xlamb = xinitm/2.0d0**(il-1)

 10			iter = iter+1
			iterloc = iterloc+1
			
			do i=1,nconst
				c0(i) = c(i)
			end do

			! subtract the signal (given by FUN, with current values of C)
			! and then smooth again with current XLAMB -
			call sec (xlamb,c)
			
			!** PRINT **
			if (iprint.eq.5) then
				call writeout (3,c)
				stop
			endif
			!***********

			! find the parameters that minimize the residuals
			call fit (c,resd,ierrorflag)
!            xin = real(c(1:nconst))
!            call minimize(n, nconst, xin, resid, resd)
!            ierrorflag = 0


			!** PRINT ******
			if (iprint.ge.1) then
				call writeout (2,c)
				if (iprint.ge.4) call writeout (3,c)
			endif
			!***************
			
			rtol = 0.0d0

			do j=1,nconst
				rtol = rtol+abs((c(j)-c0(j)))/max(tiny,abs(c(j)+c0(j)))
			end do

			!** PRINT ******
			if (iprint.ge.2) then
				! if error in AMOEBA -
				if (ierrorflag.eq.1) then
					iamoeba = iamoeba+1
					aerror='!'	! print "!" in current step
				else
					aerror=' '
				endif
			
				write (6,'( i4, x, a, f10.2, 2f15.8, 3x, a1)' ), &
				              iter, "->", xlamb*1.0d7, rtol, resd, aerror
				call flush (6)
			endif
			!***************

			if (abs((resd-resd0)/(resd+resd0)).lt.1.0d-12) then
				rtol=ftol
				write (*,*) ' ==> WARNING: Emergency exit in FITLAMB !'
				write (*,*) ' '
			endif
	   
			if (rtol.gt.ftol) then
				! no convergence in IMAX iterations -
				if (iterloc.gt.imax) then
					write (6,'( a,a,/ )') "  ==> WARNING: No convergency for ", &
                           "max. iter. in FITLAMB !"
					amess='!'
					if (iprint.ge.3) call writeout (1,c)
					return
				endif
				! if we haven't done IMAX iterations yet, and RTOL is
				! still bigger than FTOL, continue -
				resd0 = resd
				goto 10
			endif
		
			iterloc=0

			if (iprint.ge.1) write (3,*) ' '
		end do


		if (iprint.ge.3) call writeout (1,c)

		if (iprint.ge.1) write (6,*) ' '

		write (6,*) "  Number of iterations done: ", iter

		if (iamoeba.gt.0) write (6,*) "Forced jumps in AMOEBA", &
                                          iamoeba

		return
	
  end subroutine fitlamb
