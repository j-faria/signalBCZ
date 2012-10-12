!--------------------------------------------------------------------
	double precision function funk(c)
!	 the function to be minimized is defined as the residuals for
!	 a fit in a least squares sense

		use commonvar, only : nconst, include_errors
		! contains npt, n, l, sd, sig, xn, w -
		use commonarray
		
	 	implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (ncp=20)
		dimension c(ncp)

		
		resid = 0.0d0
		! if not using errors -
		if (include_errors == 'no' .or. include_errors == 'n') then
			do i=1,n
				ww = w(i)
				ll = l(i)
				sf = fun(c,ww,ll)
				resid = resid + (sd(i)-sf)**2
			end do
		! if using errors -
		else if (include_errors == 'yes' .or. include_errors == 'y') then
			do i=1,n
				ww = w(i)
				ll = l(i)
				sf = fun(c,ww,ll)
				resid = resid + ((sd(i)-sf)/sig(i))**2
			end do

		endif
        
		funk = resid

		return
	
	end function funk
