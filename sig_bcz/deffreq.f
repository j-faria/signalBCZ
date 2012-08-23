! Joao Faria
! Last changed: 19/08/2012 
!--------------------------------------------------------------------
	subroutine deffreq (intype,afile,c)
!	 Define the reference values of the parameters C(NCP)

		use commonvar
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		
		character(len=80)    :: afile
		character(len=80)    :: afile0
		
		! need to check how to define c when it is an argument 
		! of the function
		
		integer, parameter   :: ncp = 20
		!real, dimension(ncp) :: c
		dimension c(ncp)

		w0 = w0ref
		tau0 = tau0ref
		
		! first 2 parameters
		c(1)=tau0*w0*fac	! argument
		c(2)=phi0ref		! phase

		xamp0=xamp0ref
		! third parameter
		c(3)=xamp0		! amplitude

		! output to terminal -
		!write(*,*) c(1), c(2), c(3)
		
		if (afile(1:5).eq.'00000') then
			afile0='freqs'
		else
			afile0='stop'
		endif
		
		write(*,*) ' '
		
		if (include_errors == 'yes' .or. include_errors == 'y') then
			write (*,*) " name of input file (l,n,v,sigma)  --> "
		else if (include_errors == 'no' .or. include_errors == 'n') then
			write (*,*) " name of input file (l,n,v)  --> "
		endif

		read (*,'(a80)') afile

		write (*,*) ' '

		if (afile(1:1).eq.' ') afile = afile0
		if (afile(1:4).eq.'stop') stop

		write (*,*) "  Reading frequencies from File: ", afile

		return
	end
