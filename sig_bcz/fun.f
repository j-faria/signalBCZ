!**********************************************************
  double precision function fun (c, w, l)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone
!
!	 see Monteiro et al. (1994), eq (20)

		implicit none
	
		double precision, intent(in)  :: c(*)
		double precision, intent(in)  :: w
		integer, intent(in)           :: l

		double precision :: xarg

		
		xarg = 2.0d0 * ( c(1)*w + c(2) )
	  	fun  = ( c(3)/w**2 ) * sin(xarg)

		return
		
  end function fun

