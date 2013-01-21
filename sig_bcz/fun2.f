!**********************************************************
  real function fun2 (c, w, l)
!	 this is the function to be fitted. It is the signal
!	 produced by the sharp transition in the base of the
!	 convection zone
!
!	 see Monteiro et al. (1994), eq (20)

		implicit none
	
		real, intent(in)  :: c(*)
		real, intent(in)  :: w
		integer, intent(in)           :: l

		real :: xarg

		
		xarg = 2.0d0 * ( c(1)*w + c(2) )
	  	fun2  = ( c(3)/w**2 ) * sin(xarg)

		return
		
  end function fun2

