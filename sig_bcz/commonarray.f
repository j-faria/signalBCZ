!--------------------------------------------------------------------
!	Joao Faria: 21/08/2012 
!--------------------------------------------------------------------
!	 Module that contains the common arrays that some subroutines
!	 need to share. 

module commonarray
	
	implicit none
	
	integer, parameter, public       :: npt = 2000
	integer, public                  :: n
	
	integer, dimension(npt), public  :: l
	
	real, dimension(npt), public     :: sd
	real, dimension(npt), public     :: sig
	real, dimension(npt), public     :: xn
	real, dimension(npt), public     :: w

end module commonarray
