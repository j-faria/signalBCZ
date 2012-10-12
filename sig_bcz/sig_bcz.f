!****************************************************************************
! Mario Monteiro: Feb 2011
! Last changed: Feb 2011
!****************************************************************************
	subroutine sig_bcz
! In this case we have 3 constants C(NCONST) to fit the expression of
! the signal in the frequencies - using only very low degree data.
! The constants are determined as the values that give the minimum of the
! residuals.
! The selection of the data and the numerical parameters of the fit are
! read form the file "SIG_BCZ_INPUT".

	use commonvar
	
	implicit double precision (b-h,o-z) ! a comment
	implicit integer (i-n)
	
	character (len=80)   :: afile
	character (len=1)    :: amess
	character (len=80)   :: afilepar
	
	integer, parameter   :: ncp = 20
	!integer              :: nconst

	real                 :: varlim, var
	!real                 :: xinitm
	double precision, dimension(ncp) :: c
	!dimension c(ncp)
	

	afile='00000'
	write (6,'(/, a, //)')"---------------> PROGRAM SIG_BCZ <---------------"
	
!+++++++++++++++++++++++++++++++++++++++++++
!--- Number of parameters to fit -
	nconst=3
	
!--- File with input parameters -
 	afilepar='sig_bcz_in.dat'
 	call parameters (afilepar)
 	
!--- Initializing all quantities and output files -
	call deffreq (intype,afile,c)
	call init (afile)
	xinitm = xinit
	write(*,*) ' '
	write(*,'("   Smoothing parameter = " d8.2,/)') xinitm 
	call openfiles (afile,xinitm)
	call flush (6)
	varlim = 0.2d0
	
!--- Finding the best parameters -
	amess = ' '
	call fitlamb (xinitm,c,res,amess)	
	
!--- Writing the results -
	! variation in tau0 relative to initial value
	var = abs(((c(1)/(w0*fac))-tau0)/tau0)
	if (var .gt. varlim .and. amess(1:1) .eq. ' ') then
	   amess='.'
	   write (*,*)"  ==> WARNING: Value of taud not admissible! [.]"
	endif
	
	call output (afile,c,amess)

	if (iprint.ge.1) close (3)
	write (6,*)"---------------> PROGRAM SIG_BCZ <---------------"
	call flush (6)
	call flush (9)

	end subroutine sig_bcz
