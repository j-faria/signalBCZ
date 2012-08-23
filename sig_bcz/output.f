!--------------------------------------------------------------------
!--------------------------------------------------------------------
!	joao faria: 20/08/2012 
!		should remove INTYPE as an argument, as it is contained 
!		in the module COMMONVAR (although it is not used here)
!--------------------------------------------------------------------
	subroutine output (intype,afile,c,amess)
!	 write the OUTPUT

		use commonvar
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		
		character(len=80) :: afile
		character(len=1)  :: amess
		
		real              :: taud
		
		integer           :: nfile

		parameter (ncp=20)
		dimension c(ncp)

		taud = c(1) / (w0*fac)
		write (6,*) "  Frequencies from file: ", afile
	
		write (6,1010) 'Results:', 'Tau_d = ', taud, &
                               'Phi_d = ', c(2), &
                               'A_d = ', c(3)
 1010	format (3x, a, //, 6x, a, f9.4, 6x, a, f8.5, //, 6x, a, f10.8, /)


		nfile=length(afile)

	! output to "res" file -
	
!		if (intype.eq.0) then
!			write (9,1015) afile(nfile-2:nfile),taud,c(2),
!     *                    c(3),amess
! 1015	   format (a4,'  ',f9.4,'  ',f7.5,'  ',f10.8,
!     *             ' ',a1)

!	else if (intype.eq.1) then
!	   write (9,1016) afile(56:60),taud,c(2),
!     *                    c(3),valtype,amess
! 1016	   format (a4,'  ',f9.4,'  ',f7.5,'  ',f10.8,
!     *             '  ',f6.3,' ',a1)

!	else if (intype.eq.2) then
!	   fw=(w0-wlower)/(wupper-wlower)
!	   write (9,1017) itermod,taud,c(2),
!     *                    c(3),xmass,w0,fw,amess
! 1017	   format (i3,'  ',f8.3,'  ',f6.4,'  ',f8.6,
!     *             ' ',f5.3,'  ',
!     *             f6.1,'  ',f4.2,' ',a1)

!	else 
!	   continue
!	endif

        return
        
	end subroutine output
