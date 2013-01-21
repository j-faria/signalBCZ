!--------------------------------------------------------------------
!--------------------------------------------------------------------
!	joao faria: 20/08/2012 
!		should remove INTYPE as an argument, as it is contained 
!		in the module COMMONVAR (although it is not used here)
!--------------------------------------------------------------------
	subroutine output (afile,c,amess)
!	 write the OUTPUT

		use commonvar
		
		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		
		character(len=80) :: afile
		character(len=1)  :: amess
		
		real            :: taud
		real            :: chi2, chi2norm
		
		integer     :: nfile
		parameter (ncp=20)
		dimension c(ncp)


		taud = c(1) / (w0*fac)
		call get_chi_square(chi2, chi2norm, c)
		
		write (6,*) "  Frequencies from file: ", afile
	
		write (6,1010) 'Results:', &
		                   'Tau_d = ', taud, 'Phi_d = ', c(2), &
                           'A_d = ', c(3) / 1.d6, & ! need to check why we do this conversion!!!!!!
                           'chi2 = ', chi2, 'chi2norm = ', chi2norm
        write(6,*) c(3)
 1010	format (3x, a, //, &
                6x, a, f9.4, 6x, a, f8.5, //, &
                6x, a, f10.8, //, &
                6x, a, f15.3, 5x, a, f15.3, /)


		nfile=length(afile)

		! output to "res" file -
		if (intype.eq.0) then
			write (9,9003) afile(nfile-6:nfile-4), taud, c(2), c(3), amess
 9003		format (3x, a, x, f9.4, 2x, f7.5, 2x, f10.8, 2x, a1)
		endif

        return
        
	end subroutine output
