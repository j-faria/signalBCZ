!--------------------------------------------------------------------
	subroutine skpcom (nunit)
!	 checks if there is a line starting by "#" in file NUNIT file 
!	 and skips it

		implicit integer (i-n)
		character (len=1)  :: ain
	
		ain = '#'
		do while (ain.eq.'#')
			read (nunit,'(a1)',end=2) ain
		enddo

		backspace (nunit)

 2		return

	end subroutine skpcom
