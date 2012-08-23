c***************************************************************************
	subroutine skpcom (nunit)
c This subroutine checks if there is a line starting by "#" and
c if there is it skips it.
c
	implicit integer (i-n)
	character ain*1
c
 1	read (nunit,1000,end=2) ain
	if (ain.eq.'#') goto 1
	backspace (nunit)
 2	return
c
 1000	format (a1)
	end
c***************************************************************************
