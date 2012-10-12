c***************************************************************************
	subroutine ord (xmod,m,n,mc)
c In this routine the model is ordered by increasing values of XMOD(MC,*)
c where N is the number of points and M the number of columns.
c
	implicit double precision (a-h,o-z)
	parameter (npoints=2000,ncol=20)
	dimension xmod(ncol,npoints),x1(ncol)
c
	do 100 i=2,n
		in=i-1
 10		if (xmod(mc,i).lt.xmod(mc,in)) then
 15			if (in.ge.2.and.xmod(mc,i).lt.xmod(mc,in-1)) then
				in=in-1
				goto 15
			endif
			do 25 j=1,m
				x1(j)=xmod(j,i)
 25				continue
			do 30 l=i,in+1,-1
				do 20 j=1,m
					xmod(j,l)=xmod(j,l-1)
 20					continue
 30				continue
			do 35 j=1,m
				xmod(j,in)=x1(j)
 35				continue
		endif
 100		continue
c
	return
	end
c***************************************************************************
