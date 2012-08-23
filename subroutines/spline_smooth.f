!--------------------------------------------------------------------
	subroutine spline_smooth (lval,xw,yw,n,xlamb)
!	 the N points (XW,YW) are interpolated by a smoothing
!	 spline of degree k(=3; cubic spline). 
!	 A normalization of both XW and YW to the interval [0,1]
!	 is done to reduce numerical errors.
!
!	 The smoothed fit to the N points is returned in YW.
!	 A smoothing parameter S is used:
!	    if s is too large, the curve will be too smooth and 
!		signal will be lost ; 
!	    if s is too small the curve will pick up too much noise.
!	 in the extreme cases the program will return an interpolating
!	 curve if s=0 and the least-squares polynomial curve of degree
!	 k if s is very large.


		use gnufor2

		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (loc=100,exlim=-200.0d0)
		dimension xw(lval), yw(lval), sp(lval)
		dimension w(lval), c(100), cp(24), wrk(1400), xx(lval)
		dimension db(3), de(3)
		dimension y(loc),d(loc,loc)


		!Normalization:
		!This assumes that xw,yw are ordered in the correct way!!
		xmax=xw(n)
		xmin=xw(1)
		ymax=yw(n)
		ymin=yw(1)
		xa=2.0d0/(xmax-xmin)
		xb=-(xmax+xmin)/(xmax-xmin)
		ya=2.0d0/(ymax-ymin)
		yb=-(ymax+ymin)/(ymax-ymin)
	
		do j=1,n
			xw(j)=xa*xw(j)+xb
			yw(j)=ya*yw(j)+yb
		enddo



		!The weights are taken as 1 for now:
		ww = 1.0d0
		do i=1,n
			w(i) = ww
			y(i) = 0.0d0
		enddo


		! the smoothing factor is chosen
		s = 10000.d0

		! we are interpolating a curve
		idim = 1

		! begin point derivatives of the curve
		db(1) = 0.0
		db(2) = 0.0
		db(3) = 0.0
		! end point derivatives of the curve
		de(1) = 0.0
		de(2) = 0.0
		de(3) = 0.0
		! we set up the dimension information.
		np = 24
		nb = 3
		ne = 3
		nest = 50
		lwrk = 1400
		nc = 100
		mx = lval
		! for the first approximations we will use cubic splines.
		k = 3

		! no derivative constraints
		iopt = 0
		ib = 0
		ie = 0

		! determination of the spline curve
		call concur(iopt,idim,n,yw,mx,xw,xx,w,ib,db,nb,ie,de,ne,k,s, &
                   nest,n,t,nc,c,np,cp,fp,wrk,lwrk,iwrk,ier)


		! evaluate the spline curve
		call curev(idim,t,n,c,nc,k,xw,n,mx,sp,ier)

		do i=1,n
			y(i) = sp(i)
			!"Un"-normalization:
			yw(i)=(y(i)-yb)/ya
			xw(i)=(xw(i)-xb)/xa
		enddo

	
!	call plot(xw,yw,' 5.')
!	print *,'press ENTER to go to the next example'
!	read  *

		return
	end subroutine spline_smooth
