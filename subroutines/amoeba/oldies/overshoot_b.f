c****************************************************************************
	subroutine splsmo (xw,yw,n,xlamb)
c Here, a polynomial fit of degree (N-1), to the N points (XW,YW) is done.
c A normalization of both XW and YW to the interval [0,1] is used to reduce
c numerical errors.
c The construction of the polynomial uses a smoothing parameter XLAMB which
c is associated to the third derivative. The smoothed fit to the N points
c is returned in YW.
c
	implicit double precision (b-h,o-z)
	parameter (lval=50,exlim=-200.0d0)
	dimension y(lval),xw(lval),yw(lval),d(lval,lval),c(lval)
c
c This assumes that xw and yw are ordered in the correct way!!!
c Normalization:
	xmax=xw(n)
	xmin=xw(1)
	ymax=yw(n)
	ymin=yw(1)
	xa=2.0d0/(xmax-xmin)
	xb=-(xmax+xmin)/(xmax-xmin)
	ya=2.0d0/(ymax-ymin)
	yb=-(ymax+ymin)/(ymax-ymin)
	do 10 j=1,n
		xw(j)=xa*xw(j)+xb
		yw(j)=ya*yw(j)+yb
 10		continue
c
	do 250 j=1,n
		xj=dfloat(j)
		do 260 k=j,n
			fac=0.0d0
			xk=dble(k)
			fac1=0.0d0
			fac2=0.0d0
			do 270 i=1,n
				if (xw(i).eq.0.0d0) then
					faci1=0.0d0
					faci2=0.0d0
					if (k+j.eq.2) faci1=1.0d0
					if (k+j.eq.8) faci2=1.0d0
					goto 240
c				else
c					xx=abs(xw(i))
c					elwx1=dble(k+j-2)*log(xx)
c					elwx2=dble(k+j-8)*log(xx)
				endif
c				if (elwx1.lt.exlim) then
c					faci1=0.0d0
c				else
					faci1=xw(i)**(k+j-2)
c				endif
c				if (((j+k).lt.8).or.(elwx2.lt.exlim)) then
c					faci2=0.0d0
c				else
					faci2=xw(i)**(k+j-8)
c				endif
 240				fac1=fac1+faci1
				fac2=fac2+faci2
 270				continue
			fac=fac1+fac2*xlamb*(xk-1.0d0)*(xk-2.0d0)*(xk-3.0d0)*
     *				(xj-1.0d0)*(xj-2.0d0)*(xj-3.0d0)
			d(k,j)=fac
			d(j,k)=fac
 260			continue
		y(j)=0.0d0
		do 280 i=1,n
			if (j.eq.1) then
				sum=yw(i)
			else
				sum=yw(i)*xw(i)**(j-1)
			endif
			y(j)=y(j)+sum
 280			continue
 250		continue
c
	call gauss (n,d,y,c)
c
	do 300 i=1,n
		y(i)=c(n)
		do 310 k=n-1,1,-1
			y(i)=y(i)*xw(i)+c(k)
 310			continue
		yw(i)=(y(i)-yb)/ya
 300		continue
c
	return
	end
c****************************************************************************
	subroutine gauss (n,y,f,c)
c Giving y and f, the system y.c=f is solved being n the dimension:
c y(n,n), c(n) and f(n). It is used the gauss elimination method.
c The Gauss elimination method with partial pivoting is used to solve
c the system.
c
	implicit double precision (b-h,o-z)
	parameter (lval=50)
	dimension y(lval,lval),f(lval),c(lval)
c
	do 10 k=1,n-1
		imax=k
		ymax=abs(y(k,k))
		do 5 i=k+1,n
			if (abs(y(i,k)).gt.ymax) then
				imax=i
				ymax=abs(y(i,k))
			endif
 5			continue
		if (imax.gt.k) then
			do 6 i=k,n
				xx=y(imax,i)
				y(imax,i)=y(k,i)
				y(k,i)=xx
 6				continue
			xx=f(imax)
			f(imax)=f(k)
			f(k)=xx
		endif
c
		do 20 i=k+1,n
			if (y(i,k).eq.0.0d0) goto 20
			b=-y(i,k)/y(k,k)
			do 30 j=k+1,n
 30				y(i,j)=y(i,j)+b*y(k,j)
 			f(i)=f(i)+b*f(k)
 20			continue
 10		continue
c
	c(n)=f(n)/y(n,n)
	do 40 i=n-1,1,-1
		b=0.d0
		do 50 j=i+1,n
			b=b+y(i,j)*c(j)
 50			continue
		c(i)=(f(i)-b)/y(i,i)
 40		continue
c
	return
	end
c****************************************************************************
