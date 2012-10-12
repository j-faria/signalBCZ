!****************************************************************************
	subroutine amoeba (nconst,p,y,ftol,ierrorflag)
! This is a subroutine from NUMERICAL RECIPES which determines the 
! minimum of the function FUNK of nconst variables. 
! The point were the minimum ocurres (with tolerance FTOL) is returned
! in the matrix P (where the rows are the vertices of the simplex contain
! the solution within FTOL.
! For more details see page 292.
!
! This subroutine was modified from the original one regarding the criterion
! to stop the iteration. It was changed from the advance in the value of the
! function to the change in the parameters.
!
	implicit double precision (b-h,o-z)
	implicit integer (i-n)
	parameter (xalpha=1.0d0,beta=0.5d0,gamma=2.0d0)
	parameter (itmax=2000,ndim=20)
	dimension p(ndim,ndim),y(ndim)
	dimension pr(ndim),prr(ndim),pbar(ndim)
!
	mpts=nconst+1
	iter=0
 1	do 120 i=1,nconst
	   sum=0.0d0
	   do 130 j=1,mpts
	      sum=sum+p(j,i)
 130	   continue
	   pbar(i)=sum/dble(mpts)
 120	continue
	rtol=0.0d0
	do 150 k=1,nconst
	   dis=abs(p(mpts,k)-p(nconst,k))
	   do 160 i=1,nconst-1
	      do 170 j=i+1,mpts
		 disp=abs(p(j,k)-p(i,k))
		 if (disp.gt.dis) dis=disp
 170	      continue
 160	   continue
	   rtol=rtol+dis/abs(pbar(k))
 150	continue
!
	iho=1
	if (y(1).gt.y(2)) then
	   ihi=1
	   inhi=2
	else
	   ihi=2
	   inhi=1
	endif
	do 11 i=1,mpts
	   if (y(i).lt.y(iho)) iho=i
	   if (y(i).gt.y(ihi)) then
	      inhi=ihi
	      ihi=i
	   else if (y(i).gt.y(inhi)) then
	      if (i.ne.ihi) inhi=i
	   endif
 11	continue
!
!	rtol=2.0d0*abs(y(ihi)-y(iho))/(abs(y(ihi))+abs(y(iho)))
	if (rtol.lt.ftol) return
	if (iter.eq.itmax) then
!          write (*,*) 'ERROR: To many iter. in AMOEBA !'
!          write (*,*) '       Tolerance reduced to ',fotl*10.0d0
	   ierrorflag=1
	   ftol=ftol*10.0d0
	   iter=0
	endif
	iter=iter+1
	do 12 j=1,nconst
	   pbar(j)=0.0d0
 12	continue
	do 14 i=1,mpts
	   if (i.ne.ihi) then
	      do 13 j=1,nconst
		 pbar(j)=pbar(j)+p(i,j)
 13	      continue
	   endif
 14	continue
	do 15 j=1,nconst
	   pbar(j)=pbar(j)/dble(nconst)
	   pr(j)=(1.0d0+xalpha)*pbar(j)-xalpha*p(ihi,j)
 15	continue
	ypr=funk(pr)
	if (ypr.le.y(iho)) then
	   do 16 j=1,nconst
	      prr(j)=gamma*pr(j)+(1.0d0-gamma)*pbar(j)
 16	   continue
	   yprr=funk(prr)
	   if (yprr.lt.y(iho)) then
	      do 17 j=1,nconst
		 p(ihi,j)=prr(j)
 17	      continue
	      y(ihi)=yprr
	   else
	      do 18 j=1,nconst
		 p(ihi,j)=pr(j)
 18	      continue
	      y(ihi)=ypr
	   endif
	else if (ypr.ge.y(inhi)) then
	   if (ypr.lt.y(ihi)) then
	      do 19 j=1,nconst
		 p(ihi,j)=pr(j)
 19	      continue
	      y(ihi)=ypr
	   endif
	   do 21 j=1,nconst
	      prr(j)=beta*p(ihi,j)+(1.0d0-beta)*pbar(j)
 21	   continue
	   yprr=funk(prr)
	   if (yprr.lt.y(ihi)) then
	      do 22 j=1,nconst
		 p(ihi,j)=prr(j)
 22	      continue
	      y(ihi)=yprr
	   else
	      do 24 i=1,mpts
		 if (i.ne.iho) then
		    do 23 j=1,nconst
		       pr(j)=0.5d0*(p(i,j)+p(iho,j))
		       p(i,j)=pr(j)
 23		    continue
		    y(i)=funk(pr)
		 endif
 24	      continue
	   endif
	else
	   do 25 j=1,nconst
	      p(ihi,j)=pr(j)
 25	   continue
	   y(ihi)=ypr
	endif
	goto 1
!
	end
!**********************************************************************
