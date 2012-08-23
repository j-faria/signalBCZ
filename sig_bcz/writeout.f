!--------------------------------------------------------------------
	subroutine writeout (iout,c)
!	 this subroutine writes out the output

		implicit double precision (b-h,o-z)
		implicit integer (i-n)
		parameter (ncp=20,npt=2000)
		dimension c(ncp),w(npt),l(npt),sd(npt),xn(npt),sig(npt)
		dimension temp(ncp,npt)
		common /blockpar00/nconst
		common /blockval01/n
		common /blockval02/sd
		common /blockval03/w
		common /blockval04/sig
		common /blockval05/l
		common /blockval06/xn
		common /blockconst1/fac
		common /blockconst2/w0,xl02

		! writing the final signal -
		if (iout.eq.1) then
			write (10,1240) c(1)/(w0*fac),(c(i),i=2,nconst)
 1240	   format ('# With parameters (taud,phi,Ad):',/, &
                   '#       ',f7.2,f8.5,f8.5,/,'#',/, &
                   '#   v        v-vs    l  n   sig     fit',/, &
                   '#------------------------------------------')

		do 220 i=1,n
			ww=w(i)
			ll=l(i)
			temp(1,i)=w0*ww
			temp(2,i)=sd(i)
			temp(3,i)=fun(c,ww,ll)
			temp(4,i)=dble(ll)
			temp(5,i)=xn(i)
			temp(6,i)=sig(i)
 220	   continue
c	   call ord (temp,5,n,1)
c	   call order (1,temp,5,n,ncp,npt)
	   do 221 i=1,n
	      write (10,1310) (temp(j,i),j=1,2),int(temp(4,i)),
     *                        int(temp(5,i)),temp(6,i),temp(3,i)
 221	   continue
 1310	   format (f9.3,f10.6,2i3,f7.3,f10.6)
	   close (10)
	endif
c--- Writing the parameters of this iteration ---
	if (iout.eq.2) then
	   write (3,1140) c(1)/(w0*fac),(c(i),i=2,nconst)
 1140	   format (f8.2,f10.5,'  ',4f9.5)
	   call flush (3)
	endif
c--- Writing the signal for this iteration ---
	if (iout.eq.3) then
	   open (8,file='qft',status='unknown')
	   do 230 i=1,n
	      ww=w(i)
	      ll=l(i)
	      write (8,1310) w0*ww,sd(i),ll,int(xn(i)),sig(i),fun(c,ww,ll)
 230	   continue
c 1150	   format (f10.3,2f12.5,i4,3d16.8,i5)
	   close (8)
	endif
c---
	return
	end
c****************************************************************************
