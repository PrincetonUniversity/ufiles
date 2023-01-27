	subroutine jc_day(weekday,day)	                           !12/07/84
	character*(*) weekday,day
	character*9 today,days(7),cdate
	data days /'Sunday','Monday','Tuesday',
     >		'Wednesday','Thursday','Friday','Saturday'/
c	call idate(im,id,iy)
c	write(today,'(i2.2,1h/,i2.2,1h/,i2.2)')im,id,iy
	call c9date(cdate)
	write(today,'(a9)') cdate
	day=today
	end
	subroutine jc_trnlog(ctabl,clogical,ctrans,nc,igood) 
	character ctabl*(*),clogical*(*),ctrans*(*)
	igood=usr_trnlog(ctabl,clogical,ctrans,nc,igood)  !   
	end
	subroutine jc_malloc(kbytes,jxx)
#ifdef __f90
	real, pointer :: jxx
#else
	real xx(*)
	pointer (jxx,xx)
#endif
	igood=lib_get_vm(kbytes,jxx)  !   
	if(jxx.eq.0) then  !   
	  call at_msg('**Cant get more memory')  !   
	  call ufgood_exit  !
	endif  !   
	end
	subroutine jc_mfree(kbytes,jxx)
#ifdef __f90
	real, pointer :: jxx
#else
	real xx(*)
	pointer (jxx,xx)
#endif
	if(kbytes.le.0) return
	call lib_free_vm(kbytes,jxx)  !   
	end
