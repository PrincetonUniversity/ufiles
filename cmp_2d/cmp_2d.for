      program cmp_2d
c
c	Dimension Statements for the UFILES programs
c
      parameter (max_x=102400)
      parameter (max_y=100)
      real xarray(max_x,2), yarray(max_y,2), work(max_y,2)
      integer iwork(max_y,2), num_x(2), num_y(2),ishot(2)
      real zarray(max_x, max_y,2)
      character*30	flab(2), xlab(2), ylab(2)
      real	yscale(2), dt(2)
      real	y_grid(max_y), calib(max_y)
      real	res(max_x,max_y)
      character*30	rlab
 
      parameter (nscmax=50)
      real scval(nscmax,2)
      character*4 tdev(2)
      character*10 shdate(2), shtime(2)
      character*10 sclab(3,nscmax,2)
      integer nsc(2)
 
      character*10 uf_dev(2), uf_dir(2)
      character*6 uf_pre(2)
      character*3  uf_ext(2)
      character*30 ztitle
 
      logical lxout
      character*1 ans
      character*6   indicator
 
      logical done

      call ufsetr(9,'C','RCA','my_ufiles:',' ')
 
      call uinita(0)
 
      yscale(1) = 1.
      yscale(2) = 1.
 
      write(6,*) 'This program reads two 2d UFiles, and maps'
      write(6,*) 'the second dimension of each data file to be on the'
      write(6,*) 'same grid'
      write(6,*) ' '
      write(6,*)
     >  'It then allows you to look at the ratio (first/second) or '
      write(6,*) 'difference (first-second) between the files. '
      write(6,*) 'Scaling options are available on the input y-scale.'
      write(6,*) ' '
      write(6,*)
     >  'The result (ratio or diff) can be written to an output file'
      write(6,*) ' '
 
10    DO i=1,2
          IF (i.eq.1) THEN
      	indicator = 'first'
	
          ELSE
      	indicator = 'second'
      	if( ishot(2) .eq. 0) ishot(2 ) = ishot(1)
          END IF
 
 
11        call ureadi('Shot number for '//indicator//
     1		' data file (<=0 to stop)',ishot(i))
          if(ishot(i) .le. 0) stop
 
          call ufnter(10+i,'INPUT: '//indicator//' file')
      	    call ufoprd(10+i,ishot(i),ierr)
          if( ierr .eq. 0) then
      	call uf2drd( 10+i, tdev(i),shdate(i),zarray(1,1,i),max_x,
     1		xarray(1,i),max_x,yarray(1,i),max_y,
     1		iproc, num_x(i), num_y(i), 0, flab(i), xlab(i), ylab(i),
     2		nscmax, nsc(i), scval(1,i), sclab(1,1,i), ierr)
      		call ufclos(10+i)
          endif
          if( ierr .ne. 0) then
      	write(6,*) 'Error reading',indicator,' input file'
      	goto 11
          endif
 
          call ureadf('Scale factor for y grid',yscale(i))
 
          if( yscale(i) .ne. 0. .and. yscale(i) .ne. 1.) then
      	DO j=1,num_y(i)
      	    yarray(j,i) = yarray(j,i)*yscale(i)
      	END DO
          endif
 
      END DO
 
      DO i=1,2
          call ufgetr(10+i, uf_pre(i), uf_ext(i), uf_dev(i),
     1		uf_dir(i), ilshot,ierr)
      END DO
 
      IF (ishot(1) .ne. ishot(2)) THEN
          write(ztitle,'(a5,i5,a1,a3,a1,a5,i5,a1,a3)')
     1	uf_pre(1), ishot(1),'.',uf_ext(1),';',
     2	uf_pre(2), ishot(2),'.',uf_ext(2)
	
      ELSE
          ztitle = uf_pre(1)//'.'//uf_ext(1)//';  '//
     1	    uf_pre(2)//'.'//uf_ext(2)
      END IF
 
 
c
c	map to the same time base
c
      dt1 = (xarray(num_x(1),1) - xarray(1,1))/(num_x(1) - 1)
      dt2 = (xarray(num_x(2),2) - xarray(1,2))/(num_x(2) - 1)
 
      write(6,*) '1st Data time varies from',xarray(1,1),' to ',
     1	xarray(num_x(1),1),' in ',num_x(1),' steps'
      write(6,*) '2nd Data time varies from',xarray(1,2),' to ',
     1	xarray(num_x(2),2),' in ',num_x(2),' steps'
 
      IF (xarray(num_x(2),2).lt.xarray(1,1) .or.
     1	xarray(num_x(1),1).lt.xarray(1,2)) THEN
 
          write(6,*) 'No overlap !!'
          goto 10
      END IF
 
18    call ureadi('Choose Time grid to interpolate to (1 or 2)',jt)
 
      IF (jt .eq. 2) THEN
          j = 1
 
          DO i=1,num_x(2)
      	DO k=1,num_y(1)
      	    res(i,k) = 0.
      	END DO
 
      	if( i .ne. 1) then
      	    xf =  (xarray(i,2)+xarray(i-1,2))/2.
      	else
      	    xf = 1.5*xarray(1,2) - xarray(2,2)/2.
      	endif
 
      	DO WHILE (xarray(j,1).lt. xf
     1		.and. j .le. num_x(1) )
      	    j = j+1
      	END DO
 
      	if( i .ne. num_x(2) ) then
      	    xf = (xarray(i,2) + xarray(i+1,2))/2.
      	else
      	    xf = 1.5*xarray(i,2) - xarray(i-1,2)/2.
      	endif
 
      	n = 0
      	DO WHILE (xarray(j,1).le. xf .and.
     1		  j .le. num_x(1) )
      	    DO k=1,num_y(1)
      		res(i,k) = res(i,k) + zarray(j,k,1)
      	    END DO
 
      	    j = j+1
      	    n = n+1
      	END DO
 
      	if( n .gt. 0) then
      	    DO k=1,num_y(1)
      		res(i,k) = res(i,k)/n
      	    END DO
 
      	else if( j .ne. 1) then
      	    DO k=1,num_y(1)
      		res(i,k) = zarray(j-1,k,1) +
     1		 (zarray(j,k,1) - zarray(j-1,k,1))*
     1		 (xarray(i,2)-xarray(j-1,1))/(xarray(j,1)-xarray(j-1,1))
      	    END DO
 
      	else
      	    DO k=1,num_y(1)
      		res(i,k) = zarray(1,k,1) +
     1		 (zarray(2,k,1) - zarray(1,k,1))*
     1		 (xarray(i,2)-xarray(1,1))/(xarray(2,1)-xarray(1,1))
      	    END DO
 
      	endif
          END DO
 
          num_x(1) = num_x(2)
          DO i=1,num_y(1)
      	DO j=1,num_x(1)
      	    zarray(j,i,1) = res(j,i)
      	END DO
          END DO
 
          DO j=1,num_x(1)
      	xarray(j,1) = xarray(j,2)
          END DO
	
      ELSE IF (jt .eq. 1) THEN
          j = 1
 
          DO i=1,num_x(1)
      	DO k=1,num_y(2)
      	    res(i,k) = 0.
      	END DO
 
      	if( i .ne. 1) then
      	    xf =  (xarray(i,1)+xarray(i-1,1))/2.
      	else
      	    xf = 1.5*xarray(1,1) - xarray(2,1)/2.
      	endif
 
      	DO WHILE (xarray(j,2).lt. xf
     1		.and. j .le. num_x(2) )
      	    j = j+1
      	END DO
 
      	if( i .ne. num_x(1) ) then
      	    xf = (xarray(i,1) + xarray(i+1,1))/2.
      	else
      	    xf = 1.5*xarray(i,1) - xarray(i-1,1)/2.
      	endif
 
      	n = 0
      	DO WHILE (xarray(j,2).le. xf
     1		.and. j .le. num_x(2) )
      	    DO k=1,num_y(2)
      		res(i,k) = res(i,k) + zarray(j,k,2)
      	    END DO
 
      	    j = j+1
      	    n = n+1
      	END DO
 
      	if( n .gt. 0) then
      	    DO k=1,num_y(2)
      		res(i,k) = res(i,k)/n
      	    END DO
 
      	else if( j .ne. 1) then
      	    DO k=1,num_y(2)
      		res(i,k) = zarray(j-1,k,2) +
     1		 (zarray(j,k,2) - zarray(j-1,k,2))*
     1		 (xarray(i,1)-xarray(j-1,2))/(xarray(j,2)-xarray(j-1,2))
      	    END DO
 
      	else
      	    DO k=1,num_y(2)
      		res(i,k) = zarray(1,k,2) +
     1		 (zarray(2,k,2) - zarray(1,k,2))*
     1		 (xarray(i,1)-xarray(1,2))/(xarray(2,2)-xarray(1,2))
      	    END DO
 
      	endif
          END DO
 
          num_x(2) = num_x(1)
          DO i=1,num_y(2)
      	DO j=1,num_x(2)
      	    zarray(j,i,2) = res(j,i)
      	END DO
          END DO
	
          DO j=1,num_x(2)
      	xarray(j,2) = xarray(j,1)
          END DO
 
      ELSE
          goto 18
 
      END IF
 
      write(6,*) '1st Data y varies from',
     1   yarray(1,1),' to ',yarray(num_y(1),1),
     1   ' in ',num_y(1),' steps'
      write(6,*) '2nd Data y varies from',
     1   yarray(1,2),' to ',yarray(num_y(2),2),
     1   ' in ',num_y(2),' steps'
 
      IF (yarray(num_y(2),2).lt.yarray(1,1) .or.
     1	yarray(num_y(1),1).lt.yarray(1,2)) THEN
 
          write(6,*) 'No overlap !!'
          goto 10
      END IF
 
20    call ureadi('Choose Y grid to interpolate to (1 or 2)',j)
 
      IF (j .eq. 1) THEN
          k = 2
	
      ELSE IF (j .eq. 2) THEN
          k = 1
	
      ELSE
          goto 20
      END IF
 
      l = 1
      DO i=1,num_y(j)
c	    IF (yarray(i,j).ge.yarray(1,k) .and.
c	1	    yarray(i,j).le.yarray(num_y(k),k)) THEN
      	y_grid(l) = yarray(i,j)
      	l = l+1
c	    END IF
      END DO
      num_grid = l-1
 
      do i=1,2
          call map_2d(zarray(1,1,i),max_x,max_y,xarray(1,i),yarray(1,i),
     1		num_x(i),num_y(i),
     1		y_grid,num_grid,work,iwork)
      enddo
 
 
 
c	dt(1) = (xarray(num_x(1),1)-xarray(1,1))/num_x(1)
c	dt(2) = (xarray(num_x(2),2)-xarray(1,2))/num_x(2)
c
c	if( dt(1) .gt. dt(2)) then
c	    j = 1
c	    k = 2
c	else
c	    j = 2
c	    k = 1
c	endif
c
c	DO i=1,num_x(j)
c	    DO l=1,num_grid
c		res(i,l) = 0.
c	    END DO
c
c	    IF ( xarray(i,j).lt.xarray(1,k) .or.
c	1		xarray(i,j).gt.xarray(num_x(k),k) ) GOTO 25
c
c	    IF (i .eq. 1) THEN
c		t0 = xarray(1,j) - dt(j)
c		t1 = (xarray(1,j)+xarray(2,j))/2.
c
c	    ELSE IF (i .eq. num_x(j)) THEN
c		t0 = t1
c		t1 = xarray(i,j)+dt(j)
c
c	    ELSE
c		t0 = t1
c		t1 = (xarray(i,j)+xarray(i+1,j))/2.
c	    END IF
c
c	    kave = 0
c
c	    DO ik=1,num_x(k)
c		IF (xarray(ik,k) .ge. t0 .and. xarray(ik,k).le. t1) THEN
c		    DO l=1,num_grid
c			res(i,l) = res(i,l)+zarray(ik,l,k)
c		    END DO
c
c		    kave = kave + 1
c		END IF
c	    END DO
c
c	    IF( kave .ne. 0) THEN
c		DO l=1,num_grid
c		    res(i,l) = res(i,l)/kave
c		END DO
c	    ELSE
c		DO l=1,num_grid
c		    res(i,l) = 0.
c		END DO
c	    ENDIF
c
c25	END DO
 
 
30    call ureadw('Enter R for ratio (1/2), D for difference (1-2)',ans)
 
      IF (ans .eq. 'D') THEN
          rlab = 'difference$'
c	    IF (j .eq. 1) THEN
c		DO l=1,num_grid
c		    DO i=1,num_x(j)
c			res(i,l) = zarray(i,l,j) - res(i,l)
c		    END DO
c		END DO
c
c	    ELSE
c		DO l=1,num_grid
c		    DO i=1,num_x(j)
c			res(i,l) = res(i,l) - zarray(i,l,j)
c		    END DO
c		END DO
c	    END IF
 
          DO l=1,num_grid
      	DO i=1,num_x(j)
      	    res(i,l) = zarray(i,l,1) - zarray(i,l,2)
      	END DO
          END DO
 
      ELSE IF( ans .eq. 'R') THEN
          rlab = 'ratio$'
c	    IF (j .eq. 1) THEN
c		DO l=1,num_grid
c		    DO i=1,num_x(j)
c		      if( abs(res(i,l)).gt. abs(zarray(i,l,j))*1.e-20) then
c			res(i,l) = zarray(i,l,j) / res(i,l)
c		      else
c			res(i,l) = 0.
c		      endif
c		    END DO
c		END DO
c
c	    ELSE
c		DO l=1,num_grid
c		    DO i=1,num_x(j)
c		      if( abs(zarray(i,l,j)) .gt. abs(res(i,l))*1.e-20) then
c			res(i,l) = res(i,l) / zarray(i,l,j)
c		      else
c			res(i,l) = 0.
c		      endif
c		    END DO
c		END DO
c	    END IF
 
          DO l=1,num_grid
      	DO i=1,num_x(j)
      	  if( abs(zarray(i,l,2)).gt. abs(zarray(i,l,1))*1.e-20) then
      	    res(i,l) = zarray(i,l,1) / zarray(i,l,2)
      	  else
      	    res(i,l) = 0.
      	  endif
      	END DO
          END DO
 
 
      ELSE
          goto 30
 
      END IF
 
      call grf3(xarray(1,j),res,y_grid,work,num_x(j),num_grid,max_x,
     1	xlab(j)(1:10),ylab(j)(1:10),rlab,
     2	xlab(j)(20:30),ylab(j)(20:30),'none',ztitle,1,
     3	0,0,'TFTR',ishot(1),1,ishot,2,1)
 
      IF (ans .eq. 'R') THEN
50        call ureadw('Write UFile of averaged ratio [y/n]',ans)
          IF (ans .eq. 'Y') THEN
      	call ureadf('Start time for average',t0)
      	call ureadf('End time for average',t1)
      	IF (t1 .le. t0) THEN
      	    write(6,*) 'End time should be larger than start time'
      	    goto 50
      	END IF
 
 
      	DO i=1,num_grid
      	    calib(i) = 0.
      	END DO
 
      	n = 0
      	DO l=1,num_x(1)
      	    IF (xarray(l,1).ge.t0 .and. xarray(l,1).le. t1) THEN
      		DO i=1,num_grid
      		    calib(i) = calib(i) + res(l,i)
      		END DO
      		n = n+1
      	    END IF
      	END DO
 
      	DO i=1,num_grid
      	    calib(i) = calib(i)/n
      	END DO
 
      	call ufnter(9,'Avg. output: ')
      	ishot_w = ishot(1)
      	call ureadi('Shot number for ratio file output$',ishot_w)
      	call ufopwr(9,ishot_w,ier1)
      	if( ier1 .ne. 0) goto 50
 
      	call uf1dwr(9, tdev(2), shdate(2),
     1	    yarray(1,2), calib, num_grid, 0,
     2	    ylab(2),
     1	    'Cross calibration  factor     ',
     3	    nsc(2), scval(1,2), sclab(1,1,2), ier1 )
 
      	call ufclos(9)
          END IF
      END IF
 
60    call ureadw('Write UFile of 2D ratio or diff. [y/n]',ans)
      IF (ans .eq. 'Y') THEN
 
          call ufnter(9,'2D output: ')
          ishot_w = ishot(1)
          call ufcmpr(9,2)
          call ureadi('Shot number for ratio file output$',ishot_w)
          call ufopwr(9,ishot_w,ier1)
          if( ier1 .ne. 0) goto 60
 
          call uf2dwr(9, tdev(2), shdate(2),
     1	res, max_x, xarray(1,j), num_x(j), y_grid, num_grid, 0,
     2	rlab, xlab(j), ylab(2),
     3	nsc(2), scval(1,2), sclab(1,1,2), ier1 )
 
          call ufclos(9)
      END IF
 
	
      goto 10
      end
!
c ============================================================================
      subroutine map_2d(zarray,max_x,max_y,xarray,yarray,num_x,num_y,
     1	    y_grid,num_grid,w,iw)
 
      real zarray(max_x,max_y), xarray(num_x), yarray(num_y)
      real y_grid(num_grid)
 
      real  w(max_y,2)
      integer iw(max_y,2)
 
c	set up the coefficients and indexes
 
      i = 1
      DO j = 1, num_y
        DO WHILE (y_grid(i) .lt. yarray(j))
          IF (y_grid(i) .eq. yarray(j)) THEN
      	w(i,1) = 1.
      	iw(i,1) = j
      	iw(i,2) = j
 
          ELSE IF (j .eq. 1) THEN
      	w(i,1) = 1.
      	iw(i,1) = j
      	iw(i,2) = j
	
          ELSE
      	w(i,1) = (y_grid(i)-yarray(j-1))/(yarray(j)-yarray(j-1))
      	iw(i,1) = j-1
      	iw(i,2) = j
	
          END IF
 
          i=i+1
          if( i.gt. num_grid) goto 20
        END DO
      END DO
 
      IF (i .le. num_grid) THEN
          DO ii=i,num_grid
      	w(ii,1) = 1.
      	iw(ii,1) = num_y
      	iw(ii,2) = num_y
          END DO
      END IF
 
 
c
c	check if there is anything to do
c
20    DO i=1,num_grid
          IF (w(i,1) .ne. 1. .or. iw(i,1) .ne. i) THEN
      	goto 30
          END IF
      END DO
      RETURN
 
c
c	oh, well, it takes real work
c	do all the work into a temp. holding place, then put it away
 
30    DO k=1,num_x
          DO i=1,num_grid
             w(i,2) = w(i,1)*zarray(k,iw(i,2)) +
     >          (1.-w(i,1))*zarray(k,iw(i,1))
          END DO
 
          DO i=1,num_grid
             zarray(k,i) = w(i,2)
          END DO
      END DO
 
      RETURN
      END
