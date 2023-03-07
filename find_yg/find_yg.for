      program find_yg
c
c	modification of CMP_2D, Sept. 1992
c	Dimension Statements for the UFILES programs
c
      parameter (max_x=102400)
      parameter (max_y=64)
      real xarray(max_x,2), yarray(max_y,2), work(max_x,max_y)
      integer num_x(2), num_y(2),ishot(2)
      real zarray(max_x, max_y,2)
      character*30	flab(2), xlab(2), ylab(2)
      real	yscale(2)
 
      parameter (nscmax=50)
      real scval(nscmax,2)
      character*4 tdev(2)
      character*10 shdate(2)
      character*10 sclab(3,nscmax,2)
      integer nsc(2)
	
      character*6   indicator

      call ufheader("find_yg")
 
      call uinita(0)
 
      write(6,*)
     >  'This program reads two 2d UFiles, and does a least square-fit'
      write(6,*)
     >  'to the positions of the 2nd file to minimize the difference'
      write(6,*) 'in the time-evolution of the two data-sets'
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
 
      dy1 = (yarray(num_y(1),1) - yarray(1,1))/(num_y(1) - 1)
      dy2 = (yarray(num_y(2),2) - yarray(1,2))/(num_y(2) - 1)
 
      dt1 = (xarray(num_x(1),1) - xarray(1,1))/(num_x(1) - 1)
      dt2 = (xarray(num_x(2),2) - xarray(1,2))/(num_x(2) - 1)
 
      IF (dt1 .le. dt2) THEN
          j = 1
 
          DO i=1,num_x(2)
      	DO k=1,num_y(1)
      	    work(i,k) = 0.
      	END DO
 
      	DO WHILE (xarray(j,1).lt. xarray(i,2)-dt2/2.
     1		.and. j .le. num_x(1) )
      	    j = j+1
      	END DO
 
      	n = 0
      	DO WHILE (xarray(j,1).le. xarray(i,2)+dt2/2. .and.
     1		  j .le. num_x(1) )
      	    DO k=1,num_y(1)
      		work(i,k) = work(i,k) + zarray(j,k,1)
      	    END DO
 
      	    j = j+1
      	    n = n+1
      	END DO
 
      	if( n .gt. 0) then
      	    DO k=1,num_y(1)
      		work(i,k) = work(i,k)/n
      	    END DO
      	endif
          END DO
 
          num_x(1) = num_x(2)
          DO i=1,num_y(1)
      	DO j=1,num_x(1)
      	    zarray(j,i,1) = work(j,i)
      	END DO
          END DO
 
          DO j=1,num_x(1)
      	xarray(j,1) = xarray(j,2)
          END DO
	
      ELSE IF (dt1 .gt. dt2) THEN
          j = 1
 
          DO i=1,num_x(1)
      	DO k=1,num_y(2)
      	    work(i,k) = 0.
      	END DO
 
      	DO WHILE (xarray(j,2).lt. xarray(i,1)-dt1/2.
     1		.and. j .le. num_x(2) )
      	    j = j+1
      	END DO
 
      	n = 0
      	DO WHILE (xarray(j,2).le. xarray(i,1)+dt1/2.
     1		.and. j .le. num_x(2) )
      	    DO k=1,num_y(2)
      		work(i,k) = work(i,k) + zarray(j,k,2)
      	    END DO
 
      	    j = j+1
      	    n = n+1
      	END DO
 
      	if( n .gt. 0) then
      	    DO k=1,num_y(1)
      		work(i,k) = work(i,k)/n
      	    END DO
      	endif
          END DO
 
          num_x(2) = num_x(1)
          DO i=1,num_y(2)
      	DO j=1,num_x(2)
      	    zarray(j,i,2) = work(j,i)
      	END DO
          END DO
	
          DO j=1,num_x(2)
      	xarray(j,2) = xarray(j,1)
          END DO
      END IF
 
      t_min = max(xarray(1,1),xarray(1,2))
      t_max = min(xarray(num_x(1),1),xarray(num_x(2),2))
      t_min2 = t_min
      t_max2 = t_max
 
34    call ureadf('Set start-time for fit',t_min)
      if( t_min .lt. t_min2) goto 34
 
35    call ureadf('Set end-time for fit', t_max)
      if( t_max .le. t_min .or. t_max .gt. t_max2) goto 35
 
      it_beg = 1
      DO WHILE (xarray(it_beg,1) .lt. t_min .and. it_beg .lt. num_x(1))
          it_beg = it_beg + 1
      END DO
 
      it_end = it_beg
      DO WHILE (xarray(it_end,1) .le. t_max .and. it_end .lt. num_x(1))
          it_end = it_end + 1
      END DO
      IF (xarray(it_end,1) .gt. t_max) THEN
          it_end = it_end - 1
      END IF
 
      iy = 0
      sum_y = 0.
      DO WHILE (sum_y .eq. 0. .and. iy .lt. num_y(1))
          iy = iy+1
          sum_y = 0.
          DO i=it_beg,it_end
      	sum_y = sum_y + zarray(i,iy,1)
          END DO
      END DO
 
      if( iy .eq. num_y(1) .and. sum_y .eq. 0.) then
          write(6,*) 'No useful data to fit to !!'
          goto 10
      end if
 
      sum_y = 0.
      num_y(1) = num_y(1) + 1
      DO WHILE (sum_y .eq. 0. .and. iy .lt. num_y(1))
          num_y(1) = num_y(1) - 1
          sum_y = 0.
          DO i=it_beg,it_end
      	sum_y = sum_y + zarray(i,num_y(1),1)
          END DO
      END DO
 
      num_y(1) = num_y(1) - iy + 1
 
      write(6,*) 'Fit results:'
      write(6,*) 'nom. ratio   %rms-dev    fit   ratio   %rms-dev'
      DO i=1,num_y(2)
          call fit_2d(0,zarray(1,iy,1), max_x, max_y, xarray(1,1),
     1		yarray(iy,1),
     1		num_x(1), num_y(1), it_beg, it_end,
     2		zarray(1,i,2), yarray(i,2),
     3		fit, xfactor, xrms_err, xavg, iflag)
 
          call fit_2d(1,zarray(1,iy,1), max_x, max_y, xarray(1,1),
     1		yarray(iy,1),
     1		num_x(1), num_y(1), it_beg, it_end,
     2		zarray(1,i,2), yarray(i,2),
     3		fit, factor, rms_err, avg, iflag)
 
          call fit_2d(1,zarray(1,iy,1), max_x, max_y, xarray(1,1),
     1		yarray(iy,1),
     1		num_x(1), num_y(1), it_beg, it_end,
     2		zarray(1,i,2), yarray(i,2) - dy1,
     3		fit2, factor2, rms_err2, avg2, iflag2)
 
          IF (rms_err2/avg2 .lt. rms_err/avg) THEN
      	fit = fit2
      	factor = factor2
      	fms_err = rms_err2
      	avg = avg2
      	iflag = iflag2
          END IF
 
          call fit_2d(1,zarray(1,iy,1), max_x, max_y, xarray(1,1),
     1		yarray(iy,1),
     1		num_x(1), num_y(1), it_beg, it_end,
     2		zarray(1,i,2), yarray(i,2) + dy1,
     3		fit2, factor2, rms_err2, avg2, iflag2)
 
          IF (rms_err2/avg2 .lt. rms_err/avg) THEN
      	fit = fit2
      	factor = factor2
      	fms_err = rms_err2
      	avg = avg2
      	iflag = iflag2
          END IF
 
1000  format( 0pf8.2,1pe11.2,e11.2,0pf10.2,a1,1pe11.2,e11.2)
 
          if( iflag .eq. 0) then
      	write(6,1000) yarray(i,2),xfactor,xrms_err,
     1		fit,' ',factor,rms_err
          else if( iflag .eq. 1) then
      	write(6,1000) yarray(i,2),xfactor,xrms_err,
     1		fit,'*',factor,rms_err
          else if( iflag .eq. -1) then
      	write(6,'( 0pf8.2, a)') yarray(i,2), ' Dead'
 
          end if
      END DO
 
      goto 10
      end
!
c ============================================================================
      subroutine fit_2d(ifit, zarray,
     >  max_x,max_y,xarray,yarray,num_x,num_y,
     >  it_beg, it_end, f, value, fit, factor, rms_dev, avg,iflag)
 
      real zarray(max_x,max_y), xarray(num_x), yarray(num_y)
      real f(num_x)
 
      logical done, almost
 
c	set up the coefficient and index
 
      fit = value
      n_pts = it_end - it_beg + 1
 
      IF (fit .le. yarray(1)) THEN
          w = 0.5
          iw1 = 1
          iw2 = 2
          fit = (yarray(iw1) + yarray(iw2))/2
	
      ELSE IF (fit .ge. yarray(num_y)) THEN
          w = 0.5
          iw1 = num_y - 1
          iw2 = num_y
          fit = (yarray(iw1) + yarray(iw2))/2
	
      ELSE
          j = 2
          DO WHILE (fit .gt. yarray(j))
      	j = j+1
          END DO
 
          w = (fit-yarray(j-1))/(yarray(j)-yarray(j-1))
          iw1 = j-1
          iw2 = j
      END IF
      dw = 1/(yarray(iw2) - yarray(iw1))
 
 
      done = .false.
      almost = .false.
      offset = 0.
      ijmp = 0
      o_d = 0.
 
      DO WHILE (.not. done)
          factor = 0.
          d_f = 0.
 
          i_pts = 0
          DO i=it_beg,it_end
            IF ( f(i) .gt. 1.e-15*(zarray(i,iw1)+zarray(i,iw2)) ) THEN
      	factor = factor +
     1		(zarray(i,iw1)*w + zarray(i,iw2)*(1.-w))/f(i)
      	d_f = d_f + (zarray(i,iw1) - zarray(i,iw2))*dw/f(i)
      	i_pts = i_pts + 1
            END IF
          END DO
 
          if( i_pts .eq. 0) then
      	iflag = -1	    ! the channel must be dead, no sensible data
      	return
          end if
 
          factor = factor/i_pts
          d_f = d_f / i_pts
 
          avg = 0.
          d = 0.
          d_d = 0.
          dd_d = 0.
 
          DO i=it_beg,it_end
      	avg = avg + factor*f(i)
      	ri = (zarray(i,iw1)*w + zarray(i,iw2)*(1.-w))
      	d_ri = (zarray(i,iw1) - zarray(i,iw2))*dw
 
      	dev = 2.*(factor*f(i) - ri)/(factor*f(i) + ri)
      	d_dev = 2.*(f(i)*d_f - d_ri)/(factor*f(i) + ri)
     1	      - dev * (f(i)*d_f + d_ri)/(factor*f(i) + ri)
      	dd_dev = - 3.*d_dev * (f(i)*d_f + d_ri)/(factor*f(i) + ri)
 
      	d = d + dev**2
      	d_d = d_d + 2*dev*d_dev
      	dd_d = dd_d + 2*d_dev**2 + 2*dev*dd_dev
          END DO
 
          avg = avg/n_pts
          d = d/n_pts
          d_d = d_d/n_pts
          dd_d = dd_d/n_pts
 
          IF ( almost ) THEN
      	done = .true.
      	iflag = 1
 
          ELSE IF (ijmp*d_d .gt. 0) THEN	! will we try and go back??
      	done = .true.
      	iflag = 1
 
          ELSE IF (abs(d_d) .le. 1.e-3*d/(yarray(iw2)-yarray(iw1))
     1		    .or. ifit.eq.0 ) THEN
 
      	done = .true.
      	iflag = 0
	
          ELSE IF (dd_d .ge.
     >       1.e-4*abs(d_d)/(yarray(iw2)-yarray(iw1))) THEN
      	offset = - d_d / dd_d * 0.75
	
          ELSE IF (abs(offset) .gt. 1.0e-32) THEN
      	IF (d_d .lt. 0.) THEN
      	    offset = 0.5 * abs(offset)
	
      	ELSE
      	    offset = - 0.5*abs(offset)
      	END IF
 
          ELSE
      	offset = - d / d_d * 0.1
 
          END IF
 
          ijmp = 0
          o_fit = fit
          o_d = d
 
          IF (.not. done .and. .not. almost ) THEN
      	IF (offset.gt.0 .and. fit+offset.gt.yarray(iw2) ) THEN
      	    IF (fit .ne. yarray(iw2) ) THEN
      		fit = yarray(iw2)
      		IF (iw2 .eq. num_y) almost = .true.
	
      	    ELSE
      		IF (iw2 .ne.  num_y) THEN
      		    iw1 = iw2
      		    iw2 = iw2+1
      		    ijmp = 1
	
      		ELSE
      		    almost = .true.
      		END IF
      	    END IF
	
      	ELSE IF (offset.lt.0 .and. fit+offset.lt.yarray(iw1) ) THEN
      	    IF (fit .ne. yarray(iw1) ) THEN
      		fit = yarray(iw1)
      		IF (iw1 .eq. 1)  almost = .true.
	
      	    ELSE
      		IF (iw1 .ne.  1) THEN
      		    iw2 = iw1
      		    iw1 = iw1-1
      		    ijmp = -1
	
      		ELSE
      		    almost = .true.
      		END IF
      	    END IF
	
      	ELSE
      	    fit = fit + offset
      	END IF
          END IF
 
 
          IF (.not. done ) THEN
      	w = (fit-yarray(iw1))/(yarray(iw2)-yarray(iw1))
      	dw = 1/(yarray(iw2) - yarray(iw1))
          END IF
 
      END DO
 
      IF (d .gt. 1.e-28) THEN
          rms_dev = sqrt( d)
	
      ELSE
          rms_dev = 0.
      END IF
 
 
      RETURN
      END
