      program mult2
c
c	Dimension Statements for the UFILES programs
c
      parameter (max_x=32768)
      parameter (max_y=64)
      real xarray(max_x), yarray(max_y), calib(max_y), y2array(max_y)
      real zarray(max_x, max_y)
      character*30	flab, xlab, ylab, glab, xlab2, flab2
 
      parameter (nscmax=50)
      real scval(nscmax),scval2(nscmax)
      character*4 tdev, tdev2
      character*10 shdate, shtime, shdate2
      character*10 sclab(3,nscmax),sclab2(3,nscmax)
      logical lxout
      character*1 ans, ans2
 
      integer iblk(20)
 
      logical done
 
 
      yscale1 = 1.
      yscale2 = 1.
 
      call uinita(0)
 
      write(6,*)
     >   'This program reads a 2d UFile, and multiplies the data'
      write(6,*) 'in it by the contents of a 1d UFile. '
      write(6,*) 'e.g. for cross-calibrating ECE data'
      write(6,*) ' '
      write(6,*)
     >   'In addition, if the data consists of several separated'
      write(6,*)
     >   'window, the user gets the option of splitting the windows'
      write(6,*) 'into separate files'
      write(6,*) ' '
 
10    call ureadi('Shot number for data file (<=0 to stop)',ishot)
      if(ishot .le. 0) stop
 
      call ufnter(2,'INPUT: data file')
      	call ufoprd(2,ishot,ierr)
      if( ierr .eq. 0) then
          nmax_x = max_x
          call uf2drd(2,tdev,shdate,
     1       zarray,nmax_x,xarray,nmax_x,yarray,max_y,
     1       iproc, num_x, num_y, 0, flab, xlab, ylab,
     2       nscmax, nsc, scval, sclab, ierr)
      endif
      if( ierr .ne. 0) then
          write(6,*) 'Error reading Data input file'
          goto 10
      endif
	
      ishot_c = ishot
20    call ureadi('Shot number for calibration file ( <0 for no recal)',
     1	    ishot_c)
      if(ishot_c .le. 0) goto 30
 
      call ufnter(1,'INPUT: calibration file')
      	call ufoprd(1,ishot_c,ierr)
      if( ierr .eq. 0) then
          nmax_x = max_x
          call uf1drd( 1, tdev2,shdate2,y2array,calib,max_y,num_c,
     1	    iproc2, xlab2, flab2,
     2	    nscmax, nsc2, scval2, sclab2, ierr)
          call ufclos(1)
      endif
      if( ierr .ne. 0) then
          write(6,*) 'Error reading calibration file'
          goto 20
      endif
 
      IF (num_c .ne. num_y) THEN
          write(6,*) 'Calibration file has wrong number of points!!'
          goto 20
	
      ELSE
          DO i=1,num_y
      	IF ( (yarray(i)-y2array(i)) .gt. 1.e-3*(yarray(i)+y2array(i)) )
     1	    THEN
 
      	    write(6,*) 'Calibration file doesn''t match data axis'
      	    goto 20
      	END IF
          END DO
 
          DO i=1,num_y
      	DO j=1,num_x
      	    zarray(j,i) = zarray(j,i)*calib(i)
      	END DO
          END DO
      END IF
 
 
30    dt_avg = abs(xarray(num_x) - xarray(1))/(num_x - 1)
      dt_min = dt_avg
 
      nblk=1
      iblk(1)=1
 
      DO i=2,num_x
          dt = xarray(i) - xarray(i-1)
 
          IF (dt .ne. 0.) THEN
      	dt_min = min(dt, dt_min)
      	IF (dt .gt. dt_min*100) THEN
      	    nblk = nblk+1
      	    iblk(nblk) = i
      	END IF
          END IF
      END DO
 
      iblk(nblk+1) = num_x+1
 
      IF (nblk .gt. 1) THEN
          write(6,*) 'Data consists of multiple time windows:'
          write(6,*) '    t0            tend            nsamples'
          DO i=1,nblk
      	write(6,*) xarray(iblk(i)),xarray(iblk(i+1)-1),iblk(i+1)-iblk(i)
          END DO
 
          write(6,*) ' '
          call ureadc(
     >       'Write each time window as a separate UFile [y/n]',ans)
          IF ( ans .eq. 'Y') THEN
            call ureadc(
     1          'Re-create time axis for each time window [y/n]',
     1          ans2)
          ELSE
            ans2 = 'N'
          END IF
	
      ELSE
          write(6,*)
     >      'Data consists of a single time window => a single UFile'
          ans = 'N'
          ans2 = 'N'
      END IF
 
      IF (ans .ne. 'Y') THEN
          nblk = 1
          iblk(nblk+1) = num_x+1
      END IF
 
      DO i=1,nblk
          write(6,*) ' '
          if( ans .eq. 'Y') write(6,*) 'Now processing block',i
 
          IF (ans2 .eq. 'Y') THEN
      	dt = (xarray(iblk(i+1)-1) - xarray(iblk(i))) /
     1		(iblk(i+1)-iblk(i)-1)
      	t0 = xarray(iblk(i))
 
      	call ureadf('Initial time',t0 )
      	call ureadf('Time step per point',dt)
	
      	DO j=iblk(i),iblk(i+1)-1
      	    xarray(j) = t0 + dt*(j-iblk(i))
      	END DO
          END IF
 
          nmax_x = max_x
 
          npts = iblk(i+1) - iblk(i)
 
          IF (i .ne. 1) THEN
      	DO k=1,num_y
      	    DO j=1,npts
      		zarray(j,k) = zarray(iblk(i)+j-1, k )
      	    END DO
      	END DO
          END IF
 
35        call ufnter(2+i,'OUTPUT: file')
          call ufcmpr(2+i,2)
      	    call ufopwr(2+i,ishot,ierr)
          if( ierr .ne. 0) goto 30
 
          call uf2dwr(2+i,tdev,shdate,
     1       zarray,nmax_x,xarray(iblk(i)),npts,
     1       yarray,num_y,
     1       iproc, flab, xlab, glab,
     2       nsc, scval, sclab, ierr)
 
 
          if( i .eq. 1) call uspool( 2, 2+i)
      	    call ufclos(2+i)
          if( ierr .ne. 0) goto 35
 
      	    if(i .eq. 1) call ufclos(2)
      END DO
      goto 10
      end
