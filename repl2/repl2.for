      PROGRAM REPL2
C
C PROGRAM DESCRIPTION:
C
C	 REPL2
c	    this program allow individual points to be replicated or
c	 interpolated in 2-d UFiles.  This is particularly usefull at
c	 the begining or end of beam pulses
C
C AUTHORS:
C
C	 MCZ
C
C CREATION DATE: 	11 Mar 88
c modified mcz Aug 96 from REPL1
C
C
C	     C H A N G E   L O G
C
C	Date     | Name  | Description
C----------------+-------+-----------------------------------------------------
C [change_entry]
C
c
c	Dimension Statements for the UFILES programs
c
      parameter (nscmax=50)
      real scval(nscmax),scval2(nscmax)
      character*4 tdev
      character*10 shdate, shtime
      character*10 sclab(3,nscmax),sclab2(3,nscmax)
      logical lxout
 
      parameter (maxx=16384, maxy = 64)
      real xarray(maxx), yarray(maxx), yerrs(maxx)
      real zarray(maxx*maxy), work(maxx), slice(maxx)
      real vdel(maxx)
      integer idel(maxx), jdel(maxx), num_x, num_y
      character*30 xlab, ylab, xlabdum, ylabdum, xunits, yunits,flab
 
      character*1   cmd,ans, anst
 
      logical no_time, t_flag, slice_x, sliced, interp_x
      integer islice, jslice, n_slice
	
      call ufheader("repl2")
      call uinitg("REPL2")
 
10    call ufnter(1,'INPUT:$ ')
	
20    call ureadi('Shot Number (<=0 to stop)$',ishot)
      if( ishot .le. 0) stop
 
30      call ufoprd(1,ishot,ierr)
 
      if( ierr .eq. 0) then
          call uf2drd( 1, tdev,shdate,zarray,maxy,
     1     xarray,maxx,yarray,maxx,
     1     iproc, num_x, num_y, 1, flab, xlab, ylab,
     2     nscmax, nsc, scval, sclab, ierr)
      endif
 
      if( ierr .ne. 0) then
          write(6,*) 'Error reading input file'
      	    call ufclos(1)
          goto 10
      endif
 
      no_time = .false.
 
      if( xlab(1:4) .eq. 'TIME' .or.
     1   xlab(1:4) .eq. 'Time' .or.
     1   xlab(1:4) .eq. 'time' ) then
 
           t_flag = .true.
 
      else if( ylab(1:4) .eq. 'TIME' .or.
     1   ylab(1:4) .eq. 'Time' .or.
     3   ylab(1:4) .eq. 'time') then
 
           t_flag = .false.
 
      else
           write(6,*) ' No TIME axis !!'
           call ureadc('Continue anyway [Y/N]',anst)
           if( anst .eq. 'Y') then
      	no_time = .true.
           else
      	goto 10
           endif
      endif
 
      sliced = .false.
      ndel = 0
 
100   if( lxout(0) ) then
 
          write(6,*) ' '
          if( sliced .and. slice_x) then
      	write(6,*) 'Slice vs. ',xlab(1:10),' at ',value,ylab(20:30)
      	write(6,*) ' '
          else if( sliced) then
      	write(6,*) 'Slice vs. ',ylab(1:10),' at ',value,xlab(20:30)
      	write(6,*) ' '
          endif
 
          write(6,*) '[D] to change file names'
          write(6,*) '[S] to get new shot'
          write(6,*) ' '
          write(6,*) '[G] to plot 2D data'
          if( .not. no_time) then
      	write(6,*) '[T] to select time evolution at fixed X'
      	write(6,*) '[X] to select spatial profile at fixed Time'
          else
      	write(6,*) '[X] to select slice vs. first coordinate'
      	write(6,*) '[Y] to select slice vs. second coordinate'
          endif
          if( sliced ) then
      	write(6,*) '[P] to plot slice'
      	write(6,*) '[C] to cut out points more extreeme than a value'
      	write(6,*) '[K] to delete a point'
c		write(6,*) '[R] to remove range of data '
          endif
          if( ndel .ne. 0) write(6,*) '[Z] Un-delete last removed point'
          write(6,*) ' '
          write(6,*) '[W] to write output to file'
 
      endif
 
110   call ureadc('REPL1:  Enter choice:$',cmd)
      IF (cmd .eq. 'D') THEN
      	    call ufclos(1)
          goto 10
	
      ELSE IF (cmd .eq. 'S') THEN
      	    call ufclos(1)
          goto 20
	
!	ELSE IF (cmd .eq. 'Q') THEN
!      	    call ufclos(1)
!	    stop
 
      ELSE IF (cmd .eq. 'Z' .and. ndel .gt. 0) THEN
          call zset(zarray,idel(ndel),jdel(ndel),num_x,num_y,vdel(ndel))
          ndel = ndel - 1
 
      ELSE IF (cmd .eq. 'G') THEN
          call grf3(xarray,zarray,yarray,work,num_x,num_y,num_x,
     1	xlab(1:10),ylab(1:10),flab(1:20),
     2	xlab(20:30),ylab(20:30),flab(20:30),'Input data',0,
     3	0,0,'TFTR',ishot,0,0,1,iproc)
 
      ELSE IF (cmd .eq. 'T' .and. .not. no_time) THEN
          call ureadf('Enter fixed coordinate value for slice$',value)
          IF (t_flag ) THEN
      	slice_x = .true.
      	n_slice = num_x
 
      	call extr_y(zarray,num_x,num_y,yarray,
     1		slice, value, jslice)
	
          ELSE
      	slice_x = .false.
      	n_slice = num_y
 
      	call extr_x(zarray,num_x,num_y,xarray,
     1		slice, value, islice)
	
          END IF
          sliced = .true.
 
      ELSE IF (cmd .eq. 'X' .and. .not. no_time) THEN
          call ureadf('Enter fixed Time of slice$',value)
          IF (.not. t_flag ) THEN
      	slice_x = .true.
      	n_slice = num_x
 
      	call extr_y(zarray,num_x,num_y,yarray,
     1		slice, value, jslice)
	
          ELSE
      	slice_x = .false.
      	n_slice = num_y
 
      	call extr_x(zarray,num_x,num_y,xarray,
     1		slice, value, islice)
	
          END IF
          sliced = .true.
	
      ELSE IF (cmd .eq. 'X' .and. no_time) THEN
          call ureadf('Enter fixed Y-value for slice$',value)
          slice_x = .true.
          n_slice = num_x
 
          call extr_y(zarray,num_x,num_y,yarray,
     1	    slice, value, jslice)
          sliced = .true.
	
      ELSE IF (cmd .eq. 'Y' .and. no_time) THEN
          call ureadf('Enter fixed X-value for slice$',value)
          slice_x = .false.
          n_slice = num_y
 
          call extr_x(zarray,num_x,num_y,xarray,
     1	    slice, value, islice)
          sliced = .true.
	
      ELSE IF (cmd .eq. 'P' .and. sliced) THEN
          if( slice_x) then
      	call grfuf1(xarray,slice,num_x,
     >		  iproc,flab,xlab,ishot,tdev,shdate,1,value
     >		  ,ylab)
          else
      	call grfuf1(yarray,slice,num_y,
     >		  iproc,flab,ylab,ishot,tdev,shdate,1,value
     >		  ,xlab)
 
          end if
	
      ELSE IF (cmd .eq. 'K' .and. sliced) THEN
          call ureadf('Coordinate value of point to delete$',x1)
          interp_x = no_time .or. t_flag
 
          IF (slice_x) THEN
      	dist = abs(x1 - xarray(1))
      	ii = 1
      	DO i=2,num_x
      	    if( abs(x1 - xarray(i)) .lt. dist) then
      		dist = abs(x1 - xarray(i))
      		ii = i
      	    endif
      	END DO
 
      	ndel = ndel + 1
      	idel(ndel) = ii
      	jdel(ndel) = jslice
      	call zinterp(zarray,ii,jslice,xarray,yarray,
     1		  num_x,num_y,vdel(ndel),interp_x)
      	call zget(zarray,ii,jslice,num_x,num_y,slice(ii))
 
          ELSE
      	dist = abs(x1 - yarray(1))
      	ii = 1
      	DO i=2,num_y
      	    if( abs(x1 - yarray(i)) .lt. dist) then
      		dist = abs(x1 - yarray(i))
      		ii = i
      	    endif
      	END DO
 
      	ndel = ndel + 1
      	idel(ndel) = islice
      	jdel(ndel) = ii
      	call zinterp(zarray,islice,ii,xarray,yarray,
     1		  num_x,num_y,vdel(ndel),interp_x)
      	call zget(zarray,islice,ii,num_x,num_y,slice(ii))
 
          END IF
	
      ELSE IF (cmd .eq. 'R' .and. sliced) THEN
          call ureadf('Lower coord. value of range to delete$',x0)
          call ureadf('Upper coord. value of range to delete$',x1)
 
          ndel_o = ndel
          interp_x = no_time .or. t_flag
 
          if( x0 .gt. x1) then
      	write(6,*) 'Range is vacuous!'
 
          ELSE IF (slice_x .and. interp_x) THEN
      	ib = 1
      	DO WHILE (xarray(ib).lt.x0 .and. ib.lt.num_x)
      	    ib = ib+1
      	END DO
 
      	IF (xarray(ib) .le. x1) THEN
      	    ie = ib
      	    DO WHILE (xarray(ie).le.x1 .and. ie.lt.num_x)
      		ie = ie+1
      	    END DO
      	    IF (xarray(ie) .gt. x1) ie = ie-1
 
      	    IF (ib.eq.1 .and. ie.eq.num_x) THEN
      		write(6,*) 'Trying to delete whole slice!'
      		write(6,*) '***Not implemented and not defined***'
      		goto 100
	
      	    ELSE IF (ib .eq. 1) THEN
      		call zget(zarray,ie+1,jslice,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE IF (ie .eq. num_x) THEN
      		call zget(zarray,ib-1,jslice,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE
      		call zget(zarray,ib-1,jslice,num_x,num_y,val0)
      		call zget(zarray,ie+1,jslice,num_x,num_y,val1)
      		factor = (val1 - val0)/(xarray(ie+1) - xarray(ib-1))
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val0 + factor*( xarray(i)-xarray(ib-1))
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
      	    END IF
      	END IF
 
		         ! slice_y and interp_y
          ELSE if( .not. slice_x .and. .not. interp_x) THEN
      	ib = 1
      	DO WHILE (yarray(ib).lt.x0 .and. ib.lt.num_y)
      	    ib = ib+1
      	END DO
 
      	IF (yarray(ib) .le. x1) THEN
      	    ie = ib
      	    DO WHILE (yarray(ie).le.x1 .and. ie.lt.num_y)
      		ie = ie+1
      	    END DO
      	    IF (yarray(ie) .gt. x1) ie = ie-1
 
      	    IF (ib.eq.1 .and. ie.eq.num_y) THEN
      		write(6,*) 'Trying to delete whole slice!'
      		write(6,*) '***Not implemented and not defined***'
      		goto 100
	
      	    ELSE IF (ib .eq. 1) THEN
      		call zget(zarray,islice,ie+1,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE IF (ie .eq. num_y) THEN
      		call zget(zarray,islice,ib-1,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE
      		call zget(zarray,islice,ib-1,num_x,num_y,val0)
      		call zget(zarray,islice,ie+1,num_x,num_y,val1)
      		factor = (val1 - val0)/(yarray(ie+1) - yarray(ib-1))
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val0 + factor*( yarray(i)-yarray(ib-1))
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
      	    END IF
      	END IF
 
          ELSE IF( slice_x) THEN	! and interp_y
      	DO ii=1,num_x
      	    if( xarray(ii).ge.x0 .and. xarray(ii).le.x1 ) then
      		ndel = ndel + 1
      		idel(ndel) = ii
      		jdel(ndel) = jslice
      		call zinterp(zarray,ii,jslice,xarray,yarray,
     1			  num_x,num_y,vdel(ndel),interp_x)
      		call zget(zarray,ii,jslice,num_x,num_y,slice(ii))
      	    endif
      	END DO
 
          ELSE			! slice_y and interp_x
      	DO ii=1,num_y
      	    if( yarray(ii).ge.x0 .and. yarray(ii).le.x1 ) then
      		ndel = ndel + 1
      		idel(ndel) = islice
      		jdel(ndel) = ii
      		call zinterp(zarray,islice,ii,xarray,yarray,
     1			  num_x,num_y,vdel(ndel),interp_x)
      		call zget(zarray,islice,ii,num_x,num_y,slice(ii))
      	    endif
      	END DO
 
          END IF
 
          if( ndel .eq. ndel_o) then
      	write(6,*) 'No points in range!'
          endif
 
      ELSE IF (cmd .eq. 'C' .and. sliced) THEN
          call ureadf('Value to cut beyond$',x0)
 
          ndel_o = ndel
          interp_x = no_time .or. t_flag
 
          IF (slice_x .and. interp_x) THEN
      	ib = 1
 
500   	call zget(zarray,ib,jslice,num_x,num_y,val)
      	DO WHILE (( val*x0.le.0. .or. abs(val).lt.abs(x0) )
     1		 .and. ib.lt.num_x)
      	    ib = ib+1
      	    call zget(zarray,ib,jslice,num_x,num_y,val)
      	END DO
 
      	IF (val*x0.gt.0. .and. abs(val).ge.abs(x0)) THEN
      	    ie = ib
      	    DO WHILE (val*x0.gt.0. .and. abs(val).ge.abs(x0)
     1		      .and. ie.lt.num_x)
      		ie = ie+1
      		call zget(zarray,ie,jslice,num_x,num_y,val)
      	    END DO
      	    IF (.not.(val*x0.gt.0..and.abs(val).ge.abs(x0))) ie = ie-1
 
      	    IF (ib.eq.1 .and. ie.eq.num_x) THEN
      		write(6,*) 'Trying to delete whole slice!'
      		write(6,*) '***Not implemented and not defined***'
      		goto 100
	
      	    ELSE IF (ib .eq. 1) THEN
      		call zget(zarray,ie+1,jslice,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE IF (ie .eq. num_x) THEN
      		call zget(zarray,ib-1,jslice,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE
      		call zget(zarray,ib-1,jslice,num_x,num_y,val0)
      		call zget(zarray,ie+1,jslice,num_x,num_y,val1)
      		factor = (val1 - val0)/(xarray(ie+1) - xarray(ib-1))
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = i
      		    jdel(ndel) = jslice
      		    call zget(zarray,i,jslice,num_x,num_y,vdel(ndel))
      		    slice(i) = val0 + factor*( xarray(i)-xarray(ib-1))
      		    call zset(zarray,i,jslice,num_x,num_y,slice(i))
      		END DO
      	    END IF
 
      	    ib = ie
      	    goto 500
      	END IF
 
		         ! slice_y and interp_y
          ELSE if( .not. slice_x .and. .not. interp_x) THEN
      	ib = 1
510   	call zget(zarray,islice,ib,num_x,num_y,val)
      	DO WHILE (( val*x0.le.0. .or. abs(val).lt.abs(x0) )
     1		 .and. ib.lt.num_y)
      	    ib = ib+1
      	    call zget(zarray,islice,ib,num_x,num_y,val)
      	END DO
 
      	IF (val*x0.gt.0. .and. abs(val).ge.abs(x0)) THEN
      	    ie = ib
      	    DO WHILE (val*x0.gt.0. .and. abs(val).ge.abs(x0)
     1		      .and. ie.lt.num_y)
      		ie = ie+1
      		call zget(zarray,islice,ie,num_x,num_y,val)
      	    END DO
      	    IF (.not.(val*x0.gt.0..and.abs(val).ge.abs(x0))) ie = ie-1
 
      	    IF (ib.eq.1 .and. ie.eq.num_y) THEN
      		write(6,*) 'Trying to delete whole slice!'
      		write(6,*) '***Not implemented and not defined***'
      		goto 100
	
      	    ELSE IF (ib .eq. 1) THEN
      		call zget(zarray,islice,ie+1,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE IF (ie .eq. num_y) THEN
      		call zget(zarray,islice,ib-1,num_x,num_y,val)
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
	
      	    ELSE
      		call zget(zarray,islice,ib-1,num_x,num_y,val0)
      		call zget(zarray,islice,ie+1,num_x,num_y,val1)
      		factor = (val1 - val0)/(yarray(ie+1) - yarray(ib-1))
 
      		DO i=ib,ie
      		    ndel = ndel+1
      		    idel(ndel) = islice
      		    jdel(ndel) = i
      		    call zget(zarray,islice,i,num_x,num_y,vdel(ndel))
      		    slice(i) = val0 + factor*( yarray(i)-yarray(ib-1))
      		    call zset(zarray,islice,i,num_x,num_y,slice(i))
      		END DO
      	    END IF
 
      	    ib = ie
      	    goto 510
      	END IF
 
          ELSE IF( slice_x) THEN	! and interp_y
      	DO ii=1,num_x
      	    call zget(zarray,ii,jslice,num_x,num_y,val)
      	    if( val*x0.gt.0. .and. abs(val).ge.abs(x0) ) then
      		ndel = ndel + 1
      		idel(ndel) = ii
      		jdel(ndel) = jslice
      		call zinterp(zarray,ii,jslice,xarray,yarray,
     1			  num_x,num_y,vdel(ndel),interp_x)
      		call zget(zarray,ii,jslice,num_x,num_y,slice(ii))
      	    endif
      	END DO
 
          ELSE			! slice_y and interp_x
      	DO ii=1,num_y
      	    call zget(zarray,islice,ii,num_x,num_y,val)
      	    if( val*x0.gt.0. .and. abs(val).ge.abs(x0) ) then
      		ndel = ndel + 1
      		idel(ndel) = islice
      		jdel(ndel) = ii
      		call zinterp(zarray,islice,ii,xarray,yarray,
     1			  num_x,num_y,vdel(ndel),interp_x)
      		call zget(zarray,islice,ii,num_x,num_y,slice(ii))
      	    endif
      	END DO
 
          END IF
 
          if( ndel .eq. ndel_o) then
      	write(6,*) 'No points in range!'
          endif
 
      ELSE IF (cmd .eq. 'W') THEN
          call ufnter(20,'OUTPUT:$ ')
          call ufcmpr(20,2)
          call ufopwr(20,ishot,ierr)
          if( ierr .ne. 0) then
      	write(6,*) 'Could not open output file'
      	goto 100
          endif
 
          call uf2dwr( 20, tdev,shdate,zarray,num_x,
     1     xarray,num_x,yarray,num_y,
     1     iproc, flab, xlab, ylab,
     2     nsc, scval, sclab, ierr)
 
          if( ierr .ne. 0) then
      	write(6,*) 'Could not write output data'
      	call ufclos(20)
      	goto 100
          endif
 
          call ufopcf(21,ier)
          if( ierr.ne.0) then
      	write(6,*) 'Could not write comment file'
 
          else
      	write(21,*) ' points deleted or interpolated by REPL2'
      	IF (ndel .eq. 0) THEN
      	    write(21,*) ' None!'
	
      	ELSE
      	    write(21,*) xlab(1:10),' ',ylab(1:10),' value'
      	    DO i=1,ndel
      		write(21,*) xarray(idel(i)),yarray(jdel(i)),vdel(i)
      	    END DO
      	END IF
 
 
      	call ucsend(21, 20)
 
      	call uspool(1,20)
          endif
 
          call ufclos(20)
          call ufclos(1)
 
          ishot = ishot + 1
          goto 20
      END IF
 
      goto 100
      END
 
!
c ********************************************************************
      subroutine zget( zarray, i,j, num_x,num_y, value)
      real zarray(num_x, num_y)
      value = zarray(i,j)
      return
      end
!
c ********************************************************************
      subroutine zset( zarray, i,j, num_x,num_y, value)
      real zarray(num_x, num_y)
      zarray(i,j) = value
      return
      end
!
c ********************************************************************
      subroutine zinterp( zarray, i,j, xarray, yarray, num_x,num_y,
     1		    value, x_interp)
      real zarray(num_x, num_y), xarray(num_x), yarray(num_y)
      logical x_interp
 
      value = zarray(i,j)
 
      if( x_interp ) then
          if( i .eq. 1) then
      	zarray(i,j) = zarray(i+1,j)
          else if( i .eq. num_x) then
      	zarray(i,j) = zarray(i-1,j)
          else
      	fac = (xarray(i)-xarray(i-1))/(xarray(i+1)-xarray(i-1))
      	zarray(i,j) = zarray(i-1,j) + fac*(zarray(i+1,j)-zarray(i-1,j))
          endif
      else
          if( j .eq. 1) then
      	zarray(i,j) = zarray(i,j+1)
          else if( j .eq. num_y) then
      	zarray(i,j) = zarray(i,j-1)
          else
      	fac = (yarray(j)-yarray(j-1))/(yarray(j+1)-yarray(j-1))
      	zarray(i,j) = zarray(i,j-1) + fac*(zarray(i,j+1)-zarray(i,j-1))
          endif
      endif
      return
      end
!
c ********************************************************************
      subroutine extr_x(work,nx,ny,x_i,f, time,islice)
 
      implicit none
      integer nx, ny
 
      real work(nx,ny)
      real x_i(nx), f(ny)
 
      integer count, i, j
      real time, dtave
      real factor
      integer islice
 
      IF ((time .le. x_i(1) .and. x_i(2) .gt. x_i(1)) .or.
     1   (time .ge. x_i(1) .and. x_i(2) .lt. x_i(1)) ) THEN
 
          do j=1, ny
      	f(j) = work(1,j)
          enddo
          islice = 1
	
      ELSE IF ((time .ge. x_i(nx) .and. x_i(nx-1) .lt. x_i(nx)) .or.
     1   (time .le. x_i(nx) .and. x_i(nx-1) .gt. x_i(nx)) ) THEN
 
          do j=1, ny
      	f(j) = work(nx,j)
          enddo
          islice = nx
	
      ELSE IF (x_i(nx) .gt. x_i(1) ) THEN
          i = 2
          DO WHILE (time .gt. x_i(i))
      	i = i+1
          END DO
 
          if( abs(time-x_i(i)) .gt. abs(time-x_i(i-1)) ) i = i-1
 
          do j=1, ny
      	f(j) = work(i,j)
          enddo
          islice = i
 
      ELSE
          i = 2
          DO WHILE (time .lt. x_i(i))
      	i = i+1
          END DO
 
          if( abs(time-x_i(i)) .gt. abs(time-x_i(i-1)) ) i = i-1
 
          do j=1, ny
      	f(j) = work(i,j)
          enddo
          islice = i
 
      END IF
 
      return
      end
!
c ********************************************************************
      subroutine extr_y(work,nx,ny,y_i,f, time, islice)
 
      implicit none
      integer nx, ny
 
      real work(nx,ny)
      real y_i(ny), f(nx)
 
      integer count, i, j
      real time, dtave
      real factor
      integer islice
 
 
      IF ((time .le. y_i(1) .and. y_i(2) .gt. y_i(1)) .or.
     1   (time .ge. y_i(1) .and. y_i(2) .lt. y_i(1)) ) THEN
 
          do j=1, nx
      	f(j) = work(j,1)
          enddo
          islice = 1
 
      ELSE IF ((time .ge. y_i(ny) .and. y_i(ny-1) .lt. y_i(ny)) .or.
     1   (time .le. y_i(ny) .and. y_i(ny-1) .gt. y_i(ny)) ) THEN
 
          do j=1, nx
      	f(j) = work(j,ny)
          enddo
          islice = ny
 
      ELSE IF (y_i(ny) .gt. y_i(1) ) THEN
          i = 2
          DO WHILE (time .gt. y_i(i))
      	i = i+1
          END DO
 
          if( abs(time-y_i(i)) .gt. abs(time-y_i(i-1)) ) i = i-1
 
          do j=1, nx
      	f(j) = work(j,i)
          enddo
          islice = i
 
      ELSE
          i = 2
          DO WHILE (time .lt. y_i(i))
      	i = i+1
          END DO
 
          if( abs(time-y_i(i)) .gt. abs(time-y_i(i-1)) ) i = i-1
 
          do j=1, nx
      	f(j) = work(j,i)
          enddo
          islice = i
 
      END IF
 
      return
      end
