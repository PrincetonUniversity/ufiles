      PROGRAM REPL1
	
C
C PROGRAM DESCRIPTION:
C
C	 REPL1
c	    this program allow individual points to be replicated or
c	 interpolated in 1-d UFiles.  This is particularly usefull at
c	 the begining or end of beam pulses
C
C AUTHORS:
C
C	 MCZ
C
C CREATION DATE: 	11 Mar 88
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
 
      parameter (maxarray=20 000)
      real xarray(maxarray), yarray(maxarray), yerrs(maxarray)
      character*30 xlab, ylab, xlabdum, ylabdum, xunits, yunits
 
      character   cmd*1
	
      call uinitg("REPL1")
 
10    call ufnter(1,'INPUT:$ ')
      call ufnter(20,'OUTPUT:$ ')
	
20    call ureadi('Shot Number$',ishot)
 
30      call ufoprd(1,ishot,ier1)
        call uf1drd(1,tdev,shdate,xarray,yarray,maxarray,
     >	      npts_tot,iproc,xlab,ylab,nscmax,nsc,scval,sclab,ier2)
 
 
100   if( lxout(0) ) then
          write(6,*) ' '
 
          write(6,*) '[D] to change file names'
          write(6,*) '[S] to get new shot'
          write(6,*) '[P] to plot data'
          write(6,*) '[R] to remove data points'
          write(6,*) '[I] to interpolate new data points'
          write(6,*) '[W] to write output to file'
          write(6,*) ' '
          write(6,*) '[Q] to quit'
 
      endif
 
110   call ureadc('REPL1:  Enter choice:$',cmd)
      IF (cmd .eq. 'D') THEN
          call ufclos(1)
          goto 10
	
      ELSE IF (cmd .eq. 'S') THEN
          call ufclos(1)
          goto 20
	
      ELSE IF (cmd .eq. 'Q') THEN
          stop
	
      ELSE IF (cmd .eq. 'P') THEN
          call grfuf1(xarray,yarray,npts_tot,
     >		  iproc,ylab,xlab,ishot,tdev,shdate,0,scval(1)
     >		  ,sclab(1,1))
	
      ELSE IF (cmd .eq. 'R') THEN
          call ureadf('Lower bound of time range to delete$',x0)
          call ureadf('Upper bound of time range to delete$',x1)
 
          IF (x1 .lt. x0) goto 100
 
          call ureadc(
     >       'Do you REALLY want to delete this data [Y/N]??$',cmd)
          IF (cmd .eq. 'Y') THEN
      	i = 1
      	DO WHILE (i .le. npts_tot)
      	    IF (xarray(i).ge.x0 .and. xarray(i).le.x1) THEN
      		DO j=i+1,npts_tot
      		    xarray(j-1) = xarray(j)
      		    yarray(j-1) = yarray(j)
      		END DO
 
      		npts_tot = npts_tot - 1
      	    ELSE
      		i = i + 1
      	    END IF
      	END DO
          END IF
	
      ELSE IF (cmd .eq. 'I') THEN
          call ureadf('Time of new point to interpolate$',x2)
          x0 = xarray(1)
          x1 = xarray(1)
          j0 = 1
          j1 = 1
 
          DO i=1,npts_tot
      	IF (xarray(i) .gt. x2) THEN
      	    IF (xarray(i).lt. x1 .or. x1 .lt. x2) THEN
      		x1 = xarray(i)
      		j1 = i
      	    END IF
	
      	ELSE IF (xarray(i) .lt. x2) THEN
      	    IF (xarray(i).gt. x0 .or. x0 .gt. x2) THEN
      		x0 = xarray(i)
      		j0 = i
      	    END IF
	
      	ELSE
      	    write(6,*) 'There is already a point at this time!!'
      	    goto 100
      	END IF
          END DO
 
          y0 = yarray(j0)
          y1 = yarray(j1)
 
          IF (x0 .gt. x2) THEN
      	j0 = 0
      	x0 = 0.
      	y0 = 0.
          END IF
 
          IF (x1 .lt. x2) THEN
      	j1 = npts_tot
      	x0 = 0.
      	y0 = 0.
          END IF
 
          write(6,*) 'Neighboring points:'
          write(6,*) 'f1 ',y0,' at ',x0
          write(6,*) 'f2 ',y1,' at ',x1
          write(6,*) ' '
          write(6,*) 'New value at',x2,'will be (1-A)*f1 + A*f2'
          write(6,*) 'Increased A => more weight on f2'
          write(6,*)
     1       'ratio of time locations (to give linear interpolation:',
     1	    (x2-x0)/(x1-x0)
          write(6,*) ' '
          call ureadf('Input value of A$',factor)
          y2 = (1.-factor)*y0 + factor*y1
 
          DO i=npts_tot, j0+1, -1
      	xarray(i+1) = xarray(i)
      	yarray(i+1) = yarray(i)
          END DO
 
          xarray(j0+1) = x2
          yarray(j0+1) = y2
          npts_tot = npts_tot + 1
	
      ELSE IF (cmd .eq. 'W') THEN
          call ufcmpr(20,2)
          call ufopwr(20,ishot,ierr)
          if( ierr .ne. 0) then
      	write(6,*) 'Could not open output file'
      	goto 100
          endif
 
          call uf1dwr( 20, tdev,shdate,xarray,yarray,npts_tot,
     1	iproc, xlab, ylab, nsc, scval, sclab, ierr)
 
          if( ierr .ne. 0) then
      	write(6,*) 'Could not write output data'
      	call ufclos(20)
      	goto 100
          endif
 
          call ufopcf(21,ier)
          if( ierr.ne.0) then
      	write(6,*) 'Could not write comment file'
 
          else
      	write(21,*) ' points deleted or interpolated by REPL1'
 
 
      	call ucsend(21, 20)
 
c      		call ufoprd(1,ishot,ier1)
      	call uspool(1,20)
          endif
 
          call ufclos(1)
          call ufclos(20)
          goto 30
      END IF
 
      goto 100
      END
