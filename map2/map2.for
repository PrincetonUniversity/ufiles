      program map2
c
c	Dimension Statements for the UFILES programs
c
      parameter (max_x=16384)
      parameter (max_y=128)
      integer iwork(max_x,2)
      real xarray(max_x), yarray(max_y), grid(max_x), work(max_x,2)
      real zarray(max_x, max_y)
      character*30	flab, xlab, ylab, glab
 
      parameter (nscmax=50)
      real scval(nscmax)
      character*4 tdev
      character*10 shdate
      character*10 sclab(3,nscmax)
      character*1 ans, ans2
 
 
      yscale1 = 1.
      yscale2 = 1.
 
      call uinita(0)
 
      write(6,*)
     >   'This program reads two 2d UFiles, and maps either dimension'
      write(6,*) 'of the second "data" file to be on the same grid '
      write(6,*)
     >   'as the corresponding dimension of the first "grid" file'
      write(6,*) ' '
      write(6,*)
     >   'This program requires that the first dimension of the file'
      write(6,*) 'be the "long" dimension'
      write(6,*) ' '
 
10    call ureadi('Shot number for grid file (<=0 to stop)',ishot)
      if(ishot .le. 0) stop
 
      call ufnter(1,'INPUT: grid file')
      	call ufoprd(1,ishot,ierr)
      if( ierr .eq. 0) then
          nmax_x = max_x
          call uf2drd( 1, tdev,shdate,
     1       zarray,nmax_x,xarray,nmax_x,grid,max_y,
     1       iproc, num_x, num_grid, 0, flab, xlab, glab,
     2       nscmax, nsc, scval, sclab, ierr)
          call ufclos(1)
      endif
      if( ierr .ne. 0) then
          write(6,*) 'Error reading Grid input file'
          goto 10
      endif
 
      write(6,*) 'x-axis: ',xlab
      write(6,*) 'y-axis: ',ylab
      write(6,*) ' '
15    call ureadw('Choose axis to map to (X/Y)', ans2)
 
      IF (ans2 .eq. 'X') THEN
          num_grid = num_x
          DO i=1,num_grid
      	grid(i) = xarray(i)
          END DO
 
      ELSE IF( ans2 .ne. 'Y') THEN
          goto 15
      END IF
 
c	check to make sure grid is ordered correctly
      IF (grid(num_grid) .lt. grid(1) ) THEN
          DO i=1,num_grid/2			! flip it
      	tmp = grid(i)
      	grid(i) = grid(num_grid + 1 - i)
      	grid(num_grid + 1 - i) = tmp
          END DO
      END IF
 
      call ureadf('Scale factor for grid',yscale1)
 
      if( yscale1 .ne. 0. .and. yscale1 .ne. 1.) then
          DO i=1,num_grid
      	grid(i) = grid(i)*yscale1
          END DO
      endif
 
	
20    call ureadi('Shot number for data file',ishot)
      if(ishot .le. 0) goto 10
 
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
          goto 20
      endif
 
      call ureadf('Scale factor for data y-axis',yscale2)
 
      if( yscale2 .ne. 0. .and. yscale2 .ne. 1.) then
          DO i=1,num_y
      	yarray(i) = yarray(i)*yscale2
          END DO
      endif
 
      IF (ans2 .eq. 'Y') THEN
          write(6,*) 'Data y varies from',yarray(1),' to ',yarray(num_y)
          write(6,*) 'Grid y varies from',grid(1),' to ',grid(num_grid)
 
          IF (yarray(num_y).lt.grid(1) .or.
     >        yarray(1).gt.grid(num_grid)) THEN
      	write(6,*) 'No overlap !!'
      	goto 10
          END IF
 
 
          IF (yarray(1).lt.grid(1) .or.
     >        yarray(num_y).gt.grid(num_grid)) THEN
      	call ureadw('Expand grid to include excess (Y/N)',ans)
      	IF (ans .eq. 'Y' .and. yarray(1).lt.grid(1)) THEN
      	    j = 1
      	    DO WHILE (yarray(j+1) .lt. grid(1))
      		j = j+1
      	    END DO
 
      	    DO i=num_grid,1,-1
      		grid(i+j) = grid(i)
      	    END DO
 
      	    DO i=1,j
      		grid(i) = yarray(i)
      	    END DO
 
      	    num_grid = num_grid+j
      	END IF
 
      	IF (ans .eq. 'Y' .and. yarray(num_y).gt.grid(num_grid)) THEN
      	    j = 1
      	    DO WHILE (yarray(num_y-j) .gt. grid(num_grid))
      		j = j+1
      	    END DO
 
      	    DO i=1,j
      		grid(num_grid+i) = yarray(num_y-j+i)
      	    END DO
 
 
      	    num_grid = num_grid + j
      	END IF
          END IF
 
 
          nmax_x = max_x
          call map_2dy(zarray,nmax_x,max_y,xarray,yarray,num_x,num_y,
     1		grid,num_grid,work,iwork)
	
      ELSE
          write(6,*) 'Data x varies from',xarray(1),' to ',xarray(num_x)
          write(6,*) 'Grid x varies from',grid(1),' to ',grid(num_grid)
 
          IF (xarray(num_x).lt.grid(1) .or.
     >        xarray(1).gt.grid(num_grid)) THEN
      	write(6,*) 'No overlap !!'
      	goto 10
          END IF
 
 
          IF (xarray(1).lt.grid(1) .or.
     >        xarray(num_x).gt.grid(num_grid)) THEN
      	call ureadw('Expand grid to include excess (Y/N)',ans)
      	IF (ans .eq. 'Y' .and. xarray(1).lt.grid(1)) THEN
      	    j = 1
      	    DO WHILE (xarray(j+1) .lt. grid(1))
      		j = j+1
      	    END DO
 
      	    DO i=num_grid,1,-1
      		grid(i+j) = grid(i)
      	    END DO
 
      	    DO i=1,j
      		grid(i) = xarray(i)
      	    END DO
 
      	    num_grid = num_grid+j
      	END IF
 
      	IF (ans .eq. 'Y' .and. xarray(num_x).gt.grid(num_grid)) THEN
      	    j = 1
      	    DO WHILE (xarray(num_x-j) .gt. grid(num_grid))
      		j = j+1
      	    END DO
 
      	    DO i=1,j
      		grid(num_grid+i) = xarray(num_x-j+i)
      	    END DO
 
 
      	    num_grid = num_grid + j
      	END IF
          END IF
 
 
          nmax_x = max_x
          call map_2dx(zarray,nmax_x,max_y,xarray,yarray,num_x,num_y,
     1		grid,num_grid,work,iwork)
 
      END IF
 
30    call ufnter(3,'OUTPUT: file')
      call ufcmpr(3,2)
      	call ufopwr(3,ishot,ierr)
      if( ierr .ne. 0) goto 30
 
      if( ans2 .eq. 'Y') then
          nmax_x = max_x
          call uf2dwr(3,tdev,shdate,
     1       zarray,nmax_x,xarray,num_x,grid,num_grid,
     1       iproc, flab, xlab, glab,
     2       nsc, scval, sclab, ierr)
 
      else
          nmax_x = max_x
          call uf2dwr(3,tdev,shdate,zarray,
     1       nmax_x,grid,num_grid,yarray,num_y,
     1       iproc, flab, xlab, glab,
     2       nsc, scval, sclab, ierr)
      endif
 
      call uspool( 2, 3)
      	call ufclos(3)
      if( ierr .ne. 0) goto 30
 
      	call ufclos(2)
      goto 10
      end
!
c ============================================================================
      subroutine map_2dy(zarray,max_x,max_y,xarray,yarray,num_x,num_y,
     1	    y_grid,num_grid,w,iw)
 
      real zarray(max_x,max_y), xarray(num_x), yarray(num_y)
      real y_grid(num_grid)
 
      real  w(max_y,2)
      integer iw(max_y,2)
 
c	set up the coefficients and indexes
 
      i = 1
      DO j = 1, num_y
        DO WHILE (y_grid(i) .lt. yarray(j))
          IF (j .eq. 1) THEN
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
 
c	do all the work into a temp. holding place, then put it away
 
20    DO k=1,num_x
          DO i=1,num_grid
            w(i,2) = w(i,1)*zarray(k,iw(i,2)) +
     >               (1.-w(i,1))*zarray(k,iw(i,1))
          END DO
 
          DO i=1,num_grid
            zarray(k,i) = w(i,2)
          END DO
      END DO
 
      RETURN
      END
!
c ============================================================================
      subroutine map_2dx(zarray,max_x,max_y,xarray,yarray,num_x,num_y,
     1	    x_grid,num_grid,w,iw)
 
      real zarray(max_x,max_y), xarray(num_x), yarray(num_y)
      real x_grid(num_grid)
 
      real  w(max_x,2)
      integer iw(max_x,2)
 
c	set up the coefficients and indexes
 
      i = 1
      DO j = 1, num_x
        DO WHILE (x_grid(i) .lt. xarray(j))
          IF (j .eq. 1) THEN
      	w(i,1) = 1.
      	iw(i,1) = j
      	iw(i,2) = j
	
          ELSE
      	w(i,1) = (x_grid(i)-xarray(j-1))/(xarray(j)-xarray(j-1))
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
      	iw(ii,1) = num_x
      	iw(ii,2) = num_x
          END DO
      END IF
 
c	do all the work into a temp. holding place, then put it away
 
20    DO k=1,num_y
          DO i=1,num_grid
            w(i,2) = w(i,1)*zarray(iw(i,2),k) +
     >               (1.-w(i,1))*zarray(iw(i,1),k)
          END DO
 
          DO i=1,num_grid
            zarray(i,k) = w(i,2)
          END DO
      END DO
 
      RETURN
      END
