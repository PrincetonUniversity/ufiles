c	general purpose 2-d extractor.  Will extract a profile
c	at a specified time (with an averaging window) from a 2-D UFile
c
c  ** UFILE HAS BEEN OPENED BY CALLER **
c
c	M.C. Zarnstorff		Jan. 1985
c ************************************************************************
      subroutine uf1xtr(
     1	LUN,	! log. unit number to UFile
     2	tdev,	! tokamak name
     3	shdate,	! date
     5	time,	! time to extract at
     6	dtave,	! delta time to average over
     4	x,	! array to contain independent variable of data
     5	f,	! array to contain dependent variable ( data )
     6	nmax,	! size of X and F arrays.
     7	n,	! number of points of function read in
     8	iproc,	! process code
     9	xlab,	! label for X axis
     1	flab,	! label for function
     1	nscmax,	! max number of scalers
     2	nsc,	! actual number of scalers read
     3	scval,	! scaler values
     4	sclab,	! scaler labels
     7	ier)	! returned error code
 
      implicit none
 
      integer max_x
      parameter (max_x = 16384)
 
      integer iproc, ier, nsc, nscmax, n, nmax, lun
      character*10 shdate
      character*4 tdev
 
      real time, dtave
 
      real scval(nscmax)
      real x(nmax), f(nmax)
      character*10	sclab(3,nscmax)
      character*30    xlab, flab
 
      real work(40*max_x)
      real x_i(max_x), y_i(max_x)
      character*30	ylab
      integer nx, ny
      integer i,j

c$r8real_input: time,dtave
c$r8real_output: x,f,scval
 
c	go get the raw data
      call uf2drd(lun, tdev, shdate, work, 40, x_i, max_x, y_i, max_x,
     1	iproc, nx, ny, 1, flab, xlab, ylab,
     2	nscmax, nsc, scval, sclab, ier)
 
      if( ier .eq. 0) then
 
        do i=1, nmax
      	f(i) = 0.0
        enddo
 
        if( xlab(1:4) .eq. 'TIME' .or. xlab(1:4) .eq. 'time') then
          xlab = ylab
          n = ny
 
          if( n .gt. nmax) then
      	ier = 1
 
          else
      	do i=1, n
      	  x(i) = y_i(i)
      	enddo
 
              call extr_x(work,nx,ny,x_i,f, time, dtave)
          endif
        else if( ylab(1:4) .eq. 'TIME' .or. ylab(1:4) .eq. 'time')then
          n = nx
 
          if( n .gt. nmax) then
      	ier = 1
 
          else
      	do i=1, n
      	  x(i) = x_i(i)
      	enddo
 
              call extr_y(work,nx,ny,y_i,f, time, dtave)
          endif
 
        else
          ier = 1
 
        endif
 
      endif
 
      return
      end
c ********************************************************************
      subroutine extr_x(work,nx,ny,x_i,f, time, dtave)
 
      implicit none
      integer nx, ny
 
      real work(nx,ny)
      real x_i(nx), f(ny)
 
      integer count, i, j
      real time, dtave

c$r8real_input: work,x_i,time,dtave
c$r8real_output: f 
 
      count = 0
 
      do j=1, ny
        f(j) = 0.0
      enddo
 
      do i=1, nx
        if(  abs(x_i(i) - time) .le. dtave ) then
          count = count + 1
 
          do j=1, ny
             f(j) = f(j) + work(i,j)
          enddo
        endif
 
      enddo
 
      if( count .ne. 0) then
        do j=1, ny
          f(j) = f(j)/count
        enddo
      endif
 
      return
      end
c ********************************************************************
      subroutine extr_y(work,nx,ny,y_i,f, time, dtave)
 
      implicit none
      integer nx, ny
 
      real work(nx,ny)
      real y_i(ny), f(nx)
 
      integer count, i, j
      real time, dtave
 
c$r8real_input: work,y_i,time,dtave
c$r8real_output: f 
 
      count = 0
 
      do j=1, nx
        f(j) = 0.0
      enddo
 
      do i=1, ny
        if(  abs(y_i(i) - time) .le. dtave ) then
          count = count + 1
 
          do j=1, nx
             f(j) = f(j) + work(j,i)
          enddo
        endif
 
      enddo
 
      if( count .ne. 0) then
        do j=1, nx
          f(j) = f(j)/count
        enddo
      endif
 
      return
      end
