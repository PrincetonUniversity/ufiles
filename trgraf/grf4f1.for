      subroutine grf4f1(f,nf1,nf2, x,nx, y,ny, z,nz,
     >   flab, xlab,ylab,zlab, funs, xuns,yuns,zuns,
     >   ztitle, iproc)
c
c  dmc 12 Aug 2000 -- trgraf "slicer" for f(x,y,z) gridded data
c
c  input:
      real f(nf1,nf2,nz)                ! the data
      real x(nx)                        ! x grid (1st dim.), nx.le.nf1
      real y(ny)                        ! y grid (2nd dim.), ny.le.nf2
      real z(nz)                        ! z grid (3rd dim.)
c
      character*(*) flab                ! data label (recommend <~20 chars)
      character*(*) xlab                ! x label (recommend <~10 chars)
      character*(*) ylab                ! y label (recommend <~10 chars)
      character*(*) zlab                ! z label (recommend <~10 chars)
c
      character*(*) funs                ! data units (recommend <~10 chars)
      character*(*) xuns                ! x units (recommend <~10 chars)
      character*(*) yuns                ! y units (recommend <~10 chars)
      character*(*) zuns                ! z units (recommend <~10 chars)
c
c 40 characters or so:
      character*(*) ztitle              ! general title label for all plots
c
c for Ufiles/grf3 compatibility.  if in doubt set iproc=0
c
      integer iproc               !0=NOTHING, 1=AVG'D, 2=SMOOTHED, ETC...
c
c output:
c
c  **sglib tekvec plots, via grf3f1**
c
c this routine asks the user to fix one coordinate, specify the order
c of the other two cordinates, and then calls grf3f1 to plot the resulting
c 2d slice-- usual menu of 1d slices, contour, isometric, ...
c
c automatic:
c
      real wk(max(nx,ny,nz))
c
      real, dimension(:), allocatable :: x1,x2
      real, dimension(:,:), allocatable :: fslice
c
      character*50 x1lab,x2lab,x1uns,x2uns,zlabel
c
      integer luntrm
      logical lxout,lchar
c-----------------------------
c
      luntrm=lnout(0)
c
      xmin=x(1)
      xmax=x(1)
c
      ymin=y(1)
      ymax=y(1)
c
      zmin=z(1)
      zmax=z(1)
c
      do i=1,nx
         xmin=min(xmin,x(i))
         xmax=max(xmax,x(i))
      enddo
c
      do i=1,ny
         ymin=min(ymin,y(i))
         ymax=max(ymax,y(i))
      enddo
c
      do i=1,nz
         zmin=min(zmin,z(i))
         zmax=max(zmax,z(i))
      enddo
c
 10   continue
c
      if(lxout(0)) then
         write(luntrm,*) ' grf4f1:  f(x,y,z) data:  ',flab,' ',funs
c
         write(luntrm,*) ' '
         write(luntrm,*) ' x axis:  ',nx,' points:  ',xlab,' ',xuns
         write(luntrm,*) '     xmin = ',xmin,' xmax = ',xmax
c
         write(luntrm,*) ' '
         write(luntrm,*) ' y axis:  ',ny,' points:  ',ylab,' ',yuns
         write(luntrm,*) '     ymin = ',ymin,' ymax = ',ymax
c
         write(luntrm,*) ' '
         write(luntrm,*) ' z axis:  ',nz,' points:  ',zlab,' ',zuns
         write(luntrm,*) '     zmin = ',zmin,' zmax = ',zmax
c
         write(luntrm,*) ' '
c
         write(luntrm,1001)
 1001    format(' grf4f1 options:'/
     >      ' enter "0" or "q" to quit, or:'//
     >      '   +1  for f(x,y) @fixed z'/
     >      '   -1  for f(y,x) @fixed z'//
     >      '   +2  for f(x,z) @fixed y'/
     >      '   -2  for f(z,x) @fixed y'//
     >      '   +3  for f(y,z) @fixed x'/
     >      '   -3  for f(z,y) @fixed x'/)
      endif
c
      call uread('grf4f1:  option code:$')
      if(lchar('Q')) return
      if(lchar('0')) return
c
      ians=idcod(iert)
      if(iert.ne.0) go to 10
      if(ians.eq.0) return
      if(abs(ians).gt.3) then
         call uermsi('?grf4f1:  invalid option:  ',ians,0)
         go to 10
      endif
c
      if(abs(ians).eq.1) then
         call grf4f1_fix('z axis',zlab,zuns,z,nz,izfix,zlabel)
         if(izfix.eq.0) go to 10
      else if(abs(ians).eq.2) then
         call grf4f1_fix('y axis',ylab,yuns,y,ny,iyfix,zlabel)
         if(iyfix.eq.0) go to 10
      else
         call grf4f1_fix('x axis',xlab,xuns,x,nx,ixfix,zlabel)
         if(ixfix.eq.0) go to 10
      endif
c
c  z fixed...
      if(ians.eq.1) then
         allocate (x1(nx))
         nx1=nx
         x1=x
         x1lab=xlab
         x1uns=xuns
c
         allocate (x2(ny))
         nx2=ny
         x2=y
         x2lab=ylab
         x2uns=yuns
c
         allocate (fslice(nx,ny))
         fslice=f(1:nx,1:ny,izfix)
      endif
c
      if(ians.eq.-1) then
         allocate (x1(ny))
         nx1=ny
         x1=y
         x1lab=ylab
         x1uns=yuns
c
         allocate (x2(nx))
         nx2=nx
         x2=x
         x2lab=xlab
         x2uns=xuns
c
         allocate (fslice(ny,nx))
         do i=1,ny
            fslice(i,1:nx)=f(1:nx,i,izfix)
         enddo
      endif
c
c  y fixed...
      if(ians.eq.2) then
         allocate (x1(nx))
         nx1=nx
         x1=x
         x1lab=xlab
         x1uns=xuns
c
         allocate (x2(nz))
         nx2=nz
         x2=z
         x2lab=zlab
         x2uns=zuns
c
         allocate (fslice(nx,nz))
         fslice=f(1:nx,iyfix,1:nz)
      endif
c
      if(ians.eq.-2) then
         allocate (x1(nz))
         nx1=nz
         x1=z
         x1lab=zlab
         x1uns=zuns
c
         allocate (x2(nx))
         nx2=nx
         x2=x
         x2lab=xlab
         x2uns=xuns
c
         allocate (fslice(nz,nx))
         do i=1,nz
            fslice(i,1:nx)=f(1:nx,iyfix,i)
         enddo
      endif
c
c  x fixed...
      if(ians.eq.3) then
         allocate (x1(ny))
         nx1=ny
         x1=y
         x1lab=ylab
         x1uns=yuns
c
         allocate (x2(nz))
         nx2=nz
         x2=z
         x2lab=zlab
         x2uns=zuns
c
         allocate (fslice(ny,nz))
         fslice=f(ixfix,1:ny,1:nz)
      endif
c
      if(ians.eq.-3) then
         allocate (x1(nz))
         nx1=nz
         x1=z
         x1lab=zlab
         x1uns=zuns
c
         allocate (x2(ny))
         nx2=ny
         x2=y
         x2lab=ylab
         x2uns=yuns
c
         allocate (fslice(nz,ny))
         do i=1,nz
            fslice(i,1:ny)=f(ixfix,1:ny,i)
         enddo
      endif
c
c  make plots
c
      call grf3f1(x1,fslice,x2,wk,nx1,nx2,nx1,
     >   x1lab,flab,x2lab,x1uns,funs,x2uns,
     >   ztitle,zlabel,iproc)
c
c  free arrays
c
      deallocate(fslice)
      deallocate(x1)
      deallocate(x2)
c
      go to 10
      end
c------------------------------------------
      subroutine grf4f1_fix(slbl,xlbl,xuns,x,nx,ixfix,zlabel)
c
c  choose a fixed value for the given axis
c
c  input:
      character*(*) slbl                ! short label
      character*(*) xlbl                ! axis label
      character*(*) xuns                ! axis units
      real x(nx)                        ! the axis
c
c  output:
      integer ixfix                     ! the selected value (or 0 to bail)
      character*(*) zlabel              ! corresponding label
c
c----------------
      logical lxout,lchar
c----------------
c
      zlabel=' '
      ixfix=0
c
      luntrm=lnout(0)
c
      if(lxout(0)) then
         write(luntrm,*) 'choosing fixed value for ',slbl,'.'
         write(luntrm,*) 'choose fixed value for axis:  ',xlbl,' ',xuns
         write(luntrm,*) 'nearest available point will be chosen.'
      endif
      call uread('grf4f1:  choose fixed value or "H" or "Q":$')
      if(lchar('H')) return             ! retype min/max info
      if(lchar('Q')) return
c
      zanswer=fdcod(iert)
      if(iert.ne.0) return
c
      imin=1
      ztest=abs(zanswer-x(imin))
      do i=2,nx
         ztesnew=abs(zanswer-x(i))
         if(ztesnew.lt.ztest) then
            ztest=ztesnew
            imin=i
         endif
      enddo
c
      write(luntrm,*) 'nearest point:  index=',imin,' value=',x(imin)
      ixfix=imin
c
      ilu=len_trim(xuns)
      ilb=len_trim(xlbl)
      if((ilu+ilb+16).gt.len(zlabel)) then
         write(zlabel,
     >      '(''fixed '',a,'' value:  '',1pe12.5)') trim(slbl),x(imin)
      else
         write(zlabel,'(a,'' = '',1pe12.5,'' '',a)') trim(xlbl),
     >        x(imin),trim(xuns)
      endif
c
      return
      end
