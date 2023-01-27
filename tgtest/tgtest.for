      program tgtest
C
C  generate some plots -- trgraf display routines test program
C
      integer nsize,nsiz2
      parameter (nsize=1000)            ! data curves
      parameter (nsizy=300)             ! for f(x,y)
      parameter (nsiz2=30)              ! simulated data pts for fitting
C
      real zx(nsize),zcos(nsize),zsin(nsize),zsin2(nsize),zsin3(nsize)
      real zwk(nsize),zy(nsizy)
C
      real f2d(nsize,nsizy)
C
      real zxdata(nsiz2),zdata(nsiz2),zerx(nsiz2),zery(nsiz2)
C
      real rc(nsize,4),yc(nsize,4)      ! contour set
      real zz(4)
      integer iselec(4)
C
      character*60 mnlab(3)
      character*17 kylab(5)
      character*10 lbsel(15)
C
      logical lchar,lxout
C
      character*60 zlbl
C
      data zz/1.,2.,3.,4./
C
C-------------------
C
      call uinitg('TGTEST')
      luntrm=lnout(0)
C
      zlbl='tgtest:  trgraf test program'
C
      twopi=6.283185
C
      do ix=1,nsize
         zth=twopi*float(ix-1)/float(nsize-1)
         zx(ix)=zth
         zcos(ix)=cos(zth)
         zsin(ix)=sin(zth)
         zsin2(ix)=sin(2.0*zth)
         zsin3(ix)=sin(3.0*zth)
         do i=1,4
            rc(ix,i)=zcos(ix)
         enddo
         yc(ix,1)=zcos(ix)
         yc(ix,2)=zsin(ix)
         yc(ix,3)=zsin2(ix)
         yc(ix,4)=zsin3(ix)
      enddo
C
      do iy=1,nsizy
         zthy=twopi*float(iy-1)/float(nsizy-1)
         zy(iy)=zthy
         zcosy=cos(zthy)
         do ix=1,nsize
            zth=twopi*float(ix-1)/float(nsize-1)
            f2d(ix,iy)=zcosy*cos(2.0*zth)
         enddo
      enddo
C
      zfac=0.1
      do j=1,nsiz2
         zxdata(j)=twopi*float(j-1)/float(nsiz2-1)
         zth=zxdata(j)
         zth100=100.0*zth
         zdata(j)=sin(zth)+zfac*cos(zth100)
         zery(j)=zfac*(1.0+sin(zth100)**2)
         zerx(j)=zfac*(1.0+abs(sin(zth))/(0.25+abs(cos(zth))))
      enddo
C
 10   continue
C
      if(lxout(0)) write(luntrm,1001)
 1001 format(/
     >' tgtest test plot options:'/
     >'  1 -- basic f(x) plot'/
     >'  2 -- {f1(x),f2(x)} plot'/
     >'  3 -- {f1(x),f2(x),f3(x)} plot'/
     >'  4 -- {f1(x),f2(x),f3(x),f4(x)} plot'/
     >'  C -- plot of contour traces'/
     >'  F -- "data fit" plot'/
     >'  E -- "data fit" plot with error bars'/
     >'  G -- plot of f(x,y) data'/
     >'  Q -- quit')
      call uread('tgtest:  enter 1 character option code:$')
      if(lchar('Q')) then
         call uexita(0)
         stop
      else if(lchar('1')) then
C  f(x)
         call grafx1(zx,zcos,nsize,'x','y',zlbl,
     >      'basic f(x) plot','y=cos(x)')
      else if(lchar('2')) then
C  2 fcns
         call grafx2(zx,zcos,zsin,nsize,'x','y',zlbl,
     >      '{f1(x),f2(x)} plot','cos(x), sin(x)')
      else if(lchar('3')) then
C  3 fcns
         call grafx3(zx,zcos,zsin,zsin2,nsize,'x','y',zlbl,
     >      '{f1(x),f2(x),f3(x)} plot','cos(x), sin(x), sin(2*x)')
      else if(lchar('4')) then
C  4 fcns
         call grafx4(zx,zcos,zsin,zsin2,zsin3,nsize,'x','y',zlbl,
     >      '{f1(x),f2(x),f3(x),f4(x)} plot',
     >      'cos(x), sin(x), sin(2*x), sin(3*x)')
      else if(lchar('F')) then
C  "data fit"
         call grafxf(zx,zsin,nsize,zxdata,zdata,nsiz2,'x','y',zlbl,
     >      'f(x) as fit around data points','y=sin(x)')
      else if(lchar('E')) then
C  "data fit with error bars"
         call gscspc(1,3,-0.1,6.3)      ! set x scale
         call gscspc(2,3,-1.2,1.2)      ! set y scale
         call grafxe(zx,zsin,nsize,zxdata,zdata,nsiz2,
     >      nsiz2,zerx,nsiz2,zery,'x','y',zlbl,
     >      'f(x) as fit around data, with error bars','y=sin(x)')
         call gscspc(1,1,zdum,zdum)     ! restore x autoscaling
         call gscspc(2,1,zdum,zdum)     ! restore y autoscaling
      else if(lchar('G')) then
C  plot of f(x,y)
         call grf3f1(zx,f2d,zy,zwk,nsize,nsizy,nsize,
     >      'x (0 to 2pi)','cos(2x)cos(y)','y (0 to 2pi)',
     >      'x','f','y','tgtest display test program','grf3f1 test',0)
      else if(lchar('C')) then
C  contours plot
         do i=1,4
            iselec(i)=i
         enddo
C
         mnlab(1)='tgtest:  grfmm1 test plot'
         mnlab(2)='contours for x btw 0 and 2pi'
         mnlab(3)='(cos(x),cos(x)), (cos(x),sin(n*x)), n=1,2,3'
C
         kylab(1)='z label'
         kylab(2)='z units'
         kylab(3)='kylab(3)'
         kylab(4)='kylab(4)'
         kylab(5)='kylab(5)'
C
         lbsel(1)='contour 1'
         lbsel(2)='contour 2'
         lbsel(3)='contour 3'
         lbsel(4)='contour 4'
C
         call grfmm1(rc,yc,nsize,nsize,1,zz,4,iselec,4,
     >      'r','y',mnlab,kylab,lbsel)
      else
         call uermsg('%tgtest:  unrecognized menu option.',' ',0)
      endif
C
      go to 10
C
      end
