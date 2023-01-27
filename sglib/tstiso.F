c   test isometric routines
	program tstiso
	character*1 back
c
	isom=0
	call at_bint('Number of som (no contour) plots',isom,back)
	do ii=1,isom
	  call somplot
	enddo
c
	isoc=0
	call at_bint('Number of soc (contour) plots',isoc,back)
	do ii=1,isoc
	  call socplot
	enddo
c
	end
c
	subroutine somplot
c
	parameter (nxp=10)
	parameter (nyp=10)
	real xdata(nxp)
	real ydata(nyp)
	real zdata(nxp,nyp),zbase(nxp,nyp)
c
	real xmin, xmax 
	real zmin, zmax	!zmin><0
	real zscale
	integer linc
	character*12 xuns	
	character*12 yuns	
	character*12 zuns
	data xmin /10./, xmax /100./
	data zmin /-20.0/, zmax /60.0/	!zmin><0
	data zscale /1./
	data linc /1/
	data xuns	/'X data$'/
	data yuns	/'Y data$'/
	data zuns	/'Z data$'/
	logical zlog, lhide
	character*1 back
	data (xdata(i),i=1,nxp) /10.,20.,30.,40.,50.,60.,70.,80.,90.,100./
	data ((zbase(i,j),i=1,nxp),j=1,nyp) /
     >		60.,40.,20.,00.,-20.,-20.,00.,20.,40.,60.,
     >		60.,40.,20.,00.,-20.,-20.,00.,20.,40.,60.,
     >		62.,42.,22.,02.,-18.,-18.,02.,22.,42.,62.,
     >		64.,44.,24.,04.,-16.,-16.,04.,24.,44.,64.,
     >		66.,46.,26.,06.,-14.,-14.,06.,26.,46.,66.,
     >		66.,46.,26.,06.,-14.,-14.,06.,26.,46.,66.,
     >		64.,44.,24.,04.,-16.,-16.,04.,24.,44.,64.,
     >		62.,42.,22.,02.,-18.,-18.,02.,22.,42.,62.,
     >		60.,40.,20.,00.,-20.,-20.,00.,20.,40.,60.,
     >		60.,40.,20.,00.,-20.,-20.,00.,20.,40.,60./
	yfact=1.
	call at_breal('Y axis scale',yfact,back)
	do jj=1,nxp		!remote exponent on y axis label
	  ydata(jj) = jj *.5 * abs(yfact)
	enddo
	call at_breal('Z scale',zscale,back)
	zmin = 0.
	zmax = 0.
	do jj=1,nyp
	  do kk=1,nxp
	    zdata(kk,jj) = zbase(kk,jj)*zscale
	    zmin = min(zmin,zdata(kk,jj))
	    zmax = max(zmax,zdata(kk,jj))
	  enddo
	enddo
	zlog=.false.
	call at_log ('Z axis as log',zlog)
	if(zlog .and. zmin.lt.0) zmin = 1.
	lhide=.true.
	call at_log('Remove hidden lines',lhide)
	call at_int('Line color',linc)
	theta=45.
	call at_real('Y angle',theta)
	call initt(120)
	call aginit
	if(linc.ne.1) call agccurv(linc)
c
	call agidlx(xmin,xmax)
cx	call agidly(ymin,ymax)
	call agidlz(zmin,zmax)
	call agixnt(0)
	call agiynt(0)
	call agiznt(0)
	if(theta.gt.90) call agislx(430,870)
	call agisly(theta,0)
	call agiskt(.false.,.true.)
	call agihid(lhide)
	if(zlog) call agizty(2)
c
	call agisom(xdata,ydata,zdata,nxp,nxp,nyp,
     >		xuns,yuns,zuns)
	call upause(0)
	call finitt(0,0)
	end
c
	subroutine socplot
c
	parameter (nxp=10)
	parameter (nyp=10)
	real xdata(nxp)
	real ydata(nyp)
	real zdata(nxp,nyp),zbase(nxp,nyp)
	real c(nyp), work(2*nxp*nyp)	!only for agisoc
	logical lhide
	real xmin, xmax
	real zmin, zmax
	real zscale
	integer linc
	character*12 xuns	
	character*12 yuns	
	character*12 zuns	

	data xmin /10./, xmax /100./
	data zmin /-60.0/, zmax /20.0/	!zmin><0
	data zscale /1./
	data linc /1/
	data xuns	/'X data$'/
	data yuns	/'Y data$'/
	data zuns	/'Z data$'/
	data (xdata(i),i=1,nxp) /10.,20.,30.,40.,50.,60.,70.,80.,90.,100./
	data ((zbase(i,j),i=1,nxp),j=1,nyp) /
     >		20.,00.,-20.,-40.,-60.,-60.,-40.,-20.,00.,20.,
     >		20.,00.,-20.,-40.,-60.,-60.,-40.,-20.,00.,20.,
     >		22.,02.,-18.,-38.,-58.,-58.,-38.,-18.,02.,22.,
     >		24.,04.,-16.,-36.,-56.,-56.,-36.,-16.,04.,24.,
     >		26.,06.,-14.,-34.,-54.,-54.,-34.,-14.,06.,26.,
     >		26.,06.,-14.,-34.,-54.,-54.,-34.,-14.,06.,26.,
     >		24.,04.,-16.,-36.,-56.,-56.,-36.,-16.,04.,24.,
     >		22.,02.,-18.,-38.,-58.,-58.,-38.,-18.,02.,22.,
     >		20.,00.,-20.,-40.,-60.,-60.,-40.,-20.,00.,20.,
     >		20.,00.,-20.,-40.,-60.,-60.,-40.,-20.,00.,20./
c
	do jj=1,nxp
	  ydata(jj) = jj *.5
	enddo
	do jj=1,nyp
	  do kk=1,nxp
	    zdata(kk,jj) = zbase(kk,jj)*zscale
	  enddo
	enddo
	lhide=.true.
	call at_log('Remove hidden lines',lhide)
	nc=-1
	call at_int('Contours (-1 for uniform, 0 for none)',nc)
	call at_int('Line color',linc)
	theta=45.
	call at_real('Y angle',theta)
	call initt(120)
	call aginit
	if(linc.ne.1) call agccurv(linc)
c
	call agidlx(xmin,xmax)
cx	call agidly(ymin,ymax)
	call agidlz(zmin,zmax)
	call agixnt(0)
	call agiynt(0)
	call agiznt(0)
	call agiskt(.false.,.true.)
	if(theta.gt.90) call agislx(430,870)
	call agisly(theta,0)
	call agihid(lhide)
	call agiszc(nc,c)
c
	call agisoc(xdata,ydata,zdata,nxp,nxp,nyp,
     >		xuns,yuns,zuns,work)
	call upause(0)
	call finitt(0,0)
	end
