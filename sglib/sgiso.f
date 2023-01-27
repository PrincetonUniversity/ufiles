c file: sgiso.for	isometric routines for sglib		31-mar-82
c
c
c  mar-82	modify agilya to handle character or numeric strings.
c		change many routines to permit hidden line removal
c		 in logarithmic plots. [j-murphy].
c
c  mar-86	revise y tick labeling to prevent blowups [met]
c
c  nov-87	modify y tick label routine to support 4105; change agicrv
c		 to move, not draw, to first point.  [met]
c		fix agiso0 so tick labels are along y axis, not at z=0.
c		 y axis rather than at z=0.
c		restructure many if's and do's to f77. [met]
c
c  feb-89	always draw in agidvs if idraw<0 to match setting in agishc.
c		set curve color in agishc for 4105.
c		test for hidden in agifrm; delete agifrh (frame hidden).
c		use grid color for frame.
c		don't mark upper/lower for skirt, frame, or contour lines.
c		try nint function in agidvs to calculate my.
c		in agish, if x is dense, draw constant y lines then
c		 call agidvs to mark upper/lower.
c		convert label arguments to character variables.
c		add agimsu/agidsu entries for 4096 resolution; fix frame/
c		 surface/contour for 4096; sort of fix y axis routines.
c		make all setup routines entries of aginit.
c		calculate klengy in agisly if argument = 0.
c		more f77 changes. [met]
c
c  sep-89	update agidvs from newest draws in ncar library -
c		 use ltemp to set upper/lower.
c		don't use nint to calculate my - didn't help.
c		always process constant y lines 1->nx in agishc/agish (agidvs
c		 draws left to right).
c		do vis1 loop in reverse order so my correct on exit.
c		put steep in common (could allow setup to modify it).
c		halve dy if (nx2-nx1)=1.  [met]
c
c  jan-90	convert to lower case.
c		move duplicated initialization from agisoc/agisom to agiso0.
c		for constant x lines, go back to processing left to right if
c		 theta>90, right to left if theta<90.
c		use agish for non-hidden with no contour lines; drop agicrv.
c		use dzmin for z log data values <=0.
c		[note: y log option probably doesn't work.]
c		slide y axis remote exponent left if theta>90.
c		allow y axis labels to be in reverse order. [met]
c
c  nov-90	add new argument to mnmx calls.
c		fix x/y clipping in agisom by restricting data indices.
c		make first step smaller in agisom if agistx/agisty called
c		 so surface line is drawn at last index.
c		combine agish and agishc.
c		change agidvs to "walk" along line drawing each visible
c		 segment instead of just checking the end points.
c		fix processing of "steep" lines in agidvs. [met]
c
c  dec-90	fix test for whether data should be plotted for log mode.
c
c  jan-91	make 2 passes across each line of constant y, drawing on
c		  on first, mark on second, instead of draw from min to max.
c		major rewrite of slanted axis tick/label code to match
c		  sggrf routines.
c		allow reversed y axis (from max to min). [met]
c
c  jul-91	draw contours in color.
c		modify juster/notate to match changes in sggrf. [met]
c
c  nov-91	fix bottom view test (undefined lower=1024, not 0). [met]
c
c  feb-93	put bppcom in each routine instead of calling comget. [met]
c
c-------------------------------------------------------------/aginit
c * initialize ag and isometric common
c
	subroutine aginit
	implicit none
	include 'sgcopr_inc'
	include 'sgiso_inc'
c
	logical hide
	real c(*)
	logical clipz
	logical xskirt, yskirt
c
	integer ic, is, nc, ixtype, iytype, iztype, ixstep, iystep
	integer n, minx, maxx, leny, minz, maxz, iside, iround
	real cmin, cmax, dc, xmin, xmax, ymin, ymax,zmin,zmax
	real theta

	call binitt
c
	dxmin=0.
	dxmax=0.
	dymin=0.
	dymax=0.
	dzmin=0.
	dzmax=0.
	kxtype=1
	kytype=1
	kztype=1
	kxstep=1
	kystep=1
	ksxmin=150
	ksxmax=590
	thetad=45.
	klengy=440
	kszmin=120
	kszmax=465
	refx=.false.
	refy=.false.
	hidden=.false.
	khside=0
	ncont=0
	zclip=.true.
	kyrond=3
	steep=3.
	call agseet(i4105,kfac,ic,is)
	return
c%
c-------------------------------------------------------------/agihid
c * set hidden flag
c
	entry agihid(hide)
c
	hidden=hide
	return
c%
c-------------------------------------------------------------/agiszc
c * set contours
c
	entry agiszc(nc,c)
c
c equally spaced contours
	if(nc.lt.0) then
	  cmin=c(1)
	  cmax=c(2)
	  if(cmin.eq.cmax) then
c  must set cont after zmin,zmax determined
	    ncont=nc
	  else
c  user specified range
	    ncont=min(max(2,abs(nc)),41)
	    dc=(cmax-cmin)/(ncont-1)
	    do n=1,ncont
	      cont(n)=cmin+n*dc	!was n-1
	    enddo
	  endif
c
c  user set contours
	elseif(nc.gt.0) then
	  ncont=min(nc,41)
	  do n=1,ncont
	    cont(n)=c(n)
	  enddo
c
c no contours
	else	!(nc.eq.0)
	  ncont=nc
	endif
	return
c%
c-------------------------------------------------------------/agisid
c * set side flag
c
	entry agisid(iside)
c
	khside=iside
	return
c%
c-------------------------------------------------------------/agidlx
c * set x data limits
c
	entry agidlx(xmin,xmax)
c
	dxmin=xmin
	dxmax=xmax
	return
c%
c-------------------------------------------------------------/agidly
c * set y data limits
c
	entry agidly(ymin,ymax)
c
	dymin=ymin
	dymax=ymax
	return
c%
c-------------------------------------------------------------/agidlz
c * set z data limits
c
	entry agidlz(zmin,zmax)
c
	dzmin=zmin
	dzmax=zmax
	return
c%
c-------------------------------------------------------------/agizcl
c * clip z
c
	entry agizcl(clipz)	!true or false
c
	zclip=clipz
	return
c%
c-------------------------------------------------------------/agixty
c * set x transformation type
c
	entry agixty(ixtype)	!1 linear, 2 log
c
	kxtype=ixtype
	return
c%
c-------------------------------------------------------------/agiyty
c * set y transformation type
c
	entry agiyty(iytype)	!1 linear, 2 log
c
	kytype=iytype
	return
c%
c-------------------------------------------------------------/agizty
c * set z transformation type
c
	entry agizty(iztype)	!1 linear, 2 log
c
	kztype=iztype
	return
c%
c-------------------------------------------------------------/agistx
c * set x increment
c
	entry agistx(ixstep)
c
	kxstep=max(ixstep,1)
	return
c%
c-------------------------------------------------------------/agisty
c * set y increment
c
	entry agisty(iystep)
c
	kystep=max(iystep,1)
	return
c%
c-------------------------------------------------------------/agislx
c * set x screen limits
c
	entry agislx(minx,maxx)
c
	ksxmin=minx
	ksxmax=maxx
	return
c%
c-------------------------------------------------------------/agisly
c * set y screen limits - theta in degrees; y axis length in screen units
c
	entry agisly(theta,leny)
c
	thetad=theta
	if(leny.gt.0) then
	  klengy=leny
	else
	  klengy=(ksxmax-ksxmin)*.7071068/cos(theta/57.29578)	!cos(45)
	  klengy=abs(klengy)
	endif
	return
c%
c-------------------------------------------------------------/agislz
c * set z screen limits
c
	entry agislz(minz,maxz)
c
	kszmin=minz
	kszmax=maxz
	return
c%
c-------------------------------------------------------------/agiskt
c * set flag to draw skirt to x,y plane
c
	entry agiskt(xskirt,yskirt)	!true or false
c
	refx=xskirt
	refy=yskirt
	return
c%
c-------------------------------------------------------------/agixnt
c * set x rounding option
c
	entry agixnt(iround)
c
	call xneat(iround)
	return
c%
c-------------------------------------------------------------/agiynt
c * set y rounding option
c
	entry agiynt(iround)
c
	kyrond=iround+1.5
	return
c%
c-------------------------------------------------------------/agiznt
c * set z rounding option
c
	entry agiznt(iround)
c
	call yneat(iround)
	return
	end
c%
c-------------------------------------------------------------/agisoc
c * isometric with option for hidden line removal and contours
c
	subroutine agisoc(x,y,z,nxdim,nx,ny,xu,yu,zu,m)
	implicit none
	integer nxdim,nx,ny
	include 'sgiso_inc'
	real x(nxdim), y(ny), z(nxdim,ny), m(2,nx,ny)
	character*(*) xu,yu,zu
c
c draw isometric
	call agiso0(x,y,z,nxdim,nx,ny,xu,yu,zu)
c
c draw surface, then frame
	call agishc(x,y,z,nxdim,nx,ny,m)
	call agifrm
c
	return
	end
c%
c-------------------------------------------------------------/agisom
c * isometric with option for hidden line removal
c
	subroutine agisom(x,y,z,nxdim,nx,ny,xu,yu,zu)
	implicit none
	include 'sgiso_inc'
	integer nxdim,nx,ny
	real x(nxdim), y(ny), z(nxdim,ny)
	character*(*) xu,yu,zu
c
c draw isometric
	call agiso0(x,y,z,nxdim,nx,ny,xu,yu,zu)
c
c draw surface, then frame
	call agish(x,y,z,nxdim,nx,ny)
	call agifrm
c
	return
	end
c%
c-------------------------------------------------------------/agish
c * draw surface with options for hidden line removal
c
	subroutine agish(x,y,z,nxdim,nx,ny)
	implicit none
	integer nxdim,nx,ny
	real x(nxdim), y(ny), z(nxdim,ny), m(2,nx,ny)
	include 'sgiso_inc'
	logical lcont,dcont
	integer ibasec
	parameter (ibasec=1)
	integer j
	real comget
	common /bppcom/ comget(80)
	
	real yymin, ynorm, yymax, yins, zmin, dc
	real xxmin, xxmax
	integer ihide, mark, ipos, icurv, n, i, jn1, jny, jxx
	integer in1, inx, ixx, ixinc, ixbeg, ixend, ix, iy1
	integer ix1, ix2, iy2, jx, jdraw, ih, im, jj


c  function to allow y data to be in reverse order
	yins(j) = ((yymax-y(j))*ynorm)*((yymin-y(j))*ynorm)
c
	ncont=0			!guarantee no contours
	lcont=.false.
	go to 100
c
	entry agishc(x,y,z,nxdim,nx,ny,m)
c
	lcont=.true.
 100	continue
c
	zmin=dzmin
	if(kztype.eq.2) zmin=10.**zmin
c
c set whether to do hidden line removal
	if(hidden) then
	  ihide=1		!draw visible segments
	  mark=1		!mark upper/lower arrays
	else
	  ihide=-1		!draw all segments
	  mark=0		!dont mark arrays
	endif
c
c initialize height arrays
	do ipos=1,1024
	  upper(ipos)=0
	  lower(ipos)=1024
	enddo
c
c set line color for curves/skirt
	icurv=-1
	if(i4105.ne.0) then
	  icurv=comget(ibasec+10)
	  call linclr(icurv)
	endif
c
c set contour levels if user requested z span option
	if(ncont.lt.0) then
	  ncont=min(abs(ncont),41)	!max(2,abs(ncont))
	  dc=(dzmax-dzmin)/(ncont-1)	!was ncont+1
	  do n=1,ncont
	    cont(n)=dzmin+n*dc
	  enddo
	endif
c
c map z(i,j) to ix(i,j),iy(i,j)
	if(lcont) then
	  do j=1,ny
	    do i=1,nx
	      call agicau(x(i),y(j),z(i,j),m(1,i,j),m(2,i,j))
	    enddo
	  enddo
	endif
c
c restrict to y limits
	yymin=dymin
	yymax=dymax
	if(kytype.eq.2) then
	  yymin=10.**yymin
	  yymax=10.**yymax
	endif
	if(yymin.ne.yymax) then
	  ynorm=1./(yymax-yymin)
	else
	  ynorm=1.
	endif
	jn1=1
	do while (yins(jn1).gt.0. .and. jn1.le.ny)
	  jn1=jn1+1
	enddo
	jny=ny
	do while (yins(jny).gt.0. .and. jny.ge.jn1)
	  jny=jny-1
	enddo
	if(jny.le.jn1) then
cx	  call agseter()
	  return
	endif
	jxx=mod((jny-jn1),kystep)
	if(jxx.eq.0) jxx=kystep	!get to a multiple of kystep
c
c restrict to x limits
	xxmin=dxmin
	xxmax=dxmax
	if(kxtype.eq.2) then
	  xxmin=10.**xxmin
	  xxmax=10.**xxmax
	endif
	in1=1
	do while (xxmin.gt.x(in1) .and. in1.le.nx)
	  in1=in1+1
	enddo
	inx=nx
	do while (xxmax.lt.x(inx) .and. inx.ge.in1)
	  inx=inx-1
	enddo
	if(inx.le.in1) then
cx	  call agseter()
	  return
	endif
	ixx=mod((inx-in1),kxstep)	!get to a multiple of kxstep
	if(ixx.eq.0) ixx=kxstep
c
c set which side to draw x skirt
	if(thetad.gt.90.) then
	  ixinc=1
	  ixbeg=in1
	  ixend=inx
	else	!thetad <= 90
	  ixinc=-1
	  ixbeg=inx
	  ixend=in1
	endif
c
c draw skirt lines to the x axis using grid color
	if(refx) then
	  ix=ixx		!first incr goes to kxstep
	  i=in1
	  do while (i.le.inx)
	    call agicau(x(i),y(jn1),zmin,ix1,iy1)	!from zmin
	    if(lcont) then
	      ix2=m(1,i,jn1)
	      iy2=m(2,i,jn1)
	    else
	      call agicau(x(i),y(jn1),z(i,jn1),ix2,iy2)	!to z at x,y
	    endif
	    if(iy1.ne.iy2) call agidvs(ix1,iy1,ix2,iy2,ihide,0)
	    i=i+ix
	    ix=kxstep
	  enddo
	endif
c
c draw skirt lines to the y axis
	if(refy) then
	  jx=jxx		!first incr goes to kystep
	  j=jn1
	  do while (j.le.jny)
	    call agicau(x(ixbeg),y(j),zmin,ix1,iy1)
	    if(lcont) then
	      ix2=m(1,ixbeg,j)
	      iy2=m(2,ixbeg,j)
	    else
	      call agicau(x(ixbeg),y(j),z(ixbeg,j),ix2,iy2)
	    endif
	    if(iy1.ne.iy2) call agidvs(ix1,iy1,ix2,iy2,ihide,0)
	    j=j+jx
	    jx=kystep
	  enddo
	endif
c
c draw surface with lines of constant x and y
	j=jn1
	jdraw=jn1
	do while (j.le.jny)
	  if(j.eq.jn1) then
	    jdraw=jdraw+jxx
	  else
	    jdraw=jdraw+kystep
	  endif
	  ih=ihide		!draw first time, mark second time
	  do im=0,mark		!loop twice if hidden, once if not
c
c draw constant y line (for y fixed, draw x vs z from in1 to inx)
	  if(lcont) then
	    ix1=m(1,in1,j)
	    iy1=m(2,in1,j)
	  else
	    call agicau(x(in1),y(j),z(in1,j),ix1,iy1)	!i=in1
	  endif
	  do i=in1+1,inx	!was kxstep
	    if(lcont) then
	      ix2=m(1,i,j)
	      iy2=m(2,i,j)
	    else
	      call agicau(x(i),y(j),z(i,j),ix2,iy2)
	    endif
	    call agidvs(ix1,iy1,ix2,iy2,ih,im)
	    ix1=ix2
	    iy1=iy2
	  enddo
	  ih=0			!dont draw second time thru
	  enddo
c
c draw constant x lines (for each x, draw y vs z from j to jdraw)
	  if(j.lt.jny) then
	    if(ixinc.gt.0) then	!first i increment
	      ix=ixx
	    else
	      ix=kxstep
	    endif
	    i=ixbeg
	    do while ((ixend-i)*ixinc.ge.0)
	      ih=ihide
	      dcont=ncont.gt.0 .and. i.lt.inx
	      do im=0,mark
	      if(lcont) then
		ix1=m(1,i,j)
		iy1=m(2,i,j)
	      else
		call agicau(x(i),y(j),z(i,j),ix1,iy1)
	      endif
	      do jj=j+1,jdraw
	      if(lcont) then
		ix2=m(1,i,jj)
		iy2=m(2,i,jj)
	      else
		call agicau(x(i),y(jj),z(i,jj),ix2,iy2)
	      endif
	      call agidvs(ix1,iy1,ix2,iy2,ih,im)
	      if(dcont) then
		call agidzc(z,nxdim,nx,ny,m,i,jj-1,cont,ncont)
	      endif
	      ix1=ix2
	      iy1=iy2
	      enddo
	      ih=0
	      dcont=.false.
	      enddo
	      i=i+ix*ixinc
	      ix=kxstep
	      if(ixinc.lt.0 .and. (i-ix).lt.ixend) ix=ixx
	    enddo
	  endif
	  j=jdraw
c
	enddo
c
	return
	end
c%
c-------------------------------------------------------------/agiso0
c * ag ii isometric graph (internal)
c
	subroutine agiso0(x,y,z,nxdim,nx,ny,xu,yu,zu)
	implicit none
	integer nxdim,nx,ny
	real x(nxdim), y(ny), z(nxdim,ny)
	character*(*) xu,yu,zu
	include 'sgiso_inc'
	integer j
	real     gran, dtic
	real     finfin, agisym
	integer  nbase, ibasex, ibasey, ix0, ix1, kount
	integer  i, ntics
	real     toff
	integer  k1,k2,nmod,ibeg
	common /sgaxi/ k1(2),k2(2),nmod(2),gran(2),ibeg(2),dtic(2)
	integer ibasec
	parameter (ibasec=1)
	real comget
	common /bppcom/ comget(80)
c
	if(dzmin.eq.0. .and. dzmax.eq.0.) then
	  finfin=comget(ibasec+3)
	  dzmin=finfin
	  dzmax=-finfin
	  do j=1,ny
	    call agmnmx(z(1,j),dzmin,dzmax,nx,kztype)
	  enddo
	endif
c
c  draw grid for x vs z @ymin
	call npts(nx)
	call slimx(ksxmin,ksxmax)	!screen/data limits
	call slimy(kszmin,kszmax)
	call dlimx(dxmin,dxmax)
	call dlimy(dzmin,dzmax)
	call yfrm(2)		!ticks out, no grid
	call xfrm(2)
	call xtype(kxtype)	!linear/log flag
	call ytype(kztype)
	if(thetad.gt.90.) call ylocrt(0)	!position z axis
	call check(x,z)
	call agdspl(x,y)
	call agtchs(xu,0)	!labels on x,z axes
	call agtcvs(zu,0)
c
c  set isometric scales
	nbase=ibasex(0)
	dxmin=comget(nbase+26)
	dxmax=comget(nbase+27)
	agisxm=float(ksxmax-ksxmin)/(dxmax-dxmin)
	agisxb=float(ksxmin)-agisxm*dxmin
c
	nbase=ibasey(0)
	dzmin=comget(nbase+26)
	dzmax=comget(nbase+27)
	agiszm=float(kszmax-kszmin)/(dzmax-dzmin)
	agiszb=float(kszmin)-agiszm*dzmin
c
c scale y
	call slimy(kszmin,kszmin+klengy)
	call dlimy(dymin,dymax)		!if dymin<>0 or dymax<>0
	call npts(ny)
	call ytype(kytype)
	call yneat(kyrond-1)
	call rgchek(nbase,y)		!calc data range
	call loptim(nbase,2)		!calc endpoints, # ticks
	dymin=comget(nbase+26)
	dymax=comget(nbase+27)
c
c  position of y axis endpoint
	agicth=cos(thetad/57.29578)
	agisth=sin(thetad/57.29578)
	agisym=klengy/(dymax-dymin)
	xslope=agisym*agicth
	zslope=agisym*agisth
	ksymxx=ksxmin+ifix(klengy*agicth)
	ksymxz=kszmin+ifix(klengy*agisth)
c
c  compute axis endpoints
	if(thetad.le.90.) then
	  ix0=ksxmax
	  ix1=ksymxx+ksxmax-ksxmin
cx	  xpos=dxmax
	else
	  ix0=ksxmin
	  ix1=ksymxx
cx	  xpos=dxmin
	endif
cx	iz0=kszmin
cx	iz1=ksymxz
c
c  draw axis line
	call agimsu(ix0,kszmin)
	call agidsu(ix1,ksymxz)
c
c draw ticks with labels
	call agidya(ix0,kszmin,ix1)
c
c  output y unit label
	call agilya(ix0,kszmin,ix1,yu)
c
c  set the z tick values in common for contour values
	call agizc(ix0,kszmin,ix1)
	if(ncont.eq.-1 .or. ncont.eq.-2) then
	  kount=0
	  do i=1,nztics
	    if(ncont.eq.-2 .or. lzmjmn(i)) then
	    kount=kount+1
	    cont(kount)=vztics(i)
	    endif
	  enddo
	  ncont=kount
	endif
	return
	end
c%
c-------------------------------------------------------------/agizc
c * internal contour setting routine
c
	subroutine agizc(ix0,iz0,ix1)
	implicit none
c
c  call after the z axis has been drawn (i.e. after dsplay)
c  the lzmjmn array is logical and corresponds to vztics.
c  if the level is a major tick, lzmjmn will be true.
c
	integer ix0,iz0,ix1
	include 'sgiso_inc'
	real dtic2, y, x, toff
	integer kount, ntics
	integer k1, k2, nmod, ibeg
	real    gran, dtic
	common /sgaxi/ k1(2),k2(2),nmod(2),gran(2),ibeg(2),dtic(2)
	real tablog(9)
	data tablog/  0.,
     *		0.30103, 0.47712, 0.60206, 0.69897,
     *		0.77815, 0.84510, 0.90309, 0.95424/
	logical logm
	integer isign, k, i, iyl, ixl, izl
	real comget
	common /bppcom/ comget(80)
c
	logm=(nmod(2).eq.3 .or. nmod(2).eq.9)
	isign=1
cx	if(k2(2).lt.k1(2)) isign=-1	!draw ticks in reverse order (1/90)
	dtic2=isign*dtic(2)
	kount=0
	ntics=0
	do k=k1(2),k2(2)	!,isign
c  tick interval
	  i=mod(k,nmod(2))
	  iyl=(ibeg(2)+ifix(ntics*dtic2))/kfac-iz0
	  ixl=ix0+ifix(iyl*agicth)
	  izl=iz0+ifix(iyl*agisth)
	  if(i.eq.0 .or. .not.logm) then	!major tick or minor linear
	    call revcot(ixl,izl,x,y)
	    if(kztype.eq.2) y=log10(y)
	    if(kount.ge.41) go to 200
	    kount=kount+1
	    vztics(kount)=y
	    lzmjmn(kount)=(i.eq.0)
	    ntics=ntics+1
	  else			!minor log
	    if(nmod(2).eq.9) then
	      toff=tablog(i+1)
	    elseif(i.eq.1) then
	      toff=tablog(2)
	    else
	      toff=tablog(5)
	    endif
	    iyl=(ibeg(2)+ifix((ntics-1+toff)*dtic2))/kfac-iz0
	    ixl=ix0+ifix(iyl*agicth)
	    izl=iz0+ifix(iyl*agisth)
	    call revcot(ixl,izl,x,y)
	    if(kztype.eq.2) y=log10(y)
	    if(kount.ge.41) go to 200
	    kount=kount+1
	    vztics(kount)=y
	    lzmjmn(kount)=.false.
	  endif
	enddo
c
 200	nztics=kount
	return
	end
c%
c-------------------------------------------------------------/agigzc
c * return tick values (in user units) of the z axis
c * and show whether each is a major or minor tick (in lvltic).
c * if lvltic is true, it's major; if false, it's minor.
c * the number of values is returned in nc and will be.le.41.
c
	subroutine agigzc(nc,c,lvltic)
	implicit none
	real c(*)
	logical lvltic(*)
	integer nc 
	include 'sgiso_inc'
	integer i
c
	nc=nztics
	do i=1,nc
	  c(i)=vztics(i)
	  lvltic(i)=lzmjmn(i)
	enddo
c
	return
	end
c%
c-------------------------------------------------------------/agidya
c * draw slanted y axis
c
	subroutine agidya(ix0,iz0,ix1)
	implicit none
	integer ix0,iz0,ix1
	integer i, isign, nbase, mbase,lmaj, ivert,  ihorz
	integer ih2, lmin, iwidth, iexp
	real dmin, dtic2, comget, fac, valab, toff
	integer ntics, k, iyl, ixl, izl, iypos, ixpos
	integer len
c
	include 'sgiso_inc'
	character clabl*12
	real tablog(9)
	logical logm
	integer k1(2),k2(2),nmod(2),ibeg(2)
	real    gran(2),dtic(2)
	common /sgaxi/ k1,k2,nmod,gran,ibeg,dtic
	real    roundd, roundu         ! functions
	integer ibasey, ibasex         ! functions
c
	data tablog/ 0.,
     *	0.30103, 0.47712, 0.60206, 0.69897,
     *	0.77815, 0.84510, 0.90309, 0.95424/
c
	logm=(nmod(2).eq.3 .or. nmod(2).eq.9)
	if(logm) then
	  dmin=dymin			!value of first tick
	  dmin=roundu(dmin,1.)
	endif
	isign=1	!sign(1,k2(2)-k1(2))
	if(k2(2).lt.k1(2)) isign=-1	!draw ticks in reverse order (1/90)
	dtic2=isign*dtic(2)
c
	nbase=ibasey(0)
	mbase=ibasex(0)
	lmaj=comget(mbase+6)		!use x axis tick lengths
	call csize(ihorz,ivert)
	ih2=ihorz/2
	if(ix1.lt.ix0) then		!direction of ticks and labels
	  lmaj=-lmaj
	  ih2=-ih2
	endif
	lmin=lmaj/2			! for y, too
	iwidth=comget(nbase+17)		!width of axis labels
	iexp=comget(nbase+18)		!remote exponent
	fac=10.**(-iexp)
c
	ntics=0
	do k=k1(2),k2(2),isign
c  tick interval
	  i=mod(k,nmod(2))
	  iyl=(ibeg(2)+ifix(ntics*dtic2))/kfac-iz0
	  ixl=ix0+ifix(iyl*agicth)
	  izl=iz0+ifix(iyl*agisth)
	  if(i.eq.0) then	!major tick (linear or log)
	    call agimsu(ixl,izl)
	    call drwrel(lmaj,0)
	    call movrel(ih2,-ivert/3)
	    call seeloc(ixpos,iypos)
	    if(.not.logm) then
	      valab=k*gran(2)*fac
	    else
	      valab=dmin+ntics
	    endif
	    len=iwidth
	    call numset(valab,nbase,clabl,len)
	    call movabs(ixpos,iypos)
	    call juster(clabl,len,-lmaj)
	    ntics=ntics+1
	  elseif(logm) then	!minor tick log
	    if(nmod(2).eq.9) then
	      toff=tablog(i+1)
	    elseif(i.eq.1) then
	      toff=tablog(2)
	    else
	      toff=tablog(5)
	    endif
	    iyl=(ibeg(2)+ifix((ntics-1+toff)*dtic2))/kfac-iz0
	    ixl=ix0+ifix(iyl*agicth)
	    izl=iz0+ifix(iyl*agisth)
	    call agimsu(ixl,izl)
	    call drwrel(lmin,0)
	  else			!minor tick linear
	    call agimsu(ixl,izl)
	    call drwrel(lmin,0)
	    ntics=ntics+1
	  endif
	enddo
c
	if(iexp.ne.0) then	!.and. itype.ne.2
	  iyl=(ibeg(2)+ifix(ntics*dtic2))/kfac-iz0
	  ixl=ix0+ifix(iyl*agicth)
	  izl=iz0+ifix(iyl*agisth)
	  if(thetad.gt.90) ixl=ixl-ihorz*3
	  call remlab(nbase,+1,0,ixl*kfac,izl*kfac)
	endif
	return
	end
c%
c-------------------------------------------------------------/agilya
c * label y axis
c
	subroutine agilya(ix0,iz0,ix1,string)
	implicit none
	integer  ix0,iz0,ix1
c
	include 'sgiso_inc'
	character string*(*)
	character*80 astrin
	integer nchar, nbase
	integer iwidth, ldir, ixc
	real    xchar
	integer ibasec
	parameter (ibasec=1)
	real comget
	common /bppcom/ comget(80)
	integer iyc,linhgt,linwdt,itext,j
	real    vert,horz,w,h
	integer ibasey                ! function
c
	nchar=0
	call agtlng(string,.false.,astrin,nchar)
	if(nchar.eq.0) return
c
	nbase=ibasey(0)
cx	iytype=comget(nbase+15)
	iwidth=comget(nbase+17)
	ldir=1
	if(ix1-ix0.lt.0) ldir=-1
c
	xchar=nchar
	if(ldir.lt.0) then
	  xchar=xchar-1.8
	  iwidth=iwidth+1	!was 0.8
	endif
	iwidth=iwidth+1
c
	ixc=ix0+klengy*agicth*0.5	!midpoint of axis
	iyc=iz0+klengy*agisth*0.5
	vert=linhgt(1)/kfac
	horz=linwdt(1)/kfac
	w=agicth*vert*ldir
	h=agisth*vert*ldir
	ixc=ixc-0.5*xchar*w+ldir*(35+iwidth*horz)+agicth*vert
	iyc=iyc-0.5*xchar*h-agisth*vert
c
	if(i4105.ne.0) then
	  itext=comget(ibasec+12)
	  call agtxclr(itext)
	endif
	do j=1,nchar
	  call agimsu(ixc,iyc)
	  call agths(astrin(j:j),1)
	  ixc=ixc+w
	  iyc=iyc+h
	enddo
	return
	end
c%
c-------------------------------------------------------------/agifrm
c * draw the frame, with inside lines hidden
c
	subroutine agifrm
	implicit none
	include 'sgiso_inc'
	integer lx, lz, icurv
	integer ibasec
	parameter (ibasec=1)
	real comget
	common /bppcom/ comget(80)
c
	icurv=-1
	if(i4105.ne.0) then
	  icurv=comget(ibasec+11)	!grid color
	  call linclr(icurv)
	endif
c
	lx=ksxmax-ksxmin
	lz=kszmax-kszmin
	if(thetad.le.90.) then
c
	  call agimsu(ksxmin,kszmax)	!front,top,left
	  call agidsu(ksymxx,ksymxz+lz)	!back,top,left
	  call agidsu(ksymxx+lx,ksymxz+lz)	!back,top,right
	  call agidsu(ksymxx+lx,ksymxz)	!back,bottom,right
c
	  if(hidden) then
	  call agidvs(ksymxx+lx,ksymxz,ksymxx,ksymxz,1,0)	!was 1,1
	  call agidvs(ksymxx,ksymxz,ksymxx,ksymxz+lz,1,0)
	  call agidvs(ksxmin,kszmin,ksymxx,ksymxz,1,0)
c
	  else
	  call agidsu(ksymxx,ksymxz)	!back,bottom,left
	  call agidsu(ksymxx,ksymxz+lz)	!back,top,left
	  call agimsu(ksymxx,ksymxz)
	  call agidsu(ksxmin,kszmin)	!front,bottom,left
	  endif
c
	else		!thetad.gt.90.
c
	  call agimsu(ksxmax,kszmax)
	  call agidsu(ksymxx+lx,ksymxz+lz)
	  call agidsu(ksymxx,ksymxz+lz)
	  call agidsu(ksymxx,ksymxz)
c
	  if(hidden) then
	  call agidvs(ksymxx+lx,ksymxz,ksymxx,ksymxz,1,0)
	  call agidvs(ksymxx+lx,ksymxz,ksymxx+lx,ksymxz+lz,1,0)
	  call agidvs(ksymxx+lx,ksymxz,ksxmax,kszmin,1,0)
	  else
c
	  call agidsu(ksymxx+lx,ksymxz)
	  call agidsu(ksymxx+lx,ksymxz+lz)
	  call agimsu(ksymxx+lx,ksymxz)
	  call agidsu(ksxmax,kszmin)
	  endif
c
	endif
cx	if(icurv.ge.0) call linclr(1)	!reset
c
	return
	end
c%
c-------------------------------------------------------------/agidvs
c * draw visible part of surface line
c
	subroutine agidvs (mx1,my1,mx2,my2,idraw,imark)
c
c draw the visible part of the line connecting (mx1,my1) and (mx2,my2).
c if idraw.gt.0, the visible part of the line is drawn.
c if idraw.eq.0, the line is not drawn
c if idraw.lt.0, the entire line is drawn.
c if imark.ne.0, the visibility array is marked.
c if khside.eq.0, both top and bottom sides are drawn
c if khside.lt.0, the lower side is drawn.
c if khside.gt.0, the upper side is drawn.
c
	logical vis1,vis2
	include 'sgiso_inc'
c
	if (khside.lt.0) go to 180
	imrk = iabs(imark)
c
c draw/mark upper side.
c make line left to right.
	if (mx1.le.mx2) then
	  nx1 = mx1
	  ny1 = my1
	  nx2 = mx2
	  ny2 = my2
	else
	  nx1 = mx2
	  ny1 = my2
	  nx2 = mx1
	  ny2 = my1
	endif
	if (idraw.le.0) then
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)	!draw and mark
	  go to 180
	endif
c
c check visibility.
	vis1 = ny1.ge.(upper(nx1))
 20	continue
c
c process vertical line.
	if (nx1.eq.nx2) then
	  vis2 = ny2.ge.(upper(nx2))
	  if (.not.(vis1 .or. vis2)) go to 180	!invisible
	  if (vis1 .and. (.not.vis2)) ny2 = upper(nx1)
	  if ((.not.vis1) .and. vis2) ny1 = upper(nx1)
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)
	  go to 180
	endif
c
	dy = float(ny2-ny1)/float(nx2-nx1)
	if (vis1) then
c (nx1,ny1) visible, find last visible point along connecting line.
	  mx = nx1
	  my = ny1
	  do mxx=nx1+1,nx2
	    myy = ny1+ifix((mxx-nx1)*dy) !float(ny1)+float(k)*dy
	    vis2 = myy.ge.upper(mxx)	!check this point
	    if (.not.vis2) then		!if not visible
	      if(dy.lt.-steep .and. upper(mxx).gt.0) then
		mx=mxx
		my=upper(mxx)
	      endif
	      call agidm(nx1,ny1,mx,my,idraw,imrk)	!draw to (mx,my)
	      nx1 = mxx			!reset nx1,ny1
	      ny1 = myy
	      vis1 = vis2
	      go to 20			!start over
	    endif
	    mx = mxx
	    my = myy
	  enddo
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)	!draw to (nx2,ny2)
c
	else
c (nx1,ny1) not visible, find next visible point along connecting line.
	  do mxx=nx1+1,nx2
	    myy = ny1+ifix((mxx-nx1)*dy) !float(ny1)+float(k)*dy
	    vis2 = myy.ge.upper(mxx)	!check this point
	    if (vis2) then		!if visible
	      if(dy.gt.steep .and. upper(mxx).gt.0) myy = upper(mxx)
	      nx1 = mxx			!reset nx1,ny1
	      ny1 = myy
	      vis1 = vis2
	      go to 20			!start over
	    endif
	  enddo
	endif
c
  180	if (khside.gt.0) go to 380
	imrk = -imrk
c
c draw/mark lower side.
	if (mx1.le.mx2) then
	  nx1 = mx1
	  ny1 = my1
	  nx2 = mx2
	  ny2 = my2
	else
	  nx1 = mx2
	  ny1 = my2
	  nx2 = mx1
	  ny2 = my1
	endif
	if (idraw.le.0) then
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)	!draw and mark
	  go to 380
	endif
c
c check visibility.
	vis1 = ny1.le.(lower(nx1))
 320	continue
c
c process vertical line.
	if (nx1.eq.nx2) then
	  vis2 = ny2.le.(lower(nx2))
	  if (.not.(vis1 .or. vis2)) go to 380	!invisible
	  if (vis1 .and. (.not.vis2)) ny2 = lower(nx1)
	  if ((.not.vis1) .and. vis2) ny1 = lower(nx1)
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)
	  go to 380
	endif
c
	dy = float(ny2-ny1)/float(nx2-nx1)
	if (vis1) then
c (nx1,ny1) visible, find last visible point along connecting line.
	  mx = nx1
	  my = ny1
	  do mxx=nx1+1,nx2
	    myy = ny1+ifix((mxx-nx1)*dy) !float(ny1)+float(k)*dy
	    vis2 = myy.le.lower(mxx)	!check this point
	    if (.not.vis2) then		!if not visible
	      if(dy.gt.steep .and. lower(mxx).lt.1024) then
		mx=mxx
		my=lower(mxx)
	      endif
	      call agidm(nx1,ny1,mx,my,idraw,imrk)	!draw to (mx,my)
	      nx1 = mxx			!reset nx1,ny1
	      ny1 = myy
	      vis1 = vis2
	      go to 320			!start over
	    endif
	    mx = mxx
	    my = myy
	  enddo
	  call agidm(nx1,ny1,nx2,ny2,idraw,imrk)	!draw to (nx2,ny2)
c
	else
c (nx1,ny1) not visible, find next visible point along connecting line.
	  do mxx=nx1+1,nx2
	    myy = ny1+ifix((mxx-nx1)*dy) !float(ny1)+float(k)*dy
	    vis2 = myy.le.lower(mxx)	!check this point
	    if (vis2) then		!if visible
	      if(dy.lt.-steep .and. lower(mxx).lt.1024) myy = lower(mxx)
	      nx1 = mxx			!reset nx1,ny1
	      ny1 = myy
	      vis1 = vis2
	      go to 320			!start over
	    endif
	  enddo
	endif
c
  380	return
	end
c%
c-------------------------------------------------------------/agidm
	subroutine agidm(nx1,ny1,nx2,ny2,idraw,imark)
c
	include 'sgiso_inc'
c
c draw visible part of line.
	if (idraw.ne.0) then
	  call agimsu(nx1,ny1)
	  call agidsu(nx2,ny2)
	endif
c
c set visibility (upper/lower) array.
	if (imark.gt.0) then
	  if (nx1.eq.nx2) then
	    ltemp = max(ny1,ny2)
	    upper(nx1) = max(upper(nx1),ltemp)
	  else
	    dy = float(ny2-ny1)/float(nx2-nx1)
	    do k=nx1,nx2
	      ltemp = ny1+ifix((k-nx1)*dy)	!float(ny1)+float(k-nx1)*dy
	      upper(k) = max(upper(k),ltemp)
	    enddo
	  endif
	elseif (imark.lt.0) then
	  if (nx1.eq.nx2) then
	    ltemp = min(ny1,ny2)
	    lower(nx1) = min(lower(nx1),ltemp)
	  else
	    dy = float(ny2-ny1)/float(nx2-nx1)
	    do k=nx1,nx2
	      ltemp = ny1+ifix((k-nx1)*dy)	!float(ny1)+float(k-nx1)*dy
	      lower(k) = min(lower(k),ltemp)
	    enddo
	  endif
	endif
	end
c%
c-------------------------------------------------------------/agimsu
c * move screen units
c
	subroutine agimsu(kxs,kzs)
	include 'sgiso_inc'
c
	call movabs(kxs*kfac,kzs*kfac)
	return
	end
c%
c-------------------------------------------------------------/agidsu
c * draw absolute user units
c
	subroutine agidsu(kxs,kzs)
	include 'sgiso_inc'
c
	call drwabs(kxs*kfac,kzs*kfac)
	return
	end
c%
c-------------------------------------------------------------/agicau
c * convert user to screen
c
	subroutine agicau(xxcoor,yycoor,zzcoor,ixs,izs)
	implicit none
	real xxcoor,yycoor,zzcoor
	integer ixs,izs

	include 'sgiso_inc'

	real    xcoor, ycoor, zcoor
	real    xs, zs, ys
c
	xcoor=xxcoor
	ycoor=yycoor
	zcoor=zzcoor
c log
	if(kxtype.eq.2) then
	  if(xcoor.gt.0) then
	    xcoor=log10(xcoor)
	  else
	    xcoor=(dxmin)		!log10 of user value in dlimx
	  endif
	endif
	if(kytype.eq.2) then
	  if(ycoor.gt.0) then
	    ycoor=log10(ycoor)
	  else
	    ycoor=log10(dymin)
	  endif
	endif
	if(kztype.eq.2) then
	  if(zcoor.gt.0) then
	    zcoor=log10(zcoor)
	  else
	    zcoor=(dzmin)		!log10 of user value in dlimy
	  endif
	endif
c
	if(zclip) then
	  zcoor=max(zcoor,dzmin)
	  zcoor=min(zcoor,dzmax)
	endif
c
	xs=xcoor*agisxm+agisxb
	zs=zcoor*agiszm+agiszb
	ys=ycoor-dymin
	ixs=xs+ys*xslope
	izs=zs+ys*zslope
	return
	end
c%
c-------------------------------------------------------------/agidzc
c * compute lines of constant z (contour lines)
c
	subroutine agidzc (z,mx,nx,ny,m,i0,j0,cl,ncl)
	implicit none
	include 'sgiso_inc'
c
c compute lines of constant z (contour lines) in one cell of the array z
c z,mx,nx,ny are the same as in srface.
c m	     by the time agidzc is first called, m contains
c	     the two-space plotter location of each z point.
c	     u(z(i,j))=m(1,i,j).  v(z(i,j))=m(2,i,j)
c i0,j0      the cell z(i0,j0) to z(i0+1,j0+1) is the one to
c	     be contoured.
c
	integer mx,nx,ny,i0,j0,ncl
	integer m(2,nx,ny)
	real    z(mx,ny), cl(ncl)
	integer kmap(6)
	integer ibasec
	parameter (ibasec=1)
	real comget
	common /bppcom/ comget(80)
	data kmap /2,3,4,5,6,7/
	integer i1, i1p1, j1, j1p1, is, ic, icurv, idub, k, kindx
	integer k1, k2, k3, k4, jump, mua
	real    ra
	integer h1, h2, h3, h4
	real    cv, rb
	integer mva, mub, mvb

C 7/16/07:CLF to mak it compile
	real r(100,100)
c ???????
C	r(ho,hu) = (ho-cv)/(ho-hu)
c ??????
	r(1,1) = 1.
	print *,'agidzc not implemented'
	if (r(1,1) .eq. 1.) return
c
	i1 = i0
	i1p1 = i1+1
	j1 = j0
	j1p1 = j1+1
	h1 = z(i1,j1)
	h2 = z(i1,j1p1)
	h3 = z(i1p1,j1p1)
	h4 = z(i1p1,j1)
	if(kztype.eq.2) then
C	  if(h1.gt.0.) h1=log10(h1)
C	  if(h2.gt.0.) h2=log10(h2)
C	  if(h3.gt.0.) h3=log10(h3)
C	  if(h4.gt.0.) h4=log10(h4)
	endif
	if (min(h1,h2,h3,h4).gt.cl(ncl)) return
	call agseet(i4105,kfac,ic,is)
	icurv=comget(ibasec+10)		!get current color (??seeclr)
	icurv=max(icurv,1)
	idub = 0
	do k=1,ncl
c
c for each contour level, decide which of the 16 basic sit-
c uations exists, then interpolate in two-space to find the
c end points of the contour line segment within this cell.
c
	  if(i4105.ne.0) then
	    kindx = mod(k,6)+1
	    call linclr(kmap(kindx))
	  endif
	  cv = cl(k)
	  k1 = (ifix(sign(1.,h1-cv))+1)/2
	  k2 = (ifix(sign(1.,h2-cv))+1)/2
	  k3 = (ifix(sign(1.,h3-cv))+1)/2
	  k4 = (ifix(sign(1.,h4-cv))+1)/2
	  jump = 1+k1+k2*2+k3*4+k4*8
	  go to (120, 30, 50, 60, 70, 20, 80, 90, 90, 80,
     1		 40, 70, 60, 50, 30,110), jump
c
   20	  idub = 1
   30	  ra = r(h1,h2)
	  mua = real(m(1,i1,j1))+ra*(m(1,i1,j1p1)-m(1,i1,j1))
	  mva = real(m(2,i1,j1))+ra*(m(2,i1,j1p1)-m(2,i1,j1))
	  rb = r(h1,h4)
	  mub = real(m(1,i1,j1))+rb*(m(1,i1p1,j1)-m(1,i1,j1))
	  mvb = real(m(2,i1,j1))+rb*(m(2,i1p1,j1)-m(2,i1,j1))
	  go to 100
   40	  idub = -1
   50	  ra = r(h2,h1)
	  mua = real(m(1,i1,j1p1))+ra*(m(1,i1,j1)-m(1,i1,j1p1))
	  mva = real(m(2,i1,j1p1))+ra*(m(2,i1,j1)-m(2,i1,j1p1))
	  rb = r(h2,h3)
	  mub = real(m(1,i1,j1p1))+rb*(m(1,i1p1,j1p1)-m(1,i1,j1p1))
	  mvb = real(m(2,i1,j1p1))+rb*(m(2,i1p1,j1p1)-m(2,i1,j1p1))
	  go to 100
   60	  ra = r(h2,h3)
	  mua = real(m(1,i1,j1p1))+ra*(m(1,i1p1,j1p1)-m(1,i1,j1p1))
	  mva = real(m(2,i1,j1p1))+ra*(m(2,i1p1,j1p1)-m(2,i1,j1p1))
	  rb = r(h1,h4)
	  mub = real(m(1,i1,j1))+rb*(m(1,i1p1,j1)-m(1,i1,j1))
	  mvb = real(m(2,i1,j1))+rb*(m(2,i1p1,j1)-m(2,i1,j1))
	  go to 100
   70	  ra = r(h3,h2)
	  mua = real(m(1,i1p1,j1p1))+ra*(m(1,i1,j1p1)-m(1,i1p1,j1p1))
	  mva = real(m(2,i1p1,j1p1))+ra*(m(2,i1,j1p1)-m(2,i1p1,j1p1))
	  rb = r(h3,h4)
	  mub = real(m(1,i1p1,j1p1))+rb*(m(1,i1p1,j1)-m(1,i1p1,j1p1))
	  mvb = real(m(2,i1p1,j1p1))+rb*(m(2,i1p1,j1)-m(2,i1p1,j1p1))
	  idub = 0
	  go to 100
   80	  ra = r(h2,h1)
	  mua = real(m(1,i1,j1p1))+ra*(m(1,i1,j1)-m(1,i1,j1p1))
	  mva = real(m(2,i1,j1p1))+ra*(m(2,i1,j1)-m(2,i1,j1p1))
	  rb = r(h3,h4)
	  mub = real(m(1,i1p1,j1p1))+rb*(m(1,i1p1,j1)-m(1,i1p1,j1p1))
	  mvb = real(m(2,i1p1,j1p1))+rb*(m(2,i1p1,j1)-m(2,i1p1,j1p1))
	  go to 100
   90	  ra = r(h4,h1)
	  mua = real(m(1,i1p1,j1))+ra*(m(1,i1,j1)-m(1,i1p1,j1))
	  mva = real(m(2,i1p1,j1))+ra*(m(2,i1,j1)-m(2,i1p1,j1))
	  rb = r(h4,h3)
	  mub = real(m(1,i1p1,j1))+rb*(m(1,i1p1,j1p1)-m(1,i1p1,j1))
	  mvb = real(m(2,i1p1,j1))+rb*(m(2,i1p1,j1p1)-m(2,i1p1,j1))
	  idub = 0
  100	  call agidvs(mua,mva,mub,mvb,1,0)	!was 1,1
	  if(idub.lt.0) go to 90
	  if(idub.gt.0) go to 70
  110	enddo
  120	continue
	if(i4105.ne.0) then
	  call linclr(icurv)		!reset color
	endif
	return
	end
c%
