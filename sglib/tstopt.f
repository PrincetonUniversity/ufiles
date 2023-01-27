c file: tstopt.for	sglib type, neat, color options
c
c  test sg lin/log, neat, color options; binitt/upause loop
c
	program tstopt
c
	real xpts(16384),ypta(16384),yptb(16384)
	common /xopts/ lxlog,lxform,lxneat,lxtic,lgcolr,lclip,lskip,lxwid
	common /yopts/ lylog,lyform,lyneat,lytic
	common /curv/ isymb(6),iline(6),icolr(6),istar,istep
	common /dats/ ncurv,jform,xlims(2),ylims(2),numxy
	common /text/ itcolr,itsize
	data lxlog/1/, lxform/5/, lxneat/1/, lxtic/1/, lskip/1/
	data lylog/1/, lyform/5/, lyneat/1/, lytic/1/
	data lgcolr/1/, lclip/1/, itcolr/1/, itsize/1/
	data ncurv/2/, jform/2/
	data iline/6*0/, isymb/6*0/, icolr/6*1/
	character cmd*4,c*1
	equivalence (cmd,c)
	character*20 title
	character*3 places(7) /'STD','UPH','LOH','UL4','ur4','LL4','lr4'/
	character*16 legtext(6)
c
	do while(.true.)
	  cmd='C'
	  do while(c.ne.'P')
	  call at_char(' Data,xAxis,Curve,Labels,Terminal,Plot,eXit',cmd)
	  if(c.eq.'X'.or.cmd.eq.'EX') go to 800
	  if(c.eq.'D') call dataop
	  if(c.eq.'A') call xaxiop
	  if(c.eq.'C') call curvop
	  if(c.eq.'L') call lablop
	  if(c.eq.'T') call termop
	  enddo
c
	  call initt(0)
	  call erase
	  call data1(xpts,xmax,ypta,yptb,nx,ny)
c
	  do jj=1,3
	    call binitt
	    if(lclip.eq.0) call agnclp
	    call place(places(3+jj))
	    call xtype(lxlog)
	    call xlab(lxtic)
	    call xneat(lxneat)
	    call yneat(lyneat)
	  call xwdth(lxwid)
	    if(jform.eq.3) then
	      call agnpts(nx,ny)
	    elseif(jform.ge.4) then
	      if(jform.eq.4) call agbasx(1)
	      call npts(nx)
	    endif
	    call xfrm(lxform)
	    call yfrm(lxform)
	    call stepl(lskip)
	    call agcgrid(lgcolr)
	    call line(iline(1))
	    call symbl(isymb(1))
	    call agsteps(istar,istep)
	    call agccurv(icolr(1))
	    call chrsiz(itsize)
	    call agctext(itcolr)
	    call check(xpts, ypta)
	    call dsplay(xpts, ypta)
	    if(lxform.gt.0) call frame
	    call line(iline(2))
	    call symbl(isymb(2))
	    call agccurv(icolr(2))
	    call cplot(xpts, yptb)
	    call seetw(ilft,irit,ibot,itop)
	  legtext(1) = 'line 1'
	  legtext(2) = 'line 2'
	  call aglegnd(2,2,legtext,6,isymb,iline)
	    title = 'LINEAR'
	    if(lylog.ne.1) title = 'LOG'
	    call agtcvs(title)
	    title = 'X LINEAR'
	    if(lxlog.eq.2) then
	      title = 'X LOG'
	    endif
	    call agtchs(title)
	    call chrsiz(1)
	    title = 'extend to data'
	    if(lxneat.eq.1) then
	      title = 'to minor ticks'
	    elseif(lxneat.eq.2) then
	      title = 'to major ticks'
	    endif
	    call agtcgt(0,title)
	    call datai(xpts,xmax)
	  enddo
c
	  call place('LR4')	!7
	  call setwin
	  call seetw(ilft,irit,ibot,itop)
	  title = 'no x labels'	!lxtic = 0
	  if(lxtic.eq.1) then
	    title = 'label ticks'
	  elseif(lxtic.eq.2) then
	    title = 'label ends'
	  endif
	  ihite = linhgt(1)
	  itop = itop-40
	  call agtmhs(ilft,itop,title)
	  imove = linwdt(12)
	  call agtmhs(ilft+imove,itop,'!')
	  itop = itop-ihite
	  if(isymb(1).ne.0) then
	    title = 'symbol'
	    call agtmhs(ilft,itop,title)
	    call movabs(ilft+120,itop+5)
	    call teksym(isymb(1),1.)
	  endif
	  itop = itop-(ihite*3/2)
	  title = '  0deg'
	  call agtmrs(ilft,itop,0,title)
	  title = '270deg'
	  call agtmrs(ilft+80,itop,270,title)
	  title = '180deg'
	  call agtmrs(ilft+80,itop-80,180,title)
	  title = ' 90deg'
	  call agtmrs(ilft,itop-80,90,title)
	  call upause(0)
	  call finitt(0,0)
	enddo
 800	continue
	end
c%
	subroutine dataop
	common /xopts/ lxlog,lxform,lxneat,lxtic,lgcolr,lclip,lskip,lxwid
	common /yopts/ lylog,lyform,lyneat,lytic
	common /curv/ isymb(6),iline(6),icolr(6),istar,istep
	common /dats/ ncurv,jform,xlims(2),ylims(2),numxy
	common /text/ itcolr,itsize
	character cmd*4,c*1
	equivalence (cmd,c)
c
	real x(*),y1(*),y2(*)
	real ypts(7)	/40.,10.,30.,20.,25.,25.,25./
	integer numxy /6/
	integer ncall /0/
c
	call at_int(
     >	  '  form(1=long,2=short/long,3=short/non,4=base/non,5=non-std)',
     >		jform)
	call at_int('  number of points',numxy)
	call at_real('  x start',xfir)
	call at_real('  increment ',xdel)
	call at_real('  y multiplier ',ymag)
	call at_real('  y offset ',yoff)
	return
c
	entry data1(x,xmax,y1,y2,nx,ny)
c
	data xfir /1./, xdel /.8/
	data ymag /1./, yoff /0./
c
	ncall=-1
	xmag=10.**(6*ncall)
	nx=0
	ny=0
	if(jform.ge.3) then
	  jj=0
	  ny=numxy
	else
	  jj=1
	  y1(1)=numxy
	  y2(1)=numxy
	endif
	if(jform.eq.1 .or. jform.eq.5) then
	  do ii=1,numxy
	    kk=ii+jj
	    x(kk)=xfir+xdel*(ii-1)
	    x(kk)=x(kk)*xmag
	  enddo
	  if(jform.eq.1) then
	    x(1)=numxy
	  else	!jform.eq.5
	    nx=numxy
	  endif
	elseif(jform.eq.2 .or. jform.eq.3) then
	  x(1)=-1.
	  x(2)=numxy
	  x(3)=xfir*xmag
	  x(4)=xdel*xmag
	else	!jform=4
	  x(1)=numxy
	  x(2)=xfir*xmag
	  x(3)=xdel*xmag
	  nx=numxy
	endif
	ydel=6./float(numxy)
	n6=numxy/6
	do ii=1,numxy
	  kk=ii+jj
	  iuse=(ii-1)/n6+1
	  ins=mod(ii-1,n6)
	  yval=ypts(iuse)+(ypts(iuse+1)-ypts(iuse))*ydel*(ins)
	  y1(kk)=yoff+yval*ymag
	  y2(kk)=y1(kk)-10.
	enddo
c
	return
c
	entry datai(x,xmax)
c
	ncall=ncall+1
	xmag=10.**(6*ncall)
	if(jform.ge.3) then
	  jj=0
	else
	  jj=1
	endif
	if(jform.eq.1 .or. jform.eq.5) then
	  do ii=1,numxy
	    kk=ii+jj
	    x(kk)=xfir+xdel*(ii-1)
	    x(kk)=x(kk)*xmag
	  enddo
	elseif(jform.eq.2 .or. jform.eq.3) then
	  x(3)=xfir*xmag
	  x(4)=xdel*xmag
	else
	  x(2)=xfir*xmag
	  x(3)=xdel*xmag
	endif
	return
	end
c%
	subroutine xaxiop
	common /xopts/ lxlog,lxform,lxneat,lxtic,lgcolr,lclip,lskip,lxwid
	common /yopts/ lylog,lyform,lyneat,lytic
	common /curv/ isymb(6),iline(6),icolr(6),istar,istep
	common /dats/ ncurv,jform,xlims(2),ylims(2),numxy
	common /text/ itcolr,itsize
	character cmd*4,c*1
	equivalence (cmd,c)
c
	call at_int('  log (1=linear,2=log)',lxlog)
	call at_int(
     >	  '  tic form (2=out,3=in,4=thru,5=long thru,6=long in)',lxform)
	call at_int('  label width',lxwid)
	call at_int('  neat (0=edges,1=minor,2=major)',lxneat)
	call at_int('  tic labels (0=none,1=ticks,2=ends)',lxtic)
	call at_int('  skip factor',lskip)
	call at_int('  axis color',lgcolr)
	return
	end
c%
	subroutine curvop
	common /xopts/ lxlog,lxform,lxneat,lxtic,lgcolr,lclip,lskip,lxwid
	common /yopts/ lylog,lyform,lyneat,lytic
	common /curv/ isymb(6),iline(6),icolr(6),istar,istep
	common /dats/ ncurv,jform,xlims(2),ylims(2),numxy
	common /text/ itcolr,itsize
	character cmd*4,c*1
	equivalence (cmd,c)
c
	do ii=1,ncurv
	  call at_int('  line  ',iline(ii))
	  call at_int('  symbol',isymb(ii))
	  call at_int('  color ',icolr(ii))
	enddo
	call at_int('  symbol start',istar)
	call at_int('  symbol step',istep)
	call at_int('  1 to clip, 0 not to',lclip)
	return
	end
c%
	subroutine lablop
	common /curv/ isymb(6),iline(6),icolr(6),istar,istep
	common /dats/ ncurv,jform,xlims(2),ylims(2),numxy
	common /text/ itcolr,itsize
	character cmd*4,c*1
	equivalence (cmd,c)
c
	call at_int('  text size',itsize)
	call at_int('  text color',itcolr)
	return
	end
	subroutine termop
	character cmd*4,c*1
	equivalence (cmd,c)
c
	data jterm /0/, jsres /1024/
c
	call at_int('  terminal type (0=4014,1=4105)',jterm)
	call at_int('  screen resolution (1024,4096)',jsres)
	call tminit(jterm)
	call term(0,jsres)
	return
	end
	subroutine aglegnd(legtyp,nlines,texts,nchrs,ksymbl,kdashs)
c
c  Put a legend for multiple curves in the top right or left of a plot.
c  Each line of the legend has a short piece of line with one symbol
c  (if any) and a short text describing the line.
c
c  arguments:
c
	integer legtyp	!where to put legend (1=top right, 2=top left)
	integer nlines	!number of lines in legend
	character*16 texts(nlines)	!array of legend texts
	integer nchrs		!text length to output
	integer ksymbl(nlines)		!array of symbol codes
	integer kdashs(nlines)		!array of dash lines codes
c
c  get parameters from sg common
	ltick = comget(ibasey(6))	!tick length
	symsiz = comget(ibasec(7))
	call seetw(lx,mx,ly,my)	!location of min/max x/y
	ihgt=linhgt(1)		!char height
	iwdt=linwdt(1)		!char width
c
c  output legend
	do ndrw=1,nlines
	  if(legtyp.eq.2) then		!inside top right
	    ihorz=mx-iwdt*nchrs-iwdt*3-ltick
	    ivert=my-ihgt*ndrw-ltick
	  elseif(legtyp.eq.1) then	!inside top left
	    ihorz=lx+ltick
	    ivert=my-ihgt*ndrw-ltick
	  endif
	  call movabs(ihorz,ivert+ihgt/2)
	  call dshrel(iwdt*3,0,kdashs(ndrw))
	  if(ksymbl(ndrw).ne.0) then
	    call movrel(-iwdt,0)
	    call symout(ksymbl(ndrw),symsiz)
	    call movrel(iwdt,0)
	  endif
	  call movrel(4,-ihgt/2)
	  call agths(texts(ndrw)(1:nchrs))
	enddo
	return
	end
