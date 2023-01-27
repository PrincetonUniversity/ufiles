c  xplot.for	display a plot file on a terminal
c
c
c  06/10/98  CAL: On unix, use XPLOT_OUTPUT (can not include "$")
c 
c  jan-89	build program from macro predecessor.
c		accept plot file, frame selection (opt), destination
c		list (opt), and display frame(s) on tty; allow user to
c		skip or goto a specified frame. [met]
c
c  accept plot file, frame selection (opt), destination list (opt)
c    display frame(s) to screen and allow user to skip or goto a
c    specified frame
c
c----------------------------------------------------------------/xplot
	program xplot
	implicit none
c
	include 'xplot_inc'
	integer hex7f, hexdf
	character fspec*128
	character fname*64
	character dests*255
	character optc*1 	!   
	data optc       /'-'/	!   
	character plout*64
	character c4105*4
	logical lnoq,lwout,lplot
	logical lnews,igood

	integer na, ld, ls, ll, le, ios
	integer linit, lout, lx, istat, kstat, nfo, khar
	integer ierr, itypd, k4105, if, ic, is
	integer jc_trim

	data hex7f /z'7f'/, hexdf /z'df'/
	data fspec	/' '/
	data dests	/' '/

c
c  process command line switches
	call at_msg('XPLOT - 01/10/94; help xplot for info')
	lstop = .false.		!xplot mode
	lnoq = .false.		!query at end of file
	lwait = .false.		!wait for vgds screen
	lttyo = .true.		!output only to tty
c
	fspec = ' '		!get command line and strip options
	call lib_get_foreign(fspec,na,ld)
	do while(fspec(1:1).eq.optc)	!check for /noquery /wait /lwid
	  ls = index(fspec,' ')
	  ll = index(fspec(2:ls),optc)
	  if(ll.gt.0) ls = ll
	  if(fspec(2:4).eq.'NOQ') then
	    lnoq = .true.
	  elseif(fspec(2:5).eq.'WAIT') then
	    lwait = .true.
	  elseif(fspec(2:3).eq.'LW') then
	    le = index(fspec,'=')+1
	    if(le.gt.1) read(fspec(le:le),'(i1)',iostat=ios) lwid
	  else
	    call at_msg(' ** invalid option ignored: '//fspec(1:ls))
	  endif
	  fspec = fspec(ls+1:)
	enddo
c
	lplot = .false.
	if(fspec.eq.' ') then	!if no file on command line
	  fspec = 'PLOT'	!use logical PLOT
	  lplot = .true.
	endif
	  call at_reset		!prevent TIC from calling get_foreign
	ls = index(fspec,optc)-1	!translate input file name
	if(ls.le.0) ls = jc_trim(fspec)
	call usr_trnlog(' ',fspec(:ls),dests,ld,igood)
	if(igood) then
	  fspec = dests(:ld)//fspec(ls+1:)
	elseif(lplot) then
	  fspec = ' '
	endif
c
c  process file spec
 80	continue
	call set_input(fspec,fname,li4105,linit)
	if(lwait) nfn = 0	!if slaved, ignore /F /T
c
	call usr_trnlog(' ','XPLOT_OUTPUT',plout,lout,igood) !   
	if(.not.igood) plout='stdout' !   
	call usr_trnlog(' ','TERMINAL_4105',c4105,lx,igood)
c
	call rinitt(plout,istat)	!override PLOT
	if(li4105 .and. c4105.ne.'NO') call tminit(1)
	kstat = 0	!0=vt100, 1=Tek during header, 2=past header
c
c  start new file
 100	continue
	incnt = 0		!read pointer
	inptr = 1		!write pointer (incnt+1)
	inblk = 0
	nfo = 0			!number of chars output
	kpage = 1		!frame being processed
	kskip = 0
	lpage = 99999
	spage(1,1) = 1		!disk address of first frame
	spage(2,1) = 1
	call set_temp(kpage,lwout)
	if(kpage.ge.99999) go to 8000
	ios = 0
	kmode=kalfm		!start in alf mode
c
c  process next character
	do while (ios.ge.0)
	  lnews = .false.		!not a new screen
	  call get_char(khar,ios)
	  if(ios.lt.0) then
	    lpage = kpage		!eof - set marker
	    nfn = 0
	    ios = 0			!ignore error for now
	    incnt = 0
	    if(lwait) then		!if slaved wait for next file
	      call tsend
	      call set_open(fname,ierr)
	      go to 100
	    endif
	    lnews = .true.
	    go to 320			!dont set spage
	  endif
c
	  if(kmode.eq.kescm .and. khar.eq.12) then	!have a form feed
	    itypd = 0			!assume throw away
	    if(nfo.le.linit) then	!empty buffer
cx	      call agfkeep(0)		!throw buffer away
	      spage(1,kpage) = inblk	!save ptr to ff
	      spage(2,kpage) = inptr
	      nfo = 0
	      go to 380
	    endif
	    if(kpage.lt.lpage) kpage = kpage+1
	    spage(1,kpage) = inblk	!save ptr to ff
	    spage(2,kpage) = inptr
	    lnews = .true.
	    go to 320

	  else			!anything but a form feed
	    if(kpage.ge.kskip .and. lwout) then
	      if(kstat.eq.0) then
		call initt(0)		!open term, screen switch
		kstat = 1
		call agseet(k4105,if,ic,is)
		lo4105=(k4105.eq.1)
		ldiff=lo4105.neqv.li4105
		if(lo4105 .and. lwid.gt.0) then
		endif
	      endif
	      if(nfo.eq.0) then
		call erase		!resets line width,style,color
		if(lwid.gt.0) then	!if for HC0 printer
		call cmdout('MW')	!set line width
		call intout(lwid)
		call cmdout('MQ')	!set string precision (vs stroke)
		call intout(1)
		endif
	      endif
	      call xout(khar)
	    endif
	    nfo = nfo+1
	    if(nfn.eq.-1 .and. .not.lkeep) then	!?.and. nfo.gt.linit
	      call ck_match(khar,nfo,lwout)
	    endif
	    if(khar.eq.27) kmode = kescm
	  endif
c
 320	  if(lnews) then		!new screen
	    if(ltemp .and. kpage.gt.kskip) then
	      call movabs(0,0)		!put cursor at bottom
	      call upause(0)
	      call upauch(itypd)
	    endif
c			!turn off /f= when frame is found
	    if(ltemp .or. kpage.ge.lpage) nfn = 0
c			!convert to upper case
	    itypd = iand(itypd,hex7f)	!drop parity
	    if(iand(itypd,64).ne.0) itypd = iand(itypd,hexdf)
c			!if "D" and output open, process as CR
	    if(itypd.eq.ichar('D') .and. lout.gt.0) itypd = 13
c			!if at eof, process CR same as alpha character
	    if(kpage.ge.lpage .and. itypd.lt.ichar(' '))
     >		 itypd = ichar(' ')
c			!if noquery, just stop
	    if(kpage.ge.lpage .and. lnoq .and. lttyo) go to 400
c			!if alpha char, ask user what to do
	    if(lttyo .and. itypd.ge.ichar(' ')) then
	      call finitt(0,0)
	      kstat=0
	      call set_frame(itypd)	!ask user what to do
	      if(kpage.ge.99999) go to 400	!leave
	      if(kpage.eq.99998) then
		fspec=' '
		go to 80
	      endif
	    endif
c
	    call set_temp(kpage,lwout)
	    if(kpage.ge.99999) go to 400	!leave
	    nfo = 0
	    kmode = 0
	  endif
c
 380	enddo
 400	continue
 8000	continue
	end
c
	subroutine set_frame(itypd)
	implicit none
c
	include 'xplot_inc'
	character cmd*1,ucmd*1
	integer   itypd, igo, kk

	data ucmd /'X'/
c
	if(kpage.eq.lpage)
     >		call at_msgi(' ** at end of file; page',lpage)
	call iag_upcase(cmd,char(itypd))
	igo = 0
	do while (igo.eq.0)
	  igo = index('FBTXNS',cmd)
	  if(igo.eq.5) then	!new file
	    kpage = 99998
	    return
	  elseif(igo.eq.4) then	!exit
	    kpage = 99999
	    return
	  elseif(igo.eq.3) then	!To frame
	    call at_int(' to frame number',kskip)
	    go to 120	!leave
	  elseif(igo.eq.2) then
	    call at_int(' back # frames',kskip)
	    kskip = kpage - kskip-1
	    go to 120	!leave
	  elseif(igo.eq.1 .or. igo.eq.6) then
	    call at_int(' skip # frames',kskip)
	    kskip = kpage + kskip
	    go to 120	!leave
	  endif
	  call at_char(
     >      'skip Forward, Backward, To frame, New file, eXit',
     >      ucmd)
	  cmd = ucmd
	enddo
 120	continue
	kskip=max(kskip,1)
	kskip=min(kskip,lpage)
	if(kskip.gt.maxpage) then
	  call at_msgi(' ** setting page to maxpage =',maxpage)
	  kskip = maxpage
	endif
	if(spage(1,kskip).ne.0) then
	  inblk = spage(1,kskip)-1
	  inptr = spage(2,kskip)
	  incnt = 0		!force read
	  kpage = kskip
	  kskip = 0
	elseif(kskip.gt.kpage) then
	  kk=kskip-1		!go to last page read
	  do while (kk.ge.kpage .and. spage(1,kk).eq.0)
	    kk=kk-1
	  enddo
	  if(kk.gt.kpage) then
	    inblk = spage(1,kk)-1
	    inptr = spage(2,kk)
	    incnt = 0
	    kpage = kk
	  endif
	endif
	end
c
