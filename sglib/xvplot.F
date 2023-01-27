c  xvplot.for	subroutines for both xplot and vplot
c
c----------------------------------------------------------------/set_input
	subroutine set_input(fspec,fname,l4105,linit)
	implicit none

	integer linit
	character*(*) fspec
	character fname*(*)

	include 'xplot_inc'
	character fopts*64
	character match*16
	character*1 back
	logical lfile,l4105

        integer lf, ibrak, idot, ios, lq, lpo, lpc
	integer num,lx,    iopt
	integer jc_trim

	data fopts	/' '/
c
	lfile = .false.		!no input file yet
	fopts = ' '
	fname = ' '
c
	do while (.not.lfile)	!get a good input file
	  if(fspec.eq.' ')
     >    call at_text('input plot file',fspec)
	  if(fspec.eq.' ') go to 110
	  lf = index(fspec,' ')
	  if(fspec(lf:).ne.' ') then
	    call at_msg(' ** enter as  PLOTFILE.PLT[/F=n1,..]')
	    fspec = ' '
	    go to 110
	  endif
	  if(lf.gt.1) lf = lf-1
	  if(fspec(1:1).eq.',') then
	    fspec = fspec(2:)		!check for ",xxx" or "xxx,"
	  elseif(fspec(lf:lf).eq.',') then
	    fspec(lf:) = ' '
	  endif
	  lf = index(fspec,'/')-1	!strip off options
	  if(lf.le.0) lf = jc_trim(fspec)
	  fname = fspec(1:lf)
	  fopts = fspec(lf+2:)
	  call set_open(fname,ios)
	  if(ios.ne.0) then
	    fspec = ' '
	    call at_reset
	  else
	    lfile = .true.
	  endif
 110	enddo
c
	ibrak = index(fname,']')
	idot = index(fname(ibrak+1:),'.')+ibrak
	l4105 = .false.
	linit = 10		!length of initialization sequence
	if(idot.ne.0) then
	  if(fname(idot+1:idot+4).eq.'4105') then
	    l4105 = .true.
	    linit =  100
	  endif
	endif
	inblk = 0
c
 120	nfn = 0
	if(fopts.eq.' ') go to 200
	call at__upcase(fopts,fopts)
	iopt = index('FT',fopts(1:1))	!convert option letter to index
	if(iopt.eq.0) then
	  call at_msg(' ** file options are: /F=n1[,n2..] or /T')
	  call at_text('enter option',fopts)
	  go to 120
	endif
	lq = index(fopts,'=')
	if(lq.eq.0) then
	  fopts = ' '
	else
	  fopts = fopts(lq+1:)
	endif
c
	lpo = index(fopts,'(')
	if(lpo.ne.0) fopts(lpo:lpo) = ' '
	lpc = index(fopts,')')
	if(lpc.ne.0) fopts(lpc:lpc) = ' '
c
	lf = jc_trim(fopts)
	if(fopts.ne.' ') then
	  if(iopt.eq.1) then	!for frame numbers
	  call at_macro('/B')	!will get this after user input
	  endif
	  call at_macro(fopts(1:lf))
	endif
c
	nftop = 0
	if(iopt.eq.1) then	!select frame numbers
	  num = 1
	  call at_bint('enter frame number(s)',num,back)
	  do while(back.eq.' ')
	    if(num.gt.0) then
	    nfn = nfn+1
	    fnlist(nfn) = num
	    if(lstop) nftop = max(nftop,num)
	    endif
	    call at_bint('next frame (/B to end)',num,back)
	    if(nfn.ge.maxfn-1) back = 'B'
	  enddo
	  if(nfn.eq.0) then
	    call at_msg(' ** frame list empty - will process all frames')
	  endif
	elseif(iopt.eq.2) then
	  nfn = -1		!match text
	  ntx = 0
	  match = ' '
	  do while(match.eq.' ')
	    call at_text('enter text',match)
	    lx = jc_trim(match)
	  enddo
	  ntx = ntx+1
	  txlist(ntx) = match
	  fnlist(ntx) = lx
	endif
 200	return
c
	entry get_input
	return
	end
c
	subroutine set_open(fname,ios)
	character fname*(*)
	character fopen*64
	logical igood
c
	lf = jc_trim(fname)
	ibrak = index(fname,']')
	idot = index(fname(ibrak+1:),'.')+ibrak
	if(idot.eq.0) then
	  call usr_trnlog(' ',fname(:lf),fopen,lf,igood)
	  if(.not.igood) fopen = fname
	else
	  fopen = fname
	endif
c
	lf = jc_trim(fopen)
	call tname_disk(fopen(1:lf))  !   
	call tview_disk  !   
	ios=0  !   
	return
	end
c
	subroutine set_temp(ifr,lwout)
	implicit none
c
	logical lwout
	include 'xplot_inc'
	include 'sgvgds_inc'
	integer ifr, ii, istat
c
	if(nfn.gt.0) then	!wait for frame match
	  ltemp = .false.	!dont create vgds temp file
	  if(nftop.gt.0 .and. ifr.gt.nftop) then
	    kpage = 99999	!if at end of list, all done
	    return
	  endif
	  do ii=1,nfn		!see if frame number in list
	    if(ifr.eq.fnlist(ii)) ltemp = .true.
	  enddo
	  lkeep = ltemp
	elseif(nfn.eq.0) then	!no frame/text match, or found text match
	  ltemp = .true.
	  lkeep = .true.
	  if(nftop.gt.0 .and. ifr.gt.nftop) kpage = 99999
	else !nfn.lt.0		!wait for text match
	  ltemp = .true.	!create vgds temp file
	  if(lttyo) ltemp = .false.	!delay if only to tty
	  lkeep = .false.
	endif
	if(ltemp) then
	  iovgds = iosave
	  if(.not.lttyo) then
	    call ag_inio('VGDS',istat)
	  endif
	  lwout = .true.
	else
	  iovgds = 0		!throw away frame
	  lwout = .false.
	endif
	end
c
	subroutine get_char(khar,ios)
	implicit none

	include 'xplot_inc'
	integer hex0, khar
	integer i, ngot, ios
	data hex0/z'00'/	
c
	if(inptr.gt.incnt) then		!if buffer empty, fill it
	  do i=1,mfbuf
#ifdef __f90
	     fbuf(i)=char(hex0)
#else
	     fbuf(i) = 0
#endif
	  enddo
	  inblk = inblk+1
	  call tread_disk(fbuf,mfbuf,inblk,ngot)  !   
c	  call tread_disk(fbuf,%val(mfbuf),%val(inblk),ngot)  !   
	  ios = 0  !   
	  if(ngot.eq.0) ios = -1 !   
	  if(ios.eq.36) ios = -1	!no such block => eof
	  if(ios.lt.0) return	!?ios.ne.0
c				!reset ptr unless set by vskip
	  if(inptr.eq.incnt+1) inptr = 1
	  incnt = mfbuf		!recalc incnt
#ifdef __f90
	  do while (fbuf(incnt).eq. char(hex0))
#else
	  do while (fbuf(incnt).eq.0)
#endif
	    incnt = incnt-1
	  enddo
	endif
#ifdef __f90	
	khar = ichar(fbuf(inptr))
#else
	khar = fbuf(inptr)
#endif
	inptr = inptr+1
	return
	end
c
	subroutine ck_match(khar,nfo,lwout)
	implicit none
c
	logical lwout
	include 'xplot_inc'
	integer  nfo, ii, khar, jj

#ifdef __f90
	integer(1) mbuf(16)
#else
	byte mbuf(16)		!match buffer
#endif
c
	if(nfo.eq.0) then
	  do ii=1,16
	    mbuf(ii) = 0
	  enddo
	endif
	do ii=2,16		!shuffle buffer left  !   
	  mbuf(ii-1)=mbuf(ii)  !   
	enddo  !   
	mbuf(16) = khar		!store new character
	do jj=1,ntx		!compare bytes in plot file to user text
	  do ii=1,fnlist(jj)
	    if(mbuf(ii).ne. ichar(txlist(jj)(ii:ii))) go to 100	!cycle
	  enddo
	  lkeep = .true.	!have a match
	  if(lstop) nftop = kpage	!stop after this frame
	  nfn = 0		!stop text search
c				!if no output yet to tty
	  if(lttyo .and. (.not.ltemp)) then
	  inblk = spage(1,kpage)-1	!back up to start of frame
	  inptr = spage(2,kpage)
	  incnt = 0		!force read
	  call set_temp(kpage,lwout)	!enable output
	  nfo = 0
	  endif
	  go to 200	!leave
 100	enddo
 200	continue
	end
c
c
	subroutine xout(khar)
	implicit none
	include 'xplot_inc'
	character eseq*120
	integer ls, is, lt, khar, irot, ix, linhgt, iy
	integer linwdt, ksize, iff, ihalf, ii, ip
	integer iang, idiam, ic30, is30
	real    rod
	integer ipos(8)
	logical is_cap

	save ls,ip,irot,ix,iy

	data eseq/' '/
c
	if(li4105 .and. (.not.lo4105)) then
c					!input=4105, output=4014
	  if(kmode.eq.kescm) then		!escape mode
	    if(khar.ge.ichar(' ').and.ls.lt.120) then
	      ls = ls+1		!collect bytes until ctrl char
	      eseq(ls:ls) = char(khar)
	      return
	    else
	    if(ls.eq.0) then		!if empty
	      call toutpt(27)		!just output esc
	      kmode=kalfm
	    elseif(eseq(1:2).eq.'LT') then	!text
	      is = 3
	      call getint(eseq,is,lt)	!get length
	      call resets
	      if(irot.eq.0) then
	      call xoutst(eseq(is:),lt,lo4105)	!min(lt,ls-is)
	      else
	      call movabs(ix-linhgt(1),iy+linwdt(lt))
	      call agtvs(eseq(is:),lt)
	      endif
	      ls = 0
	    elseif(eseq(1:2).eq.'MC') then	!char size
	      is = 3
	      call getint(eseq,is,ksize)
	      if(ksize.lt.33) then	!51*.68 = 34
		call chrsiz(3)		!small
	      else
		call chrsiz(1)		!large
	      endif
	      ls = 0
	    elseif(eseq(1:2).eq.'MR') then	!rotation
	      is = 3
	      call getint(eseq,is,irot)
	      if(irot.eq.180) irot=0
	      ls = 0
	    elseif(eseq(1:2).eq.'DA') then	!maccircle
	      is = 3
	      call getint(eseq,is,iff)	!1=frame,2=fill,3=both
	      call getint(eseq,is,iang)	!angle = 0 to 360
	      call getint(eseq,is,iang)
	      call getint(eseq,is,idiam)	!4*rod
	      call getint(eseq,is,idiam)
	      ls = 0
	      rod=real(idiam)*.25
	      ic30=nint(rod*.8660254)
	      is30=nint(rod*.5)
	      ihalf=nint(rod)
	      call resets
	      call movabs(ix,iy)
	      call movrel(ihalf,0)
	      call drwrel(ic30-ihalf,is30)
	      call drwrel(is30-ic30,ic30-is30)
	      call drwrel(-is30,ihalf-ic30)
	      call drwrel(-is30,ic30-ihalf)
	      call drwrel(-ic30+is30,is30-ic30)
	      call drwrel(-ihalf+ic30,-is30)
	      call drwrel(-ic30+ihalf,-is30)
	      call drwrel(-is30+ic30,-ic30+is30)
	      call drwrel(is30,-ihalf+ic30)
	      call drwrel(is30,-ic30+ihalf)
	      call drwrel(ic30-is30,-is30+ic30)
	      call drwrel(ihalf-ic30,0+is30)
	      call movabs(ix,iy)
	    elseif(eseq(1:2).eq.'LP') then	!panel = filled symbol
	      call toutpt(kvecm)	!vector mode
	      do ii=5,ls		!output moves
		call toutpt(ichar(eseq(ii:ii)))
	      enddo
	      ls = 0
	    elseif(.not.is_cap(ichar(eseq(1:1)))) then	!4014 cmd
	      call toutpt(27)		!replace esc
	      do ii=1,ls		!output string
		call toutpt(ichar(eseq(ii:ii)))
	      enddo
	      ls = 0
	    endif
	    kmode=kalfm
	    ls=0
	    endif
	  elseif(kmode.eq.kvecm) then
	    ip = ip+1
	    ipos(ip) = khar
c	    if((khar/'40'o).eq.2) then
	    if((khar/32).eq.2) then
	      call getxy(ipos,ip,ix,iy)
	      ip = 0
	    endif
	  endif
	elseif((.not.li4105) .and. lo4105) then
c					!input=4014, output=4105
	  if(kmode.eq.kescm) then
	    if(khar.eq.27) return	!after esc ff
	    kmode=kvecm
	    if(khar.gt.55 .and. khar.le.59) then
	      call chrsiz(khar-55)	!set character size
	      return
	    endif		!96-104 = dash type
	    call toutpt(27)		!replace esc
	  elseif(kmode.eq.kalfm) then
	    if(khar.ge.ichar(' ')) then
	      ls=ls+1
	      eseq(ls:ls)=char(khar)
	      return
	    elseif(ls.gt.0) then
	      call xoutst(eseq,ls,lo4105)
	      ls=0
	    endif
	  endif
	endif
c
	if(khar.eq.31) then
	  kmode = kalfm		!US -> alpha mode
	elseif(khar.eq.29) then
	  kmode = kvecm		!GS -> vector mode
	  ip = 0
	elseif(khar.eq.28) then
	  kmode = kpntm		!FS -> point mode
	elseif(khar.eq.27) then
	  kmode = kescm		!ECS -> escape mode
	endif
c
	if(khar.eq.07) then
c		!ignore bell
	elseif(ldiff .and. kmode.eq.kescm) then
c		!hold escape seqs
	else
	  call toutpt(khar)
	endif
	return
	end
c
	subroutine getint(str,is,int)
	implicit none

	character str*(*)
	integer is,int
	integer khar

	int = 0
	khar = ichar(str(is:is))
	is = is+1
	if(khar.gt.64) then
	  int = khar-64
	  khar = ichar(str(is:is))
	  is = is+1
	endif
	int = int*16 + (khar-48)
	return
	end
c
	subroutine getxy(ipos,lp,ix,iy)
	implicit integer (a-z)
	integer ipos(*)
	logical noy
c
c init pieces
c	highx=ix/'40'O
	highx=ix/32
	highy=iy/32
c	lowx=ix.and.'37'O
	lowx=iand(ix,31)
	lowy=iand(iy,31)
	noy=.true.
	ip=0
c  get next nibble
 100	continue
	ip=ip+1	!?check ip<lp
	ich=ipos(ip)
c  high 2 bits are indicator, low 5 bits are address
	msb=ich/32
	lsb=iand(ich,31)
	if(msb.eq.1.and.noy) then
	  highy=lsb
	  noy=.false.
	  go to 100
	elseif(msb.eq.3) then
	  lowy=lsb
	  noy=.false.
	  go to 100
	elseif(msb.eq.1) then
	  highx=lsb
	  go to 100
	elseif(msb.eq.2) then
	  lowx=lsb
	endif
c  compute new coordinates
	ix=highx*32 + lowx
	iy=highy*32 + lowy
	if(ix.gt.1024) ix=1024
	if(iy.gt.780)  iy=780
	return
	end
c
	subroutine xoutst(str,ns,l4105)
	implicit none
        integer ns
	character str*(*)
	logical l4105
	integer ii

	if(l4105) then
	  call cmdout('LT')
	  call intout(ns)
	else
	  call toutpt(31)	!US
	endif
	do ii=1,ns
	  call toutpt(ichar(str(ii:ii)))
	enddo
	return
	end
c
	subroutine resets
	implicit none

	include 'sgtcs_inc'
        integer ii

	do ii=1,4
	  kpchar(ii)=-1
	enddo
	end

	logical function is_cap(khar)
 	is_cap = (khar.ge.ichar('A') .and. khar.le.ichar('Z'))
	return
	end
c
	subroutine at_vaxer(msg,icode)
	implicit none

	integer       icode
	character*(*) msg
	character*80 text
	integer  lx, ltext, ierr

	write(text,'(i2)',iostat=ierr) icode  !   
	ltext = 2  !   
	call at_msg('  '//msg)
	call at_msg('  '//text(1:ltext))
	end
