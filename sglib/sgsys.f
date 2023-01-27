c  file: sgsys.for      system level routines for sglib
c
c
c  mar-93	split off from sgiof.for.  [met]
c
c-----------------------------------------------------------------/tsend
	subroutine tsend
c  dump disk and terminal buffers
c
	implicit none
c
	include 'sgiof_inc'	!sg_flag, sg_buff
	include 'sgvgds_inc'	!iovgds,xpndsk
c
	character*(*) dsknam
	character*(*) msg
	logical ltrap
c
	include '($ssdef)'
	include '($fabdef)'
	include '($namdef)'
	record /fabdef/ ofab
	record /namdef/ onam
c
	include '($fibdef)'
	include '($iodef)'
	include 'sgfat_inc'
	record /fibdef/ ofib
	record /fatdef/ ofat	!file attributes block
	integer ofibd(2)	!descriptor for ofib
	structure /atrdef/
	  integer*2 atr$w_size		!  size of attribute in bytes
	  integer*2 atr$w_type		!  attribute type code
	  integer*4 atr$l_addr		!  address of attribute text
	  integer*4 end
	endstructure
	record /atrdef/ attb
	integer*2 iosbd(4),iosbt(4),iosbx(4)
	integer kdemp(2)		!size of empty file
c
cx	include '($ttdef)'
cx	include '($tt2def)'
	integer tt$m_mbxdsabl /'00010000'X/
	integer tt$m_nobrdcst /'00020000'X/
	integer tt2$m_brdcstmbx /'00000010'X/
cx	integer ss$_devactive /'02c4'X/
c
	character*64 trmnam,auxnam
	character*64 fibnam /' '/
	integer kxqef /0/
	integer ttset(3)	!tty characteristics during plotting
	integer ttsens(3)	!tty characteristics when assigned
	logical both
	logical usembx /.true./

	integer  maxmsg, maxbuf, kdblk, ioflag
	integer  igood, kdgot, kff, kdb
	integer  maxds, kdput, lvnam, llnam, lmsg, ii, ld
	integer  kdalc, kdpup, lsnam, ltnam
	integer  lib$get_ef, iag_trim
	integer  lib$asn_wth_mbx
        integer  lib$spawn

	parameter (maxmsg=20+120)	!max mailbox msg (20 bytes overhead)
	parameter (maxbuf=maxmsg*10)	!system dynamic memory for mbx msgs
	byte mbox(maxmsg)
	character*4 bsiz		!logical for buffer size
c
	integer i, spawn_sts
	character*200 command
c
	integer sys$parse
	integer sys$assign
	integer sys$qiow,sys$qio,sys$synch
c
	integer*4 msgvec(4)
     >	  /2,0,0,0/
c
	both = .true.
	go to 100
c
c---------------------------------------------------------------/tsend_disk
c
	entry tsend_disk
c
	both = .false.
 100	continue
	if(outdsk.gt.0 .and. kdptr.ne.0) then
c					!disk output
	  if((kdblk+maxds-1).gt.kdalc) then
	    ofib.fib$w_exctl = fib$m_extend .or. fib$m_alconb
	    ofib.fib$l_exsz = maxds
	    ofib.fib$l_exvbn = 0
c				!extend the file
	    ioflag = io$_modify
	    igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		ofibd,,,,,)
c
	    if(igood) igood = iosbd(1)
	    if(.not.igood) then
	      msgvec(2)=igood
	      call sys$putmsg(msgvec,,,)
	    endif
c
	    kdgot = iosbd(3)
	    kdalc = kdalc + kdgot
c				!erase allocated blocks
	    ioflag = io$_writevblk .or. io$m_erase
	    igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		0,%val(kdgot*512),%val(ofib.fib$l_exvbn),,,)
	    if(igood) igood = iosbd(1)
	    if(.not.igood) then
	      msgvec(2)=igood
	      call sys$putmsg(msgvec,,,)
	    endif
c
	  endif
c
	  kdpup=kdptr		!round up to even num of bytes
	  if(kdpup.and.1) then
	    kdpup=kdpup+1
	    dbuff(kdpup)=0
	  endif
c				!write this buffer
	  ioflag = io$_writevblk
	  igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >	    dbuff,%val(kdpup),%val(kdblk),,attb,)
c
	  if(igood) igood = iosbd(1)
	  if(.not.igood) then
	    msgvec(2)=igood
	    call sys$putmsg(msgvec,,,)
	  endif
c
	  ofib.fib$w_exctl = 0
	  kff=kdptr+1
	  if(kdptr.eq.maxdb) then
	    kdptr=0
	    kff=0
	    kdblk=kdblk+maxds
	  endif
	  kdb=kdblk
	  do while(kff.ge.512)
	    kff=kff-512
	    kdb=kdb+1
	  enddo
c
	  ofat.fat$w_efblkl = kdb
	  ofat.fat$w_ffbyte = kff
c				!modify file attributes
	  ioflag = io$_modify
	  igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		ofibd,,,,attb,)
c
	  if(igood) igood = iosbd(1)
	  if(.not.igood) then
	    msgvec(2)=igood
	    call sys$putmsg(msgvec,,,)
	  endif
c
	endif
c
	if(.not.both) return
	go to 200
c
c---------------------------------------------------------------/tsend_term
c
	entry tsend_term
c
	both = .false.
 200	continue
	if(outtrm.eq.1 .and. ktptr.ne.0) then
c					!terminal output
	  ioflag = io$_writevblk .or. io$m_noformat
	  igood = sys$qiow(,%val(ktqio),%val(ioflag),iosbt,,,
     >	    tbuff,%val(ktptr),,,,)
c
	  if(igood) igood = iosbt(1)
	  if(.not.igood) then
	    msgvec(2)=igood
	    call sys$putmsg(msgvec,,,)
	  endif
	endif
c
	if(outaux.eq.1 .and. ktptr.ne.0) then
c					!aux term output
	  if(kxqef.ne.0) then
	    igood = sys$synch(%val(kxqef),iosbx)
	    if(igood) igood = iosbx(1)
	    if(.not.igood) then
	      msgvec(2)=igood
	      call sys$putmsg(msgvec,,,)
	    endif
	  else
	    igood = lib$get_ef(kxqef)
	    if(.not.igood) call lib$stop(%val(igood))
	  endif
c
	  ioflag = io$_writevblk .or. io$m_noformat
	  igood = sys$qio(%val(kxqef),%val(kxqio),%val(ioflag),iosbx,,,
     >	    tbuff,%val(ktptr),,,,)
c
cx	  if(igood) igood = iosbx(1)
	  if(.not.igood) then
	    msgvec(2)=igood
	    call sys$putmsg(msgvec,,,)
	  endif
	endif
c
	ktptr=0
	return
c
c---------------------------------------------------------------/tback_disk
c
	entry tback_disk
c
	kdblk = kdsav(1)
	kdptr = kdsav(2)
	if(outdsk.le.0) return
c
	if(kdptr.ne.0) then
	  ioflag = io$_readvblk
	  igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >	    dbuff,%val(maxdb),%val(kdblk),,,)
c
	  if(igood) igood = iosbd(1)
	  if(.not.igood) then
	    msgvec(2)=igood
	    call sys$putmsg(msgvec,,,)
	  endif
	endif
c
	ofib.fib$w_exctl = fib$m_trunc
	ofib.fib$l_exsz = 0
	ofib.fib$l_exvbn = kdblk + 1
c
 320	kff = kdptr
	kdb=kdblk
	do while(kff.ge.512)
	  kff=kff-512
	  kdb=kdb+1
	enddo
	if(kff.ne.0) kff = kff+1
c
	ofat.fat$w_efblkl = kdb
	ofat.fat$w_ffbyte = kff
	ioflag = io$_modify
	igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		ofibd,,,,attb,)
c
	if(igood) igood = iosbd(1)
	if(.not.igood) then
	  msgvec(2)=igood
cx	  call sys$putmsg(msgvec,,,)	!ignore eof error
	endif
c
	kdalc = kdalc - ofib.fib$l_exsz
	return
c
c---------------------------------------------------------------/tkeep_disk
c
	entry tkeep_disk
c
	kdsav(1) = kdblk	!update start of frame addr
	kdsav(2) = kdptr
	ofib.fib$w_exctl = 0
	ofib.fib$l_exsz = 0
cx	go to 320
	return
c
c---------------------------------------------------------------/tback_term
c
	entry tback_term
c
	ktptr = 0
	return
c
c---------------------------------------------------------------/tname_disk
c
	entry tname_disk(dsknam)
c
	ofab.fab$b_bid = fab$c_bid
	ofab.fab$b_bln = fab$c_bln
	ofab.fab$l_fna = %loc(dsknam)
	ofab.fab$b_fns = iag_trim(dsknam)
	ofab.fab$w_ifi = 0
	ofab.fab$l_nam = %loc(onam)
c
	onam.nam$b_bid = nam$c_bid
	onam.nam$b_bln = nam$c_bln
	onam.nam$l_esa = %loc(xpndsk)
	onam.nam$b_ess = len(xpndsk)
	onam.nam$b_nop = nam$v_synchk
c
	igood = sys$parse(ofab)
	if(.not.igood) go to 9000
	return
c
c---------------------------------------------------------------/tname_term
c
	entry tname_term(dsknam)
c
	trmnam = dsknam
	return
c
c---------------------------------------------------------------/tname_auxl
c
	entry tname_auxl(dsknam)
c
	auxnam = dsknam
	return
c
c---------------------------------------------------------------/topen_disk
c
	entry topen_disk
c
	lvnam = ichar(onam.nam$t_dvi(1:1))	!length of device id
	igood = sys$assign(onam.nam$t_dvi(2:lvnam+1),kdqio,,)
	if(.not.igood) go to 9000
c
		!deny writes to others
	ofib.fib$l_acctl = fib$m_write .or. fib$m_nowrite
	ofib.fib$w_exctl = 0
	ofib.fib$w_nmctl = fib$m_newver
	ofib.fib$w_did(1) = onam.nam$w_did(1)
	ofib.fib$w_did(2) = onam.nam$w_did(2)
	ofib.fib$w_did(3) = onam.nam$w_did(3)
	ofib.fib$w_fid(1) = 0
	ofib.fib$w_fid(2) = 0
	ofib.fib$w_fid(3) = 0
c
	ofibd(1) = fib$k_length
	ofibd(2) = %loc(ofib)
	attb.atr$w_type = 4	!atr$c_recattr
	attb.atr$w_size = 32
	attb.atr$l_addr = %loc(ofat)
	attb.end = 0
c
	ofat.fat$b_rtype = fat$c_fixed + fat$c_sequential
	ofat.fat$w_rsize = 512
	ofat.fat$w_maxrec = 512
	ofat.fat$w_efblkl = 0
	ofat.fat$w_ffbyte = 0
c
	llnam = onam.nam$b_name + onam.nam$b_type
	lsnam = onam.nam$b_dev  + onam.nam$b_dir + 1
	fibnam = xpndsk(lsnam:)
	ioflag = io$_create .or. io$m_access .or. io$m_create
c			,chan,func,iosbd,,,
	igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
c		p1=fib,p2=file,p3=result length,p4=result string
     >		ofibd,fibnam(1:llnam),,xpndsk(lsnam:),attb,)
	if(igood) igood = iosbd(1)
	if(.not.igood) go to 9000
c
	ofib.fib$w_did(1) = 0	!inhibit lookup subfnc for modify
	ofib.fib$w_did(2) = 0
	ofib.fib$w_did(3) = 0
	ofib.fib$l_acctl = fib$m_writethru
c
	lxnam = index(xpndsk,char(0)) -1
	kdblk=1
	kdptr=0
	kdalc=0
	kdsav(1) = 1		!output file block
	kdsav(2) = 0		!word in file
	kdemp(1) = 0
	kdemp(2) = 0
c
c	if(dbufa.eq.0) then
c	  call usr_trnlog(' ','SG_DISK_BLOKS',bsiz,lb,igood)
c	  if(.not.igood) bsiz=' '
c	  read(bsiz,'(bn,i)',iostat=ios) maxds
c	  if(maxds.le.0) maxds = 30	!default is 30 blocks
	  if(maxds.le.0) maxds = 1	!default is 1 block
	  maxdb = maxds*512		!size in bytes
c	  igood = lib$get_vm(maxdb,dbufa)
c	  if(.not.igood) call lib$signal(%val(igood))
c	endif
	outdsk = 1		!flag file as open
c
cx	call toutpt('37'o)	!first file char for versatec
	return
c
c---------------------------------------------------------------/tmark_disk
c
	entry tmark_disk
c
	if(kdemp(1).eq.0) then
	  kdemp(1) = kdblk
	  kdemp(2) = kdptr
	endif
	return
c
c---------------------------------------------------------------/topen_term
c
	entry topen_term
c
	ltnam = iag_trim(trmnam)
	if(usembx) then
	  igood = lib$asn_wth_mbx(trmnam(1:ltnam),maxmsg,maxbuf,ktqio,
     >		ktmbx)
	endif
	if(.not.usembx .or. igood.eq.ss$_devactive) then
	  call sys$dassgn(%val(ktmbx))
	  ktmbx = 0
	  igood = sys$assign(trmnam(1:ltnam),ktqio,,)
	endif
	if(.not.igood) then
	  msgvec(2) = igood
	  call sys$putmsg(msgvec,,,)
	  type *, 'INITT/RINITT: cant open '//trmnam(:ltnam)
	  call exit(msgvec(2))
	endif
c
c
	if(ktmbx.ne.0) then
	  igood = sys$qio(,%val(ktqio),%val(io$_sensemode),,,,
     >		ttsens,%val(12),,,,)	!12 bytes = 3 longwords
	  if(.not.igood .or. (ttsens(2).and.tt$m_nobrdcst).ne.0) then
	    call sys$dassgn(%val(ktmbx))
	    ktmbx = 0
	    ttset(1) = 0
	  else
	    ttset(1) = ttsens(1)
	    ttset(2) = ttsens(2).or.tt$m_mbxdsabl.or.tt$m_nobrdcst
	    ttset(3) = ttsens(3).or.tt2$m_brdcstmbx
	  endif
	endif
c
	if(ktmbx.ne.0) then
	  igood = sys$qio(,%val(ktqio),%val(io$_setmode),,,,
     >		ttset,%val(12),,,,)
	endif
c
c	if(tbufa.eq.0) then
c	  call usr_trnlog(' ','SG_TERM_BYTES',bsiz,lb,igood)
c	  if(.not.igood) bsiz=' '
c	  read(bsiz,'(bn,i)',iostat=ios) maxtb
c	  if(maxtb.le.0) maxtb = 512	!default is 512 bytes
c	  maxtb = min(maxtb,512)	!don't allow bigger
c	  igood = lib$get_vm(maxtb,tbufa)
c	  if(.not.igood) call lib$signal(%val(igood))
c	endif
	outtrm = 1
	return
c
c---------------------------------------------------------------/topen_auxl
c
	entry topen_auxl
c
	ltnam = iag_trim(auxnam)
	igood = sys$assign(auxnam(1:ltnam),kxqio,,)
	if(.not.igood) then
	  msgvec(2) = igood
	  call sys$putmsg(msgvec,,,)
	  type *, 'INITT/RINITT: cant open '//auxnam(:ltnam)
	  call exit(msgvec(2))
	endif
c
c	if(tbufa.eq.0) then
c	  call usr_trnlog(' ','SG_TERM_BYTES',bsiz,lb,igood)
c	  if(.not.igood) bsiz=' '
c	  read(bsiz,'(bn,i)',iostat=ios) maxtb
c	  if(maxtb.le.0) maxtb = 512	!default is 512 bytes
c	  maxtb = min(maxtb,512)	!don't allow bigger
	maxtb = 512 !default is 512 bytes
c	  igood = lib$get_vm(maxtb,tbufa)
c	  if(.not.igood) call lib$signal(%val(igood))
c	endif
	outaux = 1
	return
c
c---------------------------------------------------------------/tclos_disk
c
	entry tclos_disk
c
	if(outdsk.le.0) return
	kff = kdptr
	kdb=kdblk
	do while(kff.ge.512)
	  kff=kff-512
	  kdb=kdb+1
	enddo
	if(kff.ne.0) kff = kff+1
c
	ofib.fib$w_exctl = 0
	ofat.fat$w_efblkl = kdb
	ofat.fat$w_ffbyte = kff
c				!close the file
	ioflag = io$_deaccess
	igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		ofibd,,,,attb,)
c				!if empty, delete it
	if(kdemp(1).eq.0) then
	  kdemp(1) = 1
	  kdemp(2) = 12
	endif
	if(kdblk.le.kdemp(1) .and. kdptr.le.kdemp(2)) then
	  ofib.fib$w_did(1) = onam.nam$w_did(1)	!delete dir entry
	  ofib.fib$w_did(2) = onam.nam$w_did(2)
	  ofib.fib$w_did(3) = onam.nam$w_did(3)
	  ioflag = io$_delete .or. io$m_delete
	  igood = sys$qiow(,%val(kdqio),%val(ioflag),iosbd,,,
     >		ofibd,xpndsk(lsnam:lxnam),,,,)
	  if(igood) igood = iosbd(1)
	  if(igood) lxnam = 0
	endif
c
	i=index(xpndsk,'.PLT')-1
	command = '@SOURCEROOT:[XTCTEK]TEK2PS_DCL.COM '//xpndsk(1:i)
        if (outps .eq. 1) then
	    igood = lib$spawn(command,,,,,,spawn_sts,,,,,)
	    if(igood .ne. SS$_NORMAL) then
		call lib$signal(%val(igood))
	    endif
	    if(spawn_sts .ne. SS$_NORMAL) then
		call lib$signal(%val(spawn_sts))
	    endif
	endif
	call sys$dassgn(%val(kdqio))
	outdsk = -1		!need to re-open
c
	return
c
c---------------------------------------------------------------/tclos_term
c
	entry tclos_term
c
	if(ktmbx.ne.0) then	!restore terminal characteristics
	  igood = sys$qio(,%val(ktqio),%val(io$_setmode),,,,
     >		ttsens,%val(12),,,,)
	  call sys$dassgn(%val(ktmbx))
	  ktmbx = 0
	endif
c
	if(outtrm.eq.1) then
	  call sys$dassgn(%val(ktqio))
	  outtrm = -1		!need to reopen
	endif
c
	if(outaux.eq.1) then
	  if(kxqef.ne.0) then
	    igood = sys$synch(%val(kxqef),iosbx)
	    call lib$free_ef(kxqef)
	    kxqef = 0
	  endif
	  call sys$dassgn(%val(kxqio))
	  outaux = -1		!need to reopen
	endif
	return
c
c-----------------------------------------------------------------/agtrap
c
	entry agtrap(ltrap)
c
c  enable/disable broadcast message trapping
c
	usembx = ltrap
	return
c
c-----------------------------------------------------------------/agtgbm
c
	entry agtgbm(msg,lmsg)
c  get broadcast messages held by system during broadcast trapping
c
	lmsg = 0
	if(ktmbx.eq.0) return	!no mailbox
c
	ioflag = io$_readvblk .or. io$m_now
	igood = sys$qiow(,%val(ktmbx),%val(ioflag),iosbt,,,
     >		mbox,%val(maxmsg),,,,)
	if(igood) igood = iosbt(1)
	if(igood) then
	  lmsg = mbox(21)	!actually 21:22
	  lmsg = min(len(msg),lmsg)
	  do ii=1,lmsg		!copy bytes 23:(22+lmsg)
	    msg(ii:ii) = char(mbox(22+ii))
	  enddo
	endif
	return
c				errors
 9000	continue
	msgvec(2) = igood
	call sys$putmsg(msgvec,,,)
	ld = iag_trim(dsknam)
	type *, 'INITT/RINITT: cant open disk file '//dsknam(:ld)
	call exit(igood)
c
	end
c
c-----------------------------------------------------------------/tinput
	subroutine tinput(inch)
c
	implicit none
	integer inch
	logical lspurg
c
	include 'sgiof_inc'	!sg_flag, sg_buff
	include 'sgvgds_inc'	!user (xtc)
c
	integer sys$assign,sys$qiow
	include '($iodef)'
	integer ss$_normal /'0001'X/
	integer ss$_partescape /'01fc'X/
	integer ss$_endoffile /'0870'X/
	character trmnam*64
	character xtcgin*64
c
	integer*4 sys$open,sys$connect,sys$get
	integer*2 iosb(4)
	byte tbufi(4)	!allow for 4 char escape sequence
	logical lpurg /.true./
c
	integer igood, iag_trim, ltnam, lsnam, lnam, ix, ierr, mbxout
	integer i, kqii, mbxin, ioflag, ionow
c
	include '($fabdef)'
	include '($rabdef)'
	record /fabdef/ ifab
	record /rabdef/ irab
c
	integer*4 msgvec(4)
     >	  /2,0,0,0/
	integer rms$_eof /'1827a'x/
c
c					!dump tty buffer if any
	if((outtrm.or.outaux).eq.1) call tsend_term
c
	if(inflg.eq.0) then		!first time thru
c				!determine input device
	  call usr_trnlog(' ','TERMINAL_INPUT',trmnam,lnam,igood)
	  if(trmnam.eq.'NONE') then
	    inflg = 3			!no input
	    go to 200
	  endif
	  if(trmnam.eq.' ' .and. xpntrm(1:3).eq.'MBA') then
	    	!try XTC V3 name first
	    call usr_trnlog(' ','XTC_GIN_'//user,trmnam,lnam,igood)
	    if(igood) go to 120
	    ix=index(xpntrm,':')-1
	    read(xpntrm(4:ix),'(i)',iostat=ierr) mbxout
	    do i=0,4
	      xtcgin='XTC_GIN_'//char(i+ichar('0'))//'_'//user
	      call usr_trnlog(' ',xtcgin,trmnam,lnam,igood)
	      if(igood) then
		ix=index(trmnam,':')-1
		read(trmnam(4:ix),'(i)',iostat=ierr) mbxin
		if(mbxin.gt.mbxout .and. mbxin.le.mbxout+2) go to 120
	      endif
	    enddo
	  endif
 120	  continue
	  if(trmnam.eq.' ') then	!translate sys$command
	    call usr_trnlog(' ','SYS$COMMAND',trmnam,lnam,igood)
	  endif
c				check device type
	  call usr$getdvi(trmnam(1:lnam),inflg,igood)
	  if(inflg.eq.0) then
	    inflg = 3
	    go to 200
	  endif
	  if(trmnam(1:3).eq.'MBA') inflg=5
cx  ignore disk name from def/user sys$command sys$input in a com file
cx	  if(inflg.eq.1 .and. index(trmnam(1:lnam),'[').eq.0) inflg=3
c
c				!open the input device
	  if(inflg.eq.1) then
c				device is a disk
	    ifab.fab$b_bid = fab$c_bid
	    ifab.fab$b_bln = fab$c_bln
	    ifab.fab$b_fac = 2**fab$v_get
	    ifab.fab$l_fna = %loc(trmnam)
	    ifab.fab$b_fns = lnam
	    ifab.fab$l_fop = 2**fab$v_sqo
c
	    igood = sys$open(ifab)
	    if(.not.igood) go to 9000
c
	    irab.rab$b_bid = rab$c_bid
	    irab.rab$b_bln = rab$c_bln
	    irab.rab$l_fab = %loc(ifab)
	    irab.rab$b_rac = rab$c_seq
	    irab.rab$l_rop = 2**rab$v_nlk .or. 2**rab$v_tpt
c
	    igood = sys$connect(irab)
	    if(.not.igood) go to 9000
c
	  elseif(inflg.eq.2 .or. inflg.eq.5) then
c				device is a terminal or mailbox
	    igood = sys$assign(trmnam(1:lnam),kqii,,)
	    if(.not.igood) go to 9000
c
	  endif
c
	endif
c
200	continue		!get input character
	if(inflg.eq.1) then
c
	  irab.rab$l_ubf = %loc(tbufi)
	  irab.rab$w_usz = 1
c
	  igood = sys$get(irab)
	  if(igood.eq.rms$_eof) then
	    tbufi(1)=ichar(' ')
	    igood = ss$_normal
	  endif
c
	elseif(inflg.eq.2) then
c
		!io$m_escape permits escape sequence from VersaTerm enter
	  ioflag = io$_readvblk .or. io$m_nofiltr .or. io$m_noecho
     >		.or. io$m_escape
	  if(lpurg) ioflag = ioflag .or. io$m_purge
	  igood = sys$qiow(,%val(kqii),%val(ioflag),iosb,,,
     >	    tbufi,%val(1),,,,)
	  if(igood) igood=iosb(1)
	  if(igood.eq.ss$_partescape) igood = ss$_normal
c
	elseif(inflg.eq.5) then		!XTC mailbox
c
	  ioflag = io$_readvblk .or. io$m_nofiltr
	  if(lpurg) then
	    ionow = ioflag .or. io$m_now
cx	    iosb(1) = ss$_normal
	    do while (iosb(1).ne.ss$_endoffile)	!iosb(1).eq.ss$_normal
	      igood = sys$qiow(,%val(kqii),%val(ionow),iosb,,,
     >		tbufi,%val(1),,,,)
	    enddo
	  endif
	  igood = sys$qiow(,%val(kqii),%val(ioflag),iosb,,,
     >	    tbufi,%val(1),,,,)
	  if(igood) igood=iosb(1)
c
	else	!inflg.eq.3
c
	  tbufi(1) = 13		!cr
	  igood = ss$_normal
	endif
	if(.not.igood) then
	  msgvec(2)=igood
	  call sys$putmsg(msgvec,,,)
	  tbufi(1) = 0
	endif
	lpurg = .true.
c
	inch = tbufi(1)
	return
c
c-----------------------------------------------------------------/agtpurg
	entry agtpurg(lspurg)
c
	lpurg = lspurg
	return
c
 9000	continue
	call agsalf
	msgvec(2)=igood
	call sys$putmsg(msgvec,,,)
	lnam = iag_trim(trmnam)
	type *, 'TINPUT: cant open input device '//trmnam(1:lnam)
	call exit(igood)
	end
c
c-----------------------------------------------------------------/iowait
	subroutine iowait(itenth)
c
	implicit none
c  arguments:
c	itenth - integer duration in tenths of seconds
c
	include 'sgtcs_inc'	!model
	include 'sgiof_inc'	!sg_flag, sg_buff
c
	integer itenth, igood, istat, ltabl
	integer sys$setef,sys$setimr,sys$wfland
	integer deltat(2) /-1,-1/
c
	integer timflg
	parameter (timflg=1)
	integer wflmsk
	parameter (wflmsk=2**1)
c
	integer*4 msgvec(4)
     >	  /2,0,0,0/
c
	if(outtrm.eq.1 .and. model(1:2).eq.'TK') then
c				!ignore if not tektronix
c
	  call tsend_term
	  deltat(1) = itenth * (-1000000)	!100-ns units
	  istat = sys$setef(%val(timflg))
	  istat = sys$setimr(%val(timflg),deltat,,)
	  if(.not.istat) then
	    msgvec(2)=istat
	    call sys$putmsg(msgvec,,,)
	  endif
c
	  istat = sys$wfland(%val(timflg),%val(wflmsk))
	  if(.not.istat) then
	    msgvec(2)=istat
	    call sys$putmsg(msgvec,,,)
	  endif
c
	endif
c
800	return
	end
c
c-------------------------------------------------------------/usr_trnlog
	subroutine usr_trnlog(table,loglnam,physnam,lphys,istat)
c
	implicit none
c  translate logical name to physical name
c
c  arguments:
	character*(*) table	!table to search
	character*(*) loglnam	!logical name to be translated
	character*(*) physnam	!(return) physical name
	integer       lphys	!(return) length of physical name
	integer       istat	!(return) status code
c
c  local variables:
	include '($lnmdef)'
cx	include '($ssdef)'
c
	character*64 holdnam
	character*16 ttabl
	integer*4 lhold		!length of logical name
	integer*4 kattr		!string attributes
c
	structure /itmlst/
	  integer*2 att_leng /4/		!length
	  integer*2 att_code /lnm$_attributes/	!code
	  integer*4 att_addr /0/
	  integer*4 att_desc /0/
	  integer*2 str_leng /64/		!length
	  integer*2 str_code /lnm$_string/	!code
	  integer*4 str_addr /0/
	  integer*4 str_desc /0/
	  integer*4 lnm_end  /0/
	endstructure
	record /itmlst/ trn
	logical more
cx	integer ss$_notran /'0629'X/
	integer sys$trnlnm
	integer ltabl, igood
	integer iag_trim
c
	ttabl = table			!set table name for translation
	if(ttabl.eq.' ') ttabl = 'LNM$FILE_DEV'
	ltabl = iag_trim(ttabl)
	holdnam = loglnam		!copy logical name to local
	lhold = iag_trim(holdnam)	!get length
c
	trn.att_addr=%loc(kattr)	!first item returns attributes
	trn.str_addr=%loc(physnam)	!second item returns name
	trn.str_desc=%loc(lphys)
	physnam=' '
	lphys=0
	istat=0			!no status yet
	more=.true.
c
	do while (more)
	  igood = sys$trnlnm(lnm$m_case_blind,ttabl(1:ltabl),
     >		holdnam(1:lhold),,trn)
c
	  if(igood) then		!if success
	    if(istat.eq.0) istat=1      !need to return 0 or 1
	    holdnam = physnam(1:lphys)	!set up to iterate
	    lhold = lphys
	    if((kattr.and.lnm$m_terminal).ne.0) more=.false.
	  else
	    more=.false.
	  endif
	enddo
c	  	  	  	  check for process perm file
	if(ichar(physnam(1:1)) .eq. '1b'x) then
	  physnam = physnam(5:lphys)	 !adjust length
	  lphys = lphys - 4
	endif
	physnam(lphys+1:) = ' '
c
	return
	end
c
c-------------------------------------------------------------/usr$getdvi
	subroutine usr$getdvi(physnam,klass,istat)
c
	implicit none
c  get device class for a physical device name
c
c  arguments:
	character*(*) physnam	!physical name of device
	integer       klass	!(return) device class
			! 0=unknown, 1=disk, 2=terminal, 5=mailbox(nl:)
	integer       istat	!(return) status code
c
c  local variables:
	include '($dvidef)'
	include '($dcdef)'
c
	integer sys$getdviw
c
	structure /itmlst/
	  integer*2 dvi_leng /4/	!length of response
	  integer*2 dvi_code /dvi$_devclass/	!code for device class
	  integer*4 dvi_bufr /0/	!address of response
	  integer*4 dvi_resl /0/	!address of response length
	endstructure
c
	record /itmlst/ dvi
	integer class		!response buffer
	integer lphys		!length of device name
C
	integer ldot, lcol, igood
c
	lphys = len(physnam)
	lcol = index(physnam(1:lphys),' ')-1
	if(lcol.gt.0) lphys = lcol
	istat = 1		!assume success
	lcol = index(physnam(1:lphys),':')
	ldot = index(physnam(1:lphys),'.')
	if(ldot.ne.0 .and. lcol.eq.0) then
	  klass = 1		!has a dot, but no colon => disk
	  go to 800
	endif
	class = 0		!assume undefined
	dvi.dvi_bufr = %loc(class)
c
	igood = sys$getdviw(,,physnam(1:lphys),dvi,,,,)
cx	if(.not.igood) go to 800
	if(class.eq.0) then
	  klass = 0	!unknown
	elseif(class.eq.dc$_term) then
	  klass = 2	!terminal (on this node)
	elseif(class.eq.dc$_mailbox) then
	  klass = 5	!null device or xtc
	else
	  klass = 1	!disk
	endif
c
	istat = igood
 800	return
	end
c
c-------------------------------------------------------------/ag_info
	subroutine ag_info(mode,ipid,myterm,user)
c
	implicit none
c  get job information
c  mode = 0(other), 1(network), 2(batch), 3(interactive)
c  ipid = process id number
c  myterm = terminal name (char*8)
c  user = user name (char*12)
	character*(*) myterm,user
	structure /itmlst/
	  integer*2 buflen,itmcod
	  integer*4 bufadr,retadr
        end structure
	record  /itmlst/ jpi_list(6)
	include '($jpidef)'
	integer sys$getjpi

	integer lterm, luser, mode, ipid, ii, igood
c
	myterm = ' '
	user = ' '
	lterm=len(myterm)
	luser=len(user)
	jpi_list(1).itmcod = jpi$_mode
	jpi_list(1).buflen = 4
	jpi_list(1).bufadr = %loc(mode)
	jpi_list(2).itmcod = jpi$_pid
	jpi_list(2).buflen = 4
	jpi_list(2).bufadr = %loc(ipid)
	jpi_list(3).itmcod = jpi$_terminal
	jpi_list(3).buflen = lterm
	jpi_list(3).bufadr = %loc(myterm)
	jpi_list(4).itmcod = jpi$_username
	jpi_list(4).buflen = luser
	jpi_list(4).bufadr = %loc(user)
	jpi_list(5).itmcod = 0
c
	igood = sys$getjpi(,,,jpi_list,,,)
	if(.not.igood) then
cx	  stop 'in SGLIB: cant get job information'
	  mode=0
	  ipid=0
	  myterm=' '
	  user=' '
	endif
c
cx	if(mode.ne.jpi$k_interactive) myterm = ' '
	do ii=1,lterm
	  if(ichar(myterm(ii:ii)).eq.0) myterm(ii:ii) = ' '
	enddo
	end
