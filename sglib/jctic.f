c	TIC -- John Coonrod's version of the CICADA TIC routines	!01/15/85
C
c	This package provides three levels of TIC routines:
c	1. Normal	at_xxxx
c	2. Backspace	at_bxxxx
c	3. Limit-check	at_lxxxx
c	4. Limit/Back	at_blxxxx
C
	subroutine at_int(prompt,ival)		!integer value		!11/09/84
	implicit none
	character prompt*(*),string*12,back*(*)
	character istrin*12
	logical needb,lims
	integer ival, ihi, ilo, ios, i

	needb=.false.
	lims=.false.
	go to 100
c
	entry at_bint(prompt,ival,back)
	needb=.true.
	lims=.false.
	back=' '
	go to 100
c
	entry at_lint(prompt,ival,ilo,ihi)
	needb=.false.
	lims=.true.
	go to 100
c
	entry at_blint(prompt,ival,ilo,ihi,back)
	needb=.true.
	lims=.true.
	back=' '
 100	continue
	call jc__cint(ival,istrin)	!Convert integer to string
	ios=1
	do while(ios.ne.0)		!Repeat until a good value appears
	  string=istrin
	  if(needb) then
	    call at_bchar(prompt,string,back)
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)
	  endif
	  read(string,'(bn,i12)',iostat=ios) i
	  if(ios.ne.0) then
	    call at__error(string,'Integer')
	  elseif(lims) then
	    if(i.lt.ilo .or. i.gt.ihi) then
	      write(*,*)'Sorry, integer out of range',ilo,ihi
	      ios=-1
	    endif
	  endif
	enddo
	ival=i
	end
	subroutine at_hex(prompt,ival)		!hex value		!11/09/84
	implicit none
	character prompt*(*),string*8,back*(*)
	character istrin*8
	logical needb,lims
	integer ival, ihi, ilo, ios, i
	real val
	needb=.false.
	lims=.false.
	go to 100
c
	entry at_bhex(prompt,ival,back)
	needb=.true.
	lims=.false.
	back=' '
	go to 100
c
	entry at_lhex(prompt,ival,ilo,ihi)
	needb=.false.
	lims=.true.
 100	continue
	write(istrin,'(z8)')ival	!Convert to hex
	ios=1
	do while(ios.ne.0)
	  string=istrin
	  if(needb) then
	    call at_bchar(prompt,string,back)
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)
	  endif
	  read(string,'(bn,z8)',iostat=ios) i
	  if(ios.ne.0) then
	    call at__error(string,'Hex')
	  elseif(lims) then
	    if(i.lt.ilo .or. i.gt.ihi) then
	      write(*,*)'Sorry, hex value out of range',ilo,ihi
	      ios=-1
	    endif
	  endif
	enddo
	ival=i
	end
	subroutine at_real(prompt,val)		!real value		!11/09/84
	implicit none
	real val
	character prompt*(*),string*20,back*(*),sval*(*)
	character istrin*20
	logical needb,lims,spchk
	external setr,chkr
	real rhi, rlo, y
	integer ios

	needb=.false.
	lims=.false.
	spchk=.false.
	go to 100
c
	entry at_breal(prompt,val,back)
	needb=.true.
	lims=.false.
	back=' '
	spchk=.false.
	go to 100
c
	entry at_lreal(prompt,val,rlo,rhi)
	needb=.false.
	lims=.true.
	spchk=.false.
	go to 100
c
	entry at_blreal(prompt,val,rlo,rhi,back)
	needb=.true.
	lims=.true.
	back=' '
	spchk=.false.
	go to 100
c
	entry at_sreal(prompt,val,setr,chkr,sval)
	needb=.false.
	lims=.false.
	spchk=.true.
c
 100	continue
	istrin=' '
	if(spchk) then
	  call setr(istrin,val,sval)
	endif
	if(istrin.eq.' ') then
	  call jc__creal(val,istrin)	!convert real to short string
	endif
	ios=1
	do while(ios.ne.0)
	  string=istrin
	  if(needb) then
	    call at_bchar(prompt,string,back)
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)
	  endif
	  if(spchk) then
	    call chkr(string,y,sval)
	  endif
	  if(ios.ne.0) then
	    read(string,'(bn,e20.0)',iostat=ios) y
	    if(ios.ne.0) then
	      call at__error(string,'Real')
	    elseif(lims) then
	      if(y.lt.rlo*.9999999 .or. y.gt.rhi) then
		write(*,*)'Sorry, real out of range',rlo,rhi
		ios=-1
	      endif
	    endif
	  endif
	enddo
	val=y
	end
	subroutine at_dbl(prompt,dval)		!doubl precision	!11/09/84
	implicit none
	character prompt*(*),string*30,back*(*)
	character istrin*30
	real*8 dval,y,dlo,dhi
	logical needb,lims
	integer ios
	needb=.false.
	lims=.false.
	go to 100
c
	entry at_bdbl(prompt,dval,back)
	needb=.true.
	lims=.false.
	back=' '
	go to 100
c
	entry at_ldbl(prompt,dval,dlo,dhi)
	needb=.false.
	lims=.true.
 100	continue
	ios=1
c	write(istrin,'(D)') dval
	write(istrin,'(bn,d30.0)') dval
	do while(ios.ne.0)
	  string=istrin
	  if(needb) then
	    call at_bchar(prompt,string,back)
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)
	  endif
	  read(string,'(bn,d30.0)',iostat=ios) y
	  if(ios.ne.0) then
	    call at__error(string,'Double Precision')
	    elseif(lims) then
	      if(y.lt.dlo .or. y.gt.dhi) then
		write(*,*)'Sorry, real*8 out of range',dlo,dhi
		ios=-1
	      endif
	    endif
	enddo
	dval=y
	end
	subroutine at_log(prompt,val)		!logical value 		!02/13/85
	implicit none
	character prompt*(*),string*1,back*(*)
	character istrin*1
	logical val,needb
	integer ios, nlist, i

	needb=.false.
	go to 100
c
	entry at_blog(prompt,val,back)
	needb=.true.
	back=' '
 100	continue
	ios=1
	istrin='N'
	if(val) istrin='Y'
	do while(ios.ne.0)
	  string=istrin
	  if(needb) then
	    call at_bchar(prompt,string,back)
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)
	  endif
	  if(index('TY1FN0',string).ne.0) then
	val=.false.
	    if(index('TY1',string).ne.0) val=.true.
	    ios=0
	  else
	    call at__error(string,'Logical')
	  endif
	enddo
	end
	subroutine at_bchar(prompt,ans,back)	!string			!11/09/84
	implicit none
	character*(*) prompt,ans,back	!Given the prompt, input the answer
	include 'jcticcom'
cx	character tans*2	!in case ans is length 1
	integer nlist, i, nc, jc_trim, j, ierr
	back=' '
	if(lmenu) then
	  call at_char('<M>'//prompt,ans)
	elseif(lhelp) then
	  call at_char('<H>'//prompt,ans)
	else
	  call at_char(prompt,ans)
	endif
	if(ans.eq.'?') then
	  back='H'
	  ans='0'
	elseif(ans(1:1).eq.'/' .and. index('BH',ans(2:2)).ne.0) then
	  back=ans(2:2)
	  ans='0'
	endif
	end
	subroutine at_lchar(prompt,ans,clist,nlist) !legal string	!11/09/84
	implicit none
	integer nlist
	character*(*) prompt,ans,clist(nlist),back
	character string*72
	logical nomatch,needb
	integer i, jc_trim, j, ierr, nc

	needb=.false.
	go to 100
c
	entry at_blchar(prompt,ans,clist,nlist,back)
	needb=.true.
	back=' '
100	continue
	nomatch=.true.
	do while(nomatch)
	  string=ans	!copy ans to temporary
	  if(needb) then
	    call at_bchar(prompt,string,back)	!string
	    if(back.ne.' ') return
	  else
	    call at_char(prompt,string)		!string
	  endif
	  i=0
	  do while(i.lt.nlist .and. nomatch)
	    i=i+1
	    nc=jc_trim(clist(i))
	    if(index(clist(i),'*').ne.0) then
	      nomatch=.false.
	      j=0
	      do while (j.lt.nc .and. .not.nomatch)
		j=j+1
		if(string(j:j).ne.clist(i)(j:j) .and. clist(i)(j:j).ne.'*')
     >		  nomatch=.true.
	      enddo
	    else
	      nomatch=string(1:nc).ne.clist(i)(1:nc)
	    endif
	  enddo
	  if(nomatch) then
	    write(*,*)'The legal input strings are:'
	    write(*,*)(clist(i),' ',i=1,nlist)	!//
	  else
	    ans=string
	  endif
	enddo
	end
	subroutine at_error(ierr,string)	!user called error msg	!06/20/88
	implicit none
	integer ierr
	character string*(*),istrin*12
	integer nc, jc_trim

	call jc__cint(ierr,istrin)
	nc=jc_trim(string)
	call at_msg(string(1:nc)//': '//istrin)
	end

	subroutine at_msg(string)		!output text string	!02/08/85
	implicit none
	character*(*) string,istrin*12
	integer nrem, nc, jc_trim, ival
	real rhi, rlo

	go to 100
	entry at_msgf(string)
	call at_stkln(nrem)
	if(nrem.ne.0) return
 100	continue
	nc=jc_trim(string)
	write(*,*)string(1:nc)
	return
	entry at_msgi(string,ival)	!text followed by intgr	!01/08/90
	call jc__cint(ival,istrin)
	nc=jc_trim(string)
	write(*,*)string(1:nc),' ',istrin	!//
	end

	subroutine at_rlimits(prompt,rlo,rhi)			!03/26/85
	implicit none
	real rlo, rhi
	character*(*) prompt
	call at_real(prompt//', low limit',rlo)
	call at_real(prompt//', high limit',rhi)
	end

	subroutine at_ucmmd(opt,ierr)
	implicit none
	integer ierr
	character opt*(*),op*1,opx*4
	integer ngot
	real opt1

	op=opt(2:2)
	if(op.eq.'N') op=opt(3:3)
	if(index('CE',op).gt.0) then	!allow /E,/NE,/C,/NC
	  call at_switch(opx,opt1,ngot)
	endif
cx	/T  = always output prompt (for batch jobs)
cx	/NT = no typeahead prompt
	end

	subroutine at_pause
	implicit none
	include 'jcticcom'
	character cdum*1
	logical lsave
	if(level.eq.1) then
	  lsave=lcrok
	  lcrok=.true.			!allow CR
	  call at_char('Hit <CR> to continue',cdum)
	  lcrok=lsave
	endif
	end
	subroutine at__error(string,kind)	!Internal error routine	!04/03/85
	implicit none
	character*(*) string,kind
	integer jc_trim
	call at_reset
	write(*,*)'Sorry, ',string(1:jc_trim(string)),' is not a valid ',
     >		kind,' input.'
	end
