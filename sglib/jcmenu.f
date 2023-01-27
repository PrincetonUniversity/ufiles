c
	subroutine at_menu(prompt,item)		!get user response to menu
        implicit none

	logical jc_isdigits
	external jc_isdigits
	integer  item
	character*(*) prompt,tmatch
	include 'jcticcom'
	integer maxp
	parameter (maxp=20)
	character*40 proms(maxp)
	character*4 tmats(maxp)
	integer ntm(maxp)
	integer nprom
	save nprom,proms
	character aline*48,respons*32
	character back*1
	integer i, ierr, nr, jc_trim, nc
	data ntm/maxp*0/
	data tmats/maxp*'    '/
c
	respons=' '
	back='H'
	item=0
	lhelp=.true.		!bchar treats CR as /H
	lmenu=.true.
	do while (back.eq.'H' .or. item.eq.0)
	  call at_bchar(prompt,respons,back)
	  if(back.eq.'H') then	!output help lines
	    do i=1,nprom
	      write (aline,'(i3,2h) ,a)') i,proms(i)
	      call at_msg(aline)
	    enddo
c
	  else		!check user input
	    if(jc_isdigits(respons)) then	!numeric entry
	      read(respons,'(bn,i20)',iostat=ierr) item
	      if(item.le.0 .or. item.gt.nprom) then
	        item=0
	        ierr=99
	      endif
c
	    else		!match initial string
	      ierr=99
	      nr=jc_trim(respons)	!length of user response
	      do i=1,nprom
		if(nr.ge.ntm(i)) then	!if > minimum length
	        nc=min(nr,ntm(i))
		if(item.eq.0 .or.	!and if no match yet
     >		  ntm(i).gt.ntm(item)) then	! or longer match
		    if(respons(1:nc).eq.tmats(i)(1:nc)) item=i
		endif
		endif
	      enddo
	    endif
	    if(item.eq.0) call at_msg('?Eh')
	  endif
	enddo
	lmenu=.false.
	lhelp=.false.
	return
c
	entry at_imenu		!initialize menu
	nprom=0
	return
c
	entry at_amenu(prompt,tmatch)		!add item to menu
	if(nprom.ge.maxp) then
	  call at_msg('**Too many prompts')
	  return
	endif
	nprom=nprom+1
	proms(nprom)=prompt
	tmats(nprom)=tmatch
	ntm(nprom)=jc_trim(tmatch)
	return
	end
	subroutine at_helpon(back)
        implicit none
	include 'jcticcom'
	character back*1
	lhelp=.true.
	back='H'
	return
	entry at_helpoff
	lhelp=.false.
	end
