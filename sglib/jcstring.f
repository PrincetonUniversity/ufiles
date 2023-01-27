	integer function jc_trim(string)	!subr Find last blank  !02/06/85
	implicit none
	character string*(*)
	integer i, jl
	jl=1				!Returns length >= 1
	i=len(string)
	do while (i.gt.jl)
	  if(ichar(string(i:i)).gt.ichar(' ')) jl=i
	  i=i-1
	enddo
	jc_trim=jl
	end
	integer function jc_len(string)		!subr Find string length	!12/06/90
	implicit none
	character string*(*)
	integer i, jl
	jl=0				!blank string returns length 0
	i=len(string)
	do while (i.gt.jl)
	  if(ichar(string(i:i)).gt.ichar(' ')) jl=i
	  i=i-1
	enddo
	jc_len=jl
	end
	subroutine jc_term(c)		!Return terminal type		!04/08/85
	implicit none
	character c*(*),cterm*4
	integer i, jl, igood, nn
	call jc_trnlog(' ','TERMINAL_TYPE',cterm,nn,igood)
	c=cterm
	end
	subroutine jc__bs(line) 	!Remove control chars		!11/13/84
	implicit none
C				!original version processed backspaces
	character line*(*)
	integer i
	do i=1,len(line)		!convert CTRL characters to blanks
	  if(ichar(line(i:i)).lt.ichar(' ')) line(i:i)=' '
	enddo
	end
	subroutine jc_upper(string)	!Convert to upper case
	implicit none
	character string*(*)
	integer i, ich
	integer hexdf
	data hexdf/Z'df'/

	do i=1,len(string)
	  ich=ichar(string(i:i))
	  if(ich.ge.ichar('a') .and. ich.le.ichar('z')) then
	     ich=iand(ich,hexdf)
	     string(i:i)=char(ich)
	  endif
	enddo
	end
	subroutine jc__creal(a,c)	!Real-->Shortest string		!10/03/84
	implicit none
	character c*(*),string*16
	real a
	integer n, jc_trim
c	write(string,'(1pg)') a
	write(string,'(bn,1pg16.9)') a
	n=index(string,'E+00')-1	!drop zero exponent
	if(n.le.0) n=jc_trim(string)	!find last blank
	if(index(string(1:n),'E').eq.0) then	!if no exponent
	  do while(string(n:n).eq.'0')	! drop trailing zeros
	    n=n-1
	  end do
	endif
	if (n .lt. len(string)) string(n+1:)=' '
	call jc_left(string,c)		!Left justify the string
	end
	subroutine jc_left(c1,c2)	!Left justify the string	!02/23/85
	implicit none
	character*(*) c1,c2		!Input, output string
	integer j, n
	n=len(c1)
	j=1
	do while(c1(j:j).le.' '.and.j.lt.n)
	  j=j+1
	end do
	c2=c1(j:n)
	end
	subroutine jc__cint(i,c)	!Integer-->Shortest string	!10/03/84
	implicit none
	character c*(*),string*12
	integer i
	write(string,'(i12)') i
	call jc_left(string,c)
	end
	function jc_isdigits(c)		!subr Check for digits		!10/04/89
	implicit none
	character c*(*),c1*1
	logical jc_isdigits
	integer j, nc, jc_len
	jc_isdigits=.false.
	nc=jc_len(c)
	if(nc.eq.0) return
	do j=1,nc
	  c1=c(j:j)
	  if(c1.lt.'0' .or. c1.gt.'9') return
	enddo
	jc_isdigits=.true.
	end
	function jc_isreal(c)		!subr Check for real number	!09/18/94
	implicit none
	character c*(*),c1*1
	logical jc_isreal,isan
	integer j, nc
	integer jc_len
	jc_isreal=.false.
	nc=jc_len(c)
	if(nc.eq.0) return
	do j=1,nc
	  c1=c(j:j)
	  isan=.false.
	  if(c1.ge.'0' .and. c1.le.'9') isan=.true.
	  if(c1.eq.'.' .or.  c1.eq.'-') isan=.true.
	  if(.not.isan) return
	enddo
	jc_isreal=.true.
	end
	function jc_isalnum(c)		!subr Check for alpha-numeric	!04/01/93
	implicit none
	character c*1
	logical jc_isalnum,isan
	isan=.false.
	if(c.ge.'0' .and. c.le.'9') isan=.true.
	if(c.ge.'A' .and. c.le.'Z') isan=.true.
	if(c.eq.'.') isan=.true.
	if(c.eq.'-') isan=.true.
	jc_isalnum=isan
	end

	integer function jc_match(clist,cval,nlist) !subr Match string array	!08/12/90
	implicit none
	character cval*(*),clist(nlist)*(*)
	integer nlist
	integer i
	do i=1,nlist
	  if(cval.eq.clist(i)) then
	    jc_match=i
	    return
	  endif
	enddo
	jc_match=0
	end
	integer function jc_skipc(str,ist,chr)	!subr Skip past chr in str	!01/20/97
	implicit none
	character str*(*),chr*1
	integer ist, iptr, lstr
	integer jc_len
	iptr=ist
	lstr=jc_len(str)
	do while (iptr.le.lstr .and. str(iptr:iptr).eq.chr)
	  iptr=iptr+1
	enddo
	jc_skipc=iptr
	end
