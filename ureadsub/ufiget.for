C******************** START FILE ufiget.FOR ; GROUP UREAD5 ******************
c---------------------------------------------------------------
c& ufiget  GET UREAD INPUT FROM FILE(S) - BUFFERING SCHEME
C
c  two barrelled buffered line fetcher for subroutine uread
c  version 3, [transp]uread3.for on the VAX
c  by D. Mc Cune 7 Jan 1981 pppl
c
c  this routine controls the passage of asci lines
c  buffered out of an indirect input file,
c  from the input device through uread to the interactive
c  applications program.  this routine controls the opening and
c  closing of indirect input files and manages the buffers
 
 
      subroutine ufiget(zfile,ICODE0,ier,ieof)
 
      use USERIO
 
      parameter (isiz00=250)
c
c  this parameter gives the size in 128 character asci lines of
c  each of the two buffers.  The buffers are for indirect input
c  files only.  Two buffers allows the buffering of two levels of
c  indirect input.  If an indirect input file fits entirely inside
c  the buffer then it may perform a tight loop without having to
c  reopen/reread the file.  Or one file may loop as a subroutine
c  of another buffered file which is looping.
c     isiz00 size pages of indirect input file are paged into
c  one of the two buffer barrels each time an old page runs out
c  or when a new input file is opened.  see subroutine ugbuf0 below.
c    5 functions are supported by this routines
c    (1) open an indirect input file
c    (2) read in a line from an indirect input file.
c    (3) close an indirect input file
c        reestablish communications with higher level input file
c    (4) rewind the current buffer-- set the line pointer to the
c        beginning of the current buffer
c    (5) rewind the file and load the buffer with the first page
c
C  Modified:
C
C  05/15/95  TBT - IOncrease length of zfile to 130 from 64.

      character*130 zfile        ! 130 is arbitrary  tbt
c
c  zfile - 130 char filename
c  icode-- function select 1,2,3,4,5
c  ier - error flag
c  ieof - end-of-file flag
c
c  local symbols
c
      character*(urdbufl) zbufr(isiz00,2)
      integer ilun(2), ilunin(2), ilasl(2), iendl(2), ipt(2),
     >          isiz(2), istat(2)
      logical iopen(2),iavail(2),itotal(2)
      character*130 zfilep(2)      ! 130 is arbitrary  tbt
c
      save zbufr,ilasl,iendl,ipt,istat,iavail
c
c  zbufr: 2 barrelled buffer
c  ilun: lun of file read into each buffer
c  ilunin: stack location of file (bigger number --> more nested input)
c  ilasl: last line of buffer containing data line from file
c  iendl: location of end of file 0= not known
c         i==> i-1 is last line in file
c         iendl=1 would be an error
c  ipt:   points to current line from buffer
c  isiz:  size of each buffer
c  istat: flag for determining if file fits in buffer
c  zfilep:  name of file read into each buffer
c
c  iopen: .true.==>file for buffer is open
c  itotal: .true.==> entire file fits into buffer
c
      data ilun/2*0/
      data ilunin/2*0/
      data iopen/2*.false./
      data itotal/2*.false./
      data isiz/2*isiz00/
      data zfilep/2*' ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?'/
      data icur/2/
c
c  local variable icur points to the currently active buffer
c  this statement function toggles the current buffer ptr.
c
      lswich(icur)=3-icur
c
c
C  MOD DMC DEC 1989 -- PASSED NAME CHANGED FROM ICODE TO ICODE0
C  ICODE=IABS(ICODE0) SIGN OF ICODE0 ALLOWS EXTRA CONTROL -- E.G.
C  FOR ICODE0=-1, A DISK READ OF THE INPUT FILE IS FORCED; THE
C  INTERNAL BUFFER IS NOT USED.
C
c  start of executable code
c
      IF(ICODE0.LT.0) THEN
        ISIGN=-1
      ELSE
        ISIGN=1
      ENDIF
c
      icode=ISIGN*ICODE0
c
      ieof=0
      ier=0

      if(nljava) then
         ! this suppresses buffering, in JAVA mode
         !   --necessary because JAVA code can be used to manipulate
         !     scripts between invocations.
         zfilep(1) = ' ? ? ? '
         zfilep(1) = ' ? ? ? '
      endif
c
      go to (100,200,300,400,500) icode
c
c  open indirect file
 100  continue
cx	write(6, 7001)
cx 7001	format('     ... open file')
c  switch to new buffer
      iprev=icur
      icur=lswich(icur)
      if(itotal(icur).and.
     >     (zfile.eq.zfilep(icur)).and.(isign.eq.1)) then
c  buffer already loaded -- reuse
        if(ibatch.eq.0) then
         if(nlecho.or.(.not.nlindi)) then
          ilfiln=index(zfile,' ')-1
          if(ilfiln.le.0) ilfiln=len(zfile)
          write(luntrm,2001) zfile(1:ilfiln)
         endif
        endif
        call uindop(zfile,ier,2)
        if(ier.ne.0) go to 190
        ilun(icur)=luntin(nlunin)
        ilunin(icur)=nlunin
        go to 400
 2001 format(/' @FILENAME=',a,' [INTERNAL]'/)
      endif
c
c  rewritten dmc may 1986.  If a file in the input file stack and
c  buffer is about to be replaced in the buffer by a lower level input
c  file, make sure that the file pointer is up to date
c
      if(ilunin(icur).lt.ilunin(iprev)) then
        if(ipt(icur).lt.ilasl(icur)) then
c  fix dmc may 1986
          if(ilun(icur).eq.0) then
            call uermsi('%ufiget:  uread buffer error',nlunin,1)
            go to 190
          endif
c
          if(.not.iopen(icur)) then
            CALL URDOPEN(ILUN(ICUR),ZFILEP(ICUR),1,IERR)
            IF(IERR.NE.0) GO TO 190
          else
            rewind ilun(icur)
          endif
c
          do 110 i=1,ipt(icur)
            read(ilun(icur),5001,END=110) zbufr(1,icur)
            call urd_cln_input(zbufr(1,icur))
 5001       format(a)
 110      continue
c
        endif
      endif
c
c  open indirect file
      call uindop(zfile,ier,1)
c  check for error exit
      if(ier.ne.0) go to 190
c  initialize and read first bufferfull
      iendl(icur)=0
      itotal(icur)=.false.
      iopen(icur)=.true.
      istat(icur)=1
      ilun(icur)=luntin(nlunin)
      ilunin(icur)=nlunin
      zfilep(icur)=zfile
      ilasl(icur)=0
c
      call ugbuf0(istat(icur),zbufr(1,icur),isiz(icur),ilasl(icur),
     >              itotal(icur),iendl(icur))
      if(iendl(icur).eq.1) then
        write(luntrm,9001) zfile
        go to 190
      endif
 9001 format(' % null file:  ',a)
c  all done-- initialize buffer and exit
      go to 400
c
c  error exit
 190  continue
      ier=1
      icur=lswich(icur)
      return
c---------------------------------
 200  continue
cx	write(6, 7002) icur,ipt(icur)
cx 7002	format('     ... read a line icur=',i2,' ip=',i2)
c  read a line from the buffer- check for e-o-f marker
      ip=ipt(icur)+1
      if(ip.eq.iendl(icur)) go to 280
      ncmnd(nlunin)=ncmnd(nlunin)+1
c  check for end of buffer
      if(ip.le.ilasl(icur)) go to 240
c  reset pointer if at end of buffer page
      if(ilasl(icur).eq.isiz(icur)) then
        ip=1
        istat(icur)=2
      endif
c  end-of-buffer: refill, or complete buffer page after break to
c  subsidiary indirect file
      call ugbuf0(istat(icur),zbufr(1,icur),isiz(icur),ilasl(icur),
     >              itotal(icur),iendl(icur))
      if(ip.eq.iendl(icur)) go to 280
c
 240  continue
c  transfer line from buffer and exit
      cbuff=zbufr(ip,icur)
      ipt(icur)=ip
      return
c  end-of-file marker encountered
 280  continue
      ieof=1
      return
c----------------------------------------------------------
 300  continue
cx	write(6, 7003)
cx 7003	format('     ... close file')
c  close the input file and establish communications with the next
c  level up
      if(iopen(icur)) close(unit=luntin(nlunin))
      iopen(icur)=.false.
      ilun(icur)=0
c
c  switch to other buffer
c
      icur=lswich(icur)
c  done if other buffer has input file page in core or if higher
c  level file is user - direct i/o
      if(nlunin.le.max0(2,(nlunio+1))) return
      if(ilun(icur).eq.luntin(nlunin-1)) return
c  reestablish link to higher level file
      ilun(icur)=luntin(nlunin-1)
      ilunin(icur)=nlunin-1
c  restart buffered input for file on unit ilun(icur)
 350  continue
      ipt(icur)=0
      ilasl(icur)=0
      iendl(icur)=0
      iopen(icur)=.true.
      itotal(icur)=.false.
      istat(icur)=2
c  buffer will be refilled with next isiz00 lines from file open on
c  unit luntin(nlunin-1), on the next call to read a line
      return
c---------------------------------------------------------
 400  continue
      if(icode.eq.1) ncmnd(nlunin)=0
      if(icode.eq.4) ncmnd(nlunin)=ncmnd(nlunin)-ipt(icur)
cx	write(6, 7004)
cx 7004	format('     ... initialize buffer pointer; buffer rewind')
c  initialize/re-initialize or rewind already filled buffer
      ipt(icur)=0
      return
c
 500  continue
cx	write(6, 7005)
cx 7005	format('     ... rewind file')
c  rewind file
      rewind luntin(nlunin)
      ncmnd(nlunin)=0
      go to 350
      end
C******************** END FILE ufiget.FOR ; GROUP UREAD5 ******************

      subroutine urd_cln_input(zstr)

      !  remove control characters, e.g. DOS \r, from input line

      implicit NONE

      character*(*) zstr

      integer :: ilen,ic,icc

      ilen = len(trim(zstr))

      do ic=1,ilen
         icc=ichar(zstr(ic:ic))
         if((icc.eq.127).or.((icc.ne.9).and.(icc.lt.32))) then
            zstr(ic:ic)=' '
         endif
      enddo

      return
      end

