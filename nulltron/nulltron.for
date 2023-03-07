C-----------------------------------------------------------------
C  NULLTRON!  THIS IS NULLTRON!  WHY HAVE YOU WAKENED ME?  I DO NOTHING!
C
      PROGRAM NULLTRON
C
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
C
      LOGICAL LDQUOT
C
C  PLEASE DO NOT DELETE ME.  PLEASE!! ARGHH AYYAAHHHH GAK SPUTTER
C
      CHARACTER*512 ZWORD
C
      character*20 :: pname
C----------------------------------------
C
CCC      CALL USET_TTY('none')  ! uncomment to suppress reverse video prompts
      call ufheader("nulltron")
      CALL UINITA(0)
C
      CALL URDEXT('ifi')  ! DEFAULT FILENAME .EXT
      CALL URDPMA(0,0)    ! SUPPRESS PROMPT MATCH CHECKING
C
CX	CALL URDCMI('*U NOVIDEO')
C
      call uread_arg_count(nargu,nargc)
      write(ilunt,'(1x,a,i3,a,i3,a)') '(uread_arg_count: nargu=',
     >     nargu,'; nargc=',nargc,')'
C
      ICT=0
      iup=1
C
      ILUNT=LNOUT(0)
C
C----------------------------------------
C
      call get_arg(0,pname)
      write(ilunt,*) ' '
      write(ilunt,*) ' Running: '//trim(pname)
      write(ilunt,*) ' '

      if(lxout(0)) then
         write(ilunt,*) ' enter ".Q" to quit'
         write(ilunt,*) ' enter ".L" to allow lowercase words'
         write(ilunt,*) ' enter ".U" to force uppercase words (default)'
         write(ilunt,*) ' enter ".MODE1" for urdmod(1) mode'
         write(ilunt,*) ' enter ".MODE2" for urdmod(2) mode'
         write(ilunt,*) ' enter ".OTHER" for other tests...'
      endif
C
      imode=1
 10   CONTINUE
C
C
      ICT=ICT+1
      if(imode.eq.1) then
         CALL UREAD('NULLTRON:  ENTER UREAD INPUT:  $$')
C
         ZWORD=' '
C
         IF(LDQUOT(0)) THEN
            WRITE(ILUNT,'('' % USER ENTERED A QUOTE STRING'')')
         ENDIF
C
         IF(LCHAR(',')) THEN
            ZWORD(1:1)=','
            WRITE(ILUNT,'('' % USER ENTERED A COMMA'')')
         ELSE IF(LCHAR(' ')) THEN
            ZWORD(1:1)=' '
            WRITE(ILUNT,'('' % USER ENTERED A BLANK'')')
         ELSE IF(LCHAR('"')) THEN
            ZWORD(1:1)='"'
            WRITE(ILUNT,'('' % USER ENTERED A DOUBLE QUOTE'')')
         ELSE
            if(iup.eq.0) then
               call CDCODL(ZWORD)
            else
               CALL CDCOD(ZWORD)
            endif
         ENDIF
C
      else
         call ureadll('NULLTRON:  enter input line:  $',zword)
      endif
 
      IBL=ILNURD(ZWORD)
C
      WRITE(ILUNT,1001) ICT,ZWORD(1:IBL)
 1001 FORMAT(' NULLTRON WORD ',I6,':  ',A)
C
      call uspcap(zword)
C
      if(zword(1:ibl).eq.'.MODE1') then
         write(ilunt,'('' ... call urdmod(1) ... '')')
         call urdmod(1)
         imode=1
      endif
      if(zword(1:ibl).eq.'.MODE2') then
         write(ilunt,'('' ... call urdmod(2) ... '')')
         call urdmod(2)
         imode=2
      endif

      if(zword(1:ibl).eq.'.Q') then
         call uexita(0)
      endif

      if(zword(1:ibl).eq.'.L') then
         iup=0
      endif

      if(zword(1:ibl).eq.'.U') then
         iup=1
      endif

      if(zword(1:ibl).eq.'.OTHER') then
         call nulltron2
      endif
C
      GO TO 10
C
      END
C--------------------------------------------------------
      subroutine nulltron2

      implicit NONE
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC

      integer i_in,iert
      real f_in
      real*8 r8_in
      character*40 w_in
      character*128 line_in
      character*1 c_in

      logical lword

      integer idcod,ixuopc
      real fdcod
      real*8 r8dcod

      character*4 :: LTEST = 'ABCD'

      integer lnout

C------------------------------

 10   continue

      if(lxout(0)) write(lnout(0),1001)
 1001 format(/' options:'/
     > '  C -- for CHARACTER input tests'/
     > '  W -- for WORD input tests'/
     > '  L -- for LINE input tests'/
     > '  N -- for NUMERIC input tests'/
     > '  Q -- quit')
      call uread('nulltron2 option:$$')

      if(lchar('Q')) then
         return

      else if(lchar('C')) then

         do

            call ureadc(' enter character (uppercase forced)',c_in)
            write(lnout(0),*) '   received: "',c_in,'"'

            call ureadLc(' enter character (uppercase NOT forced)',c_in)
            write(lnout(0),*) '   received: "',c_in,'"'

            call uread(' input character, check against: '//ltest//':$')
            i_in=ixuopc(ltest)
            if(i_in.eq.0) then
               write(lnout(0),*) ' input character NOT in '//ltest
               write(lnout(0),*) '  buffer flushed...'
               call uclear
            else
               write(lnout(0),*) ' input character found in '//ltest
            endif

            call uread(' input "Z" or some other character:$')
            if(lyesc('Z')) then
               write(lnout(0),*) ' "z" or "Z" was entered.'
            else
               write(lnout(0),*) ' "z" or "Z" was NOT entered.'
            endif

            call uread(' repeat CHARACTER tests? (Y/N):$$')
            if(.not.LYES(0)) exit
         enddo

      else if(lchar('W')) then

         do

            call uread(' CDCOD input word (uppercase forced)')
            call cdcod(w_in)
            write(lnout(0),*) '   received: "',trim(w_in),'"'

            call uread(' CDCODL input word (uppercase NOT forced)')
            call cdcodL(w_in)
            write(lnout(0),*) '   received: "',trim(w_in),'"'


            call ureadw(' input word (uppercase forced)',w_in)
            write(lnout(0),*) '   received: "',trim(w_in),'"'

            call ureadLw(' input word (uppercase NOT forced)',w_in)
            write(lnout(0),*) '   received: "',trim(w_in),'"'


            call uread(' type in "Yes" (case blind):$')
            if(lword('Yes')) then
               write(lnout(0),*) ' "Yes" was typed.'
            else
               write(lnout(0),*) ' "Yes" was NOT typed.'
               write(lnout(0),*) '  buffer flushed...'
               call uclear
            endif

            call uread(' repeat WORD tests? (Y/N):$$')
            if(.not.LYES(0)) exit
         enddo

      else if(lchar('L')) then

         do
            call ureadL('input a line of text (UREADL):$',line_in)
            write(lnout(0),*) ' received: ',trim(line_in)

            call ureadLL('input a line of text (UREADLL):$',line_in)
            write(lnout(0),*) ' received: ',trim(line_in)

            call uread(' repeat LINE tests? (Y/N):$$')
            if(.not.LYES(0)) exit
         enddo

      else if(lchar('N')) then

         do

            i_in=-1
            f_in=1.0
            r8_in=2.0d0

            call ureadi(' input integer (UREADI):$',i_in)
            write(lnout(0),*) '  received: ',i_in
            call uread(' input integer (IDCOD):$')
            i_in=idcod(iert)
            write(lnout(0),*) '  received: ',i_in

            call ureadf(' input float (UREADF):$',f_in)
            write(lnout(0),*) '  received: ',f_in
            call uread(' input float (FDCOD):$')
            f_in=fdcod(iert)
            write(lnout(0),*) '  received: ',f_in

            call ureadr8(' input real*8 (UREADR8):$',r8_in)
            write(lnout(0),*) '  received: ',r8_in
            call uread(' input real*8 (R8DCOD):$')
            r8_in=r8dcod(iert)
            write(lnout(0),*) '  received: ',r8_in

            call uread(' repeat NUMERIC tests? (Y/N):$$')
            if(.not.LYES(0)) exit
         enddo

      else
         call uermsg(' ?nulltron: unknown option',' ',0)
      endif

      go to 10

      end

