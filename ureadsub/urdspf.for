C&  URDSPF PROCESS UREAD SPECIAL FCNS ("* COMMAND LINES)
C
C  SPECIAL FUNCTIONS-- SET ECHO, CALL FOR HELP PACKAGE
C  SET EXPERT MODE, TURN OFF ECHO, TURN OFF EXPERT MODE
C
C  COMMAND FROM FILE:IER SET ONLY FOR UNRECOGNIZED COMMAND
C   THIS WILL BE RETURNED TO CALLER FOR PARSE
C
      SUBROUTINE URDSPF(IER)
c
      USE USERIO
C
      CHARACTER*1 ZATTY,ZACMD
C
      CHARACTER*4 ZZANS
C
      CHARACTER*16 ZKEYWD
      character(len=128) :: zcmd
C
      LOGICAL ILHOLD,il1lin
C
*** Add next variable - jpj@JET/PPPL 23-feb-1993
      Integer istatsys
C
C-----------------------------
C
      ILHOLD=NLHOLD
C
      IER=0
C
      NLHOLD=.FALSE.
C
      ILFILN=INDEX(XFILE(NLUNIN),' ')-1
      IF(ILFILN.LE.0) ILFILN=LFILN
C
C  CODE HERE MAKES IT SO THAT "* C" IS NOT SAME AS "*C"
C    NO IMBEDDED BLANKS IN COMMANDS
      ZACMD=CBUFF(2:2)
      call uupper(ZACMD)
      CALL UROTOR(1)
      CALL UDELIM(ZACMD,ISTAT)
      IF(ISTAT.GT.0) GO TO 20
C
      IFCN=JXUOPC('ACDEFXNHJRSTPU$',ZACMD)
      IF(IFCN.NE.0) GO TO 50
C-----
 20   CONTINUE
      IF(NLECHO) WRITE(LUNTRM,9001) ZACMD
 9001 FORMAT(
     >   ' % NOT A UREAD FUNCTION, "*',A1,'", TYPE "*H FOR HELP.')
C  REINSERT ASTERISK, RETURN TO CALLER
      IER=1
      CALL URDTTI('*')
      GO TO 2000
C-----
 50   CONTINUE
C
C              A  C  D   E   F   X   N   H   J   R   S   T
      GO TO (80,90,100,200,250,300,400,500,600,700,800,900,
     >         1000,1100) IFCN
C              P    U
C
 80   CONTINUE
      IF(LCMDIN.EQ.2) THEN
        IER=1
        GO TO 2000
      ENDIF
C
C immediate exit with no verification prompt: "*A!"
C
      IF(CBUFF(2:2).eq.'!') THEN
        CALL UEXITA(0)
        CALL URDABO
      ENDIF
C
C  *A [MSG]  ABORT REQUEST FROM INDIRECT INPUT FILE
C
      IF(NCHARC.GT.1) WRITE(LUNTRM,3901)
     >     XFILE(NLUNIN)(1:ILFILN),CBUFF(2:NCHARC)
      IF(IBATCH.EQ.1) THEN
        CALL UEXITA(0)
        CALL URDABO
      ENDIF
C
      CALL UPROMTX(
     >   ' "*A" INPUT REQUESTS PROGRAM HALT, ENTER "Y" TO VERIFY: ',0)
      CALL URDSTT('VERIFICATION (Y/N)',ZZANS)
C
      CALL UCLEAR
C
      ZATTY=ZZANS(1:1)
      IF((ZATTY.EQ.'Y').OR.(ZATTY.EQ.'y')) THEN
        CALL UEXITA(0)
        CALL URDABO
      ELSE
C  TREAT AS END OF INDIRECT INPUT FILE
        IF(NLINDI) CALL URDEOF(1)
      ENDIF
      GO TO 2000
C-------------------------------
C  *C  CREATE SUBPROCESS
C
 90   CONTINUE
      IF(NCHARC.GT.2) THEN
        WRITE(LUNTRM,3001) CBUFF(2:NCHARC)
 3001   FORMAT(' % UREAD (*C) CREATING SUBPROCESS "',A,'"')
        zcmd = 'csh '//CBUFF(2:NCHARC)
        call execute_command_line(zcmd,exitstat=istatsys)
        write(luntrm,'(1x)')
      ELSE
        IF((IBATCH.EQ.1).OR.(NLINDI).or.(NLJAVA)) THEN
          WRITE(LUNTRM,3002)
 3002     FORMAT(
     >' ? UREAD:  CANNOT SPAWN INTERACTIVE SUBPROCESS IN BATCH OR '/
     >'   FROM INDIRECT INPUT FILE OR FROM JAVA CLIENT')
        ELSE
          WRITE(LUNTRM,3003)
 3003     FORMAT(
     >' % UREAD (*C) CREATING INTERACTIVE SUBPROCESS'/
     >'   LOGOUT TO RETURN TO PROGRAM CONTROL')
          zcmd = 'csh -f'//char(0)
          call execute_command_line(zcmd,exitstat=istatsys)
          write(luntrm,'(1x)')
        ENDIF
      ENDIF
C
      CALL UCLEAR
      GO TO 2000
C
 100  CONTINUE
C  *D
      NLECHO=.FALSE.
      CALL UROTOR(1)
      IF(NLXOUT) WRITE(LUNTRM,2011)
 2011 FORMAT(' *D DEFAULT ECHO INVOKED (ECHO ONLY TTY INPUT)')
      GO TO 2000
C
C *E
 200  CONTINUE
      NLECHO=.TRUE.
      CALL UROTOR(1)
      IF(NLXOUT) WRITE(LUNTRM,2001)
 2001 FORMAT(' *E ECHO MODE INVOKED; ENTER *D TO TERMINATE IT')
      GO TO 2000
C
 250  CONTINUE
C
C  *F - OPEN FILES LIST COMMAND
      irotor=1
      CALL URSTAT
      CALL UROTOR(irotor)
      GO TO 2000
C
 300  CONTINUE
      IF(NLXOUT) WRITE(LUNTRM,2002)
 2002 FORMAT(' *X EXPERT MODE INVOKED; ENTER *N FOR NOVICE MODE')
      NLXPER=.TRUE.
      CALL UROTOR(1)
      GO TO 2000
C
 400  CONTINUE
      IF(NLXOUT) WRITE(LUNTRM,2002)
 2012 FORMAT(' *N MODE INVOKED; FULL MENU PROMPTING')
      NLXPER=.FALSE.
      CALL UROTOR(1)
      GO TO 2000
C
 500  CONTINUE
      WRITE(LUNTRM,'('' %URDSPF:  online help not available'')')
      write(luntrm,*) '  (point web browser at: ',
     >   'http://w3.pppl.gov/~pshare/help/uread_hlp.html )'
      CALL UROTOR(1)
      GO TO 2000
C
 600  CONTINUE
C
C  dmc DEC 2009: added *J:<var-name>:<filename> support
C    (echo value of variable to a file)
C
      if(cbuff(2:2).eq.':') then
         call urdspf_varout(cbuff(3:))
         call uclear
         go to 2000
      else
         GO TO 500
      endif
C
C  *R
 700  CONTINUE
      IF(NLECHO) WRITE(LUNTRM,2701)
 2701 FORMAT(' % PROMPT MISMATCH FULL ERROR MESSAGE RESTORED')
      LPMMSG=1
      CALL UROTOR(1)
      GO TO 2000
C
C  *S
 800  CONTINUE
      IF(CBUFF(1:2).EQ.'S*') THEN
        IF(NLXOUT) WRITE(LUNTRM,27021)
27021   FORMAT(
     >' % UREAD PROMPT MATCH CHECKING SUSPENDED, ENTER *R TO RESTORE')
        LPMMSG=-1
        CALL UROTOR(2)
      ELSE
        IF(NLECHO) WRITE(LUNTRM,2702)
 2702   FORMAT(' % PROMPT MISMATCH ERROR MESSAGE TURNED OFF')
        LPMMSG=0
        CALL UROTOR(1)
      ENDIF
      GO TO 2000
C
C  *T
 900  CONTINUE
      IF(LCMDIN.EQ.2) THEN
        WRITE(LUNTRM,3905) CBUFF(2:MAX0(2,NCHARC))
 3905   FORMAT(/' MESSAGE FROM UREAD COMMAND INPUT:'/3X,A/)
        CALL UCLEAR
        GO TO 2000
      ENDIF
      WRITE(LUNTRM,3901)
     >     XFILE(NLUNIN)(1:ILFILN),CBUFF(2:MAX0(2,NCHARC))
CT	CALL UPAUST(1)
      CALL UCLEAR
      GO TO 2000
 3901 FORMAT(' MESSAGE FROM:  "',A,'":'/3X,A/)
C
C------------------------------------
C
 1000 CONTINUE
C
C  *P  -- REASSIGN SGLIB PLOT DEVICE
      IF(NTTYP.LT.0) THEN
        CALL UERMSG(
     >'?UREAD/URDSPF - APPLICATION DOES NOT SUPPORT GRAPHICS I/O',' ',
     >    0)
      ELSE
C
        NPLOTD=0  ! CLEAR GRAPHICS PAGE COUNT
C
        IF(NCHARC.LE.2) THEN
C
C  *P COMMAND WITH NO ARGUMENT
          CALL RINITT('stdout',istatr)
          WRITE(LUNTRM,3101)
 3101 FORMAT(' %UREAD/URDSPF -- REASSIGNING SGLIB OUTPUT TO TTY')
          CALL UCLEAR
        ELSE
C
C  *P COMMAND HAS ARGUMENT - LEFT JUSTIFY
          CBUFF(1:1)=' '
          CALL UDECOD
          INDA=INDEX(CBUFF,' ')-1
          IF(INDA.LE.0) INDA=LEN(CBUFF)
          CALL RINITT(CBUFF(1:INDA),istatr)
          WRITE(LUNTRM,3102) CBUFF(1:INDA)
 3102 FORMAT(' %UREAD/URDSPF -- REASSIGNING SGLIB OUTPUT TO "',
     >         A,'"')
          CALL UROTOR(INDA)
        ENDIF
      ENDIF
      GO TO 2000
C
 1100 CONTINUE
C
C *U OPTION
C
      CALL UROTOR(1)
 1110 CONTINUE
      IF(NCHARC.LE.0) GO TO 2000
C
      CALL CDCOD(ZKEYWD)
C
      IF((ZKEYWD.EQ.'VIDEO').OR.(ZKEYWD.EQ.'REVERSE')) THEN
        CALL UPRSET('R')
      ELSE IF(ZKEYWD.EQ.'BOLD') THEN
        CALL UPRSET('B')
      ELSE IF(ZKEYWD.EQ.'UNDERLINE') THEN
        CALL UPRSET('U')
      ELSE IF(ZKEYWD(1:2).EQ.'NO') THEN
        CALL UPRSET('N')
      ELSE
        CALL UERMSG('%ITEM NOT RECOGNIZED AS *U COMMAND ARGUMENT',
     >      ZKEYWD,0)
      ENDIF
      GO TO 1110
C--------------------------------------------------------------
C
 2000 CONTINUE
      NLHOLD=ILHOLD
      IF(IER.EQ.0) THEN
        IF(NLHOLD) CALL UCLEAR
      ENDIF
      RETURN
      END

      subroutine urdspf_varout(znamf)
c
c  write value of indicated UREAD variable to file.
c    znamf is expected to have syntax <var-name> or <var-name>:<filename>
c    if the former syntax, echo to standard output
c
      use USERIO
c
      character*(*), intent(in) :: znamf
c
      character*16 varnam
      character*96 varval
      character*200 filenam
c
      logical :: ifile
c
      integer :: ilen,icolon,ilun_tmp
c
c--------------------------------
c
      ilen = len(trim(znamf))
      icolon = index(znamf,':')
c
      if(icolon.eq.1) then
         write(luntrm,*) ' ?urdspf "*J:" syntax error:'
         write(luntrm,*) ' *'//trim(cbuff)
         call uclear
         return
      endif
c
      ifile=.FALSE.
c
      if((icolon.gt.0).and.(icolon.lt.ilen)) then
         ifile=.TRUE.
         filenam = znamf(icolon+1:ilen)
         varnam = znamf(1:icolon-1)
         call find_io_unit(ilun_tmp)
      else if(icolon.eq.ilen) then
         varnam = znamf(1:icolon-1)
         ilun_tmp = luntrm
      else
         varnam = znamf(1:ilen)
         ilun_tmp = luntrm
      endif
c
      call uupper(varnam)
      call uspget(varnam,varval,ier)
      if(ier.ne.0) then
         write(luntrm,*) ' ?urdspf "*J:" syntax error:'
         write(luntrm,*) ' *'//trim(cbuff)
         write(luntrm,*) '   variable name unrecognized: '//trim(varnam)
         call uclear
         return
      endif
c
      if(ifile) then
         open(unit=ilun_tmp,file=filenam,status='UNKNOWN',iostat=ier)
         if(ier.ne.0) then
            write(luntrm,*) ' ?urdspf "*J:" syntax error:'
            write(luntrm,*) ' *'//trim(cbuff)
            write(luntrm,*) '   file open error: '//trim(filenam)
            call uclear
            return
         endif
      endif

      write(ilun_tmp,'(1x,a)') trim(varval)

      if(ifile) then
         close(unit=ilun_tmp)
      endif

      return
      end
