C******************** START FILE USINIT.FOR ; GROUP USINIT *************
C=================================
C& USINIT  INITIALIZE 'USERIO' UREAD COMMON
C
      SUBROUTINE USINIT

C-  Modifications

C  19-May-2010  jim.conboy@ccfe.ac.uk
C               String carg_delim in program argument list terminates uread 
C               argument scan ( allowing remaining args to be processed 
C               by the user code )
C               See also ureadsub/uread_arg_count

      use USERIO
C
C  980723 Ted Terpstra - Put in check for 0 length zbuff from
C                        Getlog which is 0 for some xterm sessions.
C                        i.e. no USER in detached xterm window!!
C
      CHARACTER*(urdbufl) ZBUFF
      CHARACTER*1 ZTTCMD
      CHARACTER*120 clterm
      character(len=2), parameter  :: carg_delim='-!'
!                                     terminates uread args on command line
C
      LOGICAL STATUS
C
C-----------------------------------------------------------------------
C
      IF(NLUINI) RETURN
      NLUINI=.TRUE.
C
#ifdef _DEBUG
C                Catch unwanted interactive code in debug runs..
      write(6,*) 'USINIT -I- called'
      call errtra()
#endif
C
      call urdprlev(-1)         ! force linkage of block data init...
C
C  SET FLAG FOR ERROR MESSAGE WHEN PROMPT MISMATCH IS DETECTED IN
C  INDIRECT INPUT FILE
      LPMMSG=1
C
C  (JAN 1988) DEFINE PROMPT MATCH = MATCH ON FIRST 3 WORDS IN PROMPT:
      CALL URDPMA(1,3)
C
C  CLEAR BRANCH-TO-MARKER FLAG
      LMARKN=0
C
C  DEFAULT COMMAND INTERPRETATION LEVEL IN EFFECT
      LEVCMD=levdflt
      call uread_cmd_enable
C
C  BATCH OR INTERACTIVE MODE?
      CALL ufisbatch(IBATCH)
C
C  JAVA client mode?
      NLJAVA=.FALSE.
C-----------
C <Eliot, insert environment test here>
C   to set NLJAVA=.TRUE.
      call sget_env('JAVA_CLTERM',clterm)
      if (clterm .ne. ' ') then
	NLJAVA = .TRUE.
      endif
C-----------
C
C  DMC MAY 1986 - SET LENGTH OF UREAD LANGUAGE ELEMENTS
C
      LBUFF=LEN(CBUFF)
      LFILN=LEN(XFILE(1))
      LPNAM=LEN(PGLNAM(1))
      LPVAL=LEN(PGLVAL(1))
C
C  DEFAULT -- NO USER PUNCTUATION
C
      LPUNCT=0
C
C  FROM HHT [NBEAM.TERMCTL]TERMCTL LIBRARY
C  MOD DMC -- SEE USET_TTY SUBROUTINE
C
C  USER CAN SUPPRESS TTY PARTS OF GFLIB WITH A "CALL USET_TTY('NONE')"
C  *PRIOR* TO CALL UINITA OR CALL USINIT ***
C
      IF((NTTYP.NE.-1).OR.(UTTYP(1:5).NE.'NONE ')) THEN
        UTTYP='        '
        CALL TERM_CHECK(UTTYP,ZTTCMD)
        CALL USET_TTY(UTTYP)
      ENDIF
C  CLEAR GRAPHICS PAGE COUNT
      NPLOTD=0
C  CLEAR UPAUSE INPUT CHARACTER
      UPCHAR=' '
C  LOGICAL SWITCHES
      NLEDIT=.FALSE.
      NLECHO=.FALSE.
C  DEFAULT EDITOR BREAK STRING-- BREAK AT EVERY COMMAND
      NLSKIP=.FALSE.
      NLINDO=.FALSE.
      NLXOUT=.TRUE.
      NLXPER=.FALSE.
C  DEFAULT REVERSE VIDEO PROMPTING ON IF TTY SUPPORTS IT...
      CALL UPRSET('R')
C  # OF CHARACTERS PER SINGLE PRECISION VAX WORD
      NCPWD=4
      NLINDI=.FALSE.
C
      NPRBUF=0
C
      NSAVED=0
C  DEFAULT INDIRECT INPUT FILE EXTENSION-- "IND"
      CALL URDEXT('ind')
C  TERMINAL - READ, AND INDIRECT INPUT FILE LUNS
      LUNTIN(1)=5
C  DMC OCT 1990 -- IF SYS$INPUT NOT A FILE, USE VIRTUAL KEYBOARD INPUT
C  ** DMC OCT 1991 -- THIS IS VAXONLY CODE ***
C
C  INDIRECT FILE LUNS:
      IF((LUNTIN(2).LT.10).OR.(LUNTIN(2).GT.91)) LUNTIN(2)=10
      DO IL=3,10
        LUNTIN(IL)=LUNTIN(2)+IL-2
      ENDDO
C  MAX NO. OF OPEN CHANNELS:
      MAXLIN=10
C  CURRENTLY OPEN CHANNEL
      NLUNIN=1
C  NUMBER OF ACTIVE OUTPUT CHANNELS *** OUTPUT OF INDIRECT FILES
      NLUNIO=0
C  TERMINAL - WRITE LUN
      LUNTRM=6
C   INDIRECT FILE *OUTPUT* LUN -- FOR CREATION OF NEW INDIRECT FILES
      LUNIDO=0
C  TRACEBACK INFO-- COMMAND COUNTS AND
C  INPUT FILENAMES --
      XFILE(1)=
     >'SYS$INPUT (W/O UREAD OUTPUT)                                    '
      NCMND(1)=0
      DO 10 I=2,MAXLIN
        XFILE(I)=
     >'                                                                '
        NCMND(I)=0
 10   CONTINUE
C
C  NUMBER OF PREVIOUS INPUT LINES SAVED TO DUMP IN CASE OF ERROR:
      NCBUFP=3
C  STRING PARAMETER INFO
      NLSSUB=.FALSE.
      NPGMAX=1500
      NPLMAX=300
      NPGC=0
      DO 20 I=1,MAXLIN
        NPLC(I)=0
 20   CONTINUE
C
      DO 30 I=1,NPGMAX
        PGLNAM(I)='                '
        PGLVAL(I)='                                                '
 30   CONTINUE
C
      DO 40 IL=1,MAXLIN
        DO 41 I=1,NPLMAX
         PLCNAM(I,IL)='                '
         PLCVAL(I,IL)=
     1	'                                                '
 41     CONTINUE
 40   CONTINUE
C
C  PROMPTING OPTIONS
C
      CALL UBLINE(0)  ! NO LEADING BLANK LINES
C
C  INPUT INTERPRETATION SWITCHES--
C  COMMAND MODE
C   LEFT JUSTIFIED CAPITALIZED INPUT, ALL UREAD FCNS, NO NULL LINES
C   TYPEAHEAD ENABLED
C
      CALL URDCRI(0)  ! DEFAULT -- NO NULL LINES, COMMAND MODE
      CALL URDCH0     ! DEFAULT CHARACTER SET
      CALL URDMOD(1)  ! ESTABLISH COMMAND MODE AS INITIAL MODE (for URDCMI)
C
C  (TEXT MODE ALSO AVAILABLE)
C
C  CLEAR TYPEAHEAD BUFFER AND STACK
      CALL UCLEAR
      DO IL=1,MAXLIN
        AHDBUF(IL)=CBUFF
      ENDDO
C
C  DEFINE GLOBAL PARAMETERS "$DATE" AND "$LOGIN" AND "$PID"
C
      CALL C9DATE(ZBUFF(1:9))
      CALL URDCMI('/$DATE=='//ZBUFF(1:9),IER)
C
      CALL UGETLOG(ZBUFF)
      ILEN=LEN_TRIM(ZBUFF)
        If (ilen .LE. 0 ) Then
           ilen = 7
           Zbuff = 'UNKNOWN'
      EndIf
 
      CALL URDCMI('/$LOGIN=='//ZBUFF(1:ILEN),IER)
C
      call sget_pid_str(zbuff(1:20),ilen)
      call urdcmi('/$PID=='//ZBUFF(1:ILEN),IER)
C
C  LREADY: FLAG INDICATING READINESS TO ACCEPT COMMAND
      CALL URDCMI('/$MATCH==FALSE',IER)
      CALL UREADY(1)
C
C  CHECK FOR INI COMMAND FILE
C
      ILEN=0
      CALL UTRNLOG(' ','UREAD_INI',ZBUFF,ILEN,ISTAT)
C  EXECUTE INI COMMANDS
      IF(ISTAT.NE.1) ILEN=0
      IF(ILEN.GT.0) THEN
        OPEN(UNIT=LUNTIN(10),FILE=ZBUFF(1:ILEN),STATUS='OLD',
     >         RECL=urdbufl,ERR=50)
 45     CONTINUE
        READ(LUNTIN(10),4501,END=52) ZBUFF
 4501 FORMAT(A)
        CALL URDCMI(ZBUFF,IER)
        GO TO 45
      ENDIF
      GO TO 60
 50   CONTINUE
      WRITE(LUNTRM,9009) ZBUFF(1:ILEN)
 9009 FORMAT(' %USINIT - OPEN FAILURE, UREAD_INI FILE: ',A)
 52   CONTINUE
      CLOSE(UNIT=LUNTIN(10),ERR=60)
 60   CONTINUE
c
      call urdmod(levdflt)  ! now set user specified initial mode.
c
C  USE GETARG TO BUILD UP COMMAND LINE
C
      insrt=1
      call get_arg_count(iargn)
      l_arg: do iarg=1,iargn
        call get_arg(iarg,zbuff)
        ilarg=len_trim(zbuff)
        if( zbuff(1:ilarg) == carg_delim )  exit l_arg
        CBUFF(insrt:insrt+ilarg+1)=zbuff(1:ilarg)//' '
        insrt=insrt+ilarg+1
      end do l_arg
      NCHARC=insrt-1
#ifdef _DEBUG
      print *,'usinit -D- CBUFF :'
      print *,' "',cbuff(:ncharc),'"'
#endif
C
      CALL UDECOD
      IF(NCHARC.GT.0) CALL UPSINL(IER)
C
C  IF ANYTHING IS LEFT SILENCE OUTPUT UNTIL INTERPRETED
      CALL URXOUT
C
C--------------------------------------------------------------
      if(NLJAVA) then
         write(luntrm,*) ' %USINIT -- NLJAVA=T, JAVA_CLTERM active.'
      endif
C
      RETURN
C
      END
C******************** END FILE USINIT.FOR ; GROUP USINIT ***************
