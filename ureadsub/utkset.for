c---------------------------------------------------------/UTKSET
c     Set terminal to reverse, bold, underline or normal.
C  MOD DMC-- CODE BY J. MURPHY ADAPTED FOR USE IN UREAD LIBRARY
C  UREAD COMMON INSTEAD OF TK COMMON USED FOR DETERMINING TERMINAL
C  TYPE
C
C  DMC OCT 1991 -- modified to support UNIX systems
c
      subroutine UTKSET(set,ichange)
c
c  DMC -- FORMERLY TK_SET_TERM -- I ADDED AN ARGUMENT AND CHANGED THE
c  NAME TO FIT BETTER INTO THE UREADSUB SUBROUTINE NAMING TRADITION.
c
c     SET is 'R', 'B', 'U', 'N' for reverse, bold, underline, normal.
c
      use iso_c_binding, only: c_char
      use USERIO
c
      character*(*)   set
c
c
      character(kind=c_char) outbuf(4)
c
      character*1    reverse(4), bold(4), underline(4), off(4)
c
      data reverse   / ' ', '[', '7', 'm'/
      data bold      / ' ', '[', '1', 'm'/
      data underline / ' ', '[', '4', 'm'/
      data off       / ' ', '[', '0', 'm'/
c
      character*1    command
c
c  RETURN ARGUMENT:
c
      integer ichange  ! set 0 for no action
c			 ! set 1 for tty attributes changed
c
c----------------------------
c recoded for portability... (instead of '1B'x in data stmt)
c
      reverse(1)=char(27)
      bold(1)=char(27)
      underline(1)=char(27)
      off(1)=char(27)
 
      ichange = 0
C
C  UREAD COMMON-- TESTS FOR EXECUTION
C
      IF(IBATCH.EQ.1) RETURN  ! not for BATCH jobs
      IF(NTTYP.EQ.0) RETURN   ! tty type must be specified
C
C  SUPPORTED TERMINAL TYPES:
      IF((UTTYP(1:4).EQ.'V550').OR.(UTTYP(1:3).EQ.'MAC').OR.
     >   (UTTYP(1:5).EQ.'XTERM').OR.(UTTYP(1:3).EQ.'XTC')) THEN
c
         COMMAND=SET(1:1)
         CALL USPCAP(COMMAND)
         IF(COMMAND.EQ.'R') THEN
            ichange = 1
            DO I=1,4
               OUTBUF(I)=REVERSE(I)
            END DO
         ENDIF
         IF(COMMAND.EQ.'B') THEN
            ichange = 1
            DO I=1,4
               OUTBUF(I)=BOLD(I)
            END DO
         ENDIF
         IF(COMMAND.EQ.'U') THEN
            ichange = 1
            DO I=1,4
               OUTBUF(I)=UNDERLINE(I)
            END DO
         ENDIF
         IF(COMMAND.EQ.'N') THEN
            ichange = 1
            DO I=1,4
               OUTBUF(I)=OFF(I)
            END DO
         ENDIF
C
         call ufterm_out(outbuf,4)
c
      end if  ! SUPPORTED TTY TYPE BLOCK
c
      return
      end
