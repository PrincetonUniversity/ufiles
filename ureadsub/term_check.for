C***********************************************************************
      SUBROUTINE TERM_CHECK(MODEL,COMCHR)
C
C...This routine was written by Harry H. Towner of the Princeton
C...Plasma physics Lab.  This routine will check what model
C...the terminal is.
C
      CHARACTER MODEL*(*), COMCHR*(*), NAME*64
C
      INTEGER LEN
C
C...Check if TERMINAL_TYPE is assigned.
C
      CALL UTRNLOG(' ','TERMINAL_TYPE',NAME,LEN,ICODE)
C
C...If ICODE .EQ. 1 then terminal type has been defined.
C
      IF (ICODE .NE. 1) GO TO 2000
C
C...Check for command character.
C
      LCC = INDEX(NAME,'_')
      IF (LCC .GT. 0) THEN
C
C...Test that MODEL is less then 6 characters and that
C...COMCHR is only one character.
C
        LCCP1 = LCC + 1
        IF (LCC .GT. 7 .OR. LEN .GT. LCCP1) GO TO 2000
        MODEL = NAME(1:LCC-1)
        COMCHR = NAME(LCCP1:LEN)
      ELSE
C
C...Check that MODEL is less then 6 characters.
C
        IF (LEN .GT. 6) GO TO 2000
        MODEL = NAME(1:LEN)
      END IF
      RETURN
C
2000  MODEL = 'XTERM'
      COMCHR = ' '
      RETURN
      END
