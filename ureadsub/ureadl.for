C******************** START FILE UREADL.FOR ; GROUP UREADT ******************
C------------------------------------------------------------
C& UREADL  ISSUE UREAD PROMPT, RETURN COMPLETE LINE
C
C  READ A LINE FROM THE TTY OR INDIRECT INPUT FILE
C  uppercase forced; left justified, no leading blanks returned.
C
C  see also UREADLL, below...
C
      SUBROUTINE UREADL(PROMPT,LINE)
C
      USE USERIO
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*(*), intent(out) ::  LINE
C
      CALL UREAD(PROMPT)
C
      ILEN=MIN0(LEN(CBUFF),LEN(LINE))
C
      line=' '
      line(1:ilen)=cbuff(1:ilen)
      call uspcap(line)
C
      CALL UCLEAR
C
      RETURN
      END
C------------------------------------------------------------
C  go into text mode, read a literal line, then restore mode that
C  was previously in effect.
C
C  LINE returned can be mixed upper/lowercase.  
C  the input buffer is flushed before the user is prompted.
C
      SUBROUTINE UREADLL(PROMPT,LINE)
C
      USE USERIO
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*(*), intent(out) ::  LINE
C
C--------------------
C
      ilev=levcmd
C
      call uclear
      call urdmod(2)
C
      CALL UREAD(PROMPT)
      call urdmod(ilev)
C
      ILEN=MIN0(LEN(CBUFF),LEN(LINE))
C
      line=' '
      DO IC=1,ILEN
        LINE(IC:IC)=CBUFF(IC:IC)
      ENDDO
C
      CALL UCLEAR
C
      RETURN
      END
C------------------------------------------------------------
C******************** END FILE UREADL.FOR ; GROUP UREADT ******************
