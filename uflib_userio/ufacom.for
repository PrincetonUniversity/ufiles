C******************** START FILE UFACOM.FOR ; GROUP UFNTER ******************
C-------------------------------------------------------------------
C  UFACOM
C  ALLOW USER TO APPEND COMMENTS TO OUTPUT FILE
C
      SUBROUTINE UFACOM(ILUN)
      implicit NONE
C
      integer, intent(in) :: ilun   !  LUN for comments file
C
      integer :: luntrm,lnout,ncharc
      logical :: lxout
C
      character*132 zcbuff
C-------------------------------------------------------------------
C
      luntrm=lnout(0)
      zcbuff=' '
C
      WRITE(ILUN,4005)
 4005 FORMAT(/' USER COMMENTS:')
C
      CALL UCLEAR
      CALL CHXOUT
      IF(LXOUT(0)) WRITE(LUNTRM,2001)
 2001 format(' % UREAD typeahead turned off.  Lower case indented'/
     >'   comments may be typed.  To exit, type "x" or "X" in the'/
     >'   FIRST character position followed by a carriage return')
      CALL UREADLL(
     >'UFACOM:  ENTER COMMENTS FOR OUTPUT FILE (max 80 CHARS/LINE):$',
     >   zcbuff)

 160  CONTINUE
      ncharc=len_trim(zcbuff)

      IF(NCHARC.EQ.1) THEN
        IF((ZCBUFF(1:1).EQ.'X').OR.(ZCBUFF(1:1).EQ.'x')) GO TO 180
      ELSE IF(NCHARC.EQ.2) THEN
C  FOR COMPATIBILITY WITH OLD INDIRECT INPUT FILES...
        IF((ZCBUFF(1:2).EQ.' X').OR.(ZCBUFF(1:2).EQ.' x')) GO TO 180
      ENDIF
      WRITE(ILUN,4006) zcbuff(1:max(1,ncharc))
 4006 FORMAT(1X,A)
c
c  read next input line
c
      CALL UREADLL('$',zcbuff)
      GO TO 160
C
 180  CONTINUE
C  RE-ENABLE TYPEAHEAD WITH EMPTY BUFFER
      CALL UCLEAR
      CALL CHXOUT
C
      RETURN
      END
C******************** END FILE UFACOM.FOR ; GROUP UFNTER ******************
