C******************** START FILE UFNSHO.FOR ; GROUP UFILES ******************
C-----------------------------------------------------------
C  UFNSHO - TURN OFF SHOT NO. ENCODING IN FILENAMING
C
      SUBROUTINE UFNSHO(ILUN)
C
      USE UFILES
C
      CALL UFLCHK(ILUN,'UFNSHO    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
C
      LSHOTN(ICHAN)=.FALSE.
C
      RETURN
      END
C******************** END FILE UFNSHO.FOR ; GROUP UFILES ******************
