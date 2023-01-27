C******************** START FILE UFXCHL.FOR ; GROUP UFILS0 ******************
C---------------------------------------------------------------
C  UFXCHL
C  FIND FIRST FREE CHANNEL
C  OR RETURN IER.NE.0 IF NONE AVAILABLE
C
      SUBROUTINE UFXCHL(ILUN,ICHAN,IER)
C
      USE UFILES
C
      CALL UFINIT
      IER = 0
      DO 10 ICHAN=1,UFCHAN
        IF(.NOT.LUFSET(ICHAN)) RETURN
C  CHANNEL ICHAN IS FREE
 10   CONTINUE
C
      IER=1
      RETURN
      	END
C******************** END FILE UFXCHL.FOR ; GROUP UFILS0 ******************
