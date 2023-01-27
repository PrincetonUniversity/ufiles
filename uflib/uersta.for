C******************** START FILE UERSTA.FOR ; GROUP UFILES ******************
C------------------------------------------------------------------
C  UERSTA
C  REPORT ERROR IN CHANNEL STATUS
C
      SUBROUTINE UERSTA(ICHAN,ZSUB)
C
      USE UFILES
      CHARACTER*10 ZSUB
C
      CHARACTER*30 ZMES(6)
C
      DATA ZMES/'CLOSED                       '
     >           ,'OPEN FOR DATA WRITE          '
     >           ,'OPEN FOR WRITE OF COMMENTS   '
     >           ,'OPEN FOR DATA READ           '
     >           ,'OPEN: TEMPORARY COMMENTS FILE'
     >           ,'OPEN FOR READ OF COMMENTS    '/
C
      IC=UFSTAT(ICHAN)+1
      WRITE(UFELUN,9001) ZMES(IC),ZSUB,UFLUN(ICHAN)
 9001 FORMAT('   LOGICAL UNIT STATUS:  ',A/
     >  ' ? ',A10,' - STATUS INCORRECT;  L.U.N.= ',I5)
      RETURN
      	END
C******************** END FILE UERSTA.FOR ; GROUP UFILES ******************
