C******************** START FILE UTRAHD.FOR ; GROUP URDUTS ******************
C---------------------------------------------------------------
C& UTRAHD  ECHO TYPEAHEAD BUFFER STACK ELEMENT J
C
      SUBROUTINE UTRAHD(ZBUFF)
C
      CHARACTER*(*) ZBUFF
C
      ILEN=LEN(ZBUFF)
      DO 10 IC=ILEN,1,-1
        IF(ZBUFF(IC:IC).NE.' ') GO TO 20
 10   CONTINUE
C  NO TYPEAHEAD HERE
      RETURN
C
 20   CONTINUE
      IF(IC.LE.50) THEN
        WRITE(LNOUT(0),1000) ZBUFF(1:IC)
      ELSE
        WRITE(LNOUT(0),1001) ZBUFF(1:IC)
      ENDIF
C
 1000 FORMAT(' [TYPEAHEAD PENDING:  ',A,']')
 1001 FORMAT(' [TYPEAHEAD PENDING:  '/2X,A,']')
C
      RETURN
      END
C******************** END FILE UTRAHD.FOR ; GROUP URDUTS ******************
