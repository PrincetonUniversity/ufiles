C******************** START FILE WKCHK.FOR ; GROUP UTL2D ******************
C--------------------------------------------------------------
C  WKCHK - CHECK ARRAY DIMENSION
C
      SUBROUTINE WKCHK(NWK,NMIN,MSG,IER)
C
      CHARACTER*(*) MSG
C
      IER=0
C
      IF(NWK.LT.NMIN) THEN
        write(6,9001) MSG,NWK,NMIN
 9001 FORMAT(' ? ERROR DETECTED: ',A/
     >'   INADEQUATE WORK ARRAY DIMENSION; NWK=',I6,' NMIN=',I6)
        IER=1/IER
      ENDIF
      RETURN
C
      END
C******************** END FILE WKCHK.FOR ; GROUP UTL2D ******************
