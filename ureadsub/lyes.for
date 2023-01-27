C******************** START FILE LYES.FOR ; GROUP URDUTS ******************
C------------------------------------------------
C& LYES  UREAD LOGICAL FUNCTION .TRUE. IF INPUT IS "Y" FOR YES
C  FALSE OTHERWISE.
C
      LOGICAL FUNCTION LYES(IDUM)
      USE USERIO
C
      CHARACTER*1 ZCHAR
C
      CALL URCHAR(ZCHAR,1,ID,IER)
C
      LYES=.FALSE.
      IF(ZCHAR.EQ.'Y') LYES=.TRUE.
      CALL UROTOR(ID)
      RETURN
      END
C******************** END FILE LYES.FOR ; GROUP URDUTS ******************
