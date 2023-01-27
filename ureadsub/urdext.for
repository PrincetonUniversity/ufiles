C------------------------------------------------------------------
C  URDEXT  SET UREAD INDIRECT INPUT FILENAME EXTENSION
C
      SUBROUTINE URDEXT(ZFILEX)
C
C	Update:
C  TBT Aug 1993 - Changed ZFILEX to C*(*)
  
      USE USERIO
C
C	CHARACTER*3 ZFILEX
      CHARACTER*(*) ZFILEX
C
      FILEXT=ZFILEX
C
      RETURN
      END
