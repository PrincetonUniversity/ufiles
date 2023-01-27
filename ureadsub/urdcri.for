C----------------------------------------------------------------------
C  URDCRI  - SET/CLEAR OPTION TO ACCEPT (CR) BLANK LINE AS VALID INPUT
C
C  UREAD SYSTEM SUBROUTINE
C
      SUBROUTINE URDCRI(IOPT)
C
      USE USERIO
C
      IF(IOPT.EQ.1) THEN
        NLCRI=.TRUE.
      ELSE
        NLCRI=.FALSE.
      ENDIF
C
      IF(LEVCMD.LE.1) NLNULL=NLCRI
C
      RETURN
      END
