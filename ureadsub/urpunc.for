C-----------------------------------------------------------------------
C  URPUNC  -- RETURN CURRENTLY SET USER PUNCTUATION STRING
C
      SUBROUTINE URPUNC(ZPUNCT)
C
      USE USERIO
C
      CHARACTER*16 ZPUNCT
C
C------------------------------------
C
      ZPUNCT=' '
      IF(LPUNCT.GT.0) ZPUNCT(1:LPUNCT)=UPUNCT(1:LPUNCT)
C
      RETURN
      END
