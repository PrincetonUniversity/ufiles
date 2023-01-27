C******************** START FILE URDTCH.FOR ; GROUP USINIT ******************
C------------------------------------------------
C& URDTCH  SET UREAD CHARACTERS FOR TEXT MODE (SOME ARE BLANKED)
C
      SUBROUTINE URDTCH
C
      USE USERIO
C
      CHARACTER*1 ZNULL
C
      ZNULL=CHAR(0)
C
      MPARAM=ZNULL
      MCMD=ZNULL
C
      RETURN
      END
C******************** END FILE URDTCH.FOR ; GROUP USINIT ******************
