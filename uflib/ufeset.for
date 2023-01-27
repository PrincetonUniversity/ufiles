C******************** START FILE UFESET.FOR ; GROUP UFILS0 ******************
C---------------------------------------------------------------------
C  UFESET - SET LOGICAL UNIT FOR ERROR MESSAGES FROM UFILES
C
      SUBROUTINE UFESET(ILUN)
C
      USE UFILES
C
      CALL UFINIT
C
      UFELUN=ILUN
C
      RETURN
      END
C******************** END FILE UFESET.FOR ; GROUP UFILS0 ******************
