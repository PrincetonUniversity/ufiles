C----------------------------------------------------------------------
C  UPCGET -- RETURN CHARACTER LAST ENTERED BY USER UPON UPAUSE CALL
C
      SUBROUTINE UPCGET(ZCHAR)
C
      USE USERIO
C
      CHARACTER*1 ZCHAR
C
      ZCHAR=UPCHAR
C
      RETURN
      END
