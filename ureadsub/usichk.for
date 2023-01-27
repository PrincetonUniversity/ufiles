C******************** START FILE USICHK.FOR ; GROUP UREAD5 ******************
C----------------------------------------------------------
C& USICHK   CHECK THAT UREAD COMMON BLOCK HAS BEEN INITIALIZED
C
      SUBROUTINE USICHK
C
      USE USERIO
C
      IF(NLUINI) GO TO 50
      CALL USINIT
      WRITE(LUNTRM,1998)
 1998 FORMAT(/' *USINIT CALLED*'/)
C
 50   CONTINUE
      RETURN
      END
C******************** END FILE USICHK.FOR ; GROUP UREAD5 ******************
