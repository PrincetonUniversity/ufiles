C******************** START FILE URSTOP.FOR ; GROUP URDUTS ******************
C------------------------------------------------
C& URSTOP  UREAD AFTER ERROR, FLUSH INPUT LINE BUFFER AND CALL UPAUST
C
C  ERROR MESSAGE PROVIDED BY CALLER
C
      SUBROUTINE URSTOP(IARG)
C  IARG = 0 : NO UPAUST CALL
C       = 1 : UPAUST(0) (NO PAGE CLEAR)
C       = 2 : UPAUST(1) (PAGE CLEAR)
C
      CALL UCLEAR
      IF(IARG.EQ.0) RETURN
      IP=IARG-1
      CALL UPAUST(IP)
      RETURN
      END
C******************** END FILE URSTOP.FOR ; GROUP URDUTS ******************
