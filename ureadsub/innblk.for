C******************** START FILE INNBLK.FOR ; GROUP USPARM ******************
C
C-------------------------------------------------------------
C& INNBLK   FIND NEXT NON-BLANK CHARACTER IN UNPACKED LINE
C
C
      INTEGER FUNCTION INNBLK(ZLINE,LLINE,ISTART)
C
      CHARACTER*1 ZLINE(LLINE)
C
      IST=ISTART
 10   CONTINUE
      IST=IST+1
      IF(IST.GT.LLINE) GO TO 100
      IF(ZLINE(IST).NE.' ') GO TO 100
      GO TO 10
C
 100  CONTINUE
      INNBLK=IST
      RETURN
      END
C******************** END FILE INNBLK.FOR ; GROUP USPARM ******************
