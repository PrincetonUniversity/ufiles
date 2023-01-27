C******************** START FILE IPNBLK.FOR ; GROUP USPARM ******************
C-------------------------------------------------------------
C& IPNBLK  FIND LAST NON-BLANK CHARACTER IN UNPACKED LINE
C
C
      INTEGER FUNCTION IPNBLK(ZLINE,LLINE,ISTART)
C
      CHARACTER*1 ZLINE(LLINE)
C
      IST=ISTART
 10   CONTINUE
      IST=IST-1
      IF(IST.LE.0) GO TO 100
      IF(ZLINE(IST).NE.' ') GO TO 100
      GO TO 10
C
 100  CONTINUE
      IPNBLK=MAX0(0,IST)
      RETURN
      END
C******************** END FILE IPNBLK.FOR ; GROUP USPARM ******************
