C******************** START FILE UFGKEY.FOR ; GROUP UFSCLK ******************
C---------------------------------------------------------------
C  UFGKEY
C  ALLOW USER TO NAME A KEYWORD
C
      SUBROUTINE UFGKEY(ZKEY)
C
      CHARACTER*10 ZKEY
C
      character*20 zcbuff
C
 10   CALL UREADW(
     >'UFGKEY:  ENTER SCALAR IDENTIFICATION KEYWORD (MAX 9 CHARS):$',
     >   zcbuff)
      ncharc=len_trim(zcbuff)

      IENDKY=INDEX(zCBUFF,':')
      IF(IENDKY.EQ.0) IENDKY=NCHARC+1
      IF((IENDKY.LE.10).AND.(IENDKY.GT.1))  GO TO 20
      CALL UERMSG('% KEYWORD TOO SHORT OR TOO LONG: ',ZCBUFF,0)
      GO TO 10
C  FORM KEYWORD
 20   CONTINUE
      ZKEY='          '
      ZKEY(1:IENDKY)=ZCBUFF(1:IENDKY)
      ZKEY(IENDKY:IENDKY)=':'
      CALL UCLEAR
      RETURN
      END
C******************** END FILE UFGKEY.FOR ; GROUP UFSCLK ******************
