C******************** START FILE TSCGSH.FOR ; GROUP CONCAT ******************
C-------------------------
      SUBROUTINE TSCGSH(ICSHOT,IND,IER)
C  GET SHOT NUMBER
      USE CONCAT_MOD
C
      CHARACTER*10 ZEXT
      character*40 zcbuff
C
 10   CONTINUE
      IF(MODEI.EQ.1) THEN
      CALL UREAD('TSCGSH - ENTER SHOT NUMBER:$')
      ICSHOT=IDCOD(IERT)
      IF((IERT.NE.0).OR.(ICSHOT.LT.1).OR.
     >     (ICSHOT.GT.99999)) THEN
        CALL UERMSG('% ILLEGAL SHOT NUMBER',' ',0)
        GO TO 10
      ENDIF
      ZEXT=XSHOTX(1)
C
      ELSE
C
      CALL UREADW('TSCGSH - ENTER FILENAME EXTENSION .EXT:$',zcbuff)
C
      ilnb=min(len(zext),index(zcbuff,' ')-1)
      ZEXT(1:ilnb)=ZCBUFF(1:ilnb)
      ICSHOT=NSHOTO
      ENDIF
C
      IER=0
      DO 20 I=1,NS
      IF((ZEXT.EQ.XSHOTX(I)).AND.(ICSHOT.EQ.ISHOTS(I))) GO TO 50
 20   CONTINUE
C
      IER=1
      RETURN
C
 50   CONTINUE
      IND=I
      RETURN
      END
C******************** END FILE TSCGSH.FOR ; GROUP CONCAT ******************
