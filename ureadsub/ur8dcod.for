! 05may1999 fgtok -impno -s r8_precision.sub foo.sub "r8con.csh conversion"
C******************** START FILE UFDCOD.FOR ; GROUP URDUTS ******************
C------------------------------------------------------------
C& UFDCOD  UREAD CHARACTER STRING TO FLOATING POINT DECODE
C
      REAL*8 FUNCTION ur8dcod(STR,LENW,LENR,ICOD)
C
C  1ST THREE ARGUMENTS-- SEE URWORD
C   ICOD-- COMPLETION CODE
C    =0 NORMAL DECODE SUCCESSFUL
C    =-1 NULL WORD
C    =1 DECODE ERROR
C
      IMPLICIT NONE
C============
C idecl:  explicitize implicit INTEGER declarations:
      INTEGER ilen,lenw,lenr,ilw1,ilw,icod
C============
C idecl:  explicitize implicit REAL declarations:
      REAL*8 z
C============
      CHARACTER*(*) STR
C
C  DECODE BUFFER
C
      CHARACTER*20 ZDCOD,ZDCODL
      CHARACTER*1 ZCHAR,ZCHAR0
C
C  LIMITS OF WORD, PRESUMED INTEGER
C
      ILEN=LEN(STR)
C
      ZDCOD=' '
      ZDCODL=' '
C
      ZCHAR0=STR(1:1)
      IF((STR(1:1).EQ.'+').OR.(STR(1:1).EQ.'-')) STR(1:1)='z'
      CALL URWORD(STR,1,LENW,LENR)
C
C  CHECK IF FLOATING POINT WORD IS PROPERLY DELIMITTED; I.E. IF USER
C  HAS "+" OR "-" AS DELIMITER, WELL THIS MIGHT BE PART OF THE EXPONENT
C  HERE!
C
      IF(LENW.LT.ILEN) THEN
        ILW1=LENW+1
        IF((STR(ILW1:ILW1).EQ.'+').OR.(STR(ILW1:ILW1).EQ.'-')) THEN
          ZCHAR=STR(LENW:LENW)
          IF((ZCHAR.EQ.'D').OR.(ZCHAR.EQ.'E').OR.(ZCHAR.EQ.'d').OR.
     >         (ZCHAR.EQ.'e')) THEN
C  AN EXPONENT!
            ZCHAR=STR(ILW1:ILW1)
            STR(ILW1:ILW1)='x'  ! TEMPORARY PLACEHOLDER
            CALL URWORD(STR,1,LENW,LENR)  ! RECOMPUTE WORD LIMITS
            STR(ILW1:ILW1)=ZCHAR  ! RESTORE EXPONENT SIGN
          ENDIF
        ENDIF
      ENDIF
C
      STR(1:1)=ZCHAR0
C
      ILW=LENW
      IF(ILW.GT.0) CALL URWRDL(STR(1:ILW),ZDCODL,LENW)
C
      IF(LENW.EQ.0) THEN
        ICOD=-1
        ur8dcod=0.0D0
        RETURN
      ELSE IF(LENW.GT.20) THEN
        ICOD=1
        ur8dcod=0.0D0
        RETURN
      ENDIF
C
      ILW=20-LENW+1
      ZDCOD(ILW:20)=ZDCODL(1:LENW)
C  ATTEMPT DECODE
      READ(ZDCOD,1000,ERR=900) Z
 1000 FORMAT(G20.0)
C
C  SUCCESS
      ur8dcod=Z
      ICOD=0
      RETURN
C
 900  CONTINUE
C  ERROR
      ICOD=1
      ur8dcod=0.0D0
      RETURN
      END
C******************** END FILE UFDCOD.FOR ; GROUP URDUTS ******************
