C******************** START FILE CDCOD.FOR ; GROUP URDUTS ******************
C------------------
C&  URCHAR  UREAD INTERNAL, GET SINGLE CHAR STRING OUT OF TYPEAHEAD
C
      SUBROUTINE URCHAR(ZCHAR,iup,ID,IER)
C
      USE USERIO
C
      CHARACTER*1 ZCHAR
C
      CHARACTER*10 ZSTRNG
C
C--------------------
C
      IER=0
C
C
C  QUOTE STRING-- PROCESS AS WORD OF LENGTH 1
C
      IF(CBUFF(1:1).EQ.'"') THEN
        ZCHAR=' '
C
        CALL URWORD(CBUFF,0,IC,ID)
C
        ILNB=0
        IF(IC.GT.0) THEN
          IF(IC.LT.5) THEN
            CALL URWRDL(CBUFF(1:IC),ZSTRNG,ILNB)
          ELSE
            ILNB=2  ! QUOTE STRING .GT. 1 CHAR LONG
          ENDIF
        ENDIF
C
        IF(ILNB.EQ.1) THEN
          ZCHAR=ZSTRNG(1:1)
        ELSE IF(ILNB.GT.1) THEN
C IF THE QUOTE STRING CONTAINS .GT. 1 CHARACTER ...
          ZCHAR='?'
          IER=1
        ENDIF
C
      ELSE
C  ANY OTHER CHARACTER...
        ZCHAR=CBUFF(1:1)
        if(iup.eq.1) call uspcap(zchar)
        ID=1
      ENDIF
C
      RETURN
      END
C******************** END FILE CDCOD.FOR ; GROUP URDUTS ******************
