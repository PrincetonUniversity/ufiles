C******************** START FILE USPGET.FOR ; GROUP USPARM *************
C--------------------------------------------------------------
C&  USPGET   FETCH C*96 VALUE GIVEN 10 CHAR UREAD PARAMETER NAME
C         IER-- =0: SUCCESS
C               =1: FAILED TO LOCATE VALUE
C
      SUBROUTINE USPGET(ZNAM0,ZVAL,IER)
c
      USE USERIO
C
      CHARACTER*(*) ZNAM0
C
      CHARACTER*16 ZNAME
      CHARACTER*96 ZVAL
C
      ZNAME='                '
      IL=MIN0(16,LEN(ZNAM0))
      ZNAME(1:IL)=ZNAM0(1:IL)
C
      IER=0
      CALL USPLOC(ZNAME,ITYPE,INUM)
      IF(ITYPE.EQ.0) GO TO 500
      GO TO (10,20) ITYPE
C  LOCAL PARAMETER VALUE
 10   CONTINUE
      ZVAL=PLCVAL(INUM,MAX0(NLUNIN,NLUNIO))
      RETURN
C  GLOBAL PARAMETER VALUE
 20   CONTINUE
      ZVAL=PGLVAL(INUM)
      RETURN
C  NOT FOUND
 500  CONTINUE
C
C  mod dmc Jan 1990 -- if name begins with "$" then try a DCL symbol
C  translation
C
      IF(ZNAME(1:1).EQ.'$') THEN
        ZVAL=' '
        call sget_env(ZNAME(2:),ZVAL)
        istat=1
        IF(ISTAT.EQ.1) RETURN
      ENDIF
C
      IER=1
      RETURN
      	END
C******************** END FILE USPGET.FOR ; GROUP USPARM ***************
