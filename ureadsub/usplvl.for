C******************** START FILE USPLVL.FOR ; GROUP USPARM *************
C---------------------------------------------------------
C&  USPLVL  CHECK AND PACK UREAD PARAMETER VALUE
C          (STRIP LEADING AND TRAILING BLANKS)
C          VALUE MAY BE ANY NON-NULL ASCII STRING WITH
C          ANY CHARACTERS EXCEPT PARM. DELIMITER "/"
C
C  INPUT-- ZLINE  UNPACKED DATA LINE 1 CHAR PER WORD
C          LLINE  LENGTH OF DATA LINE
C          IND1   START LOCATION OF VALUE FIELD
C          IND2   FINISH LOCATION OF VAUE FIELD
C  OUTPUT-  ZVAL  96 CHARACTER LEFT JUSTIFIED PACKED VALUE
C          IER    =0  NORMAL SUCCESSFUL COMPLETION
C                 =1  ILLEGAL VALUE:  NULL OR TOO LONG
C
C
      SUBROUTINE USPLVL(ZLINE,LLINE,IND1,IND2,ZVAL,IER)
      USE USERIO
      CHARACTER*1 ZLINE(LLINE)
      CHARACTER*1 ZBLANK,ZUT(96)
      CHARACTER*96 ZVAL
      DATA ZBLANK/' '/
C
C  START OF EXECUTABLE CODE.  IGNORE LEADING AND TRAILING BLANKS
C
      IER=0
C
      zval=' '
      ILENMX=LEN(ZVAL)
C
 10   CONTINUE
      IF(IND1.GT.IND2) GO TO 400
      IF(ZLINE(IND1).NE.ZBLANK) GO TO 20
      IND1=IND1+1
      GO TO 10
C
 20   CONTINUE
      IF(IND2.LT.IND1) GO TO 400
      IF(ZLINE(IND2).NE.ZBLANK) GO TO 40
      IND2=IND2-1
      GO TO 20
C
 40   CONTINUE
      ILEN=IND2-IND1+1
      IF(ILEN.GT.ILENMX) GO TO 500
C VAUE OK.  PACK IT
      DO 60 I=1,ILENMX
      ZUT(I)=ZBLANK
      IF(I.GT.ILEN) GO TO 60
      IND=IND1+I-1
      ZUT(I)=ZLINE(IND)
 60   CONTINUE
      WRITE(ZVAL,1001) (ZUT(I),I=1,ILENMX)
 1001 FORMAT(96A1)

      do i=1,ilenmx
         if(zval(i:i).eq."'") go to 510
      enddo
C
      RETURN
C
C  BLANK VALUE FIELD ****
C
 400  CONTINUE
      ZVAL=';NULL'
      IER=0
      RETURN
C
C
C  ERRORS *********
C
 500  CONTINUE
      WRITE(LUNTRM,9001) ILENMX
 9001 FORMAT(' % ILLEGAL SYNTAX IN PARAMETER VALUE FIELD'/
     >  '   NULL NAME OR NAME TOO LONG (MAX ',I2,'  CHARS)'/)
      CALL UCLEAR
      CALL UPAUST(1)
      IER=1
      RETURN
C
 510  continue
      write(luntrm,9002) trim(zval)
 9002 format(' % illegal parameter value: "',a,'":'/
     >   '   (single quote) characters not allowed.'/
     >   '   (necessary to avoid infinite recursive substitution loop)')
      ier=2
      return
C
      	END
C******************** END FILE USPLVL.FOR ; GROUP USPARM ***************
