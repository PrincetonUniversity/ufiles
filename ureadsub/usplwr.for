C******************** START FILE USPLWR.FOR ; GROUP USPARM *************
C-----------------------------------------------------------------
C  D. MC CUNE NOV 12 1981
C& USPLWR   SET ALL LETTERS IN PASSED STRING TO LOWER CASE
C	    CONVERT TABS TO BLANKS
C
C  DMC 1988 -- **DO NOT** CONVERT STUFF BTW DBL QUOTES " xxx "
C
      SUBROUTINE USPLWR(ZSTR)
C
      CHARACTER*(*) ZSTR
C
      CHARACTER*2 ZFDLIM
      CHARACTER*52 ZCAP
C
      DATA ZFDLIM/'  '/  	! TAB AND SPACE
      DATA ZCAP/
     >  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'/
C
      IUP=1  ! used for quote checking
      zfdlim(2:2)=char(9) ! make sure it is a tab
C
      ILEN=LEN(ZSTR)
      IF(ILEN.EQ.0) RETURN
      DO 101 I=1,ILEN
C  CHECK FOR DBL QUOTE TO SUPPRESS CAPITALIZATION
         IF(ZSTR(I:I).EQ.'"') IUP=1-IUP
         IF(IUP.NE.1) GO TO 101
C  CHECK FOR TAB
         IF(ZSTR(I:I).EQ.ZFDLIM(2:2)) THEN
            ZSTR(I:I)=ZFDLIM(1:1)
         ELSE
C  CHECK FOR LOWER CASE LETTER
            DO 100 IC=1,26
               IF(ZSTR(I:I).NE.ZCAP(IC:IC)) GO TO 100
               ICP=IC+26
               ZSTR(I:I)=ZCAP(ICP:ICP)
               GO TO 101
 100        CONTINUE
         ENDIF
 101  CONTINUE
      RETURN
      END
C******************** END FILE USPLWR.FOR ; GROUP USPARM ***************
