C******************** START FILE USPCAP.FOR ; GROUP USPARM *************
C-----------------------------------------------------------------
C  D. MC CUNE NOV 12 1981
C& USPCAP   CAPITALIZE ALL LETTERS IN PASSED STRING
C	    CONVERT TABS TO BLANKS
C
C  DMC 1988 -- **DO NOT** CONVERT STUFF BTW DBL QUOTES " xxx "
C
      SUBROUTINE USPCAP(ZSTR)
C
      CHARACTER*(*) ZSTR
C
      CHARACTER*2 ZFDLIM
      CHARACTER*52 ZCAP
C
      DATA ZFDLIM/'  '/	! TAB AND SPACE
      DATA ZCAP/
     >  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
      IUP=1  ! used for quote checking
      zfdlim(2:2)=char(9) ! make sure it is a tab
C
      ILEN=LEN(ZSTR)
      IF(ILEN.EQ.0) RETURN
      DO I=1,ILEN
C  CHECK FOR DBL QUOTE TO SUPPRESS CAPITALIZATION
         IF(ZSTR(I:I).EQ.'"') IUP=1-IUP
C  CHECK FOR TAB
         IF(ZSTR(I:I).EQ.ZFDLIM(2:2)) THEN
            ZSTR(I:I)=ZFDLIM(1:1)
         ENDIF
         IF(IUP.eq.1) THEN
C  CHECK FOR LOWER CASE LETTER
            DO IC=1,26
               IF(ZSTR(I:I).EQ.ZCAP(IC:IC)) then
                  ICP=IC+26
                  ZSTR(I:I)=ZCAP(ICP:ICP)
                  exit
               endif
            enddo
         ENDIF
      enddo

      RETURN
      END
C******************** END FILE USPCAP.FOR ; GROUP USPARM ***************
