C******************** START FILE uf0cap.FOR ; GROUP UFILS0 ******************
C-----------------------------------------------------------------
C  D. MC CUNE NOV 12 1981
C& uf0cap   CAPITALIZE ALL LETTERS IN PASSED STRING
C
      SUBROUTINE uf0cap(ZSTR)
      CHARACTER*(*) ZSTR
      CHARACTER*52 ZCAP
      DATA ZCAP/
     >  'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
      ILEN=LEN(ZSTR)
      IF(ILEN.EQ.0) RETURN
      DO 101 I=1,ILEN
      DO 100 IC=1,26
      IF(ZSTR(I:I).NE.ZCAP(IC:IC)) GO TO 100
      ICP=IC+26
      ZSTR(I:I)=ZCAP(ICP:ICP)
      GO TO 101
 100  CONTINUE
 101  CONTINUE
      RETURN
      	END
C******************** END FILE uf0cap.FOR ; GROUP UFILS0 ******************
