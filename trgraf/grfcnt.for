C---------------------------------------------------------
C  GRFCNT - COUNT NUMBER OF PTS IN RANGE
C
      SUBROUTINE GRFCNT(X,NX,XL1,XL2,ICNT)
C
      REAL X(NX)
C
      ICNT=0
C
      ZXMIN=AMIN1(XL1,XL2)
      ZXMAX=AMAX1(XL1,XL2)
C
      DO 10 I=1,NX
        IF((X(I).GE.ZXMIN).AND.(X(I).LE.ZXMAX)) ICNT=ICNT+1
 10   CONTINUE
C
      RETURN
      END
