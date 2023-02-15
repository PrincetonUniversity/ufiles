C******************** START FILE GETINC.FOR ; GROUP THNVRG ******************
C=====================================
      SUBROUTINE GETINC(LBND,UBND,INC,X,Y,NRET)
C=====================================
C-----WRITTEN BY A. ERLICH 5-2-85
C=====================================
CC
C
C
      PARAMETER (JDSIZ=10240)
      REAL LBND,UBND,X(JDSIZ),Y(JDSIZ),INC
      INTEGER SINC
C
C
C
      SINC=0
      WRITE(5,3267)NRET
 3267 FORMAT(' GETINC NRET=',I10)
C
      DO 100 I=1,12000
C
C
      IF(X(I).GT.LBND) GO TO 10
      GOTO 100
 10   CONTINUE
      IF(X(I).GT.UBND) GO TO 101
C	TYPE *,UBND
      SINC=SINC+1
C	WRITE(5,102)SINC
C 102	FORMAT(' SINC=',I10)
 100  CONTINUE
C
C
 101  CONTINUE
      INC=1/FLOAT(SINC)
      WRITE(5,104)SINC
 104  FORMAT(' SINC=',I10)
C	TYPE *,INC
      RETURN
      END
C******************** END FILE GETINC.FOR ; GROUP THNVRG ******************
