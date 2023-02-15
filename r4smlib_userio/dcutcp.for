C-----------------------------------------------------------------------
C  DCUTCP -- COPY ARRAYS
C
      SUBROUTINE DCUTCP(Y1,Y2,N)
C
C  COPY Y1 TO Y2
C
      REAL Y1(N),Y2(N)
C
C------------------
C
      DO 10 I=1,N
        Y2(I)=Y1(I)
 10   CONTINUE
C
      RETURN
      END
