C---------------------------------------------------------------------
C  TVYINS - LOOK UP LOCATION OF SCALAR Y IN ORDERED ARRAY YA
C   ALLOW FOR OUT OF BOUNDS
C
      SUBROUTINE TVYINS(Y,YA,NYA,I,IP1)
C
      REAL YA(NYA)
C
      I=0
      IP1=1
      IF(Y.LT.YA(1)) GO TO 100
C
      I=NYA
      IP1=0
      IF(Y.GT.YA(NYA)) GO TO 100
C
      ISM1=NYA-1
      DO 90 I=1,ISM1
      IP1=I+1
      IF((Y.GT.YA(I)).AND.(Y.LT.YA(IP1))) GO TO 100
 90   CONTINUE
C
 100  CONTINUE
      RETURN
      END
