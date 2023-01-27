C******************** START FILE IXMMX.FOR ; GROUP MULFDR ******************
C----------------------------------------------------------
C  LOCATE INDEX LOCATION OF PTS BEYOND MIN AND MAX IN
C  CPLOT COMMON (X ARRAY ONLY)
C
      SUBROUTINE IXMMX(X,NX,IMIN,IMAX)
C
      use plot1mmx
C
      DIMENSION X(NX)
C
      IMIN=0
      IMAX=NX+1
C
      IF(X(1).GT.XMIN) GO TO 100
      DO 10 I=2,NX
      IF(X(I).GT.XMIN) GO TO 20
 10   CONTINUE
      I=NX+2
C
 20   CONTINUE
      IMIN=I-1
C
 100  CONTINUE
      IF(X(NX).LT.XMAX) GO TO 200
      IXM1=NX-1
      DO 110 I=IXM1,1,-1
      IF(X(I).LT.XMAX) GO TO 120
 110  CONTINUE
      I=-1
C
 120  CONTINUE
      IMAX=I+1
C
 200  CONTINUE
C  CHECK RESULT
      IF(IMAX.GT.IMIN) RETURN
      IMIN=0
      IMAX=NX+1
      RETURN
      END
C******************** END FILE IXMMX.FOR ; GROUP MULFDR ******************
