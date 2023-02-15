C******************** START FILE FIXTRP.FOR ; GROUP FIXTRP ******************
C---------------------------------------------------------
C  DMC NOV 1985
C
C  FIXTRP - EXTRAPOLATE TO INTERIOR ENDPOINT - USMOO SUBROUTINE
C
C                                  **
C                                ** |
C     ***  |                   **   |
C   **   **|                 **     |
C  --------*-------  =====> --------+-------
C          |**   **                 |     **
C          |  ***                   |   **
C                                   | **
C                                   **
C       +++ +++                  +++ +++
C
C       +++ = DELTA WIDTH
C
C
      SUBROUTINE FIXTRP(X,YS,DEL,N,I1,INUM)
C
      REAL X(N)  ! X-BASE OF SMOOTHED DATA (ORDERED)
      REAL YS(N)	! SMOOTHED DATA FUNCTION
      REAL DEL(N)	! DELTA USED IN SMOOTHING
C
C       I1,INUM - DEFINE INTERVAL OF INTEREST:
C       X(I1) TO X(I2) I2 = I1+INUM-1  (...INUM POINTS)
C
C       POINTS X(J) WHERE X(I1).LE.X(J).LT.X(I1)+DELTA  .OR.
C                         X(I2)-DELTA.LT.X(J).LE.X(I2)
C	WILL BE REPLACED BY THIS LINEAR EXTRAPOLATION BASED
C       ON THE SLOPE OF THE SMOOTHED DATA FCN AT
C	XS1=X(I1)+DELTA(I1)  AND  XS2=X(I2)-DELTA(I2)
C
C	A SMALL INTERVAL CHECK IS DONE: WARNING IF XS2.LT.XS1
C        OR IF THE NUMBER INUM IS TOO SMALL
C
C  *OUTPUT* YS IS MODIFIED
C
C  *EXCEPTION* VICINITIES OF X(1), X(N) NOT TREATED
C
      IF(INUM.LT.4) THEN
         write(6,9001) inum
 9001    format(
     >' %FIXTRP - TOO FEW PTS IN INTERVAL FOR EXTRAPOLATION: ',i4)
         RETURN
      ENDIF
C
      I2=I1+INUM-1
C
      I1P=I1+1
      I2P=I2-1
C
      XS1=AMAX1(X(I1P),(X(I1)+DEL(I1)))
      XS2=AMIN1(X(I2P),(X(I2)-DEL(I2)))
C
      IF(XS2.LT.XS1) THEN
        XS1=0.5*(XS1+XS2)
        XS2=XS1
        write(6,9002) xs1
 9002   format(
     >' %FIXTRP - EXTRAPOLATION INTERVAL SHORT RELATIVE TO DELTA AT X=',
     >      1pe13.6)
      ENDIF
C
C  FIND LEFT EXTRAPOLATION BREAK POINT
C
      INM1=INUM-1
      IF(I1.GT.1) THEN
      DO 10 IL=2,INM1
        IP=I1+IL
        I=IP-1
        IF((X(I).LE.XS1).AND.(XS1.LE.X(IP))) THEN
          ZS=(YS(IP)-YS(I))/(X(IP)-X(I))
          ZX=X(I)
          ZYS=YS(I)
          ILL=IL-1
          GO TO 15
        ENDIF
 10   CONTINUE
c
 15   CONTINUE
C
      DO 20 IL=1,ILL
        I=IL+I1-1
        YS(I)=ZYS-(ZX-X(I))*ZS
 20   CONTINUE
      ENDIF
C
C  FIND RIGHT EXTRAPOLATION BREAK POINT
C
      INM2=INM1-1
      IF(I2.LT.N) THEN
      DO 30 IL=INM2,1,-1
        IP=I1+IL
        I=IP-1
        IF((X(I).LE.XS2).AND.(XS2.LE.X(IP))) THEN
          ZS=(YS(IP)-YS(I))/(X(IP)-X(I))
          ZX=X(IP)
          ZYS=YS(IP)
          ILL=IL+1
          GO TO 35
        ENDIF
 30   CONTINUE
C
 35   CONTINUE
C
      DO 40 IL=INUM,ILL,-1
        I=IL+I1-1
        YS(I)=ZYS-(ZX-X(I))*ZS
 40   CONTINUE
      ENDIF
C
      RETURN
      END
C******************** END FILE FIXTRP.FOR ; GROUP FIXTRP ******************
