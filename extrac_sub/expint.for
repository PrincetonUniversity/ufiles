C******************** START FILE EXPINT.FOR ; GROUP EXSUBS ******************
C
C---------------------------------------------------------------
C
      SUBROUTINE EXPINT(ZX1,ZF1,IN1,ZX2,ZF2,IN2,IOPN,ZOPN)
C
C  INTERPOLATE DATA (ZX1(IN1),ZF1(IN1)) TO GRID ZX2(IN2); STORE
C    RESULT IN ARRAY ZF2(IN2)
C
C  IOPN- =0 IF ZX1 SAME AS ZXW (JUST COPY DATA)
C        =1 FOR REAL INTERPOLATION
C
C  ANY EXTRAPOLATIONS ARE DONE FLAT
C
      REAL ZX1(IN1),ZF1(IN1),ZX2(IN2),ZF2(IN2)
C
      REAL ZOPN(3,2) ! NOT USED AT THIS TIME
C
      IF(IOPN.EQ.0) THEN
        DO 10 IX=1,IN1
          ZF2(IX)=ZF1(IX)
 10     CONTINUE
        RETURN
      ELSE
C  INTERPOLATE.  BOTH X GRIDS KNOWN TO BE MONOTONIC INCREASING
        IN1M1=IN1-1
        IX0=1
        DO 20 IX=1,IN2
         ZX=ZX2(IX)
         IF(ZX.LE.ZX1(1)) THEN
           ZF2(IX)=ZF1(1)
         ELSE IF(ZX.GE.ZX1(IN1)) THEN
           ZF2(IX)=ZF1(IN1)
         ELSE
C  INTERPOLATE
           DO 15 IXL=IX0,IN1M1
             IXLP=IXL+1
             IF((ZX1(IXL).LE.ZX).AND.(ZX.LE.ZX1(IXLP))) GO TO 16
 15        CONTINUE
C  TROUBLE
           IARGHH=1/IARGHH
 16        CONTINUE
           IX0=IXL
C
           ZF2(IX)=ZF1(IXL)+(ZF1(IXLP)-ZF1(IXL))*
     >          (ZX-ZX1(IXL))/(ZX1(IXLP)-ZX1(IXL))
C
         ENDIF
 20     CONTINUE
      ENDIF
C
      RETURN
      END
C******************** END FILE EXPINT.FOR ; GROUP EXSUBS ******************
