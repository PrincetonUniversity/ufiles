C******************** START FILE UfSwXY2.FOR ; GROUP UfSwXY2 ******************
C-------------------------------------------------------------
C  UfSwXY2
C  SWITCH X AND Y INDEP. VARIABLE DIMENSIONS OF 2D ARRAY
C  AND REVERSE INDEXING OF DATA ARRAY  Version 2 - 10/93 tbt
 
C
      SUBROUTINE UfSwXY2(F,FB,ISIZ,X,NX,Y,NY,XLAB,YLAB,
     1                     Xunits, Yunits)
 
C
C  F-- 2D FUNCTION ARRAY TO BE REINDEXED
C  FB-- WORKSPACE SAME SIZE AS F
C  ISIZ- SIZE OF F, FB
C  X(NX) X INDEPENDANT VARIABLE ARRAY
C  Y(NY) Y INDEPENDANT VARIABLE ARRAY
C  XLAB X LABEL
C  YLAB Y LABEL
C
      REAL F(ISIZ),FB(ISIZ),X(ISIZ),Y(ISIZ)
      Character*30 Zltem
      CHARACTER*(*) XLAB,YLAB
      CHARACTER*(*) XUNITS, YUNITS
C
C  CLEAR FB
C
      DO 10 I=1,ISIZ
      FB(I)=0.0
 10   CONTINUE
C
C  INVERT F
C
      DO 20 IY=1,NY
      DO 20 IX=1,NX
      IP1=(IY-1)*NX+IX
      IP2=(IX-1)*NY+IY
      FB(IP2)=F(IP1)
 20   CONTINUE
C  COPY BACK
      CALL UCOPYF(FB,F,ISIZ)
C  SWITCH X,Y ARRAYS
      IMAX=MAX0(NX,NY)
      DO 30 I=1,IMAX
      ZTEM=X(I)
      X(I)=Y(I)
      Y(I)=ZTEM
 30   CONTINUE
      ITEM=NX
      NX=NY
      NY=ITEM
 
 
C  SWITCH LABELS
 
      Zltem = Xlab
      Xlab  = Ylab
      Ylab  = Zltem
 
      Zltem = Xunits
      Xunits= Yunits
      Yunits= Zltem
 
      RETURN
      END
C******************** END FILE UfSwXY2.FOR ; GROUP UfSwXY2 ******************
