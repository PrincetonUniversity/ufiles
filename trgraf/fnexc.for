C******************** START FILE FNEXC.FOR ; GROUP CONCTR **************
C-----------------------------
C  FNEXC    FIND NEXT CONTOUR INTERCEPT
C
C
      SUBROUTINE FNEXC(X,F,N,C,NC,X0,F0,IRET)
C
C  X(N)  INDEP. VARIABLE
C  F(N)  DEPENDANT VARIABLE
C  C(NC) CONTOUR VALUES
C  N,NC  ARRAY DIMENSIONS
C
C  X0,F0-- CURRENT LOCATION, UPDATED BY CALLS TO THIS ROUTINE
C  IRET -- =0==> NO CONTOUR FOUND
C  IRET -- =N==> CONTOUR #N INTERCEPT FOUND
C
C
C    27 JAN 1992
C  DMC -- IMPROVE SPEED BY REMEMBERING RECENT SEARCH START POINT
C
      DATA IPREV/-1/
C
      SAVE IPREV
C
      DIMENSION X(N),F(N),C(NC)
C
C-----------------------------------------------------------------------
C
      IRET=0
      NM1=N-1
      IF(N.GT.1) GO TO 5
        X0=AMAX1(1.0E30,1.1*X(N))
        F0=F(1)
        RETURN
 5    CONTINUE
C
      IF(IPREV.LT.1) THEN
        IPREV=1
      ELSE IF(IPREV.GE.N) THEN
        IPREV=NM1
      ENDIF
C
      ICYC=0
      DO 10 IJ=1,NM1
        J=IPREV+IJ-1-NM1*ICYC
        JNEX=J+1
        IF(J.GT.NM1) THEN
          ICYC=1
          J=J-NM1
          JNEX=JNEX-NM1
        ENDIF
C
        IF((X0.GE.X(J)).AND.(X0.LT.X(JNEX))) GO TO 20
C
 10   CONTINUE
      RETURN
C
 20   CONTINUE
      IPREV=J
C
C  MAX NEXT X IS X(JNEX)
      DELX=X(JNEX)-X0
      DELF=F(JNEX)-F0
      IF(DELF.EQ.0.0) GO TO 890
      S=DELF/DELX
      IF(S.GT.0.0) INC=+1
      IF(S.LT.0.0) INC=-1
C  FIND POINTER TO NEXT CONTOUR
      INEXC=NC-1
      IF((S.LT.0.0).AND.(F0.EQ.C(NC))) GO TO 50
      INEXC=0
      NCM1=NC-1
      DO 30 I=1,NCM1
        IF(F0.NE.C(I)) GO TO 25
        INEXC=I+INC
        GO TO 50
 25     CONTINUE
        IP1=I+1
        IF(.NOT.((F0.GT.C(I)).AND.(F0.LT.C(IP1)))) GO TO 30
        INC2=MAX0(0,INC)
        INEXC=I+INC2
        GO TO 50
 30   CONTINUE
      IF((S.LT.0.0).AND.(F0.GT.C(NC))) INEXC=NC
      IF((S.GT.0.0).AND.(F0.LT.C(1))) INEXC=1
      GO TO 50
C
 50   CONTINUE
      IF(INEXC.EQ.0) GO TO 890
      IF((S.GT.0.0).AND.(F(JNEX).LT.C(INEXC))) GO TO 890
      IF((S.LT.0.0).AND.(F(JNEX).GT.C(INEXC))) GO TO 890
C  CONTOUR IN RANGE
      IRET=INEXC
      X0=(C(INEXC)-F0)/S + X0
      F0=C(INEXC)
C
      RETURN
C  NO CONTOUR IN RANGE
 890  CONTINUE
      X0=X(JNEX)
      F0=F(JNEX)
      IRET=0
      RETURN
      	END
C******************** END FILE FNEXC.FOR ; GROUP CONCTR ****************
