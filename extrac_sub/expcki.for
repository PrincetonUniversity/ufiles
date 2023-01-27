C******************** START FILE EXPCKI.FOR ; GROUP EXSUBS ******************
C--------------------------------------------------------------------
C  EXPCKI
C
C  GIVEN RANGE OF REAL VALUES RETURN INTEGER RANGE
C
      SUBROUTINE EXPCKI(X,NX,ICALL,X1,X2,IX1,IX2)
C
C  INPUT
C   X(NX)  X ARRAY TO SEARCH; MUST BE MONOTONIC INCREASING IF ICALL=2
C   ICALL
C     IF ICALL=1, X2 AND IX2 ARE IGNORED
C     OUTPUT IX1= INDEX TO X VALUE NEAREST TO X1
C
C     IF ICALL=2,
C     OUTPUT INDEX RANGE (IX1,IX2) S.T. THE INTERVAL (X(IX1),X(IX2))
C     IS THE LARGEST INTERVAL CONTAINED IN THE INPUT RANGE (X1,X2)
C     (I.E. X(IX1-1) .LT. X1 AND X(IX2+1) .GT. X2)
C     RETURN IX1=IX2=0 IF NO SUCH RANGE EXISTS
C
C     if icall=3,
c     output index pair btw which x1 lies (x2 is ignored)
c     setup for interpolation.  x(1) .le. x1 .le. x(nx) expected.
C
      REAL X(NX)
C
      IF(ICALL.EQ.2) THEN
C-----------------------------------/ICALL=2
      IX1=0
      IX2=0
      DO 110 IZ=1,NX
      ZX=X(IZ)
      IF((X1.LE.ZX).AND.(X2.GE.ZX)) THEN
        IF(IX1.EQ.0) IX1=IZ
        IX2=IZ
      ENDIF
 110  CONTINUE
      ELSE IF(ICALL.EQ.1) THEN
C----------------------------------/ICALL=1
      ZMIN=ABS(X1-X(1))
      IMIN=1
      DO 210 IX=1,NX
        ZTES=ABS(X1-X(IX))
        IF(ZTES.LT.ZMIN) THEN
          IMIN=IX
          ZMIN=ZTES
        ENDIF
 210  CONTINUE
C
      IX1=IMIN
C----------------------------------/icall=3
      else if(icall.eq.3) then
         do ix=1,nx-1
            ixp=ix+1
            if((x(ix).le.x1).and.(x1.le.x(ixp))) then
      	 ix1=ix
      	 ix2=ixp
      	 go to 500
            endif
         enddo
      ENDIF
C
 500  continue
      RETURN
      END
C******************** END FILE EXPCKI.FOR ; GROUP EXSUBS ******************
