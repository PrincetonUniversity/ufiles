C-------------------------------------------------------------
C GRFEBY - PLOT HORIZONTAL (X) ERROR BAR
C
      SUBROUTINE GRFEBY(X0,Y0,YEB,ZDX)
C
C  (X0,Y0) PT ABOUT WHICH ERROR BAR IS DRAWN
C  YEB  Y ERROR (UNCERTAINTY) +/- YEB ABOUT X0
C  ZDX  HALF-HEIGHT OF "T" END OF ERROR BAR TO DRAW
C
C      ___
C       |
C       |
C       X
C       |
C       |
C      ---
C
      REAL XPERR(2),YPERR(2)
C
      ILINE=1
C
      YPERR(1)=Y0-YEB
      YPERR(2)=Y0+YEB
C
      XPERR(1)=X0
      XPERR(2)=X0
C
      CALL DPLOT1(XPERR,YPERR,2,ILINE)
C
      YPERR(1)=Y0-YEB
      YPERR(2)=Y0-YEB
C
      XPERR(1)=X0-ZDX
      XPERR(2)=X0+ZDX
C
      CALL DPLOT1(XPERR,YPERR,2,ILINE)
C
      YPERR(1)=Y0+YEB
      YPERR(2)=Y0+YEB
C
      CALL DPLOT1(XPERR,YPERR,2,ILINE)
C
      RETURN
      END
