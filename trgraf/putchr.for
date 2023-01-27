C******************** START FILE PUTCHR.FOR ; GROUP PUTCHR ******************
C====================================================
C  PUTCHR    D. MC CUNE  21-MAY-80
C
C  PUT A CHARACTER (A1) CENTERED AT X,Y LOCATION
C  ON SCREEN (IN AG4010 WINDOW)
C
      SUBROUTINE PUTCHR(X,Y,CHAR)
C
      use plot1mmx
C
C-----------------------------------
C
      CHARACTER*1 CHAR
C
C  ! SAVE LAST RASTER COORD WRITE (initialize to way-off-screen)
C
      integer, save :: IXPREV = -10000
      integer, save :: IYPREV = -10000
C
C   2 CHARS NEED TO FIT IN ZBUF
C  CHARACTER HALF WIDTH,HEIGHT IN SCREEN RASTER UNITS
C
      DATA IHY/7/
      DATA IHX/5/
C
C-----------------------------------
C
C  SKIP IF OUTSIDE RANGE
      IF( (X.GT.XMAX) .OR. (X.LT.XMIN) .OR.
     >      (Y.GT.YMAX) .OR. (Y.LT.YMIN)  ) RETURN
C
C  GENERATE LABEL IF FAR ENOUGH FROM PREVIOUS LABEL
      CALL WINCOT(X,Y,IX,IY) ! TRANSL TO SCREEN COORDS
      IF(MAX0(ABS(IXPREV-IX),ABS(IYPREV-IY)).LT.80) RETURN
C
      IXPREV=IX
      IYPREV=IY
C
      CALL MOVEA(X,Y)
C
      JHX=-IHX
      JHY=-IHY
C
      CALL MOVREL(JHX,JHY)
C
      CALL AGTHS(CHAR)
C
      RETURN
      	END
C******************** END FILE PUTCHR.FOR ; GROUP PUTCHR ******************
