C------------------------------------------------------------------
C  GSCRST - RESTORE PLOT SCALE TO FULL SCALE OF DATA
C   (AS PREVIOUSLY EVALUATED BY A CALL TO GSCSET)
C
      SUBROUTINE GSCRST
C
      use gaxes_blk
      use gscale_blk
      use plot1mmx
C
C  START OF EXECUTABLE CODE
C
      XMIN=GMINX0
      XMAX=GMAXX0
      IF((IXXMAP(IAXSEL).EQ.1).AND.(GMNXP0.GT.0.0)) THEN
        XMIN=GMNXP0
        XMAX=AMAX1(XMAX,1.1*XMIN)
      ENDIF
C
      YMIN=GMINY0
      YMAX=GMAXY0
      IF((IXYMAP(IAXSEL).EQ.1).AND.(GMNYP0.GT.0.0)) THEN
        YMIN=GMNYP0
        YMAX=AMAX1(YMAX,1.1*YMIN)
      ENDIF
C
      RETURN
      END
