C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  GRAFC3 - DMC - REWRITTEN BASIC F(X) GRAPHICS INTERFACE ROUTINE
C    THIS ROUTINE CONTROLS DRAWING AND LABELING OF A 1 PAGE PLOT
C    OF two contours (x(i),y(i),i=1,N) and
C    (x2(i),y2(i),i=1,N2) , AND GENERATION OF A REPLOT/RESCALE
C    (UREAD) OPTION MENU
C
C    A NEW CALLING ARGUMENT LIST IS PROVIDED
C
      SUBROUTINE GRAFC3(X,Y,N,X2,Y2,N2,X3,Y3,N3,XUN,YUN,
     >       LBL1,LBL2,LBL3)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N),X2(N2),Y2(N2),X3(N3),Y3(N3)
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  CONTROL:
      INTEGER IMENU
C
C  IF IMENU=1 JUMP DIRECTLY TO REPLOT MENU (DO NOT DRAW GRAPH 1ST)
C
      integer icounter
      integer, save :: iopcod=0
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
C
C-------------------------------------------------------------
C
      icounter=0
C-------------------------------------
C
C  SCALE SET SWITCHE CLEARED:
C
      ILOCSC=0
C
C  GET DEFAULT AXIS SELECTION
C
      CALL GAXGDF(ISEL)
C
      IF(IMENU.EQ.1) GO TO 200
C
C----------------------------------------------------------------
C  AUTOMATIC GENERATION OF PLOT
C
 100  CONTINUE
      icounter=icounter+1
C
C  SET AXES
C
      CALL GAXCUR(ISEL)
C
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X AND Y PLOT SCALE (USE GSCALE DEFAULTS)
        CALL GSCSET(X,Y,Y,0,N)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(X,N,Y,Y,0,N,1,1,1,1,N,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      zxmin=xmin
      zxmax=xmax
      zymin=ymin
      zymax=ymax
C
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X AND Y PLOT SCALE (USE GSCALE DEFAULTS)
        CALL GSCSET(X2,Y2,Y2,0,N2)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(X2,N2,Y2,Y2,0,N2,1,1,1,1,N2,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      xmin=amin1(xmin,zxmin)
      xmax=amax1(xmax,zxmax)
      ymin=amin1(ymin,zymin)
      ymax=amax1(ymax,zymax)
C
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X AND Y PLOT SCALE (USE GSCALE DEFAULTS)
        CALL GSCSET(X3,Y3,Y3,0,N3)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(X3,N3,Y3,Y3,0,N3,1,1,1,1,N3,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      xmin=amin1(xmin,zxmin)
      xmax=amax1(xmax,zxmax)
      ymin=amin1(ymin,zymin)
      ymax=amax1(ymax,zymax)
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD) ! AXIS (LOG/LIN) CHECK
C
      CALL GSCREC  ! RECORD PLOT SCALE IN MEMORY
C
      CALL DPLOT1(X,Y,N,ICOD) ! DRAW GRID AND F(X) CURVE
      CALL DPLOT1(X2,Y2,N2,2)
      CALL DPLOT1(X3,Y3,N3,2)
C
      CALL GRALBL(XUN,YUN,LBL1,LBL2,LBL3)  ! LABEL THE PLOT
C
      CALL UPAUST(2)  ! PAUSE AFTER PLOT
      GO TO 200
C
C  POST PLOT OPTIONS
C
 200  CONTINUE
C
C  GENERATE MENU AND FETCH USER REPLY
C
      CALL GRAOPT('SA',ISEL,ILOCSC,IOPCOD,GRADUM)
C
C  ACT ON USER REPLY - REPLOT?
C
      IF(IOPCOD.EQ.1) THEN
         GO TO 100
      ELSE IF(IOPCOD.EQ.2) THEN
         RETURN
      ENDIF
C
      CALL UERMSI(' ?GRAFC3 *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
