C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  GRAFXF - DMC - REWRITTEN BASIC F(X) FIT GRAPHICS INTERFACE ROUTINE
C    THIS ROUTINE CONTROLS DRAWING AND LABELING OF A 1 PAGE PLOT
C    OF A SINGLE FUNCTION F(X), AND GENERATION OF A REPLOT/RESCALE
C    (UREAD) OPTION MENU
C
C  PLOT A NUMERIC FIT CURVE (SOLID LINE)
C   PLOT ORIGINAL DATA (PT SYMBOLS)
C
C  PLOT SCALE DETERMINED BY FIT DATA
C   NORMALLY THE FIT, DRAWN AS A SOLID CURVE, SHOULD HAVE A NORMAL
C   (SORTED ASCENDING) X AXIS
C
C  THE ORIG. DATA (PT PLOT) MAY BE (X,Y) PAIRS IN ANY ORDER
C
C
C    A NEW CALLING ARGUMENT LIST IS PROVIDED
C
      SUBROUTINE GRAFXF(X,Y,N,X2,Y2,N2,
     >                    XUN,YUN,LBL1,LBL2,LBL3)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N)  ! NUMERIC FIT DATA CURVE (SOLID LINE)
      REAL X2(N2),Y2(N2)  ! "ORIGINAL" DATA (POINT PLOT)
C
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  LOCAL
      REAL XERRB(2),YERRB(2)
      integer n_sc
      real, dimension(:), allocatable :: x_sc,y_sc
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
C---------------------------------------------------------------
C
      n_sc=n+n2
      allocate(x_sc(n_sc),y_sc(n_sc))
      x_sc(1:n)=x
      x_sc(n+1:n+n2)=x2
      y_sc(1:n)=y
      y_sc(n+1:n+n2)=y2
C
C  SCALE SET SWITCH CLEARED:
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
        CALL GSCSET(x_sc,y_sc,y_sc,0,n_sc)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(x_sc,n_sc,y_sc,y_sc,0,n_sc,1,1,1,1,n_sc,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD) ! AXIS (LOG/LIN) CHECK
C
      CALL GSCREC  ! RECORD PLOT SCALE IN MEMORY
C
      CALL DPLOT1(X,Y,N,ICOD) ! DRAW GRID AND F(X) SOLID CURVE
C
      CALL DPLOT1(X2,Y2,N2,16)  ! INDIV. DATA POINTS PLOT
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
         deallocate(x_sc,y_sc)
         RETURN
      ENDIF
C
      CALL UERMSI(' ?GRAFXF *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  GRAFXF - DMC - REWRITTEN BASIC F(X) FIT GRAPHICS INTERFACE ROUTINE
C    THIS ROUTINE CONTROLS DRAWING AND LABELING OF A 1 PAGE PLOT
C    OF A SINGLE FUNCTION F(X), AND GENERATION OF A REPLOT/RESCALE
C    (UREAD) OPTION MENU
C
C  PLOT A NUMERIC FIT CURVE (SOLID LINE)
C   PLOT ORIGINAL DATA (PT SYMBOLS)
C
C  PLOT SCALE DETERMINED BY FIT DATA
C   NORMALLY THE FIT, DRAWN AS A SOLID CURVE, SHOULD HAVE A NORMAL
C   (SORTED ASCENDING) X AXIS
C
C  THE ORIG. DATA (PT PLOT) MAY BE (X,Y) PAIRS IN ANY ORDER
C
C
C    A NEW CALLING ARGUMENT LIST IS PROVIDED
C
      SUBROUTINE GRAFXF_icod(X,Y,N,X2,Y2,N2,
     >                    XUN,YUN,LBL1,LBL2,LBL3,icdd)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N)  ! NUMERIC FIT DATA CURVE (SOLID LINE)
      REAL X2(N2),Y2(N2)  ! "ORIGINAL" DATA (POINT PLOT)
C
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  LOCAL
      REAL XERRB(2),YERRB(2)
      integer n_sc
      real, dimension(:), allocatable :: x_sc,y_sc
C
C  CONTROL:
      INTEGER IMENU
C
C  IF IMENU=1 JUMP DIRECTLY TO REPLOT MENU (DO NOT DRAW GRAPH 1ST)
C
      integer icounter
      integer, save :: iopcod=0
      integer :: icdd
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
C
C-------------------------------------------------------------
C
      icounter=0
C---------------------------------------------------------------
C
      n_sc=n+n2
      allocate(x_sc(n_sc),y_sc(n_sc))
      x_sc(1:n)=x
      x_sc(n+1:n+n2)=x2
      y_sc(1:n)=y
      y_sc(n+1:n+n2)=y2
C
C  SCALE SET SWITCH CLEARED:
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
        CALL GSCSET(x_sc,y_sc,y_sc,0,n_sc)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(x_sc,n_sc,y_sc,y_sc,0,n_sc,1,1,1,1,n_sc,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD) ! AXIS (LOG/LIN) CHECK
C
      CALL GSCREC  ! RECORD PLOT SCALE IN MEMORY
C
      CALL DPLOT1(X,Y,N,ICOD) ! DRAW GRID AND F(X) SOLID CURVE
C
      CALL DPLOT1(X2,Y2,N2,icdd)  ! INDIV. DATA POINTS PLOT
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
         deallocate(x_sc,y_sc)
         RETURN
      ENDIF
C
      CALL UERMSI(' ?GRAFXF *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
C------------------------------------------------------------
      SUBROUTINE GRAFXF_icod0(X,Y,N,X2,Y2,N2,
     >                    XUN,YUN,LBL1,LBL2,LBL3,icdd)
C same as GRAFXF_icod but with small size of individual points
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N)  ! NUMERIC FIT DATA CURVE (SOLID LINE)
      REAL X2(N2),Y2(N2)  ! "ORIGINAL" DATA (POINT PLOT)
C
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  LOCAL
      REAL XERRB(2),YERRB(2)
      integer n_sc
      real, dimension(:), allocatable :: x_sc,y_sc
C
C  CONTROL:
      INTEGER IMENU
C
C  IF IMENU=1 JUMP DIRECTLY TO REPLOT MENU (DO NOT DRAW GRAPH 1ST)
C
      integer icounter
      integer, save :: iopcod=0
      integer :: icdd
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
C
C-------------------------------------------------------------
C
      icounter=0
C---------------------------------------------------------------
C
      n_sc=n+n2
      allocate(x_sc(n_sc),y_sc(n_sc))
      x_sc(1:n)=x
      x_sc(n+1:n+n2)=x2
      y_sc(1:n)=y
      y_sc(n+1:n+n2)=y2
C
C  SCALE SET SWITCH CLEARED:
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
        CALL GSCSET(x_sc,y_sc,y_sc,0,n_sc)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(x_sc,n_sc,y_sc,y_sc,0,n_sc,1,1,1,1,n_sc,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD) ! AXIS (LOG/LIN) CHECK
C
      CALL GSCREC  ! RECORD PLOT SCALE IN MEMORY
C
      CALL DPLOT1(X,Y,N,ICOD) ! DRAW GRID AND F(X) SOLID CURVE
C
      CALL DPLOT10(X2,Y2,N2,icdd)  ! INDIV. DATA POINTS PLOT
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
         deallocate(x_sc,y_sc)
         RETURN
      ENDIF
C
      CALL UERMSI(' ?GRAFXF *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
