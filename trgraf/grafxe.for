C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  GRAFXE - DMC - REWRITTEN BASIC F(X) FIT GRAPHICS INTERFACE ROUTINE
C    THIS ROUTINE CONTROLS DRAWING AND LABELING OF A 1 PAGE PLOT
C    OF A SINGLE FUNCTION F(X), AND GENERATION OF A REPLOT/RESCALE
C    (UREAD) OPTION MENU
C
C  PLOT A NUMERIC FIT CURVE (SOLID LINE)
C   PLOT ORIGINAL DATA (PT SYMBOLS)
C   PLOT OPTIONAL ERROR BARS ABOUT EACH ORIG. DATA PT
C
C
C    A NEW CALLING ARGUMENT LIST IS PROVIDED
C
      SUBROUTINE GRAFXE(X,Y,N,X2,Y2,N2,IX2E,X2E,IY2E,Y2E,
     >                    XUN,YUN,LBL1,LBL2,LBL3)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N)  ! NUMERIC FIT DATA CURVE (SOLID LINE)
      REAL X2(N2),Y2(N2)  ! "ORIGINAL" DATA (POINT PLOT)
C
      REAL X2E(N2)  ! HORIZ. ERROR BARS ON ORIG DATA PTS (OPTIONAL)
      REAL Y2E(N2)  ! VERT. ERROR BARS ON ORIG DATA PTS (OPTIONAL)
C
C    IX2E .GT. 0 IF HORIZ. ERROR BARS ARE TO BE PLOTTED
C    IY2E .GT. 0 IF VERT. ERROR BARS ARE TO BE PLOTTED
C
C      IF .GT. 0 THE NUMBER MAX(IX2E,IY2E)
C    INDICATES THE MAXIMUM NUMBER OF PTS WITH
C    ERROR BARS TO BE DRAWN
C
C   *ERROR BAR DATA* X2E(J) = +/- DELTA(X) UNCERTAINTY ABOUT X2(J)
C           *NOT PLOTTED* IF .LE. 0.0
C
C   *ERROR BAR DATA* Y2E(J) = +/- DELTA(Y) UNCERTAINTY ABOUT Y2(J)
C           *NEITHER X NOR Y ERROR BAR PLOTTED* IF .LE. 0.0
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  LOCAL
      REAL XERRB(2),YERRB(2)
C
C  CONTROL:
      INTEGER IMENU
C
C  IF IMENU=1 JUMP DIRECTLY TO REPLOT MENU (DO NOT DRAW GRAPH 1ST)
C
      integer icounter
      integer ie_counter
      character*10 zext
      integer, save :: iopcod=0
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
C
C-------------------------------------------------------------
C
      icounter=0
C-------------------------------------------------------------
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
C  IF ALL ERROR BAR PLOTTING IS OFF-- JUST DRAW FIT PT PLOT
      INUM=MAX0(IX2E,IY2E)
      IF(INUM.LE.0) THEN
        CALL DPLOT1(X2,Y2,N2,16)  ! INDIV. DATA POINTS PLOT
      ELSE
C  DRAW INUM PTS WITH ERROR BAR PLOTTING
        CALL GRFCNT(X2,N2,XMIN,XMAX,ICNT)
        INUM=MAX0(25,MIN0(INUM,ICNT))
        ZDX=0.333333*(XMAX-XMIN)/(INUM+1)
        ZDY=0.333333*(YMAX-YMIN)/(INUM+1)
        ZXSEP=2.05*ZDX
C
        XNEXT=XMIN+ZXSEP
        IPREV=0
C
C  PT. PLOT LOOP
        ie_counter=0
 150    CONTINUE
          IF(XNEXT.GT.XMAX) GO TO 160
          CALL GRFNEX(X2,N2,XNEXT,IPREV)
          IF(IPREV.GT.N2) GO TO 160
          XNEXT=X2(IPREV)+ZXSEP
C
          CALL DPLOT1(X2(IPREV),Y2(IPREV),1,16)  ! PLOT PT SYMBOL
C
          IF((IX2E.GT.0).AND.(X2E(IPREV).GT.0.0)) THEN
             ie_counter=ie_counter+1
             call make_zext
             CALL GRFEBX(X2(IPREV),Y2(IPREV),X2E(IPREV),ZDY)
          ENDIF
C
          IF((IY2E.GT.0).AND.(Y2E(IPREV).GT.0.0)) THEN
             ie_counter=ie_counter+1
             call make_zext
             CALL GRFEBY(X2(IPREV),Y2(IPREV),Y2E(IPREV),ZDX)
          ENDIF
C  LOOP END
          GO TO 150
 160    CONTINUE
      ENDIF
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
      CALL UERMSI(' ?GRAFXE *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      contains
         subroutine make_zext

         zext=' '
         if(ie_counter.le.9) then
            write(zext,'(I1)') ie_counter
         else if(ie_counter.le.99) then
            write(zext,'(I2)') ie_counter
         else if(ie_counter.le.999) then
            write(zext,'(I3)') ie_counter
         else if(ie_counter.le.9999) then
            write(zext,'(I4)') ie_counter
         else if(ie_counter.le.99999) then
            write(zext,'(I5)') ie_counter
         else if(ie_counter.le.999999) then
            write(zext,'(I6)') ie_counter
         endif

         end subroutine make_zext
      END
