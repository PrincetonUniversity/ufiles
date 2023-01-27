C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  GRAFX1 - DMC - REWRITTEN BASIC F(X) GRAPHICS INTERFACE ROUTINE
C    THIS ROUTINE CONTROLS DRAWING AND LABELING OF A 1 PAGE PLOT
C    OF A SINGLE FUNCTION F(X), AND GENERATION OF A REPLOT/RESCALE
C    (UREAD) OPTION MENU
C
C    A NEW CALLING ARGUMENT LIST IS PROVIDED
C
      SUBROUTINE GRAFX1(X,Y,N,XUN,YUN,LBL1,LBL2,LBL3)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL X(N),Y(N)
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
      character*128 zfile
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
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
      CALL DPLOT1(X,Y,N,ICOD) ! DRAW GRID AND F(X) CURVE
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
      CALL GRAOPT('SAO',ISEL,ILOCSC,IOPCOD,GRADUM)
C
C  ACT ON USER REPLY - REPLOT?
C
      IF(IOPCOD.EQ.1) THEN
         GO TO 100
      ELSE IF(IOPCOD.EQ.2) THEN
         RETURN
      else if(iopcod.eq.13) then
         call unixfile('filename for 2 column ascii output file',zfile)
         call uread_freelun(ilun)
         open(unit=ilun,file=zfile,status='unknown',iostat=istat)
         if(istat.ne.0) then
            call uermsg('?GRAFX1: file open failure: ',trim(zfile),0)
            close(unit=ilun,iostat=istat)
         else
            write(ilun,8001) 'LBL1',trim(lbl1)
            write(ilun,8001) 'LBL2',trim(lbl2)
            write(ilun,8001) 'LBL3',trim(lbl3)
            write(ilun,8001) 'XUN',trim(xun)
            write(ilun,8001) 'YUN',trim(yun)
            write(ilun,8002) n
            do ii=1,n
               write(ilun,8003) x(ii),y(ii)
            enddo
 8001       format(1x,a,' = "',a,'"')
 8002       format(1x,'N = ',i6,'; x in col.1, y in col.2:')
 8003       format(1x,1pe13.6,1x,1pe13.6)
            close(unit=ilun)
         endif
         go to 200
      ENDIF
C
      CALL UERMSI(' ?GRAFX1 *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
