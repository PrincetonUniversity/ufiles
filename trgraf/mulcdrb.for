C  MULCDRB
C
C  J F Schivell  23 Aug 95  --  based on MULCDR; removed key
C
C  DRAW MULTIPLE (X,Y) CONTOURS  - BASED ON MULFDR - DMC 9/84
C
C  DRAW A MULTIPLOT WITH KEY.
C  CALLER SETS SCALE AND AXIS TYPE
C
C  DATA TO BE PLOTTED IS PASSED IN 2D ARRAYS
C
C  NO USER I/O OR CONTROL IS PROVIDED EXCEPT TO CALL UPAUST AFTER
C  THE MULTIPLOT IS COMPLETE
C
      SUBROUTINE MULCDRB(
     >   R,Y,ND1,NC,NNC,Z,NZ,ISELF,NSEL,IAXSEL,
     >   RUN,YUN,MNLAB)
C
      use bottoms_up
      use mulfdr_blk
      use plot1mmx
C
C  # SYMBOLS PER LINE (MAX)
      implicit NONE
      integer, PARAMETER :: NSYMBS=5
C
C  ARGUMENTS -- ALL MUST BE SUPPLIED BY CALLER (OUTPUT IS GRAPH)
C
C  IN COMMON/DPLOT1/  XMIN,XMAX,YMIN,YMAX -- THE SCALE OF THE GRAPH
C
C  PASSED:
c
C   R(ND1,NZ)  DATA BUFFER OF INDICATED DIMENSIONS-- HORIZONTAL CONTOUR
C              PARAMETRIZATION
C   Y(ND1,NZ)  DATA BUFFER OF INDICATED DIMENSIONS-- VERTICAL CONTOUR
C              PARAMETRIZATION
C
C  **NC** IS THE ACTUAL NUMBER OF PTS IN EACH CONTOUR (.LE.ND1)
C   NC(NNC) -- NC(1) IS THE NO. OF PTS IN THE 1ST CONTOUR ...
C              NC(MIN(NZ,NNC)) IS THE NO. OF PTS IN THE NZ'TH CONTOUR
C              NOTE THAT BY PASSING NNC=1 ONE MAY SPECIFY THE SAME
C              NO. OF POINTS IN ALL CONTOURS, OR BY SETTING NNC=NZ
C              AND FILLING THE NC INTEGER ARRAY ONE MAY HAVE A DIFFERENT
C              NO. OF POINTS FOR EACH CONTOUR.  HOWEVER,
C                  2 .LE. NC(J) .LE. ND1
C              MUST BE MAINTAINED FOR ALL J BTW 1 AND MIN(NZ,NNC)
C
C   Z(NZ)      CONTOUR INDEXING
C
C   ISELF(NSEL) SELECTS WHICH CONTOURS ARE TO APPEAR
C     ON THE MULTIPLOT.
C   **NSEL** IS THE NUMBER OF CURVES TO APPEAR ON THE MULTIPLOT
C
C   IAXSEL      SELECTS THE AXIS TYPE:
C     =-11 FOR LINEAR-LINEAR
C     =-15 FOR LINEAR-LOG(Y)
C     =-30 FOR LOG(X)-LOG(Y)
C     =-40 FOR LOG(X)-LINEAR
C   -----------
C   CHARACTER LABELS:
C   ------------
C    RUN        10 CHAR HORIZONTAL AXIS PHYSICAL UNITS LABEL
C    YUN        10 CHAR VERTICAL AXIS PHYSICAL UNITS LABEL
C
C   MNLAB       3 60 CHARACTER "MAIN LABELS"-- 1ST TWO APPEAR
C               ABOVE GRAPH, LAST ONE BELOW
C
      integer nd1,nz,nnc,iaxsel,nsel,irast
      REAL R(ND1,NZ),Y(ND1,NZ),Z(NZ)
      INTEGER ISELF(NSEL)
      INTEGER NC(NNC)
      CHARACTER*(*) MNLAB(3)
      CHARACTER*(*) RUN,YUN
C
C  LOCAL VARIABLES-- DETERMINE LOCATION OF LABELS,
C     SIZE OF GRAPH, ETC.
C
      REAL ZXL(NSYMBS),ZYL(NSYMBS)
C--------------------------
C  other local stuff
C
      integer inum,inums,immnb,intvl,isinc,ist,i,ind,icod,ijust,iwarn
      real zzx,zzy
      character*64 wname
      character*40 gname,dname
      character*10 zext
C
C  Initialize mulfdr_blk
      call mulfdr_init
C
C--------------------------------------------------
C  START OF EXECUTABLE CODE
C
      call bu_eval(mnlab(3),' ',irast)  ! check for multi-line lower label
C
C  # PTS IN FIRST CURVE
      INUM=NC(1)
C  # OF PTS IN PLOT WINDOW
      INUMS=MAX0(1,IMMNB(R(1,ISELF(1)),Y(1,ISELF(1)),INUM))
C  SYMBOL SPACING VARIABLES
      INTVL=MAX0(1,(INUMS+NSYMBS-1)/NSYMBS)
      IF(INTVL.LT.NSYMBS) THEN
        ISINC=INTVL-1
      ELSE
        ISINC=INTVL/NSYMBS
      ENDIF
C
      IST=MAX0(1,(INTVL/2-ISINC))
C
C  DRAW AXES
C
      ZZX=XMAX+5.0*(XMAX-XMIN)
      ZZY=YMAX+5.0*(YMAX-YMIN)
      CALL DPLOTM(IXRG,IYRG,ZZX,ZZY,1,IAXSEL)
C
C  PLOT THE CURVES
C
      DO 30 I=1,NSEL
C
        INUM=NC(MIN0(I,NNC))  ! NO. OF PTS THIS CURVE
C
        IND=ISELF(I)          ! INDEX
C
C  LINE STYLE CODE
        ICOD=1
C  PLOT CONTOUR CURVE
        CALL DPLOTM(IXRG,IYRG,R(1,IND),Y(1,IND),INUM,ICOD)
 30   CONTINUE
C
C  DRAW KEY FRAME
C
      CALL MOVABS(IXRG(2),IYRG(2))
      CALL DRWABS(IXRG(3),IYRG(2))
      CALL DRWABS(IXRG(3),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(2))
C  FUNCTION UNITS
      CALL GRFLBL(IXRXU,IYRXU+irast,RUN,0,-1,0)
      CALL GRFLBL(IXRFU,IYRFU,YUN,0,-1,1)
C  MAIN GRAPH LABELS
      IJUST=-1
      DO I=1,3
         if(i.le.2) then
            CALL GRFLBL(IXRMN,IYRMN(I),MNLAB(I),60,IJUST,0)
         else
            ijust=0
            if(ibrk_lbl3.eq.0) then
               CALL GRFLBL(IXRMN,IYRMN(I),MNLAB(I),60,IJUST,0)
            else
               call grflbl(ixrmn,iyrmn(i)+irast,
     >              mnlab(i)(1:ibrk_lbl3),60,ijust,0)
               call grflbl(ixrmn,iyrmn(i),
     >              mnlab(i)(ibrk_lbl3+1:),60,ijust,0)
            endif
         endif
      ENDDO
C ---------
C GRAPH COMPLETED
C
      CALL UPAUST(2)

      call bu_reset

      RETURN
      END
