C******************** START FILE MULFDR.FOR ; GROUP MULFDR ******************
C------------------------------------------------------------
C  MULFDR
C
C  MODIF D. MC CUNE 26 OCT 1983
C  SUPPORT SINGLE PT. MODE DISPLAY--
C  IF PASSED X COORD FOR PLOTTING CONTAINS ONLY ONE POINT,
C  DATA IS PRINTED, NOT PLOTTED
C
C  DRAW A MULTIPLOT WITH KEY.
C  CALLER SETS SCALE AND AXIS TYPE
C  DATA TO BE PLOTTED IS PASSED IN A 2D ARRAY; INTEGER FLAGS
C  SELECT ALONG WHICH DIMENSION TO PLOT AND WHICH ROWS/ COLUMNS
C  OF THE 2D ARRAY TO PLOT
C
C  NO USER I/O OR CONTROL IS PROVIDED EXCEPT TO CALL UPAUST AFTER
C  THE MULTIPLOT IS COMPLETE
C
      SUBROUTINE MULFDR(F,NF1,X1,NX1,X2,NX2,ZW,
     >     ISELX,ISELF,NSEL,IAXSEL,
     >     FUNS,XUNS,
     >     MNLAB,KYLAB,LBSEL,tshift)
C
      use bottoms_up
      use mulfdr_blk
      use plot1mmx
C
C  # SYMBOLS PER LINE (MAX)
      PARAMETER (NSYMBS=5)
C
C  ARGUMENTS -- ALL MUST BE SUPPLIED BY CALLER (OUTPUT IS GRAPH)
C
C  IN COMMON/DPLOT1/  XMIN,XMAX,YMIN,YMAX -- THE SCALE OF THE GRAPH
C
C  PASSED:
C   F(NF1,NX2)  DATA BUFFER OF INDICATED DIMENSIONS
C   X1(NX1)     1ST INDEPENDANT VARIABLE OF F - ALONG 1ST DIMENSION
C   X2(NX2)     2ND INDEPENDANT VARIABLE OF F - ALONG 2ND DIMENSION
C     NOTE X1 AND X2 NEED NOT BOTH BE DEFINED (THOUGH A DIMENSION
C     .GT. 0 MUST BE SUPPLIED) -- SEE INTEGER VARIABLE ISELX
C   ZW(NX2)     WORKSPACE.  BIG ENUF TO HOLD ONE SLICE ALONG 2ND
C     DIMENSION OF F  -- USED ONLY IF ISELX=2
C   ISELX       PLOTTING INDEPENDANT VARIABLE SELECT ***
C     ISELX=1 INDICATES X1 IS TO BE PLOTTED ALONG THE X AXIS
C             (HORIZONTAL AXIS OF GRAPH); AND THERE ARE NX1 PTS.
C     ISELX=2 INDICATES X2 AND NX2.
C   ISELF(NSEL) SELECTS WHICH COLUMNS (OR ROWS) OF F ARE TO APPEAR
C     ON THE MULTIPLOT.  ISELX=1 MEANS ROWS ARE PLOTTED (1ST INDEX
C     OF F VARIED IN PLOT CURVE, 2ND INDEX FIXED), ISELX=2 FOR COLUMNS
C     **NSEL** IS THE NUMBER OF CURVES TO APPEAR ON THE MULTIPLOT
C   IAXSEL      SELECTS THE AXIS TYPE:
C     =-11 FOR LINEAR-LINEAR
C     =-15 FOR LINEAR-LOG(Y)
C     =-30 FOR LOG(X)-LOG(Y)
C     =-40 FOR LOG(X)-LINEAR
C   -----------
C   CHARACTER LABELS:
C   ------------
C   FUNS        10 CHARACTER FUNCTION (F) UNITS LABEL
C   XUNS        10 CHARACTER "X" AXIS UNITS LABEL
C   MNLAB       3 60 CHARACTER "MAIN LABELS"-- 1ST TWO APPEAR
C               ABOVE GRAPH, LAST ONE BELOW
C   KYLAB       5 17 CHARACTER KEY LABELS -- 1ST TWO APPEAR
C               AT TOP OF KEY BOX, LAST THREE AT BOTTOM
C   LBSEL(NSEL) NSEL 10 CHARACTER SELECT LABELS-- 10 CHARACTERS
C               TO ID EACH CURVE IN THE MULTIPLOT (1,2,... NSEL)
c
c   tshift(nsel) time displacement (real) -- add to labeling if non-zero
C
C *NOTE LABELS DO *NOT* NEED TO BE TERMINATED WITH "$", BUT A "$" IN
C  THE LABEL WILL CHOP OFF OUTPUT OF THE LABEL AT THAT POINT***
C
      REAL F(NF1,NX2),X1(NX1),X2(NX2),ZW(NX2)
      INTEGER ISELF(NSEL)
      CHARACTER*(*) MNLAB(3)
      CHARACTER*(*) KYLAB(5)
      CHARACTER*(*) LBSEL(NSEL),XUNS,FUNS
      real tshift(nsel)
      real zzx(1),zzy(1)
C
      CHARACTER*128 ZBUF
C
C  LOCAL VARIABLES - CONTROL SYMBOL/LINE STYLES OF MULTIPLOT
C
      DIMENSION ZXL(NSYMBS),ZYL(NSYMBS)
      INTEGER IPLOTF(2,15)
C
C  Initialize mulfdr_blk
      call mulfdr_init
C
C  SELECT LINE DASH STYLE / SYMBOL COMBINATIONS
      DATA IPLOTF/1,13,2,14,6,15,4,16,3,13,1,14,2,15,6,16,
     >              4,13,3,14,1,15,2,16,6,13,4,14,3,15/
C
C-------------------------------------------------------
C  START OF EXECUTABLE CODE
C
C  MAX OF 15 FCNS
C
      call bu_eval(mnlab(3),' ',irast)  ! check for multi-line lower label
C
      INSEL=MIN0(15,NSEL)
C  # PTS PER CURVE
      IF(ISELX.EQ.1) THEN
        INUM=NX1
        IF(INUM.GT.1) THEN
          CALL IXMMX(X1,NX1,IXA,IXB)
          INUMS=IXB-IXA-1
        ELSE
          ZX0D=X1(1)
        ENDIF
      ENDIF
      IF(ISELX.EQ.2) THEN
        INUM=NX2
        IF(INUM.GT.1) THEN
          CALL IXMMX(X2,NX2,IXA,IXB)
          INUMS=IXB-IXA-1
        ELSE
          ZX0D=X2(1)
        ENDIF
      ENDIF
C  SYMBOL SPACING VARIABLES
      IF(INUM.GT.1) THEN
C====================================/ PLOT CURVES /=============
      INTVL=MAX0(1,(INUMS+NSYMBS-1)/NSYMBS)
      ISINC=INTVL-1
      IF(INTVL.LT.NSYMBS) GO TO 10
      ISINC=INTVL/NSYMBS
C
 10   CONTINUE
      IST=MAX0(1,(INTVL/2-ISINC))
C  DRAW AXES
      ZZX=XMAX+5.0*(XMAX-XMIN)
      ZZY=YMAX+5.0*(YMAX-YMIN)
      CALL DPLOTM(IXRG,IYRG,ZZX,ZZY,1,IAXSEL)
C
C  PLOT THE CURVES
C
      DO 30 I=1,INSEL
        IND=ISELF(I)
        DO 25 J=1,INUM
C  COPY COL. (2ND DIM. SLICE) INTO WORKSPACE
          IF(ISELX.EQ.2) ZW(J)=F(IND,J)
 25     CONTINUE
C  LINE STYLE CODE
        ICOD=IPLOTF(1,I)
C  PLOT FCN (CURVE)
        IF(ISELX.EQ.1) CALL DPLOTM(IXRG,IYRG,X1,F(1,IND),INUM,ICOD)
        IF(ISELX.EQ.2) CALL DPLOTM(IXRG,IYRG,X2,ZW,INUM,ICOD)
C  PLOT SYMBOLS ON CURVE
        ICT=0
        IST=IST+ISINC
        IF(IST.GT.INTVL) IST=IST-INTVL
        IPT=IXA+IST-INTVL
        DO 26 J=1,NSYMBS
          IPT=IPT+INTVL
          IF(IPT.GT.INUM) GO TO 26
          ICT=ICT+1
          IF(ISELX.EQ.1) ZXL(ICT)=X1(IPT)
          IF(ISELX.EQ.2) ZXL(ICT)=X2(IPT)
          IF(ISELX.EQ.1) ZYL(ICT)=F(IPT,IND)
          IF(ISELX.EQ.2) ZYL(ICT)=ZW(IPT)
 26     CONTINUE
        CALL DPLOTM(IXRG,IYRG,ZXL,ZYL,ICT,IPLOTF(2,I))
 30   CONTINUE
C
C====================================/ OR DISPLAY SINGLE POINTS /====
C
      ELSE
C  DRAW BOX
      CALL UBINIT
      CALL MOVABS(IXRG(1),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(2))
      CALL DRWABS(IXRG(1),IYRG(2))
      CALL DRWABS(IXRG(1),IYRG(1)+irast)
C  WRITE DISPLAY FROM UPPER LEFT CORNER DOWN
      IX0D=IXRG(1)+30
      IY0D=IYRG(2)-2*IDY0D
C
      CALL GRFLBL(IX0D,IY0D,'SINGLE POINTS DISPLAY: '//FUNS,0,-1,0)
      IY0D=IY0D-IDY0D
C
      WRITE(ZBUF,3002) ZX0D,XUNS
 3002 FORMAT('FIXED COORD: ',1PE12.5,1X,A10,'$')
      CALL GRFLBL(IX0D,IY0D,ZBUF,0,-1,0)
      IY0D=IY0D-IDY0D
C
      IX0DA=IX0D+180
      CALL GRFLBL(IX0D,IY0D,'LABEL',0,-1,0)
      CALL GRFLBL(IX0DA,IY0D,'VALUE',0,-1,0)
      if(nf1.ge.2) call grflbl(ix0da+150,iy0d,'(+/- RMS Var.)',0,-1,0)
      IY0D=IY0D-IDY0D
C
      CALL GRFLBL(IX0D,IY0D,'------------',0,-1,0)
      CALL GRFLBL(IX0DA,IY0D,'-------------------------',0,-1,0)
      IY0D=IY0D-IDY0D
C
C  DISPLAY SINGLE PT VALUES
      DO 60 I=1,INSEL
        IND=ISELF(I)
        iflag2=0
        IF(ISELX.EQ.1) THEN
          ZD0D=F(1,IND)
          if(nf1.ge.2) then
             ZDVAR=F(2,IND)
             iflag2=1
          endif
        ELSE
          ZD0D=F(IND,1)
        ENDIF
C
        ILS=ILNURD(LBSEL(I))
        ZBUF=LBSEL(I)(1:ILS)//':  '
        CALL GRFLBL(IX0D,IY0D,ZBUF,0,-1,0)
C
        ZBUF=' '
        if(iflag2.eq.0) then
           WRITE(ZBUF,3005) ZD0D
        else
           write(zbuf,3006) zd0d,zdvar
        endif
 3005   FORMAT(1PE12.5)
 3006   format(1pe12.5,1x,1pe12.5)
        CALL GRFLBL(IX0DA,IY0D,ZBUF,0,-1,0)
C
        IY0D=IY0D-IDY0D
        if(tshift(i).ne.0) then
           zbuf=' '
           if(tshift(i).gt.0.0) then
              write(zbuf,'("(time shift +",1pe10.4," sec.)")') tshift(i)
           else
              write(zbuf,'("(time shift ",1pe11.4," sec.)")') tshift(i)
           endif
           call grflbl(ix0d+75,iy0d,zbuf,0,-1,0)
           IY0D=IY0D-IDY0D
        endif
 60   CONTINUE
C==============================================================
      ENDIF
C
C  DRAW KEY FRAME
C
      CALL MOVABS(IXRG(2),IYRG(2))
      CALL DRWABS(IXRG(3),IYRG(2))
      CALL DRWABS(IXRG(3),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(1)+irast)
      CALL DRWABS(IXRG(2),IYRG(2))
C  KEY LABELS
      DO 100 I=1,5
         ioff=0
         if(i.ge.3) ioff=irast
         CALL GRFLBL(IXRKY,IYRKY(I)+ioff,KYLAB(I),17,0,0)
 100  CONTINUE
C  KEY LINES AND LABELS
      IF(INUM.GT.1) THEN
C============/ ONLY IF CURVES WERE PLOTTED
      IYL=IYRKSL+IDRKY
      IYN=IYRKLA+IDRKY
      DO 110 I=1,INSEL
        IYL=IYL-IDRKY
        IYN=IYN-IDRKY
        CALL DPLOTK(IYL,IXRKSL,IYN,IXRKLA,IPLOTF(1,I),LBSEL(I))
        if(tshift(i).ne.0.0) then
           zbuf=' '
           if(tshift(i).gt.0.0) then
              write(zbuf,'("+",1pe10.4," sec")') tshift(i)
           else
              write(zbuf,'(1pe11.4," sec")') tshift(i)
           endif
           IYL=IYL-IDRKY
           IYN=IYN-IDRKY
           call grflbl(ixrksl,iyn,zbuf,0,-1,0)
        endif
 110  CONTINUE
C  FUNCTION UNITS
      CALL GRFLBL(IXRXU,IYRXU+irast,XUNS,0,-1,0)
      CALL GRFLBL(IXRFU,IYRFU,FUNS,0,-1,1)
      ELSE
C===================================
C  IN SINGLE PT MODE GEN LABEL REFERING TO DISPLAY
      CALL GRFLBL(IXRKSL,IYRKSL,' ID = DATA LABEL ',17,0,0)
C===================================
      ENDIF
C  MAIN GRAPH LABELS
      IJUST=-1
      ioff=0
      DO I=1,3
         if(i.le.2) then
            CALL GRFLBL(IXRMN+ioff,IYRMN(I),MNLAB(I),72,IJUST,0)
         else
            ijust=0
            ioff=-120
            if(ibrk_lbl3.eq.0) then
               CALL GRFLBL(IXRMN+ioff,IYRMN(I),MNLAB(I),72,IJUST,0)
            else
               call grflbl(ixrmn+ioff,iyrmn(i)+irast,
     >              mnlab(i)(1:ibrk_lbl3),72,ijust,0)
               call grflbl(ixrmn+ioff,iyrmn(i),
     >              mnlab(i)(ibrk_lbl3+1:),72,ijust,0)
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
C******************** END FILE MULFDR.FOR ; GROUP MULFDR ******************
