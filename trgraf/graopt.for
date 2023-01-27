C------------------------------------------------------------------   GRAOPT
C  GRAOPT - GENERATE STANDARD MENU AFTER 2D PLOT
C   MENU OPTIONS ALLOW USER TO RESCALE PLOT, CHANGE AXES TYPES
C   CHANGE PLOT OR AXES DEFAULTS, OR QUIT PLOT
 
C
      SUBROUTINE GRAOPT(CTRL,ISEL,ILOCSC,IOPCOD,XOPT)        ! 7/1/91
C
C
C   CALLER DETERMINES WHAT OPTIONS ARE TO BE MADE AVAILABLE AT
C   THIS TIME
C
C  ** THIS ROUTINE IS A USER INTERFACE ONLY **
C  NO COMPUTATION IS DONE-- JUST INPUT USER REQUEST
C
C   mod dmc 6 Nov 1997 -- option for format control of Ufile output
C        has been added; ILOCSC is "reused" -- cf rplot_sub/grfah4.for
C                                               & rplot_sub/mulgrg.for
C
C   Mod TBT July 1, 1991 - Added option W to CTRL and negative IOPCOD.
C   Mod TBT Jun 12, 1991 - Replaced NLXOUT with LXOUT to get printing.
C   Mod TBT May 28, 1991 - Fix bug - reset IOPCOD=1 after wrong option code.
C   MOD TBT Apr  5, 1991 - Clean up menu.
C   MOD TBT Mar 27, 1991 -  Clean up menu.
C   MOD TBT OCT 24, 1989 -- OPTION TO DIFFERENTIATE.
C   MOD DMC FEB 1989 -- OPTION TO RESET SCALE VIA TK CROSS-HAIR
C   CURSOR INPUT OPTION
 
C	----------------------------------------------------------------------
 
      use plot1mmx
C
      CHARACTER*(*) CTRL
C
      CHARACTER*8 ZWORD
      CHARACTER*1 ZUCHAR
      CHARACTER*64 ZBUFF
C
      EXTERNAL XOPT
C
C  CTRL - STRING DETERMINING OPTIONS TO BE MADE AVAILABLE ON THIS CALL:
C   IF "A" IS IN CTRL, AXES OPTIONS  ARE MADE AVAILABLE
C   if "O" is in CTRL, column-oriented ASCII output is an option
C   IF "S" IS IN CTRL, SCALING OPTIONS  ARE MADE AVAILABLE
C   IF "Q" GEN MSG RE. "SQUARE SCALE" FOR GRFMM1
C   IF "U" IS IN CTRL, UFILES OUTPUT OPTION IS MADE AVAILABLE
C   IF "M" IS IN CTRL, SMOOTHING OPTION IS MADE AVAILABLE
C   IF "X" IS IN CTRL *CALL* PASSED EXTERNAL ROUTINE XOPT
C          (usually XOPT is a dummy but rplot_sub/plxopt is also used; this
C          one uses CTRL values "1", "T", "F",and "K" and UREAD character
C          codes "1", "I", "J", "F", and "K").
C   IF "D" IS IN CTRL, DIFFERENTIATION OPTION IS MADE AVAILABLE.  TBT
C   If "W" is in CTRL, WAIT for "P" command to plot, i.e. do NOT plot
C                      after smoothing or differentiating, etc.  TBT 7/91
C   If "E" is in CTRL, multigraph member edit option offerred.
C
C  ISEL - RETURN AXES CONTROL CODE
C
C  ILOCSC - RETURN SCALE ACTION CODE TO CALLER
C    =-1:  DO NOTHING
C    = 0:  RECOMPUTE PLOT SCALE (X AND Y BOTH)
C    = 1:  RECOMPUTE PLOT SCALE (Y ONLY)
C
C  IOPCOD - RETURN OVERALL ACTION CODE TO CALLER
C    =  1 - REPLOT (WITH POSSIBLE SCALE/AXES MODIFICATIONS)
C    =  2 - QUIT WITHOUT REPLOTTING
C    =  3 - SMOOTH DATA & Replot
C    = -3 - Smooth data & do NOT replot.
C    =  4 - RESTORE ORIGINAL UNSMOOTHED OR UNDIFFERENTIATED DATA & replot
C    = -4 - 4 but do NOT replot.
C    =  5 - UFILES OUTPUT (ASCII)
C    =  6 - UFILES OUTPUT (COMPRESSED BINARY)
C    =  7 - DIFFERENTIATE DATA & replot
C    = -7 - Differentiate data & do NOT replot.
C    =  8 thru 12 - See below. (PLXOPT)
C    = 13 -- Column oriented ASCII output
C
C  WARNING:
C  DMC 10 MAY 1991 -- CAREFUL ABOUT ADDING POSSIBLE VALUES TO IOPCOD
C  OUTPUT!  THE RPLOT ROUTINE PLXOPT IS PASSED TO THIS ROUTINE AS
C  "XOPT" BY RPLOT ROUTINES GRFAH4 AND MULGRG; XOPT OUTPUTS IOPCOD=8
C  THRU IOPCOD=12 SO DON'T REUSE THESE VALUES!
C
C-------------------------------------------------------------------
C
C  SET INTERNAL DEFAULTS
C
      LOGICAL LWAIT     ! T => wait for "P" option before plotting.
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
      data iformat/0/
C
C	---------------------------------------------------------------------
C
C       Note:
C           IOPCOD < 0 only if it has been set so before in this routine GRAOPT.
      IF (IOPCOD .LT. 0  .AND.  INDEX(CTRL,'W') .NE. 0) THEN
C          ."W" option has been previously enterred. Keep it on until "P" chosen
         LWAIT  = .TRUE.
         IOPCOD = -1      ! -1 => go right back to GRAOPT menu.
      ELSE
         LWAIT  = .FALSE.
         IOPCOD = 1
      END IF  ! iopcod
 
 
      ILOCSC=-1  ! DO NOT RECOMPUTE SCALE
C
      LUNTRM=LNOUT(0)
C
C-----------------------------------------------------------------
C  FIRST CHECK FOR TEKTRONIX CURSOR INPUT OPTION
C
C  IF NO TYPEAHEAD IS PENDING AND RESCALING OPTIONS ARE AVAILABLE...
C
      IF(LXOUT(0).AND.(INDEX(CTRL,'S').NE.0)) THEN
        CALL UPCGET(ZUCHAR)
        IF((ZUCHAR.NE.'S').and.(ZUCHAR.ne.'s')) GO TO 10
C
C  INPUT NEW SCALE VIA TEKTRONIX CURSOR
        CALL UGMODE
        CALL AGVCUR(ZUCHAR,ZX1,ZY1)
        IF((ZUCHAR.EQ.'q').OR.(ZUCHAR.EQ.'Q')) THEN
          CALL UAMODE
          GO TO 10
        ELSE
C  MARK FIRST CROSS-HAIR POINT ...
          CALL LINCLR(2)
          CALL WINCOT(ZX1,ZY1,IX,IY)
          CALL MOVABS(IX-100,IY)
          CALL DRWABS(IX+100,IY)
          CALL MOVABS(IX,IY-100)
          CALL DRWABS(IX,IY+100)
          CALL LINCLR(1)
          CALL TSEND
        ENDIF
        CALL AGVCUR(ZUCHAR,ZX2,ZY2)
        IF((ZUCHAR.EQ.'q').OR.(ZUCHAR.EQ.'Q')) THEN
          CALL UAMODE
          GO TO 10
        ENDIF
C
C  ENCODE AND INSERT EQUIVALENT COMMAND STRING IN UREAD TYPEAHEAD BUFFER
        ZXMIN=AMIN1(ZX1,ZX2)
        ZXMAX=AMAX1(ZX1,ZX2)
        ZYMIN=AMIN1(ZY1,ZY2)
        ZYMAX=AMAX1(ZY1,ZY2)
C
C  IF BOTH INPUT X ARE OUTSIDE THE OLD RANGE USE INPUT Y ONLY:
        ZBUFF=' '
        IF((ZXMAX.LT.XMIN).OR.(ZXMIN.GT.XMAX)) THEN
          WRITE(ZBUFF,3001) ZYMIN,ZYMAX
 3001       FORMAT('S X X ',2(1PE13.6,1X))
C  IF BOTH INPUT Y ARE OUTSIDE THE OLD RANGE USE INPUT X ONLY
        ELSE IF((ZYMAX.LT.YMIN).OR.(ZYMIN.GT.YMAX)) THEN
          WRITE(ZBUFF,3002) ZXMIN,ZXMAX
 3002     FORMAT('X ',2(1PE13.6,1X))
        ELSE
C  OTHERWISE USE ALL INPUT COORDS
          WRITE(ZBUFF,3003) ZXMIN,ZXMAX,ZYMIN,ZYMAX
 3003     FORMAT('S ',4(1PE13.6,1X))
        ENDIF
        CALL URDTTY(ZBUFF) ! INSERT COMMAND INSTREAM
        IF(LXOUT(0)) CALL UAMODE
      ENDIF
C-----------------------------------------------------------------
C
 10   CONTINUE
      CALL NEUPAG
 11   CONTINUE
C
C  WRITE MENU IF AT TTY...
C
      IF (LXOUT(0))  THEN
          WRITE (LUNTRM,999)
  999     FORMAT(
     1      ' GRAOPT - OPTIONS: ENTER "C" TO SEE THE ENTIRE MENU')
      END IF  ! LXOUT
 
 12   CONTINUE
      CALL UREAD(
     >    'GRAOPT:  ENTER ONE LETTER OPCODE (.C/A/S/X/Z/G/P/Q):$$')
 
C	---------------------------------------------------------------
      IF (LCHAR('C'))  THEN    ! LOOK AT THE ENTIRE MENU.
 
       IF(LXOUT(0)) THEN
        WRITE(LUNTRM,1001)
 1001 FORMAT(/' GRAOPT - OPTIONS AFTER PLOT:')
        IF(INDEX(CTRL,'A').NE.0) THEN
          WRITE(LUNTRM,1002)
 1002 FORMAT(/' *AXIS TYPE CONTROL OPTIONS*'/
     >'  "A" CHANGE AXIS TYPES (LOG/LINEAR) AND PLOT AGAIN'/
     >'  "B" CHANGE AXIS DEFAULTS USED ON ENTRY TO PLOT ROUTINE')
        ENDIF
        IF(INDEX(CTRL,'Q').NE.0) WRITE(LUNTRM,1013)
 1013 FORMAT(/' % CURRENT PLOT TYPE USES "SQUARE SCALE"')
        IF(INDEX(CTRL,'S').NE.0) THEN
          WRITE(LUNTRM,1003)
 1003 FORMAT(/' *PLOT RESCALE OPTIONS* '/
     >'      --> ALSO TRY HITTING "S" ON UPAUSE BEEP'/
     >'  "R" RESTORE SCALE FROM A PREVIOUSLY DRAWN PLOT'/
     >'  "S" CHANGE SCALE OF PLOT (X AND Y) AND PLOT AGAIN'/
     >'  "X" CHANGE X SCALE, AUTO-RESET Y SCALE, AND PLOT AGAIN'/
     >'  "Z" ZOOM IN/OUT IN X, AUTO-RESET Y, AND PLOT AGAIN'/
     >'  "T" CHANGE *DEFAULT SCALING* USED ON ENTRY TO PLOT ROUTINE'/
     >'  "G" MODIFY MISC. PLOT DEFAULTS: GRID/FRAME/TIC/LINE SYTLE')
        ENDIF
        IF(INDEX(CTRL,'U').NE.0) THEN
          WRITE(LUNTRM,1004)
 1004 FORMAT(
     >'  "U" TO WRITE 1D',
     >       ' *PORTABLE BINARY* UFILE OF DATA IN PLOT ROUTINE'/
     >'  "V" TO WRITE 1D *ASCII* UFILE OF DATA IN PLOT ROUTINE'/
     >'  "4" To write 1d Ufile, NetCDF/HDF format options')
        ENDIF
 
        IF(INDEX(CTRL,'M').NE.0) THEN
          WRITE(LUNTRM,1005)
 1005       FORMAT(/'  "M" TO SMOOTH THE PLOT DATA')
          WRITE(LUNTRM,1008)
 1008       FORMAT('  "L" TO RESTORE UNPROCESSED PLOT DATA')
        ENDIF
 
        IF(INDEX(CTRL,'D').NE.0) THEN
          WRITE(LUNTRM,1007)
 1007     FORMAT('  "D" TO DIFFERENTIATE THE PLOT DATA')
          IF (INDEX(CTRL,'M') .EQ. 0) WRITE(LUNTRM,1008)
        ENDIF
 
        IF(INDEX(CTRL,'W').NE.0) THEN
          WRITE(LUNTRM,1009)
 1009     FORMAT('  "W" to WAIT for "P" before plotting')
        ENDIF
 
        if(index(ctrl,'E').ne.0) then
          write(luntrm,1011)
 1011     format('  "+" (plus sign) to edit multigraph plot contents')
        endif

        if(index(ctrl,'O').ne.0) then
          write(luntrm,1010)
 1010     format('  "O" (the letter O) to output 2 column ascii file')
        endif
C  EXTERNAL ROUTINE
        IF(INDEX(CTRL,'X').NE.0) CALL XOPT(1,CTRL,IOPCOD)
 
C  THESE OPTIONS ARE ALWAYS AVAILABLE
        WRITE(LUNTRM,1006)
 1006 FORMAT(/'  "P" JUST PLOT AGAIN (USING NEW DEFAULTS)'/
     >'  "Q" OR "N" TO QUIT PLOTTING (ALTERNATE RETURN CONTROL)')
       ENDIF     ! LXOUT
C       -------------------------
       GO TO 12  ! REPROMPT
 
      END IF   ! LCHAR('C)'
C
C	----------------------------------------------------------------
 
C  CHECK THE OPTIONS WHICH ARE ALWAYS AVAILABLE FIRST
C
      IF(LCHAR('N')) THEN
C  EXIT "N"
        ISEL=1
        IOPCOD=2
        RETURN
      ELSE IF(LCHAR(' ')) THEN
C  EXIT "N" ALSO
        ISEL=1
        IOPCOD=2
        RETURN
      ELSE IF(LCHAR(',')) THEN
C  EXIT "N" ALSO AGAIN
        ISEL=1
        IOPCOD=2
        RETURN
      ELSE IF(LCHAR('Q')) THEN
C  EXIT "Q"
        ISEL=2
        IOPCOD=2
        RETURN
      ELSE IF(LCHAR('P')) THEN
C  REPLOT WITHOUT CHANGES
        IOPCOD=1
        RETURN
      ENDIF
C----------------------------------------------------
C
C       .Wait option                            !TBT 7/91
      IF(INDEX(CTRL,'W').NE.0) THEN
         IF (LCHAR('W')) THEN
         LWAIT = .TRUE.
         GO TO 10
       END IF  ! LCHAR 'W'
      ENDIF    ! INDEX 'W'
C
C  column ascii output option
C
      if(index(CTRL,'O').ne.0) then
         if(lchar('O')) then
            iopcod=13
            return
         endif
      endif
C
C  MG edit option
C
      if(index(CTRL,'E').ne.0) then
         if(lchar('+')) then
            iopcod=14
            return
         endif
      endif
C
C  AXES OPTIONS
      IF(INDEX(CTRL,'A').NE.0) THEN
C
        IRETA=-1
        ISELP=ISEL
C
        IF(LCHAR('A')) THEN
          CALL GAXSET(ISEL,0)
          IRETA=1
        ELSE IF(LCHAR('B')) THEN
          CALL GAXSET(ISEL,1)
          IRETA=0
        ENDIF
C
C  RECOMPUTE SCALE AFTER AXES CHANGE - DMC SEPT 1986
        IF(ISEL.NE.ISELP) THEN
          IF((MAX0(ISEL,ISELP).EQ.2).OR.(MIN0(ISEL,ISELP).EQ.3)) THEN
C  Y AXIS CHANGE ONLY
            ILOCSC=1
          ELSE
            ILOCSC=0
          ENDIF
        ENDIF
C
        IF(IRETA.GE.0) THEN
          IF(IRETA.EQ.1) GO TO 3000
          GO TO 10
        ENDIF
C
      ENDIF
C----------------------------------------------------
C
C  RESCALE OPTIONS
      IF(INDEX(CTRL,'S').NE.0) THEN
        ILOCSP=ILOCSC  ! SAVE PREV. SETTING INCASE OF "Q"
        IXYGET=0
        IF(LCHAR('R')) THEN
          CALL GSCRSC(ISET)
          IF(ISET.GT.0) THEN
            ILOCSC=-1
            GO TO 3000
          ENDIF
          GO TO 10
        ELSE IF(LCHAR('S')) THEN
          IXYGET=1
          ILOCSC=-1
        ELSE IF(LCHAR('X')) THEN
          IXYGET=1
          ILOCSC=1
        ELSE IF(LCHAR('Z')) THEN
 31       CALL UREAD(
     >'GRAOPT: ENTER ZOOM FACTOR (.GT. 1 EXPANDS SCALE):$')
          IF(LCHAR('X')) GO TO 10
          IF(LCHAR('Q')) GO TO 10
          ZOOM=FDCOD(IERT)
          IF(IERT.NE.0) GO TO 31
          IF(ZOOM.LE.0.0) THEN
            CALL UERMSF('% ZOOM FACTOR .GT. 0 REQUIRED, INPUT WAS',
     >                    ZOOM,0)
            GO TO 31
          ENDIF
C
          ZXA=(XMAX+XMIN)*.5
          ZDXA=(XMAX-XMIN)*.5
C
          XMIN=ZXA-ZOOM*ZDXA
          XMAX=ZXA+ZOOM*ZDXA
C  REDO Y SCALE AUTOMATICALLY
          ILOCSC=1
          GO TO 3000
        ELSE IF(LCHAR('T')) THEN
C  CHANGE *DEFAULT SCALE SETTINGS*
          CALL GSCDEF
          ILOCSC=0
          GO TO 10
        ELSE IF(LCHAR('G')) THEN
          CALL GFTICI
          GO TO 10
        ENDIF
C
C  GET NEW X/Y SCALE
C
        IF(IXYGET.NE.0) THEN
          IF(LXOUT(0)) WRITE(LUNTRM,1071) XMIN,XMAX,YMIN,YMAX  ! TBT 6/91
 1071         FORMAT(/
     >           '  XMIN,XMAX:  ',2(1PE11.4,1X)/
     >           '  YMIN,YMAX:  ',2(1PE11.4,1X)/
     >           ' ENTER "X" NOT TO CHANGE A LIMIT ON THE GRAPH'/)
C
 71        CONTINUE
           CALL GSCGT1('GRAOPT','XMIN',ZXMIN,XMIN,IERT,' ')
           IF(IERT) 80,72,71
C
 72        CONTINUE
           CALL GSCGT1('GRAOPT','XMAX',ZXMAX,XMAX,IERT,' ')
           IF(IERT.EQ.-1) GO TO 80
           IF(IERT.EQ.1) GO TO 72
C
C  IF X OPTION IS IN EFFECT EXIT HERE TO REDO Y AUTOMATICALLY
           IF(ILOCSC.EQ.1) THEN
             XMIN=ZXMIN
             XMAX=ZXMAX
             GO TO 3000
           ENDIF
C
           IF(LXOUT(0)) WRITE(LUNTRM,1073)
 1073      FORMAT(' ENTER "R" TO AUTO RESCALE Y USING NEW X SCALE')
 73        CONTINUE
           CALL GSCGT1('GRAOPT','YMIN',ZYMIN,YMIN,IERT,'R')
           IF(IERT.EQ.2) THEN
             ILOCSC=1
             XMIN=ZXMIN
             XMAX=ZXMAX
             GO TO 3000
           ENDIF
           IF(IERT) 80,74,73
C
 74        CONTINUE
           CALL GSCGT1('GRAOPT','YMAX',ZYMAX,YMAX,IERT,' ')
           IF(IERT) 80,75,74
C
C  NEW SCALE HAS BEEN ENTERED
 75        CONTINUE
           XMIN=ZXMIN
           XMAX=ZXMAX
           YMIN=ZYMIN
           YMAX=ZYMAX
           GO TO 3000
C  NEW SCALE ENTRY ABORTED - RE-INITIALIZE
 80        CONTINUE
           ILOCSC=ILOCSP
           GO TO 10
        ENDIF
      ENDIF
C----------------------------------------------------
C
C  UFILES OPTION
      IF(INDEX(CTRL,'U').NE.0) THEN
        IF(LCHAR('U')) THEN
          IOPCOD=6
          ILOCSC=2
          RETURN
        ELSE IF(LCHAR('V')) THEN
          IOPCOD=5
          ILOCSC=0
          RETURN
        else if(lchar('4')) then
          call ufmtsel(iformat)
          ILOCSC=iformat
          if(iformat.eq.0) then
             IOPCOD=5
          else
             IOPCOD=6
          endif
          RETURN
        ENDIF
      ENDIF
C----------------------------------------------------
C
C  SMOOTHING OPTION
      IF(INDEX(CTRL,'M').NE.0) THEN
        IF(LCHAR('M')) THEN
          IOPCOD=3
          GO TO 3000
        ELSE IF(LCHAR('L')) THEN
          ILOCSC=1	! RECOMPUTE Y AXIS.
          IOPCOD=4
          GO TO 3000
        ENDIF
      ENDIF
C
C----------------------------------------------------
C
C  DIFFERENTIATION OPTION
      IF(INDEX(CTRL,'D').NE.0) THEN
        ILOCSC=1	! RECOMPUTE Y AXIS.
        IF(LCHAR('D')) THEN
          IOPCOD=7
          GO TO 3000
        ELSE IF(LCHAR('E')) THEN
          IOPCOD=4
          GO TO 3000
        ENDIF
      ENDIF
C
C----------------------------------------------------
C
C  EXTERNAL OPTION CODES ROUTINE
C
      IF(INDEX(CTRL,'X').NE.0) THEN
        IOPCOD=0
        CALL XOPT(2,CTRL,IOPCOD)
        IF(IOPCOD.NE.0) RETURN
 
C	  .Illegal op code. Print message and ask again.
        IOPCOD = 1               ! TBT 5/91 - reset to stop bomb
      ENDIF
C
C  ILLEGAL INPUT OPTION CODE
C
      CALL CDCOD(ZWORD)
      CALL UERMSG(' %GRAOPT - ILLEGAL OPTION CODE ',ZWORD,1)
      GO TO 11
 
C	-----------------------------------------------------------
C	.Check "Wait" option before returning.  TBT 7/91
 
 3000   CONTINUE
      IF (LWAIT)  THEN
          IOPCOD = -IABS(IOPCOD)
      END IF  ! lwait
 
      RETURN
 
C
      END
