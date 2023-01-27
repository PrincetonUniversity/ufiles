C---------------------------------------------------------------------
C  GRF3OP   GRAPHICS OPTION MENU FOR DISPLAY OF F(X,T) DATA
C
C  DMC JUNE 1988 -- EXTRACTED FROM THE OLD GRF3 SUBROUTINE
C
      SUBROUTINE GRF3OP(ZLABEL,ZTITLE,XLAB,ZLAB,ZLAB2,YLAB,
     >    CNTRL,IMAIN,ISUB,NFCN,IFCN)
C
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
C
      CHARACTER*(*) ZLABEL,ZTITLE   ! GENERIC LABELS
C
      CHARACTER*(*) XLAB    ! X INDEP COORDINATE LABEL
      CHARACTER*(*) ZLAB    ! DEPENDENT COORDINATE LABEL
      CHARACTER*(*) ZLAB2   ! 2ND DEPENDENT COORDINATE LABEL (OPTIONAL)
      CHARACTER*(*) YLAB    ! Y INDEP COORDINATE LABEL
C
      CHARACTER*(*) CNTRL   ! EXTRA CONTROL OPTIONS
C
      INTEGER IMAIN	      ! USER MAIN OPTION CHOSEN *OUTPUT*
      INTEGER ISUB	      ! USER SUBSIDIARY OPTION CHOSEN *OUTPUT*
C
      INTEGER NFCN	      ! NO. OF DEPENDENT COORDS (1 OR 2)
      INTEGER IFCN	      ! WHICH FCN TO PLOT *OUTPUT* (1 OR 2)
C
C------------------------------------------
C
      ILXL=ILNURD(XLAB)
      ILYL=ILNURD(YLAB)
      ILZL=ILNURD(ZLAB)
      ILZL2=ILNURD(ZLAB2)
      ILTIT=ILNURD(ZTITLE)
C
      IMAIN=4
      ISUB=0
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
      CALL NEUPAG
C
 11   CONTINUE
      IF(LXOUT(0)) THEN
        WRITE(LUNTRM,1000) ZLAB(1:ILZL),ZLABEL,ZTITLE(1:ILTIT)
 1000   FORMAT(/
     >           '   FUNCTION:  ',A/
     >           '  SOURCE ID:  ',A/
     >           '      TITLE:  ',A/
     >)
        IF(NFCN.EQ.2)
     >      WRITE(LUNTRM,'('' FUNCTION 2:  '',A)') ZLAB2(1:ILZL2)
C
        IF(INDEX(CNTRL,'S').NE.0) WRITE(LUNTRM,1001)
        IF(INDEX(CNTRL,'U').NE.0) WRITE(LUNTRM,1002)
        if(index(cntrl,'X').ne.0) write(luntrm,1003)
C
 1001 FORMAT(' **> ENTER "S" TO SMOOTH PLOT DATA')
 1002 FORMAT(' **> ENTER "U" TO WRITE PLOT DATA TO UFILE')
 1003 format(' **> ENTER "X" FOR ERROR BAR PLOTTING OPTIONS')
C
        WRITE(LUNTRM,1008)
     >      XLAB(1:ILXL),YLAB(1:ILYL),XLAB(1:ILXL),YLAB(1:ILYL),
     >      YLAB(1:ILYL),XLAB(1:ILXL),XLAB(1:ILXL),YLAB(1:ILYL)
C
 1008 FORMAT(' GRAPHICS OPTIONS:'/
     >  '   (1) 3-D GRAPH OF FUNCTION VS. ',A,' AND ',A/
     >  '   (2) 2-D GRAPH VS. ',A,' AT FIXED ',A/
     >  '   (3) 2-D GRAPH VS. ',A,' AT FIXED ',A/
     >  '   (4)  (OR "Q")  QUIT GRAPHICS'/
     >  '   (5) CONTOUR PLOT; FOR "FAST" PLOT ENTER "5F"'/
     >  '   (6) CHANGE DEFAULT PLOT TYPE FOR 2D PLOTS'/
     >  '   (7) CHANGE SCALING DEFAULTS FOR 2D PLOTS'/
     >  '   (8) ',A,' OR ',A,' - SLICE MULTIPLOT'/
     >  ' >ENTER CHOICE NUMBER BETWEEN 1 AND 8<')
C
      ENDIF
C
      CALL UREAD(' GRF3SG:  ENTER OPTION #:$')
C
      ISTD=1
C  CHECK FOR SPECIAL OPTIONS
      IF(INDEX(CNTRL,'S').NE.0) THEN
C  SMOOTHING
        IF(LCHAR('S')) THEN
          IMAIN=-1
          ISTD=0
        ENDIF
      ENDIF
C
      if((istd.eq.1).and.(index(cntrl,'X').ne.0)) then
        if(lchar('X')) then
           if(NFCN.ne.2) then
      	call uermsg(
     >'?grf3op programming error, X option expects NFCN=2',' ',0)
      	goto 10
           else
      	istd=0
      	imain=-3
      	if(lxout(0)) write(luntrm,1005)
 1005 	format(/
     >' enter "0" to disable error bar plotting on f(x) plots'/
     >' enter "1" to designate fcn #1 as error bars for fcn #2'/
     >' enter "2" to designate fcn #2 as error bars for fcn #1')
      	call uread('grf3op:  enter error bar option 0 1 or 2:$')
      	if(lchar('Q')) goto 10
      	if(lchar('0')) then
      	   isub=0
      	else if(lchar('1')) then
      	   isub=1
      	else if(lchar('2')) then
      	   isub=2
      	else
      	   call uermsg('%grf3op:  invalid option code.',' ',0)
      	   goto 10
      	endif
           endif
        endif
      endif
C
      IF((ISTD.EQ.1).AND.(INDEX(CNTRL,'U').NE.0)) THEN
C  UFILES
        IF(LCHAR('U')) THEN
          IMAIN=-2
          ISTD=0
          CALL UREAD(
     >'GRF3OP:  ENTER "A" FOR ASCII OR "B" FOR BINARY UFILE OUTPUT:$$')
          IF(LCHAR('A')) THEN
            ISUB=1
          ELSE IF(LCHAR('B')) THEN
            ISUB=2
          ELSE IF(LCHAR('Q')) THEN
            GOTO 10
          ELSE
            CALL UERMSG('%GRF3OP-- ILLEGAL CHOICE',' ',1)
            GOTO 10
          ENDIF
        ENDIF
      ENDIF
C
      IF(ISTD.EQ.1) THEN
C  CHECK FOR STANDARD OPTIONS
        IF(LCHAR('Q')) THEN
          IMAIN=4
        ELSE
          IMAIN=IXUOPC('12345678$')
        ENDIF
      ENDIF
C
      IF(IMAIN.GE.0) CALL UCHKOP(IMAIN,0,1,100,-1,*11)
C
      IF(IMAIN.EQ.5) THEN
        IF(LCHAR('F')) ISUB=1
      ENDIF
C
      IFCN=1
      IF(NFCN.EQ.2) THEN
        IF((IMAIN.EQ.1).OR.(IMAIN.EQ.5).OR.(IMAIN.EQ.8)) THEN
 55       CONTINUE
          IF(LXOUT(0)) WRITE(LUNTRM,5505) ZLAB(1:ILZL),ZLAB2(1:ILZL2)
 5505 FORMAT(/'  >> CHOICE OF FUNCTIONS:'/
     >   '  (1)  ',A/
     >   '  (2)  ',A/)
          CALL UREAD(' SUB:GRF3X2 >> ENTER "1" OR "2"$')
          IF(LCHAR('Q')) GOTO 10
          IFCN=IXUOPC('12$')
          IF(IFCN.EQ.0) THEN
            CALL UCLEAR
            GO TO 55
          ENDIF
        ENDIF
      ENDIF
C
      RETURN
      END
