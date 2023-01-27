C-------------------------------------------------------------------
C  GRF3F2  CONTROL ROUTINE, VARIOUS PLOTS OF F(X,T)
c
c  mod dmc 8 July 1997 -- reuse of "IPROC" --
c  specify IPROC=-1 to identify Z1 as "error bars" for Z2,
c  specify IPROC=-2 to identify Z2 as "error bars" for Z1,
c    otherwise presume neither Z1 nor Z2 are "error bars" or
c      measure of std deviation...
C
C  BASED ON OLD GRF3, WITH STREAMLINED INTERFACE, CHARACTER*(*)
C  LABEL PASSING MECHANISM, AND GRF3OP MENU SUBROUTINE
C
      SUBROUTINE GRF3F2(X,Z,Z2,Y,WK,NX,NY,NZ1,
     >                    XLAB,ZLAB,ZLAB2,YLAB,
     >                    XUN,ZUN,YUN,
     >                    ZTITLE,ZLABEL,IPROC)
C
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
C
      REAL X(NX)	! 1ST INDEP. COORD OF FCN TO BE PLOTTED
      REAL Z(NZ1,NY)	! FCN TO BE PLOTTED
      REAL Z2(NZ1,NY)	! 2ND FCN TO BE PLOTTED
      REAL Y(NY)	! 2ND INDEP. COORD OF FCN TO BE PLOTTED
      REAL WK(NY,2)	! ** WORKSPACE ** MUST BE PROVIDED
C
C  X AND Y MUST BE IN ASCENDING ORDER.
C  CAREFULLY NOTE DIMENSIONING OF Z
C
C  WK WILL BE OVERWRITTEN ON CALL TO THIS ROUTINE
C
C  RECOMMENDED NOT LONGER THAN CA. 30 CHARACTERS...
      CHARACTER*(*) XLAB  ! LABEL FOR 1ST INDEP. COORDINATE
      CHARACTER*(*) ZLAB  ! LABEL FOR FCN BEING PLOTTED
      CHARACTER*(*) ZLAB2 ! LABEL FOR 2ND FCN BEING PLOTTED
      CHARACTER*(*) YLAB  ! LABEL FOR 2ND INDEP. COORDINATE
C
C  RECOMMENDED NOT LONGER THAN CA. 20 CHARACTERS...
      CHARACTER*(*) XUN   ! UNITS LABEL FOR 1ST INDEP COORDINATE
      CHARACTER*(*) ZUN   ! UNITS LABEL FOR FCN BEING PLOTTED
      CHARACTER*(*) YUN   ! UNITS LABEL FOR 2ND INDEP. COORDINATE
C
C  RECOMMENDED NOT LONGER THAN CA. 45 CHARACTERS...
      CHARACTER*(*) ZTITLE	! TITLE LABEL FOR PLOTS
C
C  RECOMMENDED NOT LONGER THAN CA. 15 CHARACTERS
      CHARACTER*(*) ZLABEL	! GENERIC LABEL FOR PLOTS
C
C  PROCESSING FLAG (FOR LABELING ONLY)
      INTEGER IPROC	    ! 0=NOTHING, 1=AVG'D, 2=SMOOTHED, ETC...
C
C---------------------------------
C
C  THIS ROUTINE PRESENTS AN INTERACTIVE (UREAD) MENU VIA SUBROUTINE
C  GRF3OP, ALLOWING THE USER TO SELECT
C    1)  3D ISOMETRIC PLOTS
C    2)  2D SIMPLE SLICE PLOTS
C    3)  CONTOUR PLOTS
C    4)  2D SLICE MULTIPLOTS
C  OF THE PASSED LABELED DATA.
C
C  NOTE THE WORKSPACE ARRAY IS NEEDED TO BUILD SOME SLICE PLOTS AND
C  WILL BE OVERWRITTEN ON CALL TO THIS ROUTINE
C
C  ALL OTHER ARGUMENTS TO THIS ROUTINE ARE INPUT ONLY
C
C  ALL LABELS ARE OF ELASTIC LENGTH; HOWEVER, FOR SOME NONBLANK LENGTH
C  THE LABELS WILL START TO NOT FIT IN THE GENERATED TEKTRONIX DISPLAYS
C  WITHOUT LINE WRAPPING
C
C---------------------------------
C  LOCAL
C
      CHARACTER*25 ZLPROC
C
      CHARACTER*132 MNLAB(3)
C
      CHARACTER*12 ZENCOD
C
C---------------------------------
C  EXECUTABLE CODE -- SET UP PROCESSING LABEL
C
      ZLPROC=' '
      IF(IPROC.EQ.1) THEN
        ZLPROC(2:)='(AVERAGED)'
      ELSE IF(IPROC.EQ.2) THEN
        ZLPROC(2:)='(SMOOTHED)'
      ELSE IF(IPROC.EQ.3) THEN
        ZLPROC(2:)='(AVERAGED, SMOOTHED)'
      ELSE IF(IPROC.EQ.4) THEN
        ZLPROC(2:)='(PROCESSED)'
      ENDIF
C
C  error bar control
C
      if(iproc.lt.0) then
         ieb=min(2,-iproc)
      else
         ieb=0
      endif
C
C  TTY LUN
C
      LUNTRM=LNOUT(0)
C
      IPAGE=0
C
      ifun=0
C-------------------
C
C  USER OPTION MENU
C
 10   CONTINUE
C
      ifunp=ifun
      CALL GRF3OP(ZLABEL,ZTITLE,
     >    XLAB,ZLAB,ZLAB2,YLAB,'X',IMAIN,ISUB,2,IFUN)
c
      if(imain.eq.-3) then
         ieb=isub			! error bar mod
         ifun=ifunp
         goto 10
      endif
C
C  BRANCH
C
      if((imain.ne.1).and.(imain.ne.5).and.(imain.ne.8)) ifun=ifunp
      GO TO (100,200,300,400,500,600,700,800) IMAIN
C
C-------------------
C
 100  CONTINUE
C
C  3D ISOMETRIC PLOT
C
      IF(IFUN.EQ.1) THEN
C
         CALL TGRF3D(X,Z,Y,NX,NY,NZ1,
     >	    XLAB,YLAB,ZLAB,
     >	    XUN,YUN,ZUN,
     >	    ZTITLE,IPROC,
     >	    ZLABEL,0,' ',
     >	    IPAGE,' ',' ',0.0,IRET)
      ELSE
C
         CALL TGRF3D(X,Z2,Y,NX,NY,NZ1,
     >	    XLAB,YLAB,ZLAB2,
     >	    XUN,YUN,ZUN,
     >	    ZTITLE,IPROC,
     >	    ZLABEL,0,' ',
     >	    IPAGE,' ',' ',0.0,IRET)
      ENDIF
C
      IF(IRET.EQ.1) GO TO 400
C
      GO TO 10
C
C-------------------
C
C  2D GRAPH OF VARIABLE VS. 1ST INDEPENDANT VARIABLE, 2ND IN-
C  DEPENDANT VARIABLE HELD CONSTANT TO USER SPECIFIED
C  VALUE.
C
 200  CONTINUE
C
C  GET USER SPECIFIED Y VALUE
C
 1200 FORMAT('  ...PLOT VS ',A/'  AT FIXED ',A,', ',A)
C
      IF(LXOUT(0)) WRITE(LUNTRM,1200) XLAB,YLAB,YUN
      CALL UREAD(' GRF3SG:  ENTER FIXED PARAMETER:$$')
      IF(LCHAR('Q')) GOTO 10
C
      YFIX=FDCOD(IERT)
      IF(IERT.NE.0) GO TO 200
C
      DYMIN=ABS(YFIX-Y(1))
      IMIN=1
C
      DO 210 I=1,NY
        DY=ABS(YFIX-Y(I))
        IF(DY.GT.DYMIN) GO TO 210
        IMIN=I
        DYMIN=DY
 210  CONTINUE
C
C  SPECIFY FIXED PT DATA IN PLOT LABEL
C
      WRITE(ZENCOD,'(1PE12.5)') Y(IMIN)
C
C  MAKE GRAPH  (GRFAH0 WILL SET SCALE, ETC.)
C
      if(ieb.eq.0) then
c  2 fcn plot
         CALL GRAFX2(X,Z(1,IMIN),Z2(1,IMIN),NX,XUN,ZUN,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//YLAB//' = '//ZENCOD//' '//YUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//XLAB)
c
      else if(ieb.eq.1) then
c  f1 error bars on f2 plot
         call grafxe(x,z2(1,imin),nx,x,z2(1,imin),nx,0,wk,1,z(1,imin),
     1     xun,zun,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//YLAB//' = '//ZENCOD//' '//YUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//XLAB)
c
      else if(ieb.eq.2) then
c  f2 error bars on f1 plot
         call grafxe(x,z(1,imin),nx,x,z(1,imin),nx,0,wk,1,z2(1,imin),
     1     xun,zun,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//YLAB//' = '//ZENCOD//' '//YUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//XLAB)
c
      endif
C
      GO TO 10
C
C------------------
C
C  2D GRAPH OF DEPENDANT VARIABLE VS. 2ND INDEPENDANT VARIABLE
C  WITH USER SPECIFYING FIXED VALUE FOR 1ST INDEPENDANT VARIABLE.
C
 300  CONTINUE
C
C   GET USER SPECIFIED X VALUE
C
      IF(LXOUT(0)) WRITE(LUNTRM,1200) YLAB,XLAB,XUN
      CALL UREAD(' GRF3SG:  ENTER FIXED PARAMETER:$$')
      IF(LCHAR('Q')) GOTO 10
C
      XFIX=FDCOD(IERT)
      IF(IERT.NE.0) GO TO 300
C
      DXMIN=ABS(X(1)-XFIX)
      IMIN=1
C
      DO 310 I=1,NX
        DX=ABS(X(I)-XFIX)
        IF(DX.GT.DXMIN) GO TO 310
        DXMIN=DX
        IMIN=I
 310  CONTINUE
C
C  WILL PLOT COLUMN OF Z POINTS CLOSEST TO SPECIFIED X
C  VALUE.  USE WK ARRAY: GRFADH REQUIRES POINTS BEING GRAPHED
C  TO BE IN CONTIGUOUS CORE SPACE
C
      DO 315 I=1,NY
        WK(I,1)=Z(IMIN,I)
        WK(I,2)=Z2(IMIN,I)
 315  CONTINUE
C
C  SPECIFY FIXED PT DATA IN PLOT LABEL
C
      WRITE(ZENCOD,'(1PE12.5)') X(IMIN)
C
C  MAKE GRAPH  (GRFAH0 WILL SET SCALE, ETC.)
C
      if(ieb.eq.0) then
c  2 fcn plot
         CALL GRAFX2(Y,WK(1,1),WK(1,2),NY,YUN,ZUN,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//XLAB//' = '//ZENCOD//' '//XUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//YLAB)
c
      else if(ieb.eq.1) then
c  f1 error bars on f2 plot
         call grafxe(y,wk(1,2),ny,y,wk(1,2),ny,0,wk,1,wk(1,1),yun,zun,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//XLAB//' = '//ZENCOD//' '//XUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//YLAB)
c
      else if(ieb.eq.2) then
c  f2 error bars on f1 plot
         call grafxe(y,wk(1,1),ny,y,wk(1,1),ny,0,wk,1,wk(1,2),yun,zun,
     1     ZLABEL//' '//ZTITLE,
     2     'FIXED: '//XLAB//' = '//ZENCOD//' '//XUN,
     3     ZLAB//' & '//ZLAB2//' '//ZLPROC//' VS '//YLAB)
c
      endif
C
C
      GO TO 10
C
C------------------------------
C  ** EXIT
 400  CONTINUE
C
      RETURN
C
C------------------------------
C  ** CONTOUR PLOTTING
C
 500  CONTINUE
C
      IF(IFUN.EQ.1) THEN
        IF(ISUB.EQ.0) THEN
          CALL CONCTR(X,Z,Y,NX,NZ1,NY,
     >		   XLAB,ZLAB,YLAB,XUN,ZUN,YUN,ZLABEL,
     >		   0,IPROC,IPAGE)
        ELSE
          CALL CONCTS(X,Z,Y,NX,NZ1,NY,
     >		   XLAB,ZLAB,YLAB,XUN,ZUN,YUN,ZLABEL,
     >		   0,IPROC,IPAGE,1,1,1)
        ENDIF
      ELSE
        IF(ISUB.EQ.0) THEN
          CALL CONCTR(X,Z2,Y,NX,NZ1,NY,
     >		   XLAB,ZLAB2,YLAB,XUN,ZUN,YUN,ZLABEL,
     >		   0,IPROC,IPAGE)
        ELSE
          CALL CONCTS(X,Z2,Y,NX,NZ1,NY,
     >		   XLAB,ZLAB2,YLAB,XUN,ZUN,YUN,ZLABEL,
     >		   0,IPROC,IPAGE,1,1,1)
        ENDIF
      ENDIF
C
      GO TO 10
C
C------------------------------
C  CHANGE DEFAULT PLOT TYPE
C
 600  CONTINUE
      CALL GAXSET(ISEL,1)
      GO TO 10
C
C------------------------------
C  CHANGE SCALING DEFAULTS
C
 700  CALL GSCDEF
      GO TO 10
C
C------------------------------
C
C  SLICE MULTIPLOTS--
C
 800  CONTINUE
C
      MNLAB(1)=ZLABEL//' '//ZLPROC
C
      MNLAB(2)=ZTITLE
C
      MNLAB(3)=' '
C
C  USER CHOOSE X AXIS, SET OF FIXED Y, AND MAKE GRAPH
      IF(IFUN.EQ.1) THEN
        CALL G3MSEL(Z,NZ1,X,NX,Y,NY,WK,
     >	     XLAB,XUN,YLAB,YUN,ZLAB,ZUN,MNLAB)
      ELSE
        CALL G3MSEL(Z2,NZ1,X,NX,Y,NY,WK,
     >	     XLAB,XUN,YLAB,YUN,ZLAB2,ZUN,MNLAB)
      ENDIF
C
      ifun=ifunp
      GO TO 10
C
      	END
