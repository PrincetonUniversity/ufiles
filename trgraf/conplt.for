C******************** START FILE CONPLT.FOR ; GROUP CONCTR *************
C==========================================================
C  CONPLT
C    D. MC CUNE 21 MAY 1980
C  DRAW CONTOUR PLOT AND ALLOW USER TO RESCALE
C   USES RCONTR [NBEAM.AGII3] TO GENERATE CONTOURS
C
C  PROVIDES LABELING OF CONTOURS, AUTOSELECTION OF
C  CONTOURS, AND DOMAIN SELECTION
C
C  ARGUMENTS TO "XMIN" SAME AS SUBROUTINE CONCTR, ABOVE
C  ARGUMENS AFTER XMIN:
C   XMIN:   MIN X TO PLOT
C   YMIN:   MIN Y TO PLOT
C   XMAX:   MAX X TO PLOT
C   YMAX:   MAX Y TO PLOT
C   CMIN:   LOWER BOUND ON CONTOURS TO PLOT
C   CMAX:   UPPER BOUND ON CONTOURS TO PLOT
C   NMAX:   MAX # OF CONTOURS TO PLOT
C   ILOGC:  =1==> LINEAR SPACING OF CONTOURS
C   ILOGC:  =2==> PSEUDOLOGARITHMIC SPACING OF CONTOURS
C
C   ZCNTR   ARRAY OF CONTOUR VALUES
C   ACNTR   ARRAY OF 1-CHARACTER SYMBOLS FOR CONTOURS
C
C  ALL THESE VARIABLES EXCEPT ACNTR MAY BE CHANGED HERE.
C
C   NLZMMX-- SET CMIN,CMAX TO MIN , MAX OF FUNCTION ARRAY
C   WITHIN SUBDOMAIN BOUNDED BY XMIN,XMAX,YMIN,YMAX
C
C-------------------------------
C  DMC SEPT 1988 -- TRYING TO CODE AROUND AN RCONTR PROBLEM
C   OF NOT PLOTTING CONTOURS W/ TEST VALUES OF MOD LESS THAT
C   1.E-15 OR SO ...
C-------------------------------
C
      SUBROUTINE CONPLT(X,Z,Y,NX,NZ1,NY,XLAB,ZLAB,YLAB,
     >        XUN,ZUN,YUN,RLAB,IINT,ISMOO,ITYPX,ITYPY,
     >        XMIN,XMAX,CMIN,CMAX,YMIN,YMAX,NMAX,ILOGC,NLZMMX,
     >        ZCNTR,ACNTR,NPAGE)
C
      use plot1mmxb
C
C  DECLARATIONS:
C
C  PASSED ARGS
C
      DIMENSION X(NX),Z(NZ1,NY),Y(NY)
      CHARACTER*(*) XLAB,ZLAB,YLAB,RLAB,XUN,YUN,ZUN
      LOGICAL NLZMMX,LXOUT
C
c------------------  ! local copies...
      real :: zcntr_use(nmax)
      real*8 :: zcntr_r8(nmax)
      real, dimension(:,:), allocatable :: z_use
      real*8, dimension(:,:), allocatable :: z_r8
      real*8, dimension(:), allocatable :: x_r8,y_r8
c------------------
C
      REAL ZCNTR(NMAX)
      CHARACTER*1 ACNTR(NMAX)
C
      integer sts
C
C------------------------------------------
C  START OF EXECUTABLE CODE --
C
      if (allocated(z_use)) deallocate(z_use)
      allocate(z_use(nz1,ny),stat=sts)
      if (allocated(z_r8)) deallocate(z_r8)
      allocate(z_r8(nx,ny),stat=sts)
      if (allocated(x_r8)) deallocate(x_r8)
      allocate(x_r8(nx),stat=sts)
      if (allocated(y_r8)) deallocate(y_r8)
      allocate(y_r8(ny),stat=sts)

C
      if(nz1.gt.nx) then
         z_use(nx+1:nz1,1:ny)=0.0
      endif
      zcntr_use=0.0
      zcntr_r8=0.0d0
      x_r8=x
      y_r8=y
C
C  DMC SEPT 1988
      IMOD=0
      ZACMIN=AMIN1(ABS(CMIN),ABS(CMAX))
      ZACMAX=AMAX1(ABS(CMIN),ABS(CMAX))
      IF((ZACMIN.GT.0.0).AND.(ZACMIN.LT.1.E-15)) THEN
        ZFAC=1.E-15/ZACMIN
        IF(ZACMAX.LT.1.E15/ZFAC) IMOD=1
      ENDIF
C  END DMC SEPT 1988
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
C
C  SELECT SET OF CONTOURS
      IF((CMIN.EQ.0.0).AND.(CMAX.EQ.0.0)) THEN
        NRET=NMAX
      ELSE
C  AUTOSELECT CONTOUR TEST VALUES
        CALL CONSEL(LUNTRM,ILOGC,CMIN,CMAX,NMAX,ZCNTR,NRET)
      ENDIF
C
C  FIND INDEX LIMITS RELATIVE TO XMAX,XMIN, ETC. IN DOMAIN
C
      XBND1=XMIN
      XBND2=XMAX
      YBND1=YMIN
      YBND2=YMAX
C
      CALL CONDOM(X,NX,Y,NY,IMIN,IMAX,JMIN,JMAX)
C
      IMINM=MAX(1,IMIN-1)
      IMAXM=MIN(NX,IMAX+1)
      JMINM=MAX(1,JMIN-1)
      JMAXM=MIN(NY,JMAX+1)
C
C  PERFORM CHECK IF LOG AXES
      IF(ITYPX.NE.2) GO TO 4
      IF(XMAX.GT.0) GO TO 3
      ITYPX=1
      GO TO 4
 3    CONTINUE
      IF(XMIN.LE.0) XMIN=1.E-5*XMAX
 4    CONTINUE
C
      IF(ITYPY.NE.2) GO TO 8
      IF(YMAX.GT.0) GO TO 7
      ITYPY=1
      GO TO 8
 7    CONTINUE
      IF(YMIN.LE.0) YMIN=1.E-5*YMAX
 8    CONTINUE
C
C  INITIALIZE PLOT
C
      CALL UBINIT
      CALL GFTICS
      CALL SLIMX(150,750)
      CALL DLIMX(XBND1,XBND2)
      CALL DLIMY(YBND1,YBND2)
      IF(ITYPX.EQ.2) CALL XTYPE(2)
      IF(ITYPY.EQ.2) CALL YTYPE(2)
      CALL CHECK(X,Y)
      CALL GRID
      CALL LABEL(IBASEX(0))
      CALL LABEL(IBASEY(0))
      CALL GFTICX
      CALL SETWIN
C
C  DMC SEPT 1988
C
      IF(IMOD.EQ.1) THEN
        ZZMIN=1.0E37
        ZZMAX=1.0E-37
        DO 20 IC=1,NRET
          IF(ZCNTR(IC).NE.0.0) ZZMIN=AMIN1(ABS(ZCNTR(IC)),ZZMIN)
          ZZMAX=AMAX1(ABS(ZCNTR(IC)),ZZMAX)
 20     CONTINUE
C
        DO 40 IY=1,NY
          DO 30 IX=1,NX
            IF(Z(IX,IY).NE.0.0) ZZMIN=AMIN1(ABS(Z(IX,IY)),ZZMIN)
            ZZMAX=AMAX1(ABS(Z(IX,IY)),ZZMAX)
 30       CONTINUE
 40     CONTINUE
C
        ZFAC=1.E-15/ZZMIN
        IF(ZFAC.LE.1.0) THEN
          IMOD=0
        ELSE
          IF(ZZMAX.GT.1.E38/ZFAC) IMOD=0
        ENDIF
      ENDIF
C
      IF(IMOD.EQ.1) THEN
         zcntr_use(1:nret)=ZFAC*zcntr(1:nret)
         z_use(1:nx,1:ny)=ZFAC*z(1:nx,1:ny)
      else
         zcntr_use=zcntr
         z_use(1:nx,1:ny)=z(1:nx,1:ny)
      ENDIF
      zcntr_r8=zcntr
      z_r8(1:nx,1:ny)=z(1:nx,1:ny)
C
C  END DMC SEPT 1988
C
C  DRAW CONTOURS
      K2=0
      CALL RCONTR(NRET,ZCNTR_use,K2,
     >     Z_use,NZ1,X,IMINM,IMAXM,1,Y,JMINM,JMAXM,1)
C
C  LABEL THE CONTOUR PLOT
      INX=IMAX-IMIN+1
      JNY=JMAX-JMIN+1
      CALL CONLBL(X(IMIN),Z_use(IMIN,JMIN),Y(JMIN),INX,NZ1,JNY,
     >              XLAB,ZLAB,YLAB,
     >              XUN,ZUN,YUN,
     >              RLAB,IINT,ISMOO,ZCNTR_use,ACNTR,NRET,NPAGE)
C
      CALL UPAUST(2)
C
      deallocate(z_use,z_r8)
      RETURN
      	END
C******************** END FILE CONPLT.FOR ; GROUP CONCTR ***************
