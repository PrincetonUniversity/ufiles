C******************** START FILE GRFMM1.FOR ; GROUP GRFMM1 *************
C----------------------GRFMM1 SUBMODULE-----------------------
C  D. MC CUNE 15 SEP 1984 - BASED ON GRFMF1
C  SET SCALE AND
C  PLOT MULTIPLOT VIA CALL TO MULFDR AND ALLOW REPEAT OF PLOT
C  ON DIFFERENT SCALE
C
C  ** PLOTTING MOMENTS FLUX SURFACE CONTOURS** THE SCALE IS MADE
C  "SQUARE" SO THAT (R,Y) SPACE OF THE POLOIDAL PLANE IS NOT
C  DISTORTED
C    RESCALE OPTION ALLOWS USER TO SPECIFY (R,Y) OF LOWER LEFT HAND
C  CORNER OF SQUARE AND LENGTH OF EDGE
C
C  ** MOD DMC 6 JUL 1992 **
C  SQUARE SCALE PLOTTING CAN BE DISABLED BY KLUGE:  SET THE FIRST
C  ELEMENT OF THE INPUT ARRAY ISELF NEGATIVE ***; THE ABSOLUTE
C  VALUE IS USED IN THE CODE LOGIC TO ADDRESS THE CURVE TO BE
C  PLOTTED, AS BEFORE.
C
C  PLOT 1 GRAPH IMMEDIATELY ON CALL WITH ESTABLISHED DEFAULTS
C  THEN GIVE OPPORTUNITY TO USER TO CHANGE SCALING/AXIS DEFAULTS
C  OR JUST ON THE FUNCTION BEING PLOTTED WITHOUT AFFECTING DEFAULTS
C  ON FUTURE CALLS.
C  USE UREAD I/O SUBROUTINE
C
C  AXES SELECTION (LOG/LINEAR FOR EITHER/BOTH/NEITHER AXES)
C  SOFTWARE IN "GAXES.FOR" IS USED
C
C  FEB 1986 - DMC
C   REWRITTEN TO USE STANDARDIZED SCALE SETTING AND POST-PLOT RESCALE
C  MENU OPTIONS
C
      SUBROUTINE GRFMM1(R,Y,ND1,NC,NNC,Z,NZ,ISELF,NSEL,
     >     RUN,YUN,MNLAB,KYLAB,LBSEL)
C
C  ARGUMENTS -- SAME AS SUBROUTINE MULCDR (MULCDR.FOR) EXCEPT
C   ARGUMENT "IAXSEL" IS DETERMINED HERE
C
      use plot1mmx
C
      implicit NONE
C
      integer :: nd1,nz,nsel,nnc
      REAL R(ND1,NZ),Y(ND1,NZ),Z(NZ)
      INTEGER ISELF(NSEL)
      INTEGER NC(NNC)       ! NO. OF PTS / CONTOUR
      CHARACTER*(*) MNLAB(3)
      CHARACTER*(*) KYLAB(5)
      CHARACTER*(*) LBSEL(NSEL),RUN,YUN
C
C---------------------------------------------
      logical :: lbl_adjust
      character*18 kylab_18(5)
      character*11 lbsel_11(nsel)
C
C dmc curve selection option Jan 2009
C
      integer :: insel2,insel2_max
      character*11, dimension(:), allocatable :: lbsel2
      integer, dimension(:), allocatable :: iself2
      real, dimension(:), allocatable :: tshift2
C
C---------------------------------------------
C
      integer :: ilocsc,isel,i,ind,itot,icod
      real :: zdx,zdy,zd2
      integer :: luntrm,lnout
C---------------------------------------------
C
      INTEGER IPTR(nsel),LPTR(nsel),JSELF(nsel)
      real :: tshift(nsel)  ! for compatibility w/grfmf1 called routines
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      LOGICAL ISQUARE
C
      integer icounter
      integer, save :: iopcod=0
C
C---------------------------------------------
C  check for adjustment (shortening) of function labels
C
      call grfchk_adjust_lbl(kylab,nsel,lbsel,
     >     lbl_adjust,kylab_18,lbsel_11)
C
      icounter=0
C
      tshift = 0.0
C
      IF(ISELF(1).GT.0) THEN
        ISQUARE=.TRUE.
      ELSE
        ISQUARE=.FALSE.
      ENDIF
C
C  LOCAL DEFAULT OVERRIDE SWITCHES CLEARED:
      ILOCSC=0
C
C  GET DEFAULT AXIS SELECTION
      CALL GAXGDF(ISEL)
C
C  SET UP INDICES FOR SCALE CALCULATION
C
      DO 5 I=1,NSEL
        JSELF(I)=IABS(ISELF(I))
        IND=JSELF(I)
        IPTR(I)=1+(IND-1)*ND1
        LPTR(I)=NC(MIN0(I,NNC))
 5    CONTINUE
C
C  enable contents editing
C
      call trgraf_slist_num(nsel,insel2)
      insel2_max=insel2
      allocate(iself2(insel2),lbsel2(insel2),tshift2(insel2))
C
      call trgraf_slist_init(nsel,jself,lbsel_11,tshift,
     >     insel2,iself2,lbsel2,tshift2)
C
C-------------------------------------------------------------
C  AUTOMATIC GENERATION OF PLOT
C
 100  CONTINUE
      icounter=icounter+1
C
C  SET AXES
C
      CALL GAXCUR(ISEL)
C
C  SET SCALE
C
      ITOT=ND1*NZ
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X Y PLOT SCALE (USE GSCALE DEFAULTS)
        CALL GSCSMG(R,Y,ITOT,ITOT,IPTR,1,IPTR,1,LPTR,NSEL)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCSM0(R,Y,ITOT,ITOT,IPTR,1,IPTR,1,
     >                  LPTR,NSEL,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
C  MAKE IT A "SQUARE" SCALE
C
      IF(ISQUARE) THEN
        ZDX=XMAX-XMIN
        ZDY=YMAX-YMIN
        ZD2=0.5*ABS(ZDX-ZDY)
        IF(ZDX.GT.ZDY) THEN
          YMIN=YMIN-ZD2
          YMAX=YMAX+ZD2
        ELSE IF(ZDY.GT.ZDX) THEN
          XMIN=XMIN-ZD2
          XMAX=XMAX+ZD2
        ENDIF
      ENDIF
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD)
C
      CALL GSCREC  ! RECORD SCALE OF UPCOMING PLOT IN GSCALE COMMON
C
      CALL MULCDR(R,Y,ND1,NC,NNC,Z,NZ,iself2,INSEL2,ICOD,
     >     RUN,YUN,MNLAB,KYLAB_18,LBSEL2)
C
      GO TO 200
C
C
C  POST PLOT OPTIONS
C
 200  CONTINUE
C
C  GENERATE MENU AND FETCH USER REPLY
C
      IF(ISQUARE) THEN
        CALL GRAOPT('SQAE',ISEL,ILOCSC,IOPCOD,GRADUM)
      ELSE
        CALL GRAOPT('SAE',ISEL,ILOCSC,IOPCOD,GRADUM)
      ENDIF
C
C  ACT ON USER REPLY - REPLOT?
C
      IF(IOPCOD.EQ.1) THEN
         GO TO 100
      ELSE IF(IOPCOD.EQ.2) THEN
         deallocate(iself2,lbsel2,tshift2)
         RETURN
      else if(iopcod.eq.14) then
         call trgraf_slist_edit(nsel,jself,lbsel_11,tshift,
     >        insel2_max,iself2,lbsel2,tshift2,insel2)
         go to 100
      ENDIF
C
      CALL UERMSI(' ?GRFMM1 *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      	END
C******************** END FILE GRFMM1.FOR ; GROUP GRFMM1 ***************
