C******************** START FILE GRFMF1.FOR ; GROUP GRFMF1 *************
C----------------------GRFMF1 SUBMODULE-----------------------
C  D. MC CUNE 15 MAY 1982
C  SET SCALE AND
C  PLOT MULTIPLOT VIA CALL TO MULFDR AND ALLOW REPEAT OF PLOT
C  ON DIFFERENT SCALE
C
C  DMC 12/87 -- LABEL ARGUMENTS DECLARATIONS GENERALIZED --
C
C  PLOT 1 GRAPH IMMEDIATELY ON CALL WITH ESTABLISHED DEFAULTS
C  THEN GIVE OPPORTUNITY TO USER TO CHANGE SCALING/AXIS DEFAULTS
C  OR JUST ON THE FUNCTION BEING PLOTTED WITHOUT AFFECTING DEFAULTS
C  ON FUTURE CALLS.
C  USE UREAD I/O SUBROUTINE
C
C  AUTO/NON-AUTO SCALING/USER-INTERFACE ROUTINES IN "GSCALE.FOR"
C  ARE USED
C  AXES SELECTION (LOG/LINEAR FOR EITHER/BOTH/NEITHER AXES)
C  SOFTWARE IN "GAXES.FOR" IS USED
C
C  MOD DMC FEB 1986 - PLOT SCALE SETTING AND POST-PLOT RESCALE
C  OPTIONS MENU STANDARDIZED VIA NEW GFLIB SUBROUTINE CALLS
C
      SUBROUTINE GRFMF1(F,NF1,X1,NX1,X2,NX2,ZW,
     >     ISELX,ISELF,NSEL,FUNS,XUNS,
     >     MNLAB,KYLAB,LBSEL)
C
C  ARGUMENTS -- SAME AS SUBROUTINE MULFDR (MULFDR.FOR) EXCEPT
C   ARGUMENT "ISEL" IS DETERMINED HERE
C
      use plot1mmx
C
      implicit NONE
C
      integer :: nf1,nx1,nx2,nsel,iselx
C
      REAL F(NF1,NX2),X1(NX1),X2(NX2),ZW(NX2)
      INTEGER ISELF(NSEL)
C
      CHARACTER*(*) MNLAB(3)
      CHARACTER*(*) KYLAB(5)
      CHARACTER*(*) LBSEL(NSEL)
      CHARACTER*(*) XUNS
      CHARACTER*(*) FUNS
C
C-------------------------------------------
      !real :: zx,zy
      integer :: i,isel,ilocsc,incx,itot,inx,incf,incst
      integer :: icod,iymmx
      integer :: lnout,luntrm
C
      character*11 lbsel_11(nsel)
      character*18 kylab_18(5)
C
      logical :: lbl_adjust
C
      real tshift(nsel)
C
C END DMC 12/87
C
      INTEGER IPTRX(nsel),IPTRY(nsel),LPTR(nsel)
      integer, save :: iopcod=0
C
      !DIMENSION ZX(2),ZY(2)
C
      EXTERNAL GRADUM  ! FOR GRAOPT
C
      integer icounter
C
      !EQUIVALENCE(ZX,XMIN)
      !EQUIVALENCE(ZY,YMIN)
C
C-----------------------------------------------------------------------
C dmc curve selection option Jan 2009
C
      integer :: insel2,insel2_max
      character*11, dimension(:), allocatable :: lbsel2
      integer, dimension(:), allocatable :: iself2
      real, dimension(:), allocatable :: tshift2

C-----------------------------------------------------------------------
C  check for adjustment of key labels...
C
      call grfchk_adjust_lbl(kylab,nsel,lbsel,
     >     lbl_adjust,kylab_18,lbsel_11)
C
      icounter=0
C
      tshift = 0.0
C
C  enable contents editing
C
      call trgraf_slist_num(nsel,insel2)
      insel2_max=insel2
      allocate(iself2(insel2),lbsel2(insel2),tshift2(insel2))
C
      call trgraf_slist_init(nsel,iself,lbsel_11,tshift,
     >     insel2,iself2,lbsel2,tshift2)
C
C  LOCAL DEFAULT OVERRIDE SWITCHES CLEARED:
      ILOCSC=0
C
C  GET DEFAULT AXIS SELECTION
      CALL GAXGDF(ISEL)
C
C  NUMBER OF PTS/ ROW OR COLUMN BEING PLOTTED
      INCX=1
      ITOT=NF1*NX2
C
      IF(ISELX.EQ.1) THEN
        INX=NX1
        INCF=1
        INCST=NF1
      ELSE
        INX=NX2
        INCF=NF1
        INCST=1
      ENDIF
C
C  SET SCALE
      IYMMX=0
      DO 10 I=1,NSEL
        IPTRX(I)=1
        IPTRY(I)=1+(ISELF(I)-1)*INCST
        LPTR(I)=INX
 10   CONTINUE
C
C-------------------------------------------------------------
C  AUTOMATIC GENERATION OF PLOT
C
 100  CONTINUE
      icounter=icounter+1
C
C  SET AXES FOR UPCOMING PLOT
C
      CALL GAXCUR(ISEL)
C
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X Y PLOT SCALE (USE GSCALE DEFAULTS)
        IF(ISELX.EQ.1) THEN
          CALL GSCSMG(X1,F,NX1,ITOT,IPTRX,1,IPTRY,INCF,LPTR,NSEL)
        ELSE
          CALL GSCSMG(X2,F,NX2,ITOT,IPTRX,1,IPTRY,INCF,LPTR,NSEL)
        ENDIF
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        IF(ISELX.EQ.1) THEN
          CALL GSCSM0(X1,F,NX1,ITOT,IPTRX,1,IPTRY,INCF,
     >                  LPTR,NSEL,XMIN,XMAX,1)
        ELSE
          CALL GSCSM0(X2,F,NX2,ITOT,IPTRX,1,IPTRY,INCF,
     >                  LPTR,NSEL,XMIN,XMAX,1)
        ENDIF
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD)
C
      CALL GSCREC  ! RECORD SCALE OF UPCOMING PLOT IN GSCALE COMMON
C
      CALL MULFDR(F,NF1,X1,NX1,X2,NX2,ZW,
     >     ISELX,ISELF2,INSEL2,ICOD,FUNS,XUNS,
     >     MNLAB,KYLAB_18,LBSEL2,tshift2)
C
      GO TO 200
C
C  POST PLOT OPTIONS
C
 200  CONTINUE
C
C  GENERATE MENU AND FETCH USER REPLY
C
      CALL GRAOPT('SAE',ISEL,ILOCSC,IOPCOD,GRADUM)
C
C  ACT ON USER REPLY - REPLOT?
C
      IF(IOPCOD.EQ.1) THEN
         GO TO 100
      ELSE IF(IOPCOD.EQ.2) THEN
         deallocate(iself2,lbsel2,tshift2)
         RETURN
      else if(iopcod.eq.14) then
         call trgraf_slist_edit(nsel,iself,lbsel_11,tshift,
     >        insel2_max,iself2,lbsel2,tshift2,insel2)
         go to 100
      ENDIF
C
      CALL UERMSI(' ?GRFMF1 *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      END
C******************** END FILE GRFMF1.FOR ; GROUP GRFMF1 ***************

      subroutine grfchk_adjust_lbl(kylab,nsel,lbsel,
     >     lbl_adjust,kylab_18,lbsel_11)

      implicit NONE

      ! routine to provide adjusted (shortened) labels, when
      ! a common prefix allows it

C input:
      character*(*) :: kylab(5)
      integer :: nsel
      character*(*) :: lbsel(nsel)

C output:
      logical :: lbl_adjust
      character*18 kylab_18(5)
      character*11 lbsel_11(nsel)

C-----------------------------------------------

      integer :: ilen_max_kylab
      integer :: ilen_max_lbsel
      integer :: ilen2,ishare
      integer :: ii,i,idiff

C-----------------------------------------------

      ilen_max_kylab=0
      ilen_max_lbsel=0
      do ii=1,5
         ilen_max_kylab=max(ilen_max_kylab,len(trim(kylab(ii))))
      enddo
      do ii=1,nsel
         ilen_max_lbsel=max(ilen_max_lbsel,len(trim(lbsel(ii))))
      enddo
      ilen2=len(trim(kylab(2)))
      ilen2=max(1,ilen2)

C  adjust function labels for fit (a) if necessary; (b) if key labels fit;
C  (c) if the number of curves is .gt.1 (d) if the labels have a common
C  prefix.

      kylab_18=' '
      lbsel_11=' '
      lbl_adjust=.TRUE.

      if(ilen_max_lbsel.le.len(lbsel_11(1))) lbl_adjust=.FALSE.
      if(ilen_max_kylab.gt.len(kylab_18(1))) lbl_adjust=.FALSE.
      if(nsel.eq.1) lbl_adjust=.FALSE.
C
      if(lbl_adjust) then
C
C  see if there is a common prefix in LBSEL
C
         i=0
         do
            i=i+1
            idiff=0
            do ii=2,nsel
               if(lbsel(1)(1:i).ne.lbsel(ii)(1:i)) then
                  idiff=ii
                  exit
               endif
            enddo
            if(idiff.gt.0) then
               i=i-1
               exit
            endif
            if(i.eq.ilen_max_lbsel-3) exit
         enddo
C
         if(i.lt.2) lbl_adjust = .FALSE.   ! insufficient common prefix
         ishare=i
      endif
C
      if(lbl_adjust) then
         ! look for "_"
         do i=1,ishare,1
            if(lbsel(1)(i:i).eq.'_') then
               if(ilen_max_lbsel-i.le.11) then
                  ishare=i
                  exit
               endif
            endif
         enddo

         do i=1,5
            kylab_18(i)=kylab(i)
         enddo
         if(kylab_18(2).eq.' ') then
            if(8+ishare.le.17) then
               kylab_18(2)='prefix: '//lbsel(1)(1:ishare)
            else
               kylab_18(2)=lbsel(1)(1:min(18,ishare))
            endif
         else if(ilen2+1+ishare.le.18) then
            kylab_18(2)=trim(kylab_18(2))//' '//lbsel(1)(1:ishare)
         endif

         do i=1,nsel
            lbsel_11(i)=trim(lbsel(i)(ishare+1:))
         enddo
C
      else
         lbsel_11(1:nsel) = lbsel(1:nsel)
         kylab_18(1:5) = kylab(1:5)
      endif
C
      return
      end
