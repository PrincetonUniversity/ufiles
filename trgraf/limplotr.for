C-----------------------------------------------------------------------
C LIMPLOTR -- PLOT PLASMA BDY AND LIMITERS -- FOR DEBUGGING
C
      SUBROUTINE LIMPLOTR(RUNID,ZTIME, RMCB,YMCB,MIMOM,NMOMD,
     1    NONLIN,MILIM,
     2    NLINLM,ALNLMR,ALNLMY,ALNLMT,
     3    NCIRLM,CRLMR1,CRLMY1,CRLMRD,
     4    mnlim,rpts,ypts,irflim,rpts_rf,ypts_rf,
     5    nant,nrz_ant,namax,ranta,yanta)
C
C  PASSED:
C
      CHARACTER*(*) RUNID   ! TRANSP RUNID
      REAL ZTIME            ! TIME OF PLOT
      REAL RMCB(0:MIMOM,2),YMCB(0:MIMOM,2)   ! BDY CONTOUR
      INTEGER NMOMD         ! NO. OF ACTIVE MOMENTS IN BDY CONTOUR
C
      INTEGER NONLIN        ! I/O CHANNEL
      INTEGER NLINLM,NCIRLM ! LIMITER COUNTS (LINEAR, CIRCULAR)
C  LINE LIMITERS (AS IN TRANSP COMMON TRCOM):
      REAL ALNLMR(MILIM),ALNLMY(MILIM),ALNLMT(MILIM)
C  CIRCLE LIMITERS (AS IN TRANSP COMMON TRCOM):
      REAL CRLMR1(MILIM),CRLMY1(MILIM),CRLMRD(MILIM)
c
c  contour limiter -- instead of circles & lines if MNLIM > 1:
c
      integer :: MNLIM
      real :: rpts(MNLIM),ypts(MNLIM)
C
C  RF limiter
C
      integer :: irflim
      real :: rpts_rf(irflim),ypts_rf(irflim)
C
C  RF antennas
C
      integer :: nant
      integer :: nrz_ant(nant)
      integer :: namax
      real :: ranta(namax,nant),yanta(namax,nant)
C
C  LOCAL:
C
      PARAMETER (IMAXPL=15)
      PARAMETER (INPTS=500)
      REAL ZRPLOT(INPTS,IMAXPL),ZYPLOT(INPTS,IMAXPL),ZZ(IMAXPL)
      INTEGER INC(IMAXPL),ISEL(IMAXPL),inum
C
      CHARACTER*80 MNLAB(3)
      CHARACTER*20 KYLAB(5)
      CHARACTER*12 ZXLAB(15)
C
      DATA TWOPI/6.283185/
C
C-----------------------------------------------------------------------
C
      if(irflim.gt.1) then
         incr = 1
      else
         incr = 0
      endif

      if(mnlim.le.1) then

         INTOT=NLINLM+NCIRLM+incr
         WRITE(NONLIN,1001) INTOT
 1001    FORMAT(' %LIMPLOT:  NAMELIST SPECIFIES ',I2,' LIMITERS.')
C
         IF(INTOT.GE.IMAXPL) THEN
            IMM1=IMAXPL-1
            ILASL=INTOT-IMM1+1
            WRITE(NONLIN,1002) IMM1,ILASL
 1002       FORMAT(' %LIMPLOT:  CAN ONLY PLOT ',I2,' LIMITERS.'/
     >           '  MUST RESTRICT RANGE OF LIMITERS TO PLOT;'/
     >           '  ENTER # OF 1st LIMITER TO PLOT BTW. 1 AND ',I2,':')
            IPLOT1=1
            CALL UREADI('LIMPLOT: ENTER NO. OF FIRST LIMITER TO PLOT:$',
     >           IPLOT1)
            IPLOT1=MAX(1,MIN(ILASL,IPLOT1))
            IPLOT2=IPLOT1+IMM1-1-incr
         ELSE
            IPLOT1=1
            IPLOT2=INTOT
         ENDIF
      endif
C
C  SET UP PLOTTING DATA
C
      ZRMIN=1.e20
      ZRMAX=-1.e20
      zymin=1.e20
      zymax=-1.e20
C
      DO ITH=1,INPTS
C  PLASMA BDY DATA
        ZTH=TWOPI*FLOAT(ITH-1)/FLOAT(INPTS-1)
        ZR=0.0
        ZY=0.0
        DO IMOM=0,NMOMD
           ZCS=COS(IMOM*ZTH)
           ZSN=SIN(IMOM*ZTH)
           ZR=ZR+RMCB(IMOM,1)*ZCS+RMCB(IMOM,2)*ZSN
           ZY=ZY+YMCB(IMOM,1)*ZCS+YMCB(IMOM,2)*ZSN
        ENDDO
        ZRPLOT(ITH,1)=ZR
        ZYPLOT(ITH,1)=ZY
        ZRMIN=AMIN1(ZR,ZRMIN)
        ZRMAX=AMAX1(ZR,ZRMAX)
        ZYMIN=AMIN1(ZY,ZYMIN)
        ZYMAX=AMAX1(ZY,ZYMAX)
      ENDDO
C
      INC(1)=INPTS
      ISEL(1)=1
      ZZ(1)=1.0
      ZXLAB(1)='PLASMA BDY'
C
C  RF vacuum vessel contour (if any)
C
      if(irflim.gt.1) then
         inc(2)=min(inpts,irflim)
         isel(2)=2
         zz(2)=2.0
         zxlab(2)='RF VV BDY'
         zrplot(1:inc(2),2)=rpts_rf(1:inc(2))
         zyplot(1:inc(2),2)=ypts_rf(1:inc(2))
      endif
C
      IPLOT=incr
      IZ=1+incr
C
      if(mnlim.le.1) then
         DO ILIM=1,NCIRLM
            IPLOT=IPLOT+1
            IF((IPLOT1.LE.IPLOT).AND.(IPLOT.LE.IPLOT2)) THEN
C     PLOT THIS CIRCULAR LIMITER
               IZ=IZ+1
               DO ITH=1,INPTS
                  ZTH=TWOPI*FLOAT(ITH-1)/FLOAT(INPTS-1)
                  ZRPLOT(ITH,IZ)=CRLMR1(ILIM)+CRLMRD(ILIM)*COS(ZTH)
                  ZYPLOT(ITH,IZ)=CRLMY1(ILIM)+CRLMRD(ILIM)*SIN(ZTH)
               ENDDO
               INC(IZ)=INPTS
               ISEL(IZ)=IZ
               ZZ(IZ)=IZ
               WRITE(ZXLAB(IZ),'(''CRLM#'',I2)') ILIM
            ENDIF
         ENDDO
C
         ZLEN=3.0*max((ZYMAX-ZYMIN),(ZRMAX-ZRMIN))
C
         DO ILIM=1,NLINLM
            IPLOT=IPLOT+1
            IF((IPLOT1.LE.IPLOT).AND.(IPLOT.LE.IPLOT2)) THEN
C  PLOT THIS LINE LIMITER
               IZ=IZ+1
               ZTH=TWOPI*ALNLMT(ILIM)/360.0
               ZRPLOT(1,IZ)=ALNLMR(ILIM)-ZLEN*COS(ZTH)
               ZYPLOT(1,IZ)=ALNLMY(ILIM)+ZLEN*SIN(ZTH)
               ZRPLOT(2,IZ)=ALNLMR(ILIM)
               ZYPLOT(2,IZ)=ALNLMY(ILIM)
               ZRPLOT(3,IZ)=ALNLMR(ILIM)+ZLEN*COS(ZTH)
               ZYPLOT(3,IZ)=ALNLMY(ILIM)-ZLEN*SIN(ZTH)
               INC(IZ)=3
               ISEL(IZ)=IZ
               ZZ(IZ)=IZ
               WRITE(ZXLAB(IZ),'(''LNLM#'',I2)') ILIM
            ENDIF
         ENDDO
      else
         IZ=2+incr
         inum=min(INPTS,mnlim)
         INC(IZ)=inum
         ISEL(IZ)=IZ
         ZZ(IZ)=IZ
         zxlab(iz)='LIMITER'
         ZRPLOT(:,IZ)=0
         ZYPLOT(:,IZ)=0
         ZRPLOT(1:inum,IZ)=rpts(1:inum)
         ZYPLOT(1:inum,IZ)=ypts(1:inum)
      endif
C
      do ia=1,nant
         if((nrz_ant(ia).gt.0).and.(iz.lt.15)) then
            iz=iz+1
            inum=nrz_ant(ia)
            inc(iz)=inum
            isel(iz)=iz
            zz(iz)=iz
            zxlab(iz)=' '
            write(zxlab(iz),'("RF Ant.#",i2)') ia
            ZRPLOT(:,IZ)=0
            ZYPLOT(:,IZ)=0
            ZRPLOT(1:inum,IZ)=ranta(1:inum,ia)
            ZYPLOT(1:inum,IZ)=yanta(1:inum,ia)
         endif
      enddo

      MNLAB(1)='RUN '//RUNID//' PLOT OF PLASMA BDY AND LIMITERS'
      MNLAB(2)='GENERATED BY SUBROUTINE LIMPLOTR.FOR'
      WRITE(MNLAB(3),'('' TIME OF PLOT:  '',1PE11.4,'' SECONDS.'')')
     >    ZTIME
C
      KYLAB(1)='CONTOUR#1: BDY'
      if(mnlim.le.1) then
         KYLAB(2)='CIRC. LIMITERS'
         KYLAB(3)='LINE LIMITERS'
      else
         KYLAB(2)='limiter contour'
         KYLAB(3)=' '
      endif
      KYLAB(4)='RF Vacuum Vessel'
      KYLAB(5)='RF Antennas'
C
      CALL GRFMM1(ZRPLOT,ZYPLOT,INPTS,INC,IZ,ZZ,IZ,ISEL,IZ,
     >    'CM','CM',MNLAB,KYLAB,ZXLAB)
C
      RETURN
      END
