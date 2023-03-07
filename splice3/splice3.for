C--------------------------------------------------------------------
C  SPLICE3  UFILES UTILITY - COMBINE PIECES OF 3D UFILES TO MAKE NEW 3D
C    UFILE
C
      PROGRAM SPLICE3
C
C  E.G. F1(T,X,Y),F2(T,X,Y),F3(T,X,Y) ARE COMBINED TO FORM F(T,X,Y) BY TAKING
C      THE DATA FROM F2 FOR INTERVAL TA TO TB,
C      THE DATA FROM F1 FOR INTERVAL TB TO TC,
C      THE DATA FROM F3 FOR INTERVAL TC TO TD, TA.LT.TB.LT.TC.LT.TD
C
      USE SPLICE3_BLK
C
      implicit NONE
C
      INTEGER IORDER(MAXSC)
      INTEGER IUSE(MAXFI)
C
      INTEGER IFILIN(2)
C
      LOGICAL LXSAME
C
      REAL ZWK(MAXNI)
C
      CHARACTER*20 ZLBL
      CHARACTER*10 ZUNS
C
      CHARACTER*30 ZTITL
C
      CHARACTER*100 ZFILE
C
C  IDENTITY FUNCTION
      EXTERNAL XIDENT
C
      CHARACTER*7 CMPSTA(0:4)
C
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C  COMPRESSED DATA OUTPUT DEFAULT FLAG, =1 FOR COMPRESSION ON
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C  A LARGE NUMBER...
C
C  explicit declarations dmc Aug 2004...
      integer iflagf,ix,infix1,infix2,il,intvl,iback,ilun,ians,inbdy
      real zxr,zdifr,zintvl,z1,z2,zx1,zx2,zw1,zw2,zinfin
      integer imaxg,jnbdy,icprev,ict,ici,intv,iert,ifili,ipass0,ipass
      integer iout,in,ip1,i1,i2p,i2,i1m,ixi,ixo,iao,iai,inout,ismoo
      integer icompr,itoggl,i,iluni,isetl,luntrm,lnout,imaxi,iop,ishot
      integer ier,idimi,iflagq,ixuopc,idcod,iproco,in1,in2,nshoto,ifil
      integer iintv,ilunt,nfix1,nfix2,ifix,infix,ii,ixf1,ixf2,in3
      integer nordrf,iordrf(MAXFI),iia,iib,iflags
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
      LOGICAL LYES
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C => SPLICE3              BEGIN DATA BLOCK
C => LISMAK 12-APR-88  MOVED DATA STATEMENTS
      DATA CMPSTA/'ascii','????','binary','HDF','NetCDF'/
      DATA ICOMPR/2/    ! default:  portable compressed binary
      DATA ZINFIN/1.E37/
C => END DATA BLOCK
C ====================
C
      itoggl=icompr
C
      call ufheader("splice3")
      CALL UINITG("SPLICE3")
C
      NBDY=0
C
      DO 1 I=1,MAXFI
        LUNI(I)=30+I
 1    CONTINUE
C
      LUNOUT=LUNI(MAXFI)+1
      ILUNT=LUNOUT+1  ! COMMENTS FILE LUN
C
C  INPUT LOOP
C
 5    CONTINUE
C
      fin = 0.0
      NFILEI=0
      ISETL=0
      nout=0
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
      if(NFILEI.le.1) LXSAME=.TRUE.
      IMAXI=MAXFI
      IF(LXOUT(0)) WRITE(LUNTRM,1001) NFILEI,IMAXI
 1001 FORMAT(' % CURRENTLY ',I2,' INPUT FILES HAVE BEEN READ'/
     >         ' % __MAXIMUM ',I2,' INPUT FILES ALLOWED')
      CALL UREAD(
     >'SPLICE3: READ ANOTHER 3D INPUT UFILE ? (Y/N; "Q" EXITS):$$')
      IF(LCHAR('Q')) GO TO 900
      IF(.NOT.LYES(0)) GO TO 100
C
      NFILEI=NFILEI+1
      IF(NFILEI.GT.IMAXI)
     >    WRITE(LUNTRM,'('' ?? MAXIMUM INPUT FILE COUNT EXCEEDED'')')
      WRITE(ZLBL,3001) NFILEI
 3001 FORMAT('3D INPUT FILE NO. ',I2)
      CALL UFNTER(LUNI(NFILEI),ZLBL)
C
 20   CONTINUE
      CALL NEUPAG
      CALL UFSNMD(LUNTRM,ZLBL,LUNI(NFILEI))
C
      CALL URSHOT('SPLICE3',', "Q" TO BACK UP','Q',ISHOT,IOP)
      IF(IOP.EQ.1) GO TO 90
      IF(ISHOT.EQ.0) GO TO 90
      NSHOTI(NFILEI)=ISHOT
C
      CALL UFOPRD(LUNI(NFILEI),NSHOTI(NFILEI),IER)
      IF(IER.NE.0) THEN
        CALL UCLEAR
        GO TO 90
      ENDIF
C
C  READ THE DATA
      IDIMI=MAXN/MAXNI
      CALL UF3DRD(LUNI(NFILEI),IDEV(NFILEI),SDATE(NFILEI),
     1	FIN(1,NFILEI),IDIMI,1,
     1	XIN(1,1,NFILEI),MAXNI,XIN(1,2,NFILEI),MAXNI,
     1  XIN(1,3,NFILEI),MAXNI,
     1	IPROC(NFILEI),NIN(1,NFILEI),NIN(2,NFILEI),NIN(3,NFILEI),1,  ! iflag=1
     1	FLAB(1,NFILEI),XLAB(1,1,NFILEI),XLAB(1,2,NFILEI),
     1  XLAB(1,3,NFILEI),
     1	MAXSC,NUMSC(NFILEI),SCVAL(1,NFILEI),SCLAB(1,1,NFILEI),
     1	IER)
C
C  NOTE INPUT FILE IS *NOT* CLOSED IF READ SUCCESSFULLY-- HELD OPEN FOR
C  POSSIBLE COMMENT SPOOLING LATER.
C
      IF(IER.NE.0) THEN
        CALL UCLEAR
        CALL UFCLOS(LUNI(NFILEI))
        GO TO 90
      ENDIF
C
      IF(NFILEI.GT.1) THEN
C---------------------------
C  ** NOT THE FIRST FILE READ **
C    CONSISTENCY CHECKS AGAINST THE DATA FROM THE FIRST FILE
C
        IFLAGQ=0  ! WARNING FLAG
        IFLAGF=0  ! FATAL REJECT FLAG
C
C  CHECK UNITS CONSISTENCY
        IF((XLAB(3,1,NFILEI).NE.XLAB(3,1,1)).OR.
     >       (XLAB(3,2,NFILEI).NE.XLAB(3,2,1)).OR.
     >       (XLAB(3,3,NFILEI).NE.XLAB(3,3,1)).OR.
     >       (FLAB(3,NFILEI).NE.FLAB(3,1))) THEN
          CALL UCLEAR
          WRITE(LUNTRM,1090) XLAB(3,1,1),XLAB(3,1,NFILEI),
     >               XLAB(3,2,1),XLAB(3,2,NFILEI),
     >               XLAB(3,3,1),XLAB(3,3,NFILEI),
     >               FLAB(3,1),FLAB(3,NFILEI)
 1090 FORMAT(' % INPUT DATA UNITS INCONSISTENCY:'/
     >'   1ST INPUT FILE    INPUT FILE JUST READ'/
     >'  X1:  ',A10,'        ',A10/
     >'  X2:  ',A10,'        ',A10/
     >'  X3:  ',A10,'        ',A10/
     >'   F:  ',A10,'        ',A10/)
          IFLAGQ=1
        ENDIF
C
C  CHECK CONSISTENCY OF DATA ITSELF
C   ... SPLICING AXIS
        IF(NIN(NDSPLICE,1).NE.NIN(NDSPLICE,NFILEI)) THEN
          CALL UCLEAR
          IFLAGQ=1
          WRITE(LUNTRM,1091) NDSPLICE,
     >       NIN(NDSPLICE,1),NIN(NDSPLICE,NFILEI)
 1091 FORMAT(' % INPUT DATA X',I1,' AXES:  DIFFERENT NUMBER OF PTS:'/
     >'   1ST INPUT FILE, NX=',I5,'  THIS INPUT FILE, NX=',I5)
        ELSE
          ZXR=ABS(XIN(NIN(NDSPLICE,1),NDSPLICE,1)-XIN(1,NDSPLICE,1))
          DO 26 IX=1,NIN(NDSPLICE,1)
            IF((ABS(XIN(IX,NDSPLICE,1)-XIN(IX,NDSPLICE,NFILEI))/ZXR).GT.
     >		  1.E-4) GO TO 27
 26       CONTINUE
          IX=0
 27       CONTINUE
          IF(IX.GT.0) THEN
            IFLAGQ=1
            WRITE(LUNTRM,1092) NDSPLICE,IX,
     >		  XIN(IX,NDSPLICE,1),XIN(IX,NDSPLICE,NFILEI)
 1092 FORMAT(' % INPUT DATA X',I1,' AXIS:  ',
     >I5,'''TH DATA PT DIFFERENT:'/
     >'   1ST INPUT FILE, X=',1PE14.7,' THIS INPUT FILE, X=',1PE14.7)
          ENDIF
        ENDIF
C
C   ... *FIXED* AXES
        do ii=1,2
           if(ii.eq.1) then
              ifix=nfix1
              infix=infix1
           else
              ifix=nfix2
              infix=infix2
           endif
           
           IF(INFIX.NE.NIN(IFIX,NFILEI)) THEN
              CALL UCLEAR
              IFLAGF=1
              WRITE(LUNTRM,1091) IFIX,INFIX,NIN(IFIX,NFILEI)
           ELSE
              ZXR=ABS(XIN(INFIX,IFIX,1)-XIN(1,IFIX,1))
              DO 28 IX=1,NIN(IFIX,1)
                 ZDIFR=ABS(XIN(IX,IFIX,1)-XIN(IX,IFIX,NFILEI))/ZXR
                 IF(ZDIFR.GT.1.E-4) GO TO 29
 28           CONTINUE
              IX=0
 29           CONTINUE
              IF(IX.GT.0) THEN
                 IFLAGQ=1
                 IF(ZDIFR.GT.1.E-3) IFLAGF=1
                 WRITE(LUNTRM,1092) IFIX,IX,
     >              XIN(IX,IFIX,1),XIN(IX,IFIX,NFILEI)
              ENDIF
           ENDIF
        enddo
C
        IF(IFLAGF.EQ.1) THEN
          CALL UERMSG(
     >' ? ERROR IN "FIXED AXIS" OF INPUT DATA; FILE REJECTED',' ',1)
          CALL UFCLOS(LUNI(NFILEI))
          GO TO 90
        ENDIF
C
        IF(IFLAGQ.EQ.1) THEN
          CALL UREAD('SPLICE3:  USE INPUT DATA?  (Y/N):$$')
          IF(.NOT.LYES(0)) THEN
            CALL UFCLOS(LUNI(NFILEI))
            GO TO 90
          ELSE
            LXSAME=.FALSE.
          ENDIF
        ENDIF
C
      ELSE
C-------------------
C  ** FIRST FILE READ **
C  SPECIFY SPLICE DIMENSION AND FIXED DIMENSION
C
 15     CONTINUE
        CALL NEUPAG
        IF(LXOUT(0)) WRITE(LUNTRM,1010)
     >      XLAB(1,1,1),XLAB(2,1,1),XLAB(3,1,1),NIN(1,1),
     >      XLAB(1,2,1),XLAB(2,2,1),XLAB(3,2,1),NIN(2,1),
     >      XLAB(1,3,1),XLAB(2,3,1),XLAB(3,3,1),NIN(3,1)
C
 1010   FORMAT(/'  ** SPLICE3:  FIRST INPUT FILE **'//
     >'  INDEPENDENT COORDINATES:'/
     >'    X1 = "',2A10,1X,A10,'", NPTS = ',I5/
     >'    X2 = "',2A10,1X,A10,'", NPTS = ',I5/
     >'    X3 = "',2A10,1X,A10,'", NPTS = ',I5//
     >'  TWO OF THESE COORDINATES MUST BE *FIXED* FOR ALL INPUT FILES'/
     >'  THE SPLICING OPERATION APPLIES ALONG THE DIMENSION OF THE'/
     >'  NOT-FIXED COORDINATE.  E.G. FOR INPUT FILES VS. TIME AND 2'/
     >'  SPATIAL COORDINATES WHERE THE OUTPUT FILE IS TO COMBINE'/
     >'  DIFFERENT *TIME* RANGES FROM THE INPUT FILES, THE SPATIAL'/
     >'  COORDINATE MUST BE SPECIFIED AS FIXED.  INPUT FILES WILL'/
     >'  BE REJECTED WHICH DO NOT MATCH THIS SPATIAL COORDINATE'/
     >'  PRECISELY.  Here we choose the coordinate dimension along'/
     >'  which to splice; the other 2 will be fixed.')
C
        CALL UREAD('SPLICE3:  SPECIFY SPLICE COORDINATE 1 2 or 3:$$')
C
        IF(LCHAR('1')) THEN
          NDSPLICE=1
          nfix1=2
          nfix2=3
        ELSE IF(LCHAR('2')) THEN
          NDSPLICE=2
          nfix1=1
          nfix2=3
        ELSE IF(LCHAR('3')) THEN
          NDSPLICE=3
          nfix1=1
          nfix2=2
        ELSE
          CALL UERMSG(' SAY AGAIN ? ',' ',1)
          GO TO 15
        ENDIF
C
        INFIX1=NIN(NFIX1,1)
        INFIX2=NIN(NFIX2,1)
C
      ENDIF
C-------------------------------
C
C  NEXT FILE...
      GO TO 10
C
C  FILE NOT READ...
 90   CONTINUE
      NFILEI=NFILEI-1
      GO TO 10
C
C------------------------------------------------------------------
C  NOW SPLICE THE INPUT DATA
C
 100  CONTINUE
      CALL NEUPAG
C
      iflags=0
      IF(NFILEI.EQ.0) THEN
        CALL UERMSG(' ? MUST READ AT LEAST ONE INPUT FILE',' ',1)
        GO TO 10
      ELSE IF(ISETL.EQ.0) THEN
        LFILE=1
        DO 105 IL=1,3
          XLABO(IL,1)=XLAB(IL,1,LFILE)
          XLABO(IL,2)=XLAB(IL,2,LFILE)
          XLABO(IL,3)=XLAB(IL,3,LFILE)
          FLABO(IL)=FLAB(IL,LFILE)
 105    CONTINUE
C
C  COPY FIXED AXES
        DO IX=1,INFIX1
           XOUT(IX,NFIX1)=XIN(IX,NFIX1,LFILE)
        enddo
        DO IX=1,INFIX2
           XOUT(IX,NFIX2)=XIN(IX,NFIX2,LFILE)
        enddo
C
        do ii=1,nfilei
           xbdys(1,ii)=xin(1,NDSPLICE,ii)
           xbdys(2,ii)=xin(NIN(NDSPLICE,ii),NDSPLICE,ii)
        enddo

        call ck_sepseg(xbdys,nfilei,nordrf,iordrf)

        if(nordrf.le.0) then
           !  x ranges overlap -- default is just one interval using
           !  1st file data only; user needs to update...
           NBDY=2
           IFILE(1)=LFILE
           XBDY(1)=-ZINFIN
           XBDY(2)=ZINFIN
        else
           write(luntrm,*) ' %splice domains are disjoint;'
           write(luntrm,*) '  **concatenation is the default operation.'
           NBDY=NFILEI+1
           xbdy(1)=-zinfin
           xbdy(nbdy)=zinfin
           do ii=1,nfilei-1
              ifile(ii)=iordrf(ii)
              iia=iordrf(ii)
              iib=iordrf(ii+1)
              xbdy(ii+1)=(xbdys(2,iia)+xbdys(1,iib))/2
           enddo
           ifile(nfilei)=iordrf(nfilei)
           iflags=1
        endif
C
        ISETL=1
      ENDIF
C
      IF(NFILEI.eq.1) LXSAME=.FALSE.
C
      INTVL=MAX0(0,(NBDY-1))
      IBACK=1  ! MARK BRANCHBACK TO MAIN MENU - CF LIST OPTION
      ILUN=LUNTRM
C
      if(iflags.eq.1) go to 200         ! ** precompute output **
C
      IF(LXOUT(0)) WRITE(LUNTRM,2001) NFILEI,INTVL,LFILE,
     >     (xlabo(il,ndsplice),il=1,3),CMPSTA(ICOMPR),
     >     NOUT,(XLABO(IL,NDSPLICE),IL=1,3)
 2001 FORMAT(
     >' SPLICE3 UTILITY ** CURRENT NO. OF INPUT FILES = ',I2/
     >' CURRENT NUMBER OF SPLICE INTERVALS = ',I3/
     >' OUTPUT FILE LABELING BASED ON INPUT FILE NO. ',I2/
     >' OPTIONS:'/
     >'  1 - PRESET PLOT AXES/SCALING DEFAULTS'/
     >'  A - ACQUIRE MORE INPUT FILES'/
     >'  B - DEFINE SPLICE INTERVAL BDY''S +/OR ASSOCIATED INPUT DATA'/
     >'      % FOR N INTERVALS N+1 BDY''S MUST BE DEFINED'/
     >'      ==> bdy''s are:  ',2a10,1x,a10/
     >'  C - TOGGLE OUTPUT DATA FORMAT SWITCH, NOW "',A,'"'/
     >'  4 - *new* Ufile output format control menu'/
     >'  G - GRAPH INPUT DATA'/
     >'  H - GRAPH OUTPUT DATA '/
     >'      (',I5,' <<',2A10,1X,A10,'>> PTS CURRENTLY DEFINED)'/
     >'  L - LIST INPUT FILENAMES AND SPLICE INTERVALS'/
     >'  M - MODIFY OUTPUT FILE LABELING AND ASSOCIATED SCALARS LIST'/
     >'  R - RESTART:  GET NEW SET OF INPUT DATA NOW'/
     >'  W - WRITE OUTPUT DATA; GET NEW INPUT DATA'/
     >'  Q - QUIT PROGRAM NOW'/)
C
      CALL UREAD('SPLICE3:  ENTER OPCODE:$$')
C
      IANS=IXUOPC('1A4CBGHLMWRQ$')
      IF(IANS.EQ.0) THEN
        CALL CDCOD(ZLBL)
        CALL UERMSG(' % UNRECOGNIZED OPCODE:',ZLBL,1)
        GO TO 100
      ENDIF
C
      GO TO (120,10,140,150,200,300,400,500,600,700,800,900) IANS
C
 120  CONTINUE
      CALL GPRESET
      GO TO 100
C
 140  CONTINUE
      call ufmtsel(icompr)
      if(icompr.gt.0) itoggl=icompr
      GO TO 100
C
 150  CONTINUE
      ICOMPR=ITOGGL-ICOMPR
      GO TO 100
C
C------------------------------------------------------------------
 200  CONTINUE
C  SAVE OR CONCOCT OLD SETTINGS
        INBDY=NBDY
        DO 210 I=1,NBDY
          XWK(I)=XBDY(I)
          IWK(I)=IFILE(I)
 210    CONTINUE
C  MODIFY BDY'S LIST
      IMAXG=MAXG-2
      JNBDY=NBDY-2
      if(iflags.eq.0) then
         CALL INORDU(XBDY(2),ZWK(2),IMAXG,JNBDY,'SPLICE3',0)
      endif
      NBDY=JNBDY+2
      XBDY(1)=-ZINFIN
      XBDY(NBDY)=ZINFIN
C  SET DEFAULT INTERVAL - FILE ASSOCIATIONS
      ICPREV=1
      ICT=1
C  PICK UP NEW INTERVALS AT LOW END...
 230  CONTINUE
      ICI=ICT+1
      IF(ICI.GT.NBDY) GO TO 275
      IF(XBDY(ICI).GT.XWK(ICPREV)) GO TO 240
      IFILE(ICT)=IWK(ICPREV)
      ICT=ICI
      GO TO 230
C
C  PICK UP INTERMEDIATE INTERVALS-- FILE NO. TAKEN FROM FIRST
C  INTERSECTION OF FINITE MEASURE
 235  CONTINUE
      ICI=ICT+1
      IF(ICI.GT.NBDY) GO TO 275
 240  CONTINUE
      ZINTVL=AMIN1(XBDY(ICI),XWK(ICPREV+1)) -
     >           AMAX1(XBDY(ICT),XWK(ICPREV))
      IF(ZINTVL.GT.0.0) THEN
        IFILE(ICT)=IWK(ICPREV)
        ICT=ICI
        GO TO 235
      ELSE IF((ICPREV+1).EQ.INBDY) THEN
C  REACHED LAST OLD INTERVAL-- USE FILE INDEX FOR REMAINING NEW INTVLS
        DO 245 INTV=ICT,NBDY-1
          IFILE(INTV)=IWK(ICPREV)
 245    CONTINUE
        GO TO 275
      ELSE
C  ADVANCE OLD INTERVAL INDEX
        ICPREV=ICPREV+1
        GO TO 240
      ENDIF
C--------------------
C  DEFAULT FILE - INTERVAL RELATIONSHIP DEFINED-- DISPLAY TO USER
 275  CONTINUE
C
      IF(LXSAME) THEN
        IFILE(1)=MAX0(1,IFILE(1))
        IFILE(NBDY-1)=MAX0(1,IFILE(NBDY-1))
      ENDIF
C
      IBACK=2  ! BRANCH BACK MARKER - CF LIST OPTION
      ILUN=LUNTRM
      if(lxout(0)) GO TO 500
C
 276  CONTINUE
      INTVL=NBDY-1
      if(iflags.eq.1) go to 280
      CALL UREAD(
     >'SPLICE3: ENTER INTERVAL # TO MODIFY DATA ASSOCIATION, OR "Q":$$')
      IF(LCHAR('Q')) GO TO 280
C
      INTV=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 275
      IF((INTV.LE.0).OR.(INTV.GT.INTVL)) THEN
        CALL UERMSI(' ? INVALID INTERVAL #, MAX IS',INTVL,1)
        GO TO 275
      ENDIF
C
 277  CONTINUE
      IF(LXOUT(0)) THEN
        IF(LXSAME) THEN
          WRITE(LUNTRM,2277)
 2277 FORMAT(
     >' FOR INTERIOR INTERVALS SPECIFY "0" FOR SLIDING WEIGHTED'/
     >'  USE OF DATA ASSOCIATED WITH NEIGHBORING INTERVALS'/
     >'  (AVAILABLE ONLY WHEN ALL INPUT FILE X AXES ARE IDENTICAL)')
        ELSE
          WRITE(LUNTRM,2278)
 2278 FORMAT(
     >' %SPECIFY "0" TO USE *NO* DATA IN THIS INTERVAL')
        ENDIF
      ENDIF
      CALL UREAD(
     >'SPLICE3:  ENTER NEW FILE INDEX # FOR THIS INTERVAL, OR "Q":$$')
      IF(LCHAR('Q')) GO TO 275
C
      IFILI=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 275
      IF((IFILI.LT.0).OR.(IFILI.GT.NFILEI)) THEN
        CALL UERMSI(' ? INVALID FILE INDEX #, MAX=',NFILEI,1)
        GO TO 275
      ENDIF
      IF(IFILI.EQ.0) THEN
        IF(.NOT.LXSAME) THEN
          CONTINUE
        ELSE IF((INTV.EQ.1).OR.(INTV.EQ.INTVL)) THEN
          CALL UERMSG(' ? "0" CODE INVALID FOR ENDPT INTERVAL',' ',1)
          GO TO 275
        ELSE IF((IFILE(INTV-1).EQ.0).OR.(IFILE(INTV+1).EQ.0)) THEN
          CALL UERMSG(
     >' ? "0" CODE ALREADY USED BY NEIGHBORING INTERVAL',' ',1)
          GO TO 275
        ENDIF
      ENDIF
C
      IFILE(INTV)=IFILI
      GO TO 275
C-------------------------------------
C  CONSTRUCT NEW OUTPUT DATA SET
 280  CONTINUE
      CALL NEUPAG
      INTVL=NBDY-1
C  BUGFIX DMC-- IF SPLICE DIMENSION IS 1ST DIMENSION OF ARRAY, NEED
C  TWO PASSES S.T. LENGTH OF 1ST DIMENSION IS KNOWN BEFORE SPLICED
C  OUTPUT DATA IS STORED - CF LOCAL INTEGER VARIABLE INOUT
      if(nfix2.eq.2) then
         ipass0=2
      else
         ipass0=1
      endif
C
      DO 296 IPASS=IPASS0,2
        NOUT=0
        DO 295 I=1,INTVL
C
          IOUT=0
          IFILI=IFILE(I)
C
          IF(IFILI.GT.0) THEN
            IFILIN(1)=IFILI
            IFILIN(2)=IFILI
          ELSE
            IF(.NOT.LXSAME) GO TO 290
            IFILIN(1)=IFILE(I-1)
            IFILIN(2)=IFILE(I+1)
            IFILI=IFILIN(1)
          ENDIF
C
          IN=NIN(NDSPLICE,IFILI)
          IP1=I+1
C  CHECK FOR NULL INTERSECTION
          IF
     >	     ((XBDY(I).LT.XIN(1,NDSPLICE,IFILI)).AND.
     >	      (XBDY(IP1).LT.XIN(1,NDSPLICE,IFILI)))
     >	      GO TO 290
          IF
     >	     ((XBDY(I).GT.XIN(IN,NDSPLICE,IFILI)).AND.
     >	      (XBDY(IP1).GT.XIN(IN,NDSPLICE,IFILI)))
     >	      GO TO 290
C  CHECK FOR ONE SIDE OVER THE EDGE
          IF(XBDY(I).LT.XIN(1,NDSPLICE,IFILI)) THEN
            I1=1
            CALL XINTER(XIDENT,XBDY(IP1),XIN(1,NDSPLICE,IFILI),
     >         IN,I2,I2P,Z1,Z2,IER)
          ELSE IF(XBDY(IP1).GT.XIN(IN,NDSPLICE,IFILI)) THEN
            I2=IN
            CALL XINTER(XIDENT,XBDY(I),XIN(1,NDSPLICE,IFILI),
     >         IN,I1M,I1,Z1,Z2,IER)
          ELSE
            CALL XINTER(XIDENT,XBDY(I),XIN(1,NDSPLICE,IFILI),
     >          IN,I1M,I1,Z1,Z2,IER)
            CALL XINTER(XIDENT,XBDY(IP1),XIN(1,NDSPLICE,IFILI),
     >         IN,I2,I2P,Z1,Z2,IER)
          ENDIF
C
          IOUT=I2-I1+1
          IF((NOUT+IOUT).GT.MAXNI) THEN
            IF(IPASS.EQ.2) CALL UERMSI(
     >		' ? SPLICE3 OUTPUT 1D ARRAY OVERFLOW, MAXNI=',MAXNI,1)
            IOUT=0
          ENDIF
C
          IF(INFIX1*INFIX2*(NOUT+IOUT).GT.MAXN) THEN
            IF(IPASS.EQ.2) CALL UERMSI(
     >		' ? SPLICE3 OUTPUT 3D ARRAY OVERFLOW, MAXN=',MAXNI,1)
            IOUT=0
          ENDIF
C
          IF(IOUT.EQ.0) GO TO 290
C
          IF(IPASS.EQ.2) THEN
C
            ZX1=XIN(MAX0(1,(I1-1)),NDSPLICE,IFILI)
            ZX2=XIN(MIN0(NIN(NDSPLICE,IFILI),(I2+1)),NDSPLICE,IFILI)
C
            DO 285 IX=1,IOUT
C
               IXI=I1+IX-1
C
               ZW2=(XIN(IXI,NDSPLICE,IFILI)-ZX1)/(ZX2-ZX1)
               ZW1=1.0-ZW2
C
               IXO=NOUT+IX
               XOUT(IXO,NDSPLICE)=XIN(IXI,NDSPLICE,IFILI)
C
               DO 284 IXF2=1,INFIX2
                  do 283 ixf1=1,infix1
                     IF(NDSPLICE.EQ.1) THEN
                        IAO=IXO+INOUT*INFIX1*(IXF2-1)+INOUT*(IXF1-1)
                        IAI=IXI+NIN(1,IFILI)*NIN(2,IFILI)*(IXF2-1)+
     >                     NIN(1,IFILI)*(IXF1-1)
                     ELSE IF(NDSPLICE.EQ.2) THEN
                        IAO=IXF1+INFIX1*INOUT*(IXF2-1)+INFIX1*(IXO-1)
                        IAI=IXF1+NIN(1,IFILI)*NIN(2,IFILI)*(IXF2-1)+
     >                     NIN(1,IFILI)*(IXO-1)
                     ELSE
                        IAO=IXF1+INFIX1*INFIX2*(IXO-1)+INFIX1*(IXF2-1)
                        IAI=IXF1+NIN(1,IFILI)*NIN(2,IFILI)*(IXO-1)+
     >                     NIN(1,IFILI)*(IXF2-1)
                     ENDIF
                     FOUT(IAO)=ZW1*FIN(IAI,IFILIN(1))+
     >                  ZW2*FIN(IAI,IFILIN(2))
 283              continue
 284           CONTINUE
C
 285        CONTINUE
          ENDIF
          NOUT=NOUT+IOUT
          IF(IPASS.EQ.1) INOUT=NOUT
C
 290      CONTINUE
          IF(IPASS.EQ.2) WRITE(LUNTRM,2901) IOUT,I
 2901     FORMAT(' % ',I5,' PTS INSERTED, INTERVAL NO. ',I3)
C
 295    CONTINUE  ! IX
C
 296  CONTINUE  ! IPASS
C
      if(iflags.eq.0) CALL UPAUST(1)
      GO TO 100
C------------------------------------------------------------------
 300  CONTINUE
      IBACK=3 ! LIST BRANCH BACK INDICATOR
      ILUN=LUNTRM
      CALL UREAD(
     >'SPLICE3:  SPECIFY FILE #, "L" TO LIST, "Q" TO QUIT:$$')
      IF(LCHAR('Q')) GO TO 100
      IF(LCHAR('L')) GO TO 500
C
      I=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 300
      IF((I.LT.1).OR.(I.GT.NFILEI)) THEN
        CALL UERMSI(' %VALID FILE NO.S: 1 TO ',NFILEI,0)
        GO TO 300
      ENDIF
C
      ISMOO=0
      WRITE(ZTITL,2088) I
 2088   FORMAT(' SPLICE3 INPUT DATA FILE ',I2,'   ')
C
      CALL GRF4F1(FIN(1,I),nin(1,i),nin(2,i),
     >     xin(1,1,i),nin(1,i),xin(1,2,i),nin(2,i),xin(1,3,i),nin(3,i),
     >     flab(1,i)//flab(2,i), xlab(1,1,i)//xlab(2,1,i),
     >     xlab(1,2,i)//xlab(2,2,i),xlab(1,3,i)//xlab(2,3,i),
     >     flab(3,i), xlab(3,1,i),xlab(3,2,i),xlab(3,3,i),
     >     ZTITL,iproc(i))
C
      GO TO 300
C------------------------------------------------------------------
 400  CONTINUE
C
      IF(NOUT.EQ.0) THEN
        CALL UERMSG(' ? OUTPUT DATA NOT DEFINED - USE OPTION B',' ',1)
        GO TO 100
      ENDIF
C
      I=LFILE
C
      IPROCO=4
C
      ISMOO=0
      if(ndsplice.eq.1) then
         in1=nout
         in2=infix1
         in3=infix2
      else if(ndsplice.eq.2) then
         in1=infix1
         in2=nout
         in3=infix2
      else if(ndsplice.eq.3) then
         in1=infix1
         in2=infix2
         in3=nout
      endif
C
      CALL GRF4F1(FOUT,in1,in2,
     >   xout(1,1),in1,xout(1,2),in2,xout(1,3),in3,
     >   flabo(1)//flabo(2), xlabo(1,1)//xlabo(2,1),
     >   xlabo(1,2)//xlabo(2,2),xlabo(1,3)//xlabo(2,3),
     >   flabo(3), xlabo(3,1),xlabo(3,2),xlabo(3,3),
     >   'SPLICE3 OUTPUT DATA           ',iproco)
C
      GO TO 100
C
C------------------------------------------------------------------
 500  CONTINUE
C
C  LIST INTERVALS AND FILES
C
      CALL NEUPAG
      WRITE(ILUN,5001) XLAB(3,NDSPLICE,LFILE)
 5001 FORMAT(' INTERVAL #  X RANGE     (',A10,') FILE #'/
     >         ' ----------  ----------- -----------  ------')
      INTVL=NBDY-1
      DO 510 I=1,INTVL
        IP1=I+1
        WRITE(ILUN,5002) I,XBDY(I),XBDY(IP1),IFILE(I)
 5002 FORMAT(4X,I3,6X,2(1PE11.4,1X),3X,I2)
 510  CONTINUE
C
      WRITE(ILUN,5010)
 5010 FORMAT(/' FILE #  NAME'/
     >          ' ------  -----------------------------')
      DO 520 I=1,NFILEI
        CALL UFSNAM(NSHOTI(I),ZFILE,IER,LUNI(I))
        IL=INDEX(ZFILE,' ')-1
C  IF WRITING COMMENTS TO FILE, OMIT UNUSED INPUT FILES...
        IF((ILUN.NE.LUNTRM).AND.(IUSE(I).EQ.0)) GO TO 520
        WRITE(ILUN,5012) I,ZFILE(1:IL)
 5012 FORMAT(3X,I2,4X,A)
 520  CONTINUE
C
      IF(IBACK.EQ.1) CALL UPAUST(1)
      GO TO (100,276,300,610,730) IBACK  ! SELECTIVE BRANCH BACK
C
C------------------------------------------------------------------
C  SET SOURCE FILE FOR LABELING OF OUTPUT FILE
C
 600  CONTINUE
      IBACK=4  ! BRANCH BACK - LABELING CONTROL
      ILUN=LUNTRM
      GO TO 500
C
 610  CONTINUE
      CALL UREAD(
     >'SPLICE3: ENTER SOURCE FILE NO. FOR SCALARS + LABELS, OR "Q":$$')
      IF(LCHAR('Q')) GO TO 100
C
      I=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 610
      IF((I.LT.0).OR.(I.GT.NFILEI)) THEN
        CALL UERMSI(' %VALID FILE NO.S: 1 TO ',NFILEI,0)
        GO TO 610
      ENDIF
C
      LFILE=I
      DO 620 IL=1,3
        XLABO(IL,1)=XLAB(IL,1,LFILE)
        XLABO(IL,2)=XLAB(IL,2,LFILE)
        XLABO(IL,3)=XLAB(IL,3,LFILE)
        FLABO(IL)=FLAB(IL,LFILE)
 620  CONTINUE
C
      WRITE(LUNTRM,6201)
     >   XLABO(1,1),XLABO(2,1),XLABO(3,1),
     >   XLABO(1,2),XLABO(2,2),XLABO(3,2),
     >   XLABO(1,3),XLABO(2,3),XLABO(3,3),
     >	 FLABO(1),FLABO(2),FLABO(3)
 6201 FORMAT(' X1 LABEL:  ',2A10,'  UNITS:  ',A10/
     >         ' X2 LABEL:  ',2A10,'  UNITS:  ',A10/
     >         ' X3 LABEL:  ',2A10,'  UNITS:  ',A10/
     >         '  F LABEL:  ',2A10,'  UNITS:  ',A10)
C
      CALL UREAD('SPLICE3:  CHANGE *X1* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=XLABO(1,1)//XLABO(2,1)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        XLABO(1,1)=ZLBL(1:10)
        XLABO(2,1)=ZLBL(11:20)
C
        ZUNS=XLABO(3,1)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        XLABO(3,1)=ZUNS
      ENDIF
C
      CALL UREAD('SPLICE3:  CHANGE *X2* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=XLABO(1,2)//XLABO(2,2)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        XLABO(1,2)=ZLBL(1:10)
        XLABO(2,2)=ZLBL(11:20)
C
        ZUNS=XLABO(3,2)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        XLABO(3,2)=ZUNS
      ENDIF
C
      CALL UREAD('SPLICE3:  CHANGE *X3* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=XLABO(1,3)//XLABO(2,3)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        XLABO(1,3)=ZLBL(1:10)
        XLABO(2,3)=ZLBL(11:20)
C
        ZUNS=XLABO(3,3)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        XLABO(3,3)=ZUNS
      ENDIF
C
      CALL UREAD('SPLICE3:  CHANGE *Y* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=FLABO(1)//FLABO(2)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        FLABO(1)=ZLBL(1:10)
        FLABO(2)=ZLBL(11:20)
C
        ZUNS=FLABO(3)
        CALL UREADL(
     >	      'SPLICE3:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        FLABO(3)=ZUNS
      ENDIF
C
C  SCALAR MODS
      IF(NUMSC(LFILE).GT.0) CALL UFSCLK(LUNI(LFILE),
     >     NSHOTI(LFILE),NUMSC(LFILE),
     >     SCVAL(1,LFILE),SCLAB(1,1,LFILE),IORDER,1,LUNTRM)
C
      GO TO 100
C------------------------------------------------------------------
 700  CONTINUE
C
C  WRITE OUTPUT FILE
C
      if(nout.eq.0) then
         write(luntrm,7701)
 7701    format(' ? no points defined for output file.  Use'/
     >            '   option B to define location of boundary between'/
     >            '   intervals and to associate input files with'/
     >            '   intervals along the splicing axis.')
         call uclear
         go to 100
      endif
C
      NSHOTO=NSHOTI(LFILE)
      ZLBL='SPLICED OUTPUT FILE'
C
      CALL UFNTER(LUNOUT,ZLBL)
C
      CALL UREADI('SPLICE3:  SPECIFY SHOT NO. FOR OUTPUT FILE:$$',
     >              NSHOTO)
C
      CALL UFCMPR(LUNOUT,ICOMPR)
      CALL UFOPWR(LUNOUT,NSHOTO,IER)
      IF(IER.NE.0) THEN
        CALL UCLEAR
        GO TO 100
      ENDIF
C
      IPROCO=4
C
      if(ndsplice.eq.1) then
         in1=inout
         in2=infix1
         in3=infix2
      else if(ndsplice.eq.2) then
         in1=infix1
         in2=inout
         in3=infix2
      else if(ndsplice.eq.3) then
         in1=infix1
         in2=infix2
         in3=inout
      endif
C
      CALL UF3DWR(LUNOUT,IDEV(LFILE),SDATE(LFILE),
     1	FOUT,IN1,IN2,XOUT(1,1),IN1,XOUT(1,2),IN2,XOUT(1,3),IN3,
     1	IPROCO,FLABO,XLABO(1,1),XLABO(1,2),XLABO(1,3),
     1	NUMSC(LFILE),SCVAL(1,LFILE),SCLAB(1,1,LFILE),
     1	IER)
C
      IF(IER.NE.0) THEN
        CALL UFCLOS(LUNOUT)
        CALL UCLEAR
        GO TO 100
      ENDIF
C
C  APPEND COMMENTS TO FILE
C
C  DETERMINE WHICH INPUT FILES WERE ACTUALLY USED
C
      DO 720 IFIL=1,NFILEI
        IUSE(IFIL)=0
        DO 710 IINTV=1,INTVL
          IF(IFILE(IINTV).EQ.IFIL) IUSE(IFIL)=IUSE(IFIL)+1
 710    CONTINUE
 720  CONTINUE
C
C
C  WRITE INTO TEMPORARY COMMENTS FILE
      CALL UFOPCF(ILUNT,IER)
C
C  USER COMMENTS
C
      CALL UFACOM(ILUNT)
C
      WRITE(ILUNT,7301)
 7301 FORMAT(' THIS FILE DATA PRODUCED BY UFILES SPLICE3 UTILITY'/
     >'  LIST OF 3D INPUT UFILES SPLICED FROM AS FOLLOWS:'/)
C
C  RECORD INPUT FILE NAMES IN OUTPUT FILE COMMENTS
      IBACK=5
      ILUN=ILUNT
      GO TO 500
C
 730  CONTINUE
C
      DO 750 I=1,NFILEI
        IF(IUSE(I).EQ.0) GO TO 750
        CALL UFSNAM(NSHOTI(I),ZFILE,IER,LUNI(I))
        WRITE(ILUNT,7501) ZFILE
 7501 FORMAT(' ----------------------------------------------'/
     >' COMMENTS SPOOLED FROM INPUT FILE:'/1X,A/)
        CALL USPOOL(LUNI(I),ILUNT)
 750  CONTINUE
C
C  APPEND COMMENTS TO OUTPUT FILE
      CALL UCSEND(ILUNT,LUNOUT)
C
      CALL UFCLOS(LUNOUT)
C
C------------------------------------------------------------------
C  CLOSE OUT INPUT FILES; GET NEW BATCH
C
 800  CONTINUE
      DO 810 I=1,NFILEI
        CALL UFCLOS(LUNI(I))
 810  CONTINUE
      LXSAME=.TRUE.
      NFILEI=0
      ISETL=0
      fin=0.0
      GO TO 10
C------------------------------------------------------------------
C  EXIT
C
 900  CONTINUE
      CALL UEXITA(0)
      STOP
      END
