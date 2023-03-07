C--------------------------------------------------------------------
C  SPLICE  UFILES UTILITY - COMBINE PIECES OF 1D UFILES TO MAKE NEW 1D
C    UFILE
C
      PROGRAM SPLICE
C
C    E.G. F1(T),F2(T),F3(T)  ARE COMBINED TO FORM F(T) BY TAKING
C      THE DATA FROM F2 FOR INTERVAL TA TO TB,
C      THE DATA FROM F1 FOR INTERVAL TB TO TC,
C      THE DATA FROM F3 FOR INTERVAL TC TO TD, TA.LT.TB.LT.TC.LT.TD
C
      USE SPLICE_BLK
C
      INTEGER IORDER(MAXSC)
      INTEGER IUSE(MAXFI)
C
      INTEGER IFILIN(2)
      integer nordrf,iordrf(MAXFI)
C
      LOGICAL LXSAME
C
      CHARACTER*20 ZLBL
      CHARACTER*10 ZUNS
C
      CHARACTER*100 ZFILE
C
      character*60 mnlab(3)
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
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
      LOGICAL LYES
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C => SPLICE               BEGIN DATA BLOCK
C => LISMAK 12-APR-88  MOVED DATA STATEMENTS
      DATA CMPSTA/'ascii','????','binary','HDF','NetCDF'/
      DATA ICOMPR/2/      ! default:  portable compressed binary
      DATA ZINFIN/1.E38/
C => END DATA BLOCK
C ====================
C
      itoggl=icompr
C
      call ufheader("splice")
      CALL UINITG("SPLICE")
C
      NBDY=0
C
      DO 1 I=1,MAXFI
        LUNI(I)=30+I
 1    CONTINUE
      LUNOUT=LUNI(MAXFI)+1
      ILUNT=LUNOUT+1  ! COMMENTS FILE LUN
C
C  INPUT LOOP
C
 5    CONTINUE
C
      NFILEI=0
      ISETL=0
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
      if(NFILEI.le.1) LXSAME=.TRUE.
      IF(LXOUT(0)) WRITE(LUNTRM,1001) NFILEI
 1001 FORMAT(' % CURRENTLY ',I2,' INPUT FILES HAVE BEEN READ')
      CALL UREAD(
     >'SPLICE: READ ANOTHER 1D INPUT UFILE ? (Y/N; Q EXITS PROGRAM):$$')
      IF(LCHAR('Q')) GO TO 900
      IF(.NOT.LYES(0)) GO TO 100
C
      NFILEI=NFILEI+1
      WRITE(ZLBL,3001) NFILEI
 3001 FORMAT('1D INPUT FILE NO. ',I2)
      CALL UFNTER(LUNI(NFILEI),ZLBL)
C
 20   CONTINUE
      CALL NEUPAG
      CALL UFSNMD(LUNTRM,ZLBL,LUNI(NFILEI))
C
      CALL URSHOT('SPLICE',', "Q" TO BACK UP','Q',ISHOT,IOP)
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
      CALL UF1DRD(LUNI(NFILEI),IDEV(NFILEI),SDATE(NFILEI),
     >   XIN(1,NFILEI),YIN(1,NFILEI),MAXN,NIN(NFILEI),IPROC(NFILEI),
     >   XLAB(1,NFILEI),YLAB(1,NFILEI),
     >   MAXSC,NUMSC(NFILEI),SCVAL(1,NFILEI),SCLAB(1,1,NFILEI),
     >   IER)
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
        IFLAGQ=0
C  CHECK UNITS CONSISTENCY
        IF((XLAB(3,NFILEI).NE.XLAB(3,1)).OR.
     >       (YLAB(3,NFILEI).NE.YLAB(3,1))) THEN
          CALL UCLEAR
          WRITE(LUNTRM,1090) XLAB(3,1),XLAB(3,NFILEI),
     >               YLAB(3,1),YLAB(3,NFILEI)
 1090 FORMAT(' % INPUT DATA UNITS INCONSISTENCY:'/
     >'   1ST INPUT FILE    INPUT FILE JUST READ'/
     >'  X:  ',A10,'        ',A10/
     >'  Y:  ',A10,'        ',A10/)
          IFLAGQ=1
        ENDIF
C
C  CHECK CONSISTENCY OF DATA ITSELF
        IF(NIN(1).NE.NIN(NFILEI)) THEN
          CALL UCLEAR
          IFLAGQ=1
          WRITE(LUNTRM,1091) NIN(1),NIN(NFILEI)
 1091 FORMAT(' % INPUT DATA X AXES:  DIFFERENT NUMBER OF PTS:'/
     >'   1ST INPUT FILE, NX=',I5,'  THIS INPUT FILE, NX=',I5)
        ELSE
          ZXR=ABS(XIN(NIN(1),1)-XIN(1,1))
          DO 28 IX=1,NIN(1)
            IF((ABS(XIN(IX,1)-XIN(IX,NFILEI))/ZXR).GT.1.E-4) GO TO 29
 28       CONTINUE
          IX=0
 29       CONTINUE
          IF(IX.GT.0) THEN
            IFLAGQ=1
            WRITE(LUNTRM,1092) IX,XIN(IX,1),XIN(IX,NFILEI)
 1092 FORMAT(' % INPUT DATA X AXIS:  ',I5,'''TH DATA PT DIFFERENT:'/
     >'   1ST INPUT FILE, X=',1PE14.7,' THIS INPUT FILE, X=',1PE14.7)
          ENDIF
        ENDIF
C
        IF(IFLAGQ.EQ.1) THEN
          CALL UREAD('SPLICE:  USE INPUT DATA?  (Y/N):$$')
          IF(.NOT.LYES(0)) THEN
            CALL UFCLOS(LUNI(NFILEI))
            GO TO 90
          ELSE
            LXSAME=.FALSE.
          ENDIF
        ENDIF
      ENDIF
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
          XLABO(IL)=XLAB(IL,LFILE)
          YLABO(IL)=YLAB(IL,LFILE)
 105    CONTINUE
C
        do ii=1,nfilei
           xbdys(1,ii)=xin(1,ii)
           xbdys(2,ii)=xin(NIN(ii),ii)
        enddo

        call ck_sepseg(xbdys,nfilei,nordrf,iordrf)

        if(nordrf.le.0) then
           !  x ranges overlap -- default is just one interval using
           !  1st file data only; user needs to update...
           NBDY=2
           IFILE(1)=1
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
      if(iflags.eq.1) go to 200
C
      IF(LXOUT(0)) WRITE(LUNTRM,2001) NFILEI,INTVL,LFILE,
     >     CMPSTA(ICOMPR),NOUT
 2001 FORMAT(
     >' SPLICE UTILITY ** CURRENT NO. OF INPUT FILES = ',I2/
     >' CURRENT NUMBER OF SPLICE INTERVALS = ',I3/
     >' OUTPUT FILE LABELING BASED ON INPUT FILE NO. ',I2/
     >' OPTIONS:'/
     >'  1 - PRESET PLOT AXES/SCALING DEFAULTS'/
     >'  A - ACQUIRE MORE INPUT FILES'/
     >'  B - DEFINE SPLICE INTERVAL BDY''S +/OR ASSOCIATED INPUT DATA'/
     >'      % FOR N INTERVALS N+1 BDY''S MUST BE DEFINED'/
     >'      *** option B defines the splicing operation ***'/
     >'  C - TOGGLE OUTPUT DATA FORMAT SWITCH, NOW "',A,'"'/
     >'  4 - *new* Ufile output format control menu'/
     >'  G - GRAPH INPUT DATA'/
     >'  H - GRAPH OUTPUT DATA (',I5,' PTS CURRENTLY DEFINED)'/
     >'  L - LIST INPUT FILENAMES AND SPLICE INTERVALS'/
     >'  M - MODIFY OUTPUT FILE LABELING AND ASSOCIATED SCALARS LIST'/
     >'  R - RESTART:  GET NEW SET OF INPUT DATA NOW'/
     >'  W - WRITE OUTPUT DATA; GET NEW INPUT DATA'/
     >'  Q - QUIT PROGRAM NOW'/)
C
      CALL UREAD('SPLICE:  ENTER OPCODE:$$')
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
        if(iflags.eq.0) then
           CALL INORDU(XBDY,XOUT,MAXG,NBDY,'SPLICE',0)
        endif
      IF(NBDY.LT.2) THEN
        WRITE(LUNTRM,2200)
 2200 FORMAT(
     >' % NO INTERVALS DEFINED.  N+1 BDY''S NEEDED FOR N INTERVALS')
        CALL UPAUST(1)
        NBDY=2
        IFILE(1)=1
        XBDY(1)=-ZINFIN
        XBDY(2)=ZINFIN
        GO TO 100
      ENDIF
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
     >'SPLICE:  ENTER INTERVAL # TO MODIFY DATA ASSOCIATION, OR "Q":$$')
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
     >'SPLICE:  ENTER NEW FILE INDEX # FOR THIS INTERVAL, OR "Q":$$')
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
      NOUT=0
      DO 295 I=1,INTVL
C
        IFILI=IFILE(I)
        IOUT=0
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
        IN=NIN(IFILI)
C
        IP1=I+1
C  CHECK FOR NULL INTERSECTION
        IF
     >     ((XBDY(I).LT.XIN(1,IFILI)).AND.(XBDY(IP1).LT.XIN(1,IFILI)))
     >      GO TO 290
        IF
     >     ((XBDY(I).GT.XIN(IN,IFILI)).AND.(XBDY(IP1).GT.XIN(IN,IFILI)))
     >      GO TO 290
C  CHECK FOR ONE SIDE OVER THE EDGE
        IF(XBDY(I).LT.XIN(1,IFILI)) THEN
          I1=1
          CALL XINTER(XIDENT,XBDY(IP1),XIN(1,IFILI),IN,I2,I2P,
     >                  Z1,Z2,IER)
        ELSE IF(XBDY(IP1).GT.XIN(IN,IFILI)) THEN
          I2=IN
          CALL XINTER(XIDENT,XBDY(I),XIN(1,IFILI),IN,I1M,I1,Z1,Z2,IER)
        ELSE
          CALL XINTER(XIDENT,XBDY(I),XIN(1,IFILI),IN,I1M,I1,Z1,Z2,IER)
          CALL XINTER(XIDENT,XBDY(IP1),XIN(1,IFILI),IN,I2,I2P,
     >                  Z1,Z2,IER)
        ENDIF
C
        IOUT=I2-I1+1
        IF((NOUT+IOUT).GT.MAXN) THEN
          CALL UERMSI(' ? SPLICE OUTPUT ARRAY OVERFLOW, MAXN=',MAXN,1)
          IOUT=0
          GO TO 290
        ENDIF
C
        ZX1=XIN(MAX0(1,(I1-1)),IFILI)
        ZX2=XIN(MIN0(NIN(IFILI),(I2+1)),IFILI)
C
        DO 285 IX=1,IOUT
C
          IXI=I1+IX-1
C
          ZW2=(XIN(IXI,IFILI)-ZX1)/(ZX2-ZX1)
          ZW1=1.0-ZW2
C
          IXO=NOUT+IX
          XOUT(IXO)=XIN(IXI,IFILI)
          YOUT(IXO)=ZW1*YIN(IXI,IFILIN(1))+ZW2*YIN(IXI,IFILIN(2))
C
 285    CONTINUE
        NOUT=NOUT+IOUT
C
 290    CONTINUE
        WRITE(LUNTRM,2901) IOUT,I
 2901 FORMAT(' % ',I5,' PTS INSERTED, INTERVAL NO. ',I3)
 295  CONTINUE
      if(iflags.eq.0) CALL UPAUST(1)
      GO TO 100
C------------------------------------------------------------------
 300  CONTINUE
      IBACK=3 ! LIST BRANCH BACK INDICATOR
      ILUN=LUNTRM
      CALL UREAD(
     >'SPLICE:  SPECIFY FILE #, "L" TO LIST, "Q" TO QUIT:$$')
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
      INUM0=MIN0(1,NUMSC(I))
      IND=MAX0(1,INUM0)
C
      write(mnlab(1),3101) idev(i),nshoti(i),sdate(i)
 3101 format(a4,1x,i6,1x,a)
      write(mnlab(2),3102) i
 3102 format(' input file no. ',i2)
      mnlab(3)=ylab(1,i)//ylab(2,i)//' vs. '//xlab(1,i)//xlab(2,i)
C
      call grafx1(xin(1,I),yin(1,I),nin(i),xlab(3,i),ylab(3,i),
     1  mnlab(1),mnlab(2),mnlab(3))
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
      INUM0=MIN0(1,NUMSC(I))
      IND=MAX0(1,INUM0)
C
      IPROCO=4
      CALL GRFUF1(XOUT,YOUT,NOUT,IPROCO,
     >     YLABO,XLABO,NSHOTI(I),IDEV(I),
     >     SDATE(I),INUM0,SCVAL(IND,I),SCLAB(1,IND,I))
C
      GO TO 100
C
C------------------------------------------------------------------
 500  CONTINUE
C
C  LIST INTERVALS AND FILES
C
      CALL NEUPAG
      WRITE(ILUN,5001) XLAB(3,LFILE)
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
     >'SPLICE: ENTER SOURCE FILE NO. FOR LABELS, OR "Q":$$')
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
        XLABO(IL)=XLAB(IL,LFILE)
        YLABO(IL)=YLAB(IL,LFILE)
 620  CONTINUE
C
      WRITE(LUNTRM,6201)
     >   XLABO(1),XLABO(2),XLABO(3),YLABO(1),YLABO(2),YLABO(3)
 6201 FORMAT(' X LABEL:  ',2A10,'  UNITS:  ',A10/
     >         ' Y LABEL:  ',2A10,'  UNITS:  ',A10)
C
      CALL UREAD('SPLICE:  CHANGE *X* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=XLABO(1)//XLABO(2)
        CALL UREADL('SPLICE:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        XLABO(1)=ZLBL(1:10)
        XLABO(2)=ZLBL(11:20)
C
        ZUNS=XLABO(3)
        CALL UREADL('SPLICE:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        XLABO(3)=ZUNS
      ENDIF
C
      CALL UREAD('SPLICE:  CHANGE *Y* LABEL? (Y/N):$$')
      IF(LYES(0)) THEN
C
        ZLBL(1:20)=YLABO(1)//YLABO(2)
        CALL UREADL('SPLICE:  ENTER NEW LABEL (MAX 20 CHARS):$$',ZLBL)
        YLABO(1)=ZLBL(1:10)
        YLABO(2)=ZLBL(11:20)
C
        ZUNS=YLABO(3)
        CALL UREADL('SPLICE:  ENTER NEW UNITS (MAX 10 CHARS):$$',ZUNS)
        YLABO(3)=ZUNS
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
      NSHOTO=NSHOTI(LFILE)
      ZLBL='SPLICED OUTPUT FILE'
C
      CALL UFNTER(LUNOUT,ZLBL)
C
      CALL UREADI('SPLICE:  SPECIFY SHOT NO. FOR OUTPUT FILE:$$',
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
      CALL UF1DWR(LUNOUT,IDEV(LFILE),
     >     SDATE(LFILE),XOUT,YOUT,NOUT,IPROCO,
     >     XLABO,YLABO,NUMSC(LFILE),
     >     SCVAL(1,LFILE),SCLAB(1,1,LFILE),IER)
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
 7301 FORMAT(' THIS FILE DATA PRODUCED BY UFILES SPLICE UTILITY'/
     >'  LIST OF 1D INPUT UFILES SPLICED FROM AS FOLLOWS:'/)
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
      GO TO 10
C------------------------------------------------------------------
C  EXIT
C
 900  CONTINUE
      CALL UEXITA(0)
      STOP
      END
