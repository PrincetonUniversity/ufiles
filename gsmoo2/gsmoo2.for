C******************** START FILE GSMOO2.FOR ; GROUP GSMOO2 *************
C-----------------------------------------------------------
C  GSMOO2
C  PROGRAM TO SMOOTH DATA INPUT FROM UFILES DATA FILE
C
      PROGRAM GSMOO2
      USE USMOO
C
      PARAMETER (MAXSC=200)
      PARAMETER (JDIND=32768*16)     ! RGA, scaled up for R.Budny
      PARAMETER (JDSIZ=1000000*8)
C
C  DATA BUFFERS AND LABELS
      LOGICAL LSTORED    ! Has deglitched data been stored over input data?
      LOGICAL LSWCH
      LOGICAL LLIST
      LOGICAL LDEGLITCH
 
      REAL X(JDIND),Y(JDIND),ZWK(JDIND,4),XTRVAL(MAXSC)
      REAL F(JDSIZ),FF(JDSIZ),DELF(JDSIZ)
      REAL XY(JDIND,2)
      CHARACTER*10 ZSDATE,XLAB(3),YLAB(3),FLAB(3),ZSHID,ZPROG,ZDATE
      CHARACTER*10                       FFLAB(3)
      CHARACTER*10 XTRLAB(3,MAXSC)
      CHARACTER*30 ZLABLS(2)
      CHARACTER*30 TLABEL           ! Temporary label. tbt 2/15/91
      CHARACTER*45 ZTIT45
      CHARACTER*15 ZLABEL
      CHARACTER*128 ZFILE           ! RGA, ufsnam exceeded this dimension
      CHARACTER*10 ZFUNS,ZXUNS,ZYUNS,ZTEMP,ZFUNS0,ZXUNS0,ZYUNS0
      CHARACTER*4 IDEV
      CHARACTER*1 CRETURN
      INTEGER IORDER(MAXSC)
      INTEGER INRET(2)
C
      EQUIVALENCE(ZLABLS(1),XLAB(1))
      EQUIVALENCE(ZLABLS(2),YLAB(1))
C
      EQUIVALENCE(X,XY(1,1))
      EQUIVALENCE(Y,XY(1,2))
C
      EQUIVALENCE(NRETX,INRET(1))
      EQUIVALENCE(NRETY,INRET(2))
C
      CHARACTER*10 ZTITLE(3)
C
C
C-----------------------------
C flag that axis order was reversed for smoothing
      logical lxrev
      character*30 xrlab
C
      logical lyrev
      character*30 yrlab
C-----------------------------
C LAST UPDATED:
 
C   1/29/91  TBT  Added automatic 2-D deglitching. Options C, F, S & R.
C
C  PROGRAM MOD APR 1984 DMC-- DEAL WITH VAX COMPRESSED UFILES DATA
C   FORMATTED COMMENTS FOR THE OUTPUT FILE ARE PUT IN A TEMPORARY FILE
C   WHICH IS SPOOLED TO THE PERMANENT FILE WHICH IS OPTIONALLY BINARY-
C   COMPRESSED.
C  USER MAY SELECT COMPRESSED OR UNCOMPRESSED DATA OUTPUT
C
      CHARACTER*7 CMPSTA(0:4)
C
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C  COMPRESSED DATA OUTPUT DEFAULT FLAG, =1 FOR COMPRESSION ON
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C  L.U.N. FOR TEMPORARY FILE-- COMMENTS-- SCALARS MODIFICATION
C
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C-------------------------------
C
C  L.U.N FOR DATA INPUT
C
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C  L.U.N FOR DATA OUPUT
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR,LYES,lxout
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C => GSMOO2               BEGIN DATA BLOCK
C => LISMAK 12-APR-88  MOVED DATA STATEMENTS
      DATA CMPSTA/'ascii','????','binary','HDF','NetCDF'/
C
      DATA ICOMPR/2/  ! default:  portable binary
C
      DATA ILUNT/23/
      DATA ILUNI/21/
      DATA ILUNO/22/
      DATA ILUNX/24/
C => END DATA BLOCK
C ====================
C
      f=0.0
      ff=0.0
      delf=0.0
C
      ITOGGL=max(2,ICOMPR)
C
      LSTORED = .TRUE.		! No deglitched data to store.
      IDIM = 1   ! DEFAULT
 
      IALGOR = 2
      CRETURN = 'Q'
      LDEGLITCH = .FALSE.
      LLIST = .FALSE.
      IMETHOD = 2
      IWIDTH = 2
      ZFIXU = 1.E38
      ZFIXL = -ZFIXU
      ZXMAX = 1.E38
      ZXMIN = - ZXMAX
      ZGSIZE = 0
C
      MAXD=JDSIZ
      MAXIND=JDIND
C
      call ufheader("gsmoo2")
      call date_and_time(DATE=zdate)
      CALL UINITG("GSMOO2")
      luntrm = lnout(0)
CC	CALL USINIT
C  TYPE-AHEAD ENABLE
CC	CALL UAHEAD
      call urdext('ifi')
 10   CONTINUE
      CALL UFNTER(ILUNI,'2D INPUT FILES - RAW')
      CALL UFNTER(ILUNO,'OUTPUT FILES- SMOOTH')
C
 20   CONTINUE
      ZBASE=1.E35
      ZDROP=1.E35
      NBASE=0
      NDROP=0
      CALL NEUPAG
      IF(LXOUT(0)) CALL UFSNMD(LUNTRM,'               INPUT',ILUNI)
      IF(LXOUT(0)) CALL UFSNMD(LUNTRM,'              OUTPUT',ILUNO)
C
 21   CONTINUE
      CALL URSHOT('GSMOO2',', "Q" TO QUIT, "D" TO CHANGE NAMES','QD',
     >	  ISHOT,IANS)
      IF(IANS.EQ.1) GO TO 900
      IF(IANS.EQ.2) GO TO 10
      IF(ISHOT.EQ.0) GO TO 900
C-- CLEAR CONVERSION FACTORS
      ZXCONA=1.0
      ZYCONA=1.0
      ZFCONA=1.0
      ZXCONB=0.0
      ZYCONB=0.0
      ZFCONB=0.0
C---
C  CREATE TEMPORARY FILE
      CALL UFOPCF(ILUNT,IER)
      WRITE(ILUNT,7000)
 7000 FORMAT(2X)
C---
C
      LSWCH=.FALSE.
      IXUNOL=0
      IYUNOL=0
      IFUNOL=0
C
      ISM=0
      ZPROG='GSMOO2 OP.'
C
      CALL UFOPRD(ILUNI,ISHOT,IER)
      IF(IER.NE.0) CALL UCLEAR
      IF(IER.NE.0) GO TO 21
C
      INF1=MAXD/MAXIND
      CALL UF2DRD(ILUNI,IDEV,ZSDATE,
     >   F,INF1,X,MAXIND,Y,MAXIND,
     >   IPROC,NRETX,NRETY,1,
     >   FLAB,XLAB,YLAB,
     >   MAXSC,NUMSC,XTRVAL,XTRLAB,IER)
C
      ZTITLE(1)='SHOTDATE: '
      ZTITLE(2)=ZSDATE
      ZTITLE(3)=' (GSMOO2) '
C
      NUMD=NRETX*NRETY
C
      IPROCP=IPROC
      ZXUNS0=XLAB(3)
      ZYUNS0=YLAB(3)
      ZFUNS0=FLAB(3)
      IF((IPROC.EQ.0).OR.(IPROC.EQ.1)) GO TO 32
      IF(IPROC.GT.3) GO TO 34
      WRITE(LUNTRM,9091)
 9091 FORMAT(' % GSMOO2: INPUT DATA HAS ALREADY BEEN SMOOTHED')
      GO TO 34
C
 32   CONTINUE
C  PRESET PROCESS CODE TO INCLUDE "SMOOTHED"
CT	IPROC=IPROC+2
C
 34   CONTINUE
C
      IF(IER.NE.0) THEN
        CALL UCLEAR
        GO TO 290
      ENDIF
C
C  CHECK AXIS MONOTONICITY
      lxrev=.false.
      CALL GSCKAX('GSMOO2',XLAB(1),XLAB(3),X,NRETX,IER)
      IF(IER.NE.0) THEN
        CALL UCLEAR
        iabort=1
        if(ier.eq.2) then
c  repair is possible -- option to invert axis order
           call uread(
     >'gsmoo1:  reverse x axis order and continue? (Y/N):$')
           if(lyes(0)) then
      	iabort=0
      	xrlab=xlab(1)//xlab(2)//xlab(3)
      	lxrev=.true.
      	call grev2(x,y,f,nretx,nrety,nretx,1)
           endif
        endif
        if(iabort.eq.1) GO TO 290
      ENDIF
C
C  CHECK AXIS MONOTONICITY
      lyrev=.false.
      CALL GSCKAX('GSMOO2',YLAB(1),YLAB(3),Y,NRETY,IER)
      IF(IER.NE.0) THEN
        CALL UCLEAR
        iabort=1
        if(ier.eq.2) then
c  repair is possible -- option to invert axis order
           call uread(
     >'gsmoo1:  reverse y axis order and continue? (Y/N):$')
           if(lyes(0)) then
      	iabort=0
      	yrlab=ylab(1)//ylab(2)//ylab(3)
      	lyrev=.true.
      	call grev2(x,y,f,nretx,nrety,nretx,2)
           endif
        endif
        if(iabort.eq.1) GO TO 290
      ENDIF
C
      KTPPCE=0
C
C  SMOOTH THE DATA
C
        write(ZSHID,3001) IDEV,ISHOT  !*** jpj@JET Apr-1995 (was ENCODE)
 3001 FORMAT(A4,I6)
C  CHOOSE DIMENSION OPTION--
 50   CONTINUE
      CALL NEUPAG
 51   IF(LXOUT(0)) THEN
        IF(ISM.EQ.1) WRITE(LUNTRM,5000)
 5000     FORMAT(' % INPUT DATA HAS BEEN MODIFIED')
        WRITE(LUNTRM,5001) NUMSC,ZSHID,
     >     (XLAB(IJ),IJ=1,3),(YLAB(IJ),IJ=1,3),
     >     CMPSTA(ICOMPR),(FLAB(IJ),IJ=1,3)
      ENDIF
 5001 FORMAT(/' GSMOO2.EXE - FEBRUARY 9, 1991 - TBT'/
     >  /' THE FILE DATA CONTAINS ',I3,' ASSOCIATED KEYWORD SCALARS'/
     >  ' DATA: ',A10,' SMOOTHING OPTIONS:'/
     >  '  "1":  SMOOTH DATA ALONG 1ST DIMENSION (X): ',3A10/
     >  '  "2":  SMOOTH DATA ALONG 2ND DIMENSION (Y): ',3A10/
     >  '  ==> APPEND "F" FOR *FAST* SMOOTHING, (E.G. TYPE "1F")'//
     >  '  "A":  "CUT AND PASTE" THE DATA (NUMERIC REFLECTION INTERP.)'/
     >  '  "B":  "CUT AND PASTE" THE DATA (CUBIC INTERPOLANT)'/
     A  '  "C":  AUTOMATIC 2-D DEGLITCH'/
     >  '  "D":  DISPLAY/MODIFY KEYWORD SCALAR DATA'/
     >  '  "N":  Apply 1d Ufile for X or Y dependent renormalization.'/
     >  '  "G":  GRAPH THE DATA AS CURRENTLY STORED'/
     >  '  "I":  INTERCHANGE DATA INDEPENDANT VARIABLES (X<-->Y)'/
     >  '  "P":  COMPRESS- Toggle Ufile output format, NOW "',A,'"'/
     >  '  "4":  Ufile output format control menu'/
     >  '  "W":  WRITE DATA [F: ',3A10,'] TO OUTPUT FILE'/
     >  '  "X":  EXIT:  GET NEW SHOT WITHOUT WRITING DATA TO FILE'/
     >  '  "U":  [F(X,Y)] CHANGE UNITS OF DATA "F", "X", OR "Y" ARRAYS'/
     >  ' OPTIONS "1" AND "2" ALSO PERMIT PLOTTING OF THE DATA'/
     >  ' OPTIONS "1","2","U" AND "I" REPLACE THE INPUT DATA'/)
      CALL UREAD('GSMOO2:  ENTER SMOOTHING OPTION (1/2/I/W/G ... ):$')

      if(lchar('N')) go to 600

      IANS=IXUOPC('12GPIWXUDABCQ4$')
C                    12345678901234567890
      CALL UCHKOP(IANS,0,1,100,-1,*51)
      GO TO (60, 60, 53, 55, 90,100,290, 80,300,400,
     A        405,500,900,56) IANS
C               1   2   3   4   5   6   7   8   9  10
C               1   2   3   4
C
C  GRAPH THE DATA
C
 53   CONTINUE
      CALL GRF3(X,F,Y,ZWK,NRETX,NRETY,NRETX,
     >   XLAB,YLAB,FLAB,XLAB(3),YLAB(3),FLAB(3),
     >   ZTITLE,1,0,0,IDEV,ISHOT,0,ISHOT,1,ISM)
      GO TO 50
C
C  TOGGLE COMPRESSED DATA OUTPUT FLAG
C
 55   CONTINUE
      ICOMPR=ITOGGL-ICOMPR
      GO TO 50
C
 56   continue
      call ufmtsel(icompr)
      if(icompr.gt.0) itoggl=icompr
      go to 50
C
 60   CONTINUE
C
      IF (.NOT. LSTORED)  THEN
         WRITE(LUNTRM,9191)
 9191    FORMAT(' ? GSMOO2: DEGLITCHED DATA WILL BE WRITTEN OVER.'/
     1            '           CHOOSE OPTION R OR S FIRST'/ )
         CALL UPAUST(1)
         GO TO 50
      ENDIF  ! LSTORED
 
 
C  SMOOTH DATA ALONG SPECIFIED DIMENSION
      IF(LCHAR('F')) THEN
C  FAST SMOOTHING ***
        CALL USMFA2(X,Y,F,IANS,NRETX,NRETY,NRETX,FF,ZWK,MAXIND,
     >     XLAB,YLAB,FLAB,XLAB(3),YLAB(3),FLAB(3),ZPROG,IDEV,ISHOT)
        IBASE=0
        IDROP=0
C  REGULAR SMOOTHING ***
      ELSE
        CALL USMOO2(X,Y,F,IANS,NRETX,NRETY,NRETX,FF,DELF,
     >     ZWK,MAXIND,ZND1,ZND2,ZZBASE,IBASE,ZZDROP,IDROP,
     >     XLAB,YLAB,FLAB,XLAB(3),YLAB(3),FLAB(3),ZPROG,
     >     0,IDEV,ISHOT,0,ISHOT,1)
      ENDIF
 
      IF(IBASE.EQ.0) GO TO 65
      ZBASE=AMIN1(ZBASE,ZZBASE)
      NBASE=NBASE+IBASE
C
 65   CONTINUE
      IF(IDROP.EQ.0) GO TO 70
      ZDROP=AMIN1(ZDROP,ZZDROP)
      NDROP=NDROP+IDROP
C
 70   CONTINUE
C
C  VALIDATE THAT DATA WAS CHANGED BEFORE SETTING SMOOTHING FLAG
C
        DO ID=1,NUMD
          IF(F(ID).NE.FF(ID)) GO TO 72
        ENDDO
        GO TO 73
 72     CONTINUE
        IF(IPROC.LE.1) IPROC=IPROC+2
        ISM=1
 73     CONTINUE
C
      CALL UCOPYF(FF,F,NUMD)
      GO TO 50
C------
 80   CONTINUE
C
      IF (.NOT. LSTORED)  THEN
         WRITE(LUNTRM,9191)
         CALL UPAUST(1)
         GO TO 50
      ENDIF  ! LSTORED
 
C  UNITS TRANSFORMS
      ITOT=NRETX*NRETY
      CALL UCHUNS(
     >'F    ',FLAB(3),IFUNTC,ZFUNS,F,ITOT,ZFCONA,ZFCONB,2)
      IFUNOL=MAX0(IFUNOL,IFUNTC)
      CALL UCOPYF(F,FF,NUMD)
      CALL UCHUNS(
     >'X    ',XLAB(3),IXUNTC,ZXUNS,X,NRETX,ZXCONA,ZXCONB,1)
      IXUNOL=MAX0(IXUNOL,IXUNTC)
      CALL UCHUNS(
     >'Y    ',YLAB(3),IYUNTC,ZYUNS,Y,NRETY,ZYCONA,ZYCONB,1)
      IYUNOL=MAX0(IYUNOL,IYUNTC)
C
      CALL UFAXCOM(ILUNT,IXUNTC,ZXCONA,ZXCONB,IYUNTC,ZYCONA,ZYCONB,
     >     IFUNTC,ZFCONA,ZFCONB)
C
      GO TO 50
C------
 90   CONTINUE
C  INTERCHANGE DATA DIMENSIONS
 
      IF (.NOT. LSTORED)  THEN
         WRITE(LUNTRM,9191)
         CALL UPAUST(1)
         GO TO 50
      ENDIF  ! LSTORED
 
 
      CALL UFSWXY(F,FF,NUMD,X,NRETX,Y,NRETY,XLAB,YLAB)
      LSWCH=.NOT.LSWCH
C
      ITEM=IXUNOL
      IXUNOL=IYUNOL
      IYUNOL=ITEM
C
      ZTEMP=ZXUNS0
      ZXUNS0=ZYUNS0
      ZYUNS0=ZTEMP
C
      ZTEM=ZXCONA
      ZXCONA=ZYCONA
      ZYCONA=ZTEM
C
      ZTEM=ZXCONB
      ZXCONB=ZYCONB
      ZYCONB=ZTEM
C
      GO TO 50
C------
 100  CONTINUE
C
      IF(ISM.EQ.0) THEN
        WRITE(LUNTRM,9801)
 9801   FORMAT(' % GSMOO2 - UNSMOOTHED DATA WRITTEN')
        CALL UCOPYF(F,FF,NUMD)
      ENDIF
C
      CALL UFCMPR(ILUNO,ICOMPR)
      CALL UFOPWR(ILUNO,ISHOT,IER)
      IF(IER.NE.0) CALL UCLEAR
      IF(IER.NE.0) GO TO 290
C
      CALL UF2DWR(ILUNO,IDEV,ZSDATE,FF,NRETX,X,NRETX,Y,NRETY,IPROC,
     >          FLAB,XLAB,YLAB,NUMSC,XTRVAL,XTRLAB,IER)
C
C  APPEND COMMENTS TO FILE
C
      IF(IER.NE.0) CALL UCLEAR
      IF(IER.NE.0) GO TO 200
C
      CALL UFSNAM(ISHOT,ZFILE,IER,ILUNI)
      WRITE(ILUNT,4001) ZFILE
 4001 FORMAT('    PRODUCED BY PROGRAM GSMOO2 [TRANSP] '/
     >     ' >> DATA FROM INPUT FILE:  "',A,'"')
C
      if(lxrev) then
         write(ilunt,4201) xrlab
 4201    format(' '/' gsmoo1:'/
     >  1x,a,' ** order reversed to monotonic increasing **'/' ')
      endif
C
      if(lyrev) then
         write(ilunt,4201) yrlab
      endif
C
      IF(NDROP.EQ.0) GO TO 140
      WRITE(ILUNT,4003) NDROP,ZDROP
 4003 FORMAT('  ',I5,'  SPIKE PTS DROPPED; MIN. HEIGHT= ',1PE10.3)
 140  CONTINUE
      IF(NBASE.EQ.0) GO TO 150
      WRITE(ILUNT,4004) NBASE,ZBASE
 4004 FORMAT('  ',I7,' PTS RESET TO BASELINE.  BASELINE = ',1PE10.3)
C
 150  CONTINUE
      IF(LSWCH) WRITE(ILUNT,4011)
 4011 FORMAT(' DATA "X" AND "Y" AXES INTERCHANGED FROM INPUT DATA')
      IF(IXUNOL.EQ.1) WRITE(ILUNT,4012) ZXUNS0,XLAB(3)
      IF(IYUNOL.EQ.1) WRITE(ILUNT,4013) ZYUNS0,YLAB(3)
      IF(IFUNOL.EQ.1) WRITE(ILUNT,4014) ZFUNS0,FLAB(3)
 4012 FORMAT(' "X" ARRAY UNITS TRANSFORMED: "',A10,'" TO "',A10,'"')
 4013 FORMAT(' "Y" ARRAY UNITS TRANSFORMED: "',A10,'" TO "',A10,'"')
 4014 FORMAT(' "F" ARRAY UNITS TRANSFORMED: "',A10,'" TO "',A10,'"')
      IF(ISM.GT.0) WRITE(ILUNT,4005) ZPROG
 4005 FORMAT('  SMOOTHING ALGORITHM NAME:  ',A10)
C  USER COMMENTS
      CALL UFACOM(ILUNT)
C
      WRITE(ILUNT,4007) IPROCP,ZDATE
 4007 FORMAT(
     > /' =========================================================='/
     >  ' COMMENTS SPOOLED FROM FILE INPUT TO GSMOO2 [IPROC=',I1,']:'/
     >  ' ON VAX-11 ON DATE:  ',A10/)
C
C  APPEND COMMENTS GENERATED IN THIS RUN OF GSMOO:
      CALL UCSEND(ILUNT,ILUNO)
C  APPEND COMMENTS SPOOLED FROM THE INPUT FILE
      CALL USPOOL(ILUNI,ILUNO)
C
 200  CONTINUE
      CALL UFCLOS(ILUNO)
 290  CONTINUE
      CALL UFCLOS(ILUNI)
      GO TO 20
C-----
 300  CONTINUE
      CALL UFSCAD(ILUNO,ISHOT,MAXSC,NUMSC,XTRVAL,XTRLAB,IORDER,ILUNT)
      GO TO 50
C
C-----  Options A&B.
 400  CONTINUE
      IFLCUT=1
      GO TO 410
 405  CONTINUE
      IFLCUT=2
C
 410  CONTINUE
      IF(LXOUT(0)) WRITE(LUNTRM,5410) XLAB(1), YLAB(1)
 5410 FORMAT(' % DATA CUT UP ROUTINES ARE AS IN "GSMOO1" -- THEY'/
     >  '   OPERATE ON A 1D SLICE OF DATA.  ENTER "G" TO PLOT DATA.'/
     1  '    SLICE DIMENSION 1 => SLICE ALONG ', A/
     2  '    SLICE DIMENSION 2 => SLICE ALONG ', A/)
      CALL UREAD('GSMOO2:  SPECIFY SLICE DIMENSION ("1" OR "2"):$')
      IF(LCHAR('G')) GO TO 53  ! GRAPH
      IF(LCHAR('Q')) GO TO 50  ! RETREAT
      IF(LCHAR('1')) THEN
        IDIM=1
      ELSE IF(LCHAR('2')) THEN
        IDIM=2
      ELSE
        CALL UERMSG('OPTIONS ARE 1, 2, G, OR Q',' ',0)
        GO TO 410
      ENDIF
C
      IDIMC=3-IDIM
 411  CONTINUE
      IF(LXOUT(0)) THEN
           IF (IDIM .EQ. 1)  THEN
              YMIN = Y(1)
      	YMAX = Y(NRETY)
           ELSE   !
              YMIN = X(1)
      	YMAX = X(NRETX)
           END IF  ! IDIM
 
             WRITE(LUNTRM,5411) ZLABLS(IDIMC), YMIN, YMAX
      END IF  ! LXOUT(0)
 5411 FORMAT(' GSMOO2%: SELECT SLICE AT FIXED COORDINATE ',A30/
     1         '         RANGE = (', 1PE12.4,',', 1PE12.4, ')')
C
      CALL UREAD(
     >'GSMOO2:  SPECIFY FIXED COORD (NEAREST AVAIL. SLICE CHOSEN):$')
C
      IF(LCHAR('G')) GO TO 53  ! GRAPH
      IF(LCHAR('Q')) GO TO 50  ! RETREAT
C
      ZFIXD=FDCOD(IERT)
      IF(IERT.NE.0) GO TO 411
C
      IFIX=0
      CALL GET1FF(IDIM,IFIX,ZFIXD,X,NRETX,Y,NRETY,F,NRETX,ZWK,JDIND)
C
      IDIMC = 3-IDIM
      WRITE (TLABEL, 9112) ZFIXD, ZLABLS(IDIMC)(21:30)
 9112   FORMAT(' AT ', 1PE12.4, 1X, A)
 
      IF(IFLCUT.EQ.1) THEN
        CALL DCUTP3(XY(1,IDIM),ZWK(1,1),ZWK(1,2),INRET(IDIM),
     >                FLAB,ZLABLS(IDIM),ZSHID//TLABEL)
      ELSE IF(IFLCUT.EQ.2) THEN
        CALL DCUTUP(XY(1,IDIM),ZWK(1,1),ZWK(1,2),INRET(IDIM),
     >                FLAB,ZLABLS(IDIM),ZSHID//TLABEL)
      ENDIF
C
      CALL PUT1F2(IDIM,IFIX,F,NRETX,NRETY,NRETX,ZWK,JDIND)
C
      GO TO 50
C-----------------------------
C
C-----  Option C - AUTOMATIC 2-D DEGLITCHING
 
 500  CONTINUE
      IF (LSTORED)  THEN
C	    .COPY INPUT DATA INTO FF ARRAY, SINCE THERE HAS BEEN NO DEGLICHING.
          CALL UCOPYF(F,FF,NUMD)
      ENDIF  ! STORED
 
 
  520 CONTINUE
      IDIMC = 3-IDIM
 
C                                             ! SETUP VARIABLES FOR DCUTA2
      CALL DCUTST(       IDIM,
     1                     XY(1,IDIM), XY(1,IDIMC),
     1                     FLAB, ZLABLS(IDIM), ZLABLS(IDIMC),
     2			   ZLABLS,
     2                     INRET(IDIM), INRET(IDIMC),
     3                     ZGSIZE, ZGSIZ2,
     1		           ZXMIN,
     2			   ZXMAX,
     3                     ZFIXL,
     4                     ZFIXU,
     3	                   IWIDTH,
     4			   IMETHOD,
     5			   LLIST,
     6			   LDEGLITCH,
     7                     IALGOR,
     8                     CRETURN)
 
      IF (CRETURN .EQ. 'Q')  GO TO 50
 
 
 
 
      	IF (CRETURN .EQ. 'E')  THEN   ! GRAPH THE STORED DATA & DEGLITCHED,
         IF (LSTORED)  THEN
             WRITE (LUNTRM, 6010)
 6010     FORMAT(/ ' % GSMOO2: - NO DEGLITCHED DATA TO GRAPH'/
     1               '             USE "G" OPTION' )
             CALL UPAUST(1)
         ELSE    ! GRAPH THE DEGLITCHED DATA.
            ZLABEL = 'DEGL ' // FLAB(1)
            ZTIT45 = ' GSMOO2: ' // ZDATE // '  ' // ZSHID
            FFLAB(1) = 'DEGLITCHED'
              FFLAB(2) = FLAB(1)
              FFLAB(3) = FLAB(3)
            CALL GRF3F2(X,FF,F,Y,ZWK,NRETX,NRETY,NRETX,
     >                  XLAB,FFLAB,FLAB,YLAB,
     1                  XLAB(3),FLAB(3),YLAB(3),
     >                  ZTIT45, ZLABEL, IPROC)
            GOTO 520
         ENDIF   ! LSTORED
      ENDIF  ! 'G'
 
 
 
      IF (CRETURN .EQ. 'F')  THEN
         IF (LSTORED)  THEN
             WRITE (LUNTRM, 6021)
 6021     FORMAT(/ ' % GSMOO2: - NO DEGLITCHED DATA TO GRAPH'/
     1               '             USE "G" OPTION' )
             CALL UPAUST(1)
         ELSE    ! GRAPH THE DEGLITCHED DATA.
             CALL GRF3(X,FF,Y,ZWK,NRETX,NRETY,NRETX,
     >                   XLAB,YLAB,FLAB,XLAB(3),YLAB(3),FLAB(3),
     >                   ZTITLE,1,0,0,IDEV,ISHOT,0,ISHOT,1,ISM)
         ENDIF
         GOTO 520
      ENDIF  ! 'F'
 
 
      IF (CRETURN .EQ. 'G')  THEN   ! GRAPH THE STORED DATA.
         CALL GRF3(X,F,Y,ZWK,NRETX,NRETY,NRETX,
     >               XLAB,YLAB,FLAB,XLAB(3),YLAB(3),FLAB(3),
     >               ZTITLE,1,0,0,IDEV,ISHOT,0,ISHOT,1,ISM)
         GOTO 520
      ENDIF  ! 'G'
 
 
      IF (CRETURN .EQ. 'S')  THEN    ! STORE THE DEGLITCHED DATA OVER INPUT DAT
 
         IF (LSTORED)  THEN
             WRITE (LUNTRM, 6011)
 6011        FORMAT(/ ' % GSMOO2: - NO DEGLITCHED DATA TO STORE'/)
             CALL UPAUST(1)
         ELSE
             CALL UCOPYF(FF, F, NUMD)
             LSTORED = .TRUE.
         END IF  ! LSTORED
 
         GOTO 520
      ENDIF  ! 'S'
 
 
      IF (CRETURN .EQ. 'R')  THEN  ! RESET THE STORED DATA INTO THE DEGL ARRAYY
         IF (LSTORED)  THEN
             WRITE (LUNTRM, 6041)
 6041        FORMAT(/ ' % GSMOO2: - NO DEGLITCHED DATA TO RESET'/)
             CALL UPAUST(1)
         ELSE
             CALL UCOPYF(F,FF,NUMD)
             LSTORED = .FALSE.
         ENDIF
         GOTO 520
      ENDIF  ! 'R'
 
 
      IF (CRETURN .NE. 'D')  GO TO 520
 
C	.START DEGLITCHING ONE SLICE AT A TIME.  ------------------------------
 
      IDIMC = 3 - IDIM
 
C
      CALL GETNDX(IDIM,IFIXL,ZFIXL,X,NRETX,Y,NRETY)   ! GET LOWER INDEX
      CALL GETNDX(IDIM,IFIXU,ZFIXU,X,NRETX,Y,NRETY)   ! GET UPPER INDEX
C
      NTIMES = 0     ! # OF TIMES THROUGH LOOP.
      ITOTFND= 0
 
      DO 530 IFIX=IFIXL,IFIXU
         NTIMES = NTIMES+1
         CALL GET1F2(IDIM, IFIX, FF, NRETX, NRETY, NRETX, ZWK,
     1                 JDIND)
	
         CALL DCUTA2(XY(1,IDIM),ZWK(1,1),ZWK(1,2),INRET(IDIM),
     >                FLAB,ZLABLS(IDIM),ZSHID, IALGOR, NTIMES,
     1                ITOTFND, XY(IFIX,IDIMC), ZLABLS(IDIMC),
     2                     ZGSIZE,   ZGSIZ2,
     1		           ZXMIN,
     2			   ZXMAX,
     3	                   IWIDTH,
     4			   IMETHOD,
     5			   LLIST,
     6			   LDEGLITCH)
 
         IF (NTIMES.EQ.0)  GO TO 520   ! No deGLITCHing was done "Q" option.
C
 
         CALL PUT1F2(IDIM,IFIX,FF,NRETX,NRETY,NRETX,ZWK,JDIND)
 
  530   CONTINUE
 
      IF(LXOUT(0)) THEN
            WRITE(LUNTRM,5515) NTIMES, ITOTFND
 5515     FORMAT( / ' %GSMOO2: AFTER ', I5,
     1                ' SLICES, # OF DEGLITCHED PTS = ', I6/)
          CALL UPAUST(1)
      ENDIF ! LXOUT(0)
C
      LSTORED = .FALSE.
      GO TO 520
 
C----------------------------------------------
 600  continue
      if(lxout(0)) then
         write(luntrm,*) 
     >        ' Note: renormalization applied to unsmoothed data;'
         write(luntrm,*)
     >        '       smoothed data is not kept.'
         write(luntrm,*) '   F is: ',flab(1:2),' ',flab(3)
         write(luntrm,*) '   X is: ',xlab(1:2),' ',xlab(3)
         write(luntrm,*) '   Y is: ',ylab(1:2),' ',ylab(3)
         write(luntrm,*) ' '
         write(luntrm,*) ' options: '
         write(luntrm,*) ' '
         write(luntrm,*) 
     >        ' "X" -- read 1d Ufile: X-dependent renormalization'
         write(luntrm,*)
     >        '        factor R(X): F(X,Y) -> R(X)*F(X,Y)'
         write(luntrm,*) 
     >        ' "Y" -- read 1d Ufile: Y-dependent renormalization'
         write(luntrm,*)
     >        '        factor R(Y): F(X,Y) -> R(Y)*F(X,Y)'
         write(luntrm,*)
     >        ' "Q" -- quit (do nothing here).'
      endif

      call uread('gsmoo2 renorm option X/Y/Q:')
         
      if(lchar('Q')) then
         go to 50
      else if(lchar('X')) then
         call ufnter(ilunx,'R(X) renormalizer   ')
         idimr=1
      else if(lchar('Y')) then
         call ufnter(ilunx,'R(Y) renormalizer   ')
         idimr=2
      else
         call uermsg(' invalid renorm option ',' ',0)
         go to 50
      endif
C
      jshot=ishot
      call ureadi('shot number for renormalization Ufile:',jshot)
C
      call uf1f2_renorm(x,nretx,y,nrety,f,ilunx,jshot,jdind,zwk,idimr,
     >     luntrm,
     >     ier)
C
      if(ier.ne.0) then
         call uermsg(' renormalization attempt failed. ',' ',0)
      else
         ism=1
         iproc=1
         ff(1:nretx*nrety) = f(1:nretx*nrety)
      endif

      go to 50
C----------------------------------------------
 
C  PROGRAM EXIT
 900  CONTINUE
      CALL NEUPAG
      CALL UEXITA(0)
      STOP
      	END
C******************** END FILE GSMOO2.FOR ; GROUP GSMOO2 ***************
