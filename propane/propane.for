C******************** START FILE PROPANE.FOR ; GROUP PROPANE ***********
C---------------------------------------------------------
C  PROPANE
C
C  PROFILES WITHOUT PAIN
C
C  PROFILE GENERATOR
C
C  MAKE 1D PROFILES VS. MINOR RADIUS
C
C  dmc june 5 1992 -- unix portability + functional enhancement
C
      PROGRAM PROPANE
C
      PARAMETER (MAXSC=100)
C
      PARAMETER (NPTSA=128)
      PARAMETER (NPTS=10240)
C
      PARAMETER (NTYPS=20)
C
      REAL XXX(NPTS),DATA(NPTS),TIME(2),DATA2(2,NPTS),SCVAL(MAXSC)
C
      INTEGER IDTYP(NTYPS)
C
      CHARACTER*30 TIMLAB
      CHARACTER*30 RADLAB
      CHARACTER*30 DATLAB(NTYPS)
      CHARACTER*30 ZXLAB
      CHARACTER*10 SCLAB(3,MAXSC),ZKEY
C
      CHARACTER*3 FILEXT(NTYPS)
      CHARACTER*1 FILPRE(NTYPS),char1
      CHARACTER*4 TOK,YEAR
      CHARACTER*10 SDATE
      CHARACTER*64 DIR,ZDIR
      CHARACTER*16 DISK
C
C  IDTYP=1:  PROFILE (VS. RADIUS) DATA
C  IDTYP=2:  TIME SEQUENCE DATA
C  IDTYP=3:  SAWTOOTH TIMES
C
C  EXECUTABLE CODE
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
      LOGICAL LYESC
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
      DATA ZDIR/' '/
C
      DATA IDTYP/1,1,1,1,2,2,2,2,2,2,2,2,1,1,2,3,2,1,2,0/
      DATA FILPRE/'F','F','F','F','F','F','F','F','F','F','F','F',
     >	      'F','F','F','F','F','F','F','F'/
      DATA FILEXT/'TER','NER','ZFR','PBO','CUR','VSF','NEL',
     >              'ZFT','TPI','RPL','APL','TI0','TIR','XKR','VPH',
     >	            'SAW','ZPL','BPB','RBZ','???'/
      DATA RADLAB/'RADIUS    (PROPANE) CM        '/
      DATA TIMLAB/'TIME (PROPANE)      SECONDS   '/
      DATA DATLAB/'ELECTRON TEMPERATUREEV        ',
     >              'ELECTRON DENSITY    N/CM**3   ',
     >              'ZEFF PROFILE                  ',
     >              'POWER RADIATED      W/CM**3   ',
     >              'PLASMA CURRENT      AMPS      ',
     >              'SURFACE VOLTAGE     VOLTS     ',
     >              'LINE DENSITY        N/CM**2   ',
     >              'ZEFF EVOLUTION                ',
     >              'ION RECYCLING TIME  SECONDS   ',
     >              'MAJOR RADIUS        CM        ',
     >              'MINOR RADIUS        CM        ',
     >		    'ION TEMPERATURE     EV        ',
     >              'Ti PROFILE DATA     EV        ',
     >              'XKFAC PROFILE DATA            ',
     >              'AXIAL ROTATION      CM/SEC    ',
     >              'SAWTOOTH EVENT NO.  INDEX     ',
     >		    'PLASMA ELEVATION (Z)CM        ',
     >              'B FIELD TILT BP/BT            ',
     >              'R*Bz                cm*Tesla  ',
     >              '??????????????????? ????????? '/
      DATA TIME/0.0,10.0/
      DATA NUMT/2/
      DATA LUNO/30/
C
      DATA GAMMA/1.0/
C
C => END DATA BLOCK
C ====================
C
      call ufheader("propane")
      CALL UINITG("PROPANE")
C
      LUNT=LNOUT(0)
C
      iflg=0
C
 10   CONTINUE
      CALL NEUPAG
      IF(LXOUT(0)) WRITE(LUNT,1000)
 1000 FORMAT(/' ------------ '/' STARTING NEW DATASET.')
      CALL UREADI(
     >'PROPANE:  ENTER SHOT NO. FOR OUTPUT FILE, 0 TO QUIT:$$',NSHOT)
      IF(NSHOT.EQ.0) THEN
        CALL NEUPAG
        CALL UEXITA(0)
        STOP
      ENDIF
      IF((NSHOT.LT.1000).OR.(NSHOT.GT.999999)) THEN
        CALL UERMSI('% SHOT NO. OUT OF RANGE 1000 TO 999999',NSHOT,0)
        GO TO 10
      ENDIF
C
      CALL UREADW(
     >'PROPANE:  ENTER TOKAMAK LABEL (E.G. "PBX"):$$',TOK)
      IF(TOK.EQ.'Q   ') GO TO 10
C
      CALL UREADW('PROPANE:  ENTER SHOT YEAR (LABEL):$$',YEAR)
      SDATE=' .... '//YEAR
C
      DISK=' '
C
      IF(LXOUT(0)) WRITE(LUNT,
     >   '('' %PROPANE OUTPUT DIRECTORY UNDER USER CONTROL'')')
      CALL UNIXPATH(ZDIR,DIR,ILDIR)
      IF(ILDIR.EQ.0) DIR='D '
      IF(DIR(1:2).EQ.'D ') THEN
         DIR=' '
      ELSE
        ilast=index(dir,' ')-1
        if(dir(ilast:ilast).ne.'/') then
          ilp1=ilast+1
          dir(ilp1:ilp1)='/'
        endif
      ENDIF
C
 20   CONTINUE
      CALL NEUPAG
      if(iflg.eq.1) then
        iflg=0
        idtyp(id)=0
        filext(id)='???'
      endif
C
      IF(LXOUT(0)) THEN
        IBLD=MAX0(2,(INDEX(DIR,' ')-1))
C
        WRITE(LUNT,1001) DIR(1:IBLD-1),NSHOT
 1001 FORMAT(/'  OUTPUT DIRECTORY = ',A,' SHOT ##### =',I6/
     >    ' choose filename F#####.??? to gain control of labeling'/
     >    ' SELECT  DATA                 UNITS      FILENAME'/
     >    ' ------  -------------------- ---------- ----------')
        DO ID=1,NTYPS
          WRITE(LUNT,1002) ID,DATLAB(ID)(1:20),DATLAB(ID)(21:30),
     >                          FILPRE(ID),FILEXT(ID)
        ENDDO
 1002 FORMAT(4X,I2,3X,A20,1X,A10,1X,A1,'#####.',A3)
      ENDIF
C
      CALL UREAD('PROPANE:  ENTER DATA SELECTION, "Q" TO ABORT:$$')
C
      IF(LCHAR('Q')) GO TO 10
C
      ID=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 20
      IF((ID.LT.1).OR.(ID.GT.NTYPS)) THEN
        CALL UERMSI('ILLEGAL DATA SELECTION ',ID,2)
        GO TO 20
      ENDIF
C
      iflg=0
      if(idtyp(id).eq.0) then
        iflg=1
 25     continue
c
        if(lxout(0)) write(lunt,3001)
 3001   format(/
     >' user controlled file type and labeling detected!'/
     >' choose "1" for timeseries, "2" for profile data:')
        call uread('PROPANE:  choose file type (1 or 2 or "Q"):$')
        if(lchar('Q')) then
          go to 20
        else if(lchar('1')) then
          idtyp(id)=2
        else if(lchar('2')) then
          idtyp(id)=1
        else
          call uermsg('propane:  unrecognized file type spec',' ',0)
          go to 25
        endif
c
        call ureadw('PROPANE:  give 3 letter filename .EXT:$',
     >      filext(id))
c
        call ureadl('PROPANE:  give 20 character data label:$',
     >      datlab(id)(1:20))
c
        call ureadl('PROPANE:  give 10 character units label:$',
     >      datlab(id)(21:30))
c
        if(idtyp(id).eq.1) then
          write(lunt,3002)
        else
          write(lunt,3003)
        endif
 3002   format(' data type:    profile')
 3003   format(' data type:    time series')
        write(lunt,3004) filpre(id),filext(id)
        write(lunt,3005) datlab(id)(1:20)
        write(lunt,3006) datlab(id)(21:30)
 3004   format(' filename:     ',a1,'#####.',a3)
 3005   format(' data label:   ',a)
 3006   format(' units label:  ',a)
c
        call uread(
     >'PROPANE:  enter "Y" to confirm this is what you want:$')
c
        if(.not.lyesc('Y')) go to 25
      endif
C
C  OPEN FILE
C
      CALL UFSETR(LUNO,FILPRE(ID),FILEXT(ID),DISK,DIR)
C
C  SPECIFY DATA
C
      IF(IDTYP(ID).EQ.1) THEN
        ZXLAB=RADLAB
      ELSE
        ZXLAB=TIMLAB
      ENDIF
C
 30   CONTINUE
      IPIECE=1
      IF(IDTYP(ID).EQ.1) THEN
C  RADIAL DATA-- ANALYTIC OR PIECEWISE OPTION
        if(lxout(0)) then
           write(lunt,*) ' enter "1" for analytic profile over [0:1]...'
        endif
        CALL UREADC(
     >    'PROPANE:  TYPE "A" FOR ANALYTIC PROFILE OPTIONS:$',char1)
        if(char1.eq.'1') then
           ipiece=-1
        else if(char1.eq.'A') then
           IPIECE=0
        endif
      ELSE IF(IDTYP(ID).EQ.3) THEN
C  SAWTOOTH EVENT TIME SPECIFICATION
        ZINTVL=-1.E38
        INTVL=0
        IEVENT=0
        IF(LXOUT(0)) THEN
          CALL NEUPAG
          WRITE(LNOUT(0),2011)
 2011 FORMAT(/
     >' "SAWTOOTH TIME RANGES" ARE EXTENDED TIME PERIODS DURING WHICH'/
     >' SAWTEETH OCCUR.  WITHIN EACH TIMERANGE A SAWTOOTH "PERIOD"'/
     >' IS TO BE SPECIFIED INDICATING TIME DURATION BTW SAWTOOTH '/
     >' EVENTS.  *NOTE* SAWTOOTH TIME RANGE BDY''S MUST BE SPECIFIED'/
     >' IN ASCENDING ORDER.  ALSO A TIME RANGE IS NOT CONSIDERED'/
     >' TO CONTAIN SAWTOOTH EVENTS UNLESS *BOTH* A START AND STOP'/
     >' TIME HAVE BEEN SPECIFIED'/)
        ENDIF
C
 202    CONTINUE
        IF(LXOUT(0)) WRITE(LNOUT(0),2012) IEVENT
 2012 FORMAT(1X,I4,' SAWTOOTH EVENTS CURRENTLY ENTERED')
        CALL UREAD(
     >	    'PROPANE:  ENTER SAWTOOTH TIME RANGE START/STOP TIME:$$')
        IF(LCHAR('Q')) GO TO 208
        Z=FDCOD(IERT)
        IF(IERT.NE.0) GO TO 202
        IF(Z.LE.ZINTVL) THEN
          CALL UERMSF(' ?BDY TIME OUT OF ORDER, LAST TIME=',ZINTVL,1)
          GO TO 202
        ENDIF
C
        IF(INTVL.GT.0) THEN
C
          IF((Z-ZINTVL)*1.E-3.GT.ZSPER) THEN
            CALL UERMSF(
     >' ?SAWTOOTH PERIOD TOO SHORT (.GT. 1000 IN RANGE)',ZSPER,1)
            GO TO 205
          ENDIF
C
          IF(INTVL.EQ.1) ZEVENT=ZINTVL-ZSPER
 203      CONTINUE
          IEVENT=IEVENT+1
          ZEVENT=ZEVENT+ZSPER
          IF(ZEVENT.GT.Z) THEN
            IEVENT=IEVENT-1
            ZEVENT=ZEVENT-ZSPER
            GO TO 204
          ENDIF
          XXX(IEVENT)=ZEVENT
          DATA(IEVENT)=FLOAT(IEVENT)
          GO TO 203
        ENDIF
C
 204    CONTINUE
C
        INTVL=INTVL+1
        ZINTVL=Z
C
 205    CONTINUE
        CALL UREAD(
     >'PROPANE:  ENTER SAWTOOTH PERIOD DURING TIMERANGE OR "Q":$$')
        IF(LCHAR('Q')) GO TO 208
        ZSPER=FDCOD(IERT)
        IF(IERT.NE.0) GO TO 205
        IF(ZSPER.LE.0.0) THEN
          CALL UERMSF('?SAWTOOTH PERIOD .GT. 0.0 REQUIRED',ZSPER,1)
          GO TO 205
        ENDIF
C
        GO TO 202
C
 208    CONTINUE
        IF(IEVENT.EQ.0) GO TO 10
        NUMR=IEVENT
        GO TO 34
C
      ELSE
C  TIME SERIES DATA-- ANALYTIC TIME SERIES OPTION:
        CALL UREAD(
     >    'PROPANE:  TYPE "A" FOR ANALYTIC TIME SERIES OPTIONS:$')
        IF(LYESC('A')) THEN
          CALL PRPTSA(XXX,DATA,NPTSA,NUMR)
          IF(NUMR.EQ.0) GO TO 30
          GO TO 34
        ENDIF
      ENDIF
C
      IF(IPIECE.EQ.1) GO TO 32
      if(ipiece.eq.0) then
         CALL NEUPAG
         IF(LXOUT(0)) WRITE(LUNT,1010)
 1010    FORMAT(/
     >'  DATA COVERS R RANGE 0 TO RMAX (CM); RECOMMEND RMAX SLIGHTLY'/
     >'  GREATER THAN PLASMA RADIUS'//
     >'   DATA(R) = ALPHA*[ 1 - GAMMA*(R/RMAX)**X1 ]**X2 + BETA'/)
C
         CALL UREADF('PROPANE:  ENTER RMAX:$$',RMAX)
         CALL UREADF('PROPANE:  ENTER ALPHA:$$',ALPHA)
         CALL UREADF('PROPANE:  ENTER BETA:$$',BETA)
         CALL UREADF('PROPANE:  ENTER GAMMA:$$',GAMMA)
         CALL UREADF('PROPANE:  ENTER X1:$$',X1)
         CALL UREADF('PROPANE:  ENTER X2:$$',X2)
C
         NUMR=NPTSA
         DO IR=1,NUMR
            R=FLOAT(IR-1)*RMAX/FLOAT(NUMR-1)
            XXX(IR)=R
            DATA(IR)=ALPHA*(1.0-GAMMA*(R/RMAX)**X1)**X2+BETA
         ENDDO
         GO TO 34
      else
         if(lxout(0)) write(lunt,1011)
 1011    format(/
     >        '  data P(x) covers x range from 0 to slightly over 1:'/
     >        '    P(x) = P0 + (P1-P0)*f(x)'/
     >        '           P0 = axial value, @x=0'/
     >        '           P1 = edge value, @x=1'/
     >      '    f(x) = x**alpha*[1 - (beta-alpha)*(x**gamma-1)/gamma]'/
     >        '       with constraints alpha.ge.1,  gamma.gt.0,'/
     >        "       and properties f(0)=0, f'(0)=0 if alpha.gt.1;"/
     >        "                      f(1)=1, f'(1)=beta.'")
C
         call ureadf('propane:  enter P0:',P0)
         call ureadf('propane:  enter P1:',P1)

         alpha=2.0
         call ureadf('propane:  enter alpha:',alpha)
         if(alpha.lt.1.0) then
            write(lunt,*) ' alpha reset to minimum (1.0).'
            alpha=1.0
         endif

         beta=0.0
         call ureadf('propane:  enter beta:',beta)

         gamma=1.0
         call ureadf('propane:  enter gamma:',gamma)
         if(gamma.lt.1.0e-3) then
            write(lunt,*) ' gamma reset to minimum (1.0e-3)'
            gamma=1.0e-3
         endif

         numr=nptsa
         do ir=1,numr
            x=(ir-1)*1.025/(numr-1)
            xxx(ir)=x
            if(ir.eq.1) then
               fx=0.0
            else
               fx=x**alpha*(1.0+(beta-alpha)*(x**gamma-1.0)/gamma)
            endif

            data(ir)=P0 + (P1-P0)*fx
         enddo
         go to 34
      endif
C
C  PIECEWISE PROFILE
C
 32   CONTINUE
C
      NUMR=0
      INUMI=1
      CALL PCIDEF(ZZZX,ZZZY,INUMI,XXX,DATA,NUMR,NPTS,ZXLAB,DATLAB(ID),
     >     ZXLAB(21:30),DATLAB(ID)(21:30))
      IF(NUMR.LE.1) THEN
        CALL UERMSI('? TOO FEW PTS SPECIFIED:',NUMR,2)
        GO TO 90
      ENDIF
C
 34   CONTINUE
      CALL GRFUF1(XXX,DATA,NUMR,4,DATLAB(ID),ZXLAB,NSHOT,TOK,
     >     SDATE,0,XXX,ZXLAB)
C
 35   CONTINUE
      NUMSC=0
 36   CONTINUE
      IF(LXOUT(0)) THEN
        WRITE(LUNT,1014) NUMSC
 1014 FORMAT(
     >' *** ENTER "S" TO DEFINE A SCALAR FOR OUTPUT FILE (',I2,
     >' CURRENTLY DEFINED)')
        IF(IDTYP(ID).EQ.1) WRITE(LUNT,1015)
 1015 FORMAT(' ENTER 1 FOR 1D UFILE, 2 FOR 2D UFILE (2 TIME PTS)'/
     >'  OR Q FOR NO OUTPUT')
        IF(IDTYP(ID).GE.2) WRITE(LUNT,1016)
 1016 FORMAT(' ENTER 1 FOR UFILE OUTPUT, Q FOR NO OUTPUT:')
      ENDIF
C
      CALL UREAD(
     >'PROPANE:  ENTER OUTPUT OPTION (1 2 S OR Q):$$')
      IOUT=IXUOPC('12SQ$')
      IF(IOUT.EQ.0) THEN
        CALL UERMSG('WHAT?',' ',0)
        GO TO 36
      ENDIF
C
      IF((IDTYP(ID).GE.2).AND.(IOUT.EQ.2)) IOUT=1
C
      IF(IOUT.EQ.3) THEN
C  FETCH A SINGLE SCALAR AND DISPLAY ON TERMINAL
        CALL UFGKEY(ZKEY)
C  SEARCH FOR MATCH IN LIST
        IF(NUMSC.GT.0) THEN
          ISC=IUFKEY(ZKEY,SCLAB,NUMSC)
          IF(ISC.NE.0) THEN
            CALL UERMSG(' % SCALAR ALREADY IN LIST',ZKEY,1)
            GO TO 36
          ENDIF
        ENDIF
        NUMSC=NUMSC+1
        SCLAB(1,NUMSC)=ZKEY
        CALL UREADW('SPECIFY KEYWORD SCALAR LABEL (MAX 10 CHARS):$',
     >                  SCLAB(2,NUMSC))
        CALL UREADW('SPECIFY KEYWORD SCALAR UNITS (MAX 10 CHARS):$',
     >                  SCLAB(3,NUMSC))
        CALL UREADF('SPECIFY KEYWORD SCALAR VALUE (FLOATING PT.):$',
     >                  SCVAL(NUMSC))
        GO TO 36
      ELSE IF(IOUT.EQ.4) THEN
        GO TO 10
      ELSE IF(IOUT.EQ.2) THEN
        CALL UREADF('ENTER TIME PT #1 (OF 2):$$',TIME(1))
        CALL UREADF('ENTER TIME PT #2:$$',TIME(2))
        DO IT=1,NUMT
          DO IR=1,NUMR
            DATA2(IT,IR)=DATA(IR)
          ENDDO
        ENDDO
C
      CALL UFOPWR(LUNO,NSHOT,IER)
      CALL UF2DWR(LUNO,TOK,SDATE,DATA2,NUMT,TIME,NUMT,XXX,NUMR,4,
     >     DATLAB(ID),TIMLAB,ZXLAB,NUMSC,SCVAL,SCLAB,IER)
C
      ELSE
C
      CALL UFOPWR(LUNO,NSHOT,IER)
      CALL UF1DWR(LUNO,TOK,
     >   SDATE,XXX,DATA,NUMR,4,ZXLAB,DATLAB(ID),NUMSC,SCVAL,SCLAB,IER)
C
      ENDIF
C
      IF(IPIECE.EQ.0) WRITE(LUNO,4001)
      IF(IPIECE.EQ.1) WRITE(LUNO,4002)
C
 4001 FORMAT(/
     >' PROFILE DATA GENERATED BY PROPANE ROUTINE'/
     >'  R = 0 TO RMAX  ',I4,' PTS'/
     >'  DATA = ALPHA*(1-(R/RMAX)**X1)**X2 + BETA'/
     >'    WHERE:'/
     >'  RMAX=',1PE11.4,'  ALPHA=',1PE11.4,'  BETA=',1PE11.4/
     >'  X1=',1PE11.4,'  X2=',1PE11.4/)
C
 4002 FORMAT(/' USER TYPED (X,Y) PAIRS - CF PROPANE ROUTINE'/)
C
 90   CONTINUE
      CALL UFCLOS(LUNO)
C
C
      GO TO 10
      END
C******************** END FILE PROPANE.FOR ; GROUP PROPANE *************


C******************** START FILE PRPTSA.FOR ; GROUP PROPANE ******************
C------------------------------------------------------------
C  PRPTSA - ANALYTIC TIME SERIES OPTION
C
      SUBROUTINE PRPTSA(T,F,NMAX,N)
C
      REAL T(NMAX),F(NMAX)
C
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C *LISMAK* DATA SATEMENT MOVED 12-APR-88
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LXOUT
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C => PRPTSA               BEGIN DATA BLOCK
C => LISMAK 12-APR-88  MOVED DATA STATEMENTS
      DATA PI/3.14159/
      DATA ZDELT/1.E35/
C => END DATA BLOCK
C ====================
C
      CALL NEUPAG
      LUNT=LNOUT(0)
 10   CONTINUE
      IF(LXOUT(0)) WRITE(LUNT,1101)
 1101 FORMAT(/' ANALYTIC TIME SERIES OPTIONS:'/
     >'  1:  f(t) = f0 + A * sin ( 2*pi*tt/tau )'/
     >'  2:  f(t) = f0 + A * exp ( -tt/tau )'/
     >'      where tt=min(max(0.0,(t-t0)),delt); i.e. t0<t<t0+delt'/
     >'  Q:  quit'/)
C
      CALL UREAD('PRPTSA:  SPECIFY TIMESERIES OPTION:$')
      IOP=IXUOPC('Q12$')
C
      IF(IOP.EQ.0) THEN
        CALL UERMSG('PRPTSA: OPCODE NOT UNDERSTOOD',' ',-1)
        GO TO 10
      ELSE IF(IOP.EQ.1) THEN
        N=0
        RETURN
      ENDIF
C
      CALL UREADF('ENTER F0:$$',ZF0)
      CALL UREADF('ENTER A:$$',ZA)
      CALL UREADF('ENTER T0:$$',ZT0)
      CALL UREADF('ENTER DELT:$$',ZDELT)
      ZDELT=AMAX1(0.0,ZDELT)
      CALL UREADF('ENTER TAU (0.0 breaks):$$',ZTAU)
      IF(ZTAU.EQ.0.0) GO TO 10
C
      N=0
      CALL UREADI(
     >'ENTER NUMBER OF PTS N IN OUTPUT(.LT 2 ABORTS TRACE):$$',N)
      IF(N.LE.1) GO TO 10
C
      IF(N.GT.NMAX) THEN
        CALL UERMSI('NUMBER ENTERED EXCEEDS MAX OF',NMAX,-1)
        GO TO 10
      ENDIF
C
      CALL UREADF('ENTER TIME RANGE (T1 TO T2)  T1=$$',ZT1)
      CALL UREADF('ENTER TIME RANGE             T2=$$',ZT2)
C
      DO 100 I=1,N
        T(I)=ZT1+(I-1)*(ZT2-ZT1)/(N-1)
        ZT=AMIN1(ZDELT,AMAX1(0.0,(T(I)-ZT0)))
        IF(IOP.EQ.2) THEN
          F(I)=ZF0+ZA*SIN(2.0*PI*ZT/ZTAU)
        ELSE IF(IOP.EQ.3) THEN
          F(I)=ZF0+ZA*EXP(-(ZT/ZTAU))
        ENDIF
 100  CONTINUE
C
      RETURN
      END
C******************** END FILE PRPTSA.FOR ; GROUP PROPANE ******************
