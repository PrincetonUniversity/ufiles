C******************** START FILE TVGETM.FOR ; GROUP CONCAT *************
C------------------------------------------------------------
C  TVGETM
C
C  READ INPUT FILE AS SPECIFIED BY DEFAULTS SETTINGS AND
C  USER SUPPLIED SHOT NUMBER
C
C  INIT=1 ==> DEFAULTS HAVE BEEN SET
C      =0 ==> ERROR
C
C  PROFILES ARE STORED IN INPUT BUFFER.  CAPACITY 10000 WORDS.
C    NS=# OF SHOTS CURRENTLY IN BUFFER
C    NX= # OF RADIAL PTS, DATA PTS PER SHOT.
C       RADIAL VECTOR MUST NOT CHANGE FROM SHOT TO SHOT
C
C
      SUBROUTINE TVGETM(INIT)
C
      USE UFILES
      USE CONCAT_MOD
C
      COMMON/ZSAVE/ ZXMAX,IOFF
C
      CHARACTER*20 ZBUF
      CHARACTER*40 ZFILE,zcbuff
      CHARACTER*16 ZEXT
C
      LOGICAL DBUGR
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES
C
      DATA DBUGR/.FALSE./
C
C ------------------------------------------
C
      luntrm=lnout(0)
      zext=' '
C
      IF(INIT.EQ.1) GO TO 9
      WRITE(LUNTRM,9001)
 9001 FORMAT(/' ? ERROR:  FILE IDENTIFICATION DEFAULTS NOT SET'/
     >          '   (TOP NODE OPTION CODE "R")'/)
      CALL UPAUST(1)
      RETURN
C
 9    CONTINUE
C  GET UFILES CHANNEL NUMBER
      CALL UFLCHK(LUNI,'TVGETM    ',ICHANL,IER)
C
 10   CONTINUE
      CALL NEUPAG
C
 11   CONTINUE
      INSI=NS
      IF(NSHOT2.GT.0) INSI=INSI-1
C
      IF(MODEI.EQ.1) THEN
C-------------------------/MODEI=1 INPUT SPECIFICATION
C
      IF(LXOUT(0)) THEN
        WRITE(LUNTRM,2001)
 2001 FORMAT(
     >  ' TVGETM:  ENTER SHOT # OF FILE TO READ INTO INPUT BUFFER'/
     >  ' ENTER NEGATIVE SHOT # TO DELETE PROFILE FROM INPUT BUFFER'/
     >  ' ENTER "P" TO PRINT INPUT BUFFER TABLE OF CONTENTS'/
     >  ' ENTER "X" OR "0" TO EXIT'/)
        IF(NS.EQ.0) THEN
          WRITE(LUNTRM,2010)
 2010 FORMAT(/
     >' %NO INPUT DATA HAS BEEN READ IN SO FAR.'//
     >' ENTER "S" TO INPUT STARTER 2D UFILE DATA INTO WHICH ADDITIONAL'/
     >'   PROFILES MAY BE INSERTED...'/
     >'   NOTE PROFILES MAY NOT BE DELETED FROM INPUT 2D UFILE DATA'/
     >'   WITH THIS PROGRAM -- USE THE EXTRAC UTILITY INSTEAD'/)
        ELSE IF(INSI.GT.0) THEN
          WRITE(LUNTRM,2013)
 2013   FORMAT(
     >' ENTER "Y" TO CHANGE KEY VALUE ASSOCIATED WITH 1D INPUT PROFILE')
        ENDIF
      ENDIF
	
C  FIXED FILENAME EXTENSION, VARIABLE SHOT
      IEXT=0
      IEXT1=1
      IEXT2=1
      ZEXT=UEXTND(ICHANL)
C
      CALL UREAD('TVGETM(CONCAT):  ENTER SHOT # OR OPCODE:$')
      IF(LCHAR('Q')) RETURN
C
      IF(NS.EQ.0) THEN
        IF(LCHAR('S')) THEN
          CALL TV2INP(ZXMAX,IOFF)
          GO TO 10
        ENDIF
      ENDIF
C
      IYFLG=0
      IF(INSI.GT.0) THEN
        IF(LCHAR('Y')) THEN
          CALL UREAD(
     >'TVGETM:  ENTER PROFILE SHOT NO. TO MODIFY, OR "P":$$')
          IYFLG=1
          IF(LCHAR('X')) GO TO 10
          IF(LCHAR('Q')) GO TO 10
        ENDIF
      ENDIF
      IOP=IXUOPC('PX$')
      IF(IOP.EQ.1) GO TO 500
      IF(IOP.EQ.2) RETURN
C
      INSHOT=IDCOD(IERT)
      IF(IERT.NE.0) GO TO 11
      IF(INSHOT.EQ.0) RETURN
      IF(INSHOT.LT.0) GO TO 300
C--------------------------------------------------------------
      ELSE
C------------------------/MODE 2 INPUT SPECIFICATION
C  SHOT NO. IS FIXED
      INSHOT=NSHOTO
      IF(LXOUT(0)) THEN
        WRITE(LUNTRM,2011) NS,NSHOTO
 2011 FORMAT(/' MODE 2 INPUT SPEC OPTIONS:'/
     >'  (NUMBER OF PROFILES READ SO FAR = ',I2,' SHOT NO. = ',I6,')'/
     >'  I - INPUT A SINGLE FILE (SPECIFY FILENAME .EXT)'/
     >'  J - INPUT SEQUENCE OF FILES (FILENAME .EXT Amm RANGE)'/
     >'  K - INPUT SEQUENCE OF FILES (longer .EXT AAAA_mmmm OK)'/
     >'  D - DELETE DATA FROM A SINGLE FILE (SPECIFY .EXT)'/
     >'  P - PRINT CONTENTS OF INPUT BUFFER ON TTY'/
     >'  Q - QUIT THIS NODE, RETURN TO MAIN')
        IF(NS.EQ.0) THEN
          WRITE(LUNTRM,2010)
        ELSE IF(INSI.GT.0) THEN
          WRITE(LUNTRM,2013)
        ENDIF
      ENDIF
      CALL UREAD('TVGETM(CONCAT):  ENTER MODE 2 INPUT OPCODE:$')
C
      IF(NS.EQ.0) THEN
        IF(LCHAR('S')) THEN
          CALL TV2INP(ZXMAX,IOFF)
          GO TO 10
        ENDIF
      ENDIF
C
      IYFLG=0
      IF(INSI.GT.0) THEN
        IF(LCHAR('Y')) THEN
          IYFLG=1
          IANS=0
          GO TO 20
        ENDIF
      ENDIF
C
      IANS=IXUOPC('IJDPKQX0$')
      CALL UCHKOP(IANS,0,1,100,2,*11)
 20   CONTINUE
      IF(IANS.GT.5) RETURN
      IF(IANS.EQ.4) GO TO 500
      IF((IANS.LE.1).OR.(IANS.EQ.3)) THEN
        CALL UREADW('TVGETM:  SPECIFY FILENAME .EXT (3 CHARS):$',ZCBUFF)
        IEXT=0
        IEXT1=1
        IEXT2=1
        ilnb=min(len(zext),index(zcbuff,' ')-1)
        ZEXT(1:ilnb)=ZCBUFF(1:ilnb)
        CALL UROTOR(ilnb)
        IF(IANS.EQ.3) GO TO 300
      ELSE IF((ians.eq.2).or.(ians.eq.5)) then
        if(ians.eq.2) then
           istart=2
           idigs=2
           IF(LXOUT(0)) WRITE(LUNTRM,2012)
 2012 FORMAT(' SPECIFY A RANGE OF FILENAME EXTENSIONS.  FILENAME'/
     >  ' EXTENSIONS OF THE FORM Amm ARE FORMED WHERE A IS AN ALPHA-'/
     >  ' NUMERIC CHARACTER AND mm ARE 1 OR 2 DIGIT POSITIVE INTEGERS'/
     >  ' IN THE RANGE YOU SPECIFY.')
           CALL UREADC('TVGETM:  SPECIFY FILE .EXT 1ST CHAR "A":$',
     >   ZEXT(1:1))
        else
           if(lxout(0)) write(luntrm,2014)
 2014      format(' Please specify a range of filename extensions,'/
     >        ' of form <prefix>nnnn, where <prefix> is a fixed.'/
     >        ' character string and nnnn is a fixed number of digits'/
     >        ' with leading zeroes if needed, and a range of values'/
     >        ' consistent with this number of digits will be given.')
           call ureadw('tvgetm: extention <prefix>:$',zext)
           istart=ilnurd(zext)+1
           call ureadi('tvgetm: number of digits:',idigs)
           if(idigs.le.0) then
              call uermsi('?tvgetm: need a positive number.',idigs,0)
              go to 10
           else if(len(zext).lt.(istart+idigs-1)) then
              call uermsi(
     >           '?tvgetm: prefix length + #digits exceeds maximum.',
     >           len(zext),0)
              go to 10
           endif
           if(lxout(0)) write(luntrm,*)
     >        ' enter positive integers of length not to exceed: ',idigs
        endif
 21     continue
        ilim=10**idigs - 1
        CALL UREAD(
     >'TVGETM:  SPECIFY RANGE LOWER LIMIT (2 DIGIT INTEGER):$')
        IF(LCHAR('Q')) GO TO 10
        IEXT1=IDCOD(IERT)
        IF(IERT.NE.0) GO TO 21
        if((iext1.lt.0).or.(iext1.gt.ilim)) then
           call uermsi(' ?tvgetm: invalid range limit: ',iext1,0)
           go to 21
        endif
 
 22     CALL UREAD(
     >'TVGETM:  SPECIFY RANGE UPPER LIMIT (2 DIGIT INTEGER):$')
        IF(LCHAR('Q')) GO TO 10
        IEXT2=IDCOD(IERT)
        IF(IERT.NE.0) GO TO 22
        IF((IEXT1.GT.IEXT2).OR.(MIN0(IEXT1,IEXT2).LT.0).OR.
     >       (MAX0(IEXT1,IEXT2).GT.ilim)) THEN
          WRITE(LUNTRM,9011) IEXT1,IEXT2
 9011 FORMAT(' ? ILLEGAL RANGE mm = ',I6,' TO ',I6/)
          CALL UPAUST(1)
          CALL UCLEAR
          GO TO 10
        ENDIF
        CALL ZXFORM(ZEXT,IEXT1,istart,idigs)
        IEXT=IEXT1-1
      ENDIF
C--------------------------------------------/END INPUT SPEC BLOCK
      ENDIF
C
C  READ FILE, SHOT INSHOT
C
C  LOOP
      CALL NEUPAG
 39   CONTINUE
      IEXT=IEXT+1
      IF(IEXT.GT.IEXT2) GO TO 10
      IF(IEXT.GT.IEXT1) CALL ZXFORM(ZEXT,IEXT,istart,idigs)
C
C  FIRST SEE IF ALREADY HAS BEEN READ
      ISP1=NS+1
      IF(NS.EQ.0) GO TO 50
      IF((NSHOT2.GT.0).AND.(NS.EQ.1)) GO TO 50
C
      DO 30 I=1,NS
        IF((ZEXT.EQ.XSHOTX(I)).AND.(INSHOT.EQ.ISHOTS(I))) GO TO 40
 30   CONTINUE
C
      GO TO 50
C
 40   CONTINUE
      IF(IYFLG.EQ.0) THEN
        WRITE(LUNTRM,9003) INSHOT,ZEXT
 9003   FORMAT(/' % SHOT ',I6,' EXTENSION .',A,' WAS ALREADY READ'/)
        GO TO 39
      ELSE
        CALL UREAD('TVGETM:  ENTER NEW Y VALUE FOR SHOT, OR "Q":$$')
        IF(LCHAR('Q')) GO TO 10
        ZY=FDCOD(IERT)
        IF(IERT.NE.0) GO TO 10
        CALL TVYCHK(ZY,0,IER)
        IF(IER.NE.0) GO TO 10
        YSHOTS(I)=ZY
        GO TO 10
      ENDIF
C
C  OPEN AND READ THE INPUT FILE
C
 50   CONTINUE
      IF(IYFLG.EQ.1) THEN
        WRITE(LUNTRM,9004) INSHOT,ZEXT
 9004   FORMAT(/' % SHOT ',I6,' EXTENSION .',A,' UNKNOWN')
        GO TO 10
      ENDIF
      IF(ISP1.LE.NSMAX) GO TO 52
      WRITE(LUNTRM,9008) ISP1
 9008 FORMAT(
     >  ' CANNOT READ ',I3,'TH INPUT PROFILE -- ARRAYS TOO SMALL'/)
      CALL URSTOP(1)
      GO TO 10
C
 52   CONTINUE
C  SET FILENAME EXTENSION
      UEXTND(ICHANL)=ZEXT
      CALL UFOPRD(LUNI,INSHOT,IER)
      IF(IER.NE.0) CALL UERJMP(0,*39)
C
C  READ DATA
C
      IF(NS.EQ.0) IOFF=0
      IPT=1+NS*NX + IOFF
      CALL UF1DRD(LUNI,IDEVI,ZSDATI,XIN,FIN(IPT),
     >     NXMAX,INX,IPROCI,XLABI,FLABI,
     >     NSCMAX,NUMSC(ISP1),SCVALI(1,ISP1),SCLABI(1,1,ISP1),IER)
      IF(IER.NE.0) CALL UERJMP(0,*295)
C  CHECK THAT CONCATENATION VARIABLE IS PRESENT
      IF(YKEY(1:5).NE.'USER:') THEN
        IND=IUFKEY(YKEY,SCLABI(1,1,ISP1),NUMSC(ISP1))
        IF(IND.EQ.0) THEN
          WRITE(LUNTRM,9009) YKEY
 9009     FORMAT(/
     >	    ' ? SPECIFIED INPUT FILE DOES NOT CONTAIN VALUE OF'/
     >	    '   VARIABLE OF CONCATENATION (KEYWORD "',A10,'") - ',
     >	    'DATA REJECTED'/)
          CALL UERJMP(1,*295)
        ENDIF
C----
        YLABI(1)=SCLABI(2,IND,ISP1)
        YLABI(2)='          '
        YLABI(3)=SCLABI(3,IND,ISP1)
        YSHOT0(ISP1)=SCVALI(IND,ISP1)
C  DELETE KEYWORD FROM LIST
        CALL UFDKEY(IND,
     >	     SCLABI(1,IND,ISP1),SCVALI(IND,ISP1),NSCMAX,NUMSC(ISP1))
      ELSE
C  GET Y VALUE FROM USER INPUT
        IRETRY=0
 80     CONTINUE
        IF((NS.EQ.0).OR.(IRETRY.EQ.1)) THEN
          CALL UREADL(
     >       'TVGETM:  OK, GIVE 20 CHAR LABEL FOR "Y" AXIS DATA:$',ZBUF)
          YLABI(1)=ZBUF(1:10)
          YLABI(2)=ZBUF(11:20)
          CALL UREADL(
     >       'TVGETM:  GIVE 10 CHAR UNITS FOR "Y" AXIS DATA:$',YLABI(3))
        ENDIF
        CALL UREADF(
     >      'TVGETM:  GIVE "Y" VALUE TO ASSOCIATE WITH INPUT DATA:$',
     >      YSHOT0(ISP1))
        IF(LXOUT(0)) WRITE(LUNTRM,8001) YSHOT0(ISP1),
     >      (YLABI(IJ),IJ=1,3)
 8001   FORMAT(' INPUT Y VALUE:  ',1PE11.4/
     >           ' Y LABEL:  ',2A10,' Y UNITS:  ',A10)
        CALL UREAD('TVGETM:  ENTER "Y" TO VERIFY:$$')
        IF(.NOT.LYES(0)) THEN
          IRETRY=1
          GO TO 80
        ENDIF
      ENDIF
      YSHOTS(ISP1)=YSHOT0(ISP1)
      IF(LXOUT(0)) WRITE(LUNTRM,2002) YLABI(1),YSHOTS(ISP1),YLABI(3),
     >     NUMSC(ISP1)
 2002 FORMAT(' INPUT FILE ',A10,' = ',1PE10.3,1X,A10/
     >         ' FILE CONTAINS ',I2,' ADDITIONAL SCALAR QUANTITIES')
C
C  CHECK THE INPUT DATA
C
 100  CONTINUE
C
C  1ST PROFILE ?
C
      IF(NS.GT.0) GO TO 200
C  1ST PROFILE:  RADIAL VECTOR AND LABELS, ETC. ARE ALL DEFINED
      NS=1
C  LABELS:
      DO 110 I=1,3
      XLAB(I)=XLABI(I)
      YLAB(I)=YLABI(I)
      FLAB(I)=FLABI(I)
 110  CONTINUE
C
      IDEV=IDEVI
      ZSDATE=ZSDATI
      IPROC=IPROCI
C  SET UP RADIAL VECTOR AND 1ST DATA PROFILE
C
      NX=INX
      ZXMAX=0.0
      DO 120 I=1,INX
        X(I)=XIN(I)
        ZXMAX=AMAX1(ZXMAX,ABS(XIN(I)))
 120  CONTINUE
C
      IOFF=0
C
      GO TO 280
C------------
 200  CONTINUE
C
C  SAVE SUBSEQUENT PROFILE
C   LABELS AND SCALAR QUANTITIES ARE ASSUMED NOT TO CHANGE
C   THESE ARE NOT CHECKED
C   THE RADIAL VECTOR OF THE PROFILE MUST AGREE WITH THAT OF THE
C   FIRST PROFILE
C
C  CHECK RADIAL VECTOR
      IF(NX.NE.INX) GO TO 290
C
      DO 210 I=1,INX
      ZTOL= 0.001		! FRACTIONAL TOLERANCE, CM.
      IF((ABS(X(I)-XIN(I))/ZXMAX).GT.ZTOL) GO TO 290
 210  CONTINUE
C
C  CHECK LABELS
C
      IPAUSE=0
      IF(.NOT.
     >    ((IDEV.NE.IDEVI).OR.(ZSDATE.NE.ZSDATI).OR.(IPROC.NE.IPROCI)))
     >    GO TO 240
      IPAUSE=1
      WRITE(LUNTRM,9012) ISHOTS(1),XSHOTX(1),INSHOT,ZEXT,
     >     IDEV,IDEVI,ZSDATE,ZSDATI,IPROC,IPROCI
 9012 FORMAT(/10X,'SHOTS:    ',I6,1X,A10,2X,I6,1X,A10/
     >  ' DEVICE:             ',A4,'           ',A4/
     >  ' SHOT DATE:    ',A10,'     ',A10/
     >  ' PROC. CODE:            ',I1,14X,I1//
     >  ' % NON-FATAL INCONSISTANCY WARNING - DATA IN COL. 1 RETAINED'/)
C
 240  CONTINUE
      DO 245 I=1,3
      IF(XLAB(I).NE.XLABI(I)) GO TO 260
      IF(YLAB(I).NE.YLABI(I)) GO TO 260
      IF(FLAB(I).NE.FLABI(I)) GO TO 260
 245  CONTINUE
      GO TO 270
C
 260  CONTINUE
      IPAUSE=1
      WRITE(LUNTRM,9013) ISHOTS(1),XSHOTX(1),INSHOT,ZEXT,
     >     XLAB,XLABI,YLAB,YLABI,FLAB,FLABI
 9013 FORMAT(' SHOTS:   ',I6,1X,A10,15X,I6,1X,A10/
     >         ' X LABEL: ',3A10,1X,3A10/
     >         ' Y LABEL: ',3A10,1X,3A10/
     >         ' F LABEL: ',3A10,1X,3A10//
     >    ' % NON-FATAL LABEL INCONSISTANCY WARNING; 1ST COL. USED')
C
 270  CONTINUE
      IF(IPAUSE.EQ.1) CALL URSTOP(2)  ! INTERRUPT TYPEAHEAD
C
      CALL TVYCHK(YSHOTS(ISP1),0,IER)
      IF(IER.NE.0) GO TO 295
C
C OK
C
      NS=NS+1
C  INSERT IN LIST
 280  CONTINUE
      ISHOTS(NS)=INSHOT
      XSHOTX(NS)=ZEXT
C  SPOOL COMMENTS TO TEMPORARY OUTPUT FILE
      CALL CFNXID(CFILID(NS),1)
      CFILNM(5:6)=CFILID(NS)
      OPEN(UNIT=LUNC,FILE=CJUNKDIR(1:LJUNKDIR)//CFILNM,status='unknown')
      CALL UFSNAM(INSHOT,ZFILE,IER,LUNI)
      WRITE(LUNC,3001) ZFILE,INSHOT,IDEVI,ZSDATI,IPROCI
 3001 FORMAT(' ----------------------------------------------'/
     > ' COMMENTS SPOOLED BY "CONCAT" FROM FILE: '/'     "',A,'"'/
     > ' SHOT ',I6,1X,A4,1X,A10,' IPROC = ',I1,':'/)
      CALL USPOOL(LUNI,LUNC)
      CLOSE(UNIT=LUNC)
      CALL UFCLOS(LUNI)
      GO TO 39
C  ERROR:  RADIAL VECTOR CONFLICT
 290  CONTINUE
      WRITE(LUNTRM,9015) INSHOT,ZEXT,ISHOTS(1),XSHOTX(1)
 9015 FORMAT(
     > /' ?  ERROR:  "X" VECTORS IN SPECIFIED INPUT PROFILES'/
     >  '    DO NOT AGREE.  SHOTS: ',2(I6,1X,A10,1X)/)
      CALL URSTOP(1)
 295  CONTINUE
C  CLOSE FILE AFTER ERROR
      CALL UFCLOS(LUNI)
      GO TO 39
C-------------------------------------------
C  DELETE PROFILE FROM INPUT BUFFER
C
 300  CONTINUE
      IF(MODEI.EQ.1) INSHOT=-INSHOT
      DO 310 I=1,NS
      IF((ZEXT.EQ.XSHOTX(I)).AND.(INSHOT.EQ.ISHOTS(I))) GO TO 320
 310  CONTINUE
C
      WRITE(LUNTRM,9014) INSHOT
 9014 FORMAT(/' ? ERROR DELETING SPECIFIED PROFILE'/
     >          '   SHOT ',I6,' NOT FOUND.'/)
      CALL UERJMP(1,*10)
C
 320  CONTINUE
      IF(I.EQ.NS) GO TO 350
      IP1=I+1
      DO 330 I=IP1,NS
      IM1=I-1
      ISHOTS(IM1)=ISHOTS(I)
      XSHOTX(IM1)=XSHOTX(I)
      YSHOTS(IM1)=YSHOTS(I)
      YSHOT0(IM1)=YSHOT0(I)
      CFILID(IM1)=CFILID(I)
      NUMSC(IM1)=NUMSC(I)
      DO 325 ISC=1,NUMSC(IM1)
      SCVALI(ISC,IM1)=SCVALI(ISC,I)
      DO 325 IJ=1,3
      SCLABI(IJ,ISC,IM1)=SCLABI(IJ,ISC,I)
 325  CONTINUE
      JP1=(IM1-1)*NX + IOFF
      JP2=JP1+NX
      DO 330 J=1,NX
      FIN(JP1+J)=FIN(JP2+J)
 330  CONTINUE
C
 350  CONTINUE
      NS=NS-1
      GO TO 10
C---------------------------------------------------------------
 500  CONTINUE
C
C  LIST CURRENT BUFFER CONTENTS
C
      CALL TVMAK2
      CALL XSHOTS(1)
      GO TO 10
      END
C******************** END FILE TVGETM.FOR ; GROUP CONCAT ***************
