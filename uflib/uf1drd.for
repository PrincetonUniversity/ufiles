C******************** START FILE UF1DRD.FOR ; GROUP UFILES ******************
C--------------------------------------------------------------
C  UF1DRD
C  READ 1-DIMENSIONAL DATA FROM ASCII FILE
C   ...dmc... adding MDS+ access capability ... 17 Dec 1998
C
 
      SUBROUTINE UF1DRD(ILUN,IDEV,ZSDATE,
     >     X,F,NDIM,NACT,IPROC,
     >     XLAB,FLAB,
     >     NSCD,NSCA,SCVAL,SCLAB,IER)
 
C  Updated:
C  08/05/97 CAL - Add HDF file option
C  08/24/93 TBT - Changed call to UFAHED2 fro new format (3009)
C   mod dmc -- use SR ufascio instead (portability)
C
C
C* ILUN-- LOGICAL UNIT NUMBER FOR I/O
C  IDEV-- 4 LETTER TOKAMAK CODE ("PLT ", ETC.)
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C*  NDIM-- DIMENSION OF ARRAYS
C  X(N),F(N)-- REAL NUMERIC DATA TO BE READ
C      F[X]-- X THE INDEPENDENT, F THE DEPENDENT VARIABLE
C  NACT-- ACTUAL NUMBER OF POINTS IN DATA FILE
C  IPROC- PROCESS CODE: 0 = RAW, 1= AVERAGED, 2= SMOOTHED,
C    3= AVERAGED AND SMOOTHED
C  XLAB-- 30 CHARACTER LABEL FOR X
C   FIRST 20 CHARACTERS-- NAME OF X
C   LAST 10 CHARACTERS--  PHYSICAL UNITS OF X
C     E.G. XLAB = 'MAJOR RADIUS        CM        '
C  FLAB-- 30 CHARACTER LABEL FOR Y - SAME FORMAT AS XLAB
C
C*  NSCD-- SPACE ALLOCATED FOR ASSOCIATED SCALARS
C  NSCA-- RETURNED ACTUAL NUMBER OF SCALARS IN FILE
C    ** NOTE ** NSCD MAY = 0 IN WHICH CASE THESE ARE DUMMIES:
C  SCLAB-- LIST OF ASSOCIATED SCALAR KEYWORD/LABELS READ
C    FORMAT OF SCLAB: 30 CHARS, 10 CHAR KEYWORD BLOCK, 10 CHAR LABEL,
C      10 CHAR PHYS. UNITS-- SEE SUBROUTINE UF0WRT, FILE UFILS0.FOR
C
C  IER=0 ON RETURN IF READ WAS SUCCESSFUL
C
C* DENOTES INPUT
      USE UFILES
C
      REAL X(NDIM),F(NDIM),SCVAL(*)
      CHARACTER*10 ZSDATE,ZWDATE
      CHARACTER*30 XLAB,FLAB,SCLAB(*)
      CHARACTER*4 IDEV
C
      character*128 znode
C
      CHARACTER*64 ERRSTR
 
      Integer Iformat
C
C$r8real_output: x,f,scval
C-----------------------------------------------------------------
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UF1DRD    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.3) THEN
        CALL UERSTA(ICHAN,'UF1DRD    ')
        GO TO 9000
      ENDIF
C
C  CHANNEL STATUS OK
C
C  READ DATA - BINARY COMPRESSED OR ASCII DETERMINED AT OPEN TIME
C
      KMPRS=UFCPRS(ICHAN)
C
      ERRSTR='HEADER DATA'
      if(KMPRS.eq.-1) then
         ikmprs=-1
C----------------------------------------
C  MDS+ data access
C
         call ufmds_rhdr(ichan,idev,zsdate,ishot,idim,znode,iln,
     >        iproc,ier)
         if(ier.eq.1) go to 9000
C
C----------------------------------------
      endif
C
      IF(KMPRS.EQ.0) THEN
        IKMPRS=0
        CALL UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZWDATE,ZSDATE,0,
     1         IFORMAT,IERH)
        IF(IERH.NE.0) GO TO 8999
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999)
     >      ISHOT,IDEV,IDIM,IKMPRS,ZWDATE,ZSDATE
        IF((UFILEV-10*(UFILEV/10)).GE.1) WRITE(UFELUN,8001) ZWDATE
 8001   FORMAT(' % READING 1D COMPRESSED BINARY DATA WRITTEN ',A)
      ELSE IF(KMPRS.EQ.2) THEN
        call ufbhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
        if(ierh.ne.0) go to 8999
C      else if(kmprs.eq.3) then
C        call uhdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
C        if(ierh.ne.0) go to 8999
      else if(kmprs.eq.4) then
        call ucdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
        if(ierh.ne.0) go to 8999
      ENDIF
C
      IF(KMPRS.NE.IKMPRS) THEN
        WRITE(UFELUN,9008) KMPRS,IKMPRS
 9008   FORMAT(' % UF1DRD INTERNAL FLAG ERROR KMPRS, IKMPRS =',
     >2(I2,1X))
      ENDIF
C
C  CHECK THE SHOT NO.
C
      if(lshotn(ichan)) then
         IF(ISHOT.NE.UFSHOT(ICHAN)) THEN
            IF((UFILEV/10).GE.1)
     >          WRITE(UFELUN,9011) ISHOT,UFSHOT(ICHAN)
 9011       FORMAT(
     >  ' % SHOT NUMBER ',I6,' IN FILE NOT SAME AS SHOT NUMBER ',I6,
     >  ' IN FILENAME')
         ENDIF
      endif
C
C  CHECK DIMENSIONALITY OF DATA
C
      IF(IDIM.NE.1) THEN
        WRITE(UFELUN,9010) IDIM
 9010   FORMAT(
     >  ' ? DIMENSIONALITY OF INPUT FILE = ',I1,' IS INCORRECT',
     >  ' FOR UF1DRD')
        IER=1
        GO TO 9000
      ENDIF
C
C  READ ASSOCIATED SCALAR QUANTITIES
      ERRSTR='ASSOCIATED SCALAR DATA'
      if (KMPRS.EQ.-1) then            ! MDSplus
         call ufmds_0rd(ICHAN,SCVAL,SCLAB,NSCD,NSCA,znode(1:iln),IER)
         IF(IER.NE.0) GO TO 8999
C      else if (KMPRS.EQ.3) then            ! HDF
C         call uhdf0r(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
C         IF(IER.NE.0) GO TO 8999
      else if (KMPRS.EQ.4) then       ! netCDF
         call ucdf0r(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
         IF(IER.NE.0) GO TO 8999
      else
         CALL UFNSRD(KMPRS,ILUN,NSCA,IERR)
         IF(IERR.NE.0) GO TO 8999
         IF(NSCA.GT.0) THEN
            CALL UF0RDR(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
            IF(IER.NE.0) GO TO 8999
         ENDIF
      end if
 
      if(nsca.gt.0) call ufsclab_ck(sclab,nsca)
 
      IF(NSCD.GT.0 .AND. NSCD.LT.NSCA) THEN
        WRITE(UFELUN,9052) NSCD,NSCA
 9052   FORMAT(' % UF1DRD WARNING-- INSUFFICIENT SPACE FOR SCALARS:'/
     >  '   SPACE ALLOCATED=',I3,' NEED:',I3/)
      ENDIF
C
C  AXIS LABELS
C
      ERRSTR='DATA COORDINATE LABELS'
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access -- labels
C
         call ufmds_lbl(ichan,znode(1:iln),flab,1,znode(1:iln),ier)
         if(ier.ne.0) go to 9000
C
         call ufmds_lbl(ichan,'dim_of('//znode(1:iln)//',0)',xlab,0,
     1	'(dim 0)',ier)
         if(ier.ne.0) go to 9000
C
C----------------------------------------
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3003,ERR=8999,END=8999) XLAB
 3003 FORMAT(1X,A30)
C
        READ(ILUN,3003,ERR=8999,END=8999) FLAB
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,ERR=8999,END=8999) XLAB,FLAB
      else if(kmprs.eq.2) then
        call ubrchar(ilun,xlab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,flab,ier)
        if(ier.ne.0) go to 8999
      ENDIF
C
      ERRSTR='DATA PROCESSING CODE + NUMBER OF POINTS'
      if(kmprs.eq.-1) then
         iproc=0
         call ufmds_size(znode(1:iln),1,nact,ier) ! size of 1st dim
         if(ier.ne.0) go to 9000
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3006,ERR=8999,END=8999) IPROC
 3006 FORMAT(1X,I1)
C
        READ(ILUN,3007,ERR=8999,END=8999) NACT
 3007 FORMAT(1X,I10)
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,ERR=8999,END=8999) IPROC,NACT
c
      else if(kmprs.eq.2) then
         call ubr3int(ilun,iproc,ier)
         if(ier.ne.0) go to 8999
         call ubr3int(ilun,nact,ier)
         if(ier.ne.0) go to 8999
c
C      else if(kmprs .eq. 3) then
C         call uhdf1r(ilun,1,
C     &            f, x, ndim, nact, iproc,
C     &            flab,xlab,ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs .eq. 4) then
         call ucdf1r(ilun,1,
     &            f, x, ndim, nact, iproc,
     &            flab,xlab,ier)
         if(ier.ne.0) go to 8999
      ENDIF
C
      IF(NACT.LE.NDIM) GO TO 30
C
      WRITE(UFELUN,9012) NACT,NDIM
 9012 FORMAT(' ? UF1DRD-- DATA TO BE READ WILL NOT FIT IN'/
     >  '   PASSED ARRAYS:  NACT= ',I10,' .GT. NDIM= ',I10/)
      GO TO 9000
C
 30   CONTINUE
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access
C
         call ufmds_read(znode(1:iln),f,nact,ier)
         if(ier.ne.0) go to 9000
         call ufmds_read('dim_of('//znode(1:iln)//',0)',x,nact,ier)
         if(ier.ne.0) go to 9000
C
C----------------------------------------
      else if(kmprs.eq. 4) then         ! netCDF
         call ucdf1r(ilun,2, f, x, ndim, nact, iproc,
     >                 flab, xlab, ier)
         if(ier.ne.0) go to 8999
C      else if(kmprs.eq.3) then         ! HDF
C         call uhdf1r(ilun,2, f, x, ndim, nact, iproc,
C     >                 flab, xlab, ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs.eq.2) then
        call ubcprd(ilun,x,'x',nact,1,1,nact,1,ier)
        if(ier.ne.0) go to 8999
        call ubcprd(ilun,f,'f',nact,1,1,nact,1,ier)
        if(ier.ne.0) go to 8999
      else IF(KMPRS.EQ.1) THEN
C
C  READ COMPRESSED ARRAYS, DECOMPRESS, AND CONVERT TO REAL
        ERRSTR='INDEPENDENT COORDINATE (X) DATA'
        CALL UFCPRD(ILUN,X,NACT,1,1,NACT,1,IERR)
        IF(IERR.NE.0) GO TO 8999
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        CALL UFCPRD(ILUN,F,NACT,1,1,NACT,1,IERR)
        IF(IERR.NE.0) GO TO 8999
      ELSE
C
C  READ UNCOMPRESSED DATA
        ErrStr = 'Input Data Format'
        if ((IFORMAT.lt.0).or.(IFORMAT.gt.1)) go to 8999
 
        ERRSTR='INDEPENDENT COORDINATE (X) DATA'
        if(IFORMAT.eq.0) then
           READ(ILUN,3008,ERR=8999,END=8999) (X(IJ),IJ=1,NACT)
        else
           call ufascio(ilun,x,nact,'R',iendr,ierrr)
           if(ierrr.gt.0) go to 8999
           if(iendr.gt.0) go to 8999
        endif
 
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        if(IFORMAT.eq.0) then
           READ(ILUN,3008,ERR=8999,END=8999) (F(IJ),IJ=1,NACT)
        else
           call ufascio(ilun,f,nact,'R',iendr,ierrr)
           if(ierrr.gt.0) go to 8999
           if(iendr.gt.0) go to 8999
        endif
	
 3008   FORMAT(6(1PE12.4))  ! old format
      ENDIF
C
C  READ DUMMY LINE AT END ("--END-OF-DATA--...")
      ERRSTR='TRAILER RECORD'
      IF(KMPRS.EQ.0) READ(ILUN,3099,ERR=8999,END=8999) IDUMMY
 3099 FORMAT(A1)
C
      IER=0
      UFSTAT(ICHAN)=5
      RETURN
C----------
C  ERRORS
C
 8999 CONTINUE
      WRITE(UFELUN,9899) ERRSTR
 9899 FORMAT(' ? *FATAL* END OF FILE OR READ ERROR ON UFILE READ:'/
     >    6X,A)
C
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ?UF1DRD: DATA NOT READ DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=5
      RETURN
      	END
C******************** END FILE UF1DRD.FOR ; GROUP UFILES ******************
