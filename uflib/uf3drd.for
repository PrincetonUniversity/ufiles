C******************** START FILE UF3DRD.FOR ; GROUP UFILES *************
C--------------------------------------------------------------
C  UF3DRD
C  READ 3-DIMENSIONAL DATA FROM ASCII FILE
C
 
      SUBROUTINE UF3DRD(ILUN,IDEV,ZSDATE,
     >     F,NF1,NF2,X,NX,Y,NY,Z,NZ,IPROC,
     >     NXACT,NYACT,NZACT,IFLAG,
     >     FLAB,XLAB,YLAB,ZLAB,
     >     NSCD,NSCA,SCVAL,SCLAB,
     >     IER)
 
 
C  Updated:
C   02/07/01 CAL - support MDSplus
C   08/05/97 CAL - Add HDF / netCDF file options
C   08/24/93 TBT - Added UFAHED2 and updated format.
 
C
C*  ILUN-- FILE LOGICAL UNIT NUMBER
C   IDEV-- TOKAMAK DEVICE CODE (4 LETTERS E.G. "PLT ")
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C  X(NX),F(NF1,NY),Y(NY),Z(N)-- REAL NUMERIC DATA TO BE READ
C   ARRAYS AND DIMENSIONS
C  IPROC-- PROCESS CODE 0= RAW, 1 = AVERAGED
C   2= SMOOTHED, 3= AVERAGED AND SMOOTHED
C*   NX,NF1,NF2,NY,NZ ARE INPUT
C  NXACT,NYACT,NZACT ARE THE ACTUAL DIMENSIONS OF THE
C  X AND Y AND Z ARRAYS IN THE FILE ***
C*  IFLAG:
C   ---> IF IFLAG=1 THE DATA IS READ CONSECUTIVELY INTO THE
C   F ARRAY; IF IFLAG .NE. 1, THE DATA IS READ BY
C   READ (((F(IJ,IK,IL),IJ=1,NXACT),IK=1,NYACT),IL=1,NZACT)
C  XLAB-- 30 CHARACTER LABEL FOR X
C   FIRST 20 CHARACTERS-- NAME OF X
C   LAST 10 CHARACTERS--  PHYSICAL UNITS OF X
C     E.G. XLAB = 'MAJOR RADIUS        CM        '
C  FLAB-- 30 CHARACTER LABEL FOR F - SAME FORMAT AS XLAB
C  YLAB-- 30 CHARACTER LABEL FOR Y - SAME FORMAT AS XLAB
C  ZLAB-- 30 CHARACTER LABEL FOR Z - SAME FORMAT AS XLAB
C
C*  NSCD-- SPACE ALLOCATED FOR ASSOCIATED SCALARS
C  NSCA-- RETURNED ACTUAL NUMBER OF SCALARS IN FILE
C    ** NOTE ** NSCD MAY = 0 IN WHICH CASE THESE ARE DUMMIES:
C  SCVAL-- LIST OF ASSOCIATED SCALAR VALUES READ
C  SCLAB-- LIST OF ASSOCIATED SCALAR KEYWORD/LABELS READ
C    FORMAT OF SCLAB: 30 CHARS, 10 CHAR KEYWORD BLOCK, 10 CHAR LABEL,
C      10 CHAR PHYS. UNITS-- SEE SUBROUTINE UF0WRT, FILE UFILS0.FOR
C
C  IER=0 ON RETURN IF READ WAS SUCCESSFUL
C
C* DENOTES INPUT VARIABLE
      USE UFILES
C
      REAL X(NX),F(NF1,NF2,NZ),Y(NY),Z(NZ),SCVAL(*)
      CHARACTER*10 ZSDATE,ZWDATE
      CHARACTER*30 XLAB,YLAB,ZLAB,FLAB,SCLAB(*)
      CHARACTER*4 IDEV
C
      character*128 znode
      integer       iln
C
      CHARACTER*64 ERRSTR
C
      Integer Iformat
C
C$r8real_output: f,x,y,z,scval
C-----------------------------------------------------------------------
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UF3DRD    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.3) THEN
        CALL UERSTA(ICHAN,'UF3DRD    ')
        GO TO 9000
      ENDIF
C
C  SWITCH FOR COMPRESSED BINARY OR ASCII READ DETERMINED AT OPEN TIME
C
C
      KMPRS=UFCPRS(ICHAN)
C
      ERRSTR='UFILE HEADER DATA'
      if(KMPRS.eq.-1) then
         ikmprs=-1
C----------------------------------------
C  MDS+ data access
C
         call ufmds_rhdr(ichan,idev,zsdate,ishot,idim,znode,iln,
     >        iproc, ier)
         if(ier.eq.1) go to 9000
 
      else IF(KMPRS.EQ.0) THEN
        IKMPRS=0
        CALL UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZWDATE,ZSDATE,0,
     1                 Iformat,IERH)
        IF(IERH.NE.0) GO TO 8999
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999)
     >       ISHOT,IDEV,IDIM,IKMPRS,ZWDATE,ZSDATE
        IF((UFILEV-10*(UFILEV/10)).GE.1) WRITE(UFELUN,8001) ZWDATE
 8001   FORMAT(' % READING 3D COMPRESSED BINARY DATA WRITTEN ',A)
      else if(kmprs.eq.2) then
        call ufbhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
        if(ierh.ne.0) go to 8999
C      else if(kmprs. eq. 3) then
C         call uhdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
C         if(ierh.ne.0) go to 8999
      else if(kmprs. eq. 4) then
         call ucdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
         if(ierh.ne.0) go to 8999
      ENDIF
C
      IF(KMPRS.NE.IKMPRS) THEN
        WRITE(UFELUN,9008) KMPRS,IKMPRS
 9008   FORMAT(' % UF3DRD INTERNAL FLAG ERROR KMPRS, IKMPRS =',
     >       2(I2,1X))
      ENDIF
C
C  SHOT CHECK
C
      if(lshotn(ichan)) then
         IF(ISHOT.NE.UFSHOT(ICHAN)) THEN
            IF((UFILEV/10).GE.1)
     >          WRITE(UFELUN,9011) ISHOT,UFSHOT(ICHAN)
 9011       FORMAT(
     >           ' % SHOT NUMBER ',I6,
     >           ' IN FILE NOT SAME AS SHOT NUMBER ',I6,
     >           ' IN FILENAME')
          ENDIF
      endif
C
C  DIMENSION CHECK
C
      IF(IDIM.NE.3) THEN
        WRITE(UFELUN,9010) IDIM
 9010   FORMAT(
     >       ' ? DIMENSIONALITY OF INPUT FILE = ',I1,' IS INCORRECT',
     >       ' FOR UF3DRD')
        IER=1
        GO TO 9000
      ENDIF
C
C    READ ASSOCIATED SCALAR QUANTITIES
      ERRSTR='ASSOCIATED SCALAR DATA'
      if (KMPRS.EQ.-1) then            ! MDSplus
         call ufmds_0rd(ICHAN,SCVAL,SCLAB,NSCD,NSCA,znode(1:iln),IER)
         IF(IER.NE.0) GO TO 8999
      else if (KMPRS.EQ.4) then
         call ucdf0r(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
         IF(IER.NE.0) GO TO 8999
C      else if (KMPRS.EQ.3) then
C         call uhdf0r(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
C         IF(IER.NE.0) GO TO 8999
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
 9052 FORMAT(' % UF3DRD WARNING-- INSUFFICIENT SPACE FOR SCALARS:'/
     >        '   SPACE ALLOCATED=',I3,' NEED:',I3/)
      ENDIF
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
         call ufmds_lbl(ichan,'dim_of('//znode(1:iln)//',1)',ylab,0,
     1	'(dim 1)',ier)
         if(ier.ne.0) go to 9000
C
         call ufmds_lbl(ichan,'dim_of('//znode(1:iln)//',2)',zlab,0,
     1	'(dim 2)',ier)
         if(ier.ne.0) go to 9000
C
C----------------------------------------
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3003,END=8999,ERR=8999) XLAB
 3003 FORMAT(1X,A30)
C
        READ(ILUN,3003,END=8999,ERR=8999) YLAB
C
        READ(ILUN,3003,END=8999,ERR=8999) ZLAB
C
        READ(ILUN,3003,END=8999,ERR=8999) FLAB
C
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999) XLAB,YLAB,ZLAB,FLAB
      else if(kmprs.eq.2) then
        call ubrchar(ilun,xlab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,ylab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,zlab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,flab,ier)
        if(ier.ne.0) go to 8999
      ENDIF
C
      ERRSTR='DATA PROCESSING CODE + NUMBER OF POINTS'
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access -- labels
C
         call ufmds_3rd(1,znode(1:iln),f,nf1,nf2,nz,x,y,z,
     >       nxact,nyact,nzact,iflag,ier)
         if(ier.ne.0) go to 8999
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3006,END=8999,ERR=8999) IPROC
 3006 FORMAT(1X,I1)
        READ(ILUN,3017,END=8999,ERR=8999) NXACT,NYACT,NZACT
 3017   FORMAT(1X,I10/1X,I10/1X,I10)
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999) IPROC,NXACT,NYACT,NZACT
      else if(kmprs.eq.2) then
        call ubr3int(ilun,iproc,ier)
        if(ier.ne.0) go to 8999
        call ubr3int(ilun,nxact,ier)
        if(ier.ne.0) go to 8999
        call ubr3int(ilun,nyact,ier)
        if(ier.ne.0) go to 8999
        call ubr3int(ilun,nzact,ier)
        if(ier.ne.0) go to 8999
C      else if(kmprs .eq. 3) then
C         call uhdf3r(ilun,1,
C     &            f,nf1,nf2,x,nx,y,ny,z,nz,iproc,
C     &            nxact, nyact, nzact, iflag,
C     &            flab,xlab,ylab,zlab,ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs .eq. 4) then
         call ucdf3r(ilun,1,
     &            f,nf1,nf2,x,nx,y,ny,z,nz,iproc,
     &            nxact, nyact, nzact, iflag,
     &            flab,xlab,ylab,zlab,ier)
         if(ier.ne.0) go to 8999
      ENDIF
C
C  CHECK FEASIBILITY OF DIMENSIONS
C  CHECK NXACT,NYACT INDIVIDUALLY
C
      IER=0
      IF(NX.LT.NXACT) THEN
        WRITE(UFELUN,9903) NX,NXACT
 9903   FORMAT(' ? UF3DRD - X ARRAY TOO SMALL:  SIZE=',I10,
     >       ' NEED=',I10)
        IER=1
      ENDIF
C
      IF(NY.LT.NYACT) THEN
        WRITE(UFELUN,9904) NY,NYACT
 9904   FORMAT(' ? UF3DRD - Y ARRAY TOO SMALL:  SIZE=',I10,
     >       ' NEED=',I10)
        IER=1
      ENDIF
C
      IF(NZ.LT.NZACT) THEN
        WRITE(UFELUN,9914) NZ,NZACT
 9914   FORMAT(' ? UF3DRD - Z ARRAY TOO SMALL:  SIZE=',I10,
     >       ' NEED=',I10)
        IER=1
      ENDIF
C
      IF(IFLAG.NE.1) THEN
C
        IF(NF1.LT.NXACT) THEN
          WRITE(UFELUN,9905) NF1,NXACT
 9905     FORMAT(' ? UF3DRD - 1ST DIMENSION OF F ARRAY TOO SMALL'/
     >         '   SIZE = ',I10,' NEED = ',I10)
          IER=1
        ENDIF
C
        IF(NF2.LT.NYACT) THEN
          WRITE(UFELUN,9915) NF2,NYACT
 9915     FORMAT(' ? UF3DRD - 2ND DIMENSION OF F ARRAY TOO SMALL'/
     >         '   SIZE = ',I10,' NEED = ',I10)
          IER=1
        ENDIF
C
      ELSE
        ITOTA=NF1*NF2*NZ
        ITOTD=NXACT*NYACT*NZACT
        IF(ITOTA.LT.ITOTD) THEN
          WRITE(UFELUN,9906) ITOTA,ITOTD
 9906     FORMAT(' ? UF3DRD - F ARRAY TOO SMALL'/
     >         '   TOTAL WORDS = ',I10,' NEED = ',I10)
          IER=1
        ENDIF
      ENDIF
C
      IF(IER.NE.0) THEN
C
        WRITE(UFELUN,9907)
 9907   FORMAT(
     >     /' ? UF3DRD - ARRAYS PROVIDED TOO SMALL FOR DATA REQUESTED'/)
        GO TO 9000
      ENDIF
C
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access
C
         if (iflag .ne. 1) then
            call ufmds_3rd(2,znode(1:iln),f,nf1,nf2,nz,x,y,z,
     >          nxact,nyact,nzact,iflag,ier)
         else
            call ufmds_read(znode(1:iln),f,itotd,ier)
            if(ier.ne.0) go to 8999
            call ufmds_3rd(3,znode(1:iln),f,nf1,nf2,nz,x,y,z,
     >           nxact,nyact,nzact,iflag,ier)
         endif
         if(ier.ne.0) go to 8999
      else if(kmprs.eq.4) then         ! netCDF
         call ucdf3r(ilun,2,
     >       f,nf1,nf2, x,nx, y,ny, z,nz,
     >       iproc, nxact, nyact, nzact, iflag,
     >       flab, xlab, ylab, zlab, ier)
         if(ier.ne.0) go to 8999
C      else if(kmprs.eq.3) then         ! HDF
C         call uhdf3r(ilun,2,
C     >                 f,nf1,nf2, x,nx, y,ny, z,nz,
C     >                 iproc, nxact, nyact, nzact, iflag,
C     >                 flab, xlab, ylab, zlab, ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs.eq.2) then
         call ubcprd(ilun,x,'x',nxact,1,1,nxact,1,ier)
         if(ier.ne.0) go to 8999
         call ubcprd(ilun,y,'y',nyact,1,1,nyact,1,ier)
         if(ier.ne.0) go to 8999
         call ubcprd(ilun,z,'z',nzact,1,1,nzact,1,ier)
         if(ier.ne.0) go to 8999
         if(iflag.eq.1) then
            inx1=nxact
            inx2=nyact
         else
            inx1=nf1
            inx2=nf2
         endif
         ERRSTR='DEPENDENT COORDINATE (F) DATA'
         call ubcprd(ilun,f,'f',inx1,inx2,nzact,nxact,nyact,ier)
         if(ier.ne.0) go to 8999
      else IF(KMPRS.EQ.1) THEN
C
C  READ COMPRESSED ARRAYS, DECOMPRESS, AND CONVERT TO REAL
        ERRSTR='INDEPENDENT COORDINATE (X) DATA'
        CALL UFCPRD(ILUN,X,NXACT,1,1,NXACT,1,IERR)
        IF(IERR.NE.0) GO TO 8999
C
        ERRSTR='INDEPENDENT COORDINATE (Y) DATA'
        CALL UFCPRD(ILUN,Y,NYACT,1,1,NYACT,1,IERR)
        IF(IERR.NE.0) GO TO 8999
C
        ERRSTR='INDEPENDENT COORDINATE (Z) DATA'
        CALL UFCPRD(ILUN,Z,NZACT,1,1,NZACT,1,IERR)
        IF(IERR.NE.0) GO TO 8999
C
        IF(IFLAG.EQ.1) THEN
          INX1=NXACT
          INX2=NYACT
        ELSE
          INX1=NF1
          INX2=NF2
        ENDIF
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        CALL UFCPRD(ILUN,F,INX1,INX2,NZACT,NXACT,NYACT,IERR)
        IF(IERR.NE.0) GO TO 8999
C
      ELSE IF(KMPRS.EQ.0) THEN
C
C  READ UNCOMPRESSED DATA
          ErrStr = 'ASCII Data Format'
        if ((iformat.lt.0).or.(iformat.gt.1)) go to 8999
 
        ERRSTR='INDEPENDENT COORDINATE (X) DATA'
        if(iformat.eq.0) then
           READ(ILUN,3008,END=8999,ERR=8999) (X(IJ),IJ=1,NXACT)
        else
           call ufascio(ilun,x,nxact,'R',iend,ierr)
           if((iend.ne.0).or.(ierr.ne.0)) go to 8999
        endif
 
        ERRSTR='INDEPENDENT COORDINATE (Y) DATA'
        if(iformat.eq.0) then
           READ(ILUN,3008,END=8999,ERR=8999) (Y(IJ),IJ=1,NYACT)
        else
           call ufascio(ilun,y,nyact,'R',iend,ierr)
           if((iend.ne.0).or.(ierr.ne.0)) go to 8999
        endif
 
        ERRSTR='INDEPENDENT COORDINATE (Z) DATA'
        if(iformat.eq.0) then
           READ(ILUN,3008,END=8999,ERR=8999) (Z(IJ),IJ=1,NZACT)
        else
           call ufascio(ilun,z,nzact,'R',iend,ierr)
           if((iend.ne.0).or.(ierr.ne.0)) go to 8999
        endif
C
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        IF(IFLAG.NE.1) THEN
          if(iformat.eq.0) then
             READ(ILUN,3008,END=8999,ERR=8999)
     >           (((F(IJ,IK,IL),IJ=1,NXACT),IK=1,NYACT),IL=1,NZACT)
          else
            call ufascio3(ilun,f,nf1,nf2,nxact,nyact,nzact,
     >           'R',iend,ierr)
             if((iend.ne.0).or.(ierr.ne.0)) go to 8999
          endif
 3008   FORMAT(6(1X,1PE11.4))
        ELSE
          CALL UF2RUT2(ILUN,F,ITOTD,Iformat,IERR)
          IF(IERR.NE.0) GO TO 8999
        ENDIF
      ENDIF
C
C  READ PAST DUMMY LINE AT END ("--END-OF-DATA--...")
      ERRSTR='TRAILER RECORD'
      IF(KMPRS.EQ.0) READ(ILUN,3099,END=8999,ERR=8999) IDUMMY
 3099 FORMAT(A1)
C
      UFSTAT(ICHAN)=5
      IER=0
      RETURN
C----------
C  ERRORS
C
 8999 CONTINUE
      WRITE(UFELUN,9899) ERRSTR
 9899 FORMAT(' ? *FATAL* END OF FILE OR READ ERROR ON UFILE READ:'/
     >     6X,A)
C
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ? DATA NOT READ DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=5
      RETURN
      	END
C******************** END FILE UF3DRD.FOR ; GROUP UFILES ***************
