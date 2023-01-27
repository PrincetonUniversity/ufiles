C******************** START FILE UF2DRD.FOR ; GROUP UFILES ******************
C--------------------------------------------------------------
C  UF2DRD
C  READ 2-DIMENSIONAL DATA FROM ASCII FILE
C   ...dmc... adding MDS+ access capability ... 17 Dec 1998
C
 
      SUBROUTINE UF2DRD(ILUN,IDEV,ZSDATE,
     >     F,NF1,X,NX,Y,NY,IPROC,
     >     NXACT,NYACT,IFLAG,
     >     FLAB,XLAB,YLAB,
     >     NSCD,NSCA,SCVAL,SCLAB,
     >     IER)
 
 
C  Updated:
C  07/17/97 cal - Add HDF file option
C  08/24/93 tbt - Changed format to E13.6 - Call UFAHED2.
C
C*  ILUN-- FILE LOGICAL UNIT NUMBER
C   IDEV-- TOKAMAK DEVICE CODE (4 LETTERS E.G. "PLT ")
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C  X(NX),F(NF1,NY),Y(NY)-- REAL NUMERIC DATA TO BE READ
C   ARRAYS AND DIMENSIONS
C  IPROC-- PROCESS CODE 0= RAW, 1 = AVERAGED
C   2= SMOOTHED, 3= AVERAGED AND SMOOTHED
C*   NX,NF1,NY ARE INPUT
C  NXACT,NYACT ARE THE ACTUAL DIMENSIONS OF THE
C  X AND Y ARRAYS IN THE FILE ***
C*  IFLAG:
C   ---> IF IFLAG=1 THE DATA IS READ CONSECUTIVELY INTO THE
C   F ARRAY; IF IFLAG .NE. 1, THE DATA IS READ BY
C   READ ((F(IJ,IK),IJ=1,NXACT),IK=1,NYACT)
C  XLAB-- 30 CHARACTER LABEL FOR X
C   FIRST 20 CHARACTERS-- NAME OF X
C   LAST 10 CHARACTERS--  PHYSICAL UNITS OF X
C     E.G. XLAB = 'MAJOR RADIUS        CM        '
C  FLAB-- 30 CHARACTER LABEL FOR F - SAME FORMAT AS XLAB
C  YLAB-- 30 CHARACTER LABEL FOR Y - SAME FORMAT AS XLAB
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
      REAL X(NX),F(NF1,NY),Y(NY),SCVAL(*)
      CHARACTER*10 ZSDATE,ZWDATE
      CHARACTER*30 XLAB,YLAB,FLAB,SCLAB(*)
      CHARACTER*4 IDEV
C
      character*128 znode
C
      CHARACTER*64 ERRSTR
 
      Integer IFORMAT
C
C$r8real_output: x,f,y,scval
C----------------------------------------------------------------------
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UF2DRD    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.3) THEN
        CALL UERSTA(ICHAN,'UF2DRD    ')
        GO TO 9000
      ENDIF
C
C  SWITCH FOR COMPRESSED BINARY OR ASCII READ DETERMINED AT OPEN TIME
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
     >        iproc, ier)
         if(ier.eq.1) go to 9000
C
C----------------------------------------
      endif
C
      IF(KMPRS.EQ.0) THEN
        IKMPRS=0
        CALL UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZWDATE,ZSDATE,0,
     1                 IFORMAT,IERH)
        IF(IERH.NE.0) GO TO 8999
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999)
     >      ISHOT,IDEV,IDIM,IKMPRS,ZWDATE,ZSDATE
        IF((UFILEV-10*(UFILEV/10)).GE.1) WRITE(UFELUN,8001) ZWDATE
 8001   FORMAT(' % READING 2D COMPRESSED BINARY DATA WRITTEN ',A)
      else if(kmprs.eq.2) then
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
 9008   FORMAT(' % UF2DRD INTERNAL FLAG ERROR KMPRS, IKMPRS =',
     >2(I2,1X))
      ENDIF
C
C  SHOT CHECK
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
C  DIMENSION CHECK
C
      IF(IDIM.NE.2) THEN
        WRITE(UFELUN,9010) IDIM
 9010   FORMAT(
     >  ' ? DIMENSIONALITY OF INPUT FILE = ',I1,' IS INCORRECT',
     >  ' FOR UF2DRD')
        IER=1
        GO TO 9000
      ENDIF
C
C    READ ASSOCIATED SCALAR QUANTITIES
      ERRSTR='ASSOCIATED SCALAR DATA'
      if (KMPRS.EQ.-1) then            ! MDSplus
         call ufmds_0rd(ICHAN,SCVAL,SCLAB,NSCD,NSCA,znode(1:iln),IER)
         IF(IER.NE.0) GO TO 8999
C      else if (KMPRS.EQ.3) then
C         call uhdf0r(ILUN,SCVAL,SCLAB,NSCD,NSCA,IER)
C         IF(IER.NE.0) GO TO 8999
      else if (KMPRS.EQ. 4) then
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
C
      IF(NSCD.GT.0 .AND. NSCD.LT.NSCA) THEN
        WRITE(UFELUN,9052) NSCD,NSCA
 9052 FORMAT(' % UF2DRD WARNING-- INSUFFICIENT SPACE FOR SCALARS:'/
     >  '   SPACE ALLOCATED=',I3,' NEED:',I3/)
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
C----------------------------------------
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3003,END=8999,ERR=8999) XLAB
 3003 FORMAT(1X,A30)
C
        READ(ILUN,3003,END=8999,ERR=8999) YLAB
C
        READ(ILUN,3003,END=8999,ERR=8999) FLAB
C
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999) XLAB,YLAB,FLAB
      else if(kmprs.eq.2) then
        call ubrchar(ilun,xlab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,ylab,ier)
        if(ier.ne.0) go to 8999
        call ubrchar(ilun,flab,ier)
        if(ier.ne.0) go to 8999
      ENDIF
C
      ERRSTR='DATA PROCESSING CODE + NUMBER OF POINTS'
      if(kmprs.eq.-1) then
         call ufmds_size2(znode(1:iln),f,nf1*ny,x,nx,
     1	nxact,nyact,iskip1,
     1	xlab,ier)
         if(ier.ne.0) go to 9000
      else IF(KMPRS.EQ.0) THEN
        READ(ILUN,3006,END=8999,ERR=8999) IPROC
 3006   FORMAT(1X,I1)
        READ(ILUN,3017,END=8999,ERR=8999) NXACT,NYACT
 3017   FORMAT(1X,I10/1X,I10)
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999) IPROC,NXACT,NYACT
      else if(kmprs.eq.2) then
        call ubr3int(ilun,iproc,ier)
        if(ier.ne.0) go to 8999
        call ubr3int(ilun,nxact,ier)
        if(ier.ne.0) go to 8999
        call ubr3int(ilun,nyact,ier)
        if(ier.ne.0) go to 8999
C      else if(kmprs .eq. 3) then
C         call uhdf2r(ilun,1,
C     &            f,nf1,x,nx,y,ny,iproc,
C     &            nxact, nyact, iflag,
C     &            flab,xlab,ylab,ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs .eq. 4) then
         call ucdf2r(ilun,1,
     &            f,nf1,x,nx,y,ny,iproc,
     &            nxact, nyact, iflag,
     &            flab,xlab,ylab,ier)
         if(ier.ne.0) go to 8999
      ENDIF
C
C  CHECK FEASIBILITY OF DIMENSIONS
C  CHECK NXACT,NYACT INDIVIDUALLY
C
      IER=0
      IF(NX.LT.NXACT) THEN
        WRITE(UFELUN,9903) NX,NXACT
 9903   FORMAT(' ? UF2DRD - X ARRAY TOO SMALL:  SIZE=',I10,
     >       ' NEED=',I10)
        IER=1
      ENDIF
      IF(NY.LT.NYACT) THEN
        WRITE(UFELUN,9904) NY,NYACT
 9904   FORMAT(' ? UF2DRD - Y ARRAY TOO SMALL:  SIZE=',I10,
     >       ' NEED=',I10)
        IER=1
      ENDIF
C
      IF(IFLAG.NE.1) THEN
        IF(NF1.LT.NXACT) THEN
          WRITE(UFELUN,9905) NF1,NXACT
 9905     FORMAT(' ? UF2DRD - 1ST DIMENSION OF F ARRAY TOO SMALL'/
     >         '   SIZE = ',I10,' NEED = ',I10)
          IER=1
        ENDIF
C
      ELSE
        ITOTA=NF1*NY
        ITOTD=NXACT*NYACT
        IF(ITOTA.LT.ITOTD) THEN
          WRITE(UFELUN,9906) ITOTA,ITOTD
 9906     FORMAT(' ? UF2DRD - F ARRAY TOO SMALL'/
     >         '   TOTAL WORDS = ',I10,' NEED = ',I10)
          IER=1
        ENDIF
      ENDIF
C
      IF(IER.NE.0) THEN
C
        WRITE(UFELUN,9907)
 9907   FORMAT(
     >     /' ? UF2DRD - ARRAYS PROVIDED TOO SMALL FOR DATA REQUESTED'/)
        GO TO 9000
      ENDIF
C
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access
C
         call ufmds_read(znode(1:iln),f,nxact*nyact,ier)
         if(ier.ne.0) go to 9000
         if((iflag.ne.1).and.(nf1.gt.nxact)) then
            call ufmds_fmov(f,nxact,nyact,f,nf1)
         endif
C
         if(iskip1.ne.1) then
            call ufmds_read('dim_of('//znode(1:iln)//',0)',x,nxact,
     1	   ier)
            if(ier.ne.0) go to 9000
         endif
         call ufmds_read('dim_of('//znode(1:iln)//',1)',y,nyact,ier)
         if(ier.ne.0) go to 9000
C
C----------------------------------------
      else if(kmprs.eq.4) then         ! netCDF
         call ucdf2r(ilun,2,f,nf1,x,nx,y,ny,
     >                 iproc,nxact,nyact,iflag,
     >                 flab,xlab,ylab,ier)
         if(ier.ne.0) go to 8999
C      else if(kmprs.eq.3) then         ! HDF
C         call uhdf2r(ilun,2,f,nf1,x,nx,y,ny,
C     >                 iproc,nxact,nyact,iflag,
C     >                 flab,xlab,ylab,ier)
C         if(ier.ne.0) go to 8999
      else if(kmprs.eq.2) then
         call ubcprd(ilun,x,'x',nxact,1,1,nxact,1,ier)
         if(ier.ne.0) go to 8999
         call ubcprd(ilun,y,'y',nyact,1,1,nyact,1,ier)
         if(ier.ne.0) go to 8999
         if(iflag.eq.1) then
            inx1=nxact
         else
            inx1=nf1
         endif
         ERRSTR='DEPENDENT COORDINATE (F) DATA'
         call ubcprd(ilun,f,'f',inx1,nyact,1,nxact,nyact,ier)
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
        IF(IFLAG.EQ.1) THEN
          INX1=NXACT
        ELSE
          INX1=NF1
        ENDIF
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        CALL UFCPRD(ILUN,F,INX1,NYACT,1,NXACT,NYACT,IERR)
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
 
        ERRSTR='DEPENDENT COORDINATE (F) DATA'
        IF(IFLAG.NE.1) THEN
          if(iformat.eq.0) then
             READ(ILUN,3008,END=8999,ERR=8999)
     >             ((F(IJ,IK),IJ=1,NXACT),IK=1,NYACT)
          else
             call ufascio2(ilun,f,nf1,nxact,nyact,'R',iend,ierr)
             if((iend.ne.0).or.(ierr.ne.0)) go to 8999
          endif
 3008     FORMAT(6(1X,1PE11.4))       ! Old format
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
     >    6X,A)
C
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ? DATA NOT READ DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=5
      RETURN
      	END
C------------------------------
C  move data from contiguous to non-contiguous storage -- carefully,
C  because usually fin and fout are the same arrays!
C
      subroutine ufmds_fmov(fin,nxin,ny,fout,nxout)
C
      real fin(nxin,ny),fout(nxout,ny)
C
C  work by looping down in row index, overwrite avoided because
C  "to address" is always bigger than "from address"
C
C  nxout.gt.nxin is assumed!
C
      do iy=ny,1,-1
         do ix=nxin,1,-1
            fout(ix,iy)=fin(ix,iy)
         enddo
      enddo
C
      do iy=1,ny
         do ix=nxin+1,nxout
            fout(ix,iy)=0.0
         enddo
      enddo
C
      return
      end
C******************** END FILE UF2DRD.FOR ; GROUP UFILES ******************
