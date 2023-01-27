C******************** START FILE UF2DWR.FOR ; GROUP UFILES *************
C--------------------------------------------------------------
C  UF2DWR
C  COMPRESS AND WRITE 2-DIMENSIONAL DATA TO ASCII FILE
C
 
      SUBROUTINE UF2DWR(ILUN,IDEV_I,ZSDATE_I,
     >     F_i,NF1,X_i,NX,Y_i,NY,IPROC,
     >     FLAB_I,XLAB_I,YLAB_I,
     >     NSC,SCVAL,SCLAB_I,
     >     IER)
 
 
C  Updates:
C   22 Oct 2001  CAL - support MDS+
C   29 Jul 1997  CAL - Add HDF
C   23 Aug 1993  TbT - Changed format from (1X, 1PE11.4) - 1X,6(1PE13.6)
C
C  ILUN-- FILE LOGICAL UNIT NUMBER
C  IDEV-- 4 LETTER TOKAMAK DEV. CODE ("PLT ", ETC.)
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C  X(NX),F(NF1,NY),Y(NY)-- REAL NUMERIC DATA TO BE WRITTEN
C   ARRAYS AND DIMENSIONS
C  IPROC- PROCESS CODE 0= "RAW", 1= AVERAGED
C    2= SMOOTHED, 3=AVERAGED AND SMOOTHED
C  XLAB-- 30 CHARACTER LABEL FOR X
C   FIRST 20 CHARACTERS-- NAME OF X
C   LAST 10 CHARACTERS--  PHYSICAL UNITS OF X
C     E.G. XLAB = 'MAJOR RADIUS        CM        '
C  FLAB-- 30 CHARACTER LABEL FOR F - SAME FORMAT AS XLAB
C  YLAB-- 30 CHARACTER LABEL FOR Y - SAME FORMAT AS XLAB
C
C  NSC-- NUMBER OF ASSOCIATED SCALARS TO WRITE
C   ** NOTE NSC=0 IS ALLOWED **
C  SCVAL- VALUE(S) OF ASSOCIATED SCALARS
C  SCLAB- ASSOCIATED SCALARS KEYWORDS, LABELS, PHYS. UNITS.
C
C*  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
C
C* DENOTES OUTPUT VARIABLE
 
      USE UFILES
      implicit none
C
      integer       nx,    nf1,   ny,    nsc,  ilun
      character*(*) IDEV_I,ZSDATE_I,xlab_i(3),ylab_i(3),flab_i(3)
      CHARACTER*30 SCLAB_I(*)
C
      REAL X_i(NX),F_i(NF1,NY),Y_i(NY),SCVAL(*)
      CHARACTER*10 ZSDATE,ZDATE
      CHARACTER*30 XLAB,FLAB,YLAB,SCLAB(max(1,nsc))
      CHARACTER*4 IDEV
 
      Integer Iformat
      integer l
      integer ilnurd
 
      integer ier,   idim,  kmprs, iproc, iflag, ierh, ilu, iluj
      integer ierr,  ierrw, iendw, ichan, jproc
      real    ztol
C
      integer i
      real, dimension(:), allocatable :: x,y
      real, dimension(:,:), allocatable :: f
C
C$r8real_input: x_i,f_i,y_i,scval
C--------------------------------------------------------------------
C  dmc -- guard against ieee underflows... to prevent problems downstream
c
      call ufck_reset
      allocate(x(nx),y(ny),f(nf1,ny)); f=0
      call ufck_under(ufelun,'x',x_i,x,nx)
      call ufck_under(ufelun,'y',y_i,y,ny)
      do i=1,ny
         call ufck_under(ufelun,'f',f_i(1,i),f(1,i),nx)
      enddo
c
C	----------------------------------------------------------------
C  standardize labels
C
      call ufsc_lbck(zsdate_i,zsdate,idev_i,idev,sclab_i,sclab,nsc)
C
      if(len(xlab_i(1)).ne.10) then
         call uf_lbck('xlab',xlab_i,xlab)
      else
         call uf_lbck('xlab',xlab_i(1)//xlab_i(2)//xlab_i(3),xlab)
      endif
      if(len(ylab_i(1)).ne.10) then
         call uf_lbck('ylab',ylab_i,ylab)
      else
         call uf_lbck('ylab',ylab_i(1)//ylab_i(2)//ylab_i(3),ylab)
      endif
      if(len(flab_i(1)).ne.10) then
         call uf_lbck('flab',flab_i,flab)
      else
         call uf_lbck('flab',flab_i(1)//flab_i(2)//flab_i(3),flab)
      endif
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UF2DWR    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.1) THEN
        CALL UERSTA(ICHAN,'UF2DWR    ')
        GO TO 9000
      ENDIF
C
C  DIMENSION CHECK
C
      IF(NX.GT.NF1) THEN
        WRITE(UFELUN,9011) NX,NF1
 9011   FORMAT(' ? DIMENSION OF X ARRAY EXCEEDS FIRST DIMENSION OF'/
     >       '   F ARRAY --UF2DWR-- NX=',I10,' NF1=',I10/)
        GO TO 9000
      ENDIF
C
C
      IDIM=2
      ZDATE=''
      CALL C9DATE(ZDATE)
      KMPRS = UFCPRS(ICHAN)
c MDSplus
      if(kmprs.eq.-1) then
         write(ufelun,'('' ?UF2DWR:  MDS+ write '')')
         l=index(mds_node(ichan),' ')-1
         call add2duf(ufelun,mds_node(ichan)(1:l),
     >        f,nf1,x,nx,y,ny,iproc,
     >        nx,ny,iflag,flab,xlab,ylab,
     >        nsc,nsc,scval,sclab,ier)
         if (ier .eq. 0) then
            call uf_fin(ufelun,mds_node(ichan)(1:l),IDEV,ZDATE,
     >          iproc,ier)
            if (ier .eq. 0) go to 1000
         endif
         goto 9000
      endif
C
C
C  ** MOD APR 1984 -- ALL WRITES SWITCHED FOR BINARY COMPRESS OPTION
      IF(KMPRS.EQ.0) THEN
        CALL UFAHED2(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,1,
     >       Iformat,IERH)
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE
      else if(kmprs.eq.2) then
        call ufbhed(ilun,ufshot(ichan),idev,idim,kmprs,zdate,zsdate,1,
     >       ierh)
C      else if (kmprs.eq.3) then
C         call uhdfhed(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,1,
C     >      IERH)
      else if (kmprs.eq.4) then
         call ucdfhed(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,1,
     >       IERH)
      ENDIF
C
C  WRITE THE ASSOCIATED SCALARS
      CALL UF0WRT(ILUN,SCVAL,SCLAB,NSC,IER)
      IF(IER.NE.0) GO TO 9000
C
      IF(KMPRS.EQ.0) THEN
        WRITE(ILUN,3003) XLAB
 3003 FORMAT(1X,A30,T32,';-INDEPENDENT VARIABLE LABEL: X-')
C
        WRITE(ILUN,3013) YLAB
 3013 FORMAT(1X,A30,T32,';-INDEPENDENT VARIABLE LABEL: Y-')
C
        WRITE(ILUN,3004) FLAB
 3004 FORMAT(1X,A30,T32,';-DEPENDENT VARIABLE LABEL-')
C
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) XLAB,YLAB,FLAB
      else if(kmprs.eq.2) then
        call ubwchar(ilun,xlab)
        call ubwchar(ilun,ylab)
        call ubwchar(ilun,flab)
C
C HDF ?
C      else if (kmprs .eq. 3) then
C         call uhdf2w(ilun,f,nf1,x,nx,y,ny,iproc,
C     >                 flab,xlab,ylab,ier)
C         if (ier .ne. 0) go to 9000
C netCDF ?
      else if (kmprs .eq. 4) then
         call ucdf2w(ilun,f,nf1,x,nx,y,ny,iproc,
     >       flab,xlab,ylab,ier)
         if (ier .ne. 0) go to 9000
      ENDIF
C
      IF(KMPRS.EQ.0) THEN
        jproc=max(0,min(9,iproc))
        WRITE(ILUN,3006) jproc
 3006 FORMAT(1X,I1,T32,';-PROC CODE- 0:RAW 1:AVG 2:SM 3:AVG+SM')
C
        WRITE(ILUN,3007) NX,NY
 3007 FORMAT(1X,I10,T32,';-# OF X PTS-'/1X,I10,T32,
     >       ';-# OF Y PTS- X,Y,F(X,Y) DATA FOLLOW:')
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) IPROC,NX,NY
      else if(kmprs.eq.2) then
        call ubw3int(ilun,iproc)
        call ubw3int(ilun,nx)
        call ubw3int(ilun,ny)
      ENDIF
C
C
C  CONVERT INDEPENDENT DATA TO INTEGER*2 AND COMPRESS
C
      LFCLUN=.FALSE.
      if(kmprs.eq.2) then
C
         call ubcpwr(ilun,x,'x',nx,1,1,nx,1)
         call ubcpwr(ilun,y,'y',ny,1,1,ny,1)
         call ubcpwr(ilun,f,'f',nf1,ny,1,nx,ny)
C
      else IF(KMPRS.EQ.1) THEN
C------------------------------------------/ * COMPRESSED DATA WRITES *
C ERROR LOG FILE--
C
         ILUJ=ILNURD(UJUNKA)
         CALL UFOPEN(UFCLUN,UJUNKA(1:ILUJ)//'ZUFCMPRI.ERR',2,IER,0)
         IF(IER.NE.0) GO TO 9000
         LFCLUN=.TRUE.
C
         ZTOL=UFCTOL(ICHAN)
         CALL UFSTEP(ILUN,X,'X',NX,IERR)
         IF (IERR.NE.0) THEN
            CALL UFCPWR(ILUN,X,'X',NX,1,1,NX,1,ZTOL,IERR)
         ENDIF
         CALL UFSTEP(ILUN,Y,'Y',NY,IERR)
         IF (IERR.NE.0) THEN
            CALL UFCPWR(ILUN,Y,'Y',NY,1,1,NY,1,ZTOL,IERR)
         ENDIF
C
         CALL UFCPWR(ILUN,F,'F',NF1,NY,1,NX,NY,ZTOL,IERR)
C------------------------------------------/ * UNCOMPRESSED WRITES *
      ELSE IF(KMPRS.EQ.0) THEN
C
         LFCLUN=.FALSE.
C
         call ufascio(ilun,x,nx,'W',iendw,ierrw)
         call ufascio(ilun,y,ny,'W',iendw,ierrw)
         call ufascio2(ilun,f,nf1,nx,ny,'W',iendw,ierrw)
C
      ENDIF
C
      IF(KMPRS.EQ.0) WRITE(ILUN,3010)
 3010 FORMAT(' ;----END-OF-DATA-----------------COMMENTS:-----------')
C
 1000 IER=0
      UFSTAT(ICHAN)=2
C
C  IF COMPRESSION WARNING ERRORS OCCURRED, SPOOL TO TTY AND OUTPUT FILE
C
      IF(LFCLUN) THEN
        REWIND UFCLUN
        CALL USPOOL(UFCLUN,ILUN)
        CLOSE(UNIT=UFCLUN,STATUS='DELETE')
      ENDIF
C
      goto 9999
C----------
C  ERRORS
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ? DATA NOT WRITTEN DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      UFSTAT(ICHAN)=2
      IER=1

 9999 continue
      deallocate(x,y,f)
      RETURN
      END
C******************** END FILE UF2DWR.FOR ; GROUP UFILES ***************
