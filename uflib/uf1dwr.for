C******************** START FILE UF1DWR.FOR ; GROUP UFILES *************
C--------------------------------------------------------------
C  UF1DWR
C  COMPRESS AND WRITE 1-DIMENSIONAL DATA TO ASCII FILE
C
 
      SUBROUTINE UF1DWR(ILUN,IDEV_I,ZSDATE_I,
     >     X_I,F_I,N,IPROC,
     >     XLAB_I,FLAB_I,
     >     NSC,SCVAL,SCLAB_I,
     >	   IER)
 
 
C  Updated:
C  10/22/01 CAL - support MDS+
C  08/05/97 CAL - Add HDF
C  08/24/93 TBT - New format E13.6 - UFAHED2.
C
C  ILUN-- LOGICAL UNIT NUMBER ON WHICH TO WRITE
C  IDEV-- TOKAMAK DEVICE -- (4 LETTERS) "PDX ","PLT ", ETC.
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C  X(N),F(N)-- REAL NUMERIC DATA TO BE WRITTEN
C      F[X]-- X THE INDEPENDENT, F THE DEPENDENT VARIABLE
C  IPROC-- PROCESS CODE: 0 = "RAW" DATA,
C     1 = AVERAGED (OVER SEVERAL SHOTS)
C     2 = SMOOTHED
C     3 = AVERAGED AND SMOOTHED
C  XLAB-- 30 CHARACTER LABEL FOR X
C   FIRST 20 CHARACTERS-- NAME OF X
C   LAST 10 CHARACTERS--  PHYSICAL UNITS OF X
C     E.G. XLAB = 'MAJOR RADIUS        CM        '
C  FLAB-- 30 CHARACTER LABEL FOR Y - SAME FORMAT AS XLAB
C  NSC-- NUMBER OF SCALAR QUANTITIES ASSOCIATED WITH DATA TO WRITE
C         NOTE NSC = 0 **IS** ALLOWED; THEN THE FOLLOWING ARE DUMMIES:
C  SCVAL(NSC)-- LIST OF SCALAR QUANTITIES (REAL NOS.)
C  SCLAB(NSC) CHARACTER*30 LIST OF 30 CHARACTER KEYWORD/LABELS
C
C*  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
C
C* DENOTES OUTPUT VARIABLE
 
      USE UFILES
      implicit none
C
      character*(*) IDEV_I,ZSDATE_I,xlab_i(3),flab_i(3)
      CHARACTER*30 SCLAB_I(*)
C
      integer ilun,  n,     nsc
      REAL X_I(N),F_I(N),SCVAL(*)
      CHARACTER*10 ZSDATE,ZDATE
      CHARACTER*30 XLAB,FLAB,SCLAB(max(1,nsc))
      CHARACTER*4 IDEV
      Integer Iformat
      integer l
      integer ilnurd
 
      integer ier,   idim,  kmprs, iproc, iflag, ierh, ilu, iluj
      integer ierr,  ierrw, iendw, ichan, jproc
      real    ztol
C
      real, dimension(:), allocatable :: x,f
C
C$r8real_input: x_i,f_i,scval
C--------------------------------------------------------------------
C  dmc -- guard against ieee underflows... to prevent problems downstream
c
      call ufck_reset
      allocate(x(n),f(n))
      call ufck_under(ufelun,'x',x_i,x,n)
      call ufck_under(ufelun,'f',f_i,f,n)
c
C--------------------------------------------------------------------
C  standardize labels
C
      call ufsc_lbck(zsdate_i,zsdate,idev_i,idev,sclab_i,sclab,nsc)
      if(len(xlab_i(1)).ne.10) then
         call uf_lbck('xlab',xlab_i,xlab)
      else
         call uf_lbck('xlab',xlab_i(1)//xlab_i(2)//xlab_i(3),xlab)
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
      CALL UFLCHK(ILUN,'UF1DWR    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.1) THEN
        CALL UERSTA(ICHAN,'UF1DWR    ')
        GO TO 9000
      ENDIF
C
C  CHANNEL STATUS OK
C
      IDIM=1
      ZDATE=''
      CALL C9DATE(ZDATE)
      KMPRS=UFCPRS(ICHAN)
      if(kmprs.eq.-1) then
         write(ufelun,'('' ?UF1DWR:  MDS+ write '')')
         l=index(mds_node(ichan),' ')-1
         call add1duf(ufelun,mds_node(ichan)(1:l),
     >        x,f,n,iproc,
     >        n,iflag,xlab,flab,
     >        nsc,nsc,scval,sclab,ier)
         if (ier .eq. 0) then
            call uf_fin(ufelun,mds_node(ichan)(1:l),IDEV,ZDATE,
     >           iproc,ier)
            if (ier .eq. 0) go to 1000
         endif
         go to 9000
      endif
C
C  ** MOD ** DMC APR 1984.
C  ALL WRITES SWITCHED ON KMPRS-- SUPPORT COMPRESSED BINARY FORMAT
C
      IF(KMPRS.EQ.0) THEN
        CALL UFAHED2(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,
     >      1,Iformat,IERH)
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE
      else if(kmprs.eq.2) then
        call ufbhed(ilun,ufshot(ichan),idev,idim,kmprs,zdate,zsdate,
     >                1,ierh)
C      else if (kmprs.eq.3) then
C         call uhdfhed(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,1,
C     >      IERH)
      else if (kmprs.eq. 4) then
         call ucdfhed(ILUN,UFSHOT(ICHAN),IDEV,IDIM,KMPRS,ZDATE,ZSDATE,1,
     >      IERH)
      ENDIF
C
C    WRITE THE ASSOCIATED SCALARS
      CALL UF0WRT(ILUN,SCVAL,SCLAB,NSC,IER)
      IF(IER.NE.0) GO TO 9000
C
      IF(KMPRS.EQ.0) THEN
        WRITE(ILUN,3003) XLAB
 3003 FORMAT(1X,A30,T32,';-INDEPENDENT VARIABLE LABEL-')
C
        WRITE(ILUN,3004) FLAB
 3004 FORMAT(1X,A30,T32,';-DEPENDENT VARIABLE LABEL-')
C
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) XLAB,FLAB
      else if(kmprs.eq.2) then
        call ubwchar(ilun,xlab)
        call ubwchar(ilun,flab)
C HDF ?
C      else if (kmprs .eq. 3) then
C         call uhdf1w(ilun, f, x, n, iproc,
C     >                 flab, xlab, ier)
C         if (ier .ne. 0) go to 9000
C netCDF ?
      else if (kmprs .eq. 4) then
         call ucdf1w(ilun, f, x, n, iproc,
     >                 flab, xlab, ier)
         if (ier .ne. 0) go to 9000
      ENDIF
C
      IF(KMPRS.EQ.0) THEN
        jproc=max(0,min(9,iproc))
        WRITE(ILUN,3006) jproc
 3006 FORMAT(1X,I1,T32,';-PROC CODE- 0:RAW 1:AVG 2:SM. 3:AVG+SM')
C
        WRITE(ILUN,3007) N
 3007 FORMAT(1X,I10,T32,';-# OF PTS-  X, F(X) DATA FOLLOW:')
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) IPROC,N
      else if(kmprs.eq.2) then
        call ubw3int(ilun,iproc)
        call ubw3int(ilun,n)
      ENDIF
C----
C  NOW THE DATA ...
C
      LFCLUN=.FALSE.
      if(kmprs.eq.2) then
         call ubcpwr(ilun,x,'x',n,1,1,n,1)
         call ubcpwr(ilun,f,'f',n,1,1,n,1)
      else IF(KMPRS.EQ.1) THEN
C--------------------------------------/ * COMPRESSED DATA   WRITES * /
C ERROR LOG FILE--
C
         ILUJ=ILNURD(UJUNKA)
         CALL UFOPEN(UFCLUN,UJUNKA(1:ILUJ)//'ZUFCMPRI.ERR',2,IER,0)
         IF(IER.NE.0) GO TO 9000
         LFCLUN=.TRUE.
C
         ZTOL=UFCTOL(ICHAN)
C  CONVERT INDEPENDENT DATA TO INTEGER*2 AND COMPRESS
         CALL UFSTEP(ILUN,X,'X',N,IERR)
         IF (IERR.NE.0) THEN
            CALL UFCPWR(ILUN,X,'X',N,1,1,N,1,ZTOL,IERR)
         ENDIF
C
C  CONVERT DEPENDENT DATA TO INTEGER*2
         CALL UFCPWR(ILUN,F,'F',N,1,1,N,1,ZTOL,IERR)
C
C--------------------------------------/ * UNCOMPRESSED ASCII WRITES * /
C
      ELSE IF(KMPRS.EQ.0) THEN
C
         LFCLUN=.FALSE.
C
         call ufascio(ilun,x,n,'W',iendw,ierrw)
         call ufascio(ilun,f,n,'W',iendw,ierrw)
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
      go to 9999
C----------
C  ERRORS
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ? DATA NOT WRITTEN DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=2
C
 9999 continue
      deallocate(x,f)
      RETURN
      	END
C******************** END FILE UF1DWR.FOR ; GROUP UFILES ***************
