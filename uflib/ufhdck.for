C******************** START FILE UFHDCK.FOR ; GROUP UFILES *************
C--------------------------------------------------------------
C  UFHDCK
C  READ HEADER OF UFILE AND RETURN STATISTICAL INFORMATION
C
 
      SUBROUTINE UFHDCK(ILUN,INFO,IER)
 
 
C  Updated:
C    10/12/12 CLF - Comment out HDF
C    07/30/97 CLF - Added hdf support
C    08/24/93 TBT - Added call to UFAHED2 for format - put in INFO(7)
C
C  INPUT:
C    ILUN -- L.U.N. OF A JUST - OPENED  UFILE
C
C  OUTPUT:
C    INFO -- INFORMATION BLOCK
C    INFO(1)=FILE DIMENSIONALITY
C    INFO(2)=TOTAL # OF WORDS IN DEPENDENT DATA ARRAY
C    INFO(3)=SIZE OF FIRST DIMENSION OF DATA ARRAY
C    INFO(4)=SIZE OF SECOND DIMENSION OF DATA ARRAY (IF APPLICABLE)
C     ...
C    Info(7)= 0 if Pre 8/93 Data format - =1 if 8/93 format.
C    INFO(8)=NUMBER OF SCALARS IN DATA FILE
C
C    IER = 0 IF ROUTINE COMPLETES EXECUTION SUCCESSFULLY
C
      USE UFILES
C
      INTEGER INFO(8)
C
      CHARACTER*10 ZSDATE,ZWDATE
      CHARACTER*4 IDEV
      CHARACTER*30 ZBUFL,ZBUFI(8)
C
      character*128 znode
C
      CHARACTER*64 ERRSTR
C
      Integer Iformat
C----------------------------------------------------------------------
C
      IER=0
      DO 5 I=1,8
        INFO(I)=0
 5    CONTINUE
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UFHDCK    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.3) THEN
        CALL UERSTA(ICHAN,'UFHDCK    ')
        GO TO 9000
      ENDIF
C
C  SWITCH FOR COMPRESSED BINARY OR ASCII READ DETERMINED AT OPEN TIME
C
      KMPRS=UFCPRS(ICHAN)
      if(kmprs.eq.-1) then
         ikmprs=-1
C----------------------------------------
C  MDS+ data access
C
         call ufmds_rhdr(ichan,idev,zsdate,ishot,idim,znode,iln,
     >        iproc, ier)
         if(ier.eq.1) go to 9000
C
C----------------------------------------
C
      else IF(KMPRS.EQ.0) THEN
        IKMPRS=0
        CALL UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZWDATE,ZSDATE,0,
     1                 Iformat,IERH)
        Info(7) = Iformat
        IF(IERH.NE.0) GO TO 8999
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999)
     >      ISHOT,IDEV,IDIM,IKMPRS,ZWDATE,ZSDATE
      else if(kmprs.eq.2) then
         call ufbhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
         if(ierh.ne.0) go to 8999
C      else if(kmprs.eq. 3) then
C         call uhdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
C         if(ierh.ne.0) go to 8999
      else if(kmprs.eq. 4) then
         call ucdfhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
         if(ierh.ne.0) go to 8999
      ENDIF
C
      INFO(1)=IDIM
C
      IF(KMPRS.NE.IKMPRS) THEN
        WRITE(UFELUN,*) ' % UFHDCK INTERNAL FLAG ERROR KMPRS, IKMPRS =',
     >        KMPRS,IKMPRS
        go to 8999
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
C    READ ASSOCIATED SCALAR QUANTITIES
C
      ERRSTR='ASSOCIATED SCALAR DATA'
C
      if (kmprs .eq. -1) then
         call ufmds_dim(ichan,idim,info(3),nsca,znode(1:iln),ier)
         IF(IER.NE.0) GO TO 8999
         INFO(8)=NSCA
C      else if (kmprs .eq. 3) then
C         call uhdfdim(ilun,idim,info(3),nsca,iproc,ier)
C         IF(IER.NE.0) GO TO 8999
C         INFO(8)=NSCA
      else if (kmprs .eq. 4) then
         call ucdfdim(ilun,idim,info(3),nsca,iproc,ier)
         IF(IER.NE.0) GO TO 8999
         INFO(8)=NSCA
      else
         CALL UFNSRD(KMPRS,ILUN,NSCA,IERR)
         IF(IERR.NE.0) GO TO 8999
C
         INFO(8)=NSCA
C
         DO 100 ISC=1,NSCA
            IF(KMPRS.EQ.1) THEN
      	 READ(ILUN,ERR=9000,END=9000) ZVAL,ZBUFL
            ELSE IF(KMPRS.EQ.0) THEN
      	 READ(ILUN,3019,ERR=9000,END=9000) ZVAL,ZBUFL
 3019 	 FORMAT(1X,1PE11.4/1X,A30)
            else if(kmprs.eq.2) then
      	 call ubrfcmp(ilun,zval,1,ier)
      	 if(ier.ne.0) go to 9000
      	 call ubrchar(ilun,zbufl,ier)
      	 if(ier.ne.0) go to 9000
            ENDIF
 100     CONTINUE
      end if
C
      IF(INFO(1).EQ.0) GO TO 5000 ! DONE IF ITS A SCALAR UFILE
C
      ERRSTR='DATA COORDINATE LABELS'
C
      INAXES=INFO(1)+1
C
      IF(KMPRS.EQ.0) THEN
        DO 200 ID=1,INAXES
          READ(ILUN,3003,END=8999,ERR=8999) ZBUFL
 3003     FORMAT(1X,A30)
 200    CONTINUE
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999) (ZBUFI(IJ),IJ=1,INAXES)
      else if(kmprs.eq.2) then
         do id=1,inaxes
            call ubrchar(ilun,zbufl,ier)
            if(ier.ne.0) go to 8999
         enddo
      ENDIF
C
C  SIZES OF DIMENSIONS
C
      ERRSTR='DATA PROCESSING CODE + NUMBER OF POINTS'
      IF(KMPRS.EQ.0) THEN
        READ(ILUN,3006,END=8999,ERR=8999) IPROC
 3006   FORMAT(1X,I1)
        DO 250 ID=1,INFO(1)
          IDI=ID+2
          READ(ILUN,3017,END=8999,ERR=8999) INFO(IDI)
 3017     FORMAT(1X,I10)
 250    CONTINUE
      ELSE IF(KMPRS.EQ.1) THEN
        ID1=3
        ID2=2+INFO(1)
        READ(ILUN,END=8999,ERR=8999) IPROC,(INFO(IDI),IDI=ID1,ID2)
      else if(kmprs.eq.2) then
        call ubr3int(ilun,iproc,ier)
        if(ier.ne.0) go to 8999
        do id=1,info(1)
           idi=id+2
           call ubr3int(ilun,info(idi),ier)
           if(ier.ne.0) go to 8999
        enddo
      ENDIF
C
C  MULTIPLY OUT TOTAL NUMBER OF WORDS
C
      INFO(2)=1
      DO 300 ID=1,INFO(1)
        INFO(2)=INFO(2)*INFO(2+ID)
 300  CONTINUE
C
C----------
C  NORMAL EXIT
C
 5000 CONTINUE
C
      call ufrwnd_do(ilun,ichan,kmprs,ier)
C
      IER=0
C
C  return error if info(i)=0 for all i
C
      do i=1,8
         if(info(i).ne.0) go to 5100
      enddo
      ier=2
 5100 continue
C
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
C******************** END FILE UFHDCK.FOR ; GROUP UFILES ******************
