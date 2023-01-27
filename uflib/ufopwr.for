C******************** START FILE UFOPWR.FOR ; GROUP UFILES *************
C------------------------------------------------------------
C  UFOPWR
C  OPEN FILE FOR WRITE
C   ARGUMENTS-- ILUN=FILE LOGICAL UNIT NUMBER
C     ISHOT-- DATA SHOT NUMBER (5 DIGITS)
C     IER-- (OUTPUT) ERROR RETURN CODE
C      0=NORMAL
C
      SUBROUTINE UFOPWR(ILUN,ISHOT,IER)
C
      USE UFILES
C
      CHARACTER*128 ZFILE
C
      integer*1 ibhdr(12)
C
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
      data ibhdr/0,127,0,127,0,127,0,127,0,0,0,2/
C
C-----------------------------------------------------------------------
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UFOPWR    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
      IF(UFSTAT(ICHAN).NE.0) THEN
        CALL UERSTA(ICHAN,'UFOPWR    ')
        GO TO 190
      ENDIF
C
      CALL UFSNAM(ISHOT,ZFILE,IER,ILUN)
      IF(IER.NE.0) RETURN
C
      IF((UFILEV-10*(UFILEV/10)).GE.1)
     >     CALL UFSND0(UFELUN,'NAMED FILE',ZFILE)
C
C MDSplus
      if(ufcprs(ichan).eq.-1) then
         call ufopen_mds(ichan,ishot,ier,2)
         if (ier .eq. 0) then
            UFSTAT(ICHAN)=1
         endif
         go to 190
      endif
C
 
C  Unix systems -- if file already exists, move it out of the way!
C
      OPEN(UNIT=UFLUN(ICHAN),FILE=ZFILE,
     >       STATUS='OLD',IOSTAT=IOS)
      CLOSE(UNIT=UFLUN(ICHAN),ERR=99)
 99   CONTINUE
      IF(IOS.EQ.0) THEN
        ILNB=INDEX(ZFILE,' ')-1
        IF(ILNB.LE.0) ILNB=LEN(ZFILE)
        WRITE(UFELUN,8001) ZFILE(1:ILNB),ZFILE(1:ILNB)
 8001   FORMAT(/' %UFOPWR:  FILE ',A,' ALREADY EXISTS'/
     >  '  ** old file is renamed to --> ',a,'~'/)
        call frename(zfile(1:ilnb),zfile(1:ilnb)//'~',istat)
        IF(ISTAT.NE.0) WRITE(UFELUN,8002) ISTAT
 8002   FORMAT(' %UFOPWR:  file rename call returned ISTAT=',i10)
      ENDIF
C
      CALL UFOPEN(UFLUN(ICHAN),ZFILE,2,IER,UFCPRS(ICHAN))
      IF(IER.NE.0) GO TO 100
C If netCDF : ufopen does not open it
      if (UFCPRS(ICHAN) .eq. 4) then
         call cdfopen(UFLUN(ICHAN),zfile,1,ios)
         IF(IER.NE.0) GO TO 100
      end if
C
	
      niobuf(ichan)=0
      nchbuf(ichan)=0
      if(ufcprs(ichan).eq.2) then
C  write 12 byte fixed header
         call ubufo(uflun(ichan),uiobuf(1,ichan),niobuf(ichan),
     >                ibhdr,12)
      endif
C
C  ok, ready.
      UFSTAT(ICHAN)=1
C
      RETURN
C--
C  ERROR
 100  CONTINUE
      CALL UFSND0(UFELUN,'UFOPWR:  FAILED TO OPEN FILE',ZFILE)
      WRITE(UFELUN,9001) IER
 9001 FORMAT(
     >  '  IOSTAT CODE: ',1X,I3,' - HIT CARRIAGE RETURN')
      IER=1
      CLOSE(UNIT=UFLUN(ICHAN),ERR=190)
 190  CONTINUE
      CALL UFPAUS(1)
      RETURN
      	END
C******************** END FILE UFOPWR.FOR ; GROUP UFILES ***************
