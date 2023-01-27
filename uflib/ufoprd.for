C******************** START FILE UFOPRD.FOR ; GROUP UFILES *************
C------------------------------------------------------------
C  UFOPRD
C  OPEN FILE FOR READ
C   ARGUMENTS-- ILUN-- FILE LOGICAL UNIT NUMBER
C     for netcdf file ilun will be changed and coresponds to file ID
C     ISHOT-- DATA SHOT NUMBER (5 DIGITS)
C     IER-- (input = kludge for trdat) if 77: MDS already connected
C     IER-- (OUTPUT) ERROR RETURN CODE
C      0=NORMAL
C      1=FILE NOT OPENED
C      2=FILE NOT OPENED-- TRIED FROM JUKEBOX WITHOUT SUCCESS
C
C  DMC/Dick Wieland 10 Sep 1991 -- Unix portability changes
C    Jukebox interface not available on the Unix workstations
C
C-------------------------------------------
C
      SUBROUTINE UFOPRD(ILUN,ISHOT,IER)
C
      USE UFILES
C
      integer*1 ibhdr(12),ibref(12)
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
C
      CHARACTER*512 ZFILE
      integer       ikludge
C
      data ibref/0,127,0,127,0,127,0,127,0,0,0,2/
C------------------------------------------------------------------
C 12/14/01:CAL
C If called from trdat (mds_connect - troprd):
C           ier=77 = do not re-connect to MDSplus
C           pass to ufopen_mds
C
      if (ier .eq. 77) then
         ikludge=ier
      else
         ikludge=0
      endif
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UFOPRD    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 1000
      IF(UFSTAT(ICHAN).NE.0) THEN
        CALL UERSTA(ICHAN,'UFOPRD    ')
        GO TO 190
      ENDIF
C
      if(ufcprs(ichan).eq.-1) then
         ier=ikludge
         call ufopen_mds(ichan,ishot,ier,1)
         if(ier.ne.0) then
            go to 190
         else
            go to 1000
         endif
      endif
C
      CALL UFSNAM(ISHOT,ZFILE,IER,ILUN)
      IF(IER.NE.0) GO TO 1000
      IF((UFILEV-10*(UFILEV/10)).GE.1)
     >     CALL UFSND0(UFELUN,'NAMED FILE',ZFILE)
C
      UFCPRS(ICHAN)=-1  ! inquire ascii or binary in UFOPEN:
      CALL UFOPEN(UFLUN(ICHAN),ZFILE,1,IER,UFCPRS(ICHAN))
      IF(IER.NE.0) GO TO 100
C If netCDF : ufopen does not open it
      if (UFCPRS(ICHAN) .eq. 4) then
         call cdfopen(UFLUN(ICHAN),zfile,0,ios)
!         ilun=ufid(ICHAN)
!         UFLUN(ICHAN)=ilun
         IF(IER.NE.0) GO TO 100
      end if
C
      go to 800  ! check binary header if appropriate
C--
C  ERRORS
 100  CONTINUE
      IF(IER.EQ.UFIERFNOF(UFLUN(ICHAN))) THEN   !File not found on disk
C
         IER=1
         CALL UFSND0(UFELUN,
     >      '%UFOPRD:  not found on unix magnetic disk: ',ZFILE)
      end if
 
      CLOSE(UNIT=UFLUN(ICHAN),ERR=190)
C
C  after an error...
C
 190  CONTINUE
 
      CALL UFPAUS(1)
      go to 1000
C-------------------------------
C  file open successful, check binary file header
C
 800  continue
C
C  check header if new binary format detected.
C
      niobuf(ichan)=0
      nchbuf(ichan)=0
      if(ufcprs(ichan).eq.2) then
         call ubufi(uflun(ichan),uiobuf(1,ichan),niobuf(ichan),
     >                ibhdr,12)
         do ii=1,12
            if(ibhdr(ii).ne.ibref(ii)) then
      	 ier=1
      	 CALL UFSND0(UFELUN,
     >	'%UFOPRD:  byte stream header error',ZFILE)
      	 go to 190
            endif
         enddo
      endif
      UFSTAT(ICHAN)=3
      GO TO 1000
C-------------------------------
C  exit
C
 1000 continue
      RETURN
        END
C--------------------------------------------------------------------------
