C******************** START FILE UFCLOS.FOR ; GROUP UFILES ******************
C--------------------------------------------------------
C  UFCLOS
C  CLOSE FILE ASSOCIATED WITH PASSED CHANNEL NUMBER
C
      SUBROUTINE UFCLOS(ILUN_IN)
C
      USE UFILES
c
C
C  CAL 28 May 2003:   If ILUN_IN < 0: disconnect from MDS
c                     should only be called if one is done with all Ufiles
c 
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
C
      integer ilun, ilun_in
c
c-----------------------------------------
c
      CALL UFINIT
      ilun = abs(ilun_in)
      CALL UFLCHK(ILUN,'UFCLOS    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
      ISTAT=UFSTAT(ICHAN)
      IF(ISTAT.EQ.0) WRITE(UFELUN,9001) ILUN
 9001 FORMAT(' % UFCLOS: ATTEMPT TO CLOSE IMPROPERLY OPENED FILE:'/
     >  '   UNIT=',I5)
      IF(ISTAT.EQ.1) WRITE(UFELUN,9002) ILUN
 9002 FORMAT(' % UFCLOS: CLOSING FILE BEFORE DATA WRITE'/
     >  '   UNIT= ',I3)
C
      if(ufcprs(ichan).eq.2) then
C  flush output buffer
         if((istat.eq.1).or.(istat.eq.2)) then
            call ubcflush(ichan)
            call ubufo(uflun(ichan),uiobuf(1,ichan),niobuf(ichan),
     >                   uiobuf(1,ichan),0)
         endif
C
         call cftclose(uflun(ichan))
         go to 100
C
      else if(ufcprs(ichan).eq.4) then     ! netCDF
         call cdfclos(ufid(ichan))
         go to 100
      endif
C
      CLOSE(UNIT=ILUN,ERR=100)
 100  CONTINUE
C 
C check if should disconnect from MDS
      if (ilun_in .lt. 0 .and. mds_socket(ichan) .ne. 0) then
         write(ufelun,*)'%ufclos: disconnect ',mds_socket(ichan)
         istat = MdsSetSocket(mds_socket(ichan))
         Call MdsCacheDisconnect
         mds_socket(ichan)=0
      endif
      UFSTAT(ICHAN)=0
      mds_shot(ichan)=0
C  CLEAR COMPRESSED BINARY DATA WRITE FLAG
      if(ufcprs(ichan).gt.0) UFCPRS(ICHAN)=0
C
      RETURN
      	END
C******************** END FILE UFCLOS.FOR ; GROUP UFILES ******************
