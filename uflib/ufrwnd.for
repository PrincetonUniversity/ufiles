C----------------------------------------------------------------------
C  UFRWND  -- REWIND OPEN UFILE AND RESET INTERNAL STATUS
C
      SUBROUTINE UFRWND(ILUN,IER)
C
      USE UFILES
C
      CALL UFINIT
C
C  KNOWN CHANNEL?
      CALL UFLCHK(ILUN,'UFRWND    ',ICHAN,IER)
C
      IF(IER.NE.0) GO TO 9000
C
C  FILE MUST BE OPEN...
C
      ISTAT=UFSTAT(ICHAN)
      IF((ISTAT.NE.5).AND.(ISTAT.NE.3)) THEN
        CALL UERSTA(ICHAN,'UFRWND    ')
        GO TO 9000
      ENDIF
C
C  OK
      UFSTAT(ICHAN)=3
      call ufrwnd_do(ilun,ichan,ufcprs(ichan),ier)
C
      IER=0
C
      RETURN
C
C----------
C  ERRORS
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ? DATA NOT REWOUND DUE TO ERROR - HIT RETURN')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=5
      RETURN
      	END
C-------------------------------------------------------
      subroutine ufrwnd_do(ilun,ichan,kmprs,ier)
C
      use UFILES
C
      integer*1 ibhdr(12)
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
C  do a rewind
C
      if((ufcprs(ichan).lt.2).and.(ufcprs(ichan).ge.0)) then
C  ascii or old binary format
         REWIND ILUN
      else if(ufcprs(ichan).eq.2) then
C  new binary format
         call cftrewnd(ilun,ier)
C  reset buffers
         niobuf(ichan)=0
         nchbuf(ichan)=0
C  reread the first 12 bytes (don't bother checking again)
         call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ibhdr,12)
      endif
C
      return
      end
