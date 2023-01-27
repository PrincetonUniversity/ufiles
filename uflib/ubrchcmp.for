      subroutine ubrchcmp(ilun,zchar,ieod,ier)
C
C  read compressed character data.  Subroutine ubcget actually does the
C  decompression.  This routine reads uncompressed data from a buffer and
C  calls ubcget to get more if needed.
C
C  buffer contains data in form <string><DEL><string><DEL>...
C  as a byte stream.  all characters are printable except <DEL>=127 ascii.
C  ascii 9 (horizontal tab) may occur.
C
C  detect end of file (end of data code => set ieod=1)
C
      use UFILES
C
      character*(*) zchar
C
      integer*1 ibyte
      integer*1 eod2
C
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
      eod2 = -2
C-----------------------------------------------------
C
      ier=0
C
      CALL UFLCHK(ILUN,'ubwchar   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      ilenz=len(zchar)
C
      ieod=0
C
      iget=0
      if((nchbuf(ichan).le.0).or.(nchbuf(ichan).gt.UFCBUF)) then
         iget=1
      else
         if(uchbuf(nchbuf(ichan),ichan).eq.eod2) then
            iget=1
         endif
      endif
C
      if(iget.eq.1) then
         call ubcget(ichan,ieod)
         if(ieod.eq.1) go to 1000
      endif
C
C  copy string to output
C
      zchar=' '
      ist=nchbuf(ichan)-1
      i=0
 20   continue
      i=i+1
      ist=ist+1
      if(uchbuf(ist,ichan).eq.127) then
         nchbuf(ichan)=ist+1
         go to 1000                     ! line feed detected.
      endif
      if(i.gt.ilenz) then
         write(6,'('' ?ubrchcmp:  string too long for passed arg.'')')
         go to 999
      endif
C
      zchar(i:i)=char(uchbuf(ist,ichan))
C
      go to 20
C-------------------
C
 999  continue
      ier=1
C
 1000 continue
      return
      end
