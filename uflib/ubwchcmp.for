      subroutine ubwchcmp(ilun,zchar)
C
C  buffer input string for eventual compression and output
C    error if string is too long for empty buffer;
C    if string exceeds partially full buffer, flush buffer and
C    try again.
C
C  buffer consists of byte sequence: <string><del><string><del>...
C    input string is expected to be one line of printable data; all
C    control characters (except ^I, tab) will be suppressed; trailing
C    blanks are suppressed.  <del> = ascii(127).
C
      use UFILES
C
      character*(*) zchar
C
      integer*1 ibyte
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
      integer iz
c
C-----------------------------------------------------
C
      CALL UFLCHK(ILUN,'ubwchar   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
C  determine length of input string, not counting trailing blanks
C
      ilen=len(zchar)
      do i=ilen,1,-1
         ibyte=ichar(zchar(i:i))
         if(ibyte.ne.127) then
            if((ibyte.eq.9).or.(ibyte.gt.32)) go to 10
         endif
      enddo
C
      i=0
 10   continue
      ilenb=i+1    ! allow for the 127 = string terminator
C
      if(ilenb.gt.UFCBUF) then
         write(6,'('' ?ubwchcmp:  input string exceeds buffer size'')')
         go to 999
      endif
C
      if(ilenb+nchbuf(ichan).gt.ufcbuf) then
         call ubcflush(ichan)     ! flush buffer
      endif
C
C  copy the string
C
      do i=1,ilenb-1
         ibyte=ichar(zchar(i:i))
         if(ibyte.eq.127) ibyte=32
         if((ibyte.ne.9).and.(ibyte.lt.32)) ibyte=32
         ia=nchbuf(ichan)+i
         uchbuf(ia,ichan)=ibyte
      enddo
C  terminating character (ascii code 127):
      ia=nchbuf(ichan)+ilenb
      uchbuf(ia,ichan)=127
C
C  this much of buffer now in use:
      nchbuf(ichan)=ia
C
      go to 1000
C
 999  continue
C
 1000 continue
      return
      end
