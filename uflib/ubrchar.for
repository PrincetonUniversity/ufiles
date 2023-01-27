      subroutine ubrchar(ilun,zchar,ier)
C
C  UFILES low level i/o routine
C  read short uncompressed character data string
C  ...written by ubwchar
C
      use UFILES
C
      character*(*) zchar
C
      parameter (ilblen=128)
 
      integer*1 ilbuf(ilblen),ibyte    ! small local buffer
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
C-----------------------------------------------------
C
      ier=0
C
      CALL UFLCHK(ILUN,'ubrchar   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      ioff=4  ! space needed as data header
C
      zchar=' '
      ilen=len(zchar)
C
      imaxl=ilblen-ioff
C
C  read and check the length of the string
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,4)
      if(ilbuf(1).ne.2) then
         write(6,'('' ?ubrchar:  expected uncompressed char.'')')
         go to 999
      endif
C
      call ubi3dcod(ilbuf,2,ilenb)
C
      if(ilenb.gt.imaxl) then
         write(6,'('' ?ubrchar -- data string exceeds buffer.'')')
         go to 999
      endif
C
      if(ilenb.gt.ilen) then
         write(6,'('' ?ubrchar -- data string too long for arg.'')')
         go to 999
      endif
C
C  read the string data
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,ilenb)
C
C  place in output string
C
      do i=1,ilenb
         zchar(i:i)=char(ilbuf(i))
      enddo
      go to 1000
C
C  errors
C
 999  continue
      ier=1
      write(6,'('' ?UFLIB:  ubrchar low level i/o routine:  error.'')')
C
C  exit
C
 1000 continue
      return
      end
