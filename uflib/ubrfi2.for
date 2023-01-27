      subroutine ubrfi2(ilun,ichan,i2array,ilbl,inum)
C
C  read and possibly decompress an integer*2 stream
C  check that number expected = number read.
C
      use UFILES
C
C-------------------------------
C
      integer ilun                      ! i/o unit
      integer ichan                     ! ufiles channel no.
      integer*2 i2array(inum)           ! i2 data, dimensioned.
      character*(*) ilbl                ! label (for debugging)
      integer inum                      ! array dimension
C
C  local:
C
      logical ipkd
C
      integer*1 ilbuf(4)    ! small local buffer
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
C
C-------------------------------
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,4)
      if(ilbuf(1).eq.0) then
         ipkd=.false.                   ! uncompressed data
         call ubi3dcod(ilbuf,2,ibytes2)
         if(ibytes2.ne.2*inum) then
            write(6,'('' ?ubrfi2:  uncompressed data length error'')')
            go to 999
         endif
         call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),i2array,ibytes2)
      else
         ipkd=.true.                    ! compressed data
         call ubi3dcod(ilbuf,2,iwords)
         ibytes=4*iwords
C  read:
         call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ufpack,ibytes)
C  decompress:
         call xpand2(ufpack,iwords,i2array,1,inum,ierr)
         if(ierr.ne.0) then
            write(6,1902) ilbl,ierr
 1902       format(' ?ubrfi2:  ',a,' data decompress ierr=',i2)
            go to 999
         endif
      endif
C
      go to 1000
C
 999  continue
C
 1000 continue
      return
      end






