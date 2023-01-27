      subroutine ubwfi2(ilun,ichan,i2array,ilbl,inum,zrat)
C
C  compress and write an integer*2 stream
C  write uncompressed if compression does not help
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
      integer*1 ilbuf(4)
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
C  attempt compression
C
      ibytes2=2*inum
C
      call cmprs2(i2array,inum,ufpack,ufmaxr,ibitu,ierr)
      if(ierr.ne.0) then
cx         write(6,1002) ilbl,ierr
cx 1002    format(' %ubwfi2:  ',a,' data did not compress, ierr=',i2)
         ipkd=.false.
         zrat=1.0
      else
         iwords=ibitu/32+1
         ibytes=4*iwords
         ipkd=ibytes.lt.ibytes2
         zrat=min(1.0,(float(ibytes)/float(ibytes2)))
cx         write(6,1003) ilbl,zrat
cx 1003    format(' %ubwfi2:  ',a,' compression ratio:  ',1pe11.4)
      endif
C
C  write
C
      if(ipkd) then
         ilbuf(1)=1
         call ubi3ncod(iwords,ilbuf,2)
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,4)
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ufpack,ibytes)
      else
         ilbuf(1)=0
         call ubi3ncod(ibytes2,ilbuf,2)
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,4)
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),i2array,ibytes2)
      endif
C
      return
      end
 
