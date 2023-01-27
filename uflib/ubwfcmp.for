      subroutine ubwfcmp(ilun,zvals,numvals,zavrat)
C
C  UFILES low level i/o routine -- write real array
C   write with compression if more that 25 words long
C
      use UFILES
C
      real zvals(numvals)
C
      integer*1 ilbuf(4)    ! small local buffer
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
C
C-----------------------------------------------------
C
      CALL UFLCHK(ILUN,'ubwfcmp   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      call ubcflush(ichan)   ! flush compressed character output buffer
C
      if(numvals.gt.50) then
         itype=5  ! compress
      else
         itype=4  ! don't compress
      endif
      ilbuf(1)=itype
      call ubi3ncod(numvals,ilbuf,2)
      ilenb=4
C
C  transfer the buffer -- no. of floating pt words to follow ...
C
      call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,ilenb)
C
C---------------
C
C  loop through fp numbers, 1 block at a time
C
      iwarnuf=0
      iwarnof=0
C
      zsumrat=0.0
      znorm=0.0
C
      do iblk=1,numvals,ufmaxr
         ilim=min(iblk+ufmaxr-1,numvals)
         inum=ilim-iblk+1
C
C  IEEE conversion or copy
C
         call vmsieee(6,zvals(iblk),ilim-iblk+1,ufieee,1)
C
C  data now in ufieee[1 to inum]
C
         if(itype.eq.4) then
C  uncompressed write
C   standardized byte order
            call packordr(ufieee,inum,1)
            ibytes=4*inum
            call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ufieee,ibytes)
         else
C  setup for compression:  break into two I*2 streams
            call unpki2(ufieee,uffhi,ufflo,inum)
C  compress and write
            call ubwfi2(ilun,ichan,uffhi,'hi',inum,zrat)
            zsumrat=zsumrat+float(inum)*zrat
            znorm=znorm+float(inum)
            call ubwfi2(ilun,ichan,ufflo,'lo',inum,zrat)
            zsumrat=zsumrat+float(inum)*zrat
            znorm=znorm+float(inum)
         endif
      enddo
      go to 1000
C
C  errors
C
 999  continue
      write(6,'('' ?UFLIB:  ubwfcmp low level i/o routine:  error.'')')
C
C  exit
C
 1000 continue
      if(znorm.gt.0.0) then
         zavrat=zsumrat/znorm
      else
         zavrat=1.0
      endif
C
      return
      end
