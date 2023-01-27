      subroutine ubrfcmp(ilun,zvals,numvals,ier)
C
C  UFILES low level i/o routine -- read compressed real array
C   data.
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
      ier=0
C
      CALL UFLCHK(ILUN,'ubrfcmp   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      ilenb=4
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,ilenb)
      itype=ilbuf(1)
      if((itype.lt.4).or.(itype.gt.5)) then
         write(6,'('' ?ubrfcmp:  unexpected data type code.'')')
         go to 999
      endif
C
      call ubi3dcod(ilbuf,2,inum)
C
      if(inum.ne.numvals) then
         write(6,'('' ?ubrfcmp:  unexpected data count.'')')
         go to 999
      endif
C
C---------------
C
C  loop through fp numbers, 1 block at a time
C
      do iblk=1,numvals,ufmaxr
         ilim=min(iblk+ufmaxr-1,numvals)
         inum=ilim-iblk+1
C
C  IEEE conversion or copy
C
         if(itype.eq.4) then
C  uncompressed read
            ibytes=4*inum
            call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ufieee,ibytes)
            call packordr(ufieee,inum,0)
         else
C  read and decompress
            call ubrfi2(ilun,ichan,uffhi,'hi',inum)
            call ubrfi2(ilun,ichan,ufflo,'lo',inum)
C  reassemble
            call repki2(ufieee,uffhi,ufflo,inum)
         endif
C
C  convert to floating pt (if needed) and store in output array
C
         call vmsieee(6,zvals(iblk),ilim-iblk+1,ufieee,0)
C
      enddo
      go to 1000
C
C  errors
C
 999  continue
      ier=1
      write(6,'('' ?UFLIB:  ubrfcmp low level i/o routine:  error.'')')
C
C  exit
C
 1000 continue
      return
      end

