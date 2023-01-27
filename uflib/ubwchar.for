      subroutine ubwchar(ilun,zchar)
C
C  UFILES low level i/o routine
C  write short uncompressed character data string
C  ...see also ubrchar
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
      integer iz
c
C
C-----------------------------------------------------
C
      CALL UFLCHK(ILUN,'ubwchar   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      ioff=4  ! space needed as data header
C
      ilen=len(zchar)
      if(ilen.gt.ilblen-ioff) then
         write(6,'('' ?ubwchar -- input string too long.'')')
         go to 999
      endif
C
      call ubcflush(ichan)   ! flush compressed character output buffer
C
C  load the first byte with the code for uncompressed char:
C
      ibyte=2
      ilbuf(1)=ibyte
C
C  preload buffer with the code for blanks.
      ibyte=ichar(' ')
      do i=1,ilblen-ioff
         ilbuf(i+ioff)=ibyte
      enddo
C
C  load the buffer from the passed string.  Suppress control characters
C  except for ^I (horizontal tab, code 9).  Suppress write of trailing
C  blanks
C
      ilenb=ioff+1
      do i=1,ilen
         ibyte=ichar(zchar(i:i))
         if((ibyte.eq.9).or.(ibyte.gt.32)) then
            ilbuf(ioff+i)=ibyte
            ilenb=ioff+i
         endif
      enddo
C
C  transfer the buffer
C
      call ubi3ncod(ilenb-ioff,ilbuf,2)
      call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,ilenb)
C
      go to 1000
C
C  errors
C
 999  continue
      write(6,'('' ?UFLIB:  ubwchar low level i/o routine:  error.'')')
C
C  exit
C
 1000 continue
      return
      end
