      subroutine ubw3int(ilun,ivalue)
C
C  UFILES low level i/o routine
C  write 3 byte unsigned integer
C
      use UFILES
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
C-----------------------------------------------------
C
      CALL UFLCHK(ILUN,'ubw3int   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      call ubcflush(ichan)   ! flush compressed character output buffer
C
      ilbuf(1)=1
      call ubi3ncod(ivalue,ilbuf,2)
      ilenb=4
C
C  transfer the buffer
C
      call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,ilenb)
C
      go to 1000
C
C  errors
C
 999  continue
      write(6,'('' ?UFLIB:  ubw3int low level i/o routine:  error.'')')
C
C  exit
C
 1000 continue
      return
      end
