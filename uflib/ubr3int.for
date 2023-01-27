      subroutine ubr3int(ilun,ivalue,ier)
C
C  UFILES low level i/o routine
C  read 3 byte unsigned integer
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
C-----------------------------------------------------
C
      ier=0
C
      ivalue=0
C
      CALL UFLCHK(ILUN,'ubr3int   ',ICHAN,IER)
      IF(IER.NE.0) go to 999
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,4)
      if(ilbuf(1).ne.1) then
         write(6,'('' ?ubr3int:  expected integer not found.'')')
         go to 999
      endif
      call ubi3dcod(ilbuf,2,ivalue)
C
      go to 1000
C
C  errors
C
 999  continue
      write(6,'('' ?UFLIB:  ubr3int low level i/o routine:  error.'')')
      ier=1
C
C  exit
C
 1000 continue
      return
      end
