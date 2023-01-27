      subroutine ubcflush(ichan)

      use UFILES
 
C  Modified:
C  04/04/96  TBT  Added Integer ibits becuase
C                  ibits is a reserved word for HP.
C
C  character buffer full:  compress and write.
C
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
      Integer ibits
C
C  local buffer for length information
C
      integer*1 ilbuf(8)
      integer*1 bi2buf(2,ufcbuf)
C
C-----------------------------------------
C
      if(nchbuf(ichan).gt.0) then
C
         do i=1,nchbuf(ichan)
            bi2buf(1,i)=0
            bi2buf(2,i)=uchbuf(i,ichan)
         enddo
C
         call cmprs2(bi2buf,nchbuf(ichan),ufpack,UFMAXR,ibits,ierr)
C
         if(ierr.ne.0) then
            write(6,'('' ?ubcflush:  cmprsi error:  '',i3)') ierr
            go to 999
         endif
C
         iwords=ibits/32 + 1
         ibytes=iwords*4
C
         ilbuf(1)=3
         call ubi3ncod(nchbuf(ichan),ilbuf,2)
C
         ilbuf(5)=1
         call ubi3ncod(iwords,ilbuf,6)
C
C  write header
         ilun=uflun(ichan)
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,8)
C  write data
         call ubufo(ilun,uiobuf(1,ichan),niobuf(ichan),ufpack,ibytes)
C
C  done.
C
         nchbuf(ichan)=0
C
      endif
      go to 1000
C
 999  continue
C
 1000 continue
      return
      end
