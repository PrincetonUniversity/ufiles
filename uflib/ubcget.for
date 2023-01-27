      subroutine ubcget(ichan,ieod)
C
      use UFILES
C
C uncompress and
C  read character buffer; set flag if end of data detected.
C
c  dmc 21 June 1999:  moved from UFILES INCLUDE file -- no way
c  to create f77/f90 portable include file using integer*1 on SUNs...
c
      integer*1 uiobuf(ufbbuf,ufpach)  ! i/o buffers
      integer*1 uchbuf(ufcbuf,ufpach)  ! char compression buffers
      common/ufilbyte/ uiobuf,uchbuf
c
C local buffer for length information
C
      integer*1 ilbuf(8)
      integer*1 bi2buf(2,ufcbuf)
      integer*1 eod, eod2, eod3
C
C----------------------------------------
C
      eod  = -1
      eod2 = -2
      eod3 = 3
      ioed=0
C
C  peek at next byte; flag if its eod code (-1)
C
      if(uiobuf(niobuf(ichan),ichan) .eq. eod) then
         ieod=1
         go to 1000
      endif
C
C  OK there is some data to get.
C
      ilun=uflun(ichan)
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ilbuf,8)
C
      if(ilbuf(1).eq. eod) then
         ieod=1
         go to 1000  ! end of file
      endif
C
      if(ilbuf(1).ne.eod3) then
         write(6,'('' ?ubcget:  expected compressed char data'')')
         go to 999
      endif
C
C  get the unpacked length expected (bytes)
C
      call ubi3dcod(ilbuf,2,ilenb)
C
C  get the compressed data length (words)
C
      call ubi3dcod(ilbuf,6,iwords)
      ilenc=iwords*4
C
C  read the compressed data
C
      call ubufi(ilun,uiobuf(1,ichan),niobuf(ichan),ufpack,ilenc)
C
C  decompress the data
C
      call xpand2(ufpack,iwords,bi2buf,1,ilenb,ierr)
      if(ierr.ne.0) then
         write(6,'('' ?ubcget:  decompress error, ierr='',i3)') ierr
         go to 999
      endif
C
      do i=1,ilenb
         uchbuf(i,ichan)=bi2buf(2,i)
      enddo
      if(ilenb.lt.ufcbuf) uchbuf(ilenb+1,ichan)=eod2  ! mark end of buffer
      nchbuf(ichan)=1
C
      go to 1000
C
 999  continue
C
 1000 continue
      return
      end
