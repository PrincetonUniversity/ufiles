      subroutine ubcprd(ilun,data,dlbl,nd1,nd2,nz,nx,ny,ier)
C
C  read Ufile floating point data in loss free compressed binary format.
C
C  the data array is as dimensioned; the portion
C
C    (((data(i,j,k),i=1,nx),j=1,ny),k=1,nz) will be read.
C
C  if nx=nd1 and ny=nd2, then the whole array is written, in the order
C  stored.
C
C  dlbl is passed for debugging purposes -- a name for the array being
C  written.
C
C  read on UFILES channel ilun.
C
C----------------------------------
C
      use UFILES
C
      integer ier
      real data(nd1,nd2,nz)
      character*(*) dlbl
C
      real zarray(UFMAXR)               ! local buffer
C
C-----------------------------------
C
      ier=0
      ipts=nx*ny*nz                     ! no. of pts to actually read
C
      call ubr3int(ilun,imaxu,ier)      ! buffer size
      if(ier.gt.0) then
         write(6,
     >   '('' ?ubcprd:  '',a,'' buffer size read error.'')') dlbl
         go to 999
      endif
      if(imaxu.gt.ufmaxr) then
         imax=ufmaxr
         write(6,9901) dlbl,imaxu,imax
 9901    format(
     >' ?ubcprd:  ',a,' buffer size recorded in file = ',i10/
     >'  exceeds program limit = ',i10)
         go to 999
      endif
C
      do idon=0,ipts-1,imaxu            ! take data in buffer size chunks
         iget=min(ipts-idon,imaxu)
C
C  get a buffer full:
         call ubrfcmp(ilun,zarray,iget,ier)
         if(ier.ne.0) then
            write(6,'('' ?ubcprd:  '',a,'' read error.'')') dlbl
            go to 999
         endif
C  copy into output array:
         call ufp2da(data,nd1,nd2,nz,nx,ny,idon,zarray,iget)
C
      enddo
C
 999  continue
      return
      end
 
