      subroutine ubcpwr(ilun,data,dlbl,nd1,nd2,nz,nx,ny,ier)
C
C  write Ufile floating point data in loss free compressed binary format.
C
C  the data array is as dimensioned; the portion
C
C    (((data(i,j,k),i=1,nx),j=1,ny),k=1,nz) will be written.
C
C  if nx=nd1 and ny=nd2, then the whole array is written, in the order
C  stored.
C
C  dlbl is passed for debugging purposes -- a name for the array being
C  written.
C
C  write on UFILES channel ilun.
C
C----------------------------------
C
      use UFILES
C
      real data(nd1,nd2,nz)
      character*(*) dlbl
C
      real zarray(UFMAXR)               ! local buffer
C
C-----------------------------------
C
      ipts=nx*ny*nz                     ! no. of pts to actually write
C
      imaxu=ufmaxr                      ! buffer size
      call ubw3int(ilun,imaxu)
C
      zsum=0.0
      do idon=0,ipts-1,imaxu            ! take data in buffer size chunks
         iget=min(ipts-idon,imaxu)
C
C  get a buffer full:
         call ufg2da(data,nd1,nd2,nz,nx,ny,idon,zarray,iget)
C
C  write it out:
         call ubwfcmp(ilun,zarray,iget,zavrat)
C
         zsum=zsum+zavrat*iget
      enddo
C
      ilen=len(dlbl)
      zavrat=zsum/float(max(1,ipts))
      write(ufelun,1001) dlbl(1:1),zavrat
 1001 format(' %ubcpwr:  loss free compression storage ratio for "',
     >              a,'":',1pe10.3)
C
      return
      end
 
