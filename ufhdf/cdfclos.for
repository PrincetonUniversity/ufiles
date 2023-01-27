C Close a netCDF Ufile
      subroutine cdfclos(file_id)
C
      use UFILES
      implicit none
C
      include "netcdf.inc"
C
C Input
      integer file_id
      integer ierr
C
C---------------------------------------------------------------------
C
      ierr = nf_close(file_id)
 
      if (ierr .ne. NF_NOERR) then
         write(ufelun,9000) NF_STRERROR(IERR)
 9000    format(' ? CDFCLOS: nf_close '/,'     ',A)
      end if
      return
      end
 
