C Open a netCDF Ufile
      subroutine cdfopen(lun,name,option,ierr)
C
      use UFILES
      implicit none
C
      include "netcdf.inc"
C
C Input
      integer       lun
      character*(*) name
      integer       option      ! 0 = read / 1 = write
C
      integer ierr
C
      integer file_id, ichan
C
C---------------------------------------------------------------------
C
      call uflchk(lun,'cdfopen',ichan,ierr)
      if (ierr .ne. 0) return
 
      if (option .eq. 1) then   ! write
         file_id=0
         ierr = nf_create(name,NF_CLOBBER,file_id)
         if (ierr .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(IERR)
 9000       format(' ? CDFOPEN: nf_create '/,'     ',A)
            return
         end if
C
      else if (option .eq. 0) then    ! open for read
         file_id=0
         ierr = nf_open(name,NF_NOWRITE,file_id)
         if (ierr .ne. NF_NOERR) then
            write(ufelun,9001) NF_STRERROR(IERR)
 9001       format(' ? CDFOPEN: nf_open '/,'     ',A)
            return
         end if
      end if
C
      ufid(ichan) = file_id
      return
      end
