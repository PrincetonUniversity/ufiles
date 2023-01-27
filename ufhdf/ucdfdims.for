C  Get all dimensions of a HDF U-file
C
C Scalar Values -> 1st Variables
C Data
C
C 08/14/97 CAL
C--------------------------------------------------------------------------
      subroutine ucdfdim(lun,ndim,dims,nsc,iproc,retn)
C
      use UFILES
      implicit none
 
      include "netcdf.inc"
C Input:
      integer lun
      integer ndim
C Output:
      integer nsc
      integer dims(ndim)
      integer iproc
      integer retn
 
C Definitions for processing
      integer       ichan, file_id
      integer       i
      character*10  dimnam(3)
      integer       nvars, ngatts, unlimid
      integer       rank
C
C-------------------------------------------------------------------------
C
C get file name
      call uflchk(lun,'uhdfdim',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C---------------------------------------------------------------------------
C
C Inquire about file
         retn = nf_inq(file_id, rank, nvars, ngatts, unlimid)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(retn)
 9000       format(' % UCDFDIM: nf_inq'/,
     >           '     ',a)
            return
         else
            if (unlimid .gt. 1) rank = rank - 1
C            type *,'rank =',rank
         end if
C Get number of scalors
C
      retn = nf_get_att_int(file_id,NF_GLOBAL,'num_scalars',nsc)
      if (retn .ne. 0) then
         write(ufelun,9001) NF_STRERROR(retn)
 9001    format(' % UCDFDIM: nf_get_att_in -- num_scalars '/,
     >       '     ',a)
         return
      end if
C
C Inquire dimensions
         do i=1,rank
            retn = nf_inq_dim(file_id,i,dimnam(i),dims(i))
            if (retn .ne. NF_NOERR) then
               write(ufelun,9002) i, NF_STRERROR(retn)
 9002          format(' % UCDFDIM: nf_inq_dim',i2/,
     >              '     ',a)
               return
            end if
C            type *,dimnam(i),dims(i)
         end do
      return
      end
 
 
 
 
