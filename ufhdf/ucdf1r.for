C Read 1D CDF U-file
C Called from uf1drd
C
C 07/25/97 CAL
C
C----------------------------------------------------------------------------
      subroutine ucdf1r(lun,option,
     &            f, x, n_x, nx, iproc,
     &            flab, xlab,
     &            retn)
C
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input:
      integer lun
      integer option   ! 1 = get Dimensions
C                      ! 2 = get data arrays
      integer n_x
C Return:
      integer      nx, iproc
      character*30 flab, xlab
      real         x(n_x), f(n_x)
      integer retn
 
C Definitions for CDF
      integer dims
      integer rank
      integer nvars, ngatts, unlimid, nsc
      integer var_id
      integer nvdims
      character*10 dimnam
 
C Definitions for processing
      integer       file_id,  ichan
      character*20  fl,   xl
      character*10  fu,   xu
      integer       i,    ierr, l
 
      save    rank
C--------------------------------------------------------
C
C get file name
      call uflchk(lun,'ucdf1r',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
 
C
      if (option .eq. 1) then       ! get Dimensions
 
C Inquire about file
         retn = nf_inq(file_id, rank, nvars, ngatts, unlimid)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(retn)
 9000       format(' % UCDF1R: nf_inq'/,
     >           '     ',a)
            return
         end if
C
C Inquire dimensions
 
         retn = nf_inq_dim(file_id,1,dimnam,dims)
         if (retn .ne. NF_NOERR) then
            i=1
            write(ufelun,9001) i, NF_STRERROR(retn)
 9001       format(' % UCDF1R: nf_inq_dim',i2/,
     >           '     ',a)
            return
         end if
C         type *,dimnam,dims
         nx = dims
C-----------------------------------------------------------------------
      else if (option .eq. 2) then
C
 
C Get data attributes
         nsc = nvars - rank - 1
         var_id = nsc+1
         retn = nf_get_att_text(file_id,var_id,'long_name',fl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
 9002       format(' % UCDF1R: nf_get_att_text id =',i3/,
     >           '     ',a)
            return
         end if
         retn = nf_get_att_text(file_id,var_id,'units',fu)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         flab=fl(1:20)//fu(1:10)
C Get X attributes
         var_id = var_id + 1
         retn = nf_get_att_text(file_id,var_id,'long_name',xl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         retn = nf_get_att_text(file_id,var_id,'units',xu)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         else
            xlab=xl(1:20)//xu(1:10)
         end if
C
C Read X
         var_id =  nsc+2
         retn = nf_get_var_real(file_id, var_id, x)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
 
C get array data
         var_id = nsc + 1          ! Data is 1st variable after Scalars
         retn = nf_get_var_real(file_id, var_id, f)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9003) var_id, NF_STRERROR(retn)
 9003       format(' % UCDF1R: nf_get_var_real id =',i3/,
     >           '     ',a)
            return
         end if
      end if
      return
      end
 
 
 
 
 
 
