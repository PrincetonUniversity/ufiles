C Read 3D CDF U-file
C Called from uf3drd
C
C 08/15/97 CAL
C
C----------------------------------------------------------------------------
C
      subroutine ucdf3r(lun,option,
     &            f,n_f1,n_f2, x,n_x, y,n_y, xz,n_z, iproc,
     &            nx, ny, nz, iflag,
     &            flab,xlab,ylab,xzlab,
     &            retn)
 
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input:
      integer lun
      integer option   ! 1 = get Dimensions
C                      ! 2 = get data arrays
      integer n_f1, n_f2, n_x, n_y, n_z, iflag
C Return:
      integer      nx, ny, nz, iproc
      character*30 flab, xlab, ylab, xzlab
      real         x(n_x), y(n_y), xz(n_z), f(n_f1,n_f2,n_z)
      integer retn
 
C Definitions for CDF
      integer   nvars, ngatts, unlimid, nsc
      integer   var_id
      integer   nvdims(3)
      character*10 dimnam(3)
      integer dims(3), st(3), cnt(3)
      integer rank
 
C Definitions for processing
      integer       ichan, file_id
 
      character*20  fl,   xl,   yl, xzl
      character*10  fu,   xu,   yu, xzu
      integer       i, j, k
C--------------------------------------------------------
C
C
C get file name
      call uflchk(lun,'ucdf3r',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
      if (option .eq. 1) then       ! get Dimensions
C Inquire about file
         retn = nf_inq(file_id, rank, nvars, ngatts, unlimid)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(retn)
 9000       format(' % UCDF3R: nf_inq'/,
     >           '     ',a)
            return
         end if
C
C Inquire dimensions
         do i=1,3
            retn = nf_inq_dim(file_id,i,dimnam(i),dims(i))
            if (retn .ne. NF_NOERR) then
               write(ufelun,9001) i, NF_STRERROR(retn)
 9001          format(' % UCDF3R: nf_inq_dim',i2/,
     >              '     ',a)
               return
            end if
C            type *,dimnam(i),dims(i)
         end do
         nx = dims(1)
         ny = dims(2)
         nz = dims(3)
C-----------------------------------------------------------------------
      else if (option .eq. 2) then
C
 
C Get data attributes
         nsc = nvars - rank - 1
         var_id = nsc+1
         retn = nf_get_att_text(file_id,var_id,'long_name',fl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
 9002       format(' % UCDF3R: nf_get_att_text id =',i3/,
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
         end if
         xlab=xl(1:20)//xu(1:10)
 
C get Y attributes
         var_id = var_id + 1
         retn = nf_get_att_text(file_id,var_id,'long_name',yl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         retn = nf_get_att_text(file_id,var_id,'units',yu)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         ylab=yl(1:20)//yu(1:10)
C
C get Z attributes
         var_id = var_id + 1
         retn = nf_get_att_text(file_id,var_id,'long_name',xzl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         retn = nf_get_att_text(file_id,var_id,'units',xzu)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
         xzlab=xzl(1:20)//xzu(1:10)
C
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
         if (iflag .eq. 1) then    ! Read continuously into buffer
            retn = nf_get_var_real(file_id, var_id, f)
            if (retn .ne. NF_NOERR) then
               write(ufelun,9003) var_id, NF_STRERROR(retn)
 9003          format(' % UCDF3R: nf_get_var_real id =',i3/,
     >              '     ',a)
               return
            end if
         else                     ! Read according to dimensions
            st(1) = 1
            cnt(1) = dims(1)      ! x count
            cnt(2) = 1
            cnt(3) = 1
            do k = 1, dims(3)
               st(3) = k
               do j = 1,dims(2) ! For each Y : read slab into f
                  st(2) = j     ! Start of slab
                  retn = nf_get_vara_real(file_id, var_id,st,cnt,
     >                                    f(1,j,k))
                  if (retn .ne. NF_NOERR) then
                     write(ufelun,9004) var_id, NF_STRERROR(retn)
 9004                format(' % UCDF3R: nf_get_vara_real id =',i3/,
     >                    '     ',a)
                     return
                  end if
               end do
            end do
         end if
C
C Read Y
         var_id = nsc+3
         retn = nf_get_var_real(file_id, var_id, y)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
            return
         end if
      end if
 
C Read Z
!MG bug 03/03/2014
!      var_id = nsc+3
      var_id = nsc+4
      retn = nf_get_var_real(file_id, var_id, xz)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9002) var_id, NF_STRERROR(retn)
         return
      end if
 
      return
      end
 
 
 
 
 
 
 
 
 
 
