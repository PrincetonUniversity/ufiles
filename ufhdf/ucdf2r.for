C Read 2D CDF U-file
C Called from uf2drd
C
C 07/25/97 CAL
C
C----------------------------------------------------------------------------
      subroutine ucdf2r(lun,option,
     &            f,n_f1, x,n_x, y,n_y, iproc,
     &            nx, ny, iflag,
     &            flab,xlab,ylab,
     &            retn)
C
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input:
      integer lun
      integer option   ! 1 = get Dimensions
C                      ! 2 = get data arrays
      integer n_f1, n_x, n_y, iflag
C Return:
      integer      nx, ny, iproc
      character*30 flab, xlab, ylab
      real         x(n_x), y(n_y), f(n_f1,n_y)
      integer retn
 
C Definitions for CDF
      integer   nvars, ngatts, unlimid, nsc
      integer   var_id
      integer   nvdims(2)
      character*10 dimnam(2)
      integer dims(2), st(2), cnt(2)
      integer rank
      integer nxl, nxu
      integer nyl, nyu
      integer nfl, nfu
 
C Definitions for processing
      integer       ichan, file_id
 
      character*20  fl,   xl,   yl
      character*10  fu,   xu,   yu
      integer       i, j
 
      save    rank, dims
C--------------------------------------------------------
C
C
C get file name
      call uflchk(lun,'ucdf2r',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
      if (option .eq. 1) then       ! get Dimensions
C Inquire about file
         retn = nf_inq(file_id, rank, nvars, ngatts, unlimid)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(retn)
 9000       format(' % UCDF2R: nf_inq'/,
     >           '     ',a)
            return
         end if
C
C Inquire dimensions
         do i=1,2
            retn = nf_inq_dim(file_id,i,dimnam(i),dims(i))
            if (retn .ne. NF_NOERR) then
               write(ufelun,9001) i, NF_STRERROR(retn)
 9001          format(' % UCDF2R: nf_inq_dim',i2/,
     >              '     ',a)
               return
            end if
         end do
         nx = dims(1)
         ny = dims(2)
C-----------------------------------------------------------------------
      else if (option .eq. 2) then
C
C Get data attributes
         nsc = nvars - rank - 1
         var_id = nsc+1
         retn = nf_get_att_text(file_id,var_id,'long_name',fl)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) var_id, NF_STRERROR(retn)
 9002       format(' % UCDF2R: nf_get_att_text id =',i3/,
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
 9003          format(' % UCDF2R: nf_get_var_real id =',i3/,
     >              '     ',a)
               return
            end if
         else                   ! Read according to dimensions
            st(1) = 1
            cnt(1) = dims(1)      ! x count
            cnt(2) = 1
            do j = 1,dims(2)      ! For each Y : read slab into f
               st(2) = j          ! Start of slab
               retn = nf_get_vara_real(file_id, var_id,st,cnt,f(1,j))
               if (retn .ne. NF_NOERR) then
                  write(ufelun,9004) var_id, NF_STRERROR(retn)
 9004             format(' % UCDF2R: nf_get_vara_real id =',i3/,
     >                 '     ',a)
                  return
               end if
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
 
      return
      end
 
 
 
 
 
 
 
 
 
 
