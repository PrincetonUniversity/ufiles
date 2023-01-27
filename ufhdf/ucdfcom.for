C ucdfcom - Read/Write CDF - Ufile Comment
C
      subroutine ucdfcom(lun,zbuf,ilen,rw,loop,retn)
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input:
      integer      lun      !
      integer      rw       ! 0: read
C                           ! 1: write
      integer      loop     ! 1: Start
C                           ! >1: Loop
C                           !
      integer      ilen     ! length of comment if Write
C Return:
      integer      retn
C     integer      ilen     ! length of comment if Read
      character*(*) zbuf    ! buffer with comments
C
C Definitions for processing
      integer ichan, file_id
      integer dim_id, com_id, dim_l
      integer start,  icnt
C
      character*1     cnt  ! Length of comment passed to netCDF
C
C---------------------------------------------------------------------------
C
C get file name
         call uflchk(lun,'ucdfcom',ichan,retn)
         if (retn .ne. 0) return
         file_id = ufid(ichan)
C
C Read
C------
      if (rw .eq. 0) then       ! Read
C
         if (loop .eq. 1) then
 
            retn = nf_inq_unlimdim(file_id, dim_id)
            if (retn .ne. 0) then
               write(ufelun,*) 'no comment'
               return
            end if
            retn = nf_inq_dimlen(file_id, dim_id, dim_l)
            if (retn .ne. 0) then
               write(ufelun,9007)  NF_STRERROR(retn)
 9007          format(' ? UCDFCOM: - error getting comment length'/a)
               return
            end if
            retn = nf_inq_varid(file_id, 'COMMENT', com_id)
            if (retn .ne. 0) then
               write(ufelun,9005)  NF_STRERROR(retn)
 9005          format(' ? UCDFCOM: - error getting comment id'/a)
               return
            end if
            start = 1
         end if
C
         if (start .gt. dim_l) then    ! end of comment
            ilen = 0
            return
         end if
C Read Record length
         retn = nf_get_vara_text(file_id, com_id, start, 1, cnt)
         if (retn .ne. 0) then
            ilen = 0
            write(ufelun,9009)  loop, NF_STRERROR(retn)
 9009       format(' ? UCDFCOM: - error reading comment length',i4/a)
            return
         end if
C Read Record
         start = start + 1
         icnt=ichar(cnt)
         retn = nf_get_vara_text(file_id, com_id, start, icnt, zbuf)
         if (retn .ne. 0) then
            ilen = 0
            write(ufelun,9006)  loop, NF_STRERROR(retn)
 9006       format(' ? UCDFCOM: - error reading comment',i4/a)
            return
         end if
         ilen = icnt
         start = start + ilen
      else
C Write Comment
C==============
C
C Enter Define Mode
C------------------
         if (loop .eq. 1) then
            retn = nf_redef(file_id)
            if (retn .ne. 0) then
               write(ufelun,9000)  NF_STRERROR(retn)
 9000          format(' ? UCDFCOM: - nf_redef '/a)
               return
            end if
            retn = nf_def_dim(file_id,'text',NF_UNLIMITED, dim_id)
            if (retn .ne. 0) then
               write(ufelun,9001)  NF_STRERROR(retn)
 9001          format(
     >            ' ? UCDFCOM: - error defining unlimited dimension',
     >            /a)
               return
            end if
            retn = nf_def_var(file_id, 'COMMENT', NF_CHAR, 1, dim_id,
     >                        com_id)
            if (retn .ne. 0) then
               write(ufelun,9002)  NF_STRERROR(retn)
 9002          format(' ? UCDFCOM: - error defining comment'/a)
               go to 9100
            end if
 
            retn = nf_enddef(file_id)
            if (retn .ne. 0) then
               write(ufelun,9003)  NF_STRERROR(retn)
 9003          format(' ? UCDFCOM: - nf_enddef'/a)
               go to 9100
            end if
            start = 1
         end if
C
C Write comment to file
C-----------------------
C Write record length
         cnt = char(ilen)
         retn = nf_put_vara_text(file_id, com_id, start, 1,
     >                        cnt)
         if (retn .ne. 0) then
            write(ufelun,9008)  loop, NF_STRERROR(retn)
 9008       format(' ? UCDFCOM: - error writing comment count',i4/a)
            go to 9100
         end if
C Write record
         start = start + 1
         retn = nf_put_vara_text(file_id, com_id, start, ilen,
     >                        zbuf)
         if (retn .ne. 0) then
            write(ufelun,9004)  loop, NF_STRERROR(retn)
 9004       format(' ? UCDFCOM: - error writing comment',i4/a)
            go to 9100
         end if
         start = start + ilen
      end if
 
      return
 
 9100 continue
      return
      end
 
 
 
 
 
 
