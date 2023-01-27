C ucdfhed - Read/Write netCDF - Ufile Header
C
C 08/14/97 C.Ludescher
C
      subroutine ucdfhed(lun,ishot,tdev,rank,ftype,zdate,zsdate,
     &                   rw,retn)
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Definitions for CDF
 
C Input:
      integer      lun      !
      integer      rw       ! 0: read
C                           ! 1: write
C Return:
      integer      retn
      integer      rank     ! number dimensions
      integer      ishot    ! shot number
      integer      ftype    ! file type : netCDF = 4
      character*4  tdev     ! 4 char tokamak code
      character*10 zdate    ! File write date
      character*10 zsdate   ! shot (original) date
 
 
C Definitions for processing
      integer ichan
      integer file_id
 
      integer nvars, ngatts, unlimid
C---------------------------------------------------------------------------
C
C get file name
      call uflchk(lun,'ucdfhed',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
C Read
C------
      if (rw .eq. 0) then       ! Read
C Inquire about file
         retn = nf_inq(file_id, rank, nvars, ngatts, unlimid)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9004) NF_STRERROR(retn)
 9004       format(' ? UCDFHED: nf_inq',A)
            return
         end if
         if (unlimid .gt. 1) rank = rank -1
C         type *,'rank =',rank
C         type *,'n.. =',nvars, ngatts, unlimid
 
         retn = nf_get_att_int(file_id,NF_GLOBAL,'shot',ishot)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9005) NF_STRERROR(retn)
 9005       format(' ? UCDFHED: nf_put_get_int -- shot'/,'     ',A)
            return
         end if
 
         retn = nf_get_att_text(file_id,NF_GLOBAL,'date',zdate)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9006) NF_STRERROR(retn)
 9006       format(' ? UCDFHED: nf_get_att_int -- date '/,
     >             '     ',A)
            return
         end if
         retn = nf_get_att_text(file_id,NF_GLOBAL,'sdate',zsdate)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9007) NF_STRERROR(retn)
 9007       format(' ? UCDFHED: nf_get_att_text -- sdate '/,
     >             '     ',A)
            return
         end if
         retn = nf_get_att_text(file_id,NF_GLOBAL,'machine',tdev)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9008) NF_STRERROR(retn)
 9008       format(' ? UCDFHED: nf_get_att_text -- tdev '/,
     >             '     ',A)
            return
         end if
         ftype = 4
C
      else
C Write Header
C----------------------
C Define global Attributes
         retn = nf_put_att_int(file_id,NF_GLOBAL,'shot',NF_INT,1,ishot)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9000) NF_STRERROR(retn)
 9000       format(' ? UCDFHED: nf_put_att_int -- shot'/,'     ',A)
            return
         end if
 
         retn = nf_put_att_text(file_id,NF_GLOBAL,'date',10,zdate)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9001) NF_STRERROR(retn)
 9001       format(' ? UCDFHED: nf_put_att_int -- date '/,
     >             '     ',A)
            return
         end if
         retn = nf_put_att_text(file_id,NF_GLOBAL,'sdate',10,zsdate)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9002) NF_STRERROR(retn)
 9002       format(' ? UCDFHED: nf_put_att_text -- sdate '/,
     >             '     ',A)
            return
         end if
         retn = nf_put_att_text(file_id,NF_GLOBAL,'machine',4,tdev)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9003) NF_STRERROR(retn)
 9003       format(' ? UCDFHED: nf_put_att_text -- tdev '/,
     >             '     ',A)
            return
         end if
      end if
      return
      end
 
 
 
 
 
 
 
