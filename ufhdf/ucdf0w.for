C Write Scalars to netCDF Ufiles
C
C 08/13/97 CAL
C
C---------------------------------------------------------------------------
      subroutine ucdf0w(lun, option, nsca, scval, sclab, retn)
 
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input
      integer       lun, nsca
      integer       option          ! 1 = number of scalars only
C                                   ! 2 = write scalars
      character*10  sclab(3,nsca)
      real          scval(nsca)
C Return
      integer retn
 
 
C Definitions for processing
      integer n_sc, nsc
      parameter(n_sc = 200)
      integer ichan, file_id
      integer i,     j
      integer sc_id(n_sc)
C
C-----------------------------------------------------------------------
C get file name
      call uflchk(lun,'uhdf0w',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
C
      nsc = nsca
      if (nsca .gt. n_sc) then
         nsc = n_sc
         write(ufelun,9006) nsca, nsc
 9006    format(
     > ' % UCDF0W: --Warning-- Number of Scalars truncated from',
     >   i4,' to',i4)
      end if
 
C Write Number of scalars as global attribute
C
      if (option .eq. 1) then
         retn = nf_put_att_int(file_id,NF_GLOBAL,'num_scalars',
     &        NF_INT,1,nsc)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9004) NF_STRERROR(retn)
 9004       format(' % UCDF0W: nf_put_att_int - num_scalars'/,
     >       '     ',a)
         end if
         if (nsc .gt. 0) return
C
C if no scalars:
C Enter Data Mode
         retn = nf_enddef(file_id)
         if (retn .ne. NF_NOERR) then
            write(ufelun,9003) NF_STRERROR(retn)
            return
         end if
         return
      end if
C
C---------------------------------------------------------------------------
C
C Write Scalars as variables
C===========================
C       Label is broken up in :
C        1:10  =  Variable name
C       11:20  =  attribute long-name
C       21:30  =  attribute units
C
C Enter Define Mode
C      retn = nf_redef(file_id)
C      if (retn .ne. NF_NOERR) then
C            write(ufelun,9005) NF_STRERROR(retn)
C 9005       format(' % UCDF0W: nf_redef -- ',a)
C         return
C      end if
C
 
C Write Scalar Names
      do i=1,nsc
         j = index(sclab(1,i),':')-1
         retn = nf_def_var(file_id, sclab(1,i)(1:j), NF_FLOAT,
     >        0, 0, sc_id(i))
         if (retn .ne. 0) then
            write(ufelun,9001) i,NF_STRERROR(retn)
            return
         end if
         retn = nf_put_att_text(file_id,sc_id(i),'long_name',10,
     >        sclab(2,i))
         if (retn .ne. 0) then
            write(ufelun,9002) i,NF_STRERROR(retn)
            return
         end if
         retn = nf_put_att_text(file_id,sc_id(i),'units',10,
     >        sclab(3,i))
         if (retn .ne. 0) then
            write(ufelun,9002) i,NF_STRERROR(retn)
            return
         end if
      end do
 9001 format(' % UCDF0W: nf_def_var - sclab',i3/,
     >     '     ',a)
 9002 format(' % UCDF0W: nf_put_att_text - sclab',i3/,
     >     '     ',a)
 
C
C Write Scalar Values
C-----------------------
C Enter Data Mode
      retn = nf_enddef(file_id)
      if (retn .ne. NF_NOERR) then
            write(ufelun,9003) NF_STRERROR(retn)
 9003       format(' % UCDF0W: nf_enddef -- ',a)
         return
      end if
C
      do i=1,nsc
         retn = nf_put_var_real(file_id, sc_id(i), scval(i))
         if (retn .ne. 0) then
            write(ufelun,9000) i,NF_STRERROR(retn)
            return
         end if
      end do
 9000 format(' % UCDF0W: nf_put_var_real - scval',i3/,
     >     '     ',a)
C
      return
      end
 
 
 
 
 
 
 
 
