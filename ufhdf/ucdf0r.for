C Read Scalars from netCDF Ufiles
C
C 08/13/97 CAL
C
C---------------------------------------------------------------------------
      subroutine ucdf0r(lun, scval, sclab, n_sc, nsc, retn)
 
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input
      integer       lun, n_sc
C Return
      integer nsc
      character*10  sclab(3,n_sc)
      real          scval(n_sc)
      integer retn
 
 
C Definitions for processing
      integer ichan, file_id
      integer i,     j
      character*10   nam
C
C-----------------------------------------------------------------------
C get file name
      call uflchk(lun,'uhdf0r',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
C Get number of scalars
C
      retn = nf_get_att_int(file_id,NF_GLOBAL,'num_scalars',nsc)
      if (retn .ne. 0) then
         write(ufelun,9003) NF_STRERROR(retn)
 9003    format(' % UCDF0R: nf_get_att_in -- num_scalars '/,
     >       '     ',a)
         return
      end if
C
C Get Scalar Names
C Assume scalars have been written first
      do i=1,nsc
         retn = nf_inq_varname(file_id, i, nam)
         if (retn .ne. 0) then
            write(ufelun,9001) i,NF_STRERROR(retn)
            return
         end if
         retn = nf_get_att_text(file_id, i, 'long_name',sclab(2,i))
         if (retn .ne. 0) then
            write(ufelun,9002) i,NF_STRERROR(retn)
            return
         end if
         retn = nf_get_att_text(file_id, i, 'units',sclab(3,i))
         if (retn .ne. 0) then
            write(ufelun,9002) i,NF_STRERROR(retn)
            return
         end if
         j = index(nam,' ')
         sclab(1,i) = nam(1:j-1)
         sclab(1,i)(j:j) = ':'
      end do
 9001 format(' % UCDF0R: nf_inq_varname - sclab',i3/,
     >       '     ',a)
 9002 format(' % UCDF0R: nf_get_att_text - sclab',i3/,
     >       '     ',a)
C
C Read Scalars
      do i=1,nsc
         retn = nf_get_var_real(file_id, i, scval(i))
         if (retn .ne. 0) then
            write(ufelun,9000) i,NF_STRERROR(retn)
            return
         end if
C      type *,sclab(1,i),scval(i),' ',sclab(2,i),sclab(3,i)
      end do
 9000 format(' % UCDF0R: nf_get_var_real - scval',i3/,
     >       '     ',a)
C
      return
      end
 
 
 
 
 
 
 
 
