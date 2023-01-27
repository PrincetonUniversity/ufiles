C Write 1 dimensional CDF data
C Called by uf1dwr
C
C 08/14/97 CAL
C
C----------------------------------------------------------------------------
      subroutine ucdf1w(lun,
     &            f, x, nx, iproc,
     &            flab,xlab,
     &            retn)
 
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input
      integer       lun, iproc
      integer       nx
      real          x(nx), f(nx)
      character*(*) flab,  xlab
C Return
      integer retn
 
C Definitions for CDF
      character*1 namf, namx
      integer rank
      parameter (rank = 1)
      integer dims
 
C Definitions for processing
      integer       f_id, x_id
      character*20  fl, xl
      character*10  fu, xu
      integer       ichan, file_id
      integer       i, j
C
      data          namf,namx/'F','X'/
C----------------------------------------------------------------------------
C
C Define 1 Dimension
      dims = nx
C
C get file name
      call uflchk(lun,'ucdf1w',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
C
      fl = flab(1:20)
      xl = xlab(1:20)
      fu = flab(21:30)
      xu = xlab(21:30)
C
C Enter Define Mode
      retn = nf_redef(file_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9005) NF_STRERROR(retn)
 9005    format(' % UCDF1W: nf_redef '/,'     ',a)
         return
      end if
 
C Define Processing factor
      retn = nf_put_att_int(file_id,NF_GLOBAL,'scale_factor',
     &       NF_INT,1,iproc)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9000) NF_STRERROR(retn)
 9000    format(' % UCDF1W: nf_put_att_int - iproc '/,
     >        '     ',a)
         return
      end if
C
C
C Define dimensions
      retn = nf_def_dim(file_id,'X',nx,dims)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9007) namx, NF_STRERROR(retn)
 9007    format(' % UCDF1W: nf_def_dim ',a/,'     ',a)
         return
      end if
C
C Define Data Arrays
C------------------
 
C Define dependent variable F
      retn = nf_def_var(file_id, 'F', NF_FLOAT, rank, dims, f_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9001) namf, NF_STRERROR(retn)
 9001    format(' % UCDF1W: nf_def_var ',a/,'     ',a)
         return
      end if
 
      retn = nf_put_att_text(file_id,f_id,'units',10,fu)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9002) fu, NF_STRERROR(retn)
 9002    format(' % UCDF1W: nf_put_att_tex - units ',a/,'     ',a)
         return
      end if
 
      retn = nf_put_att_text(file_id,f_id,'long_name',20,fl)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9003) fl, NF_STRERROR(retn)
 9003    format(' % UCDF1W: nf_put_att_tex - label ',a/,'     ',a)
         return
      end if
 
C Define independent variable X
      retn = nf_def_var(file_id, 'X', NF_FLOAT, 1, dims, x_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9001) namx, NF_STRERROR(retn)
         return
      end if
      retn = nf_put_att_text(file_id,x_id,'units',10,xu)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9002) xu, NF_STRERROR(retn)
         return
      end if
      retn = nf_put_att_text(file_id,x_id,'long_name',20,xl)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9003) xl, NF_STRERROR(retn)
         return
      end if
C Enter Data Mode
      retn = nf_enddef(file_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9004) NF_STRERROR(retn)
 9004    format(' % UCDF1W: nf_enddef '/,'     ',a)
         return
      end if
 
C Write data array
      retn = nf_put_var_real(file_id, f_id, f)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9006) namf, NF_STRERROR(retn)
 9006 format(' % UCDF1W: nf_put_var_real ',a/,'     ',a)
      return
      end if
C
C Write X
      retn = nf_put_var_real(file_id, x_id, x)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9006) namx, NF_STRERROR(retn)
         return
      end if
      return
      end
 
 
 
