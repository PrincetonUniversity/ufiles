C Write 2 dimensional CDF data
C Called by uf2dwr
C
C 08/14/97 CAL
C
C----------------------------------------------------------------------------
      subroutine ucdf2w(lun,
     &            f,nf1,x,nx,y,ny,iproc,
     &            flab,xlab,ylab,
     &            retn)
 
      use UFILES
      implicit none
 
      include "netcdf.inc"
 
C Input
      integer       lun, iproc
      integer       nf1,   nx,    ny
      real          x(nx), y(ny), f(nf1,ny)
      character*(*) flab,  xlab,  ylab
C Return
      integer retn
 
C Definitions for CDF
      integer rank
      parameter (rank = 2)
      integer dims(rank)
      character*1 namf, namx, namy
 
C Definitions for processing
      integer       f_id, x_id, y_id
      integer       st(rank), cnt(rank)
      character*20  fl, xl, yl
      character*10  fu, xu, yu
      integer       ichan, file_id
      integer       i, j
C
      data          namf,namx,namy/'F','X','Y'/
C----------------------------------------------------------------------------
C Define 2 Dimensions
      dims(1) = nx
      dims(2) = ny
C
C get file name
      call uflchk(lun,'ucdf2w',ichan,retn)
      if (retn .ne. 0) return
      file_id = ufid(ichan)
C
C
      fl = flab(1:20)
      xl = xlab(1:20)
      yl = ylab(1:20)
      fu = flab(21:30)
      xu = xlab(21:30)
      yu = ylab(21:30)
 
C Enter Define Mode
      retn = nf_redef(file_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9005) NF_STRERROR(retn)
 9005    format(' % UCDF2W: nf_redef '/,'     ',a)
         return
      end if
 
C Define Processing factor
      retn = nf_put_att_int(file_id,NF_GLOBAL,'scale_factor',
     &       NF_INT,1,iproc)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9000) NF_STRERROR(retn)
 9000    format(' % UCDF2W: nf_put_att_int - iproc '/,
     >        '     ',a)
         return
      end if
C
C
C Define dimensions
      retn = nf_def_dim(file_id,'X',nx,dims(1))
      if (retn .ne. NF_NOERR) then
         write(ufelun,9008) namx, NF_STRERROR(retn)
 9008    format(' % UCDF2W: nf_def_dim ',a/,'     ',a)
         return
      end if
 
      retn = nf_def_dim(file_id,'Y',ny,dims(2))
      if (retn .ne. NF_NOERR) then
         write(ufelun,9007) namy, NF_STRERROR(retn)
         return
      end if
 
C Define Data Arrays
C------------------
 
C Define dependent variable F
      retn = nf_def_var(file_id, 'F', NF_FLOAT, rank, dims, f_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9001) namf, NF_STRERROR(retn)
 9001    format(' % UCDF2W: nf_def_var ',a/,'     ',a)
         return
      end if
 
      retn = nf_put_att_text(file_id,f_id,'units',10,fu)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9002) fu, NF_STRERROR(retn)
 9002    format(' % UCDF2W: nf_put_att_tex - units ',a/,'     ',a)
         return
      end if
 
      retn = nf_put_att_text(file_id,f_id,'long_name',20,fl)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9003) fl, NF_STRERROR(retn)
 9003    format(' % UCDF2W: nf_put_att_tex - label ',a/,'     ',a)
         return
      end if
 
C Define independent variable X
      retn = nf_def_var(file_id, 'X', NF_FLOAT, 1, dims(1), x_id)
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
 
C Define independent variable Y
      retn = nf_def_var(file_id, 'Y', NF_FLOAT, 1, dims(2), y_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9001) namy, NF_STRERROR(retn)
         return
      end if
      retn = nf_put_att_text(file_id,y_id,'units',10,yu)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9002) yu, NF_STRERROR(retn)
         return
      end if
      retn = nf_put_att_text(file_id,y_id,'long_name',20,yl)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9003) yl, NF_STRERROR(retn)
         return
      end if
C Enter Data Mode
      retn = nf_enddef(file_id)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9004) NF_STRERROR(retn)
 9004    format(' % UCDF2W: nf_enddef '/,'     ',a)
         return
      end if
C
C
C Write data array
C      retn = nf_put_var_real(file_id, f_id, f)
      st(1) = 1
      cnt(1) = nx
      cnt(2) = 1
      do i=1,ny
         st(2) = i
         retn = nf_put_vara_real(file_id, f_id, st, cnt, f(1,i))
         if (retn .ne. NF_NOERR) then
            write(ufelun,9007) i, NF_STRERROR(retn)
            return
         end if
      end do
 9007 format(' % UCDF2W: nf_put_vara_real y =',i4/,'     ',a)
 9006 format(' % UCDF2W: nf_put_var_real ',a/,'     ',a)
 
C Write X
       retn = nf_put_var_real(file_id, x_id, x)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9006) namx, NF_STRERROR(retn)
         return
      end if
C Write Y
       retn = nf_put_var_real(file_id, y_id, y)
      if (retn .ne. NF_NOERR) then
         write(ufelun,9006) namy, NF_STRERROR(retn)
         return
      end if
      return
      end
 
 
 
