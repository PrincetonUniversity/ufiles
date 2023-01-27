      subroutine ufilx(zfile,ilf)
C
      use USERIO
C
      character*(*) zfile
C
C  rewritten dmc 6 May 1997
C  add the default extension to the filename if it is needed
C
C  input -- zfile, filename as specified in input line
C           ilf, nonblank length of zfile
C
C output -- zfile, modified if necessary with filename .ext appended
C
C-------------------------------------
C
      character*4 zdelim
      data zdelim/':]>/'/
C
      do ic=ilf,1,-1
         if(zfile(ic:ic).eq.mdot) go to 100
         if(index(zdelim,zfile(ic:ic)).gt.0) go to 200
      enddo
      go to 200
C
C  .ext already in filename
C
 100  continue
      return
C
C  want to add .ext to filename
C
 200  continue
      zfile(ilf+1:)=mdot//filext
      return
C
      end
