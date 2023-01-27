      subroutine urdopen(ilun,zfile,iaccs,ierr)
C
C  UREAD open routine
C
C    input:  ilun -- fortran LUN to use
C           zfile -- filename
C           iaccs -- 1 for read, 2 for write
C
C   output:  ierr -- 0, normal, 1, error
C
      character*(*) zfile       ! nominal filename
C
      character*256 zfilu       ! actual filename
C
      character*20 zprog
C---------------------------------------
C
      if((iaccs.lt.1).or.(iaccs.gt.2)) then
         call uermsi(' ?urdopen:  argument error, iaccs:',iaccs,0)
         ierr=1
         return
      endif
C
C  unix only:  if slash is found in name, allow syntax
C    <env-var-name>/<filename>
C    where <env-var-name> gets translated (for unix environments)
C
      zfilu=zfile
C
      indsl=index(zfile,'/')
      if(indsl.gt.1) then
         call ufilnam(zfile(1:indsl-1),zfile(indsl+1:),zfilu)
      endif
C
      call ufopen(ilun,zfilu,iaccs,ierr,0)
C
      if(ierr.ne.0) then
         call urd_getprog(zprog)
         if((iaccs.eq.1).and.(zfile(1:1).ne.'/')) then
            call ufilnam('TRANSP_LOCATION',
     >           trim(zprog)//'/'//trim(zfile),zfilu)
            write(lnout(0),*) ' %urdopen: trying: '//trim(zfilu)
            call ufopen(ilun,zfilu,iaccs,ierr,0)
            if(ierr.ne.0) then
               call ufilnam('TRANSPROOT',
     >              'codesys/tmi/'//trim(zprog)//'/'//trim(zfile),zfilu)
               write(lnout(0),*) ' %urdopen: trying: '//trim(zfilu)
               call ufopen(ilun,zfilu,iaccs,ierr,0)
            endif
         endif
      endif
C
      return
      end
