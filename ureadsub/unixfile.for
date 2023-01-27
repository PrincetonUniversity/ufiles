C-----------------------------------------------------------------------
C  unixfile -- accept input to determine an arbitray unix filename
 
 
      subroutine unixfile(zlbl,zfile)
 
 
      character*(*) zlbl  ! (input) caller id string
      character*(*) zfile ! (output) a full filename, left justified
C
C  zlbl is used in generating the UREAD prompt for the filename
C
C  nb:  if zlbl is 'init', the default zpath is re-initialized
C   using the value zfile which is then also input
C
 
C  Modifcations:
C  0515/95  tbt   Increase file name length to 130 from 64.
 
 
      character*130  zpath,ztrans,zname       ! 130 arbitrary  tbt
 
      data zpath/' '/
C
      logical lxout
C
C-----------------------------------------------------------------------
C
      if( zlbl.eq.'init' ) then
 
        zpath=zfile
 
      else
 
        luntrm = lnout(0)
 
 5      continue
        if( lxout(0) ) write(luntrm,1001)
 1001   format(
     >' %unixfile:  enter a complete, literal unix file specification,'/
     >' or "r " or "R " to set up a directory path (with environment'/
     >' variable name translation) and then specify a relative file'/
     >' specification.'/)
 
        call ureadLw(
     >      'unixfile ('//zlbl//'):  enter file spec or "r":$',zfile)
 
        if((zfile(1:2).eq.'r ').or.(zfile(1:2).eq.'R ')) then
 
          call unixpath(zpath,ztrans,itrans)
          call ureadLw(
     >      'unixfile ('//zlbl//'):  enter relative file spec or "q":$',
     >        zname)
 
          if((zname(1:2).eq.'q ').or.(zname(1:2).eq.'Q ')) go to 5
 
          inb=index(zname,' ')-1
          if(ztrans(itrans:itrans).eq.'/') then
            zfile=ztrans(1:itrans)//zname(1:inb)
          else
            zfile=ztrans(1:itrans)//'/'//zname(1:inb)
          endif
 
        endif
 
      endif
 
      return
      end
