C-----------------------------------------------------------------------
C  unixpath -- accept input to determine path to user named unix file
C
C  input may be either a literal directory specification, or,
C  a directory specification where the first clause is an environment
C  variable, which is translated.
C
C  the caller supplies the current or default (untranslated) value
C  via the argument zpath; zpath can be modified here by user action.
C
C  the output upath(1:lupath) is a literal path string that can be
C  used to form a filename string for an OPEN statement.
C
C  lupath=0 is returned if the user takes the "d " option; this is
C  supposed to mean, use files in the current working directory.
 
C  Modifications:
C  05/15/95  tbt Increased file name length to 130.
 
 
      subroutine unixpath(zpath,upath,lupath)
 
 
 
      character*(*) zpath  ! untranslated pathname (input/output)
C
      character*(*) upath  ! translated pathname (output)
C
      integer lupath       ! nonblank length of upath (output)
C
C  local:
C
      character*130 zvalue     ! 130 is arbitrary but the same as zpath
C
      logical lxout
C
C-----------------------------------------------------------------------
C
        luntrm = lnout(0)
        call urdmod_get(imod)
 
 10     continue
 
        ilz=index(zpath,' ')-1
        if(ilz.eq.0) ilz=1
 
        if(zpath.eq.' ') then
          upath=' '
          lupath=0
        else if((zpath(1:1).eq.'/').or.(zpath(1:1).eq.'~')) then
          upath=zpath
          lupath=ilz
        else
          ilenv=index(zpath,'/')-1
          if(ilenv.lt.0) ilenv=ilz
          ist=1
          if(zpath(1:1).eq.'$') ist=2
          call sget_env(zpath(ist:ilenv),zvalue)
          if(zvalue.eq.' ') then
            upath=zpath
          else
            ilbl=index(zvalue,' ')-1
            upath=zvalue(1:ilbl)//zpath(ilenv+1:ilz+1)
          endif
          lupath=index(upath,' ')-1
        endif
 
        if(lxout(0)) write(luntrm,1001)
     >      zpath(1:ilz),upath(1:max(1,lupath))
 
 1001   format(
     >' Unixpath:  current path:  "',a,'"'/
     >'         translated path:  "',a,'"'/
     >' (blank means default, use current working directory)'//
     >' Enter "d" or "D" to use default,'/
     >' enter "a" or "A" to append to current path,'/
     >' enter "q" or "Q" to use current setting,'/
     >' or enter a path string to specify a new path.  If the first'/
     >' character of the path string is neither "/" nor "~" then the'/
     >' first word of the path string may be taken as an environment'/
     >' variable, to be translated on output.'/)
 
        zvalue = zpath

        call urdtch
        call ureadLw('unixpath:  enter path string or option code:$',
     >      zvalue)
        if(imod.lt.2) call urdchr
 
        if((zvalue(1:2).eq.'q ').or.(zvalue(1:2).eq.'Q ')) then
          go to 100
        else if((zvalue(1:2).eq.'d ').or.(zvalue(1:2).eq.'D ')) then
          zpath=' '
          go to 10
        else if((zvalue(1:2).eq.'a ').or.(zvalue(1:2).eq.'A ')) then
          call ureadLw(
     >          'unixpath: enter sub-directory to current path:$',
     >          zvalue)
          zpath = trim(zpath)//'/'//trim(zvalue)
          go to 10
        else
          zpath = zvalue
          go to 10
        endif
 
 
 100  continue
      return
      end
