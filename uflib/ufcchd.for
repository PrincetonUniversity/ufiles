c  ufcchd -- write a header line for a "concatenated" Ufile
c    same arguments as "ufsetr" + shot number.
c
      subroutine ufcchd(ilun,zpre,zext,zdevi,zfdir,ishot,ier)
c
c  all arguments are input except ier:
c    ilun gives the the unit number of an ascii file already open
c         for write.
c
c    zpre,zext,zdevi,zfdir -- have the same meaning as for ufsetr,
c         but here they give the naming info of a Ufile that exists and
c         is about to be rewritten into the concatenated Ufile on unit
c         ilun
c
c    ishot -- the ufile shot number of the existing file.
c
c    ier (output) -- completion code, 0 = normal
c
c    this routine writes a header record and then sets up Ufiles COMMON
c    so that an ordinary ascii uf?dwr call can be invoked to write the
c    data into the concatenated Ufile.
c
C------------------------------------------------------
C
      USE UFILES
C
      CHARACTER*(*) ZPRE
C
      CHARACTER*(*) ZEXT
      CHARACTER*(*) ZFDIR
      CHARACTER*(*) ZDEVI
C
        integer ishot
c
c--------------------------------------
c
        ier=0
        call ufinit
        ichan=iuxcha(ilun)
        if(ichan.eq.0) then
           call ufsetr(ilun,zpre,zext,zdevi,zfdir)
           ichan=iuxcha(ilun)
           if(ichan.eq.0) then
              ier=1
              return
           endif
        endif
c
        ufstat(ichan)=0
        mds_shot(ichan)=0
        lufset(ichan)=.true.
        call ufcmpr(ilun,0)             ! ascii
        ufshot(ichan)=ishot             ! shot no.
        if(ishot.lt.0) call ufnsho(ilun)
        ufstat(ichan)=1                 ! ready to write
c
c  write header line
c
        ilpre=index(zpre,' ')-1
        if(ilpre.lt.0) ilpre=len(zpre)
        if(ilpre.eq.0) ilpre=1
        ilext=index(zext,' ')-1
        if(ilext.lt.0) ilext=len(zext)
        if(ilext.eq.0) ilext=1
        ildevi=index(zdevi,' ')-1
        if(ildevi.lt.0) ildevi=len(zdevi)
        if(ildevi.eq.0) ildevi=1
        ilfdir=index(zfdir,' ')-1
        if(ilfdir.lt.0) ilfdir=len(zfdir)
        if(ilfdir.eq.0) ilfdir=1
c
        write(ilun,1001) zpre(1:ilpre),ishot,zext(1:ilext),
     >     zdevi(1:ildevi),zfdir(1:ilfdir)
 1001   format('>>ufcchd: pre=',a,', shot=',i6,', ext=',a,
     >     ', dev=',a,', dir=',a)
c
        return
        end
 
 
