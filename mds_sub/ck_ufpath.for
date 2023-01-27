      subroutine ck_ufpath(path,device,dirname,fname,sts)
C Check if file exists - or need to modify path
C 1. check for path
C 2. look for $DATADIR/device/dirname/fname
C
C 05/08/02 C. Ludescher
C
C Input
      character*(*) path        ! full path
      character*(*) device      ! tokamak
      character*(*) dirname     ! directory
      character*(*) fname       ! filename
      integer       sts
C Processing
      logical       ex
      integer       ierr,  lp0, lp, ldir, lf, lzp, lzp1
      character(len=512) :: zpath, zdir
C------------------------------------------------
      sts = 0
      lp0 = len(path)
      lp = len_trim(path)
      ldir=len_trim(dirname)
      lf  =len_trim(fname)
C check if file exists as passed
      inquire(file=path(1:lp),exist=ex, iostat=ierr)
      if (ex) return
C Check if file is in DATADIR
      call sget_env('DATADIR',zpath)
      lzp1 = len_trim(zpath)+1
      if (lzp1 .lt. 2) then
         print *,'? ck_ufpath: DATADIR not defined'
         sts = 1
         return
      endif
      if (dirname(1:1).eq.'/') then
         zpath(lzp1:) = '/'//device//'/'//fname(1:lf)
      else
         zpath(lzp1:) = '/'//device//'/'//dirname//fname(1:lf)
      endif
      lzp = len_trim(zpath)
      inquire(file=zpath(1:lzp),exist=ex,iostat=ierr)
C Check if dirname should be lower case
      if (.not. ex) then
         print *, '% ck_ufpath: try lower case'
         zdir = dirname
         call ulower(zdir(1:ldir))
         zpath(lzp1:) = '/'//device//'/'//zdir(1:ldir)//fname(1:lf)
         print *,zpath(1:lzp)
         inquire(file=zpath(1:lzp),exist=ex,iostat=ierr)
      endif
      if (ex) then
         if (lzp .le. lp0) then
            path = zpath(1:lzp)
            return
         else
            print *,'? ck_ufpath: path too short for expanded filename'
         endif
      else
         print *,'? ck_ufpath: ',path(1:lp),' does not exist'
      endif
      sts = 1
      return
      end
