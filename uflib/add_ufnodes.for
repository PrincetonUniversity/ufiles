      subroutine add_ufnodes(lunerr,runid,tr_socket,
     >           tr_tree,tr_pulse,device,ierr)
C
C Tree must be open for edit
C
C trtmp_rd
C Read output from trdat = <runid>TRDAT.TMP (which must have been
C previously run ) to get list of TRANSP input files and also location
C of input files.
C
C
C trtmp_add
C read ufiles and write to MDSplus
C
      implicit none
 
      integer       lunerr
      character*(*) runid
      integer       tr_socket         ! socket of TRANSP-run tree
      character*(*) tr_tree           ! name of TRANSP-run tree
      integer       tr_pulse          ! TRANSP-run pulse number
      character*(*) device            ! tokamak
      integer       ierr
C
      integer maxfiles
      parameter (maxfiles=50)
 
      character*150  uflist(maxfiles)    ! full encodes ufiles path
      character*150  ufalt(maxfiles)     ! full encodes ufiles path
      character*5    lbls(maxfiles)      ! uf_Lable = tr_node
      character*20   trlbls(maxfiles)    ! tr Label
      integer        nfiles
      character*20   trpath              ! tr Path
      real           toff
 
      integer        i

C * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 
      call trtmp_rd(runid,maxfiles,uflist,ufalt,lbls,
     .     nfiles,trlbls,trpath,toff,device,ierr)
      if (ierr .ne. 0) return
 
      print *,'add_ufnodes: ',nfiles,' files'
      call trtmp_add(lunerr,runid,tr_socket,tr_tree, tr_pulse,
     >               device,maxfiles,uflist,ufalt,lbls,
     >               nfiles,trlbls,trpath,toff,ierr)
      end
 
C----------------------------------------------------------/read_trtmp
 
      subroutine trtmp_rd(runid,
     .     maxfiles,
     .     uflist,     ! full encodes ufiles path
     .     ufalt,      ! full encodes ufiles path (fallback)
     .     lbls,       ! uf_Lable = tr_node
     .     nfiles,
     .     trlbls,     ! tr Label
     .     trpath,     ! tr Path
     .     toff,
     .     device,     ! tokamak
     .     ierr)
 
      implicit none
      integer maxfiles
      character*150 uflist(maxfiles), ufalt(maxfiles)
      character*5   lbls(maxfiles)
      character*20  trlbls(maxfiles), trpath
      character*(*) runid
      character*(*) device
      integer       nfiles
      real          toff
      integer       ierr
 
                                     ! local
      character*80  dummystr         ! for skipping lines
      character*5   lbl              ! uf_Lable = tr_node
      character*20  trlbl            ! tr Label
      character*50  prefix           ! uf prefix / uf Server
      character*50  altpre           ! uf prefix for fallback
      character*50  extension, path  ! uf suffix / uf Tree
      character*120 dirname          ! uf directory / uf node
      character*6   ashotused        ! uf shot number
      character*10  pulse            ! uf mds pulse number
      character*12  node             ! uf mds node name
      character*60  fname            ! uf name
 
      integer       i, ios, status
      logical       gotlbl, ex, delete
 
      character*120 trtmpfile
      character*150 zpath
      integer       ldir,   ldisk, istart, lf, lp
      logical       ismds
C--------------------------------------------------------------------------
C assume <runid>TRDAT.TMP file is in local directory
      ismds=.false.
      delete=.true.
      trtmpfile = runid // 'TRDAT.TMP'
      inquire(file=trtmpfile,exist=ex,iostat=ierr)
      if (.not. ex) then
         ierr = 1
         print *, '? add_ufnodes: can not find ',trtmpfile
         return
      else
         delete=.false.
      end if
 
      open (unit=29,file=trtmpfile,status='old',iostat=ierr)
      if (ierr .ne. 0) then
         print *, '? add_ufnodes: can not find ',trtmpfile
         return
      end if
	! read known format *TR.TMP file
 776  format(a8,A)
 777  format(8X,F14.6)
 778  format(a8,A20)
 779  format(8X,A)
 
      read (29,'(A)', END=999, iostat=ierr) dummystr ! runid and file label
                                                     ! or Inputdir, if MDSplus
      if (dummystr(1:10) .eq. ' Inputdir:') then
         dirname = dummystr(11:)
         ldir = index(dirname,' ')-1
         if (ldir .gt. 1) then
            if (dirname(ldir:ldir) .ne. '/') then
               ldir=ldir+1
               dirname(ldir:ldir)='/'
            endif
         endif            
         read (29,779, END=999) ashotused
         if (ashotused(1:1).eq.' ') ashotused =
     .        ashotused(2:len_trim(ashotused))
         read (29,'(A)', END=999, iostat=ierr) dummystr ! runid and file label
      endif
      if (dummystr(1:8) .ne. ' TrPath:') then
         trpath = '.INPUTS:'      ! default for TRANSP run Tree
      else
         trpath = dummystr(9:)   ! if Ufile copy
      endif
      read (29,777, END=999, iostat=ierr) toff
      print *, 'read_trtmp: toff: ',trtmpfile,toff
      i = 0
      lbl=' '
      gotlbl = .true.
      do while (.TRUE.)         ! exit via EOF on file
         if (gotlbl) read (29,'(A)', END=999) dummystr ! dashes
         read (29,779, END=999,iostat=ios) lbl
         read (29,776, END=999) dummystr,prefix ! max 10 char, I think
         call uupper(prefix)
         if (dummystr(1:8) .eq. ' Server:') then
            ismds = .true.
            read (29,779, END=999) altpre
            call uupper(altpre)
         endif
         read (29,779, END=999) extension ! normally 3 char
         call uupper(extension)
         if (ismds) then
            read (29,779, END=999) path
            read (29,779, END=999) node
            read (29,779, END=999) pulse
         else
            read (29,779, END=999) dirname
            read (29,779, END=999) ashotused
         endif
         read (29,778, END=999) dummystr,trlbl ! 20 char
         print *,dummystr(1:8),trlbl
         if (dummystr(2:6) .ne. 'TrLbl') then ! is TRDAT label missing ?
            gotlbl = .false.
            trlbl = ' '
            print *,'no TRDAT label'
         else
            gotlbl = .true.
         endif
         if (i.lt.maxfiles) then
            i = i + 1
         else
            ierr = 1
            print *, 'read_trtmp: more than maxfiles files', i
            return
         endif
! copy into corresponding arrays
 
         lbls(i) = lbl
         trlbls(i) = trlbl
 
C MDSplus
         if (ismds) then
             if (pulse .ne. ' ' .and.
     >       pulse .ne. '         0' .and.
     >       path(1:5) .ne. 'PPPL@') then
                 uflist(i) = 'MDS+::' //
     .           prefix(1:len_trim(prefix)) // '/' //       ! Server
     .           extension(1:len_trim(extension)) //        ! Tree
     .            '(' // pulse // ')/' //                  ! encode shot
     .           path(1:len_trim(path)) // '/' //
     .           node(1:len_trim(node))
             else
                 uflist(i) = 'MDS+::' //
     .           prefix(1:len_trim(prefix)) // '/' //       ! Server
     .           extension(1:len_trim(extension)) // '/' // ! Tree
     .           path(1:len_trim(path)) // '/' //
     .           node(1:len_trim(node))
             endif
C Generate "alternate" list for fallback
             if (ldir .gt. 1) then
                ufalt(i) = dirname(1:ldir)
                istart = ldir+1
             else
                istart=1
             endif
             fname = altpre(1:len_trim(altpre)) //
     .            ashotused(1:len_trim(ashotused)) //
     .            '.' //node(1:len_trim(node))
             lf = len_trim(fname)
             ufalt(i)(istart:) = fname(1:lf)
         else
C parse filename
             ldir = index(dirname,' ')-1
             ldisk = 0
             if (ldir .gt. 1) then
                 if (dirname(ldir:ldir) .ne. '/') then
                     ldir=ldir+1
                     dirname(ldir:ldir)='/'
                 endif
                 uflist(i) = dirname(1:ldir)
                 istart = ldir+1
             else
                 istart=1
             endif
             if (ashotused(1:1).eq.' ') ashotused =
     .       ashotused(2:len_trim(ashotused))
             if (ashotused(1:5).ne.'None.') then
                fname = prefix(1:len_trim(prefix)) //
     .           ashotused(1:len_trim(ashotused)) //
     .           '.' //extension(1:len_trim(extension))
             else
                 fname = prefix(1:len_trim(prefix)) //
     .           '.' // extension(1:len_trim(extension))
             endif
             lf = len_trim(fname)
             uflist(i)(istart:) = fname(1:lf)
             lp = len_trim(uflist(i))
C check if path is okay or need fix directory
             zpath=uflist(i)
             call ck_ufpath(uflist(i),device,
     >       dirname(1:ldir),fname(1:lf),ierr)
             if (ierr .ne. 0) then
                print *, '? add_ufnodes:  ' //
     >               uflist(i)(1:lp) //' does not exist'
                if (extension(1:len_trim(extension)) .eq. 'RPL') then
                   fname = prefix(1:len_trim(prefix)) //
     .                  '.' // extension(1:len_trim(extension))
                   lf = len_trim(fname)
                   uflist(i)=zpath
                   uflist(istart:) = fname(1:lf)
                   lp = len_trim(uflist(i))
                   call ck_ufpath(uflist(i),device,
     >                  dirname(1:ldir),fname(1:lf),ierr)
                   if (ierr .ne. 0) then
                      print *, '? add_ufnodes:  ' //
     >                     uflist(i)(1:lp) //' does not exist'
                   endif
                endif
             endif
C 06/05/08 CLF: Return error if file is missing
             if (ierr .ne. 0) then
                close (unit=29)
                return
             endif
         endif
C on EOF last line read will really be "Ierr:  <code>", with code in
C                                       prefix variable
 
      enddo
 999  continue  ! jump here on end-of-file
      if(delete) then
C         close (unit=29,='delete')
         close (unit=29)
      else
         close (unit=29)
      endif
      nfiles = i
      ierr = 0
      return
      end
 
 
C---------------------------------------------------------------------------
      subroutine trtmp_add(lun,runid,tr_socket,tr_tree, tr_pulse,
     <                     device,maxfiles,uflist,ufalt,nodes,
     >                     nfiles,trlbls,trpath,toff,ierr)
 
      implicit none
 
C Input
      integer       lun                ! lun for error messages
      character*(*) runid              ! TRANSP runid
      integer       tr_socket          ! TRANSP-run mds_socket
      character*(*) tr_tree            ! TRANSP-run Tree name
      integer       tr_pulse           ! TRANSP-run pulse number
      character*(*) device             ! Tokamak
      integer       maxfiles
      character*150 uflist(maxfiles)   ! ufiles name/path
      character*150 ufalt(maxfiles)    ! ufiles name/path (fallback)
      character*5   nodes(maxfiles)    ! node name in Tree
      character*20  trlbls(maxfiles)   ! Label in Tree
      character*20  trpath             ! path in Tree
      integer       nfiles
      real          toff
      integer       ierr
C Local
      integer       i, sts, opt
      integer       ilp, ln, lf, la
      character*150 ufsource
C mdsplus
      integer       idescr_long
      integer       dsc,       mdsstat
 
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      print *,nfiles,' files'
      ilp = len_trim(trpath)
      if (ilp .gt. 1) then
         if (trpath(ilp:ilp) .ne. ':') then
            ilp=ilp+1
            trpath(ilp:ilp) = ':'
         endif
      endif
      opt = 1
      do i=1,nfiles
         ln = len_trim(nodes(i))
         lf = len_trim(uflist(i))
C Read ufiles and write to MDSplus
         call mds_rdwr(lun,tr_socket,tr_tree,tr_pulse,
     >        trpath(1:ilp)//nodes(i)(1:ln),
     >        uflist(i)(1:lf),device,opt,ierr)
C If MDS failed, try local disk
         if (ierr .eq. -77) then
            write(lun,*) '...will try local disk'
            la = len_trim(ufalt(i))
            call mds_rdwr(lun,tr_socket,tr_tree,tr_pulse,
     >           trpath(1:ilp)//nodes(i)(1:ln),
     >           ufalt(i)(1:la),device,opt,ierr)
            ufsource=ufalt(i)(1:la)
            lf=la
         else
            ufsource=uflist(i)(1:lf)
         endif
C Write T_OFFSET and COMMENT
         if (ierr .eq. 0 .or. ierr .eq. 4) then
            call mds_whdr(lun,trpath(1:ilp),nodes(i)(1:ln),
     >           trlbls(i)(1:20),ufsource(1:lf),toff,ierr)
         endif
         opt = 0
 
      enddo
      return
      end
 
