      subroutine add_file(lun,runid,node,mds_server,
     >           mds_tree,mds_shot,mds_path,ier)
C add a file, e.g. TR.DAT namelist or TR.EXE to TRANSP Tree
C
C    Mods
C        24Oct2003  James.Conboy@jet.uk  ( if __JET
C                   Dont override the namelist MDS spec if 
C                   creating a TRJOB tree
C        22Aug2007  CLF: Handle commented out/missing mdsplus defs
C-----------------------------------------------------------------
C
      implicit none
 
      integer        lun          ! to write error messages
      character*(*)  runid        ! no trailing blanks
      character*(*)  node         ! no trailing blanks (where to put file)
      character*(*)  mds_server   ! namelist only
                                  ! filename if NODE=.NML:
      character*(*)  mds_tree     ! namelist only
      integer        mds_shot     ! namelist only
      character*(*)  mds_path     ! namelist only
      integer        ier
 
      integer        descr, MdsValue, MdsPut
      integer        stat, dsc, size
 
      character*240  filnam
      integer        ios, i,  ii, j, l, k, k1
      integer        i1,  i2, jj, n, icom
      integer        ll,  m,  k2, k3,   kk,  kz

      integer        nlines, linsert, total, ninsert
      logical        nml
      logical        a_kmds, a_shot, a_path, a_tree, a_srv
      logical        c_kmds, c_shot, c_path, c_tree, c_srv
      logical        iex90

      character*132  line, temp
      character*1    q
      character*1,   allocatable :: text(:)
 
C--------------------------------------------------------
C
      q = char(39)
      a_kmds = .false.
      a_shot = .false.
      a_tree = .false.
      a_path = .false.
      a_srv  = .false.
      c_kmds = .false.
      c_shot = .false.
      c_tree = .false.
      c_path = .false.
      c_srv  = .false.
      iex90  = .false.
      linsert = 0
      ninsert = 0
      if (node .eq. 'NAME_LIST') then
         filnam = runid//'TR.DAT'
         nml = .true.
      else if (node .eq. 'EXPERT_CODE') then
         filnam = runid//'ex.for'
         nml = .false.
         inquire(file=filnam, exist=iex90)
         if (iex90) then
            iex90 = .false.  ! use ex.for file as is
         else
            filnam = runid//'ex.f90'
            inquire(file=filnam, exist=iex90)
            if (.not. iex90) then
               write(lun,'(a)') '?add_file -- unable to find '
     &              //runid//'ex.for or '//runid//'ex.f90'
               ier=1 ; return
            end if
         end if
      else if (node .eq. 'CONTENTS') then
         filnam = runid//'TF.PLN'
         nml = .false.
      else if (node .eq. 'NB_NAMELIST') then
         filnam = runid//'_nubeam_init.dat'
         nml = .false.
      else if (node .eq. 'COMMENT') then
         call sget_env('QSHARE',filnam)
         ll = len_trim(filnam)+1
         filnam(ll:) = '/CC/'//runid//'CC.TMP'
         nml = .false.
      else if (node(1:5) .eq. '.NML:') then
         filnam = mds_server
         nml = .false.
      endif
 
C open file, read to get size
      print *,'open ',filnam
      open (29,file=filnam,status='OLD',iostat=ios,err=101,
     >      action="read")
      nlines = 0
      size = 0
C
      do while (ios .eq. 0)
         line=' '
         read(29,'(a)',iostat=ios,end=105,err=104)line
         nlines=nlines+1
         l = len_trim(line)
         if (l .gt. size) size = l
         if (iex90 .and. nlines==1) then
            line = adjustl(line)
            iex90 = (line /= '!f90!')   ! do not add marker if already there
         end if
         if (nml) then
            call uupper(line)
            j=index(line(1:l),'$TRDATA')
            if (j .gt. 0) then
               icom=index(line(1:l),'!') ! check if in comment
               if (icom.eq.0 .or. icom.gt.j) linsert = nlines
            endif
            j=index(line(1:l),'KMDSPLUS')
            if (j .gt. 0) then
               icom=index(line(1:l),'!') ! check if in comment
               if (icom.eq.0 .or. icom.gt.j) then
                  a_kmds = .true.
               else
                  c_kmds = .true.
               endif
            endif
            j=index(line(1:l),'MDS_')
            if (j .gt. 0) then
               icom=index(line(1:l),'!') ! check if in comment
               if (line(j:j+7) .eq. "MDS_SHOT") then
                  if (icom.eq.0 .or. icom.gt.j) then
                     a_shot=.true.
                  else
                     c_shot=.true.
                  endif
               else if (line(j:j+9) .eq. "MDS_SERVER") then
                  if (icom.eq.0 .or. icom.gt.j) then
                     a_srv=.true.
                  else
                     c_srv=.true.
                  endif
               else if (line(j:j+7) .eq. "MDS_PATH") then
                  if (icom.eq.0 .or. icom.gt.j) then
                     a_path=.true.
                  else
                     c_path=.true.
                  endif
               else if (line(j:j+7) .eq. "MDS_TREE") then
                  if (icom.eq.0 .or. icom.gt.j) then
                     a_tree=.true.
                  else
                     c_tree=.true.
                  endif
               endif
            endif
            j=index(line(1:l),'EXPERT_FILE')
            if (j .gt. 0) then
               icom=index(line(1:l),'!')     ! check if in comment
               if (icom.eq.0 .or. icom.gt.j) then
                  size = max(size,l+1)       ! not in comment but will be
               end if
            end if
         endif
      end do
      if (ios .eq. -1) go to 105
 104  close(29)
         write(lun,'(" ?add_file -- error reading ",a,i6)')
     >        filnam,ios
         ier = 1
         return
 105  close(29)
      print*,' add_file: ',nlines,' lines -',size

C Allocate text array
C--------------------
      size=max(size,(len(mds_server)+13))
      size=max(size,40)
      ninsert=0
      if (nml) then                ! namelist
         total = nlines*size
         if (a_kmds .or. c_kmds) then
            if (a_kmds) c_kmds=.false.
         else
            ninsert=ninsert+1
         endif
         if (a_shot .or. c_shot) then
            if (a_shot) c_shot=.false.
         else
            ninsert=ninsert+1
         endif
         if (a_srv .or. c_srv) then
            if (a_srv) c_srv=.false.
         else
            ninsert=ninsert+1
         endif
         if (a_tree .or. c_tree) then
            if (a_tree) c_tree=.false.
         else
            ninsert=ninsert+1
         endif
         if (a_path .or. c_path) then
            if (a_path) c_path=.false.
         else
            ninsert=ninsert+1
         endif
         nlines=nlines+ninsert
         total=total + ninsert*size
      else if (iex90) then
         ninsert = 1
         nlines  = nlines+ninsert
         total   = nlines*size 
      else                        ! other file
         total = nlines*size
      endif
      allocate(text(total),stat=stat)
      if (stat .ne. 0) then
         write(lun,'(" ?add_file -- error allocating text(",i8,")",
     >        i6)') nlines*size,stat
         ier=1
         return
      else
C clear allocated array
         do i=1,(nlines*size)
            text(i) = ' '
         end do
      endif
 
      if (nml) then
C Check if found $TRDATA
         if (ninsert .gt. 0 .and. linsert .le. 1) then
            write(lun,'(" ?add_file --  missing $TRDATA ")')
C            ier = 1
C            return
            linsert = 20
         endif
      endif
 
C open file again, to read into text array
C------------------------------------------
      open (29,file=filnam,status='OLD',iostat=ios,err=101,
     >      action="read")
      if (ninsert .gt. 0  .and. nml) then
         i1=1
         i2=linsert
         n=2
      else
         i1=1
         i2=nlines
         n=1
      endif
      do jj=1,n
         do i=i1,i2
            j=(i-1)*size+1
            if (i==1 .and. iex90) then
               line = '!f90!'
            else
               read(29,'(a)',iostat=ios,err=103)line
            end if
            l = len_trim(line)
C Namelist:
            if (nml) then
               temp=line(1:l)
               call uupper(temp)
               icom=index(temp(1:l),'!')   ! check if in comment
               if (a_kmds .or. c_kmds) then 
                  kk=index(temp(1:l),'KMDSPLUS') ! force kmdsplus=1 
               else
                  kk=0
               endif
               k=index(temp(1:l),'MDS_')   ! Input Tree -> Run Tree
               k1=index(temp(1:l),'EXT')   ! EXT -> TRANSP Label
               if (kk.gt.0 )then
                  if ( c_kmds .or. (icom.eq.0 .or. kk.lt.icom)) then
                     line = ' kmdsplus=1' ! force kmdsplus=1
                     l = len_trim(line)
                  endif
               else if (k.gt.0 ) then
C        Replace with Run MDSplus
#ifdef __JET
                 if( mds_tree(1:5) .ne. "TRJOB" )       then
#endif
                  if (temp(k:k+7) .eq. "MDS_SHOT") then
                     if (c_shot .or. (icom.eq.0 .or. k.lt.icom)) then 
                        write(line,'(" mds_shot=",i10)')mds_shot
                     endif
                  else if (temp(k:k+9) .eq. "MDS_SERVER") then
                     if (c_srv .or. (icom.eq.0 .or. k.lt.icom)) then 
                        line = ' mds_server='//q//mds_server//q
                     endif
                  else if (temp(k:k+7) .eq. "MDS_TREE") then
                     if (c_tree .or. (icom.eq.0 .or. k.lt.icom)) then 
                        line = ' mds_tree='//q//mds_tree//q
                     endif
                  else if (temp(k:k+7) .eq. "MDS_PATH") then
                     if (c_path .or. (icom.eq.0 .or. k.lt.icom)) then 
                        line = ' mds_path='//q//mds_path//q
                     endif
                  endif
#ifdef __JET
                 endif
#endif
                  l = len_trim(line)
               else if (k1.gt.0 .and. (icom.eq.0 .or. k1.lt.icom)) then
C        Replace EXT with TRANSP Label = MDS Node Name
                  k=index(line(1:l),'=')
C        Leave ZZZZZZ unchanged
                  kz=index(line(k+1:l),'ZZZZZZZZ')
                  if (kz .le. 0 .and. k .gt. 0) then
                     temp = line
                     line = temp(1:k)//q//temp(k1+3:k1+5)
                     ll = len_trim(line)+1
C Check for multiple SIM
C Form: extsim(i) = 'ext1'
                     k2=index(temp(1:k),'(')+1
                     if (k2 .gt. 1) then
                        k3=index(temp(1:k),')')-1
                        do n=k2,k3
                           if (temp(n:n) .ne. ' ') m=ichar(temp(n:n))
                        enddo
                     else
C Form: extsim = 'ext1', 'ext2'
                        if (icom .gt. 0) l=icom
                        m=0
                        k2=index(temp(k:l),',')
                        do while (k2 .gt.0 .and. k2 .le. l)
                           if (m .eq. 0) m=49
                           line(ll:) = '0'//char(m)//q//','//q//
     >                          temp(k1+3:k1+5)
                           ll=ll+8
                           k=k+k2
                           k2=index(temp(k:l),',')
                           m=m+1
                        enddo
                     endif
                     if (m .gt. 0) then
                        line(ll:) = '0'//char(m)//q
                        ll=ll+2
                     else
                        line(ll:ll)=q
                     endif
C 05/28/04 CAL: bugfix
                     l=ll
                     print*,line(1:l)
                  endif
               endif
C 10/22/13 RGA: comment out expert_file namelist variable
               ii=index(adjustl(temp(1:l)),'EXPERT_FILE')
               if (ii .eq. 1) then
                  line(2:l+1)=line(1:l)
                  line(1:1) = '!'
                  l=l+1
               end if
               
            endif
            do ii=1,l
               text(j) = line(ii:ii)
               j=j+1
            end do
         end do
C Add MDSplus specifications
         if (jj .eq. 1 .and. n .eq. 2) then
            do i=1,5
               line=' '
               if (i .eq. 1) then
                  if (.not. a_kmds .and. .not. c_kmds) 
     &               line = ' kmdsplus=1'
               else if (i .eq. 2) then
                  if (.not. a_shot .and. .not. c_shot) 
     &               write(line,'(" mds_shot=",i10)')mds_shot
               else if (i .eq. 3) then
                  if (.not. a_srv .and. .not. c_srv) 
     &               line = ' mds_server='//q//mds_server//q
               else if (i .eq. 4) then
                  if (.not. a_tree .and. .not. c_tree) 
     &               line = ' mds_tree='//q//mds_tree//q
               else if (i .eq. 5) then
                  if (.not. a_path .and. .not. c_path) 
     &               line = ' mds_path='//q//mds_path//q
               endif
               if (line .ne. ' ') then
                  l = len_trim(line)
                  linsert=linsert+1
                  j=(linsert-1)*size+1
                  do ii=1,l
                     text(j) = line(ii:ii)
                     j=j+1
                  end do
               endif
            end do
         endif
         i1 = linsert+1
         i2 = nlines
      end do
 
 103  if (ios .eq. 0 .or. ios .eq. -1) then
         dsc = descr(14,text,nlines,0,size)
         stat = MdsPut(node//CHAR(0), '$'//CHAR(0),
     >        dsc,0)
         if(mod(stat,2).ne.1) then
            call mdsmsg(lun,'?add_file -- put '//node//
     >   ' status:  ',stat)
         endif
      else
         ier = 1
         write(lun,'(" ?add_file -- error reading ",a,i6)')
     >          filnam,ios
         go to 200
      endif
 
      ier = 0
 
 200  deallocate(text)
      return
 
 101  ier = 1
      write(lun,'(" ?add_file -- error opening ",a, i6)')
     >      filnam, ios
      return
      end
 
