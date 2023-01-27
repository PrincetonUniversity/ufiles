      subroutine add_inf(lun, runid, ier)
C Read TR.INF to get date, user, comment
C 02/14/01 C.Ludescher
C
C History:
C
      implicit none
C Input
      integer         lun           ! for error messages
      character*(*)   runid
C Return
      integer         ier
 
      character*12    user
      character*120   expr
      character*132   line, zline
      character*60    infnam
      character*9     date
      character*1     zap
      integer         i, l, l1, l2, l3, ios, stat, ier1
      logical         ex, badinf, no_date_began
      character*132   text(100)
      character*1,    allocatable :: text2(:) 
      integer         MdsPut, MdsValue
      integer         idescr_cstring, idescr_long
      integer         descr, dsc, mdsstat, size
      integer         nlines, maxl, ii, jt
C------------------------------------------------------------------
C clear text array
      do i=1,100
         text(i) = ' '
      end do

      ier = 0
      ier1= 0
      badinf = .false.
      no_date_began=.false.
      zap=char(39)
 
C open TR.INF file
C-------------------
      infnam = runid//'TR.INF'
CXX      print *,infnam
      open (29,file=infnam,status='OLD',iostat=ios,err=1001,
     >      action="read")
      read(29,'(a)',iostat=ios,err=102,end=106)line
      l2 = len_trim(line)
CXX      print*,'line=',l2, line
 
C get DATE_BEGAN
      l=0
      do while (l .le. 0)
          l = index(line,'D=')
CXX          print *,'l=',l
C     Check if screwed up
          if (l .gt. 0) then
CXX           print *,'D=',l
              l1 = index(line,'-') ! date format: dd-mmm-yy
CXX           print *,'l1=',l1
              if (l1 .le. 0) then
C check if unix date
CXX               print *,'try unix'
                  l1 = index(line(l:),' ') ! date on unix: ddMmmyyyy
                  if (l1 .le. 0) then
                      l=0
                  else
                      l = l1-8
                      date = line(l:l+8)
                  endif
              else
                  if (l .gt. l1) l=l1-4
                  date = line(l+2:l+10)
              endif
          endif
 
         if (l .le. 0) then
             read(29,'(a)',iostat=ios,err=102,end=106)line
             l2 = len_trim(line)
CXX          print*,'line=',l2, line
         endif
      end do
 
      print*,'date =',date
      stat=MdsPut('DATE_BEGAN'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(date),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?add_inf: put DATE_BRGAN - stat:  ',stat)
      end if
 
C
C get USER_ID
 
C Note could be  ' BY' or ' BY:'
      l1 = index(line,' BY') +4
CXX   print *,'l1=',l1
      do while (l1 .le. 4 .and. .not. badinf)
         read(29,'(a)',iostat=ios,err=102,end=106)line
         l2 = len_trim(line)
CXX      print*,'line=',l2,line
         l1 = index(line,' BY') +4
CXX      print *,'l1=',l1
         if (index(line,'<*=*>') .gt. 0) then
            badinf=.true.
CXX         print *,'%add_inf -- premature <*=*>'
         endif
      end do
      do while (line(l1:l1) .eq. ' ')
         l1=l1+1
      end do
      l2 = index(line(l1:),' ') +l1-2
CXX   print *,'user: ',line(l1:l2),l1,l2
      if (line(l1:l2+7) .eq. 'TRANSP SYSTEM') then
          badinf = .true.
CXX       print *,'%add_inf -- TR.INF looks peculiar !!'
      endif
      user=line(l1:l2)
      stat=MdsPut('USER_ID'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(user(1:l2-l1+1)),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?add_inf: put USER_ID - stat:  ',stat)
      end if
 
C
      read(29,'(a)',iostat=ios,err=102,end=106)line
C
C Don't get comment if TR.INF is not normal
      if (badinf) goto 3000
C
c get COMMENT
      nlines=1
      read(29,'(a)',iostat=ios,err=102,end=106)line
      if (line(1:18) .eq. '# User comments on' .or.
     >    line(1:19) .eq. ' # User comments on' ) then
         read(29,'(a)',iostat=ios,err=102,end=106)line
      endif
 2000 continue
      maxl=0

C      l3=1
      do while (index(line,'<*=*>') .eq. 0)
         l2 = index(line,zap)
         if (l2 .gt. 0) then
             zline=line(1:l2-1)//zap//line(l2:)
             l=len_trim(zline)
             line=zline(1:l)
         else
             l=len_trim(line)
         endif
         if (l .gt. maxl) maxl=l 
         do ii=1,l
            text(nlines)(ii:ii) = line(ii:ii)
         end do
         
CXX      print *,l3,' ',text(1:l3)
         read(29,'(a)',iostat=ios,end=1002,err=102)line
         nlines=nlines+1
         l = len_trim(line)
CXX      print*,'line',nlines,'=',l, line
      end do
 1002 continue
C     Only write Comment of non-zero
      if (nlines .gt. 0 .and. maxl .gt. 0) then
         allocate(text2(nlines*maxl),stat=stat)
         if (stat .ne. 0) then
            write(lun,'(" ?add_inf -- error allocating text2(",i8,")",
     >        i6)') nlines*maxl,stat
            ier=1
            return
         endif
         jt=1
         do i=1,nlines
            do ii=1,maxl
               text2(jt)=text(i)(ii:ii)
               jt=jt+1
            enddo
         enddo
         dsc = descr(14,text2,nlines,0,maxl)
         stat = MdsPut('COMMENT'//CHAR(0), '$'//CHAR(0),
     >        dsc,0)
         if(mod(stat,2).ne.1) then
            call mdsmsg(lun,'?add_inf: -- put comment stat:  ',stat)
            ier = 1
         endif
         deallocate(text2)
      endif
C
C get EXPERT module
C only used if EX.FOR exists
 3000 l=len_trim(infnam)
      infnam(l-5:l)='ex.for'
      print*,'check for ', infnam
      inquire(file=infnam,exist=ex,iostat=ios)
      if (.not. ex) then
         infnam(l-5:l)='ex.f90'
         print*,'check for ', infnam
         inquire(file=infnam,exist=ex,iostat=ios)
      end if         
      if (ex) then
         print*,'add EXPERT_CODE'
C add EXPERT_CODE
         call add_file(lun,runid,'EXPERT_CODE',
     >        ' ',' ',0,' ',ier1)
      endif
      ier = ier + ier1
      return
C----------------------------------------------
C Try reading <runid>CC.TMP
 1001 continue
CXX   print *,'error opening ',infnam
      infnam = runid//'CC.TMP'
      print *,'try  ',infnam
      open (29,file=infnam,status='OLD',iostat=ios,err=1003,
     >      action="read")
      read(29,'(a)',iostat=ios,err=102,end=106)line
C Get username
C Check if <runid>.OWNER exists
      infnam = runid//'.OWNER'
CXX   print *,'try  ',infnam
      open (28,file=infnam,status='OLD',iostat=ios,action="read")
      if (ios .eq. 0) then
         read(28,'(a)',iostat=ios,err=102,end=106)user
         close(28)
         print*,'user from file = ',user
      else
         call sget_user(user)
         print*,user
      endif

C Get date
      call c9date(date)
C
C Write Nodes
 1004 stat=MdsPut('USER_ID'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(user),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?add_inf: put USER_ID - stat:  ',stat)
      end if
      if ( .not. no_date_began) then
        stat=MdsPut('DATE_BEGAN'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(date),0,size)
        if (mod(stat,2).ne.1) then
           call mdsmsg(lun,
     >        '?add_inf: put DATE_BEGAN - stat:  ',stat)
        end if
      endif
 
C Read Comment
      nlines=1
C      read(29,'(a)',iostat=ios,err=102,end=106)line
      if (line(1:18) .eq. '# User comments on' .or.
     >    line(1:19) .eq. ' # User comments on' ) then
         read(29,'(a)',iostat=ios,err=102,end=1002)line
      endif
      goto 2000
C---------------------------------------------------------------------------
C For Re-writes
 1003 continue
      infnam = runid//'.COMMENT'
      print *,'try  ',infnam
      open (29,file=infnam,status='OLD',iostat=ios,err=101,
     >      action="read")
      read(29,'(a)',iostat=ios,err=102,end=106)line
C Get username
      infnam = runid//'.USER_ID'
      print *,'try  ',infnam
      open (28,file=infnam,status='OLD',iostat=ios,err=101,
     >      action="read")
      read(28,'(a)',iostat=ios,err=102,end=106)user
      print*,user
      close(28)
C Get date began
      infnam = runid//'.DATE_BEGAN'
      print *,'try  ',infnam
      open (28,file=infnam,status='OLD',iostat=ios,err=105,
     >      action="read")
      read(28,'(a)',iostat=ios,err=102,end=106)date
      print*,date
      close(28)
      no_date_began=.false.
      goto 1004
C-----------------------------------------------------------------------------
 101  print *,'error opening ',infnam
      ier=-1
      return
 102  print *,'error reading ',infnam
      ier=1
      return
 103  print *,'error writing ',ios
      ier=1
      return
 104  print *,'error writing ',ios
      ier=1
      return
 105  print *,'error opening ',infnam
      no_date_began=.true.
      goto 1004
 106  print *,'error end of file ',infnam
      ier=1
      return
      end
 
 
 
