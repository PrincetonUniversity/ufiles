      program mds_get_inf
      implicit none
C Retrievs a text node from a TRANSP run
C and writes it in a file -> <runid>.<node-name>
C
C 04/19/04 - C. Ludescher-Furth
C
C Non-standard extracts:
C COMMENT   -> <runid>CC.TMP
C NAME_LIST -> <runid>TR.DAT
C UFNAMES =  list of ufiles name path
C DATES   =  :DATE_RUN and :DATE_EXEFILE
C USER+   =  :USER_ID  and :DATE_BEGAN and :COMMENT
C ENQUE   =  :NAME_LIST :USER_ID :ISHOT and :COMMENT
C ENQUE2  =  :NAME_LIST :USER_ID :ISHOT and :COMMENT and :EXEFILE
C
      character*24  mds_tree
      character*64  mds_server
      character*20  node
      character*10  runid
      character*4   device
      character*7   devyr
      integer       mds_shot
      integer       mds_socket
      integer       nshot
      integer       year
C Local
      character     prefix*12, ext*20
      integer       stat, ios, ier
      integer       iln, ilr,  ile, opt
      integer       i,   k,    nloop
C
      integer luntrm
      integer ilnurd
C
C
C---------------------------------------------------------------------------
      luntrm = 6
      opt = 0
 
      call ufheader("mds_get_inf")

      print*,'mds_get_inf retrievs a text node from a TRANSP run'
      print*,'         and writes it into a file <runid>.<node_label>'
      print*,'         or uses TRANSP conventions:'
      print*,'         name_list  -> <runid>TR.DAT'
      print*,'         contents   -> <runid>TF.PLN'
      print*,' '
 
      node=' '
      call mds_menu(luntrm, mds_server,mds_tree,node,
     >     runid,device,year,mds_shot,nshot,devyr)
 
      ilr=index(runid,' ')-1
      if(ilr.le.0) ilr=len(runid)
 
      call mds_conopn(luntrm,mds_server,mds_tree,runid,device,year,
     >                mds_shot, mds_socket, 3, ier)
      if (ier .ne. 0) then
         print*,' '
#ifdef __LINUX
C        unique error exit to be checked by tr_enqueue.pl
         if (ier .eq. -1) then
            print*,'?mds_get_inf failed to connect to '//mds_server
            call exit(3)         ! Failed to connect
         else if (ier .eq. -2) then
            print*,'?mds_get_inf failed: No Tree File'
            call exit(5)         ! Tree does not exist
         else
            print*,'?mds_get_inf failed'
            call exit(2)
         endif
#else
         call ufbad_exit("?mds_get_inf failed")
#endif
      endif
C
C
C Extract text from MDSplus and write into file
C----------------------------------------------
      if (node(1:1) .eq. ' ') node='COMMENT'
      iln=index(node,' ')-1
      if(iln.le.0) iln=len(node)
C generate file name: use runid or mds_shot number
      print*,'mds_shot =',mds_shot
      if (runid(1:1).ne. ' ') then
         prefix=runid
      else
         write(prefix,'(i11)',iostat=ios) mds_shot
         ilr=ilnurd(prefix)
C sqeeze out leading blanks
         if (prefix(1:1) .eq. ' ') then
            k=1
            do i=1,ilr
               if (prefix(i:i) .ne. ' ') then
                  prefix(k:k) = prefix(i:i)
                  k=k+1
               endif
            end do
            prefix(k:)=' '
            ilr=k-1
         endif
      endif
C
      if (node(1:iln) .eq. 'UFNAMES') then
         call mds_getufnames(luntrm,
     >        prefix(1:ilr)//'.UFNAMES',ier)
      else if (node(1:iln) .eq. 'COMMENT') then
         call mds_getfile(luntrm,node(1:iln),
     >        prefix(1:ilr)//'CC.TMP',ier)
      else if (node(1:iln) .eq. 'CONTENTS') then
         call mds_getfile(luntrm,node(1:iln),
     >        prefix(1:ilr)//'TF.PLN',ier)
      else if (node(1:iln) .eq. 'NAME_LIST') then
         call mds_getfile(luntrm,node(1:iln),
     >        prefix(1:ilr)//'TR.DAT',ier)
      else
         if (node(1:5) .eq. 'DATES') then
            node='DATE_RUN'
            iln=8
            ext=node
            ile=iln
            nloop=2
            opt=1
         else if (node(1:5) .eq. 'USER+') then
            node='USER_ID'
            iln=7
            ext=node
            ile=iln
            nloop=3
            opt=2
C Extract everything for tr_enqueue
         else if (node(1:5) .eq. 'ENQUE') then
            if (node(6:6) .eq. '2') then
               node='EXPERT_CODE'
               iln=11
               call mds_getfile(luntrm,node(1:iln),
     >              prefix(1:ilr)//'ex.for',ier)
               if(ier.ne. 0) then
                  print*,node(1:iln),' does not exist'
               else
                  call expert_format(prefix(1:ilr))
               endif
            endif
            node='NAME_LIST'
            iln=9
C if no namelist, stop
            call mds_getfile(luntrm,node(1:iln),
     >        prefix(1:ilr)//'TR.DAT',ier)
            if(ier.ne. 0) then
               print*,node(1:iln),' does not exist'
               goto 9000
            endif
            node='SOURCE_SHOT'
            iln=11
            call mds_getnum(luntrm,node(1:iln),
     >        prefix(1:ilr)//'.SHOT',1,ier)
            if(ier.ne. 0) then
               print*,node(1:iln),' does not exist'
            endif
            node='USER_ID'
            iln=9
            ext='OWNER'
            ile=5
            nloop=2
            opt=3
         else
            ext=node
            ile=iln
            nloop=1
         endif
         do i=1,nloop
            call mds_getfile(luntrm,node(1:iln),
     >           prefix(1:ilr)//'.'//ext(1:ile),ier)
            if (nloop .gt. 1) then
               if(ier.ne. 0)
     >            print*,node(1:iln),' does not exist'
            endif
C DATES
            if (opt .eq. 1) then
               node='DATE_EXEFILE'
               iln=12
               ext=node
               ile=iln
C USER+
            else if (opt .eq. 2) then
               if (i .eq. 1) then
                  node='COMMENT'
                  iln=7
                  ext=node
                  ile=iln
               else if (i .eq. 2) then
                  node='DATE_BEGAN'
                  iln=10
                  ext=node
                  ile=iln
               endif
C ENQUE{2}
            else if (opt .eq. 3) then
                  node='COMMENT'
                  iln=7
                  ext=node
                  ile=iln
            endif
         enddo
      endif
      if(ier .ne. 0) then
         print*,node(1:iln),' does not exist'
         if (node(1:iln) .eq. 'COMMENT' .and. opt .eq. 3) then
            call ufgood_exit
         else
            goto 9000
         endif
      else 
         call ufgood_exit
      endif
C
 9000 continue
      call ufbad_exit("")
C
      contains

      !
      ! ------------- expert_format ----------
      ! The EXPERT_CODE node will contain an f90 formatted file if the first line
      ! contains the marker '!f90!' added by add_file().
      !
      subroutine expert_format(fpre)
      character*(*), intent(in) :: fpre   ! prefix for files

      character*32  :: fpfor, fp90  ! expert files
      character*256 :: line         ! one line from ascii expert file

      integer :: ios     ! i/o status
      integer :: ii      ! discarded error

      fpfor = trim(fpre)//'ex.for'
      fp90  = trim(fpre)//'ex.f90'

      open(unit=31, file=fpfor, status='old')

      read(31,'(a)') line
      line = adjustl(line)
      
      if (line=='!f90!') then
         !
         ! remove first line and put in <runid>ex.f90 file
         !
         print *, "moving expert file from "//trim(fpfor)
     &        //" to "//trim(fp90)

         open(unit=32, file=fp90, status='replace')
         do
            read(31, '(a)',iostat=ios,end=100) line
            if (ios/=0) goto 100
            write(32,'(a)') trim(line)
         end do
 100     close(32)
         close(31)

         call fdelete(fpfor, ii)  ! do not allow <runid>ex.for to exist
      else
         !
         ! this is a <runid>ex.for file
         !     
         close(31)

         call fdelete(fp90, ii)   ! do not allow <runid>ex.f90 to exist
      end if

      end subroutine expert_format

      end
 
