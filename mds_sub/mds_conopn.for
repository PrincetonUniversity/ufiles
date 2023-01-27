      subroutine mds_conopn(lun, mds_server, mds_tree, runid, device,
     >                      year, mds_shot, mds_socket, option, ier)
      implicit none
C
C Input
      integer        lun     ! unit for error messages
      character*(*)  mds_tree   ! Tree name
      character*(*)  mds_server ! Server name
C                          ! either
      character*(*)  runid      ! Runid
      character*(*)  device     ! Tokamak
      integer        year       ! Year
      character*2    zy
C                          ! or
      integer        mds_shot   ! Mdsplus Id Number
C
C Return
      integer        mds_socket ! for repeated calls (not implemented)
 
      integer        option     !   1 = open to read output (restore)
C                               !   2 = open to read output (no-wait)
C                               !   3 = open to read (input only)
C                               !   4 = open for edit (no-restore)
C                               !   5 = Create
C                               !   6 = connect only
C
      integer       ier         !   not 0 = Error
C-----------------------------------------------------------------------
C Local
      character     devyr*7,  cshot*10
      integer       stat, ios
      integer       iln, ilt,  ils, ilr, ild, retlen, l, ls
      character*80  expr
      character*1   copt
      integer       nshot
 
C MDSplus
      integer       Mds_Value, idescr_long
      integer       mdsfconnect, MdsValue
      integer       mds_open
      integer       mdsstat, dsc, size
C
      integer ilnurd
C
      save      mdsstat
C      volatile      mdsstat
C
C---------------------------------------------------------------------------
      print*,'mds_conopn: option =', option,mds_shot,' ',mds_tree
      if (option .eq. 3) then
         copt='3'
      else if ( option .eq. 4) then
         copt='4'
      endif
 
C Connect to server
C------------------
      ils=index(mds_server,' ')-1
      if(ils.le.0) ils=len(mds_server)
      if (mds_server(1:ils) .ne. 'LOCAL' .and.
     >     mds_server(1:ils) .ne. 'local') then
         write(lun,*) ' ...connecting to server:  ',
     >        mds_server(1:ils)
         mds_socket = mdsfconnect(mds_server(1:ils)//char(0))
         if( mds_socket .lt. 0) then
            write(lun,'("?mds_conopn:  mdsfconnect stat: ",i5)')
     >           mds_socket
            ier=-1
            return
         endif
      else
         mds_socket = -1
      endif
C Connect only 
      if (option .eq. 6) then
         ier=0
         return
      endif
C
C Open Tree
C==========
      ilr=index(runid,' ')-1
      if(ilr.le.0) ilr=len(runid)
 
C User Tree TRW_
c     #@! for tree=TRW_*, set the pulse to the shot number
      if (mds_shot .eq. 0 .and. mds_tree(1:4) .eq. 'TRW_') then
C  extract shot from runid
         call checkid(runid,ilr,ls)
         read(runid(1:ls),'(i6)',iostat=ios) nshot
         mds_shot = nshot
      endif
C
C
C Option Create = 5
C==================
      if (option .eq. 5) then
         dsc = idescr_long(mdsstat)
         write(cshot,'(i10)',iostat=ier) mds_shot
         expr = 'tcl("SET TREE ' // mds_tree // '")' // CHAR(0)
         l=ilnurd(expr)
         print*, '...',expr(1:l)
         stat = MdsValue(expr, dsc, 0, size)
         if(mod(stat,2).ne.1) then
            call mdsmsg(lun,'?mds_opncon: '//expr(1:l)//' stat:  ',stat)
            ier = 0
            return
         endif
         if(mod(mdsstat,2).ne.1) then
            call mdsmsg(lun,'?mds_opncon: '//expr(1:l)//
     >                  ' mdsstat:  ',mdsstat)
            ier=1
            go to 9000
         endif

C#@! TCL(Create Pulse @{$4})
C#@create tree $3 mdsplus tree constructed in mds_opncon()
C#@! --------------
         expr = 'tcl("CREATE PULSE ' // cshot// '")' // CHAR(0)
         l=ilnurd(expr)
         print*, '...',expr(1:l)
         mdsstat=0
         dsc = idescr_long(mdsstat)
         stat = MdsValue(expr, dsc, 0, size)
         if(mod(mdsstat,2).ne.1) then
         call mdsmsg(lun,'?mds_opncon: '//expr(1:l)//
     >        ' mdsstat:  ',mdsstat)
            ier=1
            go to 9000
         endif
C
C End Create
C-----------
      else
C Open
C======
            if (mds_shot .eq. 0) then
               write(lun,'("%mds_conopn: no valid mds-pulse nubmer")')
               ier=1
               return
            endif
            ilt=index(mds_tree,' ')-1
            if(ilt.le.0) ilt=len(mds_tree)
            stat=mds_open(mds_tree(1:ilt),mds_shot)
            if(mod(stat,2).ne.1) then
               write(lun,'("%mds_conopn: failed to open ",a)')
     >              mds_tree(1:ilt)
               call mdsmsg(lun,' ?mds_conopn: mds_open '//
     >              mds_tree(1:ilt)//' stat:  ',stat)
               ier=-2
               go to 9000
            endif
         endif
C
C
C Open Tree for edit
      if (option .eq. 4 .or. option .eq. 5) then
         mdsstat=0
         dsc = idescr_long(mdsstat)
         write(cshot,'(i10)',iostat=ier) mds_shot
         expr = 'tcl("EDIT ' // trim(adjustl(mds_tree))
     >        // '/SHOT=' //trim(adjustl(cshot)) //
     >        '")' // CHAR(0)
         l=ilnurd(expr)
         print*, '...',expr(1:l)
         stat = MdsValue(expr, dsc, 0, size)
         if(mod(stat,2).ne.1) then
            call mdsmsg(lun,'?mds_conopn: '//expr(1:l)//' stat:  ',
     >           stat)
            ier = 1
            return
         endif
         if(mod(mdsstat,2).ne.1) then
            call mdsmsg(lun,'?mds_conopn: '//expr(1:l)//' cmdst: ',
     >           mdsstat)
C            if (device .eq. "HL2A") then
C               ier = 0
C            else
              ier = 1
C            endif
            return
         endif
      endif
 
      ier=0
C
 9000 return
C
      end
 
 
 
 
