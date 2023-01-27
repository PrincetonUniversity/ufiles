c
c RGA: the lines with '#' followed by '@' are script comments scanned by trman
c
C Create_pulse
c
C#@! Fortran Subroutine -- Create an MDS Pulse
c
c#@usage create_pulse(mds_server,mds_socket,mds_tree,mds_shot,runid,device,nshot,year,stat)
c
c#@arg mds_server mdsplus server already opened
c#@arg mds_socket socket connection or -1 for local
c#@arg mds_tree   name of mdsplus tree
c#@arg mds_shot   pulse number
c#@arg runid      runid
c#@arg device     tokamak
c#@arg nshot      shot number
c#@arg year       er, year
c#@arg stat       returned stat value
c#@begin
c
c     22May2007    jim.conboy@jet.uk
c     ifdef __JET 
c          Dont add Ufiles to the output tree, if it is named TRJOB 
c          [ JET data is in separate TRDATA tree, created by local 
c            web application                                      ]
c-------------------------------------------------------------------

      subroutine create_pulse(mds_server,mds_socket,mds_tree,
     >           mds_shot,runid,device,nshot,year,stat)
      implicit none
      integer ilnurd   ! ufencod function
 
 
      character*(*)  mds_server
      integer        mds_socket
      character*(*)  mds_tree
      integer        mds_shot
      character*(*)  runid
      character*(*)  device
      integer        nshot
      integer        year
      integer        stat
 
      integer        genex_tcl
C MDSplus
      integer        MdsValue, MdsPut, idescr_long, idescr_cstring
      integer        MdsSetSocket,      dsc,         mdsstat
C Local
      character*64  mds_path
      character*100 expr
      character*10  cshot
      integer       size
      integer       ierr, l, lp, ld
      character*7   devyr
      integer       lun
      character*12  code, zenv
      character*2   zy
      integer       option
C---------------------------------------------------------------
      lun = 6
      option=0
C Check if ierr = 77 = steering: add only Namelist and Ufiles
      if (stat .eq. 77) then
         stat=0
         option=2
         print*,'%create_pulse: steering'
         goto 1000
      endif

C#@! Add Source_Shot
      call mds_wr_source_shot(device,nshot,stat)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?create_pulse: put SOURCE_SHOT - stat:  ',stat)
      end if
      print *,'create pulse: did source_shot'
C#@! Add Shot_Idate
      stat=MdsPut('SHOT_IDATE'//CHAR(0),'$'//CHAR(0),
     >     idescr_long(year),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?create_pulse: put SHOT_IDATE - stat:  ',stat)
      end if
 
C#@! Add RUNID
      stat=MdsPut('RUNID'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(runid),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?create_pulse: put RUNID - stat:  ',stat)
      end if
 
C#@! Add NAME_OF_CODE
c#@use env $TRANSP_MDSPLUS set this to BEAST to have the name of the code be TRANSP-BEAST
      call sget_env('TRANSP_MDSPLUS',zenv)
      if(zenv.eq.'BEAST') then
         code = 'TRANSP-BEAST'
         l=12
      else
         code = 'TRANSP'
         l=6
      endif
      stat=MdsPut('NAME_OF_CODE'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(code(1:l)),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?create_pulse: put NAME_OF_CODE - stat:  ',stat)
      end if
      print*,'call ',DEVICE 
C#@! Add DEVICE
      stat=MdsPut('DEVICE'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(device),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?create_pulse: put DEVICE - stat:  ',stat)
      end if
C
      ld=index(device,' ')-1
      if (ld .lt. 0) ld=len(device)
      write(devyr,'(a,".",i2.2)',iostat=ierr) device(1:ld),year
      ld=ld+3
      print*,devyr(1:ld)
 
C
C#@! Add Miscellaneous, filled in from add_inf() includes DATE_BEGAN, USER_ID, COMMENT
c#@! add EXPERT_CODE file
c#@call add_inf lun runid ierr
c#@use file @{$5}TR.INF read to get date, user, comment if it exists
c#@use file @{$5}CC.TMP comment file from tr_start.pl
c#@call add_file lun $5 EXPERT_CODE 0 ier1
c#@use file @{$5}EX.FOR expert code file if it exists
C ------------------
c
 1000  continue
          print *,' Call add_inf(',runid,')'
          call add_inf(lun,runid,ierr)
          if(ierr .ne. 0) then
              print *,'Error adding comment, etc...'
          endif
c
C#@! Add NameList
c#@call add_file lun $5 NAME_LIST $1 $3 $4 <path> ierr
c#@use file @{$5}TR.DAT namelist file
C#@! <path> = PPPL@<tok.yr>@<runid> for BIRCH else .INPUTS
c#@! replace namelist variables with mdsplus pointers to @{$1},@{$3},@{$4}, and <path>
c
          if (mds_server .eq. 'BIRCH.PPPL.GOV' .and.
     >        mds_tree .eq. 'TRANSP') then
             mds_path='PPPL@'//devyr(1:ld)//'@'//runid
          else
             mds_path='.INPUTS'
          endif
          lp=index(mds_path,' ')-1
          if (lp .lt. 0) lp=len(mds_path)
          call add_file(lun,runid,'NAME_LIST',mds_server,
     >    mds_tree,mds_shot,mds_path(1:lp),ierr)
          if(ierr .ne. 0) then
              print *,'?create_pulse: Error adding NAME_LIST'
              stat=0
              return
          endif
c
C#@! Add Ufiles unless the @{$3}==TRLOOK_*
C#@call add_ufnodes lun $5 $2 $3 $4 %6 ierr
C#@! ------------
C
#ifdef __JET
          if (mds_tree(1:5) .ne. 'TRJOB'  ) then
#else
          if (mds_tree(1:7) .ne. 'TRLOOK_') then
#endif
             print *,' Call add_ufnodes(',runid,')'
             call add_ufnodes(lun,runid,mds_socket,mds_tree,
     >            mds_shot,device,ierr)
C
C Check if need to add template namelist
             call add_tmplt(lun,runid, 1, ierr)
             if(ierr .ne. 0) then
                print *,'?create_pulse: Error adding template'
             endif
C
             if (mds_socket .gt. 0) stat = MdsSetSocket(mds_socket)
          else
             print *,'?create_pulse: add_ufnodes not called for '
     >              ,trim(mds_tree)
          endif
C
C#@! TCL(WRITE)
C#@modify tree $3
c
          expr = 'tcl("WRITE")' // CHAR(0)
          l=ilnurd(expr)
          print*, '...',expr(1:l)
          dsc = idescr_long(mdsstat)
          stat = MdsValue(expr, dsc, 0, size)
          if(mod(mdsstat,2).ne.1) go to 9001
          expr = 'tcl("CLOSE")' // CHAR(0)
          l=ilnurd(expr)
          print*, '...',expr(1:l)
          dsc = idescr_long(mdsstat)
          stat = MdsValue(expr, dsc, 0, size)
          if(mod(mdsstat,2).ne.1) go to 9001
          if (ierr .eq. 0) then
              stat=1
          else
              stat=0
          endif
 
      return
 
 9000 call mdsmsg(lun,'?create_pulse: '//expr(1:l)//
     >   ' mdsstat:  ',mdsstat)
      stat = 0
      return
 9001 call mdsmsg(lun,'?create_pulse: '//expr(1:l)//
     >   ' mdsstat:  ',mdsstat)
      stat = 0
      return
      end
 
