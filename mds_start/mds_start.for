      program mds_start
c
c RGA: the lines with '#' followed by '@' are script comments scanned by trman
c
C#@! Fortran Program -- Create MDSplus Pulse
C
C#@usage mds_start [ uread_commands ]
C
C#@arg E set experiment
C#@arg <tok> tokamak
C#@arg R set runid
C#@arg <runid> the runid
C#@arg Y set year
C#@arg <yr> year of shot
C#@arg N set mdsplus pulse
C#@arg <pulse> mds pulse Number (leave as 0 to assign)
C#@arg S set server name
C#@arg <server> mdsplus server
C#@arg T set tree name
C#@arg <tree> mdsplus tree name
C#@arg Q accept current settings and generate pulse
C#@arg X quit/exit

C#@! Option Argument O
C#@! O = CONTENTS    -  Write TF.PLN to CONTENTS node 
C#@! O = FIX_DATE    -  read <runid>.DATE_BEGAN and DATE_EXEFILE
C#@! O = FIX_USER    -  read <runid>.USER_ID, DATE_RUN and COMMENT
C#@! O = ADD_MRK     -  add MRK_TIME SRC_TIME
C#@! O = STEER       -  open existing Tree
C#@! O = TEMPLATE    -  add template to Tree
C
C#@! 01/17/01 C.Ludescher
C#@!
C#@! Creates a MDSplus pulse and writes the TRANSP input (Ufiles) and most header
C#@! nodes. The VMS version is also capable of writing the TRANSP output into the
C#@! tree. Depending on the Tree, "actual data" is written, or "functions" are
C#@! written into MDSplus. ( MDSplus supports both ).
C#@!
C#@! It can create/write any kind of tree, since it is driven by *TRDAT.TMP.
C#@! So it can be used to write the user trees TRW_<user>, e.g. TRW_KAYE, TRW_BUDNY,
C#@! or to extract the ufiles from these trees ( if UFCOPY is set).
C#@! ( The latter features have not been used in a while ).C#@!
C#@!
C#@!  Note:
C#@!  VMS  prtran(pretrwv)  when writing <runid>TR.COM for Tree DBGTRANSP
C#@!                        uses "default" for mds_server, which is assumed to be
C#@!                        LOCAL on Birch
C#@!       allcom:beast2mds for Tree TRANSP also uses "default"
C#@!
C#@!  Unix pretran(pretrwu) when writing <runid>mds.sh uses $MDS_TRANSP
C#@!                        to check if MDSplus is requested
C#@!                        It then uses $MDS_TRANSP_TREE
C#@!                           and       $MDS_TRANSP_SERVER
C#@begin
C---------------------------------------------------------------------------
      implicit none
 
      integer       mdsfconnect
 
      character*24  mds_tree
      character*64  mds_server
      character*10  runid, pulse
      character*20  node
      character*4   device
      integer       mds_shot
      integer       mds_socket
      integer       nshot
      integer       year
C Local
      integer       stat, ios, ier
      integer       ild, ilt,  ils, ilr, ls, retlen, l
      character*7   zdevyr
      character*80  expr

      integer       iln,  iopt, option
      logical       znb
C MDSplus
      integer       Mds_Value, idescr_long
      integer       MdsValue,  dsc, mdsstat, size
C for uread
      LOGICAL LCHAR,  lxout
      integer lun
      integer ilnurd
 
C-----------------------------------------------------------
      lun = 6
      stat = 0
      call uinita(0)
 
      mds_tree='TRANSP_NSTX'
      mds_server='TRANSPGRID.PPPL.GOV'
C
      runid=' '
      mds_shot=0
      device='NSTX'
      year=0
      option=0
      node='NA'
C
C prompt for server, tree, ... using uread
      call mds_menu(lun, mds_server, mds_tree, node,
     >           runid,device,year,mds_shot,nshot,zdevyr)

      ils=index(mds_server,' ')-1
      if(ils.le.0) ils=len(mds_server)
      ilt=index(mds_tree,' ')-1
      if(ilt.le.0) ilt=len(mds_tree)

C Check for option
C Default is Create 
C If "NODE" is specified, add a node to an existing Tree
      iopt=5           ! Default = create Tree
      if (node .ne. 'NA') then
         iln=index(node,' ')-1
         if(iln.le.0) iln=len(node)
         if(node(1:iln) .eq. 'CONTENTS') then
            option=1
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'FIX_DATE') then
            option=2
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'FIX_USER') then
            option=3
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'ADD_MRK') then
            option=4
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'STEER') then
            option=5
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'COMMENT') then
            option=6
            iopt=4     ! Open for edit
         else if(node(1:iln) .eq. 'TEMPLATE') then
            option=7
            iopt=4     ! Open for edit

         endif
      endif
C
C#@! Connect to Server / Create Tree
C#@call mds_conopn <lun> @{$10} @{$12} @{$4} <tok> <nshot> <yr> <pulse>  <socket> <iopt> <stat>
C#@! -------------
      call mds_conopn(lun,mds_server,mds_tree,runid,device,year,
     >                mds_shot, mds_socket, iopt, ier)
      if (ier .ne. 0) then
         print*,'mds_start - failed to connect/open ',mds_tree, mds_shot
         go to 9000
      endif
      call checkid(runid,ilr,ls)
      ild=4
      if (device(4:4) .eq. ' ') ild=3

      if (option .eq. 0 .or. option .eq. 5) then 
C      Write <runid>.PULSE for tr_start.pl and for beast2mds.com
         open(7,STATUS='replace',iostat=ios,
     >        file=runid(1:ilr)//'.PULSE')
         write(7,'(" MDS_SHOT =",i11)',iostat=ios)mds_shot
         if (ios .ne. 0) then
            print*,'%mds_start: Error writing '//runid(1:ilr)//
     >           '.PULSE'
            go to 9000
         endif
         CLOSE(7)
      endif	
C#@! If fix_user
C#@call add_inf <lun> @{$4} <option> <stat>
      if (option .eq. 3) then
         call add_inf(lun, runid(1:ilr), stat)
         if (stat .ne. 0) then
            print*,'mds_start - failed to add inf'
            go to 9000
         endif
         call ufgood_exit
C#@! If pretr
C#@call add_pretr <lun> @{$4} <option> <stat>
      else if (option .ge. 1 .and. option .lt. 5) then
         if (option .eq. 4) then  ! add  nubeam_init.dat ?
            inquire(file=runid(1:ilr)//'_nubeam_init.dat',
     >              exist=znb,iostat=ios)
            if (znb) option = option+20
         endif
         call add_pretr(lun,
     >        mds_shot, runid(1:ilr), option, stat)
         if (stat .ne. 1) then
            print*,'mds_start - failed to add pretr'
            go to 9000
         endif
         if (ier .ne. 0) go to 9000
         call ufgood_exit
      else if (option .eq. 6) then
         call add_file(lun,runid(1:ilr),'COMMENT',
     >        ' ',' ',0,' ',stat)
         if (stat .ne. 0) then
            print*,'mds_start - failed to replace COMMENT'
            go to 9000
         endif         
         call ufgood_exit
      else if (option .eq. 7) then
         call add_tmplt(lun, runid(1:ilr), 1, stat)
         if (stat .ne. 0) then
            print*,'mds_start - failed to add DEFAULT Template'
            go to 9000
         endif    
         expr = 'tcl("WRITE")' // CHAR(0)
         l=ilnurd(expr)
         dsc = idescr_long(mdsstat)
         print*,expr(1:l)
         stat = MdsValue(expr, dsc, 0, size)
         print*,'stat =',stat
         if(mod(mdsstat,2).ne.1) then
            print*,'mds_start - failed to write DEFAULT Template'
            go to 9000
         endif     
         call ufgood_exit
      endif
C
C#@! Create Pulse
C#@call create_pulse @{$10} <socket> @{$12} <pulse> @{$4} <tok> <nshot> <yr> <stat>
C#@! -------------
C     If option = 5: only write Namelist and Ufiles

      if (option .eq. 5) then
         stat=77
         print *,'%mds_start: steering '
      endif
      call create_pulse(mds_server(1:ils),mds_socket,mds_tree(1:ilt),
     >     mds_shot,runid(1:ilr),device(1:ild),nshot,year,stat)
      if (stat .ne. 1) then
         print*,'mds_start - failed to create ',mds_tree, mds_shot
         print*,'device=',device(1:ild),'  on ',mds_server(1:ils)
         if (device(1:ild) .eq. 'D3D' .and. 
     >       mds_server(1:ils) .eq. 'atlas.gat.com' .or.
     >       mds_server(1:ils) .eq. '_atlas.gat.com' .or.
     >       mds_server(1:ils) .eq. 'ATLAS.GAT.COM' .or.
     >       mds_server(1:ils) .eq. '_ATLAS.GAT.COM' ) then
            print*,'exit(3)'
            call exit(3)
            call ufbad_exit
         else
            go to 9000
         endif
      endif
      call ufgood_exit
 9000 continue
         call ufbad_exit
 
      end
