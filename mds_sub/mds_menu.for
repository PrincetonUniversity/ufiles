C Menu to enter Server, Tree, etc.
C Used by mds_get_inf
C
C 02/13/03  C. Ludescher
C
      subroutine mds_menu(luntrm, mds_server,mds_tree,node,
     >           runid,device,year,mds_shot,nshot,devyr)
      implicit none
      integer       luntrm
      character*24  mds_tree
      character*64  mds_server
      character*10  runid, pulse
      character*20  node
      character*4   device
      character*7   devyr
      integer       mds_shot
      integer       nshot
      integer       year
 
C Local
      integer       mds_socket
      integer       ios, ier
      integer       ilt,  ils, ilr, iln, ls
C for uread
      LOGICAL LCHAR,  lxout
      integer ilnurd
 
C-----------------------------------------------------------
      call uinita(0)
 
      mds_tree='TRANSP_NSTX'
      mds_server='TRANSPGRID.PPPL.GOV'
C
      runid=' '
 
      mds_shot=0
      device='NSTX'
      year=0
      ils=index(mds_server,' ')-1
      if(ils.le.0) ils=len(mds_server)
      iln=index(node,' ')-1
 
C-----------------------------------------------------------------
C#@! prompt for server, tree, ... using uread
C#@! --------------------
C
 10   continue
      if (lxout(0)) then
         if (node .eq. 'NA' .or. node .eq. 'NONE') then
            write(luntrm,3002) mds_server(1:ils),mds_tree,mds_shot,
     >           runid, device, year
         else
            write(luntrm,3001) mds_server(1:ils),mds_tree,mds_shot,
     >           node, runid, device, year
         endif
 3001 format(/
     >        ' mds Server name:   ',a/
     >        ' mds Tree name:     ',a/
     >        ' mds pulse Number:  ',i10/
     >        ' Node:              ',a/
     >        ' Runid:             ',a/
     >        ' Experiment:        ',a/
     >        ' Year:              ',i2/
     >        ' options:'/
     >        '  S - set Server name'/
     >        '  T - set Tree name'/
     >        '  L - Nodename Label   (e.g. NAME_LIST, UFNAMES)'/
     >        '  N - set mds pulse Number (leave as 0 to assign)'/
     >        '  R - Runid'/
     >        '  E - Experiment'/
     >        '  Y - shot-Year'/
     >        '  Q - accept current settings'/
     >        '  X - quit/exit program'/)
 3002 format(/
     >        ' mds Server name:   ',a/
     >        ' mds Tree name:     ',a/
     >        ' mds pulse Number:  ',i10/
     >        ' Runid:             ',a/
     >        ' Experiment:        ',a/
     >        ' Year:              ',i2/
     >        ' options:'/
     >        '  S - set Server name'/
     >        '  T - set Tree name'/
     >        '  N - set mds pulse Number (leave as 0 to assign)'/
     >        '  R - Runid'/
     >        '  E - Experiment'/
     >        '  Y - shot-Year'/
     >        '  Q - accept current settings'/
     >        '  X - quit/exit program'/)
      endif
C
      call uread('mds_start:  run specs:$$')
      if(lchar('S')) then
         call ureadw('mdsrun:  server name:$$',mds_server)
         ils=index(mds_server,' ')-1
         if(ils.le.0) ils=len(mds_server)
      else if(lchar('T')) then
         call ureadw('mdsrun:  tree name:$$',mds_tree)
      else if(lchar('N')) then
         call ureadi('mdsrun:  pulse number:$$',mds_shot)
      else if(lchar('E')) then
         call ureadw('mdsrun:  Experiment:$$',device)
      else if(lchar('R')) then
         call ureadw('mdsrun:  RunId:$$',runid)
      else if(lchar('L')) then
         call ureadw('node:  Node Label:$$',node)
      else if(lchar('Y')) then
         call ureadi('mdsrun:  year:$$',year)
      else if(lchar('Q')) then
         go to 100
      else if(lchar('X')) then
         go to 9000
      else if(lchar('O')) then
         call ureadw('Option:  e.g.:CONTENTS:$$',node)
      else
         call uermsg('%mds_start:  invalid control option',' ',0)
      endif
      go to 10
 
C-----------------------------------------------------
 100  continue
      ilt=index(mds_tree,' ')-1
      if(ilt.le.0) ilt=len(mds_tree)
      call uupper(mds_tree)
C     extract shot from runid
      call checkid(runid,ilr,ls)
      read(runid(1:ls),'(i6)',iostat=ios) nshot
C      print*,'nshot = ',nshot
C
C Generate MDSplus pulse number
C -----------------------------
      if (mds_shot .eq. 0) then
         if (mds_tree(1:7) .eq. 'TRANSP_' .or.
     >        mds_tree(1:7) .eq. 'TRLOOK_' .or.
     >        mds_tree(1:ilt) .eq. 'JET') then
            call mkbeastid(runid,mds_shot,pulse)
            print*,'mds_shot = ',mds_shot
         endif
      endif
 
      ls=4
      if (device(4:4) .eq. ' ') ls=3
 
C TRANSP
      if (mds_shot .eq. 0) then
         write(devyr,'(a,".",i2.2)') device(1:ls),year
      endif
      return
 9000 call ufbad_exit
      return
      end
 
 
 
 
