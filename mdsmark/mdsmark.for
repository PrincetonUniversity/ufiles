C******************** START FILE MDSMARK.FOR ; GROUP MDSMARK_SUB ***********
C   	==============================================================
C
C  write a string to an MDS+ tree on a remote server
C  dmc 9 Apr 1998
C
C History:
C 01/10/00 CAL: support new portable MDSplus library
 
      PROGRAM MDSMARK
c
        character*64 mds_server         ! remote server name if nonblank
        character*24 mds_tree           ! tree name
C
      integer mds_shot
      integer stat
C
      character*64 mds_path,mds_string
C
      logical lxout
C
      character*128 mds_msg
      common/cmdsmsg/ mds_msg
C
C
C-----------------------------------------------------------------------------
C
      call ufheader("mdsmark")
      call uinita(0)
      luntrm=lnout(0)
C
      mds_server='Q'
      mds_tree='Q'
      mds_shot=0
      mds_path='Q'
      mds_string='Q'
C
      if(lxout(0)) write(luntrm,*) 'at any prompt enter "Q" to exit'
      if(lxout(0)) write(luntrm,*) ' '
C
      if(lxout(0)) write(luntrm,*) 'enter "local" for local server'
      call ureadw('MDSMARK:  REMOTE MDS SERVER:$$',mds_server)
      if(mds_server.eq.'Q ') goto 1000
      if(mds_server.eq.'LOCAL ') mds_server='local'
C
      call ureadw('MDSMARK:  TREE NAME:$$',mds_tree)
      if(mds_tree.eq.'Q ') goto 1000
C
      call ureadi('MDSMARK:  SHOT NUMBER:$$',mds_shot)
      if(mds_shot.le.0) goto 1000
C
C------------------------------------------
C  INITIALIZE FOR LOOKING AT THIS RUN
C
C  remote server?
      if(mds_server.ne.'local') then
         ils=index(mds_server,' ')-1
         if(ils.le.0) ils=len(mds_server)
         write(luntrm,*) '...connecting to server:  ',
     1	mds_server(1:ils)
         stat=mds_setServer(mds_server(1:ils))
         if(stat.ne.1) then
            if((2*(stat/2)).eq.stat) then
      	 write(luntrm,*) '?mdsmark:  error setting server:  ',
     1	      mds_server(1:ils)
         call ufbad_exit("")
            else
      	 call mdsg('+mds_setServer status:  ',stat)
            endif
         endif
      endif
C
C  open tree
      ilt=index(mds_tree,' ')-1
      if(ilt.le.0) ilt=len(mds_tree)
C
      write(luntrm,*) '...mds_open:  ',mds_tree(1:ilt),' ',mds_shot
      stat=mds_open(mds_tree(1:ilt),mds_shot)
      if(stat.ne.1) then
         call mdsg('+mds_open status:  ',stat)
         if((2*(stat/2)).eq.stat) call ufbad_exit("")
      endif
C
C  write the node(s)
C
 5    continue
C
      call ureadw('MDSMARK:  NODE PATH:$$',mds_path)
      if(mds_path.eq.'Q ') goto 100
C
      call ureadw('MDSMARK:  STRING VALUE TO WRITE:$$',mds_string)
      if(mds_string.eq.'Q ') goto 100
 
C
      ipass=0
 10   continue
      ipass=ipass+1
      ilvn=ilnurd(mds_path)
      ilvd=ilnurd(mds_string)
      stat=MdsPut(mds_path(1:ilvn)//CHAR(0), '$'//CHAR(0),
     >    idescr_cstring( mds_string(1:ilvd)),0)
      if(stat.ne.1) then
         call mdsg('+MdsPut('//mds_path(1:ilvn)//',..):  ',stat)
           if((ipass.eq.1).and.(mds_msg(1:13).eq.'%TREE$-W-NNF,')) then
              write(luntrm,'('' ...creating node...'')')
              stat=mds_create_node(mds_path(1:ilvn),'text')  ! text node
              if (stat.ne.1) then
                 call mdsg(' node create status:  ',stat)
              endif
              if((2*(stat/2)).eq.stat) call ufbad_exit("")
              go to 10          ! retry
           endif
         if((2*(stat/2)).eq.stat) call ufbad_exit("")
      endif
      go to 5
C
 100  continue
C
      write(luntrm,*) '...calling mds_close(...)'
C 03/01/00 CAL: using tcl
      stat=mds_value('tcl("close")',idescr_long(istat),retl)
      if(stat.ne.1) then
         call mdsg('+mds_value(tcl("close"),...):  ',stat)
         call mdsg('+mds_close status:  ',stat)
         if((2*(stat/2)).eq.stat) call ufbad_exit("")
      endif
C      stat=mds_close(mds_tree(1:ilt),mds_shot)
C      if(stat.ne.1) then
C         call mdsg('+mds_close status:  ',stat)
C         if((2*(stat/2)).eq.stat) call ufbad_exit("")
C      endif
C
 1000 continue
      write(luntrm,*) '...mdsmark:  normal exit.'
      call uexita(0)
      stop
C
      END
 
C--------------------------------------------------------------
 
      subroutine mdsg(zstr,stat)
      character*(*) zstr	! context message string
      integer stat		! mdsplus status code
C
      character*128 mds_msg
      common/cmdsmsg/ mds_msg
C
      luntrm=lnout(0)
      write(luntrm,*) zstr,stat,':'
      itmp=mds_errstr(stat,mds_msg)
      ilmsg=ilnurd(mds_msg)
      write(luntrm,*) mds_msg(1:ilmsg)
      write(luntrm,*) ' ...(reported by MDSplus server)'
C
      return
      end
