      subroutine add_pretr(lun, mds_shot, runid, opt, stat)
C
C Add extra nodes to TRANSP Tree
C 12/11/02: C.Ludescher
C
      implicit none
C Input
      integer         lun           ! for error messages
      integer         mds_shot      ! Pulse number
      character*(*)   runid         ! runid
      integer         opt           ! 1 = TF_PLN
C Return
      integer         stat
C Functions
      integer ilnurd   ! ufencod function
      integer         mds_add_node,  MdsValue,   idescr_long
C Local
      integer         nid,           size,        l
      integer         dsc,           mdsstat,     ier,   ios
      character*100   expr
      character*10    cshot
      character*64    zdate
      integer         MdsPut,       idescr_cstring, idescr_float
      real            time
C-------------------------------------------------------------------------
C
C add TF.PLN to node :CONTENTS
      if (opt .eq. 1) then
C Create node, if it does not exist
         nid = mds_add_node('CONTENTS','text',mdsstat)
         print*,'nid =',nid
         if (nid .le. 0) then
            call mdsmsg(lun,
     >      '?add_pretr: creating CONTENTS - stat:',mdsstat)
            go to 9100
         else
            call add_file(lun,runid,'CONTENTS',
     >           ' ',' ',0,' ',stat)
            stat=1
         endif
C add dates to :DATE_RUN and :DATE_EXEFILE
      else if (opt .eq. 2) then
C Create node, if it does not exist
         nid = mds_add_node('DATE_RUN','text',mdsstat)
         if (nid .le. 0) then
            call mdsmsg(lun,
     >      '?add_pretr: creating DATE_RUN - stat:',mdsstat)
            go to 9100
         else
            open (12,file=runid//'.DATE_RUN',iostat=ios,
     >            status='old')
            if (ios .ne. 0) then
               print*,'?error opening '//runid//'.DATE_RUN' 
               zdate = 'unknown'
            else
               read (12,'(a)')zdate
               close(12)
            endif
            stat=MdsPut('DATE_RUN'//CHAR(0),'$'//CHAR(0),
     >           idescr_cstring(zdate),0,size)
            if (mod(stat,2).ne.1) then
               call mdsmsg(lun,
     >              '?add_pretr: put DATE_RUN - stat:  ',stat)
               goto 9100
            end if
            open (12,file=runid//'.DATE_EXEFILE',iostat=ios,
     >            status='old')
            if (ios .ne. 0) then
               print*,'?error opening '//runid//'.DATE_EXEFILE' 
               zdate='unknown'
            else
               read (12,'(a)')zdate
               close(12)
            endif
            stat=MdsPut('DATE_EXEFILE'//CHAR(0),'$'//CHAR(0),
     >           idescr_cstring(zdate),0,size)
            if (mod(stat,2).ne.1) then
               call mdsmsg(lun,
     >              '?add_pretr: put DATE_EXEFILE - stat:  ',stat)
               goto 9100
            end if
            stat=1
         endif
      else if (opt .eq. 4 .or. opt .eq. 24 ) then
C Create node, if it does not exist
         nid = mds_add_node('MRK_TIME','numeric',mdsstat)
         if (nid .le. 0) then
            call mdsmsg(lun,
     >      '?add_pretr: creating MRK_TIME - stat:',mdsstat)
            go to 9100
         else
            open (12,file=runid//'.MRK_TIME',iostat=ios,
     >            status='old')
            if (ios .ne. 0) then
               print*,'?error opening '//runid//'.MRK_TIME' 
               goto 9100
            else
               read (12,'(G20.0)')time
               close(12)
            endif
            stat=MdsPut('MRK_TIME'//CHAR(0),'$'//CHAR(0),
     >           idescr_float(time),0,size)
            if (mod(stat,2).ne.1) then
               call mdsmsg(lun,
     >              '?add_pretr: put MRK_TIME - stat:  ',stat)
               goto 9100
            end if
         endif
         nid = mds_add_node('SRC_TIME','numeric',mdsstat)
         if (nid .le. 0) then
            call mdsmsg(lun,
     >      '?add_pretr: creating SRC_TIME - stat:',mdsstat)
            go to 9100
         else
            open (12,file=runid//'.SRC_TIME',iostat=ios,
     >            status='old')
            if (ios .ne. 0) then
               print*,'?error opening '//runid//'.SRC_TIME' 
               goto 9100
            else
               read (12,'(G20.0)')time
               close(12)
            endif
            stat=MdsPut('SRC_TIME'//CHAR(0),'$'//CHAR(0),
     >           idescr_float(time),0,size)
            if (mod(stat,2).ne.1) then
               call mdsmsg(lun,
     >              '?add_pretr: put SRC_TIME - stat:  ',stat)
               goto 9100
            end if
         endif
         if ( opt .eq. 24) then
C Add nubeam Namelist
C Create node, if it does not exist
            nid = mds_add_node('NB_NAMELIST','text',mdsstat)
            print*,'nid =',nid
            if (nid .le. 0) then
               call mdsmsg(lun,
     >              '?add_pretr: creating NB_NAMELIST - stat:',mdsstat)
               go to 9100
            else
               call add_file(lun,runid,'NB_NAMELIST',
     >              ' ',' ',0,' ',stat)
               stat=1
            endif
         endif
      endif
 
C Write/Close Tree
 
      expr = 'tcl("WRITE")' // CHAR(0)
      l=ilnurd(expr)
      dsc = idescr_long(mdsstat)
      print*,expr(1:l)
      stat = MdsValue(expr, dsc, 0, size)
      print*,'stat =',stat
      if(mod(mdsstat,2).ne.1) go to 9000
      expr = 'tcl("CLOSE")' // CHAR(0)
      l=ilnurd(expr)
      dsc = idescr_long(mdsstat)
      stat = MdsValue(expr, dsc, 0, size)
      if(mod(mdsstat,2).ne.1) go to 9000
      return
C
 9000 call mdsmsg(lun,'?add_pretr: '//expr(1:l)//
     >   ' mdsstat:  ',mdsstat)
 9100 stat = 0
      return
      end
