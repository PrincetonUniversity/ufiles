      subroutine uf_fin(lun,signam,device,date,iproc,ier)
 
C Add date/device and write
C 10/19/01 CAL
C
      implicit none
      integer ilnurd   ! ufencod function
C
      integer       lun          ! lun for error messages
      character*(*) signam
      character*(*) device, date
      integer       iproc
      integer       ier
 
C For MDSplus
      integer       mds_create_node
      integer       MdsPut,  MdsValue, idescr_long
      integer       dsc, idescr_cstring
      character*90  expr
      integer       mdsstat, stat, size, l
 
C-------------------------------------------------------------------
      ier = 0
 
C Create DEVICE
      stat=mds_create_node(signam//':DEVICE',
     >     'text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,'?uf_fin: '//signam//
     >           ':DEVICE create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
 
C Add DEVICE
      stat=MdsPut(signam//':DEVICE'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(device),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?uf_fin: put '//signam//':DEVICE - stat:  ',stat)
      end if
C Create PROC_CODE
      stat=mds_create_node(signam//':PROC_CODE',
     >     'numeric')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,'?uf_fin: '//signam//
     >           ':PROC_CODE create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
C Add PROC_CODE
      stat=MdsPut(signam//':PROC_CODE'//CHAR(0),'$'//CHAR(0),
     >     idescr_long(iproc),0,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?uf_fin: put PROC_CODE - stat:  ',stat)
      end if
 
C
C Create DATE
      stat=mds_create_node(signam//':DATE','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then ! TreeALREADY_THERE
            call mdsmsg(lun,'?uf_fin: '//signam//
     >           ':DATE create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
C Write DATE
      stat=MdsPut(signam//':DATE'//CHAR(0),'$'//CHAR(0),
     >     idescr_cstring(date),0 ,size)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,
     >        '?uf_fin: put '//signam//':DATE - stat:  ',stat)
      end if
 
C
C Write/close tree
C
 9000 continue
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
      return
 
 9001 ier = 1
      call mdsmsg(lun,
     >     '?uf_fin: '//signam//' write/close - stat:  ',stat)
      return
      end
