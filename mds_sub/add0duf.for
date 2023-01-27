      subroutine add0duf(lun,signam,nscd,nsca,scval,sclab,ier)
 
C Add Scalars to MDSplus
C 02/09/01 CAL
C
      implicit none
C
      integer       lun          ! lun for error messages
      character*(*) signam
      integer       nscd,nsca    ! size of array / actual
      real          scval(nscd)  ! Scalar values
      character*30  sclab(nscd)  ! scalar labels
      integer       ier
 
C For MDSplus
      integer       mds_create_node
      integer       MdsPut,  idescr_floatarr
      integer       idescr_cstring, idescr_cstringarr
      character*90  expr
      integer       stat
 
C-------------------------------------------------------------------
      ier = 0
 
C Create SCVAL
      stat=mds_create_node(signam//':SCVAL',
     >     'numeric')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':SCVAL create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
C Write SCVAL
      if (nsca .gt.0) then
         expr=signam//':SCVAL'//CHAR(0)
         stat=MdsPut(expr,'$'//char(0),
     >        idescr_floatarr(scval,nsca,1),0)
         if (mod(stat,2).ne.1) then
            call mdsmsg(lun,expr//' put status:  ',stat)
            ier=1
         endif
      endif
 
C Create SCLAB
      stat=mds_create_node(signam//':SCLAB','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':SCLAB create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
C Write SCLAB
      if (nsca .gt.0) then
         expr=signam//':SCLAB'//CHAR(0)
         stat=MdsPut(expr,'$'//char(0),
     >        idescr_cstringarr(sclab,nsca,1),0)
         if (mod(stat,2).ne.1) then
            call mdsmsg(lun,expr//' put status:  ',stat)
            ier=1
            go to 9000
         endif
      endif
 
      return
C
 9000 continue
      return
      end
