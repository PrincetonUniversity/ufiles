MODULE mds_readnode
  implicit none

  INTERFACE mds_read_node
     MODULE PROCEDURE mds_read_int, &
                      mds_read_float, &
                      mds_read_text
  END INTERFACE

  integer, PARAMETER :: r4 = SELECTED_REAL_KIND(6,37)
  integer, PARAMETER :: r8 = SELECTED_REAL_KIND(12,100)

CONTAINS
!-----------------------------------------------------------------

  SUBROUTINE mds_read_int(lun,node,ivalue,ier)
     implicit none
     integer,       intent(in)  :: lun       ! lun for error message
     character*(*), intent(in)  :: node      ! mds node name
     integer,       intent(in)  :: ivalue    !  0 = success
     integer,       intent(out) :: ier       !  0 = success
                                             ! -1 = Node does not exist
                                             !  1 = any other error
! MDSplus
      integer ::      Mds_Value, idescr_long
      integer ::      descr, mds__long
      integer ::      nid, ndsc
! Local
      integer ::      retl
      integer ::      stat
      character*100 :: expr


! Check if node exists
      nid=0
      ndsc = descr(mds__long(), nid, 0)
      expr='getnci("'//node//'","NID_NUMBER")'
      stat = Mds_Value(trim(expr),ndsc,retl)
      if (mod(stat,2) .ne. 1) then
         if (stat .ne. 265388144) then     !  Node Not Found
            call mdsmsg(lun, 'MdsValue('//trim(expr)//')', stat)
         endif
         ier = -1
         return
      endif

! Get Value
      stat = Mds_Value(node,idescr_long(ivalue),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//node//')',stat)
         ier=1
         return
      endif
  END SUBROUTINE 

  SUBROUTINE mds_read_float(lun,node,fvalue,ier)
     implicit none
     integer,       intent(in)  :: lun       ! lun for error message
     character*(*), intent(in)  :: node      ! mds node name
     REAL(KIND=r4), intent(in)  :: fvalue    !  
     integer,       intent(out) :: ier       !  0 = success
                                             ! -1 = Node does not exist
                                             !  1 = any other error
! MDSplus
      integer ::      Mds_Value, idescr_float
      integer ::      descr, mds__long
      integer ::      nid, ndsc
! Local
      integer ::      retl
      integer ::      stat
      character*100 :: expr


! Check if node exists
      nid=0
      ndsc = descr(mds__long(), nid, 0)
      expr='getnci("'//node//'","NID_NUMBER")'
      stat = Mds_Value(trim(expr),ndsc,retl)
      if (mod(stat,2) .ne. 1) then
         if (stat .ne. 265388144) then     !  Node Not Found
            call mdsmsg(lun, 'MdsValue('//trim(expr)//')', stat)
         endif
         ier = -1
         return
      endif

! Get Value
      stat = Mds_Value(node,idescr_float(fvalue),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//node//')',stat)
         ier=1
         return
      endif
  END SUBROUTINE
  SUBROUTINE mds_read_text(lun,node,string,ier)
     implicit none
     integer,       intent(in)  :: lun       ! lun for error message
     character*(*), intent(in)  :: node      ! mds node name
     character*(*), intent(out) :: string    ! text node
     integer,       intent(out) :: ier       !  0 = success
                                             ! -1 = Node does not exist
                                             !  1 = any other error
! MDSplus
      integer ::      Mds_Value, idescr_cstring
      integer ::      descr, mds__long
      integer ::      nid, ndsc
! Local
      integer ::      retl
      integer ::      stat
      character*100 :: expr


! Check if node exists
      nid=0
      ndsc = descr(mds__long(), nid, 0)
      expr='getnci("'//node//'","NID_NUMBER")'
      stat = Mds_Value(trim(expr),ndsc,retl)
      if (mod(stat,2) .ne. 1) then
         if (stat .ne. 265388144) then     !  Node Not Found
            call mdsmsg(lun, 'MdsValue('//trim(expr)//')', stat)
         endif
         ier = -1
         return
      endif

! Get Value
      stat = Mds_Value(node,idescr_cstring(string),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//node//')',stat)
         ier=1
         return
      endif
  END SUBROUTINE 
 
END MODULE mds_readnode
