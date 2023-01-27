      subroutine mds_getnum(lun,node,fname,type,ier)
C Extract a numerical node from MDSplus and write into file
C
      implicit none
C Input
      integer        lun
      character*(*)  node      ! mds node name
      character*(*)  fname     ! file name (to be written)
      integer        type      ! 1 = Integer
C return
      integer        ier       !  0 = success
C                              ! -1 = Node does not exist
C                              !  1 = any other error
C MDSplus
      integer       Mds_Value, idescr_long
      integer       descr, mds__long
      integer       nid, ndsc
C Local
      integer, dimension(2) :: idims
      integer :: l,  retl
      integer :: ivalue
C
      integer        stat,  ios
      character*100  expr
C
      integer        ilnurd
C----------------------------------------------------------------
! Check if node exists
      nid=0
      ndsc = descr(mds__long(), nid, 0)
      expr='getnci("'//node//'","NID_NUMBER")'
      l=ilnurd(expr)
      stat = Mds_Value(expr(1:l),ndsc,retl)
      if (mod(stat,2) .ne. 1) then
         if (stat .ne. 265388144) then     !  Node Not Found
            call mdsmsg(lun, 'MdsValue('//expr(1:l)//')', stat)
         endif
         ier = -1
         return
      endif
!

! Get Value
      if (type .eq. 1) then
         stat = Mds_Value(node,idescr_long(ivalue),retl)
      endif
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//node//')',
     &        stat)
         ier=1
         go to 9000
      else
cxx   print*,'write ',fname
! Write file
         open(7,STATUS='replace',iostat=ier,
     >        file=fname)
         if(ier.eq.0) then
            if (type .eq. 1) then
               if (ivalue .gt. 9999999) then
                  write(7,'(i11)',iostat=ier) ivalue
               else
                  write(7,'(i7)',iostat=ier) ivalue
               endif
            endif
            if (ier .ne. 0) then
               close(7,status='delete')
            else
               close(7)
            endif
         endif
         if(ier.ne.0) then
            print*,'%mds_getnum: Error writing '//fname
            go to 9000
         endif
      endif
      return
C
 9000 continue
      return
      end
