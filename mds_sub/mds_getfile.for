      subroutine mds_getfile(lun,node,fname,ier)
C Extract a text node from MDSplus and write into file
C
      implicit none
C Input
      integer        lun
      character*(*)  node      ! mds node name
      character*(*)  fname     ! file name (to be written)
C return
      integer        ier       !  0 = success
C                              ! -1 = Node does not exist
C                              !  1 = any other error
C MDSplus
      integer       Mds_Value, idescr_long, idescr_cstringarr
      integer       descr, mds__long
      integer       nid, ndsc
C Local
      integer, dimension(2) :: idims
      integer :: l,  ilz, retl
      integer :: nzlines  ! #of lines in text node
      character(120), dimension(:), allocatable :: ztext
      integer :: ialloc, iline
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
! Get size of text array
      expr = 'sizeof(' // node // ')' // char(0)
      l=ilnurd(expr)
      stat = Mds_Value(expr(1:l),idescr_long(nzlines),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//expr//')', stat)
         ier=1
         goto 9000
      else if(nzlines.le.0) then
         write(lun,*)
     &        ' ?mds_getfile: Mds_Value returned non-positive sizeof: ',
     &        nzlines
         ier=1
         goto 9000
      endif
      expr = 'size(' // node // ')' // char(0)
      l=ilnurd(expr)
      stat = Mds_Value(expr(1:l),idescr_long(nzlines),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//expr//')', stat)
         ier=1
         goto 9000
      else if(nzlines.le.0) then
         write(lun,*)
     &        ' ?mds_getfile: Mds_Value returned non-positive length: ',
     &        nzlines
         ier=1
         goto 9000
      else
! Allocate array
cxx      print *,'nzlines = ',nzlines
         if(allocated(ztext)) deallocate(ztext)
         allocate(ztext(nzlines), STAT=ialloc)
         if(ialloc  /=  0) then
            write(lun,*)
     >      '?mds_getfile: ERROR allocating ztext'
         endif
!
         idims(1)=nzlines
         idims(2)=0
! Read array
         stat = Mds_Value(node,
     &        idescr_cstringarr(ZTEXT,idims,1),retl)
         if (mod(stat,2).ne.1) then
            call mdsmsg(lun, 'Mds_Value('//node//')',
     &           stat)
            ier=1
            go to 9000
         else
cxx            print*,'write ',fname
! Write file
            open(7,STATUS='replace',iostat=ier,
     >           file=fname)
            if(ier.eq.0) then
               do iline=1,nzlines
                  ilz = len_trim(ztext(iline))
                  write(7,'(A)',iostat=ier) ztext(iline)(1:ilz)
               enddo
               if (ier .ne. 0) then
                  close(7,status='delete')
               else
                  close(7)
               endif
            else
               print*,'%mds_getfile: Error writing '//fname
               go to 9000
            endif
            CLOSE(7)	
         endif
      endif
      ier = 0
      return
C
 9000 continue
      return
      end
