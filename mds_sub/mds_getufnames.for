      subroutine mds_getufnames(lun,fname,ier)
C Extract ufile names from MDSplus and write into file
C
      implicit none
C Input
      integer        lun
      character*(*)  fname     ! file name (to be written)
C return
      integer        ier       !  0 = success
C                              ! -1 = Node does not exist
C                              !  1 = any other error
C MDSplus
      integer       Mds_Value, idescr_long, idescr_cstringarr
      integer       ndsc
C Local
      integer, dimension(2) :: idims
      integer :: l,  ilz, retl
      integer :: nfiles  ! #of Ufiles
      character(200), dimension(:), allocatable :: ztext
      integer :: ialloc, iline
C
      integer        stat,  ios
      character*100  expr
C
      integer        ilnurd
C----------------------------------------------------------------
!
! Get number of Ufiles
      nfiles=0
      expr='size(getnci(".INPUTS:*:FILENAME","NID_NUMBER"))'
      l=ilnurd(expr)
      stat = Mds_Value(expr(1:l),idescr_long(nfiles),retl)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun, 'Mds_Value('//expr//')', stat)
         ier=1
         goto 9000
      else if(nfiles.le.0) then
         write(lun,*)
     &     ' ?mds_getufnames: Mds_Value returned non-positive length: ',
     &     nfiles
         ier=-1
         goto 9000
      else
! Allocate array
         print *,'nfiles = ',nfiles
         if(allocated(ztext)) deallocate(ztext)
         allocate(ztext(nfiles), STAT=ialloc)
         if(ialloc  /=  0) then
            write(lun,*)
     >      '?mds_getufnames: ERROR allocating ztext'
         endif
!
         idims(1)=nfiles
         idims(2)=0
! Read array
         expr='getnci(".INPUTS:*:FILENAME","RECORD")'
         l=ilnurd(expr)
         stat = Mds_Value(expr(1:l),
     &        idescr_cstringarr(ZTEXT,idims,1),retl)
         if (mod(stat,2).ne.1) then
            call mdsmsg(lun, 'Mds_Value('//expr(1:l)//')',
     &           stat)
            ier=1
            go to 9000
         else
cxx            print*,'write ',fname
! Write file
            open(7,STATUS='replace',iostat=ier,
     >           file=fname)
            if(ier.eq.0) then
               do iline=1,nfiles
                  ilz = len_trim(ztext(iline))
C                 print *,ztext(iline)(1:ilz)
                  write(7,'(A)',iostat=ier) ztext(iline)(1:ilz)
               enddo
               if (ier .ne. 0) then
                  close(7,status='delete')
               else
                  close(7)
               endif
            else
               print*,'%mds_getufnames: Error writing '//fname
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
