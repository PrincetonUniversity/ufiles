   subroutine add_tmplt(lun, runid, opt, stat)
      implicit none
! Input
      integer         lun           ! for error messages
      character*(*)   runid         ! runid
      integer         opt           ! 
! Return
      integer         stat

!
 integer, parameter            ::  n_rf=3

 character*3,  dimension(n_rf) ::  rf_type
 character*30                  ::  x_name
 character*200                 ::  t_name
 character*17                  ::  node
 character*8                   ::  code_type(2,2)

 logical                       :: ex
 integer                       :: mdsstat
 integer                       :: nid,     i,k

! Functions
 integer                       :: mds_add_node


 rf_type(1) = "ECH"
 rf_type(2) = "LHH"
 rf_type(3) = "ICH"

 code_type(1,1) = 'RFTMP'    ; code_type(2,1) = 'GENRAY'
 code_type(1,2) = 'CQL3DTMP' ; code_type(2,2) = 'CQL3D'

 do k = 1, size(code_type,2)
    do i =1,n_rf
       x_name=trim(runid)//'_'//trim(rf_type(i))//'.'//trim(adjustl(code_type(1,k)))
       !print *, 'looking for '//trim(x_name)
       inquire(file=x_name,exist=ex,iostat=stat)
       if (ex) then
          print *,'found '//trim(x_name)
          open(29,file=x_name,status='OLD',iostat=stat, &
               action="read")
          if (stat .eq. 0) then
             read(29,'(A)',iostat=stat) t_name
             close(29)
             if (stat .eq. 0) then
                node='.NML'
                nid = mds_add_node(trim(node),'structure',mdsstat)
                if (nid .le. 0) then
                   call mdsmsg(lun, &
                        '?add_tmplt: error creating '//trim(node)//'  - stat:',mdsstat)
                   go to 900
                endif
                node='.NML:'//trim(adjustl(code_type(2,k)))//'_'//trim(rf_type(i))
                nid = mds_add_node(trim(node),'text',mdsstat)
                print*,'nid =',nid
                if (nid .le. 0) then
                   call mdsmsg(lun, &
                        '?add_tmplt: error creating '//trim(node)//'  - stat:',mdsstat)
                else
                   print*,' call add_file ',node,' ',trim(t_name)
                   call add_file(lun,runid,node,trim(t_name),' ',' ',' ',stat)
                   if (stat .ne. 0) then
                      write(lun,'(" ?add_tmplt -- error adding ",a)') node
                   endif
                endif
             else
                write(lun,'(" ?add_tmplt -- error opening ",a,i6)') x_name,stat
             endif
          else
             write(lun,'(" ?add_tmplt -- error reading ",a,i6)') x_name,stat
          endif
       endif
900    continue
    enddo
 end do
 return
 end

