
!
! fortran version of MdsCacheConnect(char*)
!
function MdsCacheConnect(server) result(istat)
  use iso_c_binding, only: c_char
  character*(*), intent(in) :: server   ! server for cached connection
  
  integer      :: istat                        ! socket
  character(kind=c_char) :: cserver(len_trim(server)+1)  ! C string

  integer, external :: c_MdsCacheConnect

  call cstring(trim(server), cserver, '2C')

  istat = c_MdsCacheConnect(cserver)
end function MdsCacheConnect

