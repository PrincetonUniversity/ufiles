subroutine ufheader(progname)
  implicit none
  character(len=*), intent(in) :: progname
  write(6,'(a)') ""
  write(6,'(a,72("*"))') " "
  write(6,'(2a)') " Program: ", trim(progname)
  write(6,'(2a)') " Version: ", VERSION
  write(6,'(2a)') " Revision: ", GIT_HASH
  write(6,'(2a)') " Date: ", BUILD_DATE
  write(6,'(a,72("*"))') " "
end subroutine ufheader
