subroutine urd_setprog(zprognam)

  ! set UREAD program name

  use urd_progmod
  implicit NONE

  character*(*), intent(in) :: zprognam

  !--------------------------

  progname = zprognam
  call ulower(progname)  ! enforce lower case letters

end subroutine urd_setprog
