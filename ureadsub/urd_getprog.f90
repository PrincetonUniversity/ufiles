subroutine urd_getprog(zprognam)

  ! get UREAD program name

  use urd_progmod
  implicit NONE

  character*(*), intent(out) :: zprognam

  !--------------------------

  if(progname.eq.' ') then
     ! try to get as command line arg[0] -- works in unix environments
     call get_arg(0,progname)
     call ulower(progname)
  endif

  zprognam = progname

end subroutine urd_getprog
