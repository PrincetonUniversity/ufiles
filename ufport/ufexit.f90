! normal exit from a ufile program
subroutine ufgood_exit
  call ufdatetime
  call flush(6)
  stop  ! normal exit, status = 0
end subroutine ufgood_exit

! abnormal exit from a ufile program
subroutine ufbad_exit(msg)
  character(len=*), optional, intent(in) :: msg
  if (present(msg)) then
     write(6,*) ' '
     write(6,*) msg
     write(6,*) ' '
  end if
  call ufdatetime
  call flush(6)
  stop 1  ! abnormal exit, status = 1
end subroutine ufbad_exit

! print out date and time
subroutine ufdatetime
  integer, dimension(8) :: ivals
  character(len=8) :: zzdate
  character(len=10) :: ztime
  character(len=5) :: zone
  call date_and_time(zzdate,ztime,zone,ivals)
  if(ivals(2).gt.12) write(6,1001) ivals(2)
1001 format(' ufdatetime:  ?? month .gt. 12:  ',i10)
end subroutine ufdatetime
