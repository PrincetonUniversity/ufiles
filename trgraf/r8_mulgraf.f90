subroutine r8_mulgraf(file_label,nx,nf,x,f,units_x,units_f,f_label,nf_label)

  ! gen multiplot using GRFMM1
    
  implicit NONE

  character*(*), intent(in) :: file_label  ! data source label
  integer, intent(in) :: nx   ! no. of points in x axis and in each profile
  integer, intent(in) :: nf   ! no. of profiles
  real*8, intent(in) :: x(nx) ! x axis grid data
  real*8, intent(in) :: f(nx,nf)  ! the profile data
  character*(*), intent(in) :: units_x     ! physical units label for x axis
  character*(*), intent(in) :: units_f     ! physical units label for f data
  character*(*), intent(in) :: f_label     ! generalic label for f data & plot
  character*(*), intent(in) :: nf_label(nf) ! specific label for each profile

  !--------------------
  ! local (automatic)

  integer :: nxx(nf)
  real*8 :: xx(nx,nf)
  real*8 :: zz(nf)
  integer :: iself(nf)

  character*60 mnlab(3)
  character*17 kylab(5)

  integer :: ix,if
  !--------------------

  nxx=nx
  do if=1,nf
     nxx(if)=nx
     xx(1:nx,if)=x
     zz(if)=if
     iself(if)=if
  enddo
  iself(1)=-iself(1)  ! no "square scale"

  mnlab=' '
  kylab=' '

  kylab(1)=' profile labels '

  mnlab(1)=file_label
  mnlab(3)=f_label

  call r8_grfmm1(xx,f,nx,nxx,nf,zz,nf,iself,nf, &
       units_x,units_f,mnlab,kylab,nf_label)

end subroutine r8_mulgraf
