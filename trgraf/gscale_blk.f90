module gscale_blk
  !-----------------------------------------------
  !  CONTROL BLOCK FOR THE NEXT PLOT - CF. GSCSET SUBROUTINE
  !
  real :: xmin00,xmax00
  real :: ymin00,ymax00
  real :: xrat00,yrat00
  integer :: noptx,nopty,igsini
  !
  ! RECORD OF SCALES OF RECENTLY DRAWN PLOTS
  !
  integer, PARAMETER :: MXSPRV=10 ! NUMBER OF RECENT SCALES TO REMEMBER
  !
  integer :: nscprv,ncursc
  real :: XMINP(MXSPRV),YMINP(MXSPRV),XMAXP(MXSPRV),YMAXP(MXSPRV)
  !
  ! BLOCK FOR X,Y MIN/MAX OF DATA
  !
  real :: gminx,gminy,gmaxx,gmaxy,gminxp,gminyp
  real :: gminx0,gminy0,gmaxx0,gmaxy0,gmnxp0,gmnyp0

end module gscale_blk
