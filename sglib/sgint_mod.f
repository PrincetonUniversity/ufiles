      MODULE sgInt_mod

! 03/09/99 C.Ludescher
!
!      INTERFACE 
!	subroutine agcuxyi(xarray,yarray,maxa,nx,ny,xraw,yraw,xdat,ydat)
!	implicit none
!	integer maxa,nx,ny 
!	real, dimension(:,:), pointer ::  xarray, yarray
!	real xraw,yraw,xdat,ydat
!        end subroutine agcuxyi
!      END INTERFACE

      INTERFACE
	subroutine agclins(zdata,mxd,x,nx,y,ny,cl,ncl)
        implicit none
	integer :: mxd, nx, ny, ncl
	real, dimension(:), target :: x
	real, dimension(:), target :: y
	real, dimension(:,:) :: zdata
	real cl(:)
        end subroutine agclins
      END INTERFACE 

      END MODULE sgInt_mod
