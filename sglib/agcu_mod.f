      MODULE agcu_mod
      INTERFACE 
	subroutine agcuset(xarray,yarray,maxa,nx,ny) 
        implicit none
	integer :: maxa,nx,ny
	real, dimension(maxa,*), target :: xarray
	real, dimension(maxa,*), target :: yarray
        end subroutine agcuset

        subroutine agcuxy(xraw,yraw,xdat,ydat)
        implicit none
	real xraw,yraw,xdat,ydat
        end subroutine agcuxy
      END INTERFACE
      END MODULE agcu_mod
