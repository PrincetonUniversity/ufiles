      MODULE agcGet_mod
      INTERFACE 
        subroutine agcxget(xind,fxx,x,nxs)
        implicit none
        real, dimension(:), pointer  :: x
        real     xind, fxx
        integer  nxs
        end subroutine agcxget
      END INTERFACE

      INTERFACE 
 	subroutine agcyget(yind,fyy,y,nys)
	implicit none
	real, dimension(:), pointer     :: y
	real  yind, fyy
	integer  nys
        end subroutine agcyget
      END INTERFACE 

      END MODULE agcGet_mod
