      MODULE sgnco_mod
      implicit none
      integer   :: nlm
      parameter (nlm=40)
      real, dimension(:), allocatable :: am, pl1, pl2
      real, dimension(:), pointer     :: lax,lay
      integer :: jufxy, uufxy

      logical :: iss, linx, liny
      real    :: xorg, xrang, xscal, yorg, yrang, yscal, cv
      integer :: ix, iy, is, inir, nir, kir, mir
      integer :: lam, igr, iar, ilft, irgt, npl, mpl, kfill
      integer :: nxs, nys, npu, nsym
      integer, dimension(nlm) :: kmap, ksym, kdsh
      END MODULE sgnco_mod
