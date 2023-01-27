module gftic

  !
  !  BLOCK OF SG TICMARK CONTROLS FOR 2D PLOTS
  !
  integer :: &
       KNEATX,KNEATY,KNEAT0,KNEATMIN,KNEATMAX, &
       KFRACX,KFRACY,KFRAC0,KFRACMIN,KFRACMAX, &
       KFRMCX,KFRMCY,KFRMC0,KFRMCMIN,KFRMCMAX, &
       KFRAMX,KFRAMY,KFRAM0,KFRAMMIN,KFRAMMAX, &
       KFRMMX,KFRMMY,KFRMM0,KFRMMMIN,KFRMMMAX
  !
  logical :: init_gftic = .false.
  !
  ! SEE GFLIB SUBROUTINES:  GFTIC0, GFTICI, GFTICS
  !
  integer NXR1,NXR2,NYR1,NYR2,NRSET
  integer KLINTP,KSYMTP,KSYLTP
  real SGSYSZ,SGSLSZ
  !
  integer, parameter :: IGFTMAXL=10
  integer, parameter :: IGFTMAXS=5
  integer, parameter :: IGFTSYMS=11

  real, dimension(IGFTSYMS) :: syszvc
  integer, dimension(IGFTMAXL) :: lnstyl
  integer, dimension(IGFTMAXS) :: lsystl

end module gftic
