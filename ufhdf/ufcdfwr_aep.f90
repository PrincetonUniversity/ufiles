!******************** START FILE UFWRCDF_AEP.F90 ; GROUP UFILES ******************
!--------------------------------------------------------------
!  UFCDFWR_AEP
!  WRITE Multi-DIMENSIONAL DATA TO NetCdf FILE for AEP model (Mario Pdesta)
!
SUBROUTINE UFCDFWR_AEP(filename,ZSDATE,ISHOT,CDEV,iaep_sp,&
  & NK,AAEP,TM,NT,Tstep,demin,demax,nde,dpmin,dpmax,ndp,emin,emax,ne,&
  & pmin,pmax,np,mumin,mumax,nmu,PAEP,ndemax,ndpmax,nemax,npmax,nmumax,ctunits,ceunits,IER)
! 
!  MG  03/13/2014 
!  ISHOT -- ISHOT # ("139048")
!  IDEV-- 4 LETTER TOKAMAK DEV. CODE ("PLT ", ETC.)
!  nk -- terms # in Mario Pdesta series, # of modes
!
!  iaep_sp(NK) -- species code # to which AEP mode will be applied
!  species code #
!  0                   ;ALL fast ion SPECIES in this run (in NUBEAM)
!  1                   ;deuterium
!  2                   ;tritium
!  3                   ;HE3
!  4                   ;HE4
!  5                   ;protons          
!
!  AAEP(NT,NK) -- Mode amplitude. Aaep_k_specie(time)
!  nt -- # of points in time grig TM
!  TM(nt) -- time grid in ms
!  Tstep(nk) -- time step used to investigate AEP mode
!  demin(NK), demax(NK) -- left and right boundaries for a linear grid, function of nk (# of terms in MP series)
!  dpmin(NK), dpmax(NK) -- same as above
!  emin(NK), emax(NK) -- same as above
!  pmin(NK), pmax(NK) -- same as above
!  mumin(NK), mumax(NK) -- same as above
!  PAEP(ndemax,ndpmax,nemax,npmax,nmumax,nk)  -- probability matrix
!  ndemax -- max(1,maxval(nde))
!  ndpmax -- max(1,maxval(ndp))
!  nemax -- max(1,maxval(ne))
!  npmax -- max(1,maxval(np))
!  nmumax -- max(1,maxval(nmu))
!  PROCESS CODE 0 = "RAW" raw data are saved
!  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
!
 
  use iso_c_binding, only: fp => c_double
  use netcdf
  implicit none
  character*(*),intent(in) :: filename !with path
  character*(*),intent(in):: CDEV
  integer,intent(in) :: ishot
  integer,intent(in), dimension(nk) :: iaep_sp
  integer,intent(in) :: nk,nt
  real(fp),intent(in)  :: AAEP(nt,nk)
  real(fp),intent(in)  :: tm(nt)
  integer,intent(in) :: nde(nk), ndp(nk), ne(nk), np(nk), nmu(nk)
  real(fp),intent(in)  :: tstep(nk)
  real(fp),intent(in)  :: demin(nk),demax(nk)
  real(fp),intent(in)  :: dpmin(nk),dpmax(nk)
  real(fp),intent(in)  :: emin(nk),emax(nk)
  real(fp),intent(in)  :: pmin(nk),pmax(nk)
  real(fp),intent(in)  :: mumin(nk),mumax(nk)
  integer,intent(in) :: ndemax,ndpmax,nemax,npmax,nmumax !PAEP dimentions
  real(fp),intent(in)  :: paep(ndemax,ndpmax,nemax,npmax,nmumax,nk)
  CHARACTER*9,intent(in) :: ctunits,ceunits ! units for time axis amd for energy axis
  CHARACTER*10,intent(in) :: ZSDATE !  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
  CHARACTER*10 :: ZDATE  !  ZDATE - FILE WRITE DATE
  integer :: ier
  character*30 :: cvar
  character*10 :: uflabel
  integer :: ilbl,ilbl1 !label length
!netcdf var's IDs bellow
  integer :: icdf, id_lbl, id_lbl1, id_uf, id_dev, id_ishot, id_zsdate, id_zdate        
  integer :: id_iaep_sp, id_nk, id_nt, id_aaep, id_tm,id_tstep
  integer :: id_demin, id_demax, id_dpmin, id_dpmax, id_emin, id_emax, id_pmin, id_pmax
  integer :: id_mumin, id_mumax,id_nde, id_ndp, id_ne, id_np, id_nmu, id_paep
  integer :: id_ndemax,id_ndpmax,id_nemax,id_npmax,id_nmumax
  character :: zbuf*24
!-----------------------------------------
!
!  WRITE ***
!
! sanitary check
  ier=0
  call checkdim
!OK now write netcdf file, raw data
!  IPROC = 0 
!  KMPRS = 4 !CDF file 
  call fdate(zbuf)
  if(zbuf(9:9).eq.' ')zbuf(9:9)=char(48)
  zdate=zbuf(9:10)//zbuf(5:7)//zbuf(21:24)
  call uupper(ctunits)
  call uupper(ceunits)

  call check(nf90_create(path=trim(adjustl(filename)),&
       & cmode=NF90_CLOBBER,ncid=icdf),trim(adjustl(filename)))
  
  cvar='DEVLBL'
  ilbl=4
  call check(nf90_def_dim(icdf,cvar,ilbl,id_lbl),cvar)
  cvar='SHOT'
  call check(nf90_def_var(icdf,cvar,NF90_INT,id_ishot),cvar)
  cvar='DEVICE'
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl/),id_dev),cvar)
  cvar='UFLBL'
  ilbl1=10
  call check(nf90_def_dim(icdf,cvar,ilbl1,id_lbl1),cvar)
  cvar = 'UFLABEL'  !UF label
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_uf),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zsdate),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zdate),cvar)
  cvar='NK'
  call check(nf90_def_dim(icdf,cvar,nk,id_nk),cvar)
  cvar='NT'
  call check(nf90_def_dim(icdf,cvar,nt,id_nt),cvar)
  cvar='NDEMAX'
  call check(nf90_def_dim(icdf,cvar,ndemax,id_ndemax),cvar)
  cvar='NDPMAX'
  call check(nf90_def_dim(icdf,cvar,ndpmax,id_ndpmax),cvar)
  cvar='NEMAX'
  call check(nf90_def_dim(icdf,cvar,nemax,id_nemax),cvar)
  cvar='NPMAX'
  call check(nf90_def_dim(icdf,cvar,npmax,id_npmax),cvar)
  cvar='NMUMAX'
  call check(nf90_def_dim(icdf,cvar,nmumax,id_nmumax),cvar)
  cvar = 'IAEP_SP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_iaep_sp),cvar)
  cvar='TM'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nt/),id_tm),cvar)
  cvar='AAEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nt,id_nk/),id_aaep),cvar)
  cvar='TSTEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_tstep),cvar)
  cvar='DEMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_demin),cvar)
  cvar='DEMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_demax),cvar)
  cvar='DPMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_dpmin),cvar)
  cvar='DPMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_dpmax),cvar)
  cvar='EMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_emin),cvar)
  cvar='EMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_emax),cvar)
  cvar='PMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_pmin),cvar)
  cvar='PMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_pmax),cvar)
  cvar='MUMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_mumin),cvar)
  cvar='MUMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_mumax),cvar)
  cvar = 'NDE'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_nde),cvar)
  cvar = 'NDP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_ndp),cvar)
  cvar = 'NE'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_ne),cvar)
  cvar = 'NP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_np),cvar)
  cvar = 'NMU'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_nmu),cvar)
  cvar='PAEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_ndemax,id_ndpmax,id_nemax,&
       & id_npmax,id_nmumax,id_nk/),id_paep),cvar)
! define attributes
  cvar='SHOT'
  call check(nf90_put_att(icdf,id_ishot,cvar,"-SHOT #"),cvar)
  cvar='DEVICE'
  call check(nf90_put_att(icdf,id_dev,cvar,"-DEVICE NAME"),cvar)
  cvar = 'UFLABEL'  !UF label
  call check(nf90_put_att(icdf,id_uf,cvar,"-UF LABEL"),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call check(nf90_put_att(icdf,id_zsdate,cvar,"-SHOT (DATA ORIGIN) DATE"),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call check(nf90_put_att(icdf,id_zdate,cvar,"-FILE WRITE DATE"),cvar)
  cvar = 'IAEP_SP'
  call check(nf90_put_att(icdf,id_iaep_sp,cvar,"-FAST ION code #: 0--ALL, 1--D,&
        2--T, 3--HE3, 4--HE4, 5--p"),cvar)
  cvar='NK'
  CALL check(nf90_put_att(icdf,id_nk,cvar,"-# OF TERMS IN MARIO PODESTA AEP OPERATOR"),cvar)
  cvar='NT'
  call check(nf90_put_att(icdf,id_nt,cvar,"-# OF POINTS IN TIME GRID TM FOR AEP MODE AMPLITUDE"),cvar)
  cvar='TM'
  call check(nf90_put_att(icdf,id_tm,cvar,"-TIME GRID TM FOR AEP MODE AMPLITUDE,"//ctunits),cvar)
  cvar='AAEP'
  call check(nf90_put_att(icdf,id_aaep,cvar,"-AEP MODE AMPLITUDE"),cvar)
  cvar='TSTEP'
  call check(nf90_put_att(icdf,id_tstep,cvar,"-TIME STEP USED IN SIMULATION,"//ctunits),cvar)
  cvar='DEMIN'
  call check(nf90_put_att(icdf,id_demin,cvar,"-LEFT BOUNDARY FOR DELTA E GRID,"//ceunits),cvar)
  cvar='DEMAX'
  call check(nf90_put_att(icdf,id_demax,cvar,"-RIGHT BOUNDARY FOR DELTA E GRID,"//ceunits),cvar)
  cvar='EMIN'
  call check(nf90_put_att(icdf,id_emin,cvar,"-LEFT BOUNDARY FOR ENERGY GRID,"//ceunits),cvar)
  cvar='EMAX'
  call check(nf90_put_att(icdf,id_emax,cvar,"-RIGHT BOUNDARY FOR ENERGY GRID,"//ceunits),cvar)
  cvar='DPMIN'
  call check(nf90_put_att(icdf,id_dpmin,cvar,"-LEFT BOUNDARY FOR DELTA P GRID"),cvar)
  cvar='DPMAX'
  call check(nf90_put_att(icdf,id_dpmax,cvar,"-RIGHT BOUNDARY FOR DELTA P GRID"),cvar)
  cvar='PMIN'
  call check(nf90_put_att(icdf,id_pmin,cvar,"-LEFT BOUNDARY FOR P(CANONICAL MOMENTUM) GRID"),cvar)
  cvar='PMAX'
  call check(nf90_put_att(icdf,id_pmax,cvar,"-RIGHT BOUNDARY FOR P(CANONICAL MOMENTUM) GRID"),cvar)
  cvar='MUMIN'
  call check(nf90_put_att(icdf,id_mumin,cvar,"-LEFT BOUNDARY FOR mu(MAGNETIC MOMENT) GRID"),cvar)
  cvar='MUMAX'
  call check(nf90_put_att(icdf,id_mumax,cvar,"-RIGHT BOUNDARY FOR mu(MAGNETIC MOMENT) GRID"),cvar)
  cvar='NDEMAX'
  call check(nf90_put_att(icdf,id_ndemax,cvar,"-# OF POINTS IN DE GRID, MAX(1,MAXVAL(NDE))"),cvar)
  cvar='NDPMAX'
  call check(nf90_put_att(icdf,id_ndpmax,cvar,"-# OF POINTS IN DP GRID, MAX(1,MAXVAL(NDP))"),cvar)
  cvar='NEMAX'
  call check(nf90_put_att(icdf,id_nemax,cvar,"-# OF POINTS IN E GRID, MAX(1,MAXVAL(NE))"),cvar)
  cvar='NPMAX'
  call check(nf90_put_att(icdf,id_npmax,cvar,"-# OF POINTS IN P GRID, MAX(1,MAXVAL(NP))"),cvar)
  cvar='NMUMAX'
  call check(nf90_put_att(icdf,id_nmumax,cvar,"-# OF POINTS IN MU GRID, MAX(1,MAXVAL(NMU))"),cvar)
  cvar = 'NDE'
  call check(nf90_put_att(icdf,id_nde,cvar,"-# OF POINTS IN DE GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NDP'
  call check(nf90_put_att(icdf,id_ndp,cvar,"-# OF POINTS IN DP GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NE'
  call check(nf90_put_att(icdf,id_ne,cvar,"-# OF POINTS IN E GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NP'
  call check(nf90_put_att(icdf,id_np,cvar,"-# OF POINTS IN P GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NMU'
  call check(nf90_put_att(icdf,id_nmu,cvar,"-# OF POINTS IN MU GRID THAT DEPEND ON NK"),cvar)
  cvar='PAEP'
  call check(nf90_put_att(icdf,id_ndp,cvar,"-PAEP probability matrix"),cvar)
  
  ier = nf90_enddef(icdf) ! out of def mode and go to data mode
! WRITE Variables  
  cvar='SHOT'
  call check( nf90_put_var(icdf,id_ishot,ishot),cvar)
  cvar='DEVICE'
  call uupper(cdev)
  call check( nf90_put_var(icdf,id_dev,cdev),cvar)
  cvar = 'UFLABEL'  !UF label
  uflabel="-UF NETCDF"
  call check( nf90_put_var(icdf,id_uf,uflabel),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call uupper(zsdate)
  call check( nf90_put_var(icdf,id_zsdate,zsdate),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call uupper(zdate)
  call check( nf90_put_var(icdf,id_zdate,zdate),cvar)
  cvar = 'IAEP_SP'
  call check( nf90_put_var(icdf,id_iaep_sp,iaep_sp(1:nk)),cvar)
  cvar='TM'
  call check( nf90_put_var(icdf,id_tm,tm(1:nt)),cvar)
  cvar='AAEP'
  call check( nf90_put_var(icdf,id_aaep,aaep(1:nt,1:nk)),cvar)
  cvar='TSTEP'
  call check( nf90_put_var(icdf,id_tstep,tstep(1:nk)),cvar)
  cvar='DEMIN'
  call check( nf90_put_var(icdf,id_demin,demin(1:nk)),cvar)
  cvar='DEMAX'
  call check( nf90_put_var(icdf,id_demax,demax(1:nk)),cvar)
  cvar='EMIN'
  call check( nf90_put_var(icdf,id_emin,emin(1:nk)),cvar)
  cvar='EMAX'
  call check( nf90_put_var(icdf,id_emax,emax(1:nk)),cvar)
  cvar='DPMIN'
  call check( nf90_put_var(icdf,id_dpmin,dpmin(1:nk)),cvar)
  cvar='DPMAX'
  call check( nf90_put_var(icdf,id_dpmax,dpmax(1:nk)),cvar)
  cvar='PMIN'
  call check( nf90_put_var(icdf,id_pmin,pmin(1:nk)),cvar)
  cvar='PMAX'
  call check( nf90_put_var(icdf,id_pmax,pmax(1:nk)),cvar)
  cvar='MUMIN'
  call check( nf90_put_var(icdf,id_mumin,mumin(1:nk)),cvar)
  cvar='MUMAX'
  call check( nf90_put_var(icdf,id_mumax,mumax(1:nk)),cvar)
  cvar = 'NDE'
  call check( nf90_put_var(icdf,id_nde,nde(1:nk)),cvar)
  cvar = 'NDP'
  call check( nf90_put_var(icdf,id_ndp,ndp(1:nk)),cvar)
  cvar = 'NE'
  call check( nf90_put_var(icdf,id_ne,ne(1:nk)),cvar)
  cvar = 'NP'
  call check( nf90_put_var(icdf,id_np,np(1:nk)),cvar)
  cvar = 'NMU'
  call check( nf90_put_var(icdf,id_nmu,nmu(1:nk)),cvar)
  cvar='PAEP'
  call check( nf90_put_var(icdf,id_paep,paep(1:ndemax,1:ndpmax,1:nemax,1:npmax,1:nmumax,1:nk)),cvar)
  
  ier = nf90_close( icdf)
  if(ier.ne.0) then
     call ufbad_exit(' UFCDFWR_AEP: NETCDF close failure '//trim(filename))
  endif

  RETURN

contains 
  
  subroutine check(status,varnam)
    integer, intent ( in) :: status 
    character*(*),intent(in) :: varnam ! dimension name input 
    character(len=256) :: buffer
    
    if(status /= nf90_noerr) then 
       buffer = ' UFCDFWR_AEP: error:'//trim(nf90_strerror(status))//' for var.= '//trim(varnam)
       call ufbad_exit(buffer)
    end if
  end subroutine check

  subroutine checkdim
    if(ndemax.ne.maxval(nde)) then
       ier=1
       call ufbad_exit(' UFCDFWR_AEP: error in ndemax')
    endif
    if(ndpmax.ne.maxval(ndp)) then
       ier=1
       call ufbad_exit(' UFCDFWR_AEP: error in ndpmax')
    endif
    if(nemax.ne.maxval(ne)) then
       ier=1
       call ufbad_exit(' UFCDFWR_AEP: error in nemax')
    endif
    if(npmax.ne.maxval(np)) then
       ier=1
       call ufbad_exit(' UFCDFWR_AEP: error in npmax')
    endif
    if(nmumax.ne.maxval(nmu)) then
       ier=1
       call ufbad_exit(' UFCDFWR_AEP: error in nmumax')
    endif
  end subroutine checkdim

END SUBROUTINE UFCDFWR_AEP
!******************** START FILE UFWRCDF_AEP_PACK.F90 ; GROUP UFILES *************!--------------------------------------------------------------
!  UFCDFWR_AEP_PACK
!  WRITE Multi-DIMENSIONAL DATA TO NetCdf FILE for AEP/RBQ model 
!
SUBROUTINE UFCDFWR_AEP_pack(filename,ZSDATE,ISHOT,CDEV,iaep_sp,&
     & NK,AAEP,TM,NT,Tstep,demin,demax,nde,dpmin,dpmax,ndp,emin,emax,ne,&
     & pmin,pmax,np,mumin,mumax,nmu,npack,idiff_map,PAEP,ndemax,ndpmax,nemax,npmax,nmumax,ctunits,ceunits,IER)
! 
!  MG  03/13/2014 
!  ISHOT -- ISHOT # ("139048")
!  IDEV-- 4 LETTER TOKAMAK DEV. CODE ("PLT ", ETC.)
!  nk -- terms # in Mario Pdesta series, # of modes
!
!  iaep_sp(NK) -- species code # to which AEP mode will be applied
!  species code #
!  0                   ;ALL fast ion SPECIES in this run (in NUBEAM)
!  1                   ;deuterium
!  2                   ;tritium
!  3                   ;HE3
!  4                   ;HE4
!  5                   ;protons          
!
!  AAEP(NT,NK) -- Mode amplitude. Aaep_k_specie(time)
!  nt -- # of points in time grig TM
!  TM(nt) -- time grid in ms
!  Tstep(nk) -- time step used to investigate AEP mode
!  demin(NK), demax(NK) -- left and right boundaries for a linear grid, function of nk (# of terms in MP series)
!  dpmin(NK), dpmax(NK) -- same as above
!  emin(NK), emax(NK) -- same as above
!  pmin(NK), pmax(NK) -- same as above
!  mumin(NK), mumax(NK) -- same as above
!  packed -- only non-zeros PAEP(npack)  -- probability matrix
!obsolite  PAEP(ndemax,ndpmax,nemax,npmax,nmumax,nk)  -- probability matrix
!  ndemax -- max(1,maxval(nde))
!  ndpmax -- max(1,maxval(ndp))
!  nemax -- max(1,maxval(ne))
!  npmax -- max(1,maxval(np))
!  nmumax -- max(1,maxval(nmu))
!  PROCESS CODE 0 = "RAW" raw data are saved
!  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
!
  use iso_c_binding, only: fp => c_double
  use netcdf
  implicit none
  character*(*),intent(in) :: filename !with path
  character*(*),intent(in):: CDEV
  integer,intent(in) :: ishot
  integer,intent(in), dimension(nk) :: iaep_sp
  integer,intent(in) :: nk,nt
  real(fp),intent(in)  :: AAEP(nt,nk)
  real(fp),intent(in)  :: tm(nt)
  integer,intent(in) :: nde(nk), ndp(nk), ne(nk), np(nk), nmu(nk)
  real(fp),intent(in)  :: tstep(nk)
  real(fp),intent(in)  :: demin(nk),demax(nk)
  real(fp),intent(in)  :: dpmin(nk),dpmax(nk)
  real(fp),intent(in)  :: emin(nk),emax(nk)
  real(fp),intent(in)  :: pmin(nk),pmax(nk)
  real(fp),intent(in)  :: mumin(nk),mumax(nk)
  integer,intent(in) :: ndemax,ndpmax,nemax,npmax,nmumax !PAEP dimentions
  integer,intent(in) :: npack,idiff_map(npack)
  real(fp),intent(in)  :: paep(npack)
  CHARACTER*9,intent(in) :: ctunits,ceunits ! units for time axis amd for energy axis
  CHARACTER*10,intent(in) :: ZSDATE !  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
  CHARACTER*10 :: ZDATE  !  ZDATE - FILE WRITE DATE
  integer :: ier
  character*30 :: cvar
  character*10 :: uflabel
  integer :: ilbl,ilbl1 !label length
!netcdf var's IDs bellow
  integer :: icdf, id_lbl, id_lbl1, id_uf, id_dev, id_ishot, id_zsdate, id_zdate        
  integer :: id_iaep_sp, id_nk, id_nt, id_aaep, id_tm,id_tstep
  integer :: id_demin, id_demax, id_dpmin, id_dpmax, id_emin, id_emax, id_pmin, id_pmax
  integer :: id_mumin, id_mumax,id_nde, id_ndp, id_ne, id_np, id_nmu, id_paep
  integer :: id_ndemax,id_ndpmax,id_nemax,id_npmax,id_nmumax,id_npack,id_idiff_map
  character :: zbuf*24
!-----------------------------------------
!
!  WRITE ***
!
! sanitary check
  ier=0
  call checkdim
!OK now write netcdf file, raw data
!  IPROC = 0 
!  KMPRS = 4 !CDF file 
  call fdate(zbuf)
  if(zbuf(9:9).eq.' ')zbuf(9:9)=char(48)
  zdate=zbuf(9:10)//zbuf(5:7)//zbuf(21:24)

  call check(nf90_create(path=trim(adjustl(filename)),&
       & cmode=NF90_CLOBBER,ncid=icdf),trim(adjustl(filename)))
  
  cvar='DEVLBL'
  ilbl=4
  call check(nf90_def_dim(icdf,cvar,ilbl,id_lbl),cvar)
  cvar='SHOT'
  call check(nf90_def_var(icdf,cvar,NF90_INT,id_ishot),cvar)
  cvar='DEVICE'
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl/),id_dev),cvar)
  cvar='UFLBL'
  ilbl1=10
  call check(nf90_def_dim(icdf,cvar,ilbl1,id_lbl1),cvar)
  cvar = 'UFLABEL'  !UF label
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_uf),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zsdate),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zdate),cvar)
  cvar='NK'
  call check(nf90_def_dim(icdf,cvar,nk,id_nk),cvar)
  cvar='NT'
  call check(nf90_def_dim(icdf,cvar,nt,id_nt),cvar)
  cvar='NDEMAX'
  call check(nf90_def_dim(icdf,cvar,ndemax,id_ndemax),cvar)
  cvar='NDPMAX'
  call check(nf90_def_dim(icdf,cvar,ndpmax,id_ndpmax),cvar)
  cvar='NEMAX'
  call check(nf90_def_dim(icdf,cvar,nemax,id_nemax),cvar)
  cvar='NPMAX'
  call check(nf90_def_dim(icdf,cvar,npmax,id_npmax),cvar)
  cvar='NMUMAX'
  call check(nf90_def_dim(icdf,cvar,nmumax,id_nmumax),cvar)
  cvar='NPACK'
  call check(nf90_def_dim(icdf,cvar,npack,id_npack),cvar)
  cvar = 'IDIFF_MAP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_npack/),id_idiff_map),cvar)
  cvar = 'IAEP_SP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_iaep_sp),cvar)
  cvar='TM'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nt/),id_tm),cvar)
  cvar='AAEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nt,id_nk/),id_aaep),cvar)
  cvar='TSTEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_tstep),cvar)
  cvar='DEMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_demin),cvar)
  cvar='DEMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_demax),cvar)
  cvar='DPMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_dpmin),cvar)
  cvar='DPMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_dpmax),cvar)
  cvar='EMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_emin),cvar)
  cvar='EMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_emax),cvar)
  cvar='PMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_pmin),cvar)
  cvar='PMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_pmax),cvar)
  cvar='MUMIN'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_mumin),cvar)
  cvar='MUMAX'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_nk/),id_mumax),cvar)
  cvar = 'NDE'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_nde),cvar)
  cvar = 'NDP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_ndp),cvar)
  cvar = 'NE'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_ne),cvar)
  cvar = 'NP'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_np),cvar)
  cvar = 'NMU'
  call check(nf90_def_var(icdf,cvar,NF90_INT,(/id_nk/),id_nmu),cvar)
   cvar='PAEP'
  call check(nf90_def_var(icdf,cvar,NF90_DOUBLE,(/id_npack/),id_paep),cvar)
! define attributes
  cvar='SHOT'
  call check(nf90_put_att(icdf,id_ishot,cvar,"-SHOT #"),cvar)
  cvar='DEVICE'
  call check(nf90_put_att(icdf,id_dev,cvar,"-DEVICE NAME"),cvar)
  cvar = 'UFLABEL'  !UF label
  call check(nf90_put_att(icdf,id_uf,cvar,"-UF LABEL"),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call check(nf90_put_att(icdf,id_zsdate,cvar,"-SHOT (DATA ORIGIN) DATE"),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call check(nf90_put_att(icdf,id_zdate,cvar,"-FILE WRITE DATE"),cvar)
  cvar = 'IDIFF_MAP'
  call check(nf90_put_att(icdf,id_idiff_map,cvar," indecies mapping for non-zeros"),cvar)
  cvar = 'IAEP_SP'
  call check(nf90_put_att(icdf,id_iaep_sp,cvar,"-FAST ION code #: 0--ALL, 1--D,&
        2--T, 3--HE3, 4--HE4, 5--p"),cvar)
  cvar='NK'
  CALL check(nf90_put_att(icdf,id_nk,cvar,"-# OF TERMS IN MARIO PODESTA AEP OPERATOR"),cvar)
  cvar='NT'
  call check(nf90_put_att(icdf,id_nt,cvar,"-# OF POINTS IN TIME GRID TM FOR AEP MODE AMPLITUDE"),cvar)
  cvar='TM'
  call check(nf90_put_att(icdf,id_tm,cvar,"-TIME GRID TM FOR AEP MODE AMPLITUDE,"//ctunits),cvar)
  cvar='AAEP'
  call check(nf90_put_att(icdf,id_aaep,cvar,"-AEP MODE AMPLITUDE"),cvar)
  cvar='TSTEP'
  call check(nf90_put_att(icdf,id_tstep,cvar,"-TIME STEP USED IN SIMULATION,"//ctunits),cvar)
  cvar='DEMIN'
  call check(nf90_put_att(icdf,id_demin,cvar,"-LEFT BOUNDARY FOR DELTA E GRID,"//ceunits),cvar)
  cvar='DEMAX'
  call check(nf90_put_att(icdf,id_demax,cvar,"-RIGHT BOUNDARY FOR DELTA E GRID,"//ceunits),cvar)
  cvar='EMIN'
  call check(nf90_put_att(icdf,id_emin,cvar,"-LEFT BOUNDARY FOR ENERGY GRID,"//ceunits),cvar)
  cvar='EMAX'
  call check(nf90_put_att(icdf,id_emax,cvar,"-RIGHT BOUNDARY FOR ENERGY GRID,"//ceunits),cvar)
  cvar='DPMIN'
  call check(nf90_put_att(icdf,id_dpmin,cvar,"-LEFT BOUNDARY FOR DELTA P GRID"),cvar)
  cvar='DPMAX'
  call check(nf90_put_att(icdf,id_dpmax,cvar,"-RIGHT BOUNDARY FOR DELTA P GRID"),cvar)
  cvar='PMIN'
  call check(nf90_put_att(icdf,id_pmin,cvar,"-LEFT BOUNDARY FOR P(CANONICAL MOMENTUM) GRID"),cvar)
  cvar='PMAX'
  call check(nf90_put_att(icdf,id_pmax,cvar,"-RIGHT BOUNDARY FOR P(CANONICAL MOMENTUM) GRID"),cvar)
  cvar='MUMIN'
  call check(nf90_put_att(icdf,id_mumin,cvar,"-LEFT BOUNDARY FOR mu(MAGNETIC MOMENT) GRID"),cvar)
  cvar='MUMAX'
  call check(nf90_put_att(icdf,id_mumax,cvar,"-RIGHT BOUNDARY FOR mu(MAGNETIC MOMENT) GRID"),cvar)
  cvar='NDEMAX'
  call check(nf90_put_att(icdf,id_ndemax,cvar,"-# OF POINTS IN DE GRID, MAX(1,MAXVAL(NDE))"),cvar)
  cvar='NDPMAX'
  call check(nf90_put_att(icdf,id_ndpmax,cvar,"-# OF POINTS IN DP GRID, MAX(1,MAXVAL(NDP))"),cvar)
  cvar='NEMAX'
  call check(nf90_put_att(icdf,id_nemax,cvar,"-# OF POINTS IN E GRID, MAX(1,MAXVAL(NE))"),cvar)
  cvar='NPMAX'
  call check(nf90_put_att(icdf,id_npmax,cvar,"-# OF POINTS IN P GRID, MAX(1,MAXVAL(NP))"),cvar)
  cvar='NMUMAX'
  call check(nf90_put_att(icdf,id_nmumax,cvar,"-# OF POINTS IN MU GRID, MAX(1,MAXVAL(NMU))"),cvar)
  cvar='NPACK'
  call check(nf90_put_att(icdf,id_npack,cvar,"-# OF NON-ZEROS POINTS IN PROBABILY MATRIX"),cvar)
  cvar = 'NDE'
  call check(nf90_put_att(icdf,id_nde,cvar,"-# OF POINTS IN DE GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NDP'
  call check(nf90_put_att(icdf,id_ndp,cvar,"-# OF POINTS IN DP GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NE'
  call check(nf90_put_att(icdf,id_ne,cvar,"-# OF POINTS IN E GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NP'
  call check(nf90_put_att(icdf,id_np,cvar,"-# OF POINTS IN P GRID THAT DEPEND ON NK"),cvar)
  cvar = 'NMU'
  call check(nf90_put_att(icdf,id_nmu,cvar,"-# OF POINTS IN MU GRID THAT DEPEND ON NK"),cvar)
  cvar='PAEP'
  call check(nf90_put_att(icdf,id_ndp,cvar,"-PAEP packed probability matrix"),cvar)
  
  ier = nf90_enddef(icdf) ! out of def mode and go to data mode
! WRITE Variables  
  cvar='SHOT'
  call check( nf90_put_var(icdf,id_ishot,ishot),cvar)
  cvar='DEVICE'
  call uupper(cdev)
  call check( nf90_put_var(icdf,id_dev,cdev),cvar)
  cvar = 'UFLABEL'  !UF label
  uflabel="-UF NETCDF"
  call check( nf90_put_var(icdf,id_uf,uflabel),cvar)
  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
  call uupper(zsdate)
  call check( nf90_put_var(icdf,id_zsdate,zsdate),cvar)
  cvar = 'ZDATE'    !FILE WRITE DATE
  call uupper(zdate)
  call check( nf90_put_var(icdf,id_zdate,zdate),cvar)
  cvar = 'IDIFF_MAP'
  call check( nf90_put_var(icdf,id_idiff_map,idiff_map(1:npack)),cvar)
  cvar = 'IAEP_SP'
  call check( nf90_put_var(icdf,id_iaep_sp,iaep_sp(1:nk)),cvar)
  cvar='TM'
  call check( nf90_put_var(icdf,id_tm,tm(1:nt)),cvar)
  cvar='AAEP'
  call check( nf90_put_var(icdf,id_aaep,aaep(1:nt,1:nk)),cvar)
  cvar='TSTEP'
  call check( nf90_put_var(icdf,id_tstep,tstep(1:nk)),cvar)
  cvar='DEMIN'
  call check( nf90_put_var(icdf,id_demin,demin(1:nk)),cvar)
  cvar='DEMAX'
  call check( nf90_put_var(icdf,id_demax,demax(1:nk)),cvar)
  cvar='EMIN'
  call check( nf90_put_var(icdf,id_emin,emin(1:nk)),cvar)
  cvar='EMAX'
  call check( nf90_put_var(icdf,id_emax,emax(1:nk)),cvar)
  cvar='DPMIN'
  call check( nf90_put_var(icdf,id_dpmin,dpmin(1:nk)),cvar)
  cvar='DPMAX'
  call check( nf90_put_var(icdf,id_dpmax,dpmax(1:nk)),cvar)
  cvar='PMIN'
  call check( nf90_put_var(icdf,id_pmin,pmin(1:nk)),cvar)
  cvar='PMAX'
  call check( nf90_put_var(icdf,id_pmax,pmax(1:nk)),cvar)
  cvar='MUMIN'
  call check( nf90_put_var(icdf,id_mumin,mumin(1:nk)),cvar)
  cvar='MUMAX'
  call check( nf90_put_var(icdf,id_mumax,mumax(1:nk)),cvar)
  cvar = 'NDE'
  call check( nf90_put_var(icdf,id_nde,nde(1:nk)),cvar)
  cvar = 'NDP'
  call check( nf90_put_var(icdf,id_ndp,ndp(1:nk)),cvar)
  cvar = 'NE'
  call check( nf90_put_var(icdf,id_ne,ne(1:nk)),cvar)
  cvar = 'NP'
  call check( nf90_put_var(icdf,id_np,np(1:nk)),cvar)
  cvar = 'NMU'
  call check( nf90_put_var(icdf,id_nmu,nmu(1:nk)),cvar)
  cvar='PAEP'
  call check( nf90_put_var(icdf,id_paep,paep(1:npack)),cvar)
  
  ier = nf90_close( icdf)
  if(ier.ne.0) then
     write(*,*)' UFCDFWR_AEP_PACK: NETCDF close failure '//trim(filename)
     call exit(ier)
  endif

  RETURN

contains 
  
  subroutine check(status,varnam)
    integer, intent ( in) :: status 
    character*(*),intent(in) :: varnam ! dimension name input 
    
    if(status /= nf90_noerr) then 
       write(*,*)' UFCDFWR_AEP_PACK: error:'& 
            &//trim(nf90_strerror(status))//' for var.= '//trim(varnam)
       call exit(ier) 
    end if
  end subroutine check

  subroutine checkdim
    if(ndemax.ne.maxval(nde)) then
       write(*,*)' UFCDFWR_AEP_PACK: error in ndemax'
       ier=1
       call exit 
    endif
    if(ndpmax.ne.maxval(ndp)) then
       write(*,*)' UFCDFWR_AEP_PACK: error in ndpmax'
       ier=1
       call exit(ier) 
    endif
    if(nemax.ne.maxval(ne)) then
       write(*,*)' UFCDFWR_AEP_PACK: error in nemax'
       ier=1
       call exit(ier) 
    endif
    if(npmax.ne.maxval(np)) then
       write(*,*)' UFCDFWR_AEP_PACK: error in npmax'
       ier=1
       call exit(ier) 
    endif
    if(nmumax.ne.maxval(nmu)) then
       write(*,*)' UFCDFWR_AEP_PACK: error in nmumax'
       ier=1
       call exit(ier) 
    endif
  end subroutine checkdim

END SUBROUTINE UFCDFWR_AEP_PACK
