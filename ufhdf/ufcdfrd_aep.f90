!******************** START FILE UFWRCDF_AEP.F90 ; GROUP UFILES ******************
!--------------------------------------------------------------
!  UFCDFRD_AEP
! READ Multi-DIMENSIONAL DATA TO NetCdf FILE for AEP model (Mario Pdesta)
!
SUBROUTINE UFCDFRD_AEP_DIM(nlun,ilun,icdf,NPACK,NK,NT,NDEMAX,NDPMAX,&
     NEMAX,NPMAX,NMUMAX,IER)
! 
!  MG  03/13/2014 
!  nk -- terms # in Mario Pdesta series
!  nt -- # of points in time grig TM
!  ndemax -- max(1,maxval(nde))
!  ndpmax -- max(1,maxval(ndp))
!  nemax -- max(1,maxval(ne))
!  npmax -- max(1,maxval(np))
!  nmumax -- max(1,maxval(nmu))
!  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
!
  use netcdf
  use UFILES, only : ufid
  implicit none
  integer :: ierr,ier
  integer,intent(in) :: ilun !channel ID
  integer,intent(in) :: nlun ! output file ID
  integer,intent(out) :: icdf !NetCdf ID
  integer :: i_dim       
  character*30 :: cvar
  integer,intent(out) :: nk,nt,NDEMAX,NDPMAX,NEMAX,NPMAX,NMUMAX,npack
  integer :: ichan
! it is related to include 'UFILES' for more details see inclshare/UFILES
!-----------------------------------------
!
!  WRITE ***
!
! sanitary check
  ier=0
  CALL UFINIT
  CALL UFLCHK(ILUN,'UFCDFRDAEP',ICHAN,IER)
  IF(ier.NE.0) THEN
     write(nlun,*) ' ?ufcdfrd_aep error: trying to read wrong file'
!     CALL UERSTA(ICHAN,'UFCDFRDAEP')
     ier=1
     return
  ENDIF

  icdf=ufid(ichan)
  ierr=0

  cvar='NK'
  nk=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     nk=i_dim
  endif
  if(nk.le.0.or.nk.gt.10) then
     write(nlun,*) ' ?ufcdfrd_aep error: nk for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NT'
  NT=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     NT=i_dim
  endif
  if(NT.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: NT for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NDEMAX'
  Ndemax=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     Ndemax=i_dim
  endif
  if(Ndemax.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: Ndemax for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NDPMAX'
  NDPMAX=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     NDPMAX=i_dim
  endif
  if(NDPMAX.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: NDPMAX for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NEMAX'
  NEMAX=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     NEMAX=i_dim
  endif
  if(NEMAX.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: NEMAX for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NPMAX'
  Npmax=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     Npmax=i_dim
  endif
  if(Npmax.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: Npmax for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NMUMAX'
  Nmumax=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     Nmumax=i_dim
  endif
  if(Nmumax.le.0) then
     write(nlun,*) ' ?ufcdfrd_aep error: Nmumax for AEP has not been set correctly'
     ier=ier+1
  endif

  cvar='NPACK'
  npack=0
  call check_inq_dim(icdf,cvar,i_dim,ierr)
  if(ierr.eq.0) then
     npack=i_dim
  endif
  if(npack.lt.0) then
     write(nlun,*) ' ?ufcdfrd_aep_dim error: npack for AEP has not been set correctly'
     ier=ier+1
  endif
  if(ier.gt.0) then
     ierr = nf90_close(icdf)
     ier=ier+ierr
  endif
  RETURN

contains 
  
  subroutine check_inq_dim(i_cdf,varnam,idim,ierrcdf) 
    integer,intent(in) :: i_cdf
    integer,intent(out) :: ierrcdf 
    character*(*),intent(in) :: varnam ! dimension name input 
    integer, intent(out) :: idim        ! dimension size returned 
    integer ::idd !var ID 
    ierrcdf = nf90_inq_dimid(i_cdf,trim(adjustl(varnam)),idd) 
    if(ierrcdf.ne.0) then 
       if(trim(adjustl(varnam)).eq.'NPACK') then
          ierrcdf=0
          idim=0
          return
       else
          write(nlun,*) ' ?ufcdfrd_aep_dim error: NO such dim='//trim(varnam)
          ier=ier+1
          return
       endif
    endif 
    ierrcdf = nf90_inquire_dimension(i_cdf,idd,len=idim) 
    if(ierrcdf.ne.0) then 
       write(nlun,*) ' ?ufcdfrd_aep error:'//nf90_strerror(ierrcdf)//'for var.'//trim(varnam) 
       ier=ier+1
    endif 
  end subroutine check_inq_dim

END SUBROUTINE UFCDFRD_AEP_DIM

!******************** START FILE UFWRCDF_AEP.F90 ; GROUP UFILES ******************
!--------------------------------------------------------------
!  UFCDFRD_AEP
! READ Multi-DIMENSIONAL DATA TO NetCdf FILE for AEP model (Mario Pdesta)
!
SUBROUTINE UFCDFRD_AEP(nlun,icdf,inshot,aep_sp,&
     NK,NT,NDEMAX,NDPMAX,NEMAX,NPMAX,NMUMAX,&
     TM,AAEP,TSTEP,DE,DP,E,P,ZMU,PAEP,ctunits,ceunits,IER)
! 
!  MG  03/13/2014 
!  ISHOT -- ISHOT # ("139048")
!  IDEV-- 4 LETTER TOKAMAK DEV. CODE ("PLT ", ETC.)
!  nk -- terms # in Mario Pdesta series
!  iaep_sp(NK) -- code # of species with which mode (nk) will have intercation
!  species code #
!  0                  ;ALL fast ion SPECIES in this run (in NUBEAM)
!  1                  ;deuterium
!  2                  ;tritium
!  3                  ;HE3
!  4                  ;HE4
!  5                  ;protons          
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
  integer,intent(in) :: inshot !SHOT no
  integer,intent(in) :: icdf   !NetCdf ID 
  integer,intent(in) :: nlun   !output file ID 
  integer,intent(in) :: nk,nt,NDEMAX,NDPMAX,NEMAX,NPMAX,NMUMAX
  character*9, intent(out):: ctunits,ceunits !units for Time and Energy from Ufile
  real(fp),dimension(nk) :: aep_sp !species code # to which AEP mode will be applied
  integer,dimension(nk) :: iaep_sp !species code # to which AEP mode will be applied
  integer :: ierr,ier
  integer :: ishot
  integer :: id      
  character*30 :: cvar
  real(fp) :: TM(nt),AAEP(nt,nk),Tstep(nk),DE(nk,3),DP(nk,3),E(nk,3),P(nk,3),ZMU(nk,3)
  real(fp) :: NDE(nk),NDP(nk),NE(nk),NP(nk),NMU(nk),PAEP(NDEMAX,NDPMAX,NEMAX,NPMAX,NMUMAX,NK)
  character (len=80) :: ctime_units,cenergy_units
  integer :: ilunits,maxlen_u,i
!-----------------------------------------
!
!  WRITE ***
!
! sanitary check
  ier=0
  ierr=0
!OK now write netcdf file, raw data
!  IPROC = 0 
!  KMPRS = 4 !CDF file 
!  cvar='DEVLBL'
!  ilbl=4
!  call check(nf90_def_dim(icdf,cvar,ilbl,id_lbl),cvar)
  cvar='SHOT'
  ishot=0
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ishot),cvar)
  endif
  if(ishot.ne.inshot) then
     write(nlun,*) ' !ufcdfrd_aep warning: inconsistency in shot no. found in Ufile'
  endif
!  cvar='DEVICE'
!  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl/),id_dev),cvar)
!  cvar='UFLBL'
!  ilbl1=10
!  call check(nf90_def_dim(icdf,cvar,ilbl1,id_lbl1),cvar)
!  cvar = 'UFLABEL'  !UF label
!  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_uf),cvar)
!  cvar = 'ZSDATE'   !SHOT (DATA ORIGIN) DATE
! call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zsdate),cvar)
!  cvar = 'ZDATE'    !FILE WRITE DATE
!  call check(nf90_def_var(icdf,cvar,NF90_CHAR,(/id_lbl1/),id_zdate),cvar)

  iaep_sp=-1 ! initialization
  cvar = 'IAEP_SP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,iaep_sp(:)),cvar)
     aep_sp(:)=real(iaep_sp(:),fp)
  endif
  cvar='TM'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,TM(:)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,ctime_units)
     ctunits=' '
     if(ierr.eq.0) then
        ilunits=len(ctime_units)
        do i=1,ilunits
           if(ctime_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ctunits(1:maxlen_u)=ctime_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
! get attribute to find units
  
  
  cvar='AAEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,AAEP(:,:)),cvar)
  endif
  cvar='TSTEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,TSTEP(:)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,ctime_units)
     ctunits=' '
     if(ierr.eq.0) then
        ilunits=len(ctime_units)
        do i=1,ilunits
           if(ctime_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ctunits(1:maxlen_u)=ctime_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
  
  cvar='DEMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DE(:,2)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,cenergy_units)
     ceunits=' '
     if(ierr.eq.0) then
        ilunits=len(cenergy_units)
        do i=1,ilunits
           if(cenergy_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ceunits(1:maxlen_u)=cenergy_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
  cvar='DEMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DE(:,3)),cvar)
  endif

  cvar='DPMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DP(:,2)),cvar)
  endif
  cvar='DPMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DP(:,3)),cvar)
  endif

  cvar='EMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,E(:,2)),cvar)
  endif
  cvar='EMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,E(:,3)),cvar)
  endif

  cvar='PMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,P(:,2)),cvar)
  endif
  cvar='PMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,P(:,3)),cvar)
  endif

  cvar='MUMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ZMU(:,2)),cvar)
  endif
  cvar='MUMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ZMU(:,3)),cvar)
  endif

  cvar = 'NDE'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,nde(:)),cvar)
     DE(:,1)=REAL(nde(:),fp) 
  endif

  cvar = 'NDP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ndp(:)),cvar)
     DP(:,1)=REAL(ndp(:),fp) 
  endif

  cvar = 'NE'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ne(:)),cvar)
     E(:,1)=REAL(ne(:),fp) 
  endif

  cvar = 'NP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,np(:)),cvar)
     P(:,1)=REAL(np(:),fp) 
  endif

  cvar = 'NMU'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,nmu(:)),cvar)
      ZMU(:,1)=REAL(nmu(:),fp) 
   endif

  cvar='PAEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,PAEP(:,:,:,:,:,:)),cvar)
  endif
!MG  ierr = nf90_close(icdf)

  ier=ier+ierr

  RETURN

contains 
  
  subroutine check(status,varnam)
    integer, intent ( in) :: status 
    character*(*),intent(in) :: varnam ! dimension name input 
    
    if(status /= nf90_noerr) then 
       write(nlun,*)' ?UFCDFRD_AEP: error:'& 
            &// trim(nf90_strerror(status))//' for var.= '//trim(varnam)
       ierr=ierr+1
    end if
  end subroutine check

  subroutine check_inq(i_cdf,varnam,id,ierrcdf)
    integer,intent(in) :: i_cdf
    integer,intent(out) :: id,ierrcdf
    character*(*),intent(in) :: varnam 
    ierrcdf = nf90_inq_varid(i_cdf,trim(varnam),id) 
    if(ierrcdf.ne.0) then 
       write(nlun,*) ' ?ufcdfrd_aep error: invalid var.: '//trim(varnam)
       ier=ier+1
    endif
  end subroutine check_inq

END SUBROUTINE UFCDFRD_AEP

!******************** START FILE UFWRCDF_AEP_PACK.F90 ; GROUP UFILES ******************
!--------------------------------------------------------------
!  UFCDFRD_AEP_PACK
! READ Multi-DIMENSIONAL DATA TO NetCdf FILE for AEP model (Mario Pdesta)
!
SUBROUTINE UFCDFRD_AEP_PACK(nlun,icdf,inshot,aep_sp,&
     NPACK, NK,NT,&
     TM,AAEP,TSTEP,DE,DP,E,P,ZMU,IDIFF_MAP,PAEP,ctunits,ceunits,IER)
! 
!  MG  03/13/2014 
!  ISHOT -- ISHOT # ("139048")
!  IDEV-- 4 LETTER TOKAMAK DEV. CODE ("PLT ", ETC.)
!  nk -- terms # in Mario Pdesta series
!  iaep_sp(NK) -- code # of species with which mode (nk) will have intercation
!  species code #
!  0                  ;ALL fast ion SPECIES in this run (in NUBEAM)
!  1                  ;deuterium
!  2                  ;tritium
!  3                  ;HE3
!  4                  ;HE4
!  5                  ;protons          
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
!  PAEP(npack)  -- packed probability matrix
!  PROCESS CODE 0 = "RAW" raw data are saved
!  IER=0 ON RETURN IF WRITE WAS SUCCESSFUL
!
  use iso_c_binding, only: fp => c_double
  use netcdf
  implicit none
  integer,intent(in) :: inshot !SHOT no
  integer,intent(in) :: icdf   !NetCdf ID 
  integer,intent(in) :: nlun   !output file ID 
  integer,intent(in) :: nk,nt,npack
  character*9, intent(out):: ctunits,ceunits !units for Time and Energy from Ufile
  real(fp),dimension(nk) :: aep_sp !species code # to which AEP mode will be applied
  integer,dimension(nk) :: iaep_sp !species code # to which AEP mode will be applied
  integer,dimension(npack) :: idiff_map ! indecies for non-zeros in probability martix 
  integer :: ierr,ier
  integer :: ishot
  integer :: id      
  character*30 :: cvar
  real(fp) :: TM(nt),AAEP(nt,nk),Tstep(nk),DE(nk,3),DP(nk,3),E(nk,3),P(nk,3),ZMU(nk,3)
  real(fp) :: NDE(nk),NDP(nk),NE(nk),NP(nk),NMU(nk),PAEP(npack)
  character (len=80) :: ctime_units,cenergy_units
  integer :: ilunits,maxlen_u,i
!-----------------------------------------
!
!  WRITE ***
!
! sanitary check
  ier=0
  ierr=0
!OK now write netcdf file, raw data
!  IPROC = 0 
!  KMPRS = 4 !CDF file 
!  cvar='DEVLBL'
!  ilbl=4
!  call check(nf90_def_dim(icdf,cvar,ilbl,id_lbl),cvar)
  cvar='SHOT'
  ishot=0
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ishot),cvar)
  endif
  if(ishot.ne.inshot) then
     write(nlun,*) ' !ufcdfrd_aep_pack warning: inconsistency in shot no. found in Ufile'
  endif
  iaep_sp=-1 ! initialization
  cvar = 'IAEP_SP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,iaep_sp(:)),cvar)
     aep_sp(:)=real(iaep_sp(:),fp)
  endif
  cvar='TM'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,TM(:)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,ctime_units)
     ctunits=' '
     if(ierr.eq.0) then
        ilunits=len(ctime_units)
        do i=1,ilunits
           if(ctime_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ctunits(1:maxlen_u)=ctime_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
! get attribute to find units

  cvar='AAEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,AAEP(:,:)),cvar)
  endif
 
  cvar='TSTEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,TSTEP(:)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,ctime_units)
     ctunits=' '
     if(ierr.eq.0) then
        ilunits=len(ctime_units)
        do i=1,ilunits
           if(ctime_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ctunits(1:maxlen_u)=ctime_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
  
  cvar='DEMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DE(:,2)),cvar)
     ierr = nf90_get_att(icdf,id,cvar,cenergy_units)
     ceunits=' '
     if(ierr.eq.0) then
        ilunits=len(cenergy_units)
        do i=1,ilunits
           if(cenergy_units(i:i).eq.',') then
              maxlen_u=min(9,ilunits-i) ! written by ufcdfwr_aep, but may be truncated by netcdf
              ceunits(1:maxlen_u)=cenergy_units(i+1:i+maxlen_u)
              exit
           endif
        enddo
     endif
  endif
 
  cvar='DEMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DE(:,3)),cvar)
  endif

  cvar='DPMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DP(:,2)),cvar)
  endif

  cvar='DPMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,DP(:,3)),cvar)
  endif

  cvar='EMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,E(:,2)),cvar)
  endif

  cvar='EMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,E(:,3)),cvar)
  endif

  cvar='PMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,P(:,2)),cvar)
  endif

  cvar='PMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,P(:,3)),cvar)
  endif

  cvar='MUMIN'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ZMU(:,2)),cvar)
  endif

  cvar='MUMAX'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ZMU(:,3)),cvar)
  endif

  cvar = 'NDE'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,nde(:)),cvar)
     DE(:,1)=REAL(nde(:),fp) 
  endif

  cvar = 'NDP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ndp(:)),cvar)
     DP(:,1)=REAL(ndp(:),fp) 
  endif

  cvar = 'NE'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,ne(:)),cvar)
     E(:,1)=REAL(ne(:),fp) 
  endif

  cvar = 'NP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,np(:)),cvar)
     P(:,1)=REAL(np(:),fp) 
  endif

  cvar = 'NMU'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,nmu(:)),cvar)
      ZMU(:,1)=REAL(nmu(:),fp) 
   endif

  cvar='IDIFF_MAP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,idiff_map(:)),cvar)
  endif

  cvar='PAEP'
  call check_inq(icdf,cvar,id,ierr)
  if(ierr.eq.0) then
     call check(nf90_get_var(icdf,id,PAEP(:)),cvar)
  endif

  ier=ier+ierr

  RETURN

contains 
  
  subroutine check(status,varnam)
    integer, intent ( in) :: status 
    character*(*),intent(in) :: varnam ! dimension name input 
    
    if(status /= nf90_noerr) then 
       write(nlun,*)' ?UFCDFRD_AEP_PACK: error:'& 
            &// trim(nf90_strerror(status))//' for var.= '//trim(varnam)
       ierr=ierr+1
    end if
  end subroutine check

  subroutine check_inq(i_cdf,varnam,id,ierrcdf)
    integer,intent(in) :: i_cdf
    integer,intent(out) :: id,ierrcdf
    character*(*),intent(in) :: varnam 
    ierrcdf = nf90_inq_varid(i_cdf,trim(varnam),id) 
    if(ierrcdf.ne.0) then 
       write(nlun,*) ' ?ufcdfrd_aep_pack error: invalid var.: '//trim(varnam)
       ier=ier+1
    endif
  end subroutine check_inq

END SUBROUTINE UFCDFRD_AEP_PACK

SUBROUTINE UFCDFRD_AEP_DATA(nlun,icdf,inshot,NPACK,MKAEP,MTAEP,&
     MdEAEP,MdpAEP,MEAEP,MpAEP,MmuAEP,&
     isiza,dat_buf,ctunits,ceunits,ier)
!  read data from already open file with ID=icdf
  use iso_c_binding, only: fp => c_double
  use netcdf
  implicit none
  integer,intent(in) :: inshot !SHOT no
  integer,intent(in) :: icdf   !NetCdf ID 
  integer,intent(in) :: nlun   !output file ID 
  integer,intent(in) :: NPACK,MKAEP,MTAEP,MdEAEP,MdpAEP,MEAEP,MpAEP,MmuAEP !dimensions
  integer,intent(in) :: isiza
  real(fp), dimension(:), allocatable :: tmpT,tmpTstep
  real(fp), dimension(:,:), allocatable :: DATA_tmpA,tmpDE,tmpDP,tmpE,tmpP,tmpMU
  integer, dimension(:), allocatable :: idiff_map
  real(fp), dimension(:), allocatable :: DATA_tmpP
  real(fp), dimension(3) :: AEPSP 
  character*9 :: ctunits,ceunits !units for Time and Energy from Ufile  
  integer :: ier      ! local error status variable
  integer :: il,ik,ide,idp,ie,ip,imu
  real dat_buf(isiza)  
  !
  ier = 0
  !  allocate AEP arrays 
  if(allocated(tmpT)) deallocate(tmpT)
  allocate(tmpT(MTAEP))
  tmpT=0.0_fp
  if(allocated(DATA_tmpA)) deallocate(DATA_tmpA)
  allocate(DATA_tmpA(MTAEP,MKAEP))
  DATA_tmpA=0.0_fp
  if(allocated(tmpTstep)) deallocate(tmpTstep)
  allocate(tmpTstep(MKAEP))
  tmpTstep=0.0_fp
  if(allocated(tmpDE)) deallocate(tmpDE)
  allocate(tmpDE(MKAEP,3))
  tmpDE=0.0_fp
  if(allocated(tmpDP)) deallocate(tmpDP)
  allocate(tmpDP(MKAEP,3))
  tmpDP=0.0_fp
  if(allocated(tmpE)) deallocate(tmpE)
  allocate(tmpE(MKAEP,3))
  tmpE=0.0_fp
  if(allocated(tmpP)) deallocate(tmpP)
  allocate(tmpP(MKAEP,3))
  tmpP=0.0_fp
  if(allocated(tmpMU)) deallocate(tmpMU)
  allocate(tmpMU(MKAEP,3))
  tmpMU=0.0_fp
  if(allocated(DATA_tmpP)) deallocate(DATA_tmpP)
  allocate(DATA_tmpP(npack))
  DATA_tmpP=0.0_fp
  if(allocated(idiff_map)) deallocate(idiff_map)
  allocate(idiff_map(npack))
  idiff_map=0
  ! read expantion for AEP operator, close netcdf file  
  call ufcdfrd_aep_pack(nlun,icdf,iNSHOT,AEPSP,NPACK,&
       MKAEP,MTAEP,&
       tmpT,DATA_tmpA,tmpTstep,tmpDE,tmpDP,tmpE,tmpP,&
       tmpMU,IDIFF_MAP,DATA_tmpP,ctunits,ceunits,ier)
  if(ier.gt.0)then
     return
  endif
  dat_buf=0.0
  il=1
  dat_buf(il)=real(MKAEP)
  il=il+1
  dat_buf(il)=real(MTAEP)
  il=il+1
  dat_buf(il)=real(MdEAEP)
  il=il+1
  dat_buf(il)=real(MdpAEP)
  il=il+1
  dat_buf(il)=real(MEAEP)
  il=il+1
  dat_buf(il)=real(MpAEP)
  il=il+1
  dat_buf(il)=real(MmuAEP)
  il=il+1
  dat_buf(il)=real(npack)
  il=il+1
  dat_buf(il:il+MKAEP-1)=real(AEPSP(1:MKAEP))
  il=il+MKAEP
  dat_buf(il:il+MTAEP-1)=real(tmpT(1:MTAEP))
  il=il+MTAEP
  do ik=1,MKAEP
     dat_buf(il:il+MTAEP-1)=real(DATA_tmpA(1:MTAEP,ik))
     il=il+MTAEP
  enddo
  dat_buf(il:il+MKAEP-1)=real(tmpTstep(1:MKAEP))
  il=il+MKAEP
  do ik=1,3
     dat_buf(il:il+MKAEP-1)=real(tmpDE(1:MKAEP,ik))
     il=il+MKAEP
  enddo
  do ik=1,3
     dat_buf(il:il+MKAEP-1)=real(tmpDP(1:MKAEP,ik))
     il=il+MKAEP
  enddo
  do ik=1,3
     dat_buf(il:il+MKAEP-1)=real(tmpE(1:MKAEP,ik))
     il=il+MKAEP
  enddo
  do ik=1,3
     dat_buf(il:il+MKAEP-1)=real(tmpP(1:MKAEP,ik))
     il=il+MKAEP
  enddo
  do ik=1,3
     dat_buf(il:il+MKAEP-1)=real(tmpMU(1:MKAEP,ik))
     il=il+MKAEP
  enddo
  dat_buf(il:il+npack-1)=real(idiff_map(1:npack))
  il=il+npack
  dat_buf(il:il+npack-1)=real(DATA_tmpP(1:npack))
  il=il+npack
  if(allocated(tmpT)) then
     deallocate(tmpT,idiff_map,DATA_tmpP,DATA_tmpA,tmpDE,tmpDP,tmpE,tmpP,tmpMU)
  endif
  return
end SUBROUTINE UFCDFRD_AEP_DATA

!******************** END FILE UFCDFRD_AEP.F90 ; GROUP UFILES ******************
