!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCLINS
      SUBROUTINE R8_AGCLINS(
     > R8_ZDATA,MXD,R8_X,NX,R8_Y,NY,R8_CL,NCL)

      implicit none
      external AGCLINS

! argument declarations
      INTEGER MXD
      INTEGER NX
      INTEGER NY
      INTEGER NCL

! >>> from sgnco_mod
      integer   :: nlm
      parameter (nlm=40)
  
 ! floating type, input/output:
      REAL*8 R8_ZDATA(:,:)
  
 ! floating type, input/output:
      REAL*8 R8_X(:)
  
 ! floating type, input/output:
      REAL*8 R8_Y(:)
  
 ! floating type, input/output:
      REAL*8 R8_CL(NLM)
  

! local (automatic array) declarations

      integer alloc_stat, dim, dim2
      REAL, allocatable :: ZDATA_act(:,:)
      REAL, allocatable :: ZDATA_ref(:,:)
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: CL_act(:)
      REAL, allocatable :: CL_ref(:)

! allocation of working arrays...

      dim2=size(R8_ZDATA,dim=2)
      ALLOCATE(ZDATA_act(MXD,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- ZDATA_act ALLOCATE error!')
      ALLOCATE(ZDATA_ref(MXD,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- ZDATA_ref ALLOCATE error!')

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- Y_ref ALLOCATE error!')

      ALLOCATE(CL_act(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLINS -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      ZDATA_act=R8_ZDATA
      ZDATA_ref=R8_ZDATA

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      CL_act=R8_CL
      CL_ref=R8_CL

! call to original routine:  AGCLINS

      CALL AGCLINS(
     > ZDATA_act(1,1),MXD,X_act(1),NX,Y_act(1),NY,CL_act(1),NCL)

! copy back outputs if modified.

      where(ZDATA_act.ne.ZDATA_ref)
         R8_ZDATA = ZDATA_act
      end where
      DEALLOCATE(ZDATA_act)
      DEALLOCATE(ZDATA_ref)

      where(X_act.ne.X_ref)
         R8_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R8_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

      where(CL_act.ne.CL_ref)
         R8_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCLGEN
      SUBROUTINE R8_AGCLGEN(
     > R8_Z,MXD,NX,NY,R8_CCLO,R8_CHI,R8_CINC,R8_CL,NCL,ICNST)

      external AGCLGEN

!
      integer   nlm
      parameter (nlm=40)

! argument declarations
      INTEGER MXD
      INTEGER NX
      INTEGER NY
 ! floating type, input/output:
      REAL*8 R8_CCLO
  
 ! floating type, input/output:
      REAL*8 R8_CHI
  
 ! floating type, input/output:
      REAL*8 R8_CINC
  
      INTEGER NCL
      INTEGER ICNST
 ! floating type, input/output:
      REAL*8 R8_Z(MXD,NY)
  
 ! floating type, input/output:
      REAL*8 R8_CL(NLM)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)
      REAL :: CCLO_act
      REAL :: CCLO_ref
      REAL :: CHI_act
      REAL :: CHI_ref
      REAL :: CINC_act
      REAL :: CINC_ref
      REAL, allocatable :: CL_act(:)
      REAL, allocatable :: CL_ref(:)

! allocation of working arrays...

      ALLOCATE(Z_act(MXD,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLGEN -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(MXD,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLGEN -- Z_ref ALLOCATE error!')
      ALLOCATE(CL_act(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLGEN -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLGEN -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      Z_act=R8_Z
      Z_ref=R8_Z

      CCLO_act=R8_CCLO
      CCLO_ref=R8_CCLO

      CHI_act=R8_CHI
      CHI_ref=R8_CHI

      CINC_act=R8_CINC
      CINC_ref=R8_CINC

      CL_act=R8_CL
      CL_ref=R8_CL

! call to original routine:  AGCLGEN

      CALL AGCLGEN(
     > Z_act(1,1),MXD,NX,NY,CCLO_act,CHI_act,CINC_act,CL_act(1),NCL,
     > ICNST)

! copy back outputs if modified.

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

      if(CCLO_act.ne.CCLO_ref) then
         R8_CCLO = CCLO_act
      endif

      if(CHI_act.ne.CHI_ref) then
         R8_CHI = CHI_act
      endif

      if(CINC_act.ne.CINC_ref) then
         R8_CINC = CINC_act
      endif

      where(CL_act.ne.CL_ref)
         R8_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCSTLN
      SUBROUTINE R8_AGCSTLN(
     > R8_Z,LL,MM,NN,R8_CONV,IR)

      external AGCSTLN

! argument declarations
      INTEGER LL
      INTEGER MM
      INTEGER NN
 ! floating type, input/output:
      REAL*8 R8_CONV
  
 ! floating type, input/output:
      REAL*8 R8_Z(LL,NN)
  
      INTEGER IR(*)

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)
      REAL :: CONV_act
      REAL :: CONV_ref

! allocation of working arrays...

      ALLOCATE(Z_act(LL,NN)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCSTLN -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(LL,NN)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCSTLN -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      Z_act=R8_Z
      Z_ref=R8_Z

      CONV_act=R8_CONV
      CONV_ref=R8_CONV

! call to original routine:  AGCSTLN

      CALL AGCSTLN(
     > Z_act(1,1),LL,MM,NN,CONV_act,IR)

! copy back outputs if modified.

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

      if(CONV_act.ne.CONV_ref) then
         R8_CONV = CONV_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCDRLN
      SUBROUTINE R8_AGCDRLN(
     > R8_Z,L,MM,NN,IR)

      external AGCDRLN

! argument declarations
      INTEGER L
      INTEGER MM
      INTEGER NN
 ! floating type, input/output:
      REAL*8 R8_Z(L,NN)
  
      INTEGER IR(*)

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)

! allocation of working arrays...

      ALLOCATE(Z_act(L,NN)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCDRLN -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(L,NN)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCDRLN -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      Z_act=R8_Z
      Z_ref=R8_Z

! call to original routine:  AGCDRLN

      CALL AGCDRLN(
     > Z_act(1,1),L,MM,NN,IR)

! copy back outputs if modified.

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCXGET
      SUBROUTINE R8_AGCXGET(
     > R8_XIND,R8_FXX,R8_X,NXS)

      external AGCXGET

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_XIND
  
 ! floating type, input/output:
      REAL*8 R8_FXX
  
      INTEGER NXS
 ! floating type, input/output:
      REAL*8 R8_X(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: XIND_act
      REAL :: XIND_ref
      REAL :: FXX_act
      REAL :: FXX_ref
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)

! allocation of working arrays...

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCXGET -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCXGET -- X_ref ALLOCATE error!')

! executable code:  copy for input

      XIND_act=R8_XIND
      XIND_ref=R8_XIND

      FXX_act=R8_FXX
      FXX_ref=R8_FXX

      X_act=R8_X
      X_ref=R8_X

! call to original routine:  AGCXGET

      CALL AGCXGET(
     > XIND_act,FXX_act,X_act(1),NXS)

! copy back outputs if modified.

      if(XIND_act.ne.XIND_ref) then
         R8_XIND = XIND_act
      endif

      if(FXX_act.ne.FXX_ref) then
         R8_FXX = FXX_act
      endif

      where(X_act.ne.X_ref)
         R8_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCYGET
      SUBROUTINE R8_AGCYGET(
     > R8_YIND,R8_FYY,R8_Y,NYS)

      external AGCYGET

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_YIND
  
 ! floating type, input/output:
      REAL*8 R8_FYY
  
      INTEGER NYS
 ! floating type, input/output:
      REAL*8 R8_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: YIND_act
      REAL :: YIND_ref
      REAL :: FYY_act
      REAL :: FYY_ref
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCYGET -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCYGET -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      YIND_act=R8_YIND
      YIND_ref=R8_YIND

      FYY_act=R8_FYY
      FYY_ref=R8_FYY

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGCYGET

      CALL AGCYGET(
     > YIND_act,FYY_act,Y_act(1),NYS)

! copy back outputs if modified.

      if(YIND_act.ne.YIND_ref) then
         R8_YIND = YIND_act
      endif

      if(FYY_act.ne.FYY_ref) then
         R8_FYY = FYY_act
      endif

      where(Y_act.ne.Y_ref)
         R8_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCFRST
      SUBROUTINE R8_AGCFRST(
     > R8_X,R8_Y)

      external AGCFRST

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref

! allocation of working arrays...


! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGCFRST

      CALL AGCFRST(
     > X_act,Y_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCPLST
      SUBROUTINE R8_AGCPLST(
     > R8_PL1,R8_PL2,KPL,R8_X,R8_Y)

      external AGCPLST

! argument declarations
      INTEGER KPL
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
 ! floating type, input/output:
      REAL*8 R8_PL1(:)
  
 ! floating type, input/output:
      REAL*8 R8_PL2(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: PL1_act(:)
      REAL, allocatable :: PL1_ref(:)
      REAL, allocatable :: PL2_act(:)
      REAL, allocatable :: PL2_ref(:)
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref

! allocation of working arrays...

      dim=size(R8_PL1)
      ALLOCATE(PL1_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCPLST -- PL1_act ALLOCATE error!')
      ALLOCATE(PL1_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCPLST -- PL1_ref ALLOCATE error!')

      dim=size(R8_PL2)
      ALLOCATE(PL2_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCPLST -- PL2_act ALLOCATE error!')
      ALLOCATE(PL2_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCPLST -- PL2_ref ALLOCATE error!')

! executable code:  copy for input

      PL1_act=R8_PL1
      PL1_ref=R8_PL1

      PL2_act=R8_PL2
      PL2_ref=R8_PL2

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGCPLST

      CALL AGCPLST(
     > PL1_act(1),PL2_act(1),KPL,X_act,Y_act)

! copy back outputs if modified.

      where(PL1_act.ne.PL1_ref)
         R8_PL1 = PL1_act
      end where
      DEALLOCATE(PL1_act)
      DEALLOCATE(PL1_ref)

      where(PL2_act.ne.PL2_ref)
         R8_PL2 = PL2_act
      end where
      DEALLOCATE(PL2_act)
      DEALLOCATE(PL2_ref)

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGNCOF
      SUBROUTINE R8_AGNCOF(
     > R8_XCS,R8_YCS,NCS,IAI,IAG,NAI)

      external AGNCOF

! argument declarations
      INTEGER NCS
      INTEGER NAI
 ! floating type, input/output:
      REAL*8 R8_XCS(:)
  
 ! floating type, input/output:
      REAL*8 R8_YCS(:)
  
      INTEGER IAI(*)
      INTEGER IAG(*)

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: XCS_act(:)
      REAL, allocatable :: XCS_ref(:)
      REAL, allocatable :: YCS_act(:)
      REAL, allocatable :: YCS_ref(:)

! allocation of working arrays...

      dim=size(R8_XCS)
      ALLOCATE(XCS_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGNCOF -- XCS_act ALLOCATE error!')
      ALLOCATE(XCS_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGNCOF -- XCS_ref ALLOCATE error!')

      dim=size(R8_YCS)
      ALLOCATE(YCS_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGNCOF -- YCS_act ALLOCATE error!')
      ALLOCATE(YCS_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGNCOF -- YCS_ref ALLOCATE error!')

! executable code:  copy for input

      XCS_act=R8_XCS
      XCS_ref=R8_XCS

      YCS_act=R8_YCS
      YCS_ref=R8_YCS

! call to original routine:  AGNCOF

      CALL AGNCOF(
     > XCS_act(1),YCS_act(1),NCS,IAI,IAG,NAI)

! copy back outputs if modified.

      where(XCS_act.ne.XCS_ref)
         R8_XCS = XCS_act
      end where
      DEALLOCATE(XCS_act)
      DEALLOCATE(XCS_ref)

      where(YCS_act.ne.YCS_ref)
         R8_YCS = YCS_act
      end where
      DEALLOCATE(YCS_act)
      DEALLOCATE(YCS_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCUTRN
      SUBROUTINE R8_AGCUTRN(
     > UFXY,R8_X,R8_Y,R8_FXX,R8_FYY)

      external AGCUTRN

! argument declarations
      INTEGER UFXY
      EXTERNAL UFXY
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
 ! floating type, input/output:
      REAL*8 R8_FXX
  
 ! floating type, input/output:
      REAL*8 R8_FYY
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref
      REAL :: FXX_act
      REAL :: FXX_ref
      REAL :: FYY_act
      REAL :: FYY_ref

! allocation of working arrays...


! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      FXX_act=R8_FXX
      FXX_ref=R8_FXX

      FYY_act=R8_FYY
      FYY_ref=R8_FYY

! call to original routine:  AGCUTRN

      CALL AGCUTRN(
     > UFXY,X_act,Y_act,FXX_act,FYY_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

      if(FXX_act.ne.FXX_ref) then
         R8_FXX = FXX_act
      endif

      if(FYY_act.ne.FYY_ref) then
         R8_FYY = FYY_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  RCONTR
      SUBROUTINE R8_RCONTR(
     > K1,R8_C,K2,R8_A,MAXA,R8_XARRAY,IMIN,IMAX,ISTEP,R8_YARRAY,JMIN,
     > JMAX,JSTEP)

      external RCONTR

! argument declarations
      INTEGER K1
      INTEGER K2
      INTEGER MAXA
      INTEGER IMIN
      INTEGER IMAX
      INTEGER ISTEP
      INTEGER JMIN
      INTEGER JMAX
      INTEGER JSTEP
 ! floating type, input/output:
      REAL*8 R8_C(:)
  
 ! floating type, input/output:
      REAL*8 R8_A(:,:)
  
 ! floating type, input/output:
      REAL*8 R8_XARRAY(:)
  
 ! floating type, input/output:
      REAL*8 R8_YARRAY(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim, dim2
      REAL, allocatable :: C_act(:)
      REAL, allocatable :: C_ref(:)
      REAL, allocatable :: A_act(:,:)
      REAL, allocatable :: A_ref(:,:)
      REAL, allocatable :: XARRAY_act(:)
      REAL, allocatable :: XARRAY_ref(:)
      REAL, allocatable :: YARRAY_act(:)
      REAL, allocatable :: YARRAY_ref(:)

! allocation of working arrays...

      dim=size(R8_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- C_ref ALLOCATE error!')

      dim2=size(R8_A,dim=2)
      ALLOCATE(A_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- A_act ALLOCATE error!')
      ALLOCATE(A_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- A_ref ALLOCATE error!')

      dim=size(R8_XARRAY)
      ALLOCATE(XARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- XARRAY_ref ALLOCATE error!')

      dim=size(R8_YARRAY)
      ALLOCATE(YARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RCONTR -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R8_C
      C_ref=R8_C

      A_act=R8_A
      A_ref=R8_A

      XARRAY_act=R8_XARRAY
      XARRAY_ref=R8_XARRAY

      YARRAY_act=R8_YARRAY
      YARRAY_ref=R8_YARRAY

! call to original routine:  RCONTR

      CALL RCONTR(
     > K1,C_act(1),K2,A_act(1,1),MAXA,XARRAY_act(1),IMIN,IMAX,ISTEP,
     > YARRAY_act(1),JMIN,JMAX,JSTEP)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R8_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(A_act.ne.A_ref)
         R8_A = A_act
      end where
      DEALLOCATE(A_act)
      DEALLOCATE(A_ref)

      where(XARRAY_act.ne.XARRAY_ref)
         R8_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R8_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  CONTUR
      SUBROUTINE R8_CONTUR(
     > K1,R8_C,K2,R8_A,MAXA,R8_XARRAY,IMIN,IMAX,ISTEP,R8_YARRAY,JMIN,
     > JMAX,JSTEP)

      external CONTUR

! argument declarations
      INTEGER K1
      INTEGER K2
      INTEGER MAXA
      INTEGER IMIN
      INTEGER IMAX
      INTEGER ISTEP
      INTEGER JMIN
      INTEGER JMAX
      INTEGER JSTEP
 ! floating type, input/output:
      REAL*8 R8_C(:)
  
 ! floating type, input/output:
      REAL*8 R8_A(:,:)
  
 ! floating type, input/output:
      REAL*8 R8_XARRAY(:,:)
  
 ! floating type, input/output:
      REAL*8 R8_YARRAY(:,:)
  

! local (automatic array) declarations

      integer alloc_stat, dim, dim2
      REAL, allocatable :: C_act(:)
      REAL, allocatable :: C_ref(:)
      REAL, allocatable :: A_act(:,:)
      REAL, allocatable :: A_ref(:,:)
      REAL, allocatable :: XARRAY_act(:,:)
      REAL, allocatable :: XARRAY_ref(:,:)
      REAL, allocatable :: YARRAY_act(:,:)
      REAL, allocatable :: YARRAY_ref(:,:)

! allocation of working arrays...

      dim=size(R8_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- C_ref ALLOCATE error!')

      dim2=size(R8_A,dim=2)
      ALLOCATE(A_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- A_act ALLOCATE error!')
      ALLOCATE(A_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- A_ref ALLOCATE error!')

      dim2=size(R8_XARRAY,dim=2)
      ALLOCATE(XARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- XARRAY_ref ALLOCATE error!')

      dim2=size(R8_YARRAY,dim=2)
      ALLOCATE(YARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CONTUR -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R8_C
      C_ref=R8_C

      A_act=R8_A
      A_ref=R8_A

      XARRAY_act=R8_XARRAY
      XARRAY_ref=R8_XARRAY

      YARRAY_act=R8_YARRAY
      YARRAY_ref=R8_YARRAY

! call to original routine:  CONTUR

      CALL CONTUR(
     > K1,C_act(1),K2,A_act(1,1),MAXA,XARRAY_act(1,1),IMIN,IMAX,ISTEP,
     > YARRAY_act(1,1),JMIN,JMAX,JSTEP)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R8_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(A_act.ne.A_ref)
         R8_A = A_act
      end where
      DEALLOCATE(A_act)
      DEALLOCATE(A_ref)

      where(XARRAY_act.ne.XARRAY_ref)
         R8_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R8_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCLXY
      SUBROUTINE R8_AGCLXY(
     > R8_XAR,NX,IMIN)

      external AGCLXY

! argument declarations
      INTEGER NX
      INTEGER IMIN
 ! floating type, input/output:
      REAL*8 R8_XAR(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: XAR_act(:)
      REAL, allocatable :: XAR_ref(:)

! allocation of working arrays...

      dim=size(R8_XAR)
      ALLOCATE(XAR_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLXY -- XAR_act ALLOCATE error!')
      ALLOCATE(XAR_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCLXY -- XAR_ref ALLOCATE error!')

! executable code:  copy for input

      XAR_act=R8_XAR
      XAR_ref=R8_XAR

! call to original routine:  AGCLXY

      CALL AGCLXY(
     > XAR_act(1),NX,IMIN)

! copy back outputs if modified.

      where(XAR_act.ne.XAR_ref)
         R8_XAR = XAR_act
      end where
      DEALLOCATE(XAR_act)
      DEALLOCATE(XAR_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCUSET
      SUBROUTINE R8_AGCUSET(
     > R8_XARRAY,R8_YARRAY,MAXA,NX,NY)

      external AGCUSET

! argument declarations
      INTEGER MAXA
      INTEGER NX
      INTEGER NY
 ! floating type, input/output:
      REAL*8 R8_XARRAY(:,:)
  
 ! floating type, input/output:
      REAL*8 R8_YARRAY(:,:)
  

! local (automatic array) declarations

      integer alloc_stat, dim2
      REAL, allocatable :: XARRAY_act(:,:)
      REAL, allocatable :: XARRAY_ref(:,:)
      REAL, allocatable :: YARRAY_act(:,:)
      REAL, allocatable :: YARRAY_ref(:,:)

! allocation of working arrays...

      dim2=size(R8_XARRAY,dim=2)
      ALLOCATE(XARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUSET -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUSET -- XARRAY_ref ALLOCATE error!')

      dim2=size(R8_YARRAY,dim=2)
      ALLOCATE(YARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUSET -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUSET -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      XARRAY_act=R8_XARRAY
      XARRAY_ref=R8_XARRAY

      YARRAY_act=R8_YARRAY
      YARRAY_ref=R8_YARRAY

! call to original routine:  AGCUSET

      CALL AGCUSET(
     > XARRAY_act(1,1),YARRAY_act(1,1),MAXA,NX,NY)

! copy back outputs if modified.

      where(XARRAY_act.ne.XARRAY_ref)
         R8_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R8_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCUXYI
      SUBROUTINE R8_AGCUXYI(
     > R8_XARRAY,R8_YARRAY,MAXA,NX,NY,R8_XRAW,R8_YRAW,R8_XDAT,R8_YDAT)

      external AGCUXYI

! argument declarations
      INTEGER MAXA
      INTEGER NX
      INTEGER NY
 ! floating type, input/output:
      REAL*8 R8_XRAW
  
 ! floating type, input/output:
      REAL*8 R8_YRAW
  
 ! floating type, input/output:
      REAL*8 R8_XDAT
  
 ! floating type, input/output:
      REAL*8 R8_YDAT
  
 ! floating type, input/output:
      REAL*8 R8_XARRAY(MAXA,NX)
  
 ! floating type, input/output:
      REAL*8 R8_YARRAY(MAXA,NY)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: XARRAY_act(:,:)
      REAL, allocatable :: XARRAY_ref(:,:)
      REAL, allocatable :: YARRAY_act(:,:)
      REAL, allocatable :: YARRAY_ref(:,:)
      REAL :: XRAW_act
      REAL :: XRAW_ref
      REAL :: YRAW_act
      REAL :: YRAW_ref
      REAL :: XDAT_act
      REAL :: XDAT_ref
      REAL :: YDAT_act
      REAL :: YDAT_ref

! allocation of working arrays...

      ALLOCATE(XARRAY_act(MAXA,NX)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUXYI -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(MAXA,NX)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUXYI -- XARRAY_ref ALLOCATE error!')
      ALLOCATE(YARRAY_act(MAXA,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUXYI -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(MAXA,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCUXYI -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      XARRAY_act=R8_XARRAY
      XARRAY_ref=R8_XARRAY

      YARRAY_act=R8_YARRAY
      YARRAY_ref=R8_YARRAY

      XRAW_act=R8_XRAW
      XRAW_ref=R8_XRAW

      YRAW_act=R8_YRAW
      YRAW_ref=R8_YRAW

      XDAT_act=R8_XDAT
      XDAT_ref=R8_XDAT

      YDAT_act=R8_YDAT
      YDAT_ref=R8_YDAT

! call to original routine:  AGCUXYI

      CALL AGCUXYI(
     > XARRAY_act(1,1),YARRAY_act(1,1),MAXA,NX,NY,XRAW_act,YRAW_act,
     > XDAT_act,YDAT_act)

! copy back outputs if modified.

      where(XARRAY_act.ne.XARRAY_ref)
         R8_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R8_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

      if(XRAW_act.ne.XRAW_ref) then
         R8_XRAW = XRAW_act
      endif

      if(YRAW_act.ne.YRAW_ref) then
         R8_YRAW = YRAW_act
      endif

      if(XDAT_act.ne.XDAT_ref) then
         R8_XDAT = XDAT_act
      endif

      if(YDAT_act.ne.YDAT_ref) then
         R8_YDAT = YDAT_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCULEV
      SUBROUTINE R8_AGCULEV(
     > K1,R8_C,K2,R8_ZAR,NZ,R8_CL,NCL)

      external AGCULEV

! argument declarations
      INTEGER K1
      INTEGER K2
      INTEGER NZ
      INTEGER NCL
 ! floating type, input/output:
      REAL*8 R8_C(:)
  
 ! floating type, input/output:
      REAL*8 R8_ZAR(:)
  
 ! floating type, input/output:
      REAL*8 R8_CL(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: C_act(:)
      REAL, allocatable :: C_ref(:)
      REAL, allocatable :: ZAR_act(:)
      REAL, allocatable :: ZAR_ref(:)
      REAL, allocatable :: CL_act(:)
      REAL, allocatable :: CL_ref(:)

! allocation of working arrays...

      dim=size(R8_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- C_ref ALLOCATE error!')

      dim=size(R8_ZAR)
      ALLOCATE(ZAR_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- ZAR_act ALLOCATE error!')
      ALLOCATE(ZAR_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- ZAR_ref ALLOCATE error!')

      dim=size(R8_CL)
      ALLOCATE(CL_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCULEV -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R8_C
      C_ref=R8_C

      ZAR_act=R8_ZAR
      ZAR_ref=R8_ZAR

      CL_act=R8_CL
      CL_ref=R8_CL

! call to original routine:  AGCULEV

      CALL AGCULEV(
     > K1,C_act(1),K2,ZAR_act(1),NZ,CL_act(1),NCL)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R8_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(ZAR_act.ne.ZAR_ref)
         R8_ZAR = ZAR_act
      end where
      DEALLOCATE(ZAR_act)
      DEALLOCATE(ZAR_ref)

      where(CL_act.ne.CL_ref)
         R8_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGRAPH
      SUBROUTINE R8_AGRAPH(
     > MINX,MAXX,MINY,MAXY,R8_XMIN,R8_XMAX,R8_YMIN,R8_YMAX,XLABL,YLABL)

      external AGRAPH

! argument declarations
      INTEGER MINX
      INTEGER MAXX
      INTEGER MINY
      INTEGER MAXY
 ! floating type, input/output:
      REAL*8 R8_XMIN
  
 ! floating type, input/output:
      REAL*8 R8_XMAX
  
 ! floating type, input/output:
      REAL*8 R8_YMIN
  
 ! floating type, input/output:
      REAL*8 R8_YMAX
  
      CHARACTER*(*) XLABL
      CHARACTER*(*) YLABL

! local (automatic array) declarations

      integer alloc_stat
      REAL :: XMIN_act
      REAL :: XMIN_ref
      REAL :: XMAX_act
      REAL :: XMAX_ref
      REAL :: YMIN_act
      REAL :: YMIN_ref
      REAL :: YMAX_act
      REAL :: YMAX_ref

! allocation of working arrays...


! executable code:  copy for input

      XMIN_act=R8_XMIN
      XMIN_ref=R8_XMIN

      XMAX_act=R8_XMAX
      XMAX_ref=R8_XMAX

      YMIN_act=R8_YMIN
      YMIN_ref=R8_YMIN

      YMAX_act=R8_YMAX
      YMAX_ref=R8_YMAX

! call to original routine:  AGRAPH

      CALL AGRAPH(
     > MINX,MAXX,MINY,MAXY,XMIN_act,XMAX_act,YMIN_act,YMAX_act,XLABL,
     > YLABL)

! copy back outputs if modified.

      if(XMIN_act.ne.XMIN_ref) then
         R8_XMIN = XMIN_act
      endif

      if(XMAX_act.ne.XMAX_ref) then
         R8_XMAX = XMAX_act
      endif

      if(YMIN_act.ne.YMIN_ref) then
         R8_YMIN = YMIN_act
      endif

      if(YMAX_act.ne.YMAX_ref) then
         R8_YMAX = YMAX_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGRCFR
      SUBROUTINE R8_AGRCFR(
     > R8_X,IMIN,IMAX,ISTEP,R8_Y,JMIN,JMAX,JSTEP,IGRID)

      external AGRCFR

! argument declarations
      INTEGER IMIN
      INTEGER IMAX
      INTEGER ISTEP
      INTEGER JMIN
      INTEGER JMAX
      INTEGER JSTEP
      INTEGER IGRID
 ! floating type, input/output:
      REAL*8 R8_X(:)
  
 ! floating type, input/output:
      REAL*8 R8_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGRCFR -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGRCFR -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGRCFR -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGRCFR -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGRCFR

      CALL AGRCFR(
     > X_act(1),IMIN,IMAX,ISTEP,Y_act(1),JMIN,JMAX,JSTEP,IGRID)

! copy back outputs if modified.

      where(X_act.ne.X_ref)
         R8_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R8_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGCFRAM
      SUBROUTINE R8_AGCFRAM(
     > R8_X,NX,R8_Y,NY,IGRID)

      external AGCFRAM

! argument declarations
      INTEGER NX
      INTEGER NY
      INTEGER IGRID
 ! floating type, input/output:
      REAL*8 R8_X(:)
  
 ! floating type, input/output:
      REAL*8 R8_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCFRAM -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCFRAM -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCFRAM -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGCFRAM -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGCFRAM

      CALL AGCFRAM(
     > X_act(1),NX,Y_act(1),NY,IGRID)

! copy back outputs if modified.

      where(X_act.ne.X_ref)
         R8_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R8_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end


