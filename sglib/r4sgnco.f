!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCLINS
      SUBROUTINE R4_AGCLINS(
     > R4_ZDATA,MXD,R4_X,NX,R4_Y,NY,R4_CL,NCL)

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
      REAL R4_ZDATA(:,:)
  
 ! floating type, input/output:
      REAL R4_X(:)
  
 ! floating type, input/output:
      REAL R4_Y(:)
  
 ! floating type, input/output:
      REAL R4_CL(NLM)
  

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

      dim2=size(R4_ZDATA,dim=2)
      ALLOCATE(ZDATA_act(MXD,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- ZDATA_act ALLOCATE error!')
      ALLOCATE(ZDATA_ref(MXD,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- ZDATA_ref ALLOCATE error!')

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- Y_ref ALLOCATE error!')

      ALLOCATE(CL_act(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(NLM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLINS -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      ZDATA_act=R4_ZDATA
      ZDATA_ref=R4_ZDATA

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

      CL_act=R4_CL
      CL_ref=R4_CL

! call to original routine:  AGCLINS

      CALL AGCLINS(
     > ZDATA_act(1,1),MXD,X_act(1),NX,Y_act(1),NY,CL_act(1),NCL)

! copy back outputs if modified.

      where(ZDATA_act.ne.ZDATA_ref)
         R4_ZDATA = ZDATA_act
      end where
      DEALLOCATE(ZDATA_act)
      DEALLOCATE(ZDATA_ref)

      where(X_act.ne.X_ref)
         R4_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R4_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

      where(CL_act.ne.CL_ref)
         R4_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
! r4real -- generated (f90 fixed form) REAL interface to:  AGCXGET
      SUBROUTINE R4_AGCXGET(
     > R4_XIND,R4_FXX,R4_X,NXS)

      external AGCXGET

! argument declarations
 ! floating type, input/output:
      REAL R4_XIND
  
 ! floating type, input/output:
      REAL R4_FXX
  
      INTEGER NXS
 ! floating type, input/output:
      REAL R4_X(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: XIND_act
      REAL :: XIND_ref
      REAL :: FXX_act
      REAL :: FXX_ref
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)

! allocation of working arrays...

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCXGET -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCXGET -- X_ref ALLOCATE error!')

! executable code:  copy for input

      XIND_act=R4_XIND
      XIND_ref=R4_XIND

      FXX_act=R4_FXX
      FXX_ref=R4_FXX

      X_act=R4_X
      X_ref=R4_X

! call to original routine:  AGCXGET

      CALL AGCXGET(
     > XIND_act,FXX_act,X_act(1),NXS)

! copy back outputs if modified.

      if(XIND_act.ne.XIND_ref) then
         R4_XIND = XIND_act
      endif

      if(FXX_act.ne.FXX_ref) then
         R4_FXX = FXX_act
      endif

      where(X_act.ne.X_ref)
         R4_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCYGET
      SUBROUTINE R4_AGCYGET(
     > R4_YIND,R4_FYY,R4_Y,NYS)

      external AGCYGET

! argument declarations
 ! floating type, input/output:
      REAL R4_YIND
  
 ! floating type, input/output:
      REAL R4_FYY
  
      INTEGER NYS
 ! floating type, input/output:
      REAL R4_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: YIND_act
      REAL :: YIND_ref
      REAL :: FYY_act
      REAL :: FYY_ref
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCYGET -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCYGET -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      YIND_act=R4_YIND
      YIND_ref=R4_YIND

      FYY_act=R4_FYY
      FYY_ref=R4_FYY

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGCYGET

      CALL AGCYGET(
     > YIND_act,FYY_act,Y_act(1),NYS)

! copy back outputs if modified.

      if(YIND_act.ne.YIND_ref) then
         R4_YIND = YIND_act
      endif

      if(FYY_act.ne.FYY_ref) then
         R4_FYY = FYY_act
      endif

      where(Y_act.ne.Y_ref)
         R4_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end
! r4real -- generated (f90 fixed form) REAL interface to:  AGCPLST
      SUBROUTINE R4_AGCPLST(
     > R4_PL1,R4_PL2,KPL,R4_X,R4_Y)

      external AGCPLST

! argument declarations
      INTEGER KPL
 ! floating type, input/output:
      REAL R4_X
  
 ! floating type, input/output:
      REAL R4_Y
  
 ! floating type, input/output:
      REAL R4_PL1(:)
  
 ! floating type, input/output:
      REAL R4_PL2(:)
  

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

      dim=size(R4_PL1)
      ALLOCATE(PL1_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCPLST -- PL1_act ALLOCATE error!')
      ALLOCATE(PL1_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCPLST -- PL1_ref ALLOCATE error!')

      dim=size(R4_PL2)
      ALLOCATE(PL2_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCPLST -- PL2_act ALLOCATE error!')
      ALLOCATE(PL2_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCPLST -- PL2_ref ALLOCATE error!')

! executable code:  copy for input

      PL1_act=R4_PL1
      PL1_ref=R4_PL1

      PL2_act=R4_PL2
      PL2_ref=R4_PL2

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGCPLST

      CALL AGCPLST(
     > PL1_act(1),PL2_act(1),KPL,X_act,Y_act)

! copy back outputs if modified.

      where(PL1_act.ne.PL1_ref)
         R4_PL1 = PL1_act
      end where
      DEALLOCATE(PL1_act)
      DEALLOCATE(PL1_ref)

      where(PL2_act.ne.PL2_ref)
         R4_PL2 = PL2_act
      end where
      DEALLOCATE(PL2_act)
      DEALLOCATE(PL2_ref)

      if(X_act.ne.X_ref) then
         R4_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R4_Y = Y_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGNCOF
      SUBROUTINE R4_AGNCOF(
     > R4_XCS,R4_YCS,NCS,IAI,IAG,NAI)

      external AGNCOF

! argument declarations
      INTEGER NCS
      INTEGER NAI
 ! floating type, input/output:
      REAL R4_XCS(:)
  
 ! floating type, input/output:
      REAL R4_YCS(:)
  
      INTEGER IAI(*)
      INTEGER IAG(*)

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: XCS_act(:)
      REAL, allocatable :: XCS_ref(:)
      REAL, allocatable :: YCS_act(:)
      REAL, allocatable :: YCS_ref(:)

! allocation of working arrays...

      dim=size(R4_XCS)
      ALLOCATE(XCS_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGNCOF -- XCS_act ALLOCATE error!')
      ALLOCATE(XCS_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGNCOF -- XCS_ref ALLOCATE error!')

      dim=size(R4_YCS)
      ALLOCATE(YCS_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGNCOF -- YCS_act ALLOCATE error!')
      ALLOCATE(YCS_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGNCOF -- YCS_ref ALLOCATE error!')

! executable code:  copy for input

      XCS_act=R4_XCS
      XCS_ref=R4_XCS

      YCS_act=R4_YCS
      YCS_ref=R4_YCS

! call to original routine:  AGNCOF

      CALL AGNCOF(
     > XCS_act(1),YCS_act(1),NCS,IAI,IAG,NAI)

! copy back outputs if modified.

      where(XCS_act.ne.XCS_ref)
         R4_XCS = XCS_act
      end where
      DEALLOCATE(XCS_act)
      DEALLOCATE(XCS_ref)

      where(YCS_act.ne.YCS_ref)
         R4_YCS = YCS_act
      end where
      DEALLOCATE(YCS_act)
      DEALLOCATE(YCS_ref)

! exit
      return
      end
!---------------------------------------------------------------------------
      SUBROUTINE R4_RCONTR(
     > K1,R4_C,K2,R4_A,MAXA,R4_XARRAY,IMIN,IMAX,ISTEP,R4_YARRAY,JMIN,
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
      REAL R4_C(:)
  
 ! floating type, input/output:
      REAL R4_A(:,:)
  
 ! floating type, input/output:
      REAL R4_XARRAY(:)
  
 ! floating type, input/output:
      REAL R4_YARRAY(:)
  

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

      dim=size(R4_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- C_ref ALLOCATE error!')

      dim2=size(R4_A,dim=2)
      ALLOCATE(A_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- A_act ALLOCATE error!')
      ALLOCATE(A_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- A_ref ALLOCATE error!')

      dim=size(R4_XARRAY)
      ALLOCATE(XARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- XARRAY_ref ALLOCATE error!')

      dim=size(R4_YARRAY)
      ALLOCATE(YARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_RCONTR -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R4_C
      C_ref=R4_C

      A_act=R4_A
      A_ref=R4_A

      XARRAY_act=R4_XARRAY
      XARRAY_ref=R4_XARRAY

      YARRAY_act=R4_YARRAY
      YARRAY_ref=R4_YARRAY

! call to original routine:  RCONTR

      CALL RCONTR(
     > K1,C_act(1),K2,A_act(1,1),MAXA,XARRAY_act(1),IMIN,IMAX,ISTEP,
     > YARRAY_act(1),JMIN,JMAX,JSTEP)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R4_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(A_act.ne.A_ref)
         R4_A = A_act
      end where
      DEALLOCATE(A_act)
      DEALLOCATE(A_ref)

      where(XARRAY_act.ne.XARRAY_ref)
         R4_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R4_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  CONTUR
      SUBROUTINE R4_CONTUR(
     > K1,R4_C,K2,R4_A,MAXA,R4_XARRAY,IMIN,IMAX,ISTEP,R4_YARRAY,JMIN,
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
      REAL R4_C(:)
  
 ! floating type, input/output:
      REAL R4_A(:,:)
  
 ! floating type, input/output:
      REAL R4_XARRAY(:,:)
  
 ! floating type, input/output:
      REAL R4_YARRAY(:,:)
  

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

      dim=size(R4_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- C_ref ALLOCATE error!')

      dim2=size(R4_A,dim=2)
      ALLOCATE(A_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- A_act ALLOCATE error!')
      ALLOCATE(A_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- A_ref ALLOCATE error!')

      dim2=size(R4_XARRAY,dim=2)
      ALLOCATE(XARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- XARRAY_ref ALLOCATE error!')

      dim2=size(R4_YARRAY,dim=2)
      ALLOCATE(YARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CONTUR -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R4_C
      C_ref=R4_C

      A_act=R4_A
      A_ref=R4_A

      XARRAY_act=R4_XARRAY
      XARRAY_ref=R4_XARRAY

      YARRAY_act=R4_YARRAY
      YARRAY_ref=R4_YARRAY

! call to original routine:  CONTUR

      CALL CONTUR(
     > K1,C_act(1),K2,A_act(1,1),MAXA,XARRAY_act(1,1),IMIN,IMAX,ISTEP,
     > YARRAY_act(1,1),JMIN,JMAX,JSTEP)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R4_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(A_act.ne.A_ref)
         R4_A = A_act
      end where
      DEALLOCATE(A_act)
      DEALLOCATE(A_ref)

      where(XARRAY_act.ne.XARRAY_ref)
         R4_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R4_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCLXY
      SUBROUTINE R4_AGCLXY(
     > R4_XAR,NX,IMIN)

      external AGCLXY

! argument declarations
      INTEGER NX
      INTEGER IMIN
 ! floating type, input/output:
      REAL R4_XAR(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: XAR_act(:)
      REAL, allocatable :: XAR_ref(:)

! allocation of working arrays...

      dim=size(R4_XAR)
      ALLOCATE(XAR_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLXY -- XAR_act ALLOCATE error!')
      ALLOCATE(XAR_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCLXY -- XAR_ref ALLOCATE error!')

! executable code:  copy for input

      XAR_act=R4_XAR
      XAR_ref=R4_XAR

! call to original routine:  AGCLXY

      CALL AGCLXY(
     > XAR_act(1),NX,IMIN)

! copy back outputs if modified.

      where(XAR_act.ne.XAR_ref)
         R4_XAR = XAR_act
      end where
      DEALLOCATE(XAR_act)
      DEALLOCATE(XAR_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCUSET
      SUBROUTINE R4_AGCUSET(
     > R4_XARRAY,R4_YARRAY,MAXA,NX,NY)

      external AGCUSET

! argument declarations
      INTEGER MAXA
      INTEGER NX
      INTEGER NY
 ! floating type, input/output:
      REAL R4_XARRAY(:,:)
  
 ! floating type, input/output:
      REAL R4_YARRAY(:,:)
  

! local (automatic array) declarations

      integer alloc_stat, dim2
      REAL, allocatable :: XARRAY_act(:,:)
      REAL, allocatable :: XARRAY_ref(:,:)
      REAL, allocatable :: YARRAY_act(:,:)
      REAL, allocatable :: YARRAY_ref(:,:)

! allocation of working arrays...

      dim2=size(R4_XARRAY,dim=2)
      ALLOCATE(XARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCUSET -- XARRAY_act ALLOCATE error!')
      ALLOCATE(XARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCUSET -- XARRAY_ref ALLOCATE error!')

      dim2=size(R4_YARRAY,dim=2)
      ALLOCATE(YARRAY_act(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCUSET -- YARRAY_act ALLOCATE error!')
      ALLOCATE(YARRAY_ref(MAXA,dim2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCUSET -- YARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      XARRAY_act=R4_XARRAY
      XARRAY_ref=R4_XARRAY

      YARRAY_act=R4_YARRAY
      YARRAY_ref=R4_YARRAY

! call to original routine:  AGCUSET

      CALL AGCUSET(
     > XARRAY_act(1,1),YARRAY_act(1,1),MAXA,NX,NY)

! copy back outputs if modified.

      where(XARRAY_act.ne.XARRAY_ref)
         R4_XARRAY = XARRAY_act
      end where
      DEALLOCATE(XARRAY_act)
      DEALLOCATE(XARRAY_ref)

      where(YARRAY_act.ne.YARRAY_ref)
         R4_YARRAY = YARRAY_act
      end where
      DEALLOCATE(YARRAY_act)
      DEALLOCATE(YARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCULEV
      SUBROUTINE R4_AGCULEV(
     > K1,R4_C,K2,R4_ZAR,NZ,R4_CL,NCL)

      external AGCULEV

! argument declarations
      INTEGER K1
      INTEGER K2
      INTEGER NZ
      INTEGER NCL
 ! floating type, input/output:
      REAL R4_C(:)
  
 ! floating type, input/output:
      REAL R4_ZAR(:)
  
 ! floating type, input/output:
      REAL R4_CL(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: C_act(:)
      REAL, allocatable :: C_ref(:)
      REAL, allocatable :: ZAR_act(:)
      REAL, allocatable :: ZAR_ref(:)
      REAL, allocatable :: CL_act(:)
      REAL, allocatable :: CL_ref(:)

! allocation of working arrays...

      dim=size(R4_C)
      ALLOCATE(C_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- C_ref ALLOCATE error!')

      dim=size(R4_ZAR)
      ALLOCATE(ZAR_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- ZAR_act ALLOCATE error!')
      ALLOCATE(ZAR_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- ZAR_ref ALLOCATE error!')

      dim=size(R4_CL)
      ALLOCATE(CL_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCULEV -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R4_C
      C_ref=R4_C

      ZAR_act=R4_ZAR
      ZAR_ref=R4_ZAR

      CL_act=R4_CL
      CL_ref=R4_CL

! call to original routine:  AGCULEV

      CALL AGCULEV(
     > K1,C_act(1),K2,ZAR_act(1),NZ,CL_act(1),NCL)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R4_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

      where(ZAR_act.ne.ZAR_ref)
         R4_ZAR = ZAR_act
      end where
      DEALLOCATE(ZAR_act)
      DEALLOCATE(ZAR_ref)

      where(CL_act.ne.CL_ref)
         R4_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGRCFR
      SUBROUTINE R4_AGRCFR(
     > R4_X,IMIN,IMAX,ISTEP,R4_Y,JMIN,JMAX,JSTEP,IGRID)

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
      REAL R4_X(:)
  
 ! floating type, input/output:
      REAL R4_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGRCFR -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGRCFR -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGRCFR -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGRCFR -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGRCFR

      CALL AGRCFR(
     > X_act(1),IMIN,IMAX,ISTEP,Y_act(1),JMIN,JMAX,JSTEP,IGRID)

! copy back outputs if modified.

      where(X_act.ne.X_ref)
         R4_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R4_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGCFRAM
      SUBROUTINE R4_AGCFRAM(
     > R4_X,NX,R4_Y,NY,IGRID)

      external AGCFRAM

! argument declarations
      INTEGER NX
      INTEGER NY
      INTEGER IGRID
 ! floating type, input/output:
      REAL R4_X(:)
  
 ! floating type, input/output:
      REAL R4_Y(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)

! allocation of working arrays...

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCFRAM -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCFRAM -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCFRAM -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGCFRAM -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGCFRAM

      CALL AGCFRAM(
     > X_act(1),NX,Y_act(1),NY,IGRID)

! copy back outputs if modified.

      where(X_act.ne.X_ref)
         R4_X = X_act
      end where
      DEALLOCATE(X_act)
      DEALLOCATE(X_ref)

      where(Y_act.ne.Y_ref)
         R4_Y = Y_act
      end where
      DEALLOCATE(Y_act)
      DEALLOCATE(Y_ref)

! exit
      return
      end




