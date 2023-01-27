      SUBROUTINE R8_AGIDLX(
     > R8_XMIN,R8_XMAX)

      external AGIDLX

 ! floating type, input/output:
      REAL*8 R8_XMIN
      REAL*8 R8_XMAX

      REAL :: XMIN_act
      REAL :: XMIN_ref
      REAL :: XMAX_act
      REAL :: XMAX_ref

      XMIN_act=R8_XMIN
      XMIN_ref=R8_XMIN
      XMAX_act=R8_XMAX
      XMAX_ref=R8_XMAX

      call agidlx(XMIN_act,XMAX_act)
      if (XMIN_act .ne. XMIN_ref) R8_XMIN=XMIN_act
      if (XMAX_act .ne. XMAX_ref) R8_XMAX=XMAX_act
      return
      end

      SUBROUTINE R8_AGIDLY(
     > R8_YMIN,R8_YMAX)

      external AGIDLY

 ! floating type, input/output:
      REAL*8 R8_YMIN
      REAL*8 R8_YMAX

      REAL :: YMIN_act
      REAL :: YMIN_ref
      REAL :: YMAX_act
      REAL :: YMAX_ref

      YMIN_act=R8_YMIN
      YMIN_ref=R8_YMIN
      YMAX_act=R8_YMAX
      YMAX_ref=R8_YMAX

      call agidly(YMIN_act,YMAX_act)
      if (YMIN_act .ne. YMIN_ref) R8_YMIN=YMIN_act
      if (YMAX_act .ne. YMAX_ref) R8_YMAX=YMAX_act
      return
      end

      SUBROUTINE R8_AGIDLZ(
     > R8_ZMIN,R8_ZMAX)

      external AGIDLZ

 ! floating type, input/output:
      REAL*8 R8_ZMIN
      REAL*8 R8_ZMAX

      REAL :: ZMIN_act
      REAL :: ZMIN_ref
      REAL :: ZMAX_act
      REAL :: ZMAX_ref

      ZMIN_act=R8_ZMIN
      ZMIN_ref=R8_ZMIN
      ZMAX_act=R8_ZMAX
      ZMAX_ref=R8_ZMAX

      call agidlz(ZMIN_act,ZMAX_act)
      if (ZMIN_act .ne. ZMIN_ref) R8_ZMIN=ZMIN_act
      if (ZMAX_act .ne. ZMAX_ref) R8_ZMAX=ZMAX_act
      return
      end

!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGISOC

      SUBROUTINE R8_AGISOC(
     > R8_X,R8_Y,R8_Z,NXDIM,NX,NY,XU,YU,ZU,M)

      external AGISOC

! argument declarations
      INTEGER NXDIM
      INTEGER NX
      INTEGER NY
      CHARACTER*(*) XU
      CHARACTER*(*) YU
      CHARACTER*(*) ZU
 ! floating type, input/output:
      REAL*8 R8_X(NXDIM)
  
 ! floating type, input/output:
      REAL*8 R8_Y(NY)
  
 ! floating type, input/output:
      REAL*8 R8_Z(NXDIM,NY)
  
      INTEGER M(2,NX,NY)

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)

! allocation of working arrays...

      ALLOCATE(X_act(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- X_ref ALLOCATE error!')
      ALLOCATE(Y_act(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- Y_ref ALLOCATE error!')
      ALLOCATE(Z_act(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOC -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      Z_act=R8_Z
      Z_ref=R8_Z

! call to original routine:  AGISOC

      CALL AGISOC(
     > X_act(1),Y_act(1),Z_act(1,1),NXDIM,NX,NY,XU,YU,ZU,M)

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

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGISOM
      SUBROUTINE R8_AGISOM(
     > R8_X,R8_Y,R8_Z,NXDIM,NX,NY,XU,YU,ZU)

      external AGISOM

! argument declarations
      INTEGER NXDIM
      INTEGER NX
      INTEGER NY
      CHARACTER*(*) XU
      CHARACTER*(*) YU
      CHARACTER*(*) ZU
 ! floating type, input/output:
      REAL*8 R8_X(NXDIM)
  
 ! floating type, input/output:
      REAL*8 R8_Y(NY)
  
 ! floating type, input/output:
      REAL*8 R8_Z(NXDIM,NY)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)

! allocation of working arrays...

      ALLOCATE(X_act(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- X_ref ALLOCATE error!')
      ALLOCATE(Y_act(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- Y_ref ALLOCATE error!')
      ALLOCATE(Z_act(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISOM -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      Z_act=R8_Z
      Z_ref=R8_Z

! call to original routine:  AGISOM

      CALL AGISOM(
     > X_act(1),Y_act(1),Z_act(1,1),NXDIM,NX,NY,XU,YU,ZU)

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

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGISH
      SUBROUTINE R8_AGISH(
     > R8_X,R8_Y,R8_Z,NXDIM,NX,NY)

      external AGISH

! argument declarations
      INTEGER NXDIM
      INTEGER NX
      INTEGER NY
 ! floating type, input/output:
      REAL*8 R8_X(NXDIM)
  
 ! floating type, input/output:
      REAL*8 R8_Y(NY)
  
 ! floating type, input/output:
      REAL*8 R8_Z(NXDIM,NY)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)

! allocation of working arrays...

      ALLOCATE(X_act(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- X_ref ALLOCATE error!')
      ALLOCATE(Y_act(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- Y_ref ALLOCATE error!')
      ALLOCATE(Z_act(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISH -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      Z_act=R8_Z
      Z_ref=R8_Z

! call to original routine:  AGISH

      CALL AGISH(
     > X_act(1),Y_act(1),Z_act(1,1),NXDIM,NX,NY)

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

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGISO0
      SUBROUTINE R8_AGISO0(
     > R8_X,R8_Y,R8_Z,NXDIM,NX,NY,XU,YU,ZU)

      external AGISO0

! argument declarations
      INTEGER NXDIM
      INTEGER NX
      INTEGER NY
      CHARACTER*(*) XU
      CHARACTER*(*) YU
      CHARACTER*(*) ZU
 ! floating type, input/output:
      REAL*8 R8_X(NXDIM)
  
 ! floating type, input/output:
      REAL*8 R8_Y(NY)
  
 ! floating type, input/output:
      REAL*8 R8_Z(NXDIM,NY)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)

! allocation of working arrays...

      ALLOCATE(X_act(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(NXDIM)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- X_ref ALLOCATE error!')
      ALLOCATE(Y_act(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- Y_ref ALLOCATE error!')
      ALLOCATE(Z_act(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(NXDIM,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGISO0 -- Z_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      Z_act=R8_Z
      Z_ref=R8_Z

! call to original routine:  AGISO0

      CALL AGISO0(
     > X_act(1),Y_act(1),Z_act(1,1),NXDIM,NX,NY,XU,YU,ZU)

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

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGIGZC
      SUBROUTINE R8_AGIGZC(
     > NC,R8_C,LVLTIC)

      external AGIGZC

! argument declarations
      INTEGER NC
 ! floating type, input/output:
      REAL*8 R8_C(1)
  
      LOGICAL LVLTIC(1)

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: C_act(:)
      REAL, allocatable :: C_ref(:)

! allocation of working arrays...

      ALLOCATE(C_act(1)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIGZC -- C_act ALLOCATE error!')
      ALLOCATE(C_ref(1)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIGZC -- C_ref ALLOCATE error!')

! executable code:  copy for input

      C_act=R8_C
      C_ref=R8_C

! call to original routine:  AGIGZC

      CALL AGIGZC(
     > NC,C_act(1),LVLTIC)

! copy back outputs if modified.

      where(C_act.ne.C_ref)
         R8_C = C_act
      end where
      DEALLOCATE(C_act)
      DEALLOCATE(C_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGICAU
      SUBROUTINE R8_AGICAU(
     > R8_XXCOOR,R8_YYCOOR,R8_ZZCOOR,IXS,IZS)

      external AGICAU

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_XXCOOR
  
 ! floating type, input/output:
      REAL*8 R8_YYCOOR
  
 ! floating type, input/output:
      REAL*8 R8_ZZCOOR
  
      INTEGER IXS
      INTEGER IZS

! local (automatic array) declarations

      integer alloc_stat
      REAL :: XXCOOR_act
      REAL :: XXCOOR_ref
      REAL :: YYCOOR_act
      REAL :: YYCOOR_ref
      REAL :: ZZCOOR_act
      REAL :: ZZCOOR_ref

! allocation of working arrays...


! executable code:  copy for input

      XXCOOR_act=R8_XXCOOR
      XXCOOR_ref=R8_XXCOOR

      YYCOOR_act=R8_YYCOOR
      YYCOOR_ref=R8_YYCOOR

      ZZCOOR_act=R8_ZZCOOR
      ZZCOOR_ref=R8_ZZCOOR

! call to original routine:  AGICAU

      CALL AGICAU(
     > XXCOOR_act,YYCOOR_act,ZZCOOR_act,IXS,IZS)

! copy back outputs if modified.

      if(XXCOOR_act.ne.XXCOOR_ref) then
         R8_XXCOOR = XXCOOR_act
      endif

      if(YYCOOR_act.ne.YYCOOR_ref) then
         R8_YYCOOR = YYCOOR_act
      endif

      if(ZZCOOR_act.ne.ZZCOOR_ref) then
         R8_ZZCOOR = ZZCOOR_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGIDZC
      SUBROUTINE R8_AGIDZC(
     > R8_Z,MX,NX,NY,M,I0,J0,R8_CL,NCL)

      external AGIDZC

! argument declarations
      INTEGER MX
      INTEGER NX
      INTEGER NY
      INTEGER I0
      INTEGER J0
      INTEGER NCL
 ! floating type, input/output:
      REAL*8 R8_Z(MX,NY)
  
      INTEGER M(2,NX,NY)
 ! floating type, input/output:
      REAL*8 R8_CL(NCL)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: Z_act(:,:)
      REAL, allocatable :: Z_ref(:,:)
      REAL, allocatable :: CL_act(:)
      REAL, allocatable :: CL_ref(:)

! allocation of working arrays...

      ALLOCATE(Z_act(MX,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIDZC -- Z_act ALLOCATE error!')
      ALLOCATE(Z_ref(MX,NY)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIDZC -- Z_ref ALLOCATE error!')
      ALLOCATE(CL_act(NCL)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIDZC -- CL_act ALLOCATE error!')
      ALLOCATE(CL_ref(NCL)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGIDZC -- CL_ref ALLOCATE error!')

! executable code:  copy for input

      Z_act=R8_Z
      Z_ref=R8_Z

      CL_act=R8_CL
      CL_ref=R8_CL

! call to original routine:  AGIDZC

      CALL AGIDZC(
     > Z_act(1,1),MX,NX,NY,M,I0,J0,CL_act(1),NCL)

! copy back outputs if modified.

      where(Z_act.ne.Z_ref)
         R8_Z = Z_act
      end where
      DEALLOCATE(Z_act)
      DEALLOCATE(Z_ref)

      where(CL_act.ne.CL_ref)
         R8_CL = CL_act
      end where
      DEALLOCATE(CL_act)
      DEALLOCATE(CL_ref)

! exit
      return
      end
