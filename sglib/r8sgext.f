!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  DLIMX
      SUBROUTINE R8_DLIMX(
     > R8_XMIN,R8_XMAX)

      external DLIMX

! argument declarations
 ! floating type, input:
      REAL*8 R8_XMIN
  
 ! floating type, input:
      REAL*8 R8_XMAX
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: XMIN_act
      REAL :: XMAX_act

! allocation of working arrays...


! executable code:  copy for input

      XMIN_act=R8_XMIN

      XMAX_act=R8_XMAX

! call to original routine:  DLIMX

      CALL DLIMX(
     > XMIN_act,XMAX_act)

! exit
      return
      end

      SUBROUTINE R8_DLIMY(
     > R8_XMIN,R8_XMAX)

      external DLIMY

! argument declarations
 ! floating type, input:
      REAL*8 R8_XMIN
  
 ! floating type, input:
      REAL*8 R8_XMAX
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: XMIN_act
      REAL :: XMAX_act

! allocation of working arrays...

! executable code:  copy for input

      XMIN_act=R8_XMIN

      XMAX_act=R8_XMAX

! call to original routine:  DLIMY

      CALL DLIMY(
     > XMIN_act,XMAX_act)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGVCUR
      SUBROUTINE R8_AGVCUR(
     > UCH,R8_X,R8_Y)

      external AGVCUR

! argument declarations
      CHARACTER*1 UCH
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

! call to original routine:  AGVCUR

      CALL AGVCUR(
     > UCH,X_act,Y_act)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGEYA
      SUBROUTINE R8_AGEYA(
     > R8_X,R8_Y,R8_E)

      external AGEYA

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X(:)
  
 ! floating type, input/output:
      REAL*8 R8_Y(:)
  
 ! floating type, input/output:
      REAL*8 R8_E(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: E_act(:)
      REAL, allocatable :: E_ref(:)

! allocation of working arrays...

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- Y_ref ALLOCATE error!')

      dim=size(R8_E)
      ALLOCATE(E_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- E_act ALLOCATE error!')
      ALLOCATE(E_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEYA -- E_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      E_act=R8_E
      E_ref=R8_E

! call to original routine:  AGEYA

      CALL AGEYA(
     > X_act(1),Y_act(1),E_act(1))

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

      where(E_act.ne.E_ref)
         R8_E = E_act
      end where
      DEALLOCATE(E_act)
      DEALLOCATE(E_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGEXA
      SUBROUTINE R8_AGEXA(
     > R8_X,R8_Y,R8_E)

      external AGEXA

! argument declarations
 ! floating type, input/output:
      REAL*8 :: R8_X(:)
  
 ! floating type, input/output:
      REAL*8 :: R8_Y(:)
  
 ! floating type, input/output:
      REAL*8 :: R8_E(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: E_act(:)
      REAL, allocatable :: E_ref(:)

! allocation of working arrays...

      dim=size(R8_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA E-- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA -- Y_ref ALLOCATE error!')

      dim=size(R8_E)
      ALLOCATE(E_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA -- E_act ALLOCATE error!')
      ALLOCATE(E_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGEXA -- E_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      E_act=R8_E
      E_ref=R8_E

! call to original routine:  AGEXA

      CALL AGEXA(
     > X_act(1),Y_act(1),E_act(1))

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

      where(E_act.ne.E_ref)
         R8_E = E_act
      end where
      DEALLOCATE(E_act)
      DEALLOCATE(E_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGEYS
      SUBROUTINE R8_AGEYS(
     > R8_X,R8_Y,R8_E)

      external AGEYS

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
 ! floating type, input/output:
      REAL*8 R8_E
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref
      REAL :: E_act
      REAL :: E_ref

! allocation of working arrays...


! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      E_act=R8_E
      E_ref=R8_E

! call to original routine:  AGEYS

      CALL AGEYS(
     > X_act,Y_act,E_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

      if(E_act.ne.E_ref) then
         R8_E = E_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGEXS
      SUBROUTINE R8_AGEXS(
     > R8_X,R8_Y,R8_E)

      external AGEXS

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
 ! floating type, input/output:
      REAL*8 R8_E
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref
      REAL :: E_act
      REAL :: E_ref

! allocation of working arrays...


! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      E_act=R8_E
      E_ref=R8_E

! call to original routine:  AGEXS

      CALL AGEXS(
     > X_act,Y_act,E_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

      if(E_act.ne.E_ref) then
         R8_E = E_act
      endif

! exit
      return
      end
      SUBROUTINE R8_AGEXSR(
     > R8_X,R8_Y,R8_ELO, R8_EHI)

      external AGEXSR

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
 ! floating type, input/output:
      REAL*8 R8_ELO
      REAL*8 R8_EHI
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL :: X_act
      REAL :: X_ref
      REAL :: Y_act
      REAL :: Y_ref
      REAL :: ELO_act
      REAL :: ELO_ref
      REAL :: EHI_act
      REAL :: EHI_ref

! allocation of working arrays...


! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

      ELO_act=R8_ELO
      ELO_ref=R8_ELO
      EHI_act=R8_EHI
      EHI_ref=R8_EHI

! call to original routine:  AGEXSR

      CALL AGEXSR(
     > X_act,Y_act,ELO_act,EHI_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R8_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R8_Y = Y_act
      endif

      if(ELO_act.ne.ELO_ref) then
         R8_ELO = ELO_act
      endif
      if(EHI_act.ne.EHI_ref) then
         R8_EHI = EHI_act
      endif

! exit
      return
      end







