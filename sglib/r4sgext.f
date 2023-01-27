!----------------------------------------------------------
!----------------------------------------------------------
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGEYA
      SUBROUTINE R4_AGEYA(
     > R4_X,R4_Y,R4_E)

      external AGEYA

! argument declarations
 ! floating type, input/output:
      REAL R4_X(:)
  
 ! floating type, input/output:
      REAL R4_Y(:)
  
 ! floating type, input/output:
      REAL R4_E(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: E_act(:)
      REAL, allocatable :: E_ref(:)

! allocation of working arrays...

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- Y_ref ALLOCATE error!')

      dim=size(R4_E)
      ALLOCATE(E_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- E_act ALLOCATE error!')
      ALLOCATE(E_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEYA -- E_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

      E_act=R4_E
      E_ref=R4_E

! call to original routine:  AGEYA

      CALL AGEYA(
     > X_act(1),Y_act(1),E_act(1))

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

      where(E_act.ne.E_ref)
         R4_E = E_act
      end where
      DEALLOCATE(E_act)
      DEALLOCATE(E_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGEXA
      SUBROUTINE R4_AGEXA(
     > R4_X,R4_Y,R4_E)

      external AGEXA

! argument declarations
 ! floating type, input/output:
      REAL :: R4_X(:)
  
 ! floating type, input/output:
      REAL :: R4_Y(:)
  
 ! floating type, input/output:
      REAL :: R4_E(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: X_act(:)
      REAL, allocatable :: X_ref(:)
      REAL, allocatable :: Y_act(:)
      REAL, allocatable :: Y_ref(:)
      REAL, allocatable :: E_act(:)
      REAL, allocatable :: E_ref(:)

! allocation of working arrays...

      dim=size(R4_X)
      ALLOCATE(X_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA E-- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA -- Y_ref ALLOCATE error!')

      dim=size(R4_E)
      ALLOCATE(E_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA -- E_act ALLOCATE error!')
      ALLOCATE(E_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGEXA -- E_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

      E_act=R4_E
      E_ref=R4_E

! call to original routine:  AGEXA

      CALL AGEXA(
     > X_act(1),Y_act(1),E_act(1))

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

      where(E_act.ne.E_ref)
         R4_E = E_act
      end where
      DEALLOCATE(E_act)
      DEALLOCATE(E_ref)

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  AGEYS
      SUBROUTINE R4_AGEYS(
     > R4_X,R4_Y,R4_E)

      external AGEYS

! argument declarations
 ! floating type, input/output:
      REAL R4_X
  
 ! floating type, input/output:
      REAL R4_Y
  
 ! floating type, input/output:
      REAL R4_E
  

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

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

      E_act=R4_E
      E_ref=R4_E

! call to original routine:  AGEYS

      CALL AGEYS(
     > X_act,Y_act,E_act)

! copy back outputs if modified.

      if(X_act.ne.X_ref) then
         R4_X = X_act
      endif

      if(Y_act.ne.Y_ref) then
         R4_Y = Y_act
      endif

      if(E_act.ne.E_ref) then
         R4_E = E_act
      endif

! exit
      return
      end
!----------------------------------------------------------







