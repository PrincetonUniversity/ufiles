!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  CHECK
      SUBROUTINE R4_CHECK(
     > R4_X,R4_Y)

      external CHECK

! argument declarations
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
     >  ' ?R4_CHECK -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CHECK -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CHECK -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CHECK -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  CHECK

      CALL CHECK(
     > X_act(1),Y_act(1))

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
! r4real -- generated (f90 fixed form) REAL interface to:  DSPLAY
      SUBROUTINE R4_DSPLAY(
     > R4_X,R4_Y)

      external DSPLAY

! argument declarations
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
     >  ' ?R4_DSPLAY -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_DSPLAY -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_DSPLAY -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_DSPLAY -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  DSPLAY

      CALL DSPLAY(
     > X_act(1),Y_act(1))

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
! r4real -- generated (f90 fixed form) REAL interface to:  AGDSPL
      SUBROUTINE R4_AGDSPL(
     > R4_X,R4_Y)

      external AGDSPL

! argument declarations
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
     >  ' ?R4_AGDSPL -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGDSPL -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGDSPL -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGDSPL -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGDSPL

      CALL AGDSPL(
     > X_act(1),Y_act(1))

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
! r4real -- generated (f90 fixed form) REAL interface to:  AGMNMX
      SUBROUTINE R4_AGMNMX(
     > R4_ARRAY,R4_AMIN,R4_AMAX,NPTA,ITYPE)

      external AGMNMX

! argument declarations
 ! floating type, input/output:
      REAL R4_AMIN
  
 ! floating type, input/output:
      REAL R4_AMAX
  
      INTEGER NPTA
      INTEGER ITYPE
 ! floating type, input/output:
      REAL R4_ARRAY(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: ARRAY_act(:)
      REAL, allocatable :: ARRAY_ref(:)
      REAL :: AMIN_act
      REAL :: AMIN_ref
      REAL :: AMAX_act
      REAL :: AMAX_ref

! allocation of working arrays...

      dim=size(R4_ARRAY)
      ALLOCATE(ARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGMNMX -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGMNMX -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R4_ARRAY
      ARRAY_ref=R4_ARRAY

      AMIN_act=R4_AMIN
      AMIN_ref=R4_AMIN

      AMAX_act=R4_AMAX
      AMAX_ref=R4_AMAX

! call to original routine:  AGMNMX

      CALL AGMNMX(
     > ARRAY_act(1),AMIN_act,AMAX_act,NPTA,ITYPE)
      go to 100

!-------------------------------------------------------

      ENTRY R4_mnmx(R4_array,R4_amin,R4_amax)

! allocation of working arrays...

      dim=size(R4_ARRAY)
      ALLOCATE(ARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGMNMX -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGMNMX -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R4_ARRAY
      ARRAY_ref=R4_ARRAY

      AMIN_act=R4_AMIN
      AMIN_ref=R4_AMIN

      AMAX_act=R4_AMAX
      AMAX_ref=R4_AMAX

! call to original routine:  AGMNMX

      CALL MNMX(
     > ARRAY_act(1),AMIN_act,AMAX_act)

!----------------------------------------------------------------------
! copy back outputs if modified.
 100     continue
      where(ARRAY_act.ne.ARRAY_ref)
         R4_ARRAY = ARRAY_act
      end where
      DEALLOCATE(ARRAY_act)
      DEALLOCATE(ARRAY_ref)

      if(AMIN_act.ne.AMIN_ref) then
         R4_AMIN = AMIN_act
      endif

      if(AMAX_act.ne.AMAX_ref) then
         R4_AMAX = AMAX_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r4real -- generated (f90 fixed form) REAL interface to:  CPLOT
      SUBROUTINE R4_CPLOT(
     > R4_X,R4_Y)

      external CPLOT

! argument declarations
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
     >  ' ?R4_CPLOT -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CPLOT -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CPLOT -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_CPLOT -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  CPLOT

      CALL CPLOT(
     > X_act(1),Y_act(1))

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
! r4real -- generated (f90 fixed form) REAL interface to:  AGKEYS
      SUBROUTINE R4_AGKEYS(
     > R4_X,R4_Y,IFASTX,IFASTY,LIMIT)

      external AGKEYS

! argument declarations
      INTEGER IFASTX
      INTEGER IFASTY
      INTEGER LIMIT
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
     >  ' ?R4_AGKEYS -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGKEYS -- X_ref ALLOCATE error!')

      dim=size(R4_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGKEYS -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R4_AGKEYS -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R4_X
      X_ref=R4_X

      Y_act=R4_Y
      Y_ref=R4_Y

! call to original routine:  AGKEYS

      CALL AGKEYS(
     > X_act(1),Y_act(1),IFASTX,IFASTY,LIMIT)

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













