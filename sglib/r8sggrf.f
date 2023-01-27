!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  CHECK
      SUBROUTINE R8_CHECK(
     > R8_X,R8_Y)

      external CHECK

! argument declarations
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
     >  ' ?R8_CHECK -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CHECK -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CHECK -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CHECK -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  CHECK

      CALL CHECK(
     > X_act(1),Y_act(1))

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  DSPLAY
      SUBROUTINE R8_DSPLAY(
     > R8_X,R8_Y)

      external DSPLAY

! argument declarations
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
     >  ' ?R8_DSPLAY -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_DSPLAY -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_DSPLAY -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_DSPLAY -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  DSPLAY

      CALL DSPLAY(
     > X_act(1),Y_act(1))

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGDSPL
      SUBROUTINE R8_AGDSPL(
     > R8_X,R8_Y)

      external AGDSPL

! argument declarations
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
     >  ' ?R8_AGDSPL -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGDSPL -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGDSPL -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGDSPL -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGDSPL

      CALL AGDSPL(
     > X_act(1),Y_act(1))

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  RGCHEK
      SUBROUTINE R8_RGCHEK(
     > NBASE,R8_ARRAY)

      external RGCHEK

! argument declarations
      INTEGER NBASE
 ! floating type, input/output:
      REAL*8 R8_ARRAY(2)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: ARRAY_act(:)
      REAL, allocatable :: ARRAY_ref(:)

! allocation of working arrays...

      ALLOCATE(ARRAY_act(2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RGCHEK -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(2)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_RGCHEK -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R8_ARRAY
      ARRAY_ref=R8_ARRAY

! call to original routine:  RGCHEK

      CALL RGCHEK(
     > NBASE,ARRAY_act(1))

! copy back outputs if modified.

      where(ARRAY_act.ne.ARRAY_ref)
         R8_ARRAY = ARRAY_act
      end where
      DEALLOCATE(ARRAY_act)
      DEALLOCATE(ARRAY_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGLINT
      SUBROUTINE R8_AGLINT(
     > R8_DMIN,R8_DMAX,R8_SLNGTH,IROUND,R8_GRAN,NMOD,K1,K2)

      external AGLINT

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_DMIN
  
 ! floating type, input/output:
      REAL*8 R8_DMAX
  
 ! floating type, input/output:
      REAL*8 R8_SLNGTH
  
      INTEGER IROUND
 ! floating type, input/output:
      REAL*8 R8_GRAN
  
      INTEGER NMOD
      INTEGER K1
      INTEGER K2

! local (automatic array) declarations

      integer alloc_stat
      REAL :: DMIN_act
      REAL :: DMIN_ref
      REAL :: DMAX_act
      REAL :: DMAX_ref
      REAL :: SLNGTH_act
      REAL :: SLNGTH_ref
      REAL :: GRAN_act
      REAL :: GRAN_ref

! allocation of working arrays...


! executable code:  copy for input

      DMIN_act=R8_DMIN
      DMIN_ref=R8_DMIN

      DMAX_act=R8_DMAX
      DMAX_ref=R8_DMAX

      SLNGTH_act=R8_SLNGTH
      SLNGTH_ref=R8_SLNGTH

      GRAN_act=R8_GRAN
      GRAN_ref=R8_GRAN

! call to original routine:  AGLINT

      CALL AGLINT(
     > DMIN_act,DMAX_act,SLNGTH_act,IROUND,GRAN_act,NMOD,K1,K2)

! copy back outputs if modified.

      if(DMIN_act.ne.DMIN_ref) then
         R8_DMIN = DMIN_act
      endif

      if(DMAX_act.ne.DMAX_ref) then
         R8_DMAX = DMAX_act
      endif

      if(SLNGTH_act.ne.SLNGTH_ref) then
         R8_SLNGTH = SLNGTH_act
      endif

      if(GRAN_act.ne.GRAN_ref) then
         R8_GRAN = GRAN_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGLOGT
      SUBROUTINE R8_AGLOGT(
     > R8_DMIN,R8_DMAX,R8_SLNGTH,IROUND,R8_GRAN,NMOD,K1,K2)

      external AGLOGT

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_DMIN
  
 ! floating type, input/output:
      REAL*8 R8_DMAX
  
 ! floating type, input/output:
      REAL*8 R8_SLNGTH
  
      INTEGER IROUND
 ! floating type, input/output:
      REAL*8 R8_GRAN
  
      INTEGER NMOD
      INTEGER K1
      INTEGER K2

! local (automatic array) declarations

      integer alloc_stat
      REAL :: DMIN_act
      REAL :: DMIN_ref
      REAL :: DMAX_act
      REAL :: DMAX_ref
      REAL :: SLNGTH_act
      REAL :: SLNGTH_ref
      REAL :: GRAN_act
      REAL :: GRAN_ref

! allocation of working arrays...


! executable code:  copy for input

      DMIN_act=R8_DMIN
      DMIN_ref=R8_DMIN

      DMAX_act=R8_DMAX
      DMAX_ref=R8_DMAX

      SLNGTH_act=R8_SLNGTH
      SLNGTH_ref=R8_SLNGTH

      GRAN_act=R8_GRAN
      GRAN_ref=R8_GRAN

! call to original routine:  AGLOGT

      CALL AGLOGT(
     > DMIN_act,DMAX_act,SLNGTH_act,IROUND,GRAN_act,NMOD,K1,K2)

! copy back outputs if modified.

      if(DMIN_act.ne.DMIN_ref) then
         R8_DMIN = DMIN_act
      endif

      if(DMAX_act.ne.DMAX_ref) then
         R8_DMAX = DMAX_act
      endif

      if(SLNGTH_act.ne.SLNGTH_ref) then
         R8_SLNGTH = SLNGTH_act
      endif

      if(GRAN_act.ne.GRAN_ref) then
         R8_GRAN = GRAN_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGMNMX
      SUBROUTINE R8_AGMNMX(
     > R8_ARRAY,R8_AMIN,R8_AMAX,NPTA,ITYPE)

      external AGMNMX

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_AMIN
  
 ! floating type, input/output:
      REAL*8 R8_AMAX
  
      INTEGER NPTA
      INTEGER ITYPE
 ! floating type, input/output:
      REAL*8 R8_ARRAY(:)
  

! local (automatic array) declarations

      integer alloc_stat, dim
      REAL, allocatable :: ARRAY_act(:)
      REAL, allocatable :: ARRAY_ref(:)
      REAL :: AMIN_act
      REAL :: AMIN_ref
      REAL :: AMAX_act
      REAL :: AMAX_ref

! allocation of working arrays...

      dim=size(R8_ARRAY)
      ALLOCATE(ARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGMNMX -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGMNMX -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R8_ARRAY
      ARRAY_ref=R8_ARRAY

      AMIN_act=R8_AMIN
      AMIN_ref=R8_AMIN

      AMAX_act=R8_AMAX
      AMAX_ref=R8_AMAX

! call to original routine:  AGMNMX

      CALL AGMNMX(
     > ARRAY_act(1),AMIN_act,AMAX_act,NPTA,ITYPE)
      go to 100

!-------------------------------------------------------

      ENTRY R8_mnmx(R8_array,R8_amin,R8_amax)

! allocation of working arrays...

      dim=size(R8_ARRAY)
      ALLOCATE(ARRAY_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGMNMX -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGMNMX -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R8_ARRAY
      ARRAY_ref=R8_ARRAY

      AMIN_act=R8_AMIN
      AMIN_ref=R8_AMIN

      AMAX_act=R8_AMAX
      AMAX_ref=R8_AMAX

! call to original routine:  AGMNMX

      CALL MNMX(
     > ARRAY_act(1),AMIN_act,AMAX_act)

!----------------------------------------------------------------------
! copy back outputs if modified.
 100     continue
      where(ARRAY_act.ne.ARRAY_ref)
         R8_ARRAY = ARRAY_act
      end where
      DEALLOCATE(ARRAY_act)
      DEALLOCATE(ARRAY_ref)

      if(AMIN_act.ne.AMIN_ref) then
         R8_AMIN = AMIN_act
      endif

      if(AMAX_act.ne.AMAX_ref) then
         R8_AMAX = AMAX_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  CPLOT
      SUBROUTINE R8_CPLOT(
     > R8_X,R8_Y)

      external CPLOT

! argument declarations
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
     >  ' ?R8_CPLOT -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CPLOT -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CPLOT -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_CPLOT -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  CPLOT

      CALL CPLOT(
     > X_act(1),Y_act(1))

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGKEYS
      SUBROUTINE R8_AGKEYS(
     > R8_X,R8_Y,IFASTX,IFASTY,LIMIT)

      external AGKEYS

! argument declarations
      INTEGER IFASTX
      INTEGER IFASTY
      INTEGER LIMIT
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
     >  ' ?R8_AGKEYS -- X_act ALLOCATE error!')
      ALLOCATE(X_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGKEYS -- X_ref ALLOCATE error!')

      dim=size(R8_Y)
      ALLOCATE(Y_act(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGKEYS -- Y_act ALLOCATE error!')
      ALLOCATE(Y_ref(dim)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGKEYS -- Y_ref ALLOCATE error!')

! executable code:  copy for input

      X_act=R8_X
      X_ref=R8_X

      Y_act=R8_Y
      Y_ref=R8_Y

! call to original routine:  AGKEYS

      CALL AGKEYS(
     > X_act(1),Y_act(1),IFASTX,IFASTY,LIMIT)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGBDGT
      REAL*8 FUNCTION R8_AGBDGT(
     > R8_XBASE,NXBASE,I)

      external AGBDGT
      REAL AGBDGT

! argument declarations
      INTEGER NXBASE
      INTEGER I
 ! floating type, input/output:
      REAL*8 R8_XBASE(3,NXBASE)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: XBASE_act(:,:)
      REAL, allocatable :: XBASE_ref(:,:)

! allocation of working arrays...

      ALLOCATE(XBASE_act(3,NXBASE)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGBDGT -- XBASE_act ALLOCATE error!')
      ALLOCATE(XBASE_ref(3,NXBASE)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGBDGT -- XBASE_ref ALLOCATE error!')

! executable code:  copy for input

      XBASE_act=R8_XBASE
      XBASE_ref=R8_XBASE

! call to original routine:  AGBDGT

      R8_AGBDGT=AGBDGT(
     > XBASE_act(1,1),NXBASE,I)

! copy back outputs if modified.

      where(XBASE_act.ne.XBASE_ref)
         R8_XBASE = XBASE_act
      end where
      DEALLOCATE(XBASE_act)
      DEALLOCATE(XBASE_ref)

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  AGBMNX
      SUBROUTINE R8_AGBMNX(
     > R8_ARRAY,R8_AMIN,R8_AMAX,NXBASE)

      external AGBMNX

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_AMIN
  
 ! floating type, input/output:
      REAL*8 R8_AMAX
  
      INTEGER NXBASE
 ! floating type, input/output:
      REAL*8 R8_ARRAY(3,NXBASE)
  

! local (automatic array) declarations

      integer alloc_stat
      REAL, allocatable :: ARRAY_act(:,:)
      REAL, allocatable :: ARRAY_ref(:,:)
      REAL :: AMIN_act
      REAL :: AMIN_ref
      REAL :: AMAX_act
      REAL :: AMAX_ref

! allocation of working arrays...

      ALLOCATE(ARRAY_act(3,NXBASE)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGBMNX -- ARRAY_act ALLOCATE error!')
      ALLOCATE(ARRAY_ref(3,NXBASE)
     >   ,stat=alloc_stat)
      if(alloc_stat.ne.0) call ufbad_exit(
     >  ' ?R8_AGBMNX -- ARRAY_ref ALLOCATE error!')

! executable code:  copy for input

      ARRAY_act=R8_ARRAY
      ARRAY_ref=R8_ARRAY

      AMIN_act=R8_AMIN
      AMIN_ref=R8_AMIN

      AMAX_act=R8_AMAX
      AMAX_ref=R8_AMAX

! call to original routine:  AGBMNX

      CALL AGBMNX(
     > ARRAY_act(1,1),AMIN_act,AMAX_act,NXBASE)

! copy back outputs if modified.

      where(ARRAY_act.ne.ARRAY_ref)
         R8_ARRAY = ARRAY_act
      end where
      DEALLOCATE(ARRAY_act)
      DEALLOCATE(ARRAY_ref)

      if(AMIN_act.ne.AMIN_ref) then
         R8_AMIN = AMIN_act
      endif

      if(AMAX_act.ne.AMAX_ref) then
         R8_AMAX = AMAX_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  ROUNDD
      REAL*8 FUNCTION R8_ROUNDD(
     > R8_VALUE,R8_FINT)

      external ROUNDD
      REAL ROUNDD

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_VALUE
  
 ! floating type, input/output:
      REAL*8 R8_FINT
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: VALUE_act
      REAL :: VALUE_ref
      REAL :: FINT_act
      REAL :: FINT_ref

! allocation of working arrays...


! executable code:  copy for input

      VALUE_act=R8_VALUE
      VALUE_ref=R8_VALUE

      FINT_act=R8_FINT
      FINT_ref=R8_FINT

! call to original routine:  ROUNDD

      R8_ROUNDD=ROUNDD(
     > VALUE_act,FINT_act)

! copy back outputs if modified.

      if(VALUE_act.ne.VALUE_ref) then
         R8_VALUE = VALUE_act
      endif

      if(FINT_act.ne.FINT_ref) then
         R8_FINT = FINT_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  ROUNDU
      REAL*8 FUNCTION R8_ROUNDU(
     > R8_VALUE,R8_FINT)

      external ROUNDU
      REAL ROUNDU

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_VALUE
  
 ! floating type, input/output:
      REAL*8 R8_FINT
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: VALUE_act
      REAL :: VALUE_ref
      REAL :: FINT_act
      REAL :: FINT_ref

! allocation of working arrays...


! executable code:  copy for input

      VALUE_act=R8_VALUE
      VALUE_ref=R8_VALUE

      FINT_act=R8_FINT
      FINT_ref=R8_FINT

! call to original routine:  ROUNDU

      R8_ROUNDU=ROUNDU(
     > VALUE_act,FINT_act)

! copy back outputs if modified.

      if(VALUE_act.ne.VALUE_ref) then
         R8_VALUE = VALUE_act
      endif

      if(FINT_act.ne.FINT_ref) then
         R8_FINT = FINT_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  BSYMS
      SUBROUTINE R8_BSYMS(
     > R8_X,R8_Y,ISYM)

      external BSYMS

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
      INTEGER ISYM

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

! call to original routine:  BSYMS

      CALL BSYMS(
     > X_act,Y_act,ISYM)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  SYMOUT
      SUBROUTINE R8_SYMOUT(
     > ISYM,R8_FACTR)

      external SYMOUT

! argument declarations
      INTEGER ISYM
 ! floating type, input/output:
      REAL*8 R8_FACTR
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: FACTR_act
      REAL :: FACTR_ref

! allocation of working arrays...


! executable code:  copy for input

      FACTR_act=R8_FACTR
      FACTR_ref=R8_FACTR

! call to original routine:  SYMOUT

      CALL SYMOUT(
     > ISYM,FACTR_act)

! copy back outputs if modified.

      if(FACTR_act.ne.FACTR_ref) then
         R8_FACTR = FACTR_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  TEKSYM
      SUBROUTINE R8_TEKSYM(
     > ISYM,R8_AMULT)

      external TEKSYM

! argument declarations
      INTEGER ISYM
 ! floating type, input/output:
      REAL*8 R8_AMULT
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: AMULT_act
      REAL :: AMULT_ref

! allocation of working arrays...


! executable code:  copy for input

      AMULT_act=R8_AMULT
      AMULT_ref=R8_AMULT

! call to original routine:  TEKSYM

      CALL TEKSYM(
     > ISYM,AMULT_act)

! copy back outputs if modified.

      if(AMULT_act.ne.AMULT_ref) then
         R8_AMULT = AMULT_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  BAR
      SUBROUTINE R8_BAR(
     > R8_X,R8_Y,LINE)

      external BAR

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
      INTEGER LINE

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

! call to original routine:  BAR

      CALL BAR(
     > X_act,Y_act,LINE)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  NUMSET
      SUBROUTINE R8_NUMSET(
     > R8_FNUM,NBASE,CLABL,IWIDTH)

      external NUMSET

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_FNUM
  
      INTEGER NBASE
      CHARACTER*12 CLABL
      INTEGER IWIDTH

! local (automatic array) declarations

      integer alloc_stat
      REAL :: FNUM_act
      REAL :: FNUM_ref

! allocation of working arrays...


! executable code:  copy for input

      FNUM_act=R8_FNUM
      FNUM_ref=R8_FNUM

! call to original routine:  NUMSET

      CALL NUMSET(
     > FNUM_act,NBASE,CLABL,IWIDTH)

! copy back outputs if modified.

      if(FNUM_act.ne.FNUM_ref) then
         R8_FNUM = FNUM_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  COMGET
      REAL*8 FUNCTION R8_COMGET(
     > ITEM)

      external COMGET
      REAL COMGET

! argument declarations
      INTEGER ITEM

! local (automatic array) declarations

      integer alloc_stat

! allocation of working arrays...


! executable code:  copy for input

! call to original routine:  COMGET

      R8_COMGET=COMGET(
     > ITEM)

! copy back outputs if modified.

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  COMSET
      SUBROUTINE R8_COMSET(
     > ITEM,R8_VALUE)

      external COMSET

! argument declarations
      INTEGER ITEM
 ! floating type, input/output:
      REAL*8 R8_VALUE
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: VALUE_act
      REAL :: VALUE_ref

! allocation of working arrays...


! executable code:  copy for input

      VALUE_act=R8_VALUE
      VALUE_ref=R8_VALUE

! call to original routine:  COMSET

      CALL COMSET(
     > ITEM,VALUE_act)

! copy back outputs if modified.

      if(VALUE_act.ne.VALUE_ref) then
         R8_VALUE = VALUE_act
      endif

! exit
      return
      end
      
