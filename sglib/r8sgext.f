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

      REAL :: XMIN_act
      REAL :: XMAX_act

! allocation of working arrays...


! executable code:  copy for input

      XMIN_act=real(R8_XMIN,kind(XMIN_act))

      XMAX_act=real(R8_XMAX,kind(XMAX_act))

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

      REAL :: XMIN_act
      REAL :: XMAX_act

! allocation of working arrays...

! executable code:  copy for input

      XMIN_act=real(R8_XMIN,kind(XMIN_act))

      XMAX_act=real(R8_XMAX,kind(XMAX_act))

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
  

 !call to original routine:  AGVCUR

      CALL AGVCUR(
     > UCH,real(R8_X,kind=4),real(R8_Y,kind=4))

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
  

! call to original routine:  AGEYA

      CALL AGEYA(
     > real(R8_X,kind=4),real(R8_Y,kind=4),real(R8_E,kind=4))

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
  

! call to original routine:  AGEXA

      CALL AGEXA(
     >  real(R8_X,kind=4),real(R8_Y,kind=4),real(R8_E,kind=4))

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
  

! call to original routine:  AGEYS

      CALL AGEYS(
     >  real(R8_X,kind=4),real(R8_Y,kind=4),real(R8_E,kind=4))

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
  

! call to original routine:  AGEXS

      CALL AGEXS(
     > real(R8_X,kind=4),real(R8_Y,kind=4),real(R8_E,kind=4))

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
  

! call to original routine:  AGEXSR

      CALL AGEXSR(
     > real(R8_X,kind=4),real(R8_Y,kind=4),real(R8_ELO,kind=4),
     > real(R8_EHI,kind=4))

      return
      end







