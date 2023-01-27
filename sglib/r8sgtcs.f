!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  VCURSR
      SUBROUTINE R8_VCURSR(
     > ICHAR,R8_X,R8_Y)

      external VCURSR

! argument declarations
      INTEGER ICHAR
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

! call to original routine:  VCURSR

      CALL VCURSR(
     > ICHAR,X_act,Y_act)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  MOVEA
      SUBROUTINE R8_MOVEA(
     > R8_X,R8_Y)

      external MOVEA

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

! call to original routine:  MOVEA

      CALL MOVEA(
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
      SUBROUTINE R8_MOVER(
     > R8_X,R8_Y)

      external MOVER

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

! call to original routine:  MOVER

      CALL MOVER(
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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  DRAWA
      SUBROUTINE R8_DRAWA(
     > R8_X,R8_Y)
 
      external DRAWA

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

! call to original routine:  DRAWA

      CALL DRAWA(
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
      SUBROUTINE R8_DRAWR(
     > R8_DX,R8_DY)
 
      external DRAWR

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_DX
  
 ! floating type, input/output:
      REAL*8 R8_DY
  

! local (automatic array) declarations

      integer alloc_stat
      REAL :: DX_act
      REAL :: DX_ref
      REAL :: DY_act
      REAL :: DY_ref

! allocation of working arrays...


! executable code:  copy for input

      DX_act=R8_DX
      DX_ref=R8_DX

      DY_act=R8_DY
      DY_ref=R8_DY

! call to original routine:  DRAWR

      CALL DRAWR(
     > DX_act,DY_act)

! copy back outputs if modified.

      if(DX_act.ne.DX_ref) then
         R8_DX = DX_act
      endif

      if(DY_act.ne.DY_ref) then
         R8_DY = DY_act
      endif

! exit
      return
      end
!----------------------------------------------------------
! r8real -- generated (f90 fixed form) REAL*8 interface to:  POINTA
      SUBROUTINE R8_POINTA(
     > R8_X,R8_Y)

      external POINTA

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

! call to original routine:  POINTA

      CALL POINTA(
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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  DASHA
      SUBROUTINE R8_DASHA(
     > R8_X,R8_Y,LDASH)

      external DASHA

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
      INTEGER LDASH

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

! call to original routine:  DASHA

      CALL DASHA(
     > X_act,Y_act,LDASH)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  VBGPNL
      SUBROUTINE R8_VBGPNL(
     > R8_X,R8_Y,IBOUND)

      external VBGPNL

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
      INTEGER IBOUND

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

! call to original routine:  VBGPNL

      CALL VBGPNL(
     > X_act,Y_act,IBOUND)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  WINCOT
      SUBROUTINE R8_WINCOT(
     > R8_X,R8_Y,IX,IY)

      external WINCOT

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_X
  
 ! floating type, input/output:
      REAL*8 R8_Y
  
      INTEGER IX
      INTEGER IY

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

! call to original routine:  WINCOT

      CALL WINCOT(
     > X_act,Y_act,IX,IY)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  REVCOT
      SUBROUTINE R8_REVCOT(
     > IX,IY,R8_X,R8_Y)

      external REVCOT

! argument declarations
      INTEGER IX
      INTEGER IY
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

! call to original routine:  REVCOT

      CALL REVCOT(
     > IX,IY,X_act,Y_act)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  DWINDO
      SUBROUTINE R8_DWINDO(
     > R8_XMIN,R8_XMAX,R8_YMIN,R8_YMAX)

      external DWINDO

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_XMIN
  
 ! floating type, input/output:
      REAL*8 R8_XMAX
  
 ! floating type, input/output:
      REAL*8 R8_YMIN
  
 ! floating type, input/output:
      REAL*8 R8_YMAX
  

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

! call to original routine:  DWINDO

      CALL DWINDO(
     > XMIN_act,XMAX_act,YMIN_act,YMAX_act)

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
! r8real -- generated (f90 fixed form) REAL*8 interface to:  SEEDW
      SUBROUTINE R8_SEEDW(
     > R8_XMIN,R8_XMAX,R8_YMIN,R8_YMAX)

      external SEEDW

! argument declarations
 ! floating type, input/output:
      REAL*8 R8_XMIN
  
 ! floating type, input/output:
      REAL*8 R8_XMAX
  
 ! floating type, input/output:
      REAL*8 R8_YMIN
  
 ! floating type, input/output:
      REAL*8 R8_YMAX
  

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

! call to original routine:  SEEDW

      CALL SEEDW(
     > XMIN_act,XMAX_act,YMIN_act,YMAX_act)

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




