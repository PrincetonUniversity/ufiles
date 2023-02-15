 
 
C******************** START FILE UINTEG.FOR ; GROUP GDIFF1 ******************
C-----------------------------------------------------------
 
      SUBROUTINE UINTEG (X,Y, NRET, NLXOUT, LUNTRM, YF, XZERO)
 
      IMPLICIT NONE
      LOGICAL  NLXOUT
      INTEGER LUNTRM
      INTEGER NRET
      INTEGER NRETM1
      INTEGER ILEFT,  IRIGHT, IRIGHTP1, ILEFTM1
      INTEGER I,  IERR
      REAL    X(1), Y(1), YF(1), YLOWER, XZERO
 
C	...............................................................
 
 
      IF (NLXOUT) WRITE (LUNTRM, 9001)
 9001   FORMAT(// ' INTEGRATE Y ARRAY WITH RESPECT TO X STARTING '
     1	  , 'AT A GIVEN X' //)
        CALL UREADF ('GDIFF1: ENTER LOWER LIMIT OF INTEGRAL:$$', XZERO)
 
C	.FIND WHERE XZERO LIES (BETWEEN X(ILEFT) AND X(IRIGHT)
      NRETM1 = NRET-1
 
      IF (XZERO .LT. X(1)) THEN
          XZERO = X(1)
          ILEFT  = 1
          IRIGHT = 1
      ELSE IF (XZERO .GE. X(NRET))  THEN
          XZERO = X(NRET)
          ILEFT  = NRET
          IRIGHT = NRET
      ELSE               ! XZERO IS WITHIN X(I) INTERVAL
          DO  I=1,NRETM1
      	IF (XZERO .LT. X(I+1) ) THEN
      	    ILEFT  = I
      	    IRIGHT = I+1
      	    GO TO 100      ! LEAVE I LOOP
      	END IF   ! XZERO
          END DO       ! I
          CALL UERMSG ('ERROR', 'IN SUB UINTEG - XZERO ERROR',I)
 
  100     CONTINUE
      END IF       ! XZERO
 
      YLOWER = (Y(ILEFT) + Y(IRIGHT)) / 2.
 
C	.CALCULATE THE INTEGRAL OF Y FROM XZERO TO X FOR ALL X.
      YF(ILEFT)  = .5 * (YLOWER+Y(ILEFT))  * ( X(ILEFT) -XZERO)
      YF(IRIGHT) = .5 * (YLOWER+Y(IRIGHT)) * ( X(IRIGHT)-XZERO)
 
      IRIGHTP1 = IRIGHT+1
 
      IF (IRIGHT .LT. NRET) THEN
          DO  I=IRIGHTP1,NRET
      	YF(I) = YF(I-1) + .5 * (Y(I)+Y(I-1)) *
     1				       (X(I)-X(I-1))
          END DO    ! I
      END IF   ! IRIGHT
 
      ILEFTM1 = ILEFT-1
      IF (ILEFT .GE. 2)  THEN
          DO  I=ILEFTM1,1,-1
      	YF(I) = YF(I+1) + .5 * (Y(I)+Y(I+1)) *
     1				       (X(I)-X(I+1))
          END DO   !  I
      END IF  ! ILEFT
 
 
      RETURN
 
C******************** END FILE UINTEG.FOR ; GROUP GDIFF1 ******************
      END
 
 
