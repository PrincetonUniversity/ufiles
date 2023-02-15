	
 
C******************** START FILE UDIFF1.FOR ; GROUP GDIFF1 ******************
C-------
      SUBROUTINE UDIFF1 (X, Y, NRET, YF)
C   	SUBROUTINE UDIFF1
C                         DIFFERENTIATES ARRAY Y WITH RESPECT TO ARRAY X.
C                          INPUT ARGUMENTS: X    - REAL ARRAY
C                                           Y    - REAL ARRAY
C                                           NRET - DIMENSION OF X, Y AND YF
C                         OUTPUT ARGUMENTS: YF   - REAL ARRAY OF DY/DX
C	.version 1.01
C	.version 1.02  Ted Terpstra - Changed Real to * because
C                                     of subscript check on HP at GA.
C
      IMPLICIT NONE
 
      INTEGER  NRET,  NRETM1
      INTEGER  I
 
      REAL	 X(*), Y(*), YF(*)
C	REAL	 X(1), Y(1), YF(1)
 
C	-----------------------------------------------------------------
 
	
      YF(1) = (Y(2)-Y(1)) / (X(2)-X(1))
 
      NRETM1 = NRET-1
 
        DO I=2,NRETM1
        YF(I) = (Y(I+1)-Y(I-1)) / (X(I+1)-X(I-1))
      END DO
 
      YF(NRET) = (Y(NRET)-Y(NRET-1)) / (X(NRET)-X(NRET-1))
 
      RETURN
 
 
 
C******************** END FILE UDIFF1.FOR ; GROUP GDIFF1 ******************
      END
