      FUNCTION r8sieval(N, U, X, Y, B, C, D)
      IMPLICIT NONE
      integer, parameter :: r8=selected_real_kind(12,100)
      INTEGER N
      REAL(R8)  U, X(N), Y(N), B(N), C(N), D(N)
!
!  THIS SUBROUTINE EVALUATES THE CUBIC SPLINE INTEGRAL FROM x(1) TO U
!
!    WHERE  X(I) .LT. U .LT. X(I+1), USING HORNER'S RULE
!
!  IF  U .LT. X(1) THEN  I = 1  IS USED.
!  IF  U .GE. X(N) THEN  I = N  IS USED.
!
!  INPUT..
!
!    N = THE NUMBER OF DATA POINTS
!    U = THE ABSCISSA AT WHICH THE SPLINE IS TO BE EVALUATED
!    X,Y = THE ARRAYS OF DATA ABSCISSAS AND ORDINATES
!    B,C,D = ARRAYS OF SPLINE COEFFICIENTS COMPUTED BY SPLINE
 
      INTEGER nm1
      REAL(r8) r8sieval
      real(r8), dimension(:), allocatable :: dx, x1, x2
 
      nm1 = n - 1
      allocate(dx(nm1), x1(nm1), x2(nm1))
 
      x1 = x(1:n-1)
      x2 = x(2:n)
 
      dx = 0.0
      where (x2 < u)
         dx = x2 - x1
      end where
      where ((x1 < u)  .and. (u <= x2))
         dx = u - x1
      end where
 
      r8sieval = sum( dx*( y(1:nm1) &
           & + dx*( b(1:nm1)/2.0 &
           & + dx*( c(1:nm1)/3.0 &
           & + dx*  d(1:nm1)/4.0 ) ) ) )
 
      deallocate(dx, x1, x2)
 
      RETURN
    END FUNCTION r8sieval
