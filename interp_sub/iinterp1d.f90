SUBROUTINE IInterp1d(ns,s,f,nsi,si,aifi)
!
! Cubic spline interpolation for radial profiles
! (not satisfying periodic boundary conditions)
! Given a set of NS data points (s,f), the integral of
! the function from s(1) to the si's data points is returned.
!
! A. Pletzer Aug 99
!
      IMPLICIT NONE
      integer, parameter :: r8=selected_real_kind(12,100)
      REAL(R8) r8sieval
      EXTERNAL r8sieval
      INTEGER ns, nsi
      REAL(R8) s(ns), f(ns), si(nsi), aifi(nsi)
      INTEGER i
      REAL(R8) b(ns), c(ns), d(ns)
!
      CALL spline(ns,s,f,b,c,d)
      DO i=1,nsi
         aifi(i) = r8sieval(ns, si(i), s, f, b, c, d)
      END DO
 
      RETURN
END SUBROUTINE IInterp1d
