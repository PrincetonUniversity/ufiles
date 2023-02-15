SUBROUTINE DInterp1d(ns,s,f,nsi,si,dfi)
!
! Cubic spline interpolation for radial profiles
! (not satisfying periodic boundary conditions)
! Given a set of NS data points (s,f), the derivative of
! the function at the NSI data points SI is returned.
!
! A. Pletzer Aug 99
!
      IMPLICIT NONE
      REAL*8 speval
      EXTERNAL speval
      INTEGER ns, nsi
      REAL*8 s(ns), f(ns), si(nsi), dfi(nsi)
      INTEGER i
      REAL*8 b(ns), c(ns), d(ns)
!
      CALL spline(ns,s,f,b,c,d)
 
      DO i=1,nsi
         dfi(i) = speval(ns, si(i), s, f, b, c, d)
      END DO
 
      RETURN
END SUBROUTINE DInterp1d
