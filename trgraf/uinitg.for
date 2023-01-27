      subroutine uinitg(progname)
      implicit NONE
c
c  initialize TRGRAF & UREAD
c
      character*(*), intent(in) :: progname  ! name of program

c-------------------------------
c
      call uinita(0)
      call initvis(progname)
c
      return
      end

      
