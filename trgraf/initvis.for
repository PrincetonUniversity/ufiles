C******************** START FILE INITVIS.FOR ; GROUP PLOTR1 *************
C---------------------------------------------------------
C  INITVIS
C
      SUBROUTINE INITVIS(progname_in)
C
      character*(*), intent(in) :: progname_in
C
      character*20 progname
C
C--------------------------------------------------------------
C
      progname=trim(progname_in)
      call ulower(progname)
      call urd_setprog(progname)
C
      RETURN
      END
C******************** END FILE INITVIS.FOR ; GROUP PLOTR1 ***************
