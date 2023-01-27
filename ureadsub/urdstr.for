C-----------------------------------------------------------------------
C  URDSTR -- READ A STRING IN FROM THE VIRTUAL KEYBOARD
C
C    USE SMG ROUTINES -- INPUT FROM INTERACTIVE USER.  THIS ROUTINE
C  SHOULD ONLY BE CALLED FROM AN INTERACTIVE PROCESS -- IT IS THE
C  CALLER'S RESPONSIBILITY TO DECIDE THIS.
C
C    NB DON'T USE SMG ON UNIX SYSTEMS WHERE IT DOES NOT EXIST!
C  DMC OCT 1991
C
      SUBROUTINE URDSTR(ZSTR,IEOF,IERR)
C
      USE USERIO
C
      CHARACTER*(*) ZSTR    ! STRING TO BE READ IN
      INTEGER IEOF	      ! INTEGER END OF FILE = 1
      INTEGER IERR	      ! COMPLETION CODE NORMAL = 0
      CHARACTER*1 EOT
C
      CHARACTER*1 ZNULL
C
C--------------------------------------
C
      ZSTR=' '
      ZNULL=CHAR(0)
      IERR=0
      IEOF=0
      EOT = char(3)
C
c
c  unix
c
	if (NLJAVA) then
		write(*,*) EOT
	endif

        call fgetline(' ',zstr,ichars)
        if(ichars.eq.-1) ieof=1
c
C
      RETURN
      END
