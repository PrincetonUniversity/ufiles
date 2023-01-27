C******************** START FILE URDCHR.FOR ; GROUP USINIT ******************
C--------------------------------------------------
C& URDCHR  ACTIVATE COMMAND AND PARAMETER CONTROL CHARACTERS
C
      SUBROUTINE URDCHR
      USE USERIO
C
      CHARACTER*1 ZNULL
C
      ZNULL=CHAR(0)
C
      if(levcmd.eq.1) then
         MPARAM=MSLASH
      else
         MPARAM=ZNULL
      endif
      MCMD=MSTAR
C
      RETURN
      END
