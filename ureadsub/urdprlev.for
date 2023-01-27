      subroutine urdprlev(ilev)
C
C  call URDPRLEV(0) before call UINITA(0) to suppress case conversion
C  even on the command input line arguments
C
      use USERIO
C
C-----------------------------
C
      if(ilev.lt.0) return
      levdflt=ilev
C
      return
      end
