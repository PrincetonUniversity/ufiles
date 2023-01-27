C******************** START FILE UF2RUT.FOR ; GROUP UFILES ******************
C---------------------------------------------------------------------
C  UF2RUT
C  READ 2D DATA IN CONSECUTIVELY
C
      SUBROUTINE UF2RUT(ILUN,F,N,IERR)
      DIMENSION F(N)
C
      READ(ILUN,3008,END=9000,ERR=9000) (F(IJ),IJ=1,N)
 3008 FORMAT(6(1X,1PE11.4))
      IERR=0
      RETURN
C
 9000 CONTINUE
      IERR=1
      RETURN
C
      END
C******************** END FILE UF2RUT.FOR ; GROUP UFILES ******************
