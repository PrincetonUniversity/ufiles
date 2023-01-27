C******************** START FILE IFIXR.FOR ; GROUP CONCTR ******************
C-----------------
C  FIX FLOATING PT TO INTEGER
C
C    -1/2 BECOMES -1
C    3.7 BECOMES 3
C
      INTEGER FUNCTION IFIXR(Z)
C
      REAL Z
C
      IFIXR=Z
      IF(Z.LT.0.0) IFIXR=IFIXR-1
      RETURN
      	END
C******************** END FILE IFIXR.FOR ; GROUP CONCTR ******************
