C******************** START FILE MLSCCK.FOR ; GROUP GRFMP1 ******************
C------------------------------------------------------------
C  MLSCCK
C
C  CHECK THE SCALE PAIR (ZMIN,ZMAX)
C
      SUBROUTINE MLSCCK(ZMIN,ZMAX)
C
      IF(ZMIN.GT.ZMAX) THEN
        ZTEMP=ZMIN
        ZMIN=ZMAX
        ZMAX=ZTEMP
      ENDIF
C
C  EQUALITY DISALLOWED
C
      IF(ZMIN.EQ.ZMAX) THEN
        IF(ZMIN.EQ.0.0) THEN
          ZMIN=-0.1
          ZMAX=0.1
        ELSE IF(ZMIN.LT.0.0) THEN
          ZMIN=1.01*ZMIN
          ZMAX=0.99*ZMAX
        ELSE IF(ZMIN.GT.0.0) THEN
          ZMIN=0.99*ZMIN
          ZMAX=1.01*ZMAX
        ENDIF
      ENDIF
C
      RETURN
      END
C******************** END FILE MLSCCK.FOR ; GROUP GRFMP1 ******************
