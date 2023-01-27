C******************** START FILE ILVAL.FOR ; GROUP CONCTR ******************
C================
C  ILVAL   D. MC CUNE MAY 21 1980
C   CONVERT LOGICAL TO INTEGER:
C    .TRUE.==> ILVAL=1
C    .FALSE.==> ILVAL=2
C
      INTEGER FUNCTION ILVAL(LLVAL)
      LOGICAL LLVAL
C
      ILVAL=2
      IF(LLVAL) ILVAL=1
      RETURN
      	END
C******************** END FILE ILVAL.FOR ; GROUP CONCTR ******************
