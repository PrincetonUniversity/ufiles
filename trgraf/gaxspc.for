C******************** START FILE GAXSPC.FOR ; GROUP GAXES ******************
C----------------------------------------------------------------
C  GAXSPC
C   NON-INTERACTIVE SELECT AXIS TYPE (ITYPE) FOR 2D PLOTS
C
C   ITYPE=1 LINEAR
C         2 LINEAR(X),LOG(Y)
C         3 LOG(X),LOG(Y)
C         4 LOG(X),LINEAR(Y)
C
      SUBROUTINE GAXSPC(ITYPE)
C
      use gaxes_blk
C
      IF(IGXINI.EQ.0) CALL GAXES0
      IAXSEL=ITYPE
      RETURN
      END
C******************** END FILE GAXSPC.FOR ; GROUP GAXES ******************
