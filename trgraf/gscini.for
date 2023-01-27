C******************** START FILE GSCINI.FOR ; GROUP GSCALE ******************
C-------------------------------------------------------------
C
      SUBROUTINE GSCINI
C
      use gscale_blk
C
      COMMON/GRF3PG/ NPAGEC
C
C  CHECK INITIALIZATION FLAG
      IF(IGSINI.EQ.1) RETURN
      IGSINI=1
      XMIN00=0.1
      XMAX00=1.0
      YMIN00=0.1
      YMAX00=1.0
      XRAT00=10.0
      YRAT00=10.0
C
      NOPTX=1
      NOPTY=1
C
      NPAGEC=0
C
      RETURN
      END
C******************** END FILE GSCINI.FOR ; GROUP GSCALE ******************
