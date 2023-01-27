C******************** START FILE DPLOTM.FOR ; GROUP DPLOTT *************
C-------------------------------------------------------------
C  DPLOTM
C  PLOT 1 FCN OF A MULTIPLOT VIA DPLOT1; RESTORE DEFAULT 2D
C  WINDOW
C
      SUBROUTINE DPLOTM(IXR,IYR,X,Y,N,M)
C
      use gftic
C
      INTEGER IXR(2),IYR(2),IXRP(2),IYRP(2)
      REAL X(N),Y(N)
C
      if(.not.init_gftic) call gftic0
C
      IRSET=NRSET
      IXRP(1)=NXR1
      IXRP(2)=NXR2
      IYRP(1)=NYR1
      IYRP(2)=NYR2
C
      NRSET=1
      NXR1=IXR(1)
      NXR2=IXR(2)
      NYR1=IYR(1)
      NYR2=IYR(2)
C
      IN=N
      IF(M.LE.0) IN=1
      CALL DPLOT1(X,Y,IN,M)
C
      NRSET=IRSET
      NXR1=IXRP(1)
      NXR2=IXRP(2)
      NYR1=IYRP(1)
      NYR2=IYRP(2)
C
      RETURN
      END
C******************** END FILE DPLOTM.FOR ; GROUP DPLOTT ***************
