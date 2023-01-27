C******************** START FILE IMMNB.FOR ; GROUP MULCDR ******************
C----------------------------------------------------------
C  INTEGER FCN IMMNB - COUNT NUMBER OF PTS IN PLOT WINDOW
C
      INTEGER FUNCTION IMMNB(X,Y,N)
C
      use plot1mmx
C
      REAL X(N),Y(N)
C
      ICT=0
      DO I=1,N
        IF((XMIN.LE.X(I)).AND.(XMAX.GE.X(I)).AND.
     >       (YMIN.LE.Y(I)).AND.(YMAX.GE.Y(I))) ICT=ICT+1
      ENDDO
C
      IMMNB=ICT
      RETURN
      END
C******************** END FILE IMMNB.FOR ; GROUP MULCDR ******************
