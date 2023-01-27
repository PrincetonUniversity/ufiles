C******************** START FILE ISFNDS.FOR ; GROUP MULCDR ******************
C----------------------------------------------------------
C
C  FIND THE IPT'TH PT WHICH IS INSIDE THE PLOT WINDOW
C    STORE THE PT IN THE ZXL,ZYL ARRAY AND INCREMENT CTR.
C
      SUBROUTINE ISFNDS(ZXL,ZYL,NSYMBS,ICL,X,Y,N,IPT)
C
      use plot1mmx
C
      REAL ZXL(NSYMBS),ZYL(NSYMBS),X(N),Y(N)
C
C
      ICT=0
      DO I=1,N
        IF((XMIN.LE.X(I)).AND.(XMAX.GE.X(I)).AND.
     >       (YMIN.LE.Y(I)).AND.(YMAX.GE.Y(I))) ICT=ICT+1
        IF(ICT.EQ.IPT) THEN
C  PT FOUND, STORE IT AND EXIT
          ICL=ICL+1
          ZXL(ICL)=X(I)
          ZYL(ICL)=Y(I)
          RETURN
        ENDIF
C
      ENDDO
C  PT NOT FOUND, EXIT
      RETURN
      END
C******************** END FILE ISFNDS.FOR ; GROUP MULCDR ******************
