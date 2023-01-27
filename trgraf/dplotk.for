C******************** START FILE DPLOTK.FOR ; GROUP DPLOTT *************
C------------------------------------------------------------------
C  DPLOTK
C   PLOT + LABEL 1 LINE OF THE KEY FOR A MULTIPLOT
C   USE PASSED RASTER LOCATIONS
C
C  MODIF DMC-- IF M(1)=0 SHOW SYMBOLS ONLY (NO LINES)
C
      SUBROUTINE DPLOTK(IYL,IXL,IYN,IXN,M,LBL)
C
      use gftic
C
      DIMENSION IXL(2),M(2)
      CHARACTER*(*) LBL
C
C  DRAW LINE-SYBOL COMBO
C
      if(.not.init_gftic) call gftic0
C
      CALL MOVABS(IXL(1),IYL)
      CALL DPSYST(M(2),ISYM,SIZE)
      CALL SYMOUT(ISYM,SIZE)
      IF(M(1).GT.0) THEN
C===============/DRAW LINE CONNECTING 2 SYMBOLS
        CALL DPLNST(M(1),ILINE,ISYM,SIZE)
        IF(ILINE.GE.0) THEN
          CALL DSHABS(IXL(2),IYL,ILINE)
        ELSE
          DO 10 IXSCN=1,5
            IXW1=5-IXSCN
            IXW2=4-IXW1
            IXA=(IXW1*IXL(1)+IXW2*IXL(2))/4
            CALL MOVABS(IXA,IYL)
            CALL SYMOUT(ISYM,SIZE)
 10       CONTINUE
        ENDIF
        CALL DPSYST(M(2),ISYM,SIZE)
        CALL SYMOUT(ISYM,SIZE)
C===============/DRAW 3 SYMBOLS IN A ROW
      ELSE
        IXA=(IXL(1)+IXL(2))/2
        CALL MOVABS(IXA,IYL)
        CALL SYMOUT(ISYM,SIZE)
        CALL MOVABS(IXL(2),IYL)
        CALL SYMOUT(ISYM,SIZE)
      ENDIF
C================
C
C  WRITE LABEL
      CALL GRFLBL(IXN,IYN,LBL,11,-1,0)
C
      RETURN
      END
C******************** END FILE DPLOTK.FOR ; GROUP DPLOTT ***************
