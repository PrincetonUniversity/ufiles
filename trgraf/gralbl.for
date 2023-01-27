C--------------------------------------------------------------
C  GRALBL - LABEL A 2D PLOT WITH UNITS AND MAJOR PLOT LABELS
C
C   * CALL AFTER THE PLOT HAS BEEN DRAWN; BEFORE UPAUSE CALL *
C
      SUBROUTINE GRALBL(XUN,YUN,LBL1,LBL2,LBL3)
C
c--------------------
c  allow for raising of lower edge...
      use bottoms_up
c--------------------

      CHARACTER*(*) XUN ! X (HORIZ. AXIS) UNITS LABEL
      CHARACTER*(*) YUN ! Y (VERT. AXIS) UNITS LABEL
C
      CHARACTER*(*) LBL1 ! MAIN PLOT LABEL (TOP OF GRAPH)
      CHARACTER*(*) LBL2 ! MAIN PLOT LABEL (JUST UNDER LBL1)
      CHARACTER*(*) LBL3 ! MAIN PLOT LABEL (BOTTOM OF GRAPH)
C
      CALL GRFLBL(720,50+ibu_raise*idybu,XUN,0,-1,0)  ! x units label
      CALL GRFLBL(30,680,YUN,0,-1,1)   ! VERTICAL LABEL (y units)
C
      CALL GRFLBL(180,750,LBL1,60,-1,0)
C
      if(len(trim(lbl1)).eq.0) then
        CALL GRFLBL(180,727,LBL2,60,0,0)  ! CTR LONE LABEL AT TOP
      ELSE
        CALL GRFLBL(180,727,LBL2,60,-1,0) ! RIGHT JUSTIFY 2 LBLS @TOP
      ENDIF
C
      if(ibrk_lbl3.eq.0) then
         CALL GRFLBL(100,20,LBL3,60,0,0) ! CTR AT BOTTOM
      else
         ! center both parts
         CALL GRFLBL(100,20+idybu,LBL3(1:ibrk_lbl3),60,0,0)
         CALL GRFLBL(100,20,LBL3(ibrk_lbl3+1:),60,0,0)
      endif
C
      RETURN
      END
