C----------------------------------------------------------------
C  GFTICS - SET SG TIC OPTIONS CONTROLLED FROM GFTIC BLOCK
C
      SUBROUTINE GFTICS
C
C  EXECUTE AFTER BINITT CALL; BEFORE CHECK/DISPLAY
C
      use gftic
C
      if(.not.init_gftic) call gftic0
C
      CALL XFRM(KFRAMX)
      CALL YFRM(KFRAMY)
C
      CALL XMFRM(KFRMMX)
      CALL YMFRM(KFRMMY)
C
      CALL XNEAT(KNEATX)
      CALL YNEAT(KNEATY)
C
      RETURN
      END
