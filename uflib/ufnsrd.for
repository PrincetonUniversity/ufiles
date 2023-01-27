C******************** START FILE UFNSRD.FOR ; GROUP UFILS0 ******************
C------------------------------------------------------------
C  UFNSRD
C  READ NUMBER OF SCALARS IN FILE
C
      SUBROUTINE UFNSRD(KMPRS,ILUN,NSC,IERR)
C
      USE UFILES
C
C---------------------
C
      IERR=0
C
      IF(KMPRS.EQ.0) THEN
        READ(ILUN,1000,ERR=9000,END=9000) NSC
 1000 FORMAT(1X,I3)
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,ERR=9000,END=9000) NSC
      else if(kmprs.eq.2) then
         call ubr3int(ilun,nsc,ier)
         if(ier.ne.0) go to 9000
      ENDIF
C
      RETURN
C---------------------
C  ERROR EXIT
 9000 CONTINUE
      IERR=1
      RETURN
C
      END
C******************** END FILE UFNSRD.FOR ; GROUP UFILS0 ******************
