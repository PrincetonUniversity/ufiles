C******************** START FILE EXPMCK.FOR ; GROUP EXSUBS ******************
C----------------------------------
C  EXPMCK
C
C  TAKE ERROR RETURN IF PASSED VECTOR IS NOT MONOTONIC
C
      SUBROUTINE EXPMCK(X,NX,*)
C
      REAL X(NX)
C
      INXM1=NX-1
      DO 10 IX=1,INXM1
      IXP1=IX+1
      IF(X(IX).GE.X(IXP1)) THEN
        CALL UERMSG(
     >'? NON-MONOTIC INDEPENDENT VARIABLE, USE IND OPTION ONLY',' ',2)
        RETURN 1
      ENDIF
 10   CONTINUE
C
C  OK
C
      RETURN
      END
C******************** END FILE EXPMCK.FOR ; GROUP EXSUBS ******************
