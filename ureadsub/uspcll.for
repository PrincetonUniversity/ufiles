C******************** START FILE USPCLL.FOR ; GROUP USPARM ******************
C---------------------------------------------------------------
C&  USPCLL  DELETE ALL LOCAL UREAD PARAMETERS AT CURRENT LEVEL
C
      SUBROUTINE USPCLL
C
      USE USERIO
C
      NPT=NPLC(MAX0(NLUNIN,NLUNIO))
      DO 10 J=NPT,1,-1
      CALL USPDLO(J)
 10   CONTINUE
C
      RETURN
      END
C******************** END FILE USPCLL.FOR ; GROUP USPARM ******************
