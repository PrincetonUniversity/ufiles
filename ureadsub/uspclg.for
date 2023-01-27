C******************** START FILE USPCLG.FOR ; GROUP USPARM ******************
C---------------------------------------------------------------
C&  USPCLG  DELETE ALL UREAD GLOBAL STRING PARAMETERS
C
      SUBROUTINE USPCLG
C
      USE USERIO
C
      NPT=NPGC
      DO 10 J=NPT,1,-1
      CALL USPDGL(J)
 10   CONTINUE
C
      RETURN
      END
C******************** END FILE USPCLG.FOR ; GROUP USPARM ******************
