C******************** START FILE EXPERR.FOR ; GROUP EXSUBS ******************
C
C----------------------------------
C  EXPERR - ERROR REPORTING ROUTINE
C
      SUBROUTINE EXPERR(MSG,*)
C
      CHARACTER*(*) MSG
C
      CALL UERMSG(MSG,' ',2)
      RETURN 1
      END
C******************** END FILE EXPERR.FOR ; GROUP EXSUBS ******************
