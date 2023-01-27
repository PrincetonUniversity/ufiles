C-----------------------------------------------------------------------
C  URDSTT -- READ FROM VIRTUAL KEYBOARD AND HANDLE ERRORS
C
C    THIS ROUTINE SHOULD BE ONLY CALLED IN AN INTERACTIVE PROCESS
C  THIS DECISION IS THE CALLER'S RESPONSIBILITY
C
      SUBROUTINE URDSTT(ZPROMPT,ZSTR)
      USE USERIO
C
      CHARACTER*(*) ZPROMPT
      CHARACTER*(*) ZSTR
C
C------------------------------------
C
      IECT=0
C
 10   CONTINUE
      CALL URDSTR(ZSTR,IEOF,IERR)
      IF(IEOF.EQ.1) THEN
        WRITE(LUNTRM,'(/'' *** END OF FILE EXIT ***''/)')
        CALL UEXITA(0)
      ELSE IF(IERR.NE.0) THEN
        IECT=IECT+1
        IF(IECT.LE.10) THEN
          WRITE(LUNTRM,'('' _%REPROMPT FOR '',A,'':'')') ZPROMPT
          GO TO 10
        ENDIF
        call ufbad_exit
      ENDIF
C
      RETURN
      END
