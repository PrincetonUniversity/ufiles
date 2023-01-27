      SUBROUTINE gpreset
	
	
C      FUNCTIONAL DESCRIPTION:
C
C              give menu to set plot scale and axes defaults for
C	    subsequent 2d plots
C
C      DUMMY ARGUMENTS:
C
C              none
C
C      IMPLICIT INPUTS:
C
C              tty (UREAD)
C
C      IMPLICIT OUTPUTS:
C
C              GFLIB common via subroutine calls
C
C
C      SIDE EFFECTS:
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C------------------------
C
 10   CONTINUE
C
      IF(LXOUT(0)) WRITE(LNOUT(0),2001)
 2001   FORMAT(/' *NEW* ENTER "T" FOR 2D PLOT TIC/GRID/FRAME OPTIONS')
C
      CALL UREAD(
     >'GPRESET: TYPE "A" FOR AXES, "S" FOR SCALING DEFAULTS, OR "Q":$$')
C
        IF(LCHAR('A')) THEN
          CALL GAXSET(ISEL,1)
        ELSE IF(LCHAR('S')) THEN
          CALL GSCDEF
        ELSE IF(LCHAR('T')) THEN
          CALL GFTICI
        ELSE IF(LCHAR('Q')) THEN
          RETURN
        ELSE
          CALL UERMSG('GPRESET:  WHAT?',' ',0)
        ENDIF
C
      GO TO 10
C
      END
