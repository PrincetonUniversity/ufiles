C******************** START FILE UBINIT.FOR ; GROUP UPAUST *************
C--------------------------------------------------------------
C&  UBINIT  TTY DEPENDENT INTERFACE TO SG LIB CALL BINITT
C
C  INTERFACE TO SG BINITT -
C   PREPARE FOR NEW GRAPH PAGE
C
      SUBROUTINE UBINIT(IDUM)
C
      USE USERIO
C
C------------------------------------------
C
      IF(NTTYP.GE.0) THEN
C
        NPLOTD=NPLOTD+1
C
        IF(NPLOTD.EQ.1) WRITE(LUNTRM,1001)
 1001   FORMAT(' [ GFLIB/SGLIB -- output to plot screen or file ]')
C
        CALL INITT(0)
 
        CALL BINITT
 
        IOUT=2
        CALL AGSEEF(IOUT,IDUM1,IDUM2,IDUM3)
C
C  CLEAR GRAPHICS SCREEN IN OUTPUT IF TO A TERMINAL
C    OR IF TO A FILE AND THIS IS NOT THE FIRST PLOT
C
        IF((NPLOTD.GT.0).OR.(IOUT.GE.2)) CALL NEWPAG
C
      ENDIF
C
      RETURN
      END
C******************** END FILE UBINIT.FOR ; GROUP UPAUST ***************
