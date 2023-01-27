C******************** START FILE NEUPAG.FOR ; GROUP URDUTS *************
C===============================================================
C&  NEUPAG  UREAD SWITCHED TERMINAL SCREEN CLEAR
C           -- CLEAR TEKTRONIX SCOPE WITH FORTRAN WRITE
C           FOR USER I/O-- KEEP "NEW PAGES" OUT OF
C           VERSATEC PLOTTING FILE
C
      SUBROUTINE NEUPAG
      USE USERIO
c
c  dmc -- disable screen clears -- these are anachronisms from
c         the tk4006 days of the 1970s -- and the generated
c         escape sequences in CLEAR_ALPHA have been hazardous
c         to scripted use of uread-based software...
c
C
C  ROUTINE TO CLEAR SCREEN WITHOUT OUTPUT TO VERSATEC FILE
C
C  UREAD4 PAGE-CLEAR CHECKS
C  NO PAGE CLEAR IN BATCH
C  NO PAGE CLEAR DURING INDIRECT FILE INPUT.
C  IF ECHO SWITCH IS SET (USER IS EXAMINING I/O OF INDIRECT INPUT
C  FILE AND PROGRAM FLOW CONTROL) CLEAR VIA UPAUST
C  IF OUTPUT FLAG IS FALSE DONT LET PAGE CLEAR COUNT EXCEED 1
C  (REMOVES MULTIPLE PAGE CLEAR DURING PROCESSING OF TYPE-AHEAD LINE
C
C  REV DMC NOV 1984 - INCORPORATE H TOWNER SYSTEM FOR DEVICE-DEPENDENT
C  PAGE CLEARING
C
C  rev DMC Sep 1991 -- alpha (text) page clear suppressed for scrolling
C  terminals (MACs and XTERM and XTC = XTERM w/XTC graphics)
C
      IF(NTTYP.LT.0) RETURN
C
cxx      ICLEAR=0
      IF(NLECHO.AND.(IBATCH.NE.1)) THEN
        CALL UPAUST(0)
cxx        ICLEAR=1
      ENDIF
      NCLEAR=NCLEAR+1
cxx      IF((IBATCH.NE.1).AND.(NCLEAR.LE.1)) ICLEAR=1
cxx      IF(ICLEAR.EQ.1) THEN
C  TTY-DEPENDENT PAGE CLEAR
C    UNKNOWN TTY TYPE ==> CALL NEWPAG AS OF OLD
cxx        IF(NTTYP.EQ.0) THEN
cxx          CALL CLEAR_ALPHA('TK4006')
cxx        ELSE
cxx          IF((UTTYP(1:3).NE.'MAC').AND.(UTTYP(1:5).NE.'XTERM').AND.
cxx     >         (UTTYP(1:3).NE.'XTC')) CALL CLEAR_ALPHA(UTTYP)
cxx        ENDIF
cxx      ENDIF
C
      RETURN
      	END
C******************** END FILE NEUPAG.FOR ; GROUP URDUTS ***************
