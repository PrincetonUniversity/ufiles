C******************** START FILE UEXITA.FOR ; GROUP USINIT *************
C=============================================
C&  UEXITA EXIT PROGRAM W/TTY-DEPENDENT PAGE CLEAR
C
C  EXIT A PROGRAM WHICH USED SG LIBRARY
C
      SUBROUTINE UEXITA(IDUM)
C
      USE USERIO
C
      IF(NTTYP.GE.0) THEN
        CALL NEUPAG
        CALL AGFCLOS(CBUFF)
      ENDIF
C
      call ufgood_exit  ! a normal exit, stop, status 0.
C
      STOP
      END
C******************** END FILE UEXITA.FOR ; GROUP USINIT ***************
