      subroutine uflower(ilun)
C
C  set the filename and .ext to lower case -- for unix filenames,
C  case control option (dmc 26 Sep 1994)
C
      use UFILES
C
      integer ilun
C
C------------------------------------
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UFLOWER   ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.0) THEN
        CALL UERSTA(ICHAN,'UFLOWER   ')
        GO TO 9000
      ENDIF
C
C  OK
C
        call ulower(uprfix(ichan))
        call ulower(uextnd(ichan))
C
 9000   continue
        return
        end
