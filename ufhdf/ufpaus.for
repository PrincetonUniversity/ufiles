C******************** START FILE UFPAUS.FOR ; GROUP UFILES ******************
C-------------------------------------------------------------
C& UFPAUS  UFILES CONDITIONAL PAUSE AFTER ERROR MESSAGE
C     DMC 16 JAN 1985
C
C     22 Jun 1994 -- replace UPAUST call; remove dependency of
C     some Ufiles programs on ureadsub library.
C
      SUBROUTINE UFPAUS(IDUM)
C
      USE UFILES
C
      character*1 achar
C
      DATA IBELL/7/
C
      IF(((UFILEV/10).GT.1).AND.(UFELUN.EQ.6)) THEN
         CALL ufisbatch(IBATCH)
         IF(IBATCH.EQ.0) THEN
            WRITE(6,9001) IBELL,IBELL
 9001       FORMAT(1X,A1,'[Ufiles USER ACKNOWLEDGE - HIT ANY KEY]',A1)
C
            CALL ufterm_char_in(achar)
C
         ENDIF
      ENDIF
C
      RETURN
      END
C******************** END FILE UFPAUS.FOR ; GROUP UFILES ******************
