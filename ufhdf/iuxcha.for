C******************** START FILE IUXCHA.FOR ; GROUP UFILS0 ******************
C----------------------------------------------------------------
C  IUXCHA  INTEGER FUNCTION
C  RETURN INTEGER CHANNEL NUMBER CORRESPONDING TO PASSED UNIT
C  NUMBER OR ZERO IF NOT FOUND
C
      INTEGER FUNCTION IUXCHA(ILUN)
C
      USE UFILES
C
      DO 10 ICHAN=1,UFCHAN
        IF (LUFSET(ICHAN) .AND. UFLUN(ICHAN).EQ.ILUN) THEN
C  FOUND
          IUXCHA = ICHAN
          RETURN
        ENDIF
 10   CONTINUE
C  NOT FOUND
      IUXCHA=0
      RETURN
      	END
C******************** END FILE IUXCHA.FOR ; GROUP UFILS0 ******************
