C******************** START FILE GFCTRS.FOR ; GROUP GRFLBL ******************
C--------------------------------------------------------------
C  GFCTRS - CTR A STR  E.G. "XXX  " OR "  XXX" BECOME " XXX "
C   SPECIFY A PASSED LENGTH OR USE THE STRING LENGTH IF DEFAULTED
C
      SUBROUTINE GFCTRS(STR,ILEN)
      CHARACTER*(*) STR
C
      IF(ILEN.LE.0) THEN
        IL=LEN(STR)
      ELSE
        IL=MIN0(LEN(STR),ILEN)
      ENDIF
C
      DO IC=1,IL
        IF(STR(IC:IC).NE.' ') GO TO 10
      ENDDO
      RETURN
C
 10   CONTINUE
      I1=IC
      DO IC=IL,I1,-1
        IF(STR(IC:IC).NE.' ') GO TO 20
      ENDDO
 20   CONTINUE
C
      I2=IC
      INUM=I2-I1+1
C
      IMARG=(IL-INUM)/2
C
      I1NEW=1+IMARG
C
      IF(I1.EQ.I1NEW) RETURN
      IDIF=I1-I1NEW
C
      IF(IDIF.GT.0) THEN
        DO IC=I1NEW,IL
          IOLD=IC+IDIF
          IF(IOLD.GT.IL) THEN
            STR(IC:IC)=' '
          ELSE
            STR(IC:IC)=STR(IOLD:IOLD)
          ENDIF
        ENDDO
      ELSE
        DO IC=I1NEW+INUM-1,1,-1
          IOLD=IC+IDIF
          IF(IOLD.LE.0) THEN
            STR(IC:IC)=' '
          ELSE
            STR(IC:IC)=STR(IOLD:IOLD)
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
C******************** END FILE GFCTRS.FOR ; GROUP GRFLBL ******************
