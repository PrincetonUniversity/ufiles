C******************** START FILE PUT1F2.FOR ; GROUP UTL2D ******************
C------------------------------------------------------------------
C
      SUBROUTINE PUT1F2(IDIM,IFIX,F,NF1,NF2,N1USE,WK,NWK)
C
      REAL F(NF1,NF2),WK(NWK)
C
C  REPLACE A ROW OR COLUMN OF F (2D) WITH CONTENTS OF WORK ARRAY (1D)
C
      N2USE=NF2
C  ERROR CHECKING
      IF(IDIM.EQ.1) THEN
        CALL WKCHK(NWK,N1USE,'PUT1F2',IER)
        IF(IER.NE.0) RETURN
      ELSE IF(IDIM.EQ.2) THEN
        CALL WKCHK(NWK,N2USE,'PUT1F2',IER)
        IF(IER.NE.0) RETURN
      ENDIF
C
C  LOAD WORK VECTOR
      IF(IDIM.EQ.1) THEN
        DO IX=1,N1USE
          F(IX,IFIX)=WK(IX)
        ENDDO
      ELSE IF(IDIM.EQ.2) THEN
        DO IY=1,N2USE
          F(IFIX,IY)=WK(IY)
        ENDDO
      ENDIF
C
      RETURN
      END
C******************** END FILE PUT1F2.FOR ; GROUP UTL2D ******************
