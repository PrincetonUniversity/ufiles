C******************** START FILE GET1F2.FOR ; GROUP UTL2D ******************
C------------------------------------------------------------------
C  ROUTINES FOR PULLING SLICES IN/OUT OF 2D DATA ARRAYS
C
      SUBROUTINE GET1F2(IDIM,IFIX,F,NF1,NF2,N1USE,WK,NWK)
C
      REAL F(NF1,NF2),WK(NWK)
C
C  EXTRACT A ROW OR COLUMN OF F (2D) INTO WORK ARRAY (1D)
C
      N2USE=NF2
C  ERROR CHECKING
      IF(IDIM.EQ.1) THEN
        CALL WKCHK(NWK,N1USE,'GET1F2',IER)
        IF(IER.NE.0) RETURN
      ELSE IF(IDIM.EQ.2) THEN
        CALL WKCHK(NWK,N2USE,'GET1F2',IER)
        IF(IER.NE.0) RETURN
      ENDIF
C
C  LOAD WORK VECTOR
      IF(IDIM.EQ.1) THEN
        DO IX=1,N1USE
          WK(IX)=F(IX,IFIX)
        ENDDO
      ELSE IF(IDIM.EQ.2) THEN
        DO IY=1,N2USE
          WK(IY)=F(IFIX,IY)
        ENDDO
      ENDIF
C
      RETURN
      END
C******************** END FILE GET1F2.FOR ; GROUP UTL2D ******************
