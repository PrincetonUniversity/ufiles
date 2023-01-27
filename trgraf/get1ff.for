C******************** START FILE GET1FF.FOR ; GROUP UTL2D ******************
C------------------------------------------------------------------
C  GET1FF - ACCEPT REAL FIXED PARAMETER; DETERMINE NEAREST INTEGER
C  AND CALL GET1F2
C
      SUBROUTINE GET1FF(IDIM,IFIX,ZFIXD,X,NX,Y,NY,F,NF1,ZWK,NWK)
C
      REAL X(NX),Y(NY),F(NF1,NY),ZWK(NWK)
C
      CALL WKCHK(NWK,NX,'GET1FF (X)',IER1)
      CALL WKCHK(NWK,NY,'GET1FF (Y)',IER2)
C
      IF((IER1.NE.0).OR.(IER2.NE.0)) RETURN
C
C  FIND NEAREST INDEX IN FIXED DATA DIMENSION
C
      IF(IDIM.EQ.1) THEN
        IFIX=1
        ZDIF=ABS(ZFIXD-Y(1))
        DO IY=1,NY
          ZTES=ABS(ZFIXD-Y(IY))
          IF(ZTES.LT.ZDIF) THEN
            IFIX=IY
            ZDIF=ZTES
          ENDIF
        ENDDO
      ELSE IF(IDIM.EQ.2) THEN
        IFIX=1
        ZDIF=ABS(ZFIXD-X(1))
        DO IX=1,NX
          ZTES=ABS(ZFIXD-X(IX))
          IF(ZTES.LT.ZDIF) THEN
            IFIX=IX
            ZDIF=ZTES
          ENDIF
        ENDDO
      ENDIF
C
C  EXTRACT THE WORK VECTOR
C
      CALL GET1F2(IDIM,IFIX,F,NX,NY,NF1,ZWK,NWK)
C
      RETURN
      END
C******************** END FILE GET1FF.FOR ; GROUP UTL2D ******************
