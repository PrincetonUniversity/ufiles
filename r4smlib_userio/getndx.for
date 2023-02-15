C******************** START FILE GETNDX.FOR ; GROUP       ******************
C------------------------------------------------------------------
C  GETNDX - GET INDEX - ACCEPT REAL FIXED PARAMETER; DETERMINE NEAREST INTEGER
C
      SUBROUTINE GETNDX(IDIM,IFIX,ZFIXD,X,NX,Y,NY)
 
C
C   1/29/91 TBT CREATED FROM GET1FF.FOR.
C   2/11/91 TBT This routine returned incorrect (1) IFIX for the upper limit
C               when 1.e38 was given. This is due to roundoff. Corrected by
C               putting in IF (ZFIXD .LE. Y(1)).... etc.
 
 
      REAL X(NX),Y(NY)
C
C
C  FIND NEAREST INDEX IN FIXED DATA DIMENSION
C
      IF(IDIM.EQ.1) THEN   !  scan along Y axis.
       IF (ZFIXD .LE. Y(1))  THEN
           IFIX = 1
       ELSE IF (ZFIXD .GE. Y(NY))  THEN
           IFIX = NY
       ELSE          ! Y(1) < ZFIXD < Y(YN)
        IFIX=1
        ZDIF=ABS(ZFIXD-Y(1))
        DO IY=1,NY
          ZTES=ABS(ZFIXD-Y(IY))
          IF(ZTES.LT.ZDIF) THEN
            IFIX=IY
            ZDIF=ZTES
          ENDIF
        ENDDO
       END IF  ! ZFIXD
 
      ELSE IF(IDIM.EQ.2) THEN     ! Scan along X axis.
 
       IF (ZFIXD .LE. X(1))  THEN
           IFIX = 1
       ELSE IF (ZFIXD .GE. X(NX))  THEN
           IFIX = NX
       ELSE          ! X(1) < ZFIXD < X(YN)
        IFIX=1
        ZDIF=ABS(ZFIXD-X(1))
        DO IX=1,NX
          ZTES=ABS(ZFIXD-X(IX))
          IF(ZTES.LT.ZDIF) THEN
            IFIX=IX
            ZDIF=ZTES
          ENDIF
        ENDDO
       END IF  ! ZFIXD
 
      ENDIF
C
      RETURN
      END
C******************** END FILE GETNDX.FOR ; GROUP UTL2D ******************
