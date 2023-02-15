C-----------------------------------------------------------------------
C  GSCKAX  -- CHECK AXES FOR MONOTONICITY
C
C   SMOOTHING CODES REQUIRE MONOTONIC INCREASING X AXES FOR INPUT DATA
C
      SUBROUTINE GSCKAX(ZPLABL,ZXLABL,ZXUNS,X,NX,IER)
C
C  INPUT:
C
      CHARACTER*(*) ZPLABL    ! PROGRAM LABEL (FOR WARNING MESSAGES)
      CHARACTER*(*) ZXLABL    ! X AXIS LABEL (FOR MESSAGES)
      CHARACTER*(*) ZXUNS     ! X AXIS UNITS (FOR MESSAGES)
C
C  INPUT/OUTPUT:
      REAL X(NX)		! THE X AXIS
C
C  IF TWO POINTS X(I),X(I+1) ARE EQUAL, BUT X(I-1).LT.X(I) AND
C    X(I+2).GT.X(I+1) THEN X(I) WILL BE REDUCED BY A TINY NUMBER
C    TO RESTORE MONOTONICITY; A WARNING MESSAGE WILL BE DISPLAYED
C
C  OUTPUT:
C
      INTEGER IER
C
C  IER=0:  NORMAL -- X AXIS MAY BE PATCHED WITH WARNING MESSAGE
C  IER=1:  AXIS IS NOT MONOTONIC AND CANNOT BE USED FOR SMOOTHING
C
C-----------------------------------------------------------------------
C
C  CHECK FOR MONOTONICITY VIOLATIONS
C
      IER=0
      IWARN=0
C
      IF(NX.LT.2) RETURN
C
      IF(NX.EQ.2) THEN
        ZXPREV=X(1)-(X(2)-X(1))
      ELSE
        ZXPREV=X(1)-(X(3)-X(1))
      ENDIF
C
      IPREV=1
      DO I=2,NX
        IF(X(I).LE.X(IPREV)) THEN
          IWARN=IWARN+1
          IF(IWARN.LE.2) THEN
            WRITE(6,1001) ZPLABL,ZXLABL,ZXUNS,IPREV,X(IPREV),I,X(I)
C
 1001       FORMAT(
     >' %GSCKAX FROM ',A,':  "',A,'" AXIS (',A,
     >   ') NOT MONOTONIC INCREASING:'/
     >'   X(',I7,')=',1PE12.5,'; X(',I7,')=',1PE12.5)
C
          ENDIF
          IF(X(I).LT.X(IPREV)) THEN
            IER=1  ! ACTUAL DECREASE DETECTED
          ELSE
            IF(I.LT.NX) THEN
      	IF(X(I+1).LE.X(I)) IER=1  ! TWO CONSECUTIVE NONINCREASES
            ENDIF
            IF(ZXPREV.GE.X(IPREV)) IER=1  ! TWO CONSECUTIVE NONINCREASES
          ENDIF
          IF(IER.EQ.0) THEN
C  ATTEMPT PATCH
            ZDX=1.E-6*(X(IPREV)-ZXPREV)
 10         CONTINUE
      	ZXNEW=X(IPREV)-ZDX
      	IF(ZXNEW.EQ.X(IPREV)) THEN
      	  ZDX=2.0*ZDX
      	  GO TO 10
      	ENDIF
            X(IPREV)=ZXNEW
          ENDIF
        ENDIF
        IPREV=I
        ZXPREV=X(I-1)
      ENDDO
C
      IF(IER.GT.0) THEN
        WRITE(6,1002) IWARN
 1002 FORMAT(
     >' ?GSCKAX:  PATCH TO MAKE DATA MONOTONIC WAS NOT SUCCESSFUL.'/
     >'  ...A TOTAL OF ',I7,
     >      ' NON-MONOTONIC INCREASING INTERVALS WERE DETECTED.')
C dmc 8 Feb 1996:  check for monotonic *decreasing* axis
        idec=1
        iprev=1
        do i=2,nx
           if(x(i).ge.x(iprev)) idec=0
           iprev=i
        enddo
        if(idec.eq.1) then
           IER=2
           write(6,1004)
 1004      format(
     >' %GSCKAX:  this coordinate is monotonic decreasing!'/)
        endif
      ELSE IF(IWARN.GT.0) THEN
        WRITE(6,1003) IWARN
 1003 FORMAT(
     >' %GSCKAX:  PATCH TO MAKE DATA MONOTONIC WAS *SUCCESSFUL*.'/
     >'  ...A TOTAL OF ',I7,
     >      ' NON-MONOTONIC INCREASING INTERVALS WERE DETECTED.')
      ENDIF
C
      RETURN
      END
