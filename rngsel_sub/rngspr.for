C******************** START FILE RNGSPR.FOR ; GROUP RNGSEL ******************
C----------------------------------------------------------------------
C  RNGSPR
C
C  DISPLAY CURRENT SELECTION
C
      SUBROUTINE RNGSPR(LUNTRM,LSEL,XLAB,X,N,LSHOW)
C
      LOGICAL LSEL(N),LSHOW
C
      REAL X(N)
      CHARACTER*10 XLAB(3)
C
      CHARACTER*7 STATUS
C
      IF(LSHOW) THEN
        STATUS='PRESENT'
      ELSE
        STATUS='ABSENT '
      ENDIF
C
C  START PAGE
C
      CALL NEUPAG
      WRITE(LUNTRM,1000) (XLAB(IJ),IJ=1,3),STATUS
 1000 FORMAT(/' VALUES ARE ',2A10,', ',A10/
     >' FORMAT:  index (value) [THRU index (value)]'/
     >' THE FOLLOWING ENTRIES ARE *',A,'* IN THE SELECTED SUBSEQUENCE'/)
C
      IFOUND=0
      IPR=0
C
      DO I=1,N
        IF((LSEL(I).AND.LSHOW).or.
     >       ((.NOT.LSEL(I)).AND.(.NOT.LSHOW))) THEN
          IF(IFOUND.EQ.0) IFOUND=I
        ELSE
          IF(IFOUND.GT.0) THEN
            ILFND=I-1
            CALL RNGPR1(LUNTRM,LSEL,X,N,IFOUND,ILFND)
            IPR=1
            IFOUND=0
          ENDIF
        ENDIF
      ENDDO
C  DONT FORGET LAST GROUP
      IF(IFOUND.GT.0) THEN
        CALL RNGPR1(LUNTRM,LSEL,X,N,IFOUND,N)
        IPR=1
      ENDIF
C
      IF(IPR.EQ.0) THEN
        WRITE(LUNTRM,9901)
 9901 FORMAT(/' % NONE FOUND')
      ENDIF
C
      IF(LNOUT(0).EQ.LUNTRM) CALL UPAUST(1)
      RETURN
      END
C******************** END FILE RNGSPR.FOR ; GROUP RNGSEL ******************
