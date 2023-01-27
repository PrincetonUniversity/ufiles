C******************** START FILE UF0RDR.FOR ; GROUP UFILS0 ******************
C--------------------------------------------------------------
C  UF0RDR
C  READ SCALAR VALUES AND LABELS FROM INPUT FILE
C  PRINT WARNINGS IF INSUFFICIENT ARRAY SPACE TO READ IN VALUES
C
      SUBROUTINE UF0RDR(ILUN,SCVAL,SCLAB,NDIM,NSC,IER)
C  ILUN-- UNIT ON WHICH TO READ
C  SCVAL-- SCALAR VALUES ARRAY, DATA READ IN
C  SCLAB-- SCALAR LABELS ARRAY, READ IN
C  NDIM-- SIZE OF SCVAL,SCLAB
C    MAKE-UP OF SCLAB-- SEE COMMENTS, S.R. "UF0DRD", ABOVE.
C  NSC-- ACTUAL NUMBER OF SCALARS IN FILE
C  IER-- RETURN CODE =0 FOR SUCCESS;
C    IER=1 IF DUPLICATE KEYWORD DETECTED
C
      USE UFILES
C
      REAL SCVAL(*)
      CHARACTER*30 SCLAB(*),ZBUFL
C
C------------------------------------------------------------
C
      IF(NSC.LE.0) RETURN
      ICHAN=IUXCHA(ILUN)
      KMPRS=UFCPRS(ICHAN)
      if(kmprs.eq.-1) then
         write(ufelun,'('' ?UF0RDR:  MDS+ read not supported.'')')
         go to 9000
      endif
C
      IF(NDIM.LT.NSC) THEN
        WRITE(UFELUN,9001)
 9001   FORMAT(' % UF0RDR WARNING-- NOT ENOUGH SPACE FOR SCALAR(S):')
      ENDIF
C
      DO 100 ISC=1,NSC
        IF(KMPRS.EQ.1) THEN
          READ(ILUN,ERR=9000,END=9000) ZVAL,ZBUFL
        ELSE IF(KMPRS.EQ.0) THEN
          READ(ILUN,3019,ERR=9000,END=9000) ZVAL,ZBUFL
 3019     FORMAT(1X,1PE11.4/1X,A30)
        else if (kmprs.eq.2) then
          call ubrfcmp(ilun,zval,1,ier)
          if(ier.ne.0) go to 9000
          call ubrchar(ilun,zbufl,ier)
          if(ier.ne.0) go to 9000
        ENDIF
C
C  CAPITALIZE KEYWORD FIELD
C
        CALL UF0CAP(ZBUFL(1:10))
C
        IF(ISC.GT.NDIM) THEN
          WRITE(UFELUN,9002) ZVAL,ZBUFL
 9002     FORMAT(3X,'VALUE=',1PE11.4,'   LABEL="',A30,'"')
        ELSE
          SCVAL(ISC)=ZVAL
          SCLAB(ISC)=ZBUFL
        ENDIF
 100  CONTINUE
C
      IER=0
      RETURN
C
C---------------------
C  ERROR EXIT
C
 9000 CONTINUE
      IER=1
      RETURN
C
      END
C******************** END FILE UF0RDR.FOR ; GROUP UFILS0 ******************
