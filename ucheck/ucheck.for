C******************** START FILE UCHECK.FOR ; GROUP UCHECK *************
C--------------------------------------------------------------
C  UCHECK
C  PROGRAM TO CHECK DIMENSIONS OF A UFILE
C
      PROGRAM UCHECK
C
      CHARACTER*64 ZSTATS
      INTEGER IDIM(8)
C
C  L.U.N FOR DATA INPUT
C
      DATA ILUN/21/
C
      call ufheader("ucheck")
      CALL UINITA(0)
C
 10   CONTINUE
C
      CALL UFNTER(ILUN,'INPUT UFILES        ')
C
 20   CONTINUE
C
      CALL NEUPAG
 21   CONTINUE
      CALL URSHOT('UCHECK',', "0" TO QUIT, "D" TO CHANGE NAMES','QD',
     >	  ISHOT,IOP)
C
      IF(IOP.EQ.1) GO TO 900
      IF(IOP.EQ.2) GO TO 10
      IF(ISHOT.EQ.0) GO TO 900
C
      CALL UFOPRD(ILUN,ISHOT,IER)
      IF(IER.NE.0) CALL UCLEAR
      IF(IER.NE.0) GO TO 21
C
CX	CALL UF2DRD(ILUN,IDEV,ZSDATE,
CX     >   F,IX1MAX,X,MAXIND,Y,MAXIND,IPROC,
CX     >   NXACT,NYACT,1,
CX     >   FLAB,XLAB,YLAB,MAXSC,NUMSC,SCV,SCLAB,IER)
C
      CALL UFHDCK(ILUN,IDIM,IER)
C
      IF(IER.NE.0) THEN
        CALL UCLEAR
        CALL UFCLOS(ILUN)
        GO TO 20
      ENDIF
C
      CALL UFCLOS(ILUN)
C
C-----------------------------
C
      WRITE(ZSTATS,1001) IDIM(1),IDIM(2),IDIM(8)
 1001 FORMAT('#DIMS=',I1,' #WORDS=',I7,30X,'#SCALARS=',I3)
C
      IF((IDIM(1).GT.1).AND.(IDIM(1).LT.4)) THEN
        IST=24
        ZSTATS(IST:IST)='('
        IST=IST+1
        DO 120 ID=1,IDIM(1)
          IND=IDIM(2+ID)
          WRITE(ZSTATS(IST:IST+5),1002) IND
 1002     FORMAT(I6)
          IST=IST+6
          IF(ID.LT.IDIM(1)) THEN
            ZSTATS(IST:IST)=','
            IST=IST+1
          ENDIF
 120    CONTINUE
        ZSTATS(IST:IST)=')'
      ENDIF
C
      WRITE(LNOUT(0),2001) ZSTATS
 2001 FORMAT(1X,A/)
C
      GO TO 20
C
C  EXIT
 900  CONTINUE
        CALL NEUPAG
        CALL UEXITA(0)
        STOP
C
      	END
C******************** END FILE UCHECK.FOR ; GROUP UCHECK ***************
