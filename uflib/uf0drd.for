C******************** START FILE UF0DRD.FOR ; GROUP UFILS0 ******************
C--------------------------------------------------------------
C  UF0DRD
C  READ 0-DIMENSIONAL (SCALAR) DATA ASCII FILE
C
      SUBROUTINE UF0DRD(ILUN,IDEV,ZSDATE,
     >     SCVAL,SCLAB,NDIM,NSC,IER)
 

      USE UFILES
        Integer Idim
 
C
C*  ILUN-- LOGICAL UNIT NUMBER ON WHICH TO READ
C  IDEV-- TOKAMAK DEVICE -- (4 LETTERS) "PDX ","PLT ", ETC.
C  ZSDATE-- SHOT DATE (10 CHARS, E.G. "10-AUG-81 ")
C  SCVAL-- A SERIES OF NSC SCALAR NUMBERS - DATA TO BE READ
C  SCLAB-- 30 CHARACTER LABEL FOR EACH ELEMENT OF SCVAL
C    E.G. SCVAL = A TIME IN SECONDS, AND
C         SCLAB = "T0:       ","LASER TIME","SECONDS   "
C  THREE-PART LABEL:  FIRST PART= SCALAR IDENTIFICATION KEYWORD
C    (MAX 10 CHARS INCL. DELIMITTING ":")
C    SECOND PART= DESCRIPTIVE LABEL (10 CHARS)
C    THIRD PART= PHYSICAL UNITS LABEL (10 CHARS)
C
C*  NDIM-- DIMENSION OF ARRAYS TO RECEIVE SCALAR VALUES AND LABELS
C  NSC-- THE NUMBER OF SCALAR ELEMENTS (AND ASSOCIATED LABELS)
C     ACTUALLY RETRIEVED FROM FILE
C
C  IER=0 ON RETURN IF READ WAS SUCCESSFUL (RGA, will now be 0 even if there are no scalars)
C
C* DENOTES INPUT VARIABLE
C
      CHARACTER*10 ZSDATE,ZWDATE
      REAL SCVAL(*)
      CHARACTER*30 SCLAB(*)
      CHARACTER*4 IDEV
C
      CHARACTER*64 ERRSTR
C
C$r8real_output: scval
c
      character*128 znode
c
C-----------------------------------------------------------------------
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UF0DRD    ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
      IF(UFSTAT(ICHAN).NE.3) THEN
        CALL UERSTA(ICHAN,'UF0DRD    ')
        GO TO 9000
      ENDIF
C
C  CHANNEL STATUS OK
C
C  ** MOD APR 1984 -- SUPPORT BINARY COMPRESSED DATA FORMAT
C    ASCII/BINARY COMPRESSED SWITCH DETERMINED AT OPEN TIME
C
      KMPRS=UFCPRS(ICHAN)
C
      errstr='header check'
C
      nsc    = 0
      idim   = 0
      ikmprs = kmprs
      if(kmprs.eq.-1) then
C----------------------------------------
C  MDS+ data access
C
         call ufmds_rhdr(ichan,idev,zsdate,ishot,idim,znode,iln,
     >        iproc, ier)
         if(ier.eq.1) go to 9000
C
C----------------------------------------
      endif
	
      IF(KMPRS.EQ.0) THEN
        CALL UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZWDATE,ZSDATE,0,
     >      iformat,IERH)
        IF(IERH.NE.0) GO TO 8999
      ELSE IF(KMPRS.EQ.1) THEN
        READ(ILUN,END=8999,ERR=8999)
     >      ISHOT,IDEV,IDIM,IKMPRS,ZWDATE,ZSDATE
        IF((UFILEV-10*(UFILEV/10)).GE.1) WRITE(UFELUN,8001) ZWDATE
 8001   FORMAT(' % READING SCALAR COMPRESSED BINARY DATA WRITTEN ',A)
      ELSE IF(KMPRS.EQ.2) THEN
         call ufbhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,0,ierh)
         if(ierh.ne.0) go to 8999
      ENDIF
C
      IF(KMPRS.NE.IKMPRS) THEN
        WRITE(UFELUN,9006) KMPRS,IKMPRS
 9006   FORMAT(' % UF0DRD INTERNAL FLAG ERROR KMPRS, IKMPRS =',
     >2(I2,1X))
      ENDIF
C
C  CHECK SHOT NO.
C
 3002 FORMAT(1X,A10)
      if(lshotn(ichan)) then
         IF(ISHOT.NE.UFSHOT(ICHAN)) THEN
            IF((UFILEV/10).GE.1)
     >          WRITE(UFELUN,9011) ISHOT,UFSHOT(ICHAN)
 9011       FORMAT(
     >  ' % SHOT NUMBER ',I6,' IN FILE NOT SHOT NUMBER ',I6,
     >  ' IN FILENAME')
         ENDIF
      endif
C
C  DIMENSIONALITY WARNING
C
      IF(IDIM.NE.0) THEN
        WRITE(UFELUN,9010) IDIM
 9010   FORMAT(
     >    ' %UF0DRD:  DIMENSIONALITY OF INPUT FILE = ',I1)
      ENDIF
C
C  READ IN THE NUMBER OF SCALARS TO BE READ IN THE FILE
      ERRSTR='ASSOCIATED SCALAR DATA'
      if(kmprs.eq.-1) then
         call ufmds_0rd(ICHAN,SCVAL,SCLAB,NDIM,NSC,znode(1:iln),IER)
         IF(IER.NE.0) GO TO 8999
      else
         CALL UFNSRD(KMPRS,ILUN,NSC,IERR)
         IF(IERR.NE.0) GO TO 8999
         IF(NSC.LE.0) THEN
            if (idim<=0) then
               WRITE(UFELUN,9008)
 9008          FORMAT(' !UF0DRD: NO SCALARS IN 0-D FILE-- NO DATA')
            end if
            NSC=0
            GO TO 3050
         ENDIF
C  READ IN THE SCALARS AND ASSOCIATED LABELS
         CALL UF0RDR(ILUN,SCVAL,SCLAB,NDIM,NSC,IER)
         IF(IER.NE.0) GO TO 8999
C
C READ PAST "--END-OF-DATA--" LINE
         ERRSTR='TRAILER RECORD'
         IF(KMPRS.EQ.0) READ(ILUN,3010,END=8999,ERR=8999) IDUM
 3010    FORMAT(A1)
      endif
C
 3050 continue
      IER=0
      UFSTAT(ICHAN)=5
      RETURN
C
C-------------------------------
C  ERRORS
C
 8999 CONTINUE
      WRITE(UFELUN,9899) ERRSTR
 9899 FORMAT(' ? *FATAL* END OF FILE OR READ ERROR ON UFILE READ:'/
     >    6X,A)
C
 9000 CONTINUE
      WRITE(UFELUN,9900)
 9900 FORMAT(' ?UF0DRD: data read error.')
      CALL UFPAUS(1)
      IER=1
      UFSTAT(ICHAN)=5
      RETURN
      END
C******************** END FILE UF0DRD.FOR ; GROUP UFILS0 ******************
