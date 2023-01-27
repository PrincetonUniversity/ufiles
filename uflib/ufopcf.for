C******************** START FILE UFOPCF.FOR ; GROUP UFILES *************
C------------------------------------------------------------
C  UFOPCF
C  OPEN TEMPORARY FILE FOR WRITE OF COMMENT RECORDS
C  *MOD DMC JAN 1987 - USE CONDITION HANDLER FOR ERROR CHECKING
C  *MOD DMC Sep 1993 -- Don't use condition handler -- portability
C  *Mod TBT Apr 1996 -- Took out filename in OPen with
C                       scratch to make ansi
C
C   ARGUMENTS-- ILUN=FILE LOGICAL UNIT NUMBER
C     IER-- (OUTPUT) ERROR RETURN CODE
C      0=NORMAL


      SUBROUTINE UFOPCF(ILUN,IER)

      USE UFILES
C
      CHARACTER*12 ZFILE
      CHARACTER*8 ZZBUF
C
      DATA ZFILE/'UFCMTF  .TMP'/
C
C------------------------------------------------------------
C
      IF((ILUN.LT.1).OR.(ILUN.GT.99)) THEN
        WRITE(UFELUN,9901) ILUN
 9901 FORMAT(' ? UFOPCF-- ILLEGAL UNIT NUMBER ',I5,', 1 TO 99 OK')
        CALL UFPAUS(1)
        IER=1
        RETURN
      ENDIF
C
      CALL UFINIT
C  FORM FILENAME
      WRITE(ZZBUF,8001) ILUN
 8001 FORMAT(I2,6X)
      ZFILE(7:8)=ZZBUF(1:2)
      IF(ZFILE(7:7).EQ.' ') ZFILE(7:7)='0'
C
      ICHAN=IUXCHA(ILUN)
      IF(ICHAN.EQ.0) THEN
        CALL UFXCHL(ILUN,ICHAN,IER)
        IF(IER.NE.0) THEN
C  ERROR-- ASSIGNMENT TABLE OVERFLOW
          WRITE(UFELUN,9001) ILUN,UFCHAN
 9001   FORMAT(' ? UFOPCF-- TABLE OVERFLOW ON L.U.N. ',I5/
     >         '   UFILES L.U.N. ASSIGNMENT FAILED'/
     >         '   MAXIMUM NUMBER OF L.U.N. ASSIGNMENTS: ',I3/)
          CALL UFPAUS(1)
          IER=1
          RETURN
        ENDIF
      ELSE
        IF(UFSTAT(ICHAN).NE.4) THEN
          WRITE(UFELUN,9903)
 9903 FORMAT(' ? UFOPCF-- USE UNASSIGNED L.U.N.')
          CALL UERSTA(ICHAN,'UFOPCF    ')
          IER=1
          RETURN
        ENDIF
        CLOSE(UNIT=ILUN,STATUS='DELETE')
      ENDIF
C
      LUFSET(ICHAN)=.TRUE.
      UFLUN(ICHAN)=ILUN
      UFCPRS(ICHAN)=0
      UFSTAT(ICHAN)=4
C
      IOS=0
      ILUJ=ILNURD(UJUNKA)
      OPEN(UNIT=ILUN, STATUS='SCRATCH',iostat=ios)
      if (ios .ne. 0) then
         print*,'?ufopcf: open ios=',ios
         if (ios .eq. 1) print*,'?ufopcf: Check TMPDIR!'
      endif
 
C
      IER=IOS
C
      RETURN
      END
C******************** END FILE UFOPCF.FOR ; GROUP UFILES ***************
