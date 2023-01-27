C******************** START FILE UF0WRT.FOR ; GROUP UFILS0 ******************
C-------------------------------------------------------------------
C  UF0WRT
C  WRITE THE SCALAR LABELS AND QUANTITIES
C  PARSE THE LABEL INTO 3 10 CHARACTER SECTORS:
C  FIRST 10 CHARACTERS: KEYWORD INCL. COLON (":")
C  SECOND 10 CHARACTERS: SCALAR LABEL
C  FINAL 10 CHARACTERS: SCALAR PHYSICAL UNITS
C
      SUBROUTINE UF0WRT(ILUN,SCVAL_I,SCLAB,NSC,IER)
C
C  ILUN-- L.U.N. ON WHICH TO WRITE-- HAS BEEN CHECKED OUT OK.
C*  SCVAL-- LIST OF SCALAR VALUES
C*  SCLAB-- LIST OF CORRESPONDING LABELS
C*  NSC-- NUMBER OF ELEMENTS IN SCVAL, LABELS IN SCLAB
C  IER-- RETURN CODE 0= SUCCESS, 1= LABEL ERROR
C
      USE UFILES
C
      REAL SCVAL_I(*)
      CHARACTER*30 SCLAB(*)
c
c--------------------------------------------------------
      real, dimension(:), allocatable :: scval
      real, parameter :: ZMIN = 2.0e-38
c
c  mod dmc -- guard against underflows...
c
      allocate(scval(max(1,nsc))); scval=0
      do i=1,nsc
         if(abs(scval_i(i)).ge.zmin) scval(i)=scval_i(i)
      enddo
c--------------------------------------------------------
C
C  SUPPORT COMPRESSED BINARY UFILES FORMAT ***
C
      ICHAN=IUXCHA(ILUN)
      KMPRS=UFCPRS(ICHAN)
      if(kmprs.eq.-1) then
         write(ufelun,'('' ?UF0WRT:  MDS+ write not supported.'')')
         goto 9999
      endif
C
C  WRITE THE NUMBER OF SCALAR QUANTITIES
      IF(KMPRS.EQ.0) THEN
        WRITE(ILUN,1000) NSC
 1000 FORMAT(1X,I3,T32,';-NUMBER OF ASSOCIATED SCALAR QUANTITIES-')
      ELSE IF(KMPRS.EQ.1) THEN
        WRITE(ILUN) NSC
      else if(kmprs.eq.2) then
         call ubw3int(ilun,nsc)
      else if(kmprs.eq.4) then     ! netCDF
         call ucdf0w(ilun, 1, nsc, scval, sclab, ier)
      ENDIF
C
      IF(NSC.EQ.0) GOTO 9999
C
C  WRITE LOOP
      IER=1
      DO 100 ISC=1,NSC
C  CHECK THAT THERE IS A COLON TERMINATING THE KEYWORD
        IENDKY=INDEX(SCLAB(ISC)(1:10),':')
        IF(IENDKY.LE.1) THEN
          WRITE(UFELUN,9001) SCLAB(ISC)(1:10)
 9001     FORMAT(' ? UF0WRT - SCALAR KEYWORD ',A10,
     >         ' LACKS ":" OR IS BLANK')
          GOTO 9999
        ENDIF
C  CHECK AGAINST DUPLICATION OF KEYWORDS
        DO 50 I=1,ISC-1
          IF(SCLAB(I)(1:10).EQ.SCLAB(ISC)(1:10)) THEN
      	WRITE(UFELUN,9101) SCLAB(ISC)(1:10)
 9101 	FORMAT(' ? UF0WRT - DUPLICATE SCALAR KEYWORD:  "',A10,'"')
      	GOTO 9999
          ENDIF
 50     CONTINUE
C  WRITE THE SCALAR AND LABELS
        IF(KMPRS.EQ.1) THEN
          WRITE(ILUN) SCVAL(ISC),SCLAB(ISC)
        ELSE IF(KMPRS.EQ.0) THEN
          WRITE(ILUN,3019) SCVAL(ISC),SCLAB(ISC)
 3019     FORMAT(1X,1PE11.4,T32,';-SCALAR, LABEL FOLLOWS:'/1X,A30)
        else if(kmprs.eq.2) then
          call ubwfcmp(ilun,scval(isc),1,zavrat)
          call ubwchar(ilun,sclab(isc))
        ENDIF
C  END OF LOOP
 100  CONTINUE
      IER=0
C
C HDF ?
C      if (kmprs.eq.3) then
C         call uhdf0w(ilun, nsc, scval, sclab, ier)
C netCDF ?
      if (kmprs.eq.4) then
         call ucdf0w(ilun, 2, nsc, scval, sclab, ier)
      end if
 
 9999 continue
      deallocate(scval)

      RETURN
      	END
C******************** END FILE UF0WRT.FOR ; GROUP UFILS0 ******************
 
