C******************** START FILE USPOOL.FOR ; GROUP UFILES ******************
c-----------------------------------------
C  ASCII DTEXT SPOOLING ROUTINE
C   SPOOL LINES OF LENGTH UP TO 120 CHARACTERS FROM OPEN FILE
C   ON LUN1 TO OPEN FILE ON LUN2
c
C Updated:
C 10/12/12 CLF : Comment out HDF option
C 08/01/97 CLF : Add HDF option
C
C  MOD DMC APR 1984 -- BINARY WRITE CAPABILITY ON UFILES CHANNELS
C   MUST CHECK FOR THIS
C
C------------------------------------------------------------------------------
      SUBROUTINE USPOOL(LUN1, LUN2)
C
      USE UFILES
C
      CHARACTER ZBUF*120
        integer   fid          ! hdf file id
C
C  INITIALLY ASSUME BOTH UNITS ASCII
C
      IBIN1=0
      IBIN2=0
C
      IER=0
      fid=0
C
C  CHECK INPUT CHANNEL
C
      ICHAN1=IUXCHA(LUN1)
C  ZERO CHANNEL NUMBER INDICATES NON-UFILES L.U.N. (THIS IS OK)
      IF(ICHAN1.NE.0) THEN
C  UFILES CHANNEL MUST BE OPEN FOR READ/WRITE COMMENTS
        IF((UFSTAT(ICHAN1).NE.5).AND.(UFSTAT(ICHAN1).NE.4)) THEN
          CALL UERSTA(ICHAN1,'USPOOL    ')
          WRITE(UFELUN,9001)
 9001   FORMAT(' % MUST READ UFILES DATA BEFORE SPOOLING COMMENTS')
          IER=1
        ENDIF
C  BINARY INPUT FILE?
        IBIN1=UFCPRS(ICHAN1)
        if(ibin1.eq.-1) then
           write(ufelun,'('' %uspool -- MDS+ read not supported.'')')
        endif
      ENDIF
C
C
C  CHECK OUTPUT CHANNEL
C
      ICHAN2=IUXCHA(LUN2)
C  ZERO CHANNEL NUMBER INDICATES NON-UFILES L.U.N. (THIS IS OK)
      IF(ICHAN2.NE.0) THEN
C  UFILES CHANNEL MUST BE OPEN FOR READ/WRITE COMMENTS
        IF((UFSTAT(ICHAN2).NE.2).AND.(UFSTAT(ICHAN2).NE.4)) THEN
          CALL UERSTA(ICHAN2,'USPOOL    ')
          WRITE(UFELUN,9002)
 9002   FORMAT(' % MUST WRITE UFILES DATA BEFORE SPOOLING COMMENTS')
          IER=1
        ENDIF
C  BINARY OUTPUT FILE?
        IBIN2=UFCPRS(ICHAN2)
        if(ibin2.eq.-1) then
           write(ufelun,'('' %uspool -- MDS+ write not supported.'')')
           ier=1
        endif
      ENDIF
C
      IF(IER.EQ.1) THEN
        CALL UFPAUS(1)
        RETURN
      ENDIF
c
      loop1 = 0
      loop2 = 0
C
 10   CONTINUE
      if(ibin1.eq.-1) then
         if(loop1.eq.0) then
            loop1=1
            zbuf=' %USPOOL:  MDS+ source file -- no comments spooled.'
            nj=51
         else
            go to 800
         endif
      else IF(IBIN1.EQ.0) THEN
        READ(LUN1,1001,END=800) ZBUF
 1001 FORMAT(A)
        DO 12 NJ=120,1,-1
          IF(ZBUF(NJ:NJ).NE.' ') GO TO 20
 12     CONTINUE
        NJ = 1
      ELSE if(ibin1.eq.1) then
        READ(LUN1,ERR=800,END=800) NJ
        NJ=MIN0(120,NJ)
C  READ IN 4 BYTE WORDS
        INJ=((NJ-1)/4+1)*4
        READ(LUN1) ZBUF(1:INJ)
      else if(ibin1.eq.2) then
        call ubrchcmp(lun1,zbuf,ieof,ier)
        if((ieof.gt.0).or.(ier.gt.0)) go to 800
        do nj=120,1,-1
           if(zbuf(nj:nj).ne.' ') go to 20
        enddo
        nj=1
C      else if(ibin1.eq.3) then
C         loop1 = loop1 + 1
C         call uhdfcom(lun1,zbuf,nj,0,loop1,fid,ier)
C         if (ier .lt. 0 .or. nj .eq. 0) go to 800
      else if(ibin1.eq.4) then
         loop1 = loop1 + 1
         call ucdfcom(lun1,zbuf,nj,0,loop1,ier)
         if (ier .lt. 0 .or. nj .eq. 0) go to 800
      ENDIF
C
 20   CONTINUE
c
      IF(IBIN2.EQ.0) THEN
        WRITE(LUN2,1001) ZBUF(1:NJ)
      ELSE if(ibin2.eq.1) then
C  WRITE OUT 4 BYTE WORDS
        INJ=((NJ-1)/4+1)*4
        WRITE(LUN2) NJ
        WRITE(LUN2) ZBUF(1:INJ)
      else if(ibin2.eq.2) then
        call ubwchcmp(lun2,zbuf(1:nj))
C      else if(ibin2 .eq. 3 ) then
C         loop2 = loop2 + 1
C         call uhdfcom(lun2,zbuf(1:nj),nj,1,loop2,fid,ier)
C         if (ier .lt. 0) go to 800
      else if(ibin2 .eq. 4 ) then
         loop2 = loop2 + 1
         call ucdfcom(lun2,zbuf(1:nj),nj,1,loop2,ier)
         if (ier .lt. 0) go to 800
      ENDIF
      GO TO 10
C
 800  CONTINUE
C       if HDF: close file
C      if (ibin1 .eq. 3) call uhdfcom(lun1,' ',0,0,0,fid,ier)
C      if (ibin2 .eq. 3) call uhdfcom(lun2,' ',0,1,0,fid,ier)
      RETURN
      END
C******************** END FILE USPOOL.FOR ; GROUP UFILES ******************
