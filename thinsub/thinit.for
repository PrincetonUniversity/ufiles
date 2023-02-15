C******************** START FILE THINIT.FOR ; GROUP THNVRG ******************
C
C====================================
C-----WRITTEN BY A. ERLICH 4-9-85
C
C  REWRITTEN DMC OCT 1986
C====================================
C
C      COMMENTS: BNDCHK: CALLED BY SUB.THINSET THIS
C      ROUTINE 1.)ISOLATES THE LEFT & RIGHT DATA
C      TRANSITION BNDRY., 2.)COMPARES TRANSITIONAL
C      BNDRY.WITH REGIONAL BOUNDRIES AQUIRED FROM
C      USER(SUB THINSET). IF THE REGIONAL BOUNDRIES
C      DO NOT INFRINGE UPON A TRANSITION THEY WILL
C      BE RETURNED TO THINSET UNCHANGED. HOWEVER IF
C      A REGIONAL BNDRY. DOES INFRINGE A TRANSITION
C      THAT REGIONAL BNDRY WILL BE ALTERED;THE ALTERED
C      BNDRYS. WILL BE RETURNED TO THINSET.
C
C====================================
C
C=====================================
C-----IDENTIFICATION SECTION
C=====================================
C
C===============================================================
      SUBROUTINE THINIT(X,Y,XT,YT,ISTART,INUM,NPTSA,IZONS,IOPT
     >		,IRET,NIN)
C===============================================================
C       WRITTEN BY A. ERLICH 4-19-85
C===============================================================
C       IDENTIFICATION SECTION
C
C  DATA INPUT (X,Y)  OUTPUT (XT,YT)
C
      REAL X(NIN),XT(NIN),Y(NIN),YT(NIN)
C
C  CONTROLS-- START AND EXTENT OF THINNING ZONES; THINNING FACTOR,
C   WHETHER TO AVG/SMOOTH FIRST
C
      INTEGER ISTART(IZONS),INUM(IZONS),NPTSA(IZONS),IOPT(IZONS)
C
C   NPTSA - THINNING FACTOR, EVERY NPTSA'TH PT
C   IOPT = 1 JUST THIN = 2 SMOOTH AND THIN
C
C===============================================================
C
C  1.  COPY/SMOOTH THE DATA
C
      DO IZ=1,IZONS
        IST=ISTART(IZ)
        IN=INUM(IZ)
        IF(IOPT(IZ).EQ.1) THEN
          NSM=0
        ELSE
          NSM=NPTSA(IZ)-1
        ENDIF
        CALL THNAVG(Y(IST),YT(IST),IN,NSM)
      ENDDO
C
C  2. DO THINNING OPERATION
C
      IXTHIN=0
C
      DO IZ=1,IZONS
        IST=ISTART(IZ)
        IN=INUM(IZ)
        ITHIN=MAX0(1,MIN0((IN-1),NPTSA(IZ)))
        IF(ITHIN.EQ.1) THEN
C  JUST COPY
          DO II=1,IN
            IXTHIN=IXTHIN+1
            IX0=IST+II-1
            XT(IXTHIN)=X(IX0)
            YT(IXTHIN)=YT(IX0)  ! PICK UP SMOOTHED DATA
          ENDDO
        ELSE
C  THIN...
          INC=ITHIN/3
          IX0=IST+INC-ITHIN
          ILAST=IST+IN-1
          ILMI=ILAST-INC
          ILMI2=ILMI-INC
 10       CONTINUE
            IX0=IX0+ITHIN
            IF(IX0.GT.ILMI) GO TO 20
            IF(IX0.GE.ILMI2) IX0=ILMI
              IXTHIN=IXTHIN+1
      	XT(IXTHIN)=X(IX0)
      	YT(IXTHIN)=YT(IX0)  ! PICK UP SMOOTHED DATA
              GO TO 10
 20       CONTINUE
        ENDIF
      ENDDO
C
      IRET=IXTHIN
C
      RETURN
      END
