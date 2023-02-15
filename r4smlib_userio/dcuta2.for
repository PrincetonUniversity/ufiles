C-----------------------------------------------------------------------
C  DCUTA2 -- AUTOMATIC DEGLITCHING INTERACTIVE CONTROL ROUTINE
C          - CREATED FROM DCUTA1 1/29/91 BY TBT BY ADDING ARGUMENT
C            NTIMES. ASK FOR INFO WHEN NTIMES=1, ELSE JUST DEGLITCH.
C	     DeGLITCHed results are saved over the original data.
 
      SUBROUTINE DCUTA2(X,Y,WK,N,LABY,LABX,LSHOT,IALGOR,NTIMES,
     1                     ITOTFND, ZVAL, LABZ,
     2                     ZGSIZE,  ZGSIZ2,
     1		           ZXMIN,
     2			   ZXMAX,
     3	                   IWIDTH,
     4			   IMETHOD,
     5			   LLIST,
     6			   LDEGLITCH)
 
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
 
C
      REAL X(N),Y(N),WK(N)
      REAL ZVAL               ! Input - value of 3rd coordinate at which
				!     slice is taken.
      CHARACTER*30 LABZ       ! Input - Label of 3rd coordinate.
      LOGICAL LLIST           ! List deglitched points.
      LOGICAL LDEGLITCH        ! Deglitch option "D" chosen.
C
      INTEGER NTIMES    ! IF NTIMES =1 ASK FOR INFORMATION
                          ! Input value set by calling subroutine to #
                          ! of times DCUTA2 has been called.
			  ! SET TO 0 on return if "Q" option taken.
                          ! ITOTFND is returned = # of glitches found and fixed.
 
      CHARACTER*30 LABX,LABY
      CHARACTER*(*) LSHOT
C	CHARACTER*8 END_TIME
C
      REAL XL(5)
C
C  INPUT DATA X(N) (ORDERED) AND Y(N)
C  WORKSPACE WK(N)
C
C  LABELS LABY,LABX,LSHOT
C
C  IALGOR=1:  REPLACE GLITCHES WITH WEIGHTED-REFLECTED INTERPOLANT
C  IALGOR=2:  REPLACE GLITCHES WITH LINEAR SEGMENT
 
      INTEGER IMETHOD
C  IMETHOD = 1   USE OLD CUT AND PASTE ROUTINE - DCUTA2
C  IMETHOD = 2   USE JANOS SMOOTHING - JANSMO
C
C-----------------------------------------------------------------------
      LUNTRM=LNOUT(0)
      ICALC=0
C
C
 
 
      IF (LDEGLITCH)  THEN
 
       IF ( IMETHOD .EQ. 1)  THEN
        CALL DCUTS1(X,Y,WK,N,ZXMIN,ZXMAX,IALGOR,IWIDTH,ZGSIZE,IFOUND)
       ELSE IF (IMETHOD .EQ. 2) THEN
        CALL UCOPYF ( Y, WK, N)  ! COPY DATA INTO WORK ARRAY
        CALL JANSMO(WK, X, N, ZGSIZE, ZGSIZ2, IWIDTH, ZXMIN, ZXMAX,
     1                    IFOUND)                   ! tbt  END_TIME, IERR)
       ELSE
        WRITE (LUNTRM, 9111) IMETHOD
 9111     FORMAT(/ ' DCUTA2?: ERROR - IMETHOD NOT 1 OR 2 (CURRENTLY=',
     1             I8, '):'/)
        NTIMES=0
        RETURN
       ENDIF  ! IMETHOD
 
C
        ITOTFND = ITOTFND + IFOUND
        IF(IFOUND.EQ.0) THEN
C	    WRITE(LUNTRM,'(4X,'' %NO GLITCHES FOUND FOR SLICE'',I5,
C    A            '',  TOTAL GLITCHES FOUND ='', I5)')
C    1                  NTIMES, ITOTFND
        ELSE   ! # deglitched > 0
          ICALC=IFOUND
          IF(LXOUT(0) .AND. .NOT. LLIST)         ! Print only when not listing
     1           WRITE(LUNTRM,1004) IFOUND, NTIMES, ITOTFND
 1004                   FORMAT( ' DCUTA2%:', I5,
     A               ' GLITCH(ES) FOUND FOR SLICE',
     1           I5, ',  TOTAL FOUND =', I7)
 
            IF (LLIST .AND. LXOUT(0))  THEN     ! LIST GLITCHES REMOVED.
               WRITE(LUNTRM,1005) ICALC,LABX, LABZ, ZVAL
 1005          FORMAT( ' DCUTA2%:', I5,
     A            ' GLITCHES WERE FOUND AT THE FOLLOWING X LOCATIONS'/
     >            '     (WHERE X IS ',A,'):'/
     B            ' FOR CONSTANT Y (', A, ') = ', E15.5  )
             IPREV=0
             IL=0
             DO 50 IX=1,N
               IF(Y(IX).NE.WK(IX)) THEN
                 IF(IX.GT.(IPREV+1)) THEN
                   IPREV=IX
      	     IL=IL+1
      	     IF(IL.GT.5) THEN
                     WRITE(LUNTRM,
     1                  '(5(1X,1PE11.4))') (XL(II),II=1,5)
      	       IL=1
      	     ENDIF   ! IL
      	     XL(IL)=X(IX)
                 ENDIF     ! IX
               ENDIF       ! Y(IX)
 50          CONTINUE
             IF(IL.GT.0) WRITE(LUNTRM, '(5(1X,1PE11.4))')
     A                                    (XL(II),II=1,IL)
          END IF  ! LLIST
 
 
          CALL DCUTCP(WK,Y,N)     ! Put data into original array.
 
        ENDIF    ! IFOUND > 0
C
        RETURN
C
      ENDIF  ! DEGLITCH
C
      WRITE(LUNTRM,1221)
 1221   FORMAT (//' DCUTA2?: REACHED STATEMENT 1221 IN ERROR.')
      RETURN
      END
