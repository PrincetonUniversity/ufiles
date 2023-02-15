C	SUBROUTINE DCUTST                              in library SMLIB
C
C	SUBROUTINE DCUTST does the setup of variable values for subroutine
C                         DCUTA2. DCUTST is called from GSMOO2
C		                                              TBT 2/5/91
C
      SUBROUTINE DCUTST (IDIM, X, Y,
     1                     LABZ, LABX, LABY,
     A                     ZLABLS,
     2                     N, NY,
     1                     ZGSIZE, ZGSIZ2,
     1		           ZXMIN,
     2			   ZXMAX,
     3                     ZFIXL,
     4                     ZFIXU,
     3	                   IWIDTH,
     4			   IMETHOD,
     5			   LLIST,
     6			   LDEGLITCH,
     7                     IALGOR,
     8                     CRETURN)             ! C*1 RETURN CODE.
 
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
 
      REAL X(N), Y(NY)
      CHARACTER*30 LABX, LABZ, ZLABLS(2), LABY
      CHARACTER*1  CRETURN
      LOGICAL  LLIST
      LOGICAL  LDEGLITCH
 
C
C  IALGOR=1:  REPLACE GLITCHES WITH WEIGHTED-REFLECTED INTERPOLANT
C  IALGOR=2:  REPLACE GLITCHES WITH LINEAR SEGMENT
 
      INTEGER IMETHOD
C  IMETHOD = 1   USE OLD CUT AND PASTE ROUTINE - DCUTA2
C  IMETHOD = 2   USE JANOS SMOOTHING - JAN_SMOOTH
 
C	---------------------------------------------------------------------
C
      LUNTRM = LNOUT(0)
 
 10   CONTINUE
C
      CALL NEUPAG
      IF(LXOUT(0))  THEN
         WRITE (LUNTRM, 1013)
       IF (IMETHOD .EQ. 1)  THEN   ! Cut and paste method.
         WRITE(LUNTRM,1001) IALGOR
       END IF  ! imethod
 
       WRITE (LUNTRM, 1012) IMETHOD, LABX(1:10), LLIST
 
       IF (IMETHOD .EQ. 1)  THEN   ! Cut and paste method.
         WRITE(LUNTRM,1074)  IWIDTH
       ELSE   !Janos method
         WRITE(LUNTRM,1075)  IWIDTH
       END IF  ! imethod
 
       WRITE (LUNTRM, 1076) LABX(1:10), ZXMIN, ZXMAX, LABX(21:30),
     2                        LABY(1:10), ZFIXL, ZFIXU,
     A                        LABY(21:27)
 
       IF (IMETHOD .EQ. 1)  THEN   ! Cut and paste method.
         WRITE(LUNTRM,1018)  LABZ,  ZGSIZE
       ELSE   !Janos method
         WRITE(LUNTRM,1019)  LABZ(1:10),  ZGSIZE, ZGSIZ2
       END IF  ! imethod
      END IF  ! LXOUT
 
 
 1013 FORMAT(//' DCUTST%:  AUTOMATIC 2-D DEGLITCHING ROUTINE '/
     >     '  USER OPTIONS:')
 1001   FORMAT('  "A" TO TOGGLE INTERPOLANT CHOICE',
     1             T60, '(CURRENT VALUE=',I3,'):'/
     > 9X,'INTERPOLANT 1:  WEIGHTED REFLECTION OF NEIGHBOURING POINTS'/
     > 9X,'INTERPOLANT 2:  LINEAR SEGMENT CONNECTING THE END POINTS')
 1012   FORMAT(
     2     '  "B"  TO TOGGLE METHOD CHOICE',
     A             T60, '(CURRENT VALUE=',  I3,
     3             '):'/
     4     '       METHOD 1: CUT AND PASTE ROUTINE.'/
     5     '       METHOD 2: JANOS METHOD OF SMOOTHING. '/
     6     '  "C"  TOGGLE SLICE DIRECTION (CURRENTLY = ', A, ')'/
     >     '  "D"  DEGLITCH DATA'/
     7     '  "E"  GRAPH DEGLITCHED DATA AND STORED DATA TOGETHER'/
     8     '  "F"  GRAPH DEGLITCHED DATA'/
     9     '  "G"  GRAPH THE DATA AS CURRENTLY STORED'/
     1     '  "H"  HELP - DESCIPTIONS OF MENU SELECTIONS. HELP - HELP'/
     >     '  "L"  TOGGLE LISTING LOCATION OF GLITCHES FOUND',
     A             T60, '(CURRENT VALUE=',  L3, '):'/
     A	   '  "R"  OVERWRITE DEGLITCHED DATA WITH STORED DATA'/
     B     '  "S"  OVERWRITE STORED DATA WITH DEGLITCHED DATA')
 
 1074   FORMAT(
     1     '  "W"  SET MAXIMUM GLITCH WIDTH',
     A             T60, '(CURRENT VALUE=',I3,'):')
 
 1075   FORMAT(
     1     '  "W"  SET # OF PTS TO BE SEARCHED FOR NEXT VALID DATA PT.',
     A             T60, '(CURRENT VALUE=',I3,'):')
 
 1076   FORMAT(
     >     '  "X"  RESTRICT X RANGE (',A,') OF DEGLITCHING OPERATION'/
     >             T15, 'CURRENT X RANGE:  (',1PE12.5,' TO ',
     B             1PE12.5,1X,A, '):'/
     >     '  "Y"  RESTRICT Y RANGE (',A,') OF DEGLITCHING OPERATION'/
     >             T15, 'CURRENT Y RANGE:  (',1PE12.5,' TO ',
     B             1PE12.5,1X,A, '):')
 
 1018 FORMAT(
     1     '  "Z"  SET GLITCH SIZE ', A,
     A             T56, ' (CUR VAL=',1PE12.5,'):'/
     >     '  "Q"  QUIT                       ')
C
 1019 FORMAT(
     1     '  "Z"  SET GLITCH GAIN & LOSS ', A10,
     A             T44, ' (CUR VAL=',1P2E12.5,'):'/
     >     '  "Q"  QUIT                       ')
C
      CALL UREAD(' DCUTST: ENTER OPTION CODE:$$')
C
 
      IF(LCHAR('A')) THEN
        IALGOR=3-IALGOR
        GO TO 10
      ENDIF
 
      IF(LCHAR('B')) THEN
        IMETHOD=3-IMETHOD
        GO TO 10
      ENDIF
C
      IF(LCHAR('Q')) THEN
        LDEGLITCH = .FALSE.
        CRETURN = 'Q'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('R')) THEN
        CRETURN = 'R'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('S')) THEN
        CRETURN = 'S'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('F')) THEN
        CRETURN = 'F'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('G')) THEN
        CRETURN = 'G'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
C
      IF(LCHAR('E')) THEN        ! GRAPH BOTH DEGLITCHED AND STORED DATA.
        CRETURN = 'E'
        RETURN
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('H')) THEN
         CALL NEUPAG
         IF (LXOUT(0)) WRITE (LUNTRM,1011)
 1011      FORMAT(/ ' DCUTST%: HELP MENU -.'/
     A	   '  In order to eliminate spikes in the data, one',
     b     ' can choose (B option) either'/
     c     '  1) Cut and paste method or'/
     d     '  2) Janos smoothing method.'/
     e     '  To do automatic 2-dimensional spike removal',
     f     ' (deglitching), the user specifies'/
     g     '  a slab of the X-Y grid. A slab is made up of many'/
     1     '  slices parallel to either the X or Y axis.'/
     G     '  Options "C","X" and "Y" are',
     h     ' used to define the slice and the slab limits.'/
     H     '  Option  "C" will reset max and min X&Y to extremes.'/
     i     '  Deglitching takes one slice at a time and handles'/
     j     '  spikes greater than a certain size (option Z) and',
     k     ' width (option W)'/
     m     '  Deglitched data is kept in a buffer so that the '/
     n     '  original data can be compared to the deglitched data',
     p     ' (option E)'/
     q     '  The buffer is updated with the new deglitched data',
     r     ' after every "D" option.'/
     s     '  The buffer can be stored over the original data by',
     t     ' using the "S" option.'/
     u     '  The buffer can be overwritten with the stored data',
     v     ' by using the "R" option.'/
     w     '  This allows the user to restart deglitching.'/
     >     '  THESE METHODS MAY NOT BE GOOD FOR UNEVENLY',
     z     ' SPACED DATA.'/
 
     Z                                                      )
         CALL UPAUST(1)
         GO TO 10
      ENDIF
C
C	------------------------------------------------------------------
C
 
 
      IF(LCHAR('Z')) THEN
C
C  GET GLITCH HEIGHT
C
       IF (IMETHOD .EQ. 1)  THEN    ! Cut and paste method.
 
 30     CONTINUE
        IF(LXOUT(0)) WRITE(LUNTRM,1003) LABZ
 1003   FORMAT(/' %DCUTST%-- ENTER SIZE OF GLITCH(ES) TO REMOVE'/
     >      ' IN UNITS OF THE Z AXIS:  ',A/
     >    ' TO REMOVE UPWARD SPIKES ENTER A POSITIVE NUMBER; TO REMOVE'/
     >    ' DOWNWARD SPIKES ENTER A NEGATIVE NUMBER.  SPIKES OF THE'/
     >    ' SPECIFIED SIZE (OR GREATER) WILL BE REMOVED.  ENTER "Q"'/
     >    ' TO QUIT, OR A NON-ZERO GLITCH SIZE FLOATING POINT NUMBER.'/)
        CALL UREAD('DCUTST:  SPECIFY (SIGNED) GLITCH SIZE:$$')
        IF(LCHAR('Q')) GO TO 10
        ZANS=FDCOD(IERT)
        IF(IERT.NE.0) GO TO 30
        IF(ZANS.EQ.0.0) THEN
          CALL UERMSG(' ?DCUTST:  NON-ZERO SIZE MUST BE GIVEN',' ',0)
          GO TO 30
        ENDIF
          ZGSIZE=ZANS
 
C
       ELSE    ! Janos method of deGLITCHing.  IMETHOD = 2
 
 31     CONTINUE
        IF(LXOUT(0)) WRITE(LUNTRM,1033) LABZ, ZGSIZE, ZGSIZ2
 1033   FORMAT(/' %DCUTST: ENTER TOLERENCES FOR GAIN & LOSS',
     1            ' FROM ONE DATA POINT TO THE NEXT'/
     >    ' IN UNITS OF THE Z AXIS:  ',A/
     2    ' SPIKES OF THE',
     >    ' SPECIFIED SIZE (OR GREATER) WILL BE REMOVED.'/' ENTER "Q"',
     >    ' TO QUIT, OR A POSITIVE GAIN & LOSS FLOATING POINT NUMBER.'/
     3    ' CURRENT GAIN AND LOSS VALUES = ', 1P2E15.5)
        CALL UREAD('DCUTST:  SPECIFY GAIN TOLERENCE:$$')
        IF(LCHAR('Q')) GO TO 10
        IF(.NOT. LCHAR(','))  THEN     ! Do not keep last value.
          ZANS=FDCOD(IERT)
          IF(IERT.NE.0) GO TO 31
          IF(ZANS.LE.0.0) THEN
           CALL UERMSG(' ?DCUTST:  POSITIVE GAIN SIZE MUST BE GIVEN',
     1                   ' ',0)
           GO TO 31
          ENDIF  ! zans
            ZGSIZE=ZANS
        END IF   ! LCHAR(,)
 
        CALL UREAD('DCUTST:  SPECIFY LOSS TOLERENCE:$$')
        IF(LCHAR('Q')) GO TO 10
        IF(.NOT. LCHAR(','))  THEN     ! Do not keep last value.
          ZANS=FDCOD(IERT)
          IF(IERT.NE.0) GO TO 31
          IF(ZANS.LE.0.0) THEN
           CALL UERMSG(' ?DCUTST:  POSITIVE LOSS SIZE MUST BE GIVEN',
     1                   ' ',0)
           GO TO 31
          ENDIF   ! ZANS
            ZGSIZ2=ZANS
        END IF    ! LCHAR(,)
 
C
       END IF  ! IMETHOD
 
C
        GO TO 10
      ENDIF    ! LCHAR(Z)
C
C	------------------------------------------------------------------
C
      IF(LCHAR('W')) THEN
 
C  GET MAX GLITCH WIDTH
C
  121   IMIN=1
        IMAX=MIN(20,(N/5))
        IF(LXOUT(0)) WRITE(LUNTRM,1002) LABX,N,X(1),X(N),IMIN,IMAX
 1002   FORMAT(/' %DCUTST -- X AXIS HAS LABEL:  ',A/
     >      ' NX=',I5,' XMIN=',1PE11.4,' XMAX=',1PE11.4//
     >      ' SPECIFY THE MAXIMUM WIDTH OF A GLITCH IN NO. OF X PTS'/
     >     ' I.E. "3" IF A GLITCH TO BE REMOVED MAY SPAN 3 CONSECUTIVE'/
     >      ' X DATA POINTS.  ENTER "Q" TO QUIT, OR AN INTEGER BETWEEN'/
     >      1X,I1,' AND ',I5,' = MAXIMUM ALLOWED GLITCH WIDTH ...'/)
        CALL UREAD(
     1         'DCUTA2:  SPECIFY MAX GLITCH FOR CUT & PASTE WIDTH:$$')
        IF(LCHAR('Q')) GO TO 10
        IANS=IDCOD(IERT)
        IF(IERT.NE.0) GO TO 121
C
C  new dmc -- user requested on the fly correction
        IF(IANS.GT.IMAX) THEN
           WRITE(LUNTRM,2001) IANS,IMAX
 2001      FORMAT(
     >' %DCUTST:  GLITCH WIDTH ',I5,' REDUCED TO MAX ALLOWED:  ',I5)
           IANS=IMAX
        ENDIF
C
        IF((IANS.LT.IMIN).OR.(IANS.GT.IMAX)) THEN
          CALL UERMSI(' ?DCUTA2:  SPECIFICATION OUT OF RANGE',IANS,0)
          GO TO 121
        ENDIF
C
        IWIDTH=IANS
 
        GO TO 10
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('X')) THEN
        IF(LXOUT(0)) WRITE(LUNTRM,1105) LABX,X(1),X(N)
 1105        FORMAT(/
     1             ' DCUTST: RANGE OF POSSIBLE X VALUES (', A, ') = '/
     1             10X,  1PE15.5, ' - ', 1PE15.5)
        ZZXMIN=ZXMIN
        ZZXMAX=ZXMAX
        CALL UREADF('DCUTST:  ENTER MINIMUM X FOR DEGLITCHING:$$',
     >	    ZZXMIN)
        CALL UREADF('DCUTST:  ENTER MAXIMUM X FOR DEGLITCHING:$$',
     >	    ZZXMAX)
        ZXMIN=AMIN1(ZZXMIN,ZZXMAX)
        ZXMAX=AMAX1(ZZXMIN,ZZXMAX)
        GO TO 10
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('Y')) THEN
        IF(LXOUT(0)) WRITE(LUNTRM,1005) LABY,Y(1),Y(NY)
 1005        FORMAT(/
     1             ' DCUTST: RANGE OF POSSIBLE Y VALUES (', A, ') = '/
     1             10X,  1PE15.5, ' - ', 1PE15.5)
        ZZXMIN=ZFIXL
        ZZXMAX=ZFIXU
        CALL UREADF('DCUTST:  ENTER MINIMUM Y FOR DEGLITCHING:$$',
     >	    ZZXMIN)
        CALL UREADF('DCUTST:  ENTER MAXIMUM Y FOR DEGLITCHING:$$',
     >	    ZZXMAX)
        ZFIXL=AMIN1(ZZXMIN,ZZXMAX)
        ZFIXU=AMAX1(ZZXMIN,ZZXMAX)
        GO TO 10
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('G')) THEN
C	  CALL GRFAH0(X,Y,N,LABX,LABZ,LSHOT,0,LABX)
        GO TO 10
      ENDIF
C
C	------------------------------------------------------------------
C
      IF(LCHAR('L')) THEN
        LLIST = .NOT. LLIST
        GO TO 10
      ENDIF  ! "L"
 
C	------------------------------------------------------------------
C
      IF(LCHAR('C')) THEN
 
      IDIM = 3-IDIM
 
C	  .Reset X&Y limits if X axis has changed.
           ZXMIN = Y(1)
           ZXMAX = Y(NY)      ! Switch X & Y limits because axis changed.
           ZFIXL = X(1)
           ZFIXU = X(N)
 
        CRETURN = 'C'
        RETURN
      ENDIF  ! "C"
C
 
C	------------------------------------------------------------------
C
 
      IF(LCHAR('D')) THEN
        CONTINUE
 
        IF (ZGSIZE .EQ. 0)  THEN
          WRITE (LUNTRM,9021)
 9021       FORMAT(/' DCUTST?: SIZE OF GLITCH MUST BE SPECIFIED -',
     1                       '  CHOOSE OPTION Z'/)
          CALL UPAUST(1)
          CALL UCLEAR
          GO TO 10
        END IF  ! ZGSIZE.
 
        LDEGLITCH = .TRUE.
          CRETURN = 'D'
 
      ELSE  ! (.NOT. LCHAR('D'))
          WRITE (LUNTRM,9020)
 9020       FORMAT(/' DCUTST?: OPTION ENTERED NOT IN MENU - TRY AGAIN'/)
          CALL UPAUST(1)
          CALL UCLEAR
          GO TO 10
 
      ENDIF  ! LCHAR('D')
 
      RETURN
      END
 
