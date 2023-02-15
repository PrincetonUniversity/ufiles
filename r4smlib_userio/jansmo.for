C
 
 
!-------------------------------------------------------------------- JANSMO
 
C	MODIFIED LAST:
C	    TBT  2/18/91  Took out IERR and END_TIME.
C	    TBT  2/01/91  I got this smoothing routine from Mary Corneliussen.
 
C	This routine is called by subroutine DCUTA2.
C
! Remove data spikes from thermocouple temperatures in DATA, overwriting the
! the data that was read from file. If successful, return IERR = 0; otherwise,
! return data subscript of last smoothed point in IERR, corresponding time in
! END_TIME string.
 
! This routine is used to smooth data for interesting periods during high-power
! pulse operation.  Uses larger tolerances and no linear regression so as to not
! remove larger temperature fluctuations that occur in this mode of operation.
!
! All arguments except END_TIME and IERR are inputs. RISE and DROP are the user-
! selected tolerances for temperature gain/loss from one data point to the next.
! NSMOO is the time tolerance (number of points) to be searched for the next
! valid data following a bad value;
! TBT ZXMIN and ZXMAX are the minimum and maximum values of X, between which
!     spikes will be removed.  2/8/91
! N1 and N2 are the indices of the data range
! to be smoothed.
!	I took out END_TIME & IERR since they are not used in the calling routine.
 
 
      SUBROUTINE JANSMO (DATA,  TIME,   NSAMPS, RISE,DROP,NSMOO,
     .		               ZXMIN, ZXMAX,  IFOUND)        !
      REAL DATA(NSAMPS)			!Temp.samples for 1 thermocouple
      REAL TIME(NSAMPS)			!Times (in hrs) of temp. samples
 
C	CHARACTER*8 END_TIME			!Time of point causing failure
 
      !---------------------------------------------------------------!
 
C	IERR = 0				!Returned non-0/nonblank if
C	END_TIME = ' '				!smoothing algorithm fails
      IFOUND = 0
 
      IF (ZXMIN .GE. ZXMAX  .OR.  ZXMIN .GT. TIME(NSAMPS))  RETURN
      IF (NSMOO .LE. 1)      RETURN
 
 
C	I = N1
C	DO WHILE ((DATA(I).LE.0.OR.DATA(I).GE.300).AND.I.LT.N2)
C	    I = I + 1
C	ENDDO					!try to insure 1st point is a
C	IF (I.LT.N2) THEN			!reasonable value. If none
C	    DATA(1) = DATA(I)			!found, failure will be
C	ENDIF					!detected below...
C
C	--------------------------------------------------------
C
C	.Find X axis indices that include xmin and xmax.
 
      DO 20 J=1,NSAMPS
         N1 = J
         IF (TIME(N1) .GE. ZXMIN)  GO TO 21
   20   CONTINUE
      RETURN
 
   21 CONTINUE
      DO 30 J=1,NSAMPS
          N2 = NSAMPS-J+1
          IF (TIME(N2) .LE. ZXMAX)  GO TO 31
   30   CONTINUE
      RETURN
   31   CONTINUE
 
C	-------------------------------------------------------------------
 
      I = N1+1
 
100   UP = RISE				!loop to retain temperature
      DOWN = DROP				!values while fluctuations are
      DO WHILE (I.LE.N2)			!within ideal tolerance ranges
          X = DATA(I) - DATA(I-1)
          IF (X.GE.0) THEN			!check tolerance for temperature
              IF (X.GT.UP) GO TO 105		!increase
          ELSE				!or for temperature drop
      	IF (ABS(X).GT.DOWN) GO TO 105
          ENDIF
          I = I + 1
      ENDDO
						!Here when temp is outside
105   IF (I.GT.N2) RETURN			!initial tolerance range
 
      NPTS = NSMOO - 1			!No. of pts excluding current pt
						!to search for next good value
 
C	DO J = 1, 10
          LASTPT = I + NPTS			!Start w/smallest time window;
          IF (LASTPT.GT.N2) LASTPT = N2	!check points against last good
          DO L = I, LASTPT			!temperature
              N = L
      	X = DATA(N) - DATA(I-1)		!see if any data in current time
      	IF (X.GE.0) THEN		!window falls within current
      	    IF (X.LE.UP) GO TO 150	!temp tolerances.
      	ELSE
      	    IF (ABS(X).LE.DOWN) GO TO 150
      	ENDIF
          ENDDO
 
 
C	    NPTS = NPTS + (NSMOO * .5)		!if no data qualified, enlarge
C	    UP = UP + (RISE * .5)		!tolerances and time window
C	    DOWN = DOWN + (DROP * .5)		!and try again
C	ENDDO
 
C	SECONDS = TIME(I) * 3600		!here if no fit could be found.
C	CALL HH_MM_SS (SECONDS,END_TIME)	!Get time of last good point
C	END_TIME='00:00:99'
C	IERR = I				!and bail out
      i=i+1            		        ! TBT Added to get whole row
      if (i.le.n2)  go to 100                 ! deglitched. 2/14/91
      RETURN
	
 
150   IF (I.NE.N) THEN			!Fill gap between good points
          IFOUND = IFOUND + N - I
          CALL TFILL (DATA,TIME,I,N)		!with values that fall between
      ENDIF					!the values of the good points.
      I = I + 1
      IF (I.LE.N2) GO TO 100
 
      RETURN
      END
