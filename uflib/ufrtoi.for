C******************** START FILE UFRTOI.FOR ; GROUP UFCMPR ******************
C--------------------------------------------------------------------
        SUBROUTINE UFRTOI(NBITS,NWORDS,CHAN,
     >		ICHAN,SCALE,OFFSET,IER)
c
c	This routine takes an array of real*4 words and
c	converts them to an array of integer*2 words of
c	given bit significance.
c	The transformation is of the form:
c		R = S * I + O		(linear)
c
c	INPUT:
c		NBITS	the number of significant bits in the integer I.
c		NWORDS	the length of the input array.
c		CHAN	the input array of real*4 data items [R];
c	OUTPUT:
c		ICHAN	the output array of integer*2 data items [I]
c		SCALE	the resulting scale factor [S]
c		OFFSET	the resulting offset [O]
c		IER	=0 for everything OK
c			=2 for data all zeros.
C
      REAL*4 CHAN(*)
      INTEGER*2 ICHAN(*)
C
      ZFULLS = 2**NBITS - 1
C
C--	FIND RANGE; SET SCALE & OFFSET:: R = SCALE * I + OFFSET
C
      CMAX = CHAN(1)
      CMIN = CMAX
C
      DO I = 2,NWORDS
        CMAX = MAX(CMAX,CHAN(I))
        CMIN = MIN(CMIN,CHAN(I))
      ENDDO
C
      OFFSET = CMIN
      RANGE = CMAX - CMIN
      SCALE = RANGE / ZFULLS
      IF (RANGE.NE.0.) THEN
        SCALI = ZFULLS / RANGE
      ELSE
        SCALI = 1.0
      ENDIF
C
      DO I=1,NWORDS
C
        RIN = CHAN(I)
        ICHAN(I) = (RIN - OFFSET) * SCALI  + 0.5
      ENDDO
C
      IER = 0
C
 8000 CONTINUE
      RETURN
      END
C******************** END FILE UFRTOI.FOR ; GROUP UFCMPR ******************
