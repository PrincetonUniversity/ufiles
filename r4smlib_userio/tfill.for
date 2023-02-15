 
 
 
!------------------------------------------------------------------------- TFILL
 
! Given I = subscript of point containing bad data and N = subscript of next
! good data following I, compute new data values at the times corresponding to
! the data between I-1 and N.  Returns I incremented to last subscript filled.
 
      SUBROUTINE TFILL (DATA,TIME,I,N)
      REAL DATA(*),TIME(*)
      DVAL = DATA(I-1)			!get last "good" data value and
      TVAL = TIME(I-1)			!corresponding time value,
      DDIF = DATA(N) - DVAL			!difference between the "good"
      TDIF = TIME(N) - TVAL			!time and data values, then
      DO WHILE (I.LT.N)			!compute new data values that
          PCT = (TIME(I)-TVAL)/TDIF		!are porportionally correct for
          DATA(I) = DVAL + DDIF * PCT		!the times in the "bad" data
          I = I + 1				!interval
      ENDDO
      RETURN
      END
