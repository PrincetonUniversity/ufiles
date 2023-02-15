C******************** START FILE RNGSL0.FOR ; GROUP RNGSEL ******************
C----------------------------------------------------------------------
C  RNGSEL
C
C  ROUTINES TO MEDIATE INTERACTIVE SELECTION OF A FINITE SUBSEQUENCE
C   OF A MONOTONIC INDEX
C
C  I.E. STARTING WITH INDICES 1,2,3,...,N
C   USER MAY SPECIFY 3,5, 8 THRU 12, ... ,N-4 THRU N-2
C   OR ANY OTHER ORDERED SUBSEQUENCE.
C
C----------------------------------------------------------------------
C  RNGSL0
C
C  RESET SELECTION FLAGS TO ALL .TRUE. OR ALL .FALSE.
C
      SUBROUTINE RNGSL0(LSEL,N,LSET)
C
      LOGICAL LSEL(N),LSET
C
      DO 10 I=1,N
        LSEL(I)=LSET
 10   CONTINUE
      RETURN
      END
C******************** END FILE RNGSL0.FOR ; GROUP RNGSEL ******************
