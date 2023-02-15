C******************** START FILE INDUAD.FOR ; GROUP INORD ******************
C
C--------------------------------------------------------------
C  INDUAD
C   TACK A PT ONTO THE END OF AN ARRAY
C
      SUBROUTINE INDUAD(ZA,NA,N,Z,IER)
C
      REAL ZA(NA)
C
      IF(N.GE.NA) THEN
        IER=1
      ELSE
        IER=0
        N=N+1
        ZA(N)=Z
      ENDIF
      RETURN
      END
C******************** END FILE INDUAD.FOR ; GROUP INORD ******************
