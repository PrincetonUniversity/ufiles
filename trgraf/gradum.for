C-----------------------------------------------------------------------
C  GRADUM -- DUMMY GRAOPT CONTROL SUBROUTINE
C    THIS SUBROUTINE IS PASSED AS AN ADDRESS BY MOST GRAOPT CALLERS
C    IT SHOULDN'T REALLY BE CALLED -- BUT IT COMPLETES THE GRAOPT
C    ARGUMENT LIST
C
      SUBROUTINE GRADUM(ICALL,CTRL,IOPCOD)
C
      CHARACTER*(*) CTRL
C
C-----------------------------------------------------------------------
C
      LUNTRM=LNOUT(0)
      IL=MIN0(40,LEN(CTRL))
      WRITE(LUNTRM,9901) ICALL,CTRL(1:IL)
 9901 FORMAT(/' ?? PROGRAMMING ERROR:  IMPROPER CALL TO GRADUM'/
     >    '    ICALL=',I20/'    CTRL="',A,'"'/)
C
      RETURN
      END
