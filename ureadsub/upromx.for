C******************** START FILE UPROMX.FOR ; GROUP UREAD5 ******************
C
C& UPROMX  RECORD PROGRAM PROMPT IN UREAD OUTPUT FILE
      SUBROUTINE UPROMX
      USE USERIO
      IF(NCHARA.LE.0) RETURN
      IF(NLINDO.AND.(.NOT.NLINDI)) then
         icp1a=min(131,NCP1A)
         WRITE(LUNIDO,5000) MSEMI,(BBUFF(IJ),IJ=1,icp1a)
      endif
 5000 FORMAT(132A1)
      RETURN
      END
C******************** END FILE UPROMX.FOR ; GROUP UREAD5 ******************
