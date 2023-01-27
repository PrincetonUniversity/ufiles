C******************** START FILE UERMSI.FOR ; GROUP URDUTS ******************
C------------------------------------------------------------
C& UERMSI  UREAD ERROR MESSAGE WITH INTEGER VALUE ECHO
      SUBROUTINE UERMSI(MSG,INT,ICOD)
C
      CHARACTER*(*) MSG
      CHARACTER*20 IBUF
C
      WRITE(IBUF,1001,ERR=10) INT
 1001 FORMAT(I20)
      GO TO 20
C
 10   CONTINUE
      IBUF='**.GT.I20*'
C
 20   CALL UERMSG(MSG,IBUF,ICOD)
      RETURN
      END
C******************** END FILE UERMSI.FOR ; GROUP URDUTS ******************
