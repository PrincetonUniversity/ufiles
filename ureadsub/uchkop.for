C******************** START FILE UCHKOP.FOR ; GROUP URDUTS ******************
C----------------------------------------------------------
C
C& UCHKOP   UREAD CHECK OPCODE, GENERATE ERROR MESSAGE IF BAD
C
      SUBROUTINE UCHKOP(IOPCOD,IERCOD,ILIML,ILIMH,ICTL,*)
C
      USE USERIO
C
      IF((IERCOD.EQ.0).AND.(IOPCOD.GE.ILIML).AND.(IOPCOD.LE.ILIMH))
     >     RETURN
      CALL UERMSG('% ILLEGAL OPCODE',' ',ICTL)
      RETURN 1
      END
C******************** END FILE UCHKOP.FOR ; GROUP URDUTS ******************
