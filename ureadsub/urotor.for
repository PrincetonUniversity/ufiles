C******************** START FILE UROTOR.FOR ; GROUP URDUTS ******************
C-------------------------------------------------------------
C&  UROTOR  ROTATE USED INPUT OUT OF UREAD TYPEAHEAD BUFFER.
C
C  IF "TYPE-AHEAD" IS SET, ROTATE OUT ALREADY PROCESSED PART OF
C  INPUT BUFFER; OTHERWISE CLEAR ENTIRE BUFFER
C
      SUBROUTINE UROTOR(IC)
      USE USERIO
C
      IF(NLHOLD) THEN
        CALL UCLEAR
      ELSE
        CALL NUMCLR(IC)
      ENDIF
      RETURN
      END
C******************** END FILE UROTOR.FOR ; GROUP URDUTS ******************
