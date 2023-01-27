C******************** START FILE LWORD.FOR ; GROUP URDUTS ******************
C---------------------------------------------
C&  LWORD   UREAD LOGICAL FCN .TRUE. IF INPUT MATCHES PASSED WORD
C
      LOGICAL FUNCTION LWORD(WORD)
C
      CHARACTER*(*) WORD
C
      IW=IXUOPW(WORD,1)
      IF(IW.EQ.1) THEN
        LWORD=.TRUE.
      ELSE
        LWORD=.FALSE.
      ENDIF
      RETURN
      END
C******************** END FILE LWORD.FOR ; GROUP URDUTS ******************
