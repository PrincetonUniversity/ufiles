C******************** START FILE LYESC.FOR ; GROUP URDUTS ******************
C------------------------------------------------
C&LYESC  UREAD LOGICAL FUNCTION, .TRUE. IF INPUT IS SPECIFIEC CHARACTER
C  FALSE OTHERWISE.
C
      LOGICAL FUNCTION LYESC(ICHAR)
      USE USERIO
      CHARACTER*1 ICHAR
C
      character*1 jchar
C
      CHARACTER*1 ZCHAR
C--------------------------------------
      CALL URCHAR(ZCHAR,1,ID,IER)
      call uupper(zchar)
      jchar=ichar
      call uupper(jchar)
C
      LYESC = zchar.eq.jchar
C
      CALL UROTOR(ID)
C
      RETURN
      END
C******************** END FILE LYESC.FOR ; GROUP URDUTS ******************
