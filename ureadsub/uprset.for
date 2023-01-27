C--------------------------------------------------------------------
C  UPRSET -- SET UREAD PROMPT STYLE
C
      SUBROUTINE UPRSET(STR)
C
      USE USERIO
C
      CHARACTER*(*) STR
C
      IF((STR.EQ.'V').OR.(STR.EQ.'R')) THEN
        NLRVID=.TRUE.
        VIDCHS='R'
        NVIDCH=1
      ELSE IF(STR.EQ.'B') THEN
        NLRVID=.TRUE.
        VIDCHS='B'
        NVIDCH=1
      ELSE IF(STR.EQ.'U') THEN
        NLRVID=.TRUE.
        VIDCHS='U'
        NVIDCH=1
      ELSE
        NLRVID=.FALSE.
        VIDCHS='N'
        NVIDCH=1
      ENDIF
C
      RETURN
      END
