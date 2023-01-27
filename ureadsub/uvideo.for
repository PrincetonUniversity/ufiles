C-----------------------------------------------------------------------
C UVIDEO -- RETURN USER VT100 VIDEO DISPLAY OPTIONS (UREAD)
C
      SUBROUTINE UVIDEO(ILVID,ZCHVID,ICHVID)
C
      USE USERIO
C
      LOGICAL ILVID  ! RETURN .T IF FEATURE IS ENABLED
      CHARACTER*(*) ZCHVID  ! RETURN CHAR STRING OF SETTING
      INTEGER ICHVID  ! RETURN LENGTH OF CHAR STRING SET
C
C  UREAD COMMON:
C
C-----------------------------------------------------------------------
C
      ILVID=NLRVID
C
      ZCHVID=VIDCHS(1:NVIDCH)
      ICHVID=NVIDCH
C
      RETURN
      END
