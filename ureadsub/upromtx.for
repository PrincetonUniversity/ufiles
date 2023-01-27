C-----------------------------------------------------------------------
C  UPROMTX -- DO PROMPTING OUTPUT, WITH REVERSE VIDEO IF FLAG IS SET
C
      SUBROUTINE UPROMTX(ZBUFF,ILENP)
C
      USE USERIO
C
      CHARACTER*(*) ZBUFF	  ! STRING TO USE FOR PROMPTING
C
      INTEGER ILENP		  ! PASSED LENGTH (IF .GT. 0)
C
C
      character*12 zfmt,zadvance
C--------------------------------------
C
      ILEN=LEN(ZBUFF)
      IF((ILENP.GT.0).AND.(ILENP.LT.ILEN)) ILEN=ILENP
C
C  REVERSE VIDEO OPTION
C
      ICHNG=0
      IF(NLRVID) CALL UTKSET(VIDCHS(1:NVIDCH),ICHNG)
C
CXX      if(nl1lin) then
CXX         zadvance="NO"
CXX      else
CXX         zadvance="YES"
CXX      endif
C
C  dmc: zadvance="NO" -- too many portability problems...
C
      zadvance="YES"            ! always
C
      zfmt = '(A)'
C
      WRITE(LUNTRM,zfmt,ADVANCE=zadvance) ZBUFF(1:ILEN)
      call flush(luntrm)
C
      IF(NLRVID) then
         CALL UTKSET('N',ICHNG)
CXX         if(zadvance.eq.'NO') write(luntrm,'(1x)',ADVANCE="NO")
      endif
C
      RETURN
      END
