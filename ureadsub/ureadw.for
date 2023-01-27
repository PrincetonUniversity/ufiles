C******************** START FILE UREADW.FOR ; GROUP UREADT ******************
C----------------------------------------------------------
C& UREADW  ISSUE UREAD PROMPT, RETURN BLANK/COMMA-DELIMITTED WORD
C          uppercase forced

      SUBROUTINE UREADW(PROMPT,WORD)
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*(*), intent(inout) :: WORD
    
      call ureadwi(prompt,word,1)

      return
      end
C----------------------------------------------------------
C& UREADW  ISSUE UREAD PROMPT, RETURN BLANK/COMMA-DELIMITTED WORD
C          uppercase NOT forced
C
C  dmc -- suppress parsing of "/" "parameter" strings-- usually want 
C         unix file path here...
C

      SUBROUTINE UREADLW(PROMPT,WORD)
C
      USE USERIO
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*(*), intent(inout) :: WORD
C
      character*1 zsave
    
      zsave = MPARAM
      MPARAM = char(0)

      call ureadwi(prompt,word,0)

      MPARAM = zsave

      return
      end
C----------------------------------------------------------
C
      SUBROUTINE UREADWI(PROMPT,WORD,iup)
C
      USE USERIO
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*(*), intent(inout) :: WORD
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
      ILW=LEN(WORD)
      IF(NLXOUT) THEN
        IF(ILW.LE.50) THEN
          WRITE(LUNTRM,1001) WORD
 1001     FORMAT(' [OLD VALUE:  "',A,'"]')
        ELSE
          WRITE(LUNTRM,1003) WORD(1:50)
 1003     FORMAT(' [OLD VALUE:  "',A,'..."]')
        ENDIF
      ENDIF
C
      CALL UREAD(PROMPT)
C
      IF(LCHAR(' ').OR.LCHAR(',')) THEN
        IF(NLXOUT) WRITE(LUNTRM,1002)
 1002 FORMAT(' % OLD VALUE RETAINED')
      ELSE
        CALL CDCODI(WORD,iup)
      ENDIF
C
      RETURN
      END
C******************** END FILE UREADW.FOR ; GROUP UREADT ******************
