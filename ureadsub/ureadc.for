C******************** START FILE UREADC.FOR ; GROUP UREADT ******************
C& UREADC  ISSUE UREAD PROMPT, RETURN CHARACTER -- UPPER CASE
      subroutine ureadc(prompt,char)
      character*(*), intent(in) :: prompt
      character*1, intent(inout) :: char
c
      call ureadic(prompt,char,1)
c
      return
      end
C
C& UREADLC  ISSUE UREAD PROMPT, RETURN CHARACTER -- Case unchanged
      subroutine ureadLc(prompt,char)
      character*(*), intent(in) :: prompt
      character*1, intent(inout) :: char
c
      call ureadic(prompt,char,0)
c
      return
      end
C
C----------------------------------------------------------
C& UREADC  ISSUE UREAD PROMPT, RETURN CHARACTER -- upper or lower case
C
      SUBROUTINE UREADIC(PROMPT,CHAR,iup)
C
      USE USERIO
C
      CHARACTER*(*), intent(in) :: PROMPT
      CHARACTER*1, intent(inout) :: CHAR
      integer, intent(in) :: iup
C
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
      IF(NLXOUT) WRITE(LUNTRM,1001) CHAR
 1001 FORMAT(' [OLD VALUE:  "',A,'"]')
C
      CALL UREAD(PROMPT)
C
      IF(LCHAR(' ').OR.LCHAR(',')) THEN
        IF(NLXOUT) WRITE(LUNTRM,1002)
 1002 FORMAT(' % OLD VALUE RETAINED')
      ELSE
        CALL URCHAR(CHAR,iup,ID,IER)
        CALL UROTOR(ID)
        IF(IER.NE.0) CALL UERMSG(
     >'?UREADC:  INPUT STRING NOT VALID AS SINGLE CHARACTER INPUT',
     >UWORD(1:LUWORD),1)
 
      ENDIF
C
      RETURN
      END
C******************** END FILE UREADC.FOR ; GROUP UREADT ******************
