C******************** START FILE CDCOD.FOR ; GROUP URDUTS ******************
C------------------
C&  CDCOD   UREAD RETURN CHAR STRING OUT OF TYPEAHEAD TO CALLER
C           UPPERCASE forced
C
      subroutine cdcod(zstrng)
      character*(*), intent(out) :: zstrng

      CALL CDCODI(zstrng,1)

      return
      end
C------------------
C&  CDCODL   UREAD RETURN CHAR STRING OUT OF TYPEAHEAD TO CALLER
C           do NOT enforce UPPERCASE
C
      subroutine cdcodL(zstrng)
      character*(*), intent(out) :: zstrng

      CALL CDCODI(zstrng,0)

      return
      end
C------------------
C  RETURN A CHARACTER STRING TO CALLER FROM INPUT LINE BUFFER
C  iup controls whether uppercase is forced on non-quoted characters
C
      SUBROUTINE CDCODI(ZSTRNG,iup)
C
      USE USERIO
C
      CHARACTER*(*), intent(out) :: ZSTRNG
C
      ILEN=LEN(ZSTRNG)
C
C  CLEAR ZSTRNG
C
      DO 10 IC=1,ILEN
        ZSTRNG(IC:IC)=' '
 10   CONTINUE
C
C  FIND FIRST FIELD IN BUFFER
C
      CALL URWORD(CBUFF,iup,IC,ID)
C
      IF(IC.GT.0) CALL URWRDL(CBUFF(1:IC),ZSTRNG,ILNB)
C
      CALL UROTOR(ID)
C
      RETURN
      END
C******************** END FILE CDCOD.FOR ; GROUP URDUTS ******************
