C******************** START FILE UFCHAR.FOR ; GROUP URDUTS *************
C----------------------------------------------------------------
C&  UFCHAR  OLD UREAD FIND SPECIAL CHARACTER IN INPUT LINE
C
C   ZLINE-- UNPACKED DATA LINE 1 CHAR PER WORD
C   LLINE-- LENGTH OF LINE
C   ZCHAR-- SPECIAL CHARACTER SOUGHT
C   IND  -- (INPUT) LOCATION AT WHICH TO START SEARCH
C           (OUTPUT) LOCATION OF FIRST OCCURANCE OF ZCHAR IN ZLINE
C                    OR 0 IF NOT FOUND
C  Updated:
C	 5/20/92 TBT Ignore special character if preceeded by \.
C
      SUBROUTINE UFCHAR(ZLINE,ZCHAR,IND,LLINE)
C
      CHARACTER*1 ZLINE(LLINE),ZCHAR
      character*1 Backslsh
      Character*1 Lastchar
C
      data Backslsh /'\\'/  ! unix needs escaped backslash
C
      Lastchar = ' '
C
      IND0=IND
      IND=0
      IF(IND0.LT.1) IND0=1
      IF(IND0.GT.LLINE) RETURN
C  MOD DMC 1988 -- IF IN DBL QUOTES IT DOESN'T COUNT
      ICHK=1
      DO 10 I=IND0,LLINE
        IF(ZLINE(I).EQ.'"') ICHK=1-ICHK
        IF(ICHK.NE.1) GO TO 10
        IF(ZLINE(I).EQ.ZCHAR .and. Lastchar .NE. Backslsh) GO TO 20
        Lastchar = Zline(I)
 10   CONTINUE
 
C  CHAR NOT FOUND
      RETURN
 
C  CHAR FOUND
 20   CONTINUE
      IND=I
      RETURN
      	END
C******************** END FILE UFCHAR.FOR ; GROUP URDUTS ***************
