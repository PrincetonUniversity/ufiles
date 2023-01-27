C******************** START FILE GRFMBC.FOR ; GROUP GRFLBL ******************
C--------------------------------------------------------------------
C  GRFMBC
C  REPLACE TABS AND NULLS AND "$" WITH BLANKS IN THE PASSED STRING
C
      SUBROUTINE GRFMBC(STR)
      implicit none
C
      CHARACTER*(*) STR
C
      CHARACTER*1 NULL,TAB
      integer     il, i
C
      TAB=CHAR(9)
      NULL=CHAR(0)
C
      IL=LEN(STR)
      DO 10 I=1,IL
        IF(STR(I:I).EQ.NULL) THEN
          STR(I:I)=' '
        ELSE IF(STR(I:I).EQ.TAB) THEN
          STR(I:I)=' '
        ELSE IF(STR(I:I).EQ.'$') THEN
          STR(I:I)=' '
        ENDIF
 10   CONTINUE
C
      RETURN
      END
C******************** END FILE GRFMBC.FOR ; GROUP GRFLBL ******************
