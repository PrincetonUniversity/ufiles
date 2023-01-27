C--------------------------------------------------------------------
      LOGICAL FUNCTION LDQUOT(IDUM)
C
C  RETURN .TRUE. IF TYPEAHEAD BUFFER LEAD ITEM IS A DBL QUOTE STRING,
C  OTHERWISE .FALSE.
C
      USE USERIO
C
      IF(CBUFF(1:1).EQ.'"') THEN
        LDQUOT=.TRUE.
      ELSE
        LDQUOT=.FALSE.
      ENDIF
C
      RETURN
      END
