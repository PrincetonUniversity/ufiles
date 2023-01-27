C******************** START FILE UCLEAR.FOR ; GROUP URDUTS ******************
C-------------------------------------------------------------
C&  UCLEAR  CLEAR UREAD TYPEAHEAD BUFFER E.G. AFTER ERROR
C  CLEAR LINE INPUT BUFFER AND CHECK OUTPUT (PROMPTING) STATUS
C
      SUBROUTINE UCLEAR
C
      USE USERIO
C
      NCHARC=0
      CBUFF = '                                        '//
     >          '                                        '//
     >          '                                        '//
     >          '        '
C
      LUWORD=0
      UWORD = '                                        '//
     >          '                                        '//
     >          '                                        '//
     >          '        '
C
      CALL CHXOUT
      RETURN
      END
C******************** END FILE UCLEAR.FOR ; GROUP URDUTS ******************
