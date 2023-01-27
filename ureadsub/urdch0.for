C******************** START FILE URDCHR.FOR ; GROUP USINIT ******************
C--------------------------------------------------
C& URDCHR  DEFINED DEFAULT SET OF UREAD CONTROL CHARACTERS
C
      SUBROUTINE URDCH0
C
      USE USERIO
C
C  INITIALIZE DEFAULT SET OF UREAD SPECIAL CHARACTERS
      CALL URDCMC(';')  ! SET SEMICOLON AS COMMENT CHARACTER
C                         ! MSEMI AND MCOMMT SET IN URDCMC
      MDOLLR='$'
      MSLASH='/'
      MDOT='.'
      MEQS='='
      MCAROT='>'
      MIDAT='@'
      MSTAR='*'
C
      MQMARK='?'
      MCOLON=':'
      M1QUOT=''''
      M2QUOT='"'
      MLBRAK='{'
      MRBRAK='}'
C
      MFILI=MIDAT
      MFILO=MCAROT
      MBRNCH=MQMARK
      MTARGT=MCOLON
      MSUB1=M1QUOT
      MSUB2(1)=MLBRAK
      MSUB2(2)=MRBRAK
C
C  MPARAM AND MCMD SET IN URDCHR OR URDTCH ROUTINES ...
C
      RETURN
      END
C******************** END FILE URDCHR.FOR ; GROUP USINIT ******************
