C******************** START FILE USPDGL.FOR ; GROUP USPARM ******************
C---------------------------------------------------------------
C&  USPDGL   DELETE GLOBAL UREAD PARAMETER BY NUMBER
C
      SUBROUTINE USPDGL(NP)
C
      USE USERIO
C
C--------------------
C  "$MATCH" CANNOT BE DELETED (DMC 1/88)
C
      IF(PGLNAM(NP)(1:8).EQ.'$MATCH  ') GO TO 50
C
      NPT=NPGC
      IF(NP.EQ.NPT) GO TO 20
C
      NPTM1=NPT-1
      DO 10 J=NP,NPTM1
      JP1=J+1
      PGLNAM(J)=PGLNAM(JP1)
      PGLVAL(J)=PGLVAL(JP1)
 10   CONTINUE
C
 20   CONTINUE
      NPGC=NPT-1
      PGLNAM(NPT)=' '
      PGLVAL(NPT)=' '
C
 50   CONTINUE
      RETURN
      END
C******************** END FILE USPDGL.FOR ; GROUP USPARM ******************
