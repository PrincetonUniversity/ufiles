C******************** START FILE UDECOP.FOR ; GROUP URDUTS ******************
c--------------------------------------------------
C&  UDECOP   UNPACK UREAD PROMPT
C
      SUBROUTINE UDECOP(ABUFFI)
C
      USE USERIO
C
      CHARACTER*(*) ABUFFI
C
C  MOD DMC-- IGNORE LACK OF TRAILING "$" IN PROMPT -- JAN 1988
C
C  also limit the length of program prompts to 130 characters
      integer, parameter :: ilbuf = 130
C
C----------------------------------------------------
C
      ILEN=LEN(ABUFFI)
C
      NL1LIN=.FALSE.
C
C---------------
C  FIND LAST CHAR WHICH IS NEITHER BLANK NOR MDOLLR
C
      INBLANK=0
      DO 5 IC=ILEN,1,-1
        IF(ABUFFI(IC:IC).NE.' ') INBLANK=1
        IF((INBLANK.EQ.1).AND.(ABUFFI(IC:IC).NE.MDOLLR))
     >      GO TO 6
 5    CONTINUE
      IC=0
 6    CONTINUE
C
      ICTES=MIN0(ILEN,(IC+1))
C
      IF(ABUFFI(ICTES:ICTES).NE.MDOLLR) THEN
        INDOL=0
      ELSE
        INDOL=ICTES
      ENDIF
C
      IF(INDOL.GT.0) GO TO 10
C
      INDOL=ILEN+1
      GO TO 15
C
C---------------------------------
 10   CONTINUE
C  CHECK FOR DOUBLE "$" FOR ONE LINE PROMPT/ANSWER COMBO
      IF(ILEN.GT.INDOL) THEN
        IDP=INDOL+1
        IF(ABUFFI(IDP:IDP).EQ.MDOLLR) NL1LIN=.TRUE.
      ENDIF
C
 15   CONTINUE
      IF(INDOL.LE.ILBUF) GO TO 20
C
        WRITE(LUNTRM,9002) ILBUF
 9002   FORMAT(' ? PROMPT PASSED TO SUBROUTINE UREAD'/
     >           '   EXCEEDS LIMIT OF ',I3,' CHARACTERS -- TRUNCATED')
C
        INDOL=ILBUF
C
 20   CONTINUE
C
      NCHARA=INDOL-1
C
      ICHARA=MAX0(1,NCHARA)
      DO 60 I=ICHARA,LBUFF
        BBUFF(I)=' '
 60   CONTINUE
C
      IF(NCHARA.EQ.0) GO TO 100
C
      READ(ABUFFI,1002) (BBUFF(I),I=1,ICHARA)
 1002 FORMAT(132A1)
C
 100  CONTINUE
C
      NCP1A=NCHARA+1
      NCP2A=NCHARA+2
C  INSERT '$'
      BBUFF(NCP1A)=MDOLLR
C  DEFINE ABUFF
      ABUFF = ' '
      WRITE(ABUFF,1002) (BBUFF(IJ),IJ=1,NCP1A)
C
      RETURN
      	END
C******************** END FILE UDECOP.FOR ; GROUP URDUTS ******************
