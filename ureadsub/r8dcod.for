! 05may1999 fgtok -impno -s r8_precision.sub foo.sub "r8con.csh conversion"
C******************** START FILE FDCODE.FOR ; GROUP URDUTS *************
C==========================================================
C&  FDCODE - UREAD DECODE CHARACTER TO REAL - OBSCELESCENT
C
C  DECODE E10.0 INPUT IN UNPACKED BUFFER
C  REV D. MC CUNE 3-JUN-80 ... CHECK FOR ILLEGAL CHARACTERS
C
      REAL*8 FUNCTION r8dcod(IER)
C
C  REVISED D. MC CUNE JULY 14 1980  ADD ERROR CODE
C   IER=0 DENOTES SUCCESSFUL DECODING OF E10 FORMAT
C     IER=1 DENOTES DETECTION OF ILLEGAL CHARACTER OR FORMAT
C      FCN VALUE OF 0.0 RETURNED
C
      USE USERIO
      IMPLICIT NONE
C
C----------------------------------------------------------
C
C============
C idecl:  explicitize implicit INTEGER declarations:
      INTEGER ic,id,icod,ier
C============
C idecl:  explicitize implicit REAL declarations:
      REAL*8 z,ur8dcod
C============
      Z=ur8dcod(CBUFF,IC,ID,ICOD)
C
      IF(ICOD.EQ.-1) THEN
        IER=0
        r8dcod=0.0D0
      ELSE IF(ICOD.EQ.0) THEN
        IER=0
        r8dcod=Z
      ELSE
        GO TO 900
      ENDIF
C  SUCCESSFULLY DECODED
      CALL UROTOR(ID)
      RETURN
C  ERROR
 900  CONTINUE
      r8dcod=0.0D0
      IER=1
      CALL UERMSG(' % FDCODE:  NOT FLOATING POINT:',
     >     CBUFF(1:ID),0)
      IF(NLINDI) CALL UPAUST(1)
      RETURN
      	END
C******************** END FILE FDCODE.FOR ; GROUP URDUTS ***************
