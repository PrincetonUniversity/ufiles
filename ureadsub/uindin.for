C******************** START FILE UINDIN.FOR ; GROUP UREAD5 ******************
 
      SUBROUTINE UINDIN(ZFILE,IOUTPT,IER)
 
C----------------------------------------------------------
C& UINDIN   ESTABLISH CONNECTION WITH NEWLY NAMED INDIRECT I/O FILE
C  PARSE AND PASS DOWN PARAMETER VALUES IF ANY ARE SUPPLIED
C
C  FORM OF INPUT LINE IS
C  "@<FILENAME>"  (NO PARAMETERS)
C    OR
C  "@<FILENAME>/PARM1=VAL1/PARM2=VAL2/.../PARMN=VALN
C                 (N PARAMETERS)
C  THIS ROUTINE IS CALLED WHEN THE FIRST CHARACTER OF THE INPUT
C  FILE IS "@" (MIDAT)
C
C---
C  Mod tbt May 1995 -- Increased length of Filename to 130 from 64.
C  mod tbt May 1995 -- Want to allow for unix machines
C                      @"/u/terpstra/filename"/parm1=33/parm2=39
C
C                      Added code to ignore '/' inside ".../..."
C       -- corrected dmc 12 June:  only do this for leading clause
C          in dbl quotes; make it so @file/arg1=val1/arg2="a 2nd arg"
C          works correctly.
C---
C
C  MOD DMC DEC 1989 -- IF INPUT FILE REF HAS /$NEW APPENDED, E.G.
C    "@SCRIPT/$NEW/PARM1=VAL1"
C  THEN FORCE THE INPUT FILE TO BE READ FROM DISK, NOT FROM
C  INTERNAL MEMORY BUFFER.  NOTE THE /$NEW MUST IMMEDIATELY FOLLOW
C  THE FILENAME
C
C  IOUTPT=1:  THIS IS OUTPUT FILE (O.W. IS INPUT)
C
      USE USERIO
      CHARACTER*130 ZFILE               ! 130 arbitrary tbt 5/95
C
      character*1 zbuff(urdbufl)
      logical ilnf(urdbufl)
C
      logical :: ilindi,jlindi
C
C-----------------------------------------------
C
C  ERROR IF NOT FOLLOWED BY FILENAME
C   use urdcapl logical array to identify filename
C   urdcapl(j)=.false. --> cbuff(j:j) is part of filename
C
      zbuff=dbuff
      ilen=ilnurd(cbuff)+2
      ilen=min(urdbufl,ilen)
      ilnf=.TRUE.
      call urdfcap(zbuff,ilnf,ilen)
C
      if(ilnf(2)) then
        WRITE(LUNTRM,9591)
        IER=1
        RETURN
      ENDIF
 9591 FORMAT(' % ZERO LENGTH INDIRECT FILE NAME')
C
C  save NLINDI at entry to this routine
C
      ilindi = nlindi
C
C  find limits of filename
C
      ifnbeg=2
      do ic=2,ncharc
         if(ilnf(ic)) go to 10
      enddo
      ic=ncharc+1
 10   continue
      ifnend=ic-1
C
      ifna=ifnbeg
      ifnb=ifnend
      if(cbuff(ifna:ifna).eq.'"') ifna=min(ifnb,(ifna+1))
      if(cbuff(ifnb:ifnb).eq.'"') ifnb=max(ifna,(ifnb-1))
C
C  CHECK FOR LOCAL PARAMETER DEFINITIONS
      indsl=index(cbuff(ifnend+1:),mslash)
      if(indsl.gt.0) indsl=ifnend+indsl
C
      INEW=0
 
      IF(INDSL.GT.0) THEN
        ILAST=INDSL-1
C
C-------------
        INDSL2=INDEX(CBUFF(INDSL+1:),MSLASH)
        IF(INDSL2.GT.0) THEN
          INDSL2=INDSL+INDSL2
        ELSE
          INDSL2=NCHARC+1
        ENDIF
C  CHECK FOR /$NEW SWITCH
        DO 100 IC=INDSL+1,INDSL2-4
          IF(CBUFF(IC:IC).EQ.'$') GO TO 105
 100    CONTINUE
        GO TO 110
 105    CONTINUE
        IF(CBUFF(IC:IC+3).EQ.'$NEW') INEW=1
 110    CONTINUE
C
C-------------
      ELSE
        ILAST=NCHARC
      ENDIF
C
      INLUNP=MAX0(NLUNIN,NLUNIO)
C  CREATE FILENAME
      CALL URWORD(CBUFF,0,ILENF,ILENR)
      ILENF=MIN0(ILENF,ILAST)
      ILENR=MIN0(ILENR,ILAST)
C
      zfile=cbuff(ifna:ifnb)
      ilf=ifnb-ifna+1
      CALL UFILX(ZFILE,ilf)
C
C  TRY TO OPEN FILE; CHECK FOR PROMPT MISMATCH
C   IF OK RECORD INDIRECT REFERENCE IN OUTPUT FILE
C
      IF(IOUTPT.NE.1) THEN
C  INPUT FILE...
        IF(INEW.EQ.1) THEN
          call ufiget(zfile,-1,ier,ieof)
        ELSE
          call ufiget(zfile,1,ier,ieof)
        ENDIF
      ELSE
C  OUTPUT FILE...
        CALL UIOTOP(ZFILE,IER)
      ENDIF
      IF(IER.NE.0) RETURN
      INLUN=MAX0(NLUNIN,NLUNIO)
C  PARSE AND SAVE VALUES OF LOCAL PARAMETERS (IF ANY)
      IF(INLUN.EQ.INLUNP) THEN
        CALL UERMSG('? UINDIN/UFIGET - UREAD PROGRAMMING ERROR',' ',1)
        IER=1
        RETURN
      ENDIF
C
C  INLINE STRING PARAMETER SYNTAX?
      IF(INDSL.EQ.0) GO TO 250
C  MUST DIRECTLY FOLLOW FILENAME
      IF(INEW.EQ.1) THEN
        INDSL=INDSL2
        ILENR=INDSL2-1
        IF(INDSL.GT.NCHARC) GO TO 250
      ENDIF
      IF((ILENR+1).NE.INDSL) GO TO 250
C
C  DEAL WITH INLINE STRING PARAMETERS
      CALL USPCHK(DBUFF,LBUFF,INDSL,ID1,ID2,ILENR)
      CALL UPPASS(DBUFF,INDSL,ILENR)
      IF(DBUFF(ILENR).EQ.MSLASH) DBUFF(ILENR)=' '
      CALL UFCHAR(DBUFF,MSLASH,INDSL,ILENR)
      IF(INDSL.GT.0) THEN
C
C  if invocation is from command line, make inline parameter assignments global
C
         JLINDI=NLINDI
         NLINDI=ILINDI
         CALL UPMASG(DBUFF,INDSL,ILENR,IER)
         NLINDI=JLINDI
      ENDIF
C
C
C  ROTATE OUT FILENAME
 250  CONTINUE
      CALL UROTOR(ILENR)
C  PUSH REMAINDER OF LINE ONTO TYPEAHEAD BUFFER STACK FOR LATER
C  REFERENCE-- ASSOCIATED WITH PREVIOUS I/O CHANNEL
      AHDBUF(INLUNP)=CBUFF
      IF(NLECHO.AND.(NCHARC.GT.0)) THEN
        WRITE(LUNTRM,2001) AHDBUF(INLUNP)(1:NCHARC)
 2001   FORMAT(
     >' INDIRECT FILE SUCCESSFULLY OPENED, TYPEAHEAD SAVED ON STACK:'/
     >1X,A)
      ENDIF
C
C  CLEAR BUFFER FOR NEW FILE
      CALL UCLEAR
C
 260  CONTINUE
      RETURN
      END
C******************** END FILE UINDIN.FOR ; GROUP UREAD5 ******************
