C******************** START FILE UINDOP.FOR ; GROUP UREAD5 ******************
C=============================================================
C&  UINDOP   OPEN INDIRECT FILE FOR UREAD INPUT
C    ... SUBSTITUTE FOR
C           USER; CHECK THAT INVOKATION OF INDIRECT FILE OCCURS
C           AT CORRECT NODE OF CALLING PROGRAM
C
C  BBUFF    80A1  NODE/PROMPT FROM MAIN PROGRAM; INDIRECT FILE
C  NCHARA    # OF CHARACTERS TO "$" DELIMITER IN BBUFF
C           FIRST LINE MUST MATCH THIS FOR ACCESS TO BE ALLOWED
C  ZFILE    ASCII FILENAME
C  IER      ERROR RETURN CODE 0=NORMAL 1=ERROR,FILE CLOSED
C
C----
 
 
      SUBROUTINE UINDOP(ZFILE,IER,IFLAG)
 
 
C  Modifications:
C  05/15/95  tbt  Increased length of zfile to 130 from 64.
 
 
c  if iflag=2 buffered internal open is performed -- no file i/o
c  see subroutine ufiget
C *** BBUFF NOW IN COMMON ***
C
      USE USERIO
C
      CHARACTER*130  ZFILE        ! 130 arbitrary tbt
C
      ILFILN=INDEX(ZFILE,' ')-1
      IF(ILFILN.LE.0) ILFILN=LFILN
      IF((iflag.ne.2).and.(NLECHO.OR.(.NOT.NLINDI))) THEN
         if(ibatch.eq.0) WRITE(LUNTRM,2001) ZFILE(1:ILFILN)
 2001    FORMAT(' @FILENAME=',A)
      ENDIF
      IER=0
      JLUN=MAX0(NLUNIN,NLUNIO)+1
      IF(JLUN.LE.MAXLIN-1) GO TO 205  ! was MAXLIN; see uread_freelun below
      WRITE(LUNTRM,9001)
 9001 FORMAT(' ? TOO MANY INDIRECT FILES')
      IER=1
      RETURN
 205  CONTINUE
C  OPEN INDIRECT
      if(iflag.ne.2) THEN
        CALL URDOPEN(LUNTIN(JLUN),ZFILE,1,IERR)
        IF(IERR.NE.0) GO TO 207
      ENDIF
      GO TO 210
C  OPEN ERROR
 207  CONTINUE
      WRITE(LUNTRM,9002) ZFILE(1:ILFILN)
 9002 FORMAT(' ? ERROR TRYING TO OPEN INDIRECT FILE : ',A)
      CLOSE(UNIT=LUNTIN(JLUN),ERR=208)
 208  CONTINUE
      IER=1
      RETURN
C
 210  CONTINUE
C  OK
 216  CONTINUE
CXXX	IF(.NOT.NLINDI) CALL NEUPAG
      NLINDI=.TRUE.
      NLUNIN=JLUN
      XFILE(NLUNIN)=ZFILE
      CALL CHXOUT
      RETURN
C
      	END
C
C--------------------------------------------------------------------------
      subroutine uread_freelun(ilun)
C
      use USERIO
C
      integer :: ilun
C
C  return a FREE LUN for temporary file i/o use...
C
      ilun = luntin(maxlin)
C
      return
      end
C******************** END FILE UINDOP.FOR ; GROUP UREAD5 ******************
