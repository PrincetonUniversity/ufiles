C******************** START FILE UIOTOP.FOR ; GROUP UREAD5 ******************
C=====================================================
C&  UIOTOP  SUBROUTINE TO OPEN INDIRECT OUTPUT FILE
C
C    FILE RECORDS USER/PROGRAM INTERACTION FOR FUTURE
C    REFERENCE.  ALLOWS CREATION OF FILES WHICH
C    GIVE CAPABILITY OF AUTOMATIC CONTROL OF
C    INTERACTIVE MAIN PROGRAM USING "UREAD.FOR" ROUTINES
C
C   BBUFF-- PROGRAM PROMPT *** NOW IN COMMON ***
C   NCHARA-- # OF CHARACTERS IN PROMPT BEFORE "$" DELIMITER
C   ZFILE-- NAME OF FILE TO CREATE
C   IER  -- ERROR RETURN CODE IER=0 = SUCCESS
C                             IER=1 = COULDNT OPEN
 
 
      SUBROUTINE UIOTOP(ZFILE,IER)
C 
      USE USERIO
C
      CHARACTER*(*) ZFILE
C
      character*96 :: zuname,zudate
C
C  START OF EXECUTABLE CODE
C
      IER=0
C
        ILFILN=INDEX(ZFILE,' ')-1
        IF(ILFILN.LE.0) ILFILN=LFILN
C
      INLUN=MAX0(NLUNIN,NLUNIO)+1
      ILUN=LUNTIN(INLUN)
      WRITE(LUNTRM,2001) ZFILE(1:ILFILN)
 2001 FORMAT(' UIOTOP: >CREATING UREAD FILE "',A,'"')
      OPEN(UNIT=ILUN,file=ZFILE,status='OLD',ERR=235)
      WRITE(LUNTRM,9001)
      CLOSE(UNIT=ILUN)
      write(luntrm,*) '(moved to '//trim(zfile)//'~)'
      call frename(trim(zfile),trim(zfile)//'~',istat)
C  OK--  NEW FILE TO BE CREATED
 235  CONTINUE
      CALL URDOPEN(ILUN,ZFILE,2,IERR)
      IF(IERR.NE.0) GO TO 237
      NLINDO=.TRUE.
C
      NLUNIO=INLUN
      LUNIDO=ILUN

      call uspget('$DATE',zudate,idum)
      call uspget('$LOGIN',zuname,idum)
      write(lunido,'(A)') 
     >     '; date: '//trim(zudate)//' username: '//trim(zuname)
      call flush(lunido)

C  SAVE NAME OF OUTPUT INDIRECT FILE
      XFILE(INLUN)=ZFILE
      NCMND(INLUN)=0
C
      RETURN
C  ERROR
 237  CONTINUE
      WRITE(LUNTRM,9002) ZFILE(1:ILFILN)
      IER=1
      RETURN
C
C  FORMATS
C
 9001 FORMAT(' % UIOTOP: WARNING - FILE ALREADY EXISTS')
 9002 FORMAT(' ? UIOTOP: ERROR TRYING TO OPEN INDIRECT FILE : ' / A)
      	END
C******************** END FILE UIOTOP.FOR ; GROUP UREAD5 ******************
