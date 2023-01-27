C******************** START FILE UPSADL.FOR ; GROUP USPARM *************
C---------------------------------------------------------------
C&  UPSADL   ADD DEFINITION OF A LOCAL UREAD STRING PARAMETER
C     ARGUMENTS-- ZVAL 96 CHAR VALUE
C                 ZNAM  16 CHAR NAME
C
C     ZNAM forced to uppercase
C     ZVAL NOT forced to uppercase
C
      SUBROUTINE UPSADL(ZNAM,ZVAL)
 
C  Updated:
C	 5/20/92 TBT Melt "\" in value line.
C        6/18/92 DMC -- bugfix in "\" melt code
 
      USE USERIO
 
      CHARACTER*16 ZNAM
      CHARACTER*96 ZVAL
      Character*1 Backslsh
 
      Data Backslsh /'\\'/  !unix needs escaped backslash
 
C	.Loop in case more than one "\".  "\\" yields "\"
      Indy = 0
      Do 100 I=1,96
          Indp = Indy                                      !dmc
          Indy = Indy + INDEX (ZVAL(Indy+1:96), Backslsh)  !dmc
          If (Indy .EQ. Indp) GO TO 101                    !dmc
C	    .Melt Backslash
          ZVAL(Indy:96) = ZVAL(Indy+1:96)
 100  Continue
 101  Continue
 
C
      CALL USPCAP(ZNAM)
      IF(NPLC(MAX0(NLUNIN,NLUNIO)).GE.NPLMAX) GO TO 500
      NPLC(MAX0(NLUNIN,NLUNIO))=NPLC(MAX0(NLUNIN,NLUNIO))+1
      NP=NPLC(MAX0(NLUNIN,NLUNIO))
      PLCVAL(NP,MAX0(NLUNIN,NLUNIO))=ZVAL
      PLCNAM(NP,MAX0(NLUNIN,NLUNIO))=ZNAM
      IF(NLECHO) WRITE(LUNTRM,7001) ZNAM,ZVAL
 7001 FORMAT(' LOCAL PARAMETER ASSIGNMENT SUCCESSFUL:'/
     >  '  "',A,'" = "',A,'"')
      RETURN
C
 500  CONTINUE
      WRITE(LUNTRM,9001) ZNAM
 9001 FORMAT(' % MAXIMUM LOCAL PARAMETER COUNT EXCEEDED'/
     >  '   PARAMETER "',A16,'" LOST'/)
      CALL UPAUST(1)
      RETURN
      		END
C******************** END FILE UPSADL.FOR ; GROUP USPARM ***************
