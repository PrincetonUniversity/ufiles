C******************** START FILE UPSADG.FOR ; GROUP USPARM *************
C---------------------------------------------------------------
C  UPSADG   ADD DEFINITION OF A GLOBAL UREAD STRING PARAMETER
C     ARGUMENTS-- ZVAL 96 CHAR VALUE
C                 ZNAM  16 CHAR NAME
C
C     ZNAM forced to uppercase
C     ZVAL NOT forced to uppercase
C
      SUBROUTINE UPSADG(ZNAM,ZVAL)
 
C  Updated:
C	 5/20/92 TBT Melt "\"
 
 
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
  100 Continue
  101   Continue
 
C
      CALL USPCAP(ZNAM)
      IF(NPGC.GE.NPGMAX) GO TO 500
      NPGC=NPGC+1
      NP=NPGC
      PGLVAL(NP)=ZVAL
      PGLNAM(NP)=ZNAM
      IF(NLECHO) WRITE(LUNTRM,7001) ZNAM,ZVAL
 7001 FORMAT(' GLOBAL PARAMETER ASSIGNMENT SUCCESSFUL:'/
     >  '  "',A,'" = "',A,'"')
      RETURN
C
 500  CONTINUE
      WRITE(LUNTRM,9001) ZNAM
 9001 FORMAT(' % MAXIMUM GLOBAL PARAMETER COUNT EXCEEDED'/
     >  '   PARAMETER "',A16,'" LOST'/)
      CALL UPAUST(1)
      RETURN
      		END
C******************** END FILE UPSADG.FOR ; GROUP USPARM ***************
