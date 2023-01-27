C******************** START FILE UFAHED.FOR ; GROUP UFILES ******************
C----------------------------------------------------------------------
C  UFAHED - READ/WRITE ASCII FILE HEADER
 
 
 
C  OBSOLETE - 8/24/93 TBT  Use UFAHED2.for to get Ascii data format Info
C  ________
 
C
C  DMC 14 NOV 1984-- SUPPORT 6 DIGIT SHOT NUMBER BUT READ OLD FILES
C    WITHOUT ERROR
C
      SUBROUTINE UFAHED(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZDATE,ZSDATE,
     >                    IWRITE,IERH)
C
C  ILUN - UNIT TO READ/WRITE
C  ISHOT - UP TO 6 DIGIT SHOT NUMBER
C  IDEV - 4 CHAR TOKAMAK CODE
C  IDIM - UFILES FILE DIMENSIONALITY
C  KMPRS - DATA COMPRESSION CODE
C  ZDATE - FILE WRITE DATE
C  ZSDATE - SHOT (DATA ORIGIN) DATE
C
C  IWRITE: = 0 ** READ **
C          = 1 ** WRITE **
C
C  OUTPUT IERH ERROR CODE 0=NORMAL, 1=ERROR
C   ROUTINE NOW CHECKS FOR ERROR ON READ
C
      CHARACTER*4 IDEV
      CHARACTER*10 ZDATE,ZSDATE
      CHARACTER*6 ZZSHOT
      CHARACTER*8 ZZBUF
C
C-----------------------------------------
C
C  WRITE ***
C
      IERH=1000
      write(6,'('' ?ufiles:  obsolete "ufahed" call.'')')
      RETURN
C
      END
C******************** END FILE UFAHED.FOR ; GROUP UFILES ******************
