C******************** START FILE UFAHED2.FOR ; GROUP UFILES ******************
C----------------------------------------------------------------------
C  UFAHED2 - READ/WRITE ASCII FILE HEADER
C
C
      SUBROUTINE UFAHED2(ILUN,ISHOT,IDEV,IDIM,KMPRS,ZDATE,ZSDATE,
     >                    IWRITE,IFORMAT,IERH)
 
 
C  TbT 24 Aug 1993 - Changed from UFAHED to implement E13.6 format write
 
C
C  ILUN - UNIT TO READ/WRITE
C  ISHOT - UP TO 6 DIGIT SHOT NUMBER
C  IDEV - 4 CHAR TOKAMAK CODE
C  IDIM - UFILES FILE DIMENSIONALITY
C  IFORMAT - Returned as  0 on read if "old" data format of E11.4
C            Returned as  1 on read if "new" data format of E13.6
C            Returned as 99 on read if error.
C            Returned as  1 on write.
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
      Character*1 CFORMAT
      Integer     IFORMAT
C
C-----------------------------------------
C
C  WRITE ***
C
      IFORMAT = 1    ! All writes done in E13.6 format after 8/24/93
      CFORMAT = '6'
      IERH=0
C
      IF(IWRITE.EQ.1) THEN
        WRITE(ILUN,3001) ISHOT,IDEV,IDIM,KMPRS,CFORMAT,IDIM,ZDATE
 3001   FORMAT(1X,I6,A4,1X,I1,1X,I1,1X,A1,T32,
     >    ';-SHOT #- F(X) DATA -UF',I1,'DWR- ',A10)
C
        WRITE(ILUN,3002) ZSDATE
 3002   FORMAT(1X,A10,T32,';-SHOT DATE-  UFILES ASCII FILE SYSTEM')
C
      ELSE IF(IWRITE.EQ.0) THEN
C
C  READ ***
C
        READ(ILUN,3003,ERR=9000,END=9000)
     1         ZZSHOT,IDEV,IDIM,KMPRS,CFORMAT,ZDATE
 3003   FORMAT(1X,A6,A4,1X,I1,1X,I1,1X, A1,42X,A10)
 
        If (CFORMAT .Eq. ' ') Then
          IFORMAT = 0
        Else If (CFORMAT .Eq. '6') Then
          IFORMAT = 1
        Else
C	    .Error - Field is not blank or 6.
          IFORMAT = 99
          Go To 9000
        End If
 
        READ(ILUN,3004,ERR=9000,END=9000) ZSDATE
 3004   FORMAT(1X,A10)
C
C  ** DEAL WITH POSSIBLE 5 OR 6 DIGIT SHOT NO. **
C
        ZZBUF=ZZSHOT//'  '
        IF(ZZSHOT(6:6).EQ.' ') THEN
          READ(ZZBUF,'(I5)',ERR=9000,END=9000) ISHOT
        ELSE
          READ(ZZBUF,'(I6)',ERR=9000,END=9000) ISHOT
        ENDIF
C
      ENDIF
C
      RETURN
C
C-------------------------------
C  ERROR EXIT
C
 9000 CONTINUE
      IERH=1
      RETURN
C
      END
C******************** END FILE UFAHED2.FOR ; GROUP UFILES ******************
