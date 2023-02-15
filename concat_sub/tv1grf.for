C******************** START FILE TV1GRF.FOR ; GROUP CONCAT ******************
C------------------------------------------------------------
C  TV1GRAF
C
C  GRAPH LASER PROFILES IN INPUT BUFFER
C
      SUBROUTINE TV1GRF
C
      USE CONCAT_MOD
C
      CHARACTER*10 ZEXT
      character*40 zcbuff
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR,lxout
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
C------------------------
C
      luntrm=lnout(0)
 10   CONTINUE
      CALL NEUPAG
      IF(MODEI.EQ.1) THEN
C
C--/INPUT BY SHOT NO. MODE
C
        IF(LXOUT(0)) THEN
          IF(NSHOT2.GT.0) THEN
            WRITE(LUNTRM,2000) NSHOT2
 2000   FORMAT(/' ENTER "S" TO PLOT F(X,Y) INPUT DATA FROM SHOT ',I6)
          ENDIF
          WRITE(LUNTRM,2001)
 2001 FORMAT(/' ENTER SHOT NUMBER OF INPUT PROFILE TO GRAPH,',
     >  ' OR 0 TO QUIT:'/)
        ENDIF
C
 15     CALL UREAD('TV1GRF:  ENTER 5 DIGIT SHOT NUMBER:$')
C
        IF(LCHAR('Q')) RETURN
C
        IF(NSHOT2.GT.0) THEN
          IF(LCHAR('S')) GO TO 100
        ENDIF
C
        ISHOT=IDCOD(IERT)
        IF(IERT.NE.0) GO TO 15
        IF(ISHOT.EQ.0) RETURN
C
        DO 20 I=1,NS
          IF(ISHOT.EQ.ISHOTS(I)) GO TO 50
 20     CONTINUE
C
      ELSE
C
C--/INPUT BY FILENAME EXTENSION MODE
C
        IF(LXOUT(0)) THEN
          IF(NSHOT2.GT.0) THEN
            WRITE(LUNTRM,2020) NSHOT2
 2020   FORMAT(/' ENTER "1" TO PLOT F(X,Y) INPUT DATA FROM SHOT ',I6)
          ENDIF
          WRITE(LUNTRM,2011)
 2011 FORMAT(/' ENTER FILENAME .EXT OF INPUT PROFILE TO GRAPH,',
     >  ' OR 0 TO QUIT:'/)
        ENDIF
C
 35     CALL UREADW('TV1GRF:  ENTER 3 CHARACTER FILENAME EXTENSION:$',
     >     zcbuff)
C
        IF((ZCBUFF(1:2).EQ.'0 ').OR.(ZCBUFF(1:2).EQ.'Q ')) THEN
          CALL UROTOR(1)
          RETURN
        ENDIF
C
        IF(NSHOT2.GT.0) THEN
          IF(ZCBUFF(1:2).EQ.'1 ') THEN
            CALL UROTOR(1)
            GO TO 100
          ENDIF
        ENDIF
C
        ilnb=min(len(zext),index(zcbuff,' ')-1)
        ZEXT(1:ilnb)=ZCBUFF(1:ilnb)
C
        DO 40 I=1,NS
          IF(ZEXT.EQ.XSHOTX(I)) GO TO 50
 40     CONTINUE
C
      ENDIF
C
C  ILLEGAL SHOT NUMBER OR BAD EXTENSION
      CALL UCLEAR
      CALL XSHOTS(1)
      GO TO 10
C
 50   CONTINUE
C
      IOFF=(I-1)*NX+1
C
      CALL GRFUF1(XIN,FIN(IOFF),NX,IPROC,
     >     FLAB,XLAB,ISHOT,IDEV,ZSDATE,1,YSHOTS(I),YLAB)
C
      GO TO 10
C--------------------------
C  PLOT 2D INPUT
C
 100  CONTINUE
C
      CALL GRF3(X,FIN,Y2,F2,NX,NY2,NX,XLAB,YLAB,FLAB,
     >            XLAB(3),YLAB(3),FLAB(3),FLAB,0,
     >            0,0,IDEV,NSHOT2,0,ISHOTX(1),1,0)
C
      GO TO 10
C
      END
C******************** END FILE TV1GRF.FOR ; GROUP CONCAT ******************
