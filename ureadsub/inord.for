C******************** START FILE INORD.FOR ; GROUP INORD ***************
C------------------------------------------------------------
C  INORD - INPUT A LIST OF NUMBERS - SORT IN ORDER
C
C  ******************************************************
C  *  INORD.FOR -- just input numbers from tty          *
C  *  INORDU.FOR -- input numbers from tty or Ufile     *
C  *    routines linked from different libraries        *
C  *    DMC 18 Sept 1992                                *
C  ******************************************************
C
      SUBROUTINE INORD(X,WK,NDIM,NUM,LABEL,IDUPL)
C
      CHARACTER*(*) LABEL
      REAL X(NDIM),WK(NDIM)
C
C  X(NDIM) - LIST OF INPUT FLOATING PT NUMBERS (MAY BE MODIFIED)
C   NUM    - NUMBER OF ENTRIES IN LIST (MAY BE MODIFIED)
C  LABEL --- FOR USER PROMPT (INPUT) (JUST A FEW CHARS)
C  IDUPL --- =1 TO ALLOW DUPLICATE VALUES IN LIST (INPUT)
C
C
      LOGICAL LCHAR
      LOGICAL LXOUT
C
      LUNT=LNOUT(0)
 10   CONTINUE
      CALL NEUPAG
 11   CONTINUE
      IF(LXOUT(0)) THEN
        WRITE(LUNT,1001) LABEL
 1001 FORMAT(' INORD (ORDERED LIST "',A,'" INPUT) OPTIONS:'/
     >' type a NUMBER to add a value to the list, OR:'/
     >' C TO CLEAR LIST '/
     >' D <NUMBER> TO REMOVE <NUMBER> FROM LIST'/
     >' R <NUMBER1> <NUMBER2> TO REMOVE RANGE OF NUMBERS FROM LIST'/
     >' L TO TYPE OUT LIST CONTENTS   '/' Q TO QUIT')
        IF(NUM.EQ.0) THEN
          WRITE(LUNT,1002)
        ELSE IF(NUM.EQ.1) THEN
          WRITE(LUNT,1003) X(1)
        ELSE
          WRITE(LUNT,1004) NUM,X(1),X(NUM)
        ENDIF
 1002 FORMAT(' % CURRENTLY LIST IS EMPTY')
 1003 FORMAT(' % 1 ENTRY IN LIST:  ',1PE13.6)
 1004 FORMAT(' % ',I4,' ENTRIES IN LIST, RANGE:  ',
     >1PE13.6,' TO ',1PE13.6)
      ENDIF
C
      CALL UREAD(
     >'INORD:  ENTER NEW ITEM OR OPTION FOR LIST "'//LABEL//'":$')
      IF(LCHAR('Q')) THEN
        RETURN
      ELSE IF(LCHAR('C')) THEN
        NUM=0
        GO TO 11
      ELSE IF(LCHAR('L')) THEN
        IF(NUM.EQ.0) GO TO 10
        WRITE(LUNT,1010) (X(IX),IX=1,NUM)
 1010 FORMAT(4(2X,1PE13.6))
        CALL UPAUST(1)
        GO TO 11
      ELSE IF(LCHAR('D')) THEN
        IF(NUM.EQ.0) GO TO 10
        CALL UREAD('INORD:  SELECT VALUE (DELETE BY CLOSEST MATCH):$')
          IF(LCHAR('Q')) GO TO 10
          XDEL=FDCOD(IER)
          IF(IER.NE.0) GO TO 11
C  DETERMINE ENTRY SELECTED FOR DELETION
          IDEL=1
          IF(NUM.GT.1) THEN
            DXDEL=ABS(X(1)-XDEL)
            DO IX=2,NUM
              DX=ABS(X(IX)-XDEL)
              IF(DX.LT.DXDEL) THEN
                IDEL=IX
                DXDEL=DX
              ENDIF
            ENDDO
          ENDIF
C  DELETE SELECTED ENTRY
          IF(NUM.EQ.IDEL) THEN
            NUM=NUM-1
          ELSE
            IDP1=IDEL+1
            DO IX=IDP1,NUM
              X(IX-1)=X(IX)
            ENDDO
            NUM=NUM-1
          ENDIF
        GO TO 10
      ELSE IF(LCHAR('R')) THEN
        IF(NUM.EQ.0) GO TO 10
        CALL UREAD('INORD:  SELECT DELETE RANGE VALUE 1::$')
          IF(LCHAR('Q')) GO TO 10
          XR1=FDCOD(IER)
          IF(IER.NE.0) GO TO 11
        CALL UREAD('INORD:  SELECT DELETE RANGE VALUE 2::$')
          IF(LCHAR('Q')) GO TO 10
          XR2=FDCOD(IER)
          IF(IER.NE.0) GO TO 11
C  ASSURE XR1.LE.XR2
        IF(XR1.GT.XR2) THEN
          XRTEMP=XR1
          XR1=XR2
          XR2=XRTEMP
        ENDIF
C  FIND INDEX RANGE TO DELETE
        IRDEL=0
        DO IR1=1,NUM
          IF(XR1.LE.X(IR1)) GO TO 15
        ENDDO
        GO TO 19
 15     CONTINUE
        DO IR2=IR1,NUM
          IF(XR2.LT.X(IR2)) GO TO 18
        ENDDO
        IR2=NUM+1
 18     CONTINUE
        IR2=IR2-1
        IRDEL=IR2-IR1+1
 19     CONTINUE
        CALL NEUPAG
        WRITE(LUNT,1015) IRDEL
 1015   FORMAT(/' % ',I5,' LIST ENTRIES DELETED'/)
        IF(IRDEL.EQ.0) GO TO 11
C  DELETE THEM
        IF(IR2.LT.NUM) THEN
          INUM=NUM-IRDEL
          DO I=IR1,INUM
            IOLD=I+IRDEL
            X(I)=X(IOLD)
          ENDDO
        ENDIF
        NUM=NUM-IRDEL
        GO TO 10
      ELSE
C  NEW ENTRY?
        XNEW=FDCOD(IER)
        IF(IER.NE.0) GO TO 11
C  CHECK LIMIT
        IF(NUM.EQ.NDIM) THEN
          CALL UERMSI(' % LIST FULL, NUMBER OF ENTRIES',NDIM,-1)
          GO TO 11
        ENDIF
C  INSERT IN ORDER
        IF(NUM.EQ.0) THEN
          NUM=1
          X(NUM)=XNEW
        ELSE IF(XNEW.GT.X(NUM)) THEN
          NUM=NUM+1
          X(NUM)=XNEW
        ELSE
          DO IX=1,NUM
            IF(XNEW.LE.X(IX)) GO TO 20
          ENDDO
 20       CONTINUE
          IF((XNEW.EQ.X(IX)).AND.(IDUPL.NE.1)) THEN
            CALL UERMSF(' % DUPLICATE ENTRY',XNEW,-1)
            GO TO 11
          ENDIF
          INS=IX
          DO IX=NUM,INS,-1
            X(IX+1)=X(IX)
          ENDDO
          X(INS)=XNEW
          NUM=NUM+1
        ENDIF
        GO TO 10
      ENDIF
      END
C******************** END FILE INORD.FOR ; GROUP INORD *****************
