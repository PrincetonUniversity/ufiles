C******************** START FILE TV2GRF.FOR ; GROUP CONCAT ******************
C
C---------------------------------------------------------
C  TV2GRF
C
C  GRAPH 2D OUTPUT ARRAY
C
C
      SUBROUTINE TV2GRF
C
      USE CONCAT_MOD
      DIMENSION ZWK(NXMAX)
C
      luntrm=lnout(0)
      IF(NYO.GE.2) GO TO 10
C
      WRITE(LUNTRM,9001)
 9001 FORMAT(/' ?  TOO FEW TIME POINTS.  NO OUTPUT ARRAY'/)
      CALL UPAUST(1)
      RETURN
C
 10   CONTINUE
C
      IF(NSHOT2.EQ.0) THEN
        IAV=1
        INSI=NSO
      ELSE
        IAV=1
        INSI=MAX0(1,NSO)
        IISHOT=ISHOTX(1)
        ISHOTX(1)=NSHOT2
      ENDIF
C
      CALL GRF3(X,F,Y,ZWK,NXO,NYO,NXO,XLAB,YLAB,FLAB,
     >            XLAB(3),YLAB(3),FLAB(3),FLAB,0,
     >            0,0,IDEV,NSHOTO,IAV,ISHOTX(1),INSI,0)
C
      IF(NSHOT2.NE.0) THEN
        ISHOTX(1)=IISHOT
      ENDIF
C
      RETURN
      	END
C******************** END FILE TV2GRF.FOR ; GROUP CONCAT ******************
