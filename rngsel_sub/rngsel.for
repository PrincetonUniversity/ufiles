C******************** START FILE RNGSEL.FOR ; GROUP RNGSEL ******************
C-----------------------------------------------------------------------
C
C  RNGSEL
C
C  INTERACTIVE SELECTION ROUTINE
C
      SUBROUTINE RNGSEL(XLAB,X,LSEL,N)
C
      CHARACTER*10 XLAB(3)
      REAL X(N)
      LOGICAL LSEL(N),LMOD
C
C *LISMAK* 12-APR-88  GENERATED LOGICAL DECLARATIONS
      LOGICAL LCHAR
      LOGICAL LXOUT
C *LISMAK* END OF GENERATED LOGICAL DECLARATIONS
C
      character*8 zadded,zremvd,zlabel
C
      data zadded/'added.  '/
      data zremvd/'removed.'/
C
C--------------------------------------------------------------------
      zd=x(1)
      zd1=zd
      zd2=zd
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
      CALL NEUPAG
 11   CONTINUE
C
      IF(LXOUT(0)) WRITE(LUNTRM,1001) N,(XLAB(IJ),IJ=1,2),X(1),X(N),
     >     XLAB(3)
 1001 FORMAT(/' RNGSEL-- INDEX SUBSEQUENCE SELECTION ROUTINE'/
     >'  ... INITIAL RANGE OF INDICES IS 1 TO ',I5/
     >'  FOR ',2A10,' = ',1PE11.4,' TO ',1PE11.4,1X,A10/
     >'  PRECEDE OPCODE WITH A MINUS SIGN "-" FOR COMPLIMENTARY'/
     >'  OPERATION (DESCRIBED INSIDE PARENTHESES):'//
     >' -A-  ADD (DELETE) A SINGLE INDEX FROM SUBSEQUENCE'/
     >' -B-  ADD (DELETE) A RANGE OF INDICES FROM SUBSEQUENCE'/
     >' -C-  CLEAR - SELECT ENTIRE SEQUENCE (SELECT NULL SUBSEQUENCE)'/
     >' -D-  DISPLAY QUANTITIES ASSOCIATED WITH RANGE OF INDEX VALUES'/
     >' -P-  PRINT - PRINT SUBSEQUENCE OF SELECTED (DELETED) INDICES'/
     >' -Q-  QUIT ROUTINE AND RETURN SELECTED SUBSEQUENCE'/
     >'   ...new options September 1994...'/
     >' -S-  Specify real number, add (remove) nearest index pt'/
     >' -R-  Specify range, add (remove) indices therein')
C
      CALL UREAD('RNGSEL:  ENTER OPCODE  (+/-) A/B/C/P/Q:$')
      zlabel=zadded
      LMOD=.TRUE.
      IF(LCHAR('+')) THEN
        LMOD=.TRUE.
        zlabel=zadded
      ELSE IF(LCHAR('-')) THEN
        LMOD=.FALSE.
        zlabel=zremvd
      ENDIF
C
      IOP=IXUOPC('ABCPQDSR$')
      CALL UCHKOP(IOP,0,1,100,-1,*11)
C
      GO TO (100,200,300,400,500,600,700,800) IOP
C  SINGLE VALUE
 100  CONTINUE
      CALL UREADI('RNGSEL:  ENTER SINGLE INDEX VALUE (INTEGER):$',ID)
C
      IF((ID.LE.0).OR.(ID.GT.N)) GO TO 900
      LSEL(ID)=LMOD
      GO TO 10
C  RANGE
 200  CONTINUE
      CALL UREADI('RNGSEL:  ENTER RANGE LIMIT 1 (INTEGER):$',ID1)
      IF((ID1.LE.0).OR.(ID1.GT.N)) GO TO 900
      CALL UREADI('RNGSEL:  ENTER RANGE LIMIT 2 (INTEGER):$',ID2)
      IF((ID2.LE.0).OR.(ID2.GT.N)) GO TO 900
      IDA=MIN0(ID1,ID2)
      IDB=MAX0(ID1,ID2)
C
      DO 210 I=IDA,IDB
        LSEL(I)=LMOD
 210  CONTINUE
C
      GO TO 10
C
 300  CONTINUE
C  RESET
      CALL RNGSL0(LSEL,N,LMOD)
      GO TO 10
C
 400  CONTINUE
C  DISPLAY
      CALL RNGSPR(LUNTRM,LSEL,XLAB,X,N,LMOD)
      GO TO 10
C
C  EXIT
C
 500  CONTINUE
      RETURN
C
 600  CONTINUE
      CALL UREADI('RNGSEL:  ENTER RANGE LIMIT 1 (INTEGER):$',ID1)
      ID1=MIN0(N,MAX0(1,ID1))
      CALL UREADI('RNGSEL:  ENTER RANGE LIMIT 2 (INTEGER):$',ID2)
      IF((ID2.LT.1).OR.(ID2.GT.N)) ID2=ID1
C
      IDA=MIN0(ID1,ID2)
      IDB=MIN0((IDA+22),MAX0(ID1,ID2))
C
      CALL NEUPAG
C
      WRITE(LUNTRM,1600) (XLAB(IJ),IJ=1,3)
 1600 FORMAT(/' INDEX  VALUE (',2A10,', ',A10,')')
C
      DO 650 I=IDA,IDB
      WRITE(LUNTRM,1601) I,X(I)
 1601 FORMAT(1X,I5,2X,1PE12.5)
C
 650  CONTINUE
C
      CALL UPAUST(1)
      GO TO 10
C---------------------------
C  added dmc Sept 1994
C  SINGLE VALUE (real)
C
 700  CONTINUE
      CALL UREADF('RNGSEL:  ENTER SINGLE VALUE (REAL):$',ZD)
C
      zdmin=1.e35
      do ix=1,n
         if(abs(x(ix)-zd).lt.zdmin) then
            id=ix
            zdmin=abs(x(ix)-zd)
         endif
      enddo
C
      write(luntrm,7001) x(id),id,zlabel
 7001 format(' %rngsel:  value ',1pe11.4,' at index ',i5,1x,a)
C
      LSEL(ID)=LMOD
      GO TO 10
C  RANGE
 800  CONTINUE
      CALL UREADF('RNGSEL:  ENTER RANGE LIMIT 1 (REAL):$',ZD1)
      CALL UREADF('RNGSEL:  ENTER RANGE LIMIT 2 (REAL):$',ZD2)
      ZDMIN=MIN(ZD1,ZD2)
      ZDMAX=MAX(ZD1,ZD2)
C
      IDA=N+1
      IDB=0
C
      DO I=1,N
         IF(IDA.GT.N) THEN
            IF(X(I).GE.ZDMIN) IDA=I
         ENDIF
         IF(X(I).LE.ZDMAX) IDB=I
      ENDDO
C
      write(luntrm,8001) x(ida),x(idb),ida,idb,zlabel
 8001 format(' %rngsel:  range ',1pe11.4,' to ',1pe11.4,
     >    ' (indices ',i5,' to ',i5,') ',a)
C
      DO I=IDA,IDB
        LSEL(I)=LMOD
      ENDDO
C
      GO TO 10
C
C
C  ERROR
C
 900  CONTINUE
      WRITE(LUNTRM,9001) N
 9001 FORMAT(' ? INDEX SPECIFICATION NOT IN RANGE 1 TO ',I5)
      CALL UPAUST(1)
      GO TO 10
C
      END
C******************** END FILE RNGSEL.FOR ; GROUP RNGSEL ******************
