C-----------------------------------------------------------------------
C  DCUTF1 -- EXECUTE A DEGLITCHING OPERATION
C
      SUBROUTINE DCUTF1(X,Y,YF,N,IALGOR,IALGORB,JCUT,XCUT,YCUT)
C
      REAL X(N),Y(N),YF(N)
C
      REAL XCUT(2),YCUT(2)
      INTEGER JCUT(2)
C
C  INPUT X(N),Y(N)  --  FUNCTION TO BE DEGLITCHED
C   OUTPUT YF(N)  -- DEGLITCHED FUNCTION
C
C  INPUT IALGOR  -- DEGLITCHING INTERPOLANT FORM
C         =1:  WEIGHTED REFLECTION
C         =2:  LINEAR SEGMENT
C
C  INPUT JCUT(2) -- INDICES SPECIFYING DEGLITCHING RANGE
C    DATA FROM Y(JCUT(1)) THROUGH Y(JCUT(2)) TO BE REPLACED.
C     JCUT(1).LT.JCUT(2) EXPECTED
C
C  OUTPUT XCUT(2):  MIN,MAX X AFFECTED
C  OUTPUT YCUT(2):  MIN,MAX Y AFFECTED
C
C-----------------------------------------------------------------------
C
      YCUT(1)=Y(JCUT(1))
      YCUT(2)=YCUT(1)
C
      XCUT(1)=X(JCUT(1))
      XCUT(2)=X(JCUT(2))
C
      JPLUS=JCUT(2)+1
      JMINUS=JCUT(1)-1
      NREP=JCUT(2)-JCUT(1)+1
C
      DO 160 J=JCUT(1),JCUT(2)
        IF((JPLUS.LE.N).AND.(JMINUS.GE.1)) THEN
          JOFFL=J-JCUT(1)+1
          JOFFR=JCUT(2)-J+1
          JRFLL=MAX0(1,(JMINUS-JOFFL))
          JRFLR=MIN0(N,(JPLUS+JOFFR))
          WGHTL=1.0-FLOAT(JOFFL)/(NREP+1)
          WGHTR=1.0-WGHTL
          if(ialgorb.eq.1) then
             IF(IALGOR.EQ.1) THEN
      	  YF(J)=WGHTL*(Y(JMINUS)-(Y(JRFLL)-Y(JMINUS)))
     1	     +WGHTR*(Y(JPLUS)-(Y(JRFLR)-Y(JPLUS)))
             ELSE
      	  YF(J)=WGHTL*Y(JMINUS)+WGHTR*Y(JPLUS)
             ENDIF
          else
             if(ialgor.eq.1) then
      	  yf(j)=y(jminus)
             else
      	  yf(j)=y(jplus)
             endif
          endif
        ELSE IF(JPLUS.GT.N) THEN
          JOFFL=J-JCUT(1)+1
          if(ialgorb.eq.1) then
             IF(IALGOR.EQ.1) THEN
      	  JRFLL=MAX0(1,(JMINUS-JOFFL))
      	  YF(J)=Y(JMINUS)-(Y(JRFLL)-Y(JMINUS))
             ELSE
      	  JRFLL=MAX0(1,(JMINUS-1))
      	  YF(J)=Y(JMINUS)-JOFFL*(Y(JRFLL)-Y(JMINUS))
             ENDIF
          else
             yf(j)=y(jminus)
          endif
        ELSE IF(JMINUS.LT.1) THEN
          JOFFR=JCUT(2)-J+1
          if(ialgorb.eq.1) then
             IF(IALGOR.EQ.1) THEN
      	  JRFLR=MIN0(N,(JPLUS+JOFFR))
      	  YF(J)=Y(JPLUS)-(Y(JRFLR)-Y(JPLUS))
             ELSE
      	  JRFLR=MIN0(N,(JPLUS+1))
      	  YF(J)=Y(JPLUS)-JOFFR*(Y(JRFLR)-Y(JPLUS))
             ENDIF
          else
             yf(j)=y(jplus)
          endif
        ENDIF
        YCUT(1)=AMIN1(YCUT(1),Y(J),YF(J))
        YCUT(2)=AMAX1(YCUT(2),Y(J),YF(J))
 160  CONTINUE
C
      RETURN
      END
