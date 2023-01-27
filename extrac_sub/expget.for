C******************** START FILE EXPGET.FOR ; GROUP EXSUBS ******************
C----------------------------------------------------------------------
C  EXPGET
C  EXTRACT THE SLICE
C
      SUBROUTINE EXPGET(ZF,INX1,INX2,IXSEL,ILX1,ILX2,
     >     zintrp,ZSLICE,ZSLIC2,
     >     IMAX,ZXI,ZTARG,INTARG,LTARG,ZTRAP,IER)
C
      REAL ZF(INX1,INX2),ZSLICE(IMAX),ZXI(IMAX)
      REAL ZTARG(INTARG),ZTRAP(3,2)
C
c mod dmc sep 1996 -- added zintrp argument.  if zintrp.ge.0.0,
c interpolate btw slice ilx1 and ilx2, weight zintrp to ilx1, 1-zintrp
c to ilx2.
c
C MODIF DMC  OCT 1984-- INTERPOLATE OR EXTRAPOLATE DATA TO TARGET
C
C
C  ZF(INX1,INX2) -- INPUT 2D DATA
C  IXSEL - INPUT FLAG
C          =1 IF SLICE IS VS. INPUT DATA'S 1ST INDEPENDENT COORDINATE
C          =2 IF SLICE IS VS. INPUT DATA'S 2ND INDEPENDENT COORDINATE
C  ILX1,ILX2  INPUT AVERAGING INDEX LIMITS
C     IF OUTPUT PROFILE IS VS. X THESE LIMITS REFERENCE Y
C     AND VICE VERSA
C  ZSLICE(IMAX) -- ARRAY TO HOLD SLICE (OUTPUT)
C
C  IER OUTPUT COMPLETION CODE 0 INDICATES SUCCESS
C
      IER=0
      IF(IXSEL.EQ.1) THEN
        INX=INX1
      ELSE
        INX=INX2
      ENDIF
C
      IF(INX.GT.IMAX) THEN
        IER=1
        CALL UERMSG('? EXTRAC PROGRAM ERROR, SLICE BUFFER TOO SMALL',
     >                ' ',2)
        INX=IMAX
      ENDIF
C
C  CLEAR BUFFER
C
      DO IX=1,INX
        ZSLICE(IX)=0.0
      ENDDO
C
C  OUTER LOOP-- AVERAGING
C
      zwt=1.0
      DO IAV=ILX1,ILX2
C
C  INNER LOOP-- SLICE INDEX
C
        if(zintrp.ge.0.0) then
           if(iav.eq.ilx1) zwt=zintrp
           if(iav.eq.ilx2) zwt=1.0-zintrp
        endif
        DO ISL=1,INX
C
        IF(IXSEL.EQ.1) THEN
          ZSLICE(ISL)=ZSLICE(ISL)+ZF(ISL,IAV)*zwt
        ELSE
          ZSLICE(ISL)=ZSLICE(ISL)+ZF(IAV,ISL)*zwt
        ENDIF
C
        ENDDO
C
      ENDDO
C
C  DIVIDE OUT BY NUMBER TO GET AVERAGE
C
      INUM=ILX2-ILX1+1
      if(zintrp.lt.0.0) then
         ZNI=1.0/FLOAT(INUM)
         DO ISL=1,INX
            ZSLICE(ISL)=ZSLICE(ISL)*ZNI
         ENDDO
      endif
C
C  NOW DO INTERPOLATION/EXTRAPOLATION along slice indep coord.
C
      CALL EXPINT(ZXI,ZSLICE,INX,ZTARG,ZSLIC2,INTARG,LTARG,ZTRAP)
C
      RETURN
      END
C******************** END FILE EXPGET.FOR ; GROUP EXSUBS ******************
