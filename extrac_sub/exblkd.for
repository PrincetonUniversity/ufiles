C******************** START FILE EXBLKD.FOR ; GROUP EXBLOK ******************
C----------------------------------------------------------------
C  EXBLKD
C
C  DEFINE THE TRUNCATED 2D OUTPUT DATA
C
      SUBROUTINE EXBLKD(ZF,ZX,ZY,INX,INY)
C
      USE EXTRAC
C
      REAL ZF(INX,INY),ZX(INX),ZY(INY)
C
C  DO THE AXES FIRST
C
      IXL=0
      IYL=0
C
      DO IX=1,NXACT
        IF(LX(IX)) THEN
          IXL=IXL+1
          ZX(IXL)=X(IX)
        ENDIF
      ENDDO
C
      DO IY=1,NYACT
        IF(LY(IY)) THEN
          IYL=IYL+1
          ZY(IYL)=Y(IY)
        ENDIF
      ENDDO
C
C  NOW DO THE DATA
C
      IYL=0
      DO IY=1,NYACT
        IF(LY(IY)) THEN
          IYL=IYL+1
          IXL=0
          DO IX=1,NXACT
            IF(LX(IX)) THEN
      	IXL=IXL+1
      	IADR=(IY-1)*NXACT+IX
      	ZF(IXL,IYL)=F(IADR)
            ENDIF
          ENDDO
        ENDIF
      ENDDO
C
      RETURN
      END
C******************** END FILE EXBLKD.FOR ; GROUP EXBLOK ******************
