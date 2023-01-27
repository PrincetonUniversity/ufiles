C******************** START FILE DPLNST.FOR ; GROUP DPLOTT *************
C-----------------------------------------------------------------
C  DPLNST
C  SET LINE TYPE FOR DPLOT1  SOLID (1) OR VARIOUS DASHED STYLES
C  SEE THE TEKTRONIX MANUALS (AG, TERMINAL CONTROL SYS)
C
C  MAY SUBSTITUTE SYMBOL PLOTTING FOR LINE PLOTTING (NEW 8/87 DMC)
C
C  INPUT M BTW 1 AND 10!  (SGLIB/GFLIB HISTORY)
C
      SUBROUTINE DPLNST(M,IVAL,ISYM,SIZE)
C
      use gftic
C
      if(.not.init_gftic) call gftic0
C
      IM=MIN0(10,MAX0(1,M))
      IF(KLINTP.GE.0) THEN
C
        IM=IM+KLINTP
        IF((M.GE.1).AND.(M.LE.5)) THEN
          IF(IM.GT.5) IM=IM-5
        ELSE
          IF(IM.GT.10) IM=IM-5
        ENDIF
C
        IM=MAX0(1,MIN0(10,IM))
        IVAL=LNSTYL(IM)
C
        CALL LINE(IVAL)
        ISYM=0
        SIZE=0.0
C
      ELSE
        IVAL=-4
        IF(IM.LE.5) THEN
          IM=12+IM
        ELSE
          IM=7+IM
        ENDIF
C
        ISYMTP=KSYMTP
        ZSIZE=SGSYSZ
        KSYMTP=KSYLTP
        SGSYSZ=SGSLSZ
C
        CALL DPSYST(IM,ISYM,SIZE)
C
        SGSYSZ=ZSIZE
        KSYMTP=ISYMTP
C
      ENDIF
C
      RETURN
      END
C******************** END FILE DPLNST.FOR ; GROUP DPLOTT ***************
C
      SUBROUTINE DPLNST0(M,IVAL,ISYM,SIZE)
C same as DPLNST but with small size for individual points
C
      use gftic
C
      if(.not.init_gftic) call gftic0
C
      IM=MIN0(10,MAX0(1,M))
      IF(KLINTP.GE.0) THEN
C
        IM=IM+KLINTP
        IF((M.GE.1).AND.(M.LE.5)) THEN
          IF(IM.GT.5) IM=IM-5
        ELSE
          IF(IM.GT.10) IM=IM-5
        ENDIF
C
        IM=MAX0(1,MIN0(10,IM))
        IVAL=LNSTYL(IM)
C
        CALL LINE(IVAL)
        ISYM=0
        SIZE=0.0
C
      ELSE
        IVAL=-4
        IF(IM.LE.5) THEN
          IM=12+IM
        ELSE
          IM=7+IM
        ENDIF
C
        ISYMTP=KSYMTP
        ZSIZE=SGSYSZ
        KSYMTP=KSYLTP
        SGSYSZ=SGSLSZ
C
        CALL DPSYST0(IM,ISYM,SIZE)
C
        SGSYSZ=ZSIZE
        KSYMTP=ISYMTP
C
      ENDIF
C
      RETURN
      END
C******************** END FILE DPLNST.FOR ; GROUP DPLOTT ***************
