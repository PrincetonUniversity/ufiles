C----------------------------------------------------------------------
C  GFTICI - CONTROL TICMARK/FRAME CHARACTERISTICS OF 2D PLOTS
C
      SUBROUTINE GFTICI
C
C  DMC NOV 1990 -- ADDITIONAL CONTROL OF SYMBOLS AND LINE STYLES ADDED
C
      use gftic
C
C---------------------------
C
      LOGICAL LCHAR
      LOGICAL LXOUT
      LOGICAL LYES
C
      logical, save :: loc_init
      integer, save :: klintp_save
C
C => GFTICI               BEGIN DATA BLOCK
C => LISMAK 12-APR-88  MOVED DATA STATEMENTS
      DATA IMAX/4/  ! LINE STYLE CONTROL CODE LIMIT
      DATA IMAXS/4/ ! SYMBOL SELECT CONTROL CODE LIMIT
      DATA ZS1/0.001/ ! MIN SYMBOL SIZE CONTROL
      DATA ZS2/3.0/ ! MAX SYMBOL SIZE CONTROL
C => END DATA BLOCK
C ====================
C
      if(.not.init_gftic) call gftic0
C
      if(.not.loc_init) then
         loc_init=.TRUE.
         klintp_save = max(0,klintp)
      endif
C
      LUNTRM=LNOUT(0)
C
 10   CONTINUE
      CALL NEUPAG
C
      IF(LXOUT(0)) THEN
        CALL GFTICL('A','X/YNEAT ','BIGGER --> NEATER',
     >	    KNEATX,KNEATY,KNEAT0,KNEATMIN,KNEATMAX)
        CALL GFTICL('B','X/YFRM  ','.GE.5 DRAWS GRID LINES',
     >	    KFRAMX,KFRAMY,KFRAM0,KFRAMMIN,KFRAMMAX)
        CALL GFTICL('C','X/YMFRM ',' AT MINOR TIC MARKS',
     >	    KFRMMX,KFRMMY,KFRMM0,KFRMMMIN,KFRMMMAX)
        CALL GFTICL('D','X/YFRM 2',' FARSIDE AXIS MAJOR TICS 0=OFF',
     >	    KFRACX,KFRACY,KFRAC0,KFRACMIN,KFRACMAX)
        CALL GFTICL('E','X/YMFRM2',' FARSIDE AXIS MINOR TICS',
     >	    KFRMCX,KFRMCY,KFRMC0,KFRMCMIN,KFRMCMAX)
C
        WRITE(LUNTRM,1002)
 1002   FORMAT(' ----------------------'/
     >' ADDITIONAL MISCELLANEOUS CONTROL OPTIONS:'/
     >' 0 - **RESTORE ALL DEFAULTS**'/
     >' 1 - SET PLOT GRID SIZE FOR 2D PLOT (NOT INCLUDING MULTIGRAPHS)'/
     >' 2 - SET PLOT LINE STYLE OR SUBSTITUTE SYMBOL FOR LINE PLOTTING'/
     >' 3 - SET PLOT SYMBOL TYPE AND SIZE'/
     >' 4 - MAKE ALL SYMBOLS INVISIBLE AND MAKE ALL LINES SOLID')
      ENDIF
C
      CALL UREAD('GFTICI:  ENTER OPCODE OR "Q":$$')
C-----------------------------------
C  NUMERIC OPTIONS HANDLED... (DMC AUG 1987)
      IF(LCHAR('0')) THEN
        CALL GFTIC0
        GO TO 10
      ELSE IF(LCHAR('1')) THEN
        CALL NEUPAG
        WRITE(LUNTRM,1003) NXR1,NXR2,NYR1,NYR2
        IF(LXOUT(0)) WRITE(LUNTRM,1004)
 1003   FORMAT(/
     >' CURRENT 2D PLOT X RASTER LIMITS:  ',2I10/
     >' CURRENT 2D PLOT Y RASTER LIMITS:  ',2I10)
 1004   FORMAT(/
     >'  *WARNING* THESE CONTROLS EFFECT ONLY THE LOCATION OF THE'/
     >'  PLOT GRID/AXES AND TIC LABELS, NOT AUXILLIARY LABELS'/
     >'  THESE CONTROLS HAVE NO EFFECT ON "MULTIPLOTS"'/
     >'  NO CHECK IS MADE THAT USER INPUT IS "REASONABLE" FOR A'/
     >'  GIVEN PLOTTING DEVICE.  CAVEAT EMPTOR!'/)
        IXR1=NXR1
        CALL UREADI('GFTICI:  X RASTER LOWER BOUND:$$',IXR1)
        IXR2=NXR2
        CALL UREADI('GFTICI:  X RASTER UPPER BOUND:$$',IXR2)
        IYR1=NYR1
        CALL UREADI('GFTICI:  Y RASTER LOWER BOUND:$$',IYR1)
        IYR2=NYR2
        CALL UREADI('GFTICI:  Y RASTER UPPER BOUND:$$',IYR2)
        WRITE(LUNTRM,1003) IXR1,IXR2,IYR1,IYR2
        CALL UREAD('GFTICI:  ENTER "Y" TO VERIFY RASTER LIMITS:$$')
        IF(LYES(0)) THEN
          NXR1=IXR1
          NXR2=IXR2
          NYR1=IYR1
          NYR2=IYR2
        ENDIF
        GO TO 10
      ELSE IF(LCHAR('2')) THEN
        CALL NEUPAG
        IF(LXOUT(0)) WRITE(LUNTRM,1010) KLINTP,IMAX
 1010   FORMAT(/
     >' CURRENT LINE TYPE CONTROL CODE: ',I2/
     >' SELECT CONTROL CODE BTW. 0 AND ',I2,' TO "PERMUTE" LINE STYLES'/
     >' SELECT CONTROL CODE = -1 TO SUBSTITUTE SYMBOL PLOTTING FOR'/
     >'     LINE PLOTTING IN ALL 2D GRAPHS'/
     >' SELECT CODE = -2 TO ACTUALLY MODIFY SG LINE STYLE CODES USED')
        CALL UREADI('GFTICI:  ENTER LINE TYPE CONTROL CODE:$$',ILINTP)
        ILINTP=MIN0(IMAX,MAX0(-2,ILINTP))
        IF(ILINTP.EQ.-2) THEN
C  MODIFY LINE STYLE CODES
          CALL GFTCLS
        ELSE IF(ILINTP.EQ.-1) THEN
C  SUBSTITUTE SYMBOL PLOTTING FOR LINE PLOTTING
          KLINTP=ILINTP
          WRITE(LUNTRM,1011)
 1011   FORMAT(/' %FOR SYMBOLS USED TO REPLACE LINE PLOTTING:')
          IF(LXOUT(0)) WRITE(LUNTRM,1012) KSYLTP,IMAXS,SGSLSZ,ZS1,ZS2
 1012   FORMAT(/' CURRENT SYMBOL SELECT CODE:  ',I2/
     >' ...LEGAL VALUES BTW 0 AND ',I2,' FOR "PERMUTATION" OF SYMBOLS'/
     >' CURRENT SYMBOL SIZE SETTING:  ',1PE11.4/
     >' ...LEGAL RANGE ',1PE11.4,' TO ',1PE11.4/
     >' **> ENTER -1 TO ACTUALLY MODIFY (RATHER THAN JUST PERMUTE) THE'/
     >'     SG SYMBOL SELECTION CODES')
          CALL UREADI('GFTICI:  ENTER SYMBOL SELECT CODE:$$',ISYLTP)
          IF(ISYLTP.EQ.-1) THEN
C  SELECTION OF NEW SYMBOLS FOR PLOTTING
            CALL GFTCSS
          ELSE
            KSYLTP=MIN0(IMAXS,MAX0(0,ISYLTP))
            CALL UREADF('GFTICI:  ENTER SYMBOL SIZE CONTROL:$$',
     >		SGSLSZ)
            SGSLSZ=AMIN1(ZS2,AMAX1(ZS1,SGSLSZ))
          ENDIF
        ELSE
C  "PERMUTATION" OF EXISTING STYLES
          KLINTP=ILINTP
        ENDIF
        GO TO 10
C
      ELSE IF(LCHAR('3')) THEN
        IF(LXOUT(0)) WRITE(LUNTRM,1012) KSYMTP,IMAXS,SGSYSZ,ZS1,ZS2
        CALL UREADI('GFTICI:  ENTER SYMBOL SELECT CODE:$$',ISYMTP)
        IF(ISYMTP.EQ.-1) THEN
C  SELECTION OF NEW SYMBOLS FOR PLOTTING
          CALL GFTCSS
        ELSE
          KSYMTP=MIN0(IMAXS,MAX0(0,ISYMTP))
          CALL UREADF('GFTICI:  ENTER SYMBOL SIZE CONTROL:$$',SGSYSZ)
          SGSYSZ=AMIN1(ZS2,AMAX1(ZS1,SGSYSZ))
        ENDIF
        GO TO 10
C
      ELSE IF(LCHAR('4')) THEN
C  BLOW AWAY SYMBOLS AND MAKE ALL LINES SOLID -- FOR THE MAC
        SGSYSZ=ZS1
        DO 19 I=1,IGFTMAXL
          LNSTYL(I)=0
 19     CONTINUE
        GO TO 10
      ENDIF
C-----------------------------------
      IANS=IXUOPC('QABCDES$')
      IF(IANS.LE.0) THEN
        CALL UERMSG('?GFTICI:  UNRECOGNIZED OPCODE',' ',-1)
        GO TO 10
      ENDIF
C
      IF(IANS.EQ.1) THEN
        RETURN
      ELSE IF(IANS.EQ.2) THEN
        CALL GFTICJ('X/YNEAT ',KNEATX,KNEATY,KNEAT0,KNEATMIN,KNEATMAX)
      ELSE IF(IANS.EQ.3) THEN
        CALL GFTICJ('X/YFRM  ',KFRAMX,KFRAMY,KFRAM0,KFRAMMIN,KFRAMMAX)
      ELSE IF(IANS.EQ.4) THEN
        CALL GFTICJ('X/YMFRM ',KFRMMX,KFRMMY,KFRMM0,KFRMMMIN,KFRMMMAX)
      ELSE IF(IANS.EQ.5) THEN
        CALL GFTICJ('X/YFRM 2',KFRACX,KFRACY,KFRAC0,KFRACMIN,KFRACMAX)
      ELSE IF(IANS.EQ.6) THEN
        CALL GFTICJ('X/YMFRM2',KFRMCX,KFRMCY,KFRMC0,KFRMCMIN,KFRMCMAX)

      else if(IANS.eq.7) then
c
c  new DMC July 2008 -- simplified options
c
         do
            if(lxout(0)) then
               write(lnout(0),*) ' Simplified menu: '
               write(lnout(0),*) ' ================ '
               write(lnout(0),*) ' "S" for symbol plotting'
               write(lnout(0),*) ' "L" for line plotting'
               write(lnout(0),*) ' "G" for grid lines'
               write(lnout(0),*) ' "N" for no grid lines'
               write(lnout(0),*) ' "Q" to exit with no changes.'
               write(lnout(0),*) ' '
               write(lnout(0),*) 
     >       '     (selections apply to subsequent plots until reset).'
               write(lnout(0),*) ' '
            endif

            call uread('GFTICI simplified menu option S/L/G/N/Q:$')

            if(lchar('Q')) then
               exit

            else if(lchar('L')) then
               klintp = max(0,klintp_save)
               exit

            else if(lchar('G')) then
               kframx=5
               kframy=5
               exit

            else if(lchar('N')) then
               kframx=kfram0
               kframy=kfram0
               exit

            else
               call uermsg('GFTICI: unrecognized option.',' ',0)
            endif
         enddo
         return
      ENDIF
C
      GO TO 10
C
      END
