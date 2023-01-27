C******************** START FILE URDQCD.FOR ; GROUP UREAD5 *************
C--------------------------------------------------------
C
C& URDQCD    PARSE INPUT LINE WITH "?" CONDITIONAL BRANCH/ COMMAND
C  EXECUTION CHARACTER
C
C  REMOVED FROM SUBROUTINE UREAD4 JULY 1982
C
      SUBROUTINE URDQCD(INTRP)
C
      USE USERIO
      implicit NONE
C
      integer, intent(out) :: intrp
      CHARACTER*16 ZTEST1
      CHARACTER*96 ZTEST2,ZTEST3
      CHARACTER*1 ZLTEMP(80)
C
      character*(urdbufl) ZCBUFF
C
C  CHECK FOR SPECIAL CODES TO SPECIFY CHECK OF PARAMETERS
C  FOR CONDITIONAL EXECUTION OF A PART OF AN INDIRECT INPUT FILE
C
C  FORMAT:
C   '?<PARM.NAME>="<PARM.VALUE>" SKIPTO <MARKER>'
C    .
C    .
C   ':<MARKER>
C  RESULTS IN THE EXECUTION OF THE LINES (...) BETWEEN THE
C  ?-SKIPTO COMMAND ONLY IF THE PARAMETERS <PARM.NAME>
C  DOESNT HAVE THE VALUE <PARM.VALUE>.  IF THE PARAMETER
C  DOESNT HAVE THE SPECIFIED VALUE THE LINES FROM THE
C  SKIPTO COMMAND TO THE LINE CONTAINING ":<MARKER>" ARE
C  SKIPPED OVER.  IF IN PLACE OF "=" A "#" IS USED THEN THE
C  TEST FOR THE SKIPTO COMMAND IS .NE. INSTEAD OF .EQ.
C
C  NOTE THE DIFFERENCE BETWEEN THIS SYNTAX AND ORDINARY
C  FORTRAN "IF (LOGICAL EXPRESSION) GO TO <LINE #>" SYNTAX:
C  SKIPTO IS INTERPRETIVE IN NATURE AND ONE MAY ONLY SKIP
C  "DOWN" THE INPUT FILE; SKIPPING UP TO A PRIOR LINE IN THE
C  SEQUENTIAL INPUT, WHICH WOULD ALLOW LOOPING, IS NOT PERMITTED,
C  INDEED NOT POSSIBLE SINCE THE INPUT FILE IS READ SEQUENTIALLY
C  AND INTERPRETED LINE-BY-LINE.
c   rev d. mc cune 14 nov 1981.  now looping will be allowed:
c  if end of file without finding <MARKER> is reached the file
c  will be closed and reopened and the process will be repeated once
c  only.  thus looping of sorts is supported
C
C
C  REV-- D. MC CUNE 23 JULY 1981.  IF THE "SKIPTO" AFTER THE
C  TEST CONDITION IS NOT FOUND THEN THE TEXT FOLLOWING THE TEST
C  IS ASSUMED TO BE A COMMAND LINE TO BE RETURNED TO THE MAIN
C  PROGRAM, IF THE TEST CONDITION WAS SATISFIED
C
c  rev dmc 2009 (28 years later)!
c  add this test:  ?DEFINED(<parm.name>) ...
c     TRUE if the argument is a defined parameter name, otherwise FALSE
c  also: ?UNDEFINED(<parm.name>) ...
C
C---------
      integer :: im1,id1,iu1,iarg1,iarg2,ic,indop,i1,i2,iml,imn,im2
      integer :: iper,ilen,iv1,iv2,ivx,ieqs,itest,ians,ii
      integer :: i,ishft,inum,inump1,imst,isemi,iline,iparen
      logical :: iresult
C---------
C  operate on uppercase buffer
      zcbuff=cbuff
      call uspcap(zcbuff)
C
C  CLEAR MARKER AND SUBSTITUTED INTERPRETABLE COMMAND FLAGS
      INTRP=0
      LMARKN=0
C  CHECK FOR SKIPTO BRANCH FORMAT
C  VALID ONLY FOR INPUT FROM OR OUTPUT TO INDIRECT INPUT FILES
      IM1=INDEX(ZCBUFF,'SKIPTO')

C  DMC 5/2009:
C  CHECK for DEFINED/UNDEFINED operators
      CALL chk_op('DEFINED',id1,iarg1,iarg2)
      if(id1.eq.-1) return

      if(id1.eq.0) then
         CALL chk_op('UNDEFINED',iu1,iarg1,iarg2)
         if(iu1.eq.-1) return
      else
         iu1=0
      endif

 9305 FORMAT(' ? ERROR IN "?"-BRANCH LINE, LINE IGNORED')

      if(max(iu1,id1).gt.0) then
         CALL USPLNM(DBUFF,LBUFF,iarg1,iarg2,ZTEST1,IPER)
         IF(IPER.NE.0) THEN
            WRITE(LUNTRM,9305)
            CALL UTRACE
            CALL UCBUFX
            CALL UCLEAR
            RETURN
         ENDIF
      
C     ZTEST1 contains legal parameter name

         CALL USPGET(ztest1,ztest2,itest)  ! itest=0 means: DEFINED.

         if(id1.gt.0) iresult = (itest.eq.0)
         if(iu1.gt.0) iresult = (itest.ne.0)

         if(.NOT.iresult) return  ! condition not satisfied => exit, do nothing

         if(im1.gt.0) go to 350   ! execute SKIPTO

         iparen=0
         do i=iarg2,lbuff
            if(iparen.eq.0) then
               if(dbuff(i).eq.')') iparen=i
            else
               if((dbuff(i).ne.' ').AND.(dbuff(i).ne.char(9))) exit
            endif
         enddo

         go to 344                ! to interpret remainder of line...
      endif

C  RESUME traditional legacy UREAD SKIPTO and ? branch parsing

      IF(.not.((IM1.EQ.0).OR.(NLINDI.OR.NLINDO))) then
         WRITE(LUNTRM,9301)
 9301    FORMAT(' ? "?"/SKIPTO FORMAT NOT VALID IN THIS CONTEXT'/
     >        '   NO INDIRECT INPUT FILE IS OPEN.')
         CALL UCLEAR
         RETURN
      ENDIF
C
      IF(.not.NLINDI) THEN
         IF(IM1.GT.0) WRITE(LUNTRM,9302)
 9302    FORMAT(' % CONDITIONAL SKIPTO INSERTED IN OUTPUT FILE'/
     >        '   SKIPTO DESTINATION MARKER SHOULD BE INSERTED BEFORE'/
     >        '   E-O-F AND IMMEDIATELY FOLLOWING A PROMPT MATCHING'/
     >        '   THE CURRENT PROMPT.  AWAITING INPUT:')
         IF(IM1.GT.0) RETURN
      ENDIF
C
C  PARSE SKIPTO COMMAND; INPUT IS INDIRECT (NLINDI=.TRUE.) (from file)
C
C  CHECK FOR UNCONDITIONAL SKIPTO
C  FORMAT: "?SKIPTO <MARKER>"; CONDITIONAL TEST OMITTED
      IF((IM1.GT.0).AND.(IM1.LE.4)) GO TO 350
C  LOCATION OF OPERATOR SYMBOL "=" OR "#"
      DO IC=2,LBUFF
        IF((CBUFF(IC:IC).EQ.'=').OR.(CBUFF(IC:IC).EQ.'#')) GO TO 326
      ENDDO
      IC=0
 326  INDOP=IC
C
      IF(INDOP.EQ.0) THEN
         WRITE(LUNTRM,9303)
 9303    FORMAT(/
     >      ' ? NO LOGICAL OPERATOR ("=" OR "#") IN "?"-BRANCH LINE.  ',
     >        'LINE IGNORED'/
     >        1X,A)
         CALL UTRACE
         CALL UCBUFX
         CALL UCLEAR
         RETURN
C
      ENDIF
C  GET PARAMTER NAME; CHECK LEGALITY OF NAME
      I1=2
      I2=INDOP-1
      CALL USPLNM(DBUFF,LBUFF,I1,I2,ZTEST1,IPER)
      IF(IPER.NE.0) THEN
         WRITE(LUNTRM,9305)
         CALL UTRACE
         CALL UCBUFX
         CALL UCLEAR
         RETURN
      ENDIF
C  GET PARAMETER VALUE; CHECK FOR EXISTANCE THEREOF
      CALL USPGET(ZTEST1,ZTEST2,IPER)
      IF(IPER.NE.0) THEN
         ILEN=LPVAL+2
         CALL UPQUER(ZTEST1,ZLTEMP,ILEN,ZTEST2)
      ENDIF
C
C  LOCATE COMPARISON-VALUE FIELD, DELIMITTED BY DOUBLE QUOTES
C
      IV1=INDOP+1
      IV2=INDOP
      do ii=iv1,lbuff
         ! find opening quote, must be 1st non-blank character
         if(dbuff(ii).eq.'"') then
            iv1=ii
            iv2=iv1
            exit
         else if(dbuff(ii).ne.' ') then
            exit
         endif
      enddo

      if(iv2.eq.iv1) then
         ! find closing quote -- 1st one taken...
         i1=iv1+1
         iv1=0
         do ii=i1,lbuff
            if(dbuff(ii).eq.'"') then
               iv1=i1
               iv2=ii-1
               exit
            endif
         enddo
         if(iv1.eq.0) then
            IV1=INDOP+1
            IV2=INDOP
         endif
      endif
C
      CALL USPLVL(DBUFF,LBUFF,IV1,IV2,ZTEST3,IPER)
C
      IF(IPER.NE.0) THEN
         WRITE(LUNTRM,9307)
 9307    FORMAT(
     >        ' ? ERROR IN "?"-BRANCH LINE COMPARISON VALUE FIELD')
         CALL UTRACE
         CALL UCBUFX
         CALL UCLEAR
         RETURN
      ENDIF
C
C  PERFORM TEST TO DECIDE WHETHER TO PERFORM SKIP
C
      IEQS=1
      IF(DBUFF(INDOP).EQ.'#') IEQS=-1
C
      ITEST=1
      call uspcap(ztest2)
      call uspcap(ztest3)
      IF(ZTEST2.NE.ZTEST3) ITEST=-1
C
      IANS=IEQS*ITEST
      IF(IANS.NE.1) RETURN
C
C  CONDITIONAL OPERATION IS TO BE PERFORMED.  FIND MARKER FIELD
C
C  SEE ABOVE ***IM1=INDEX(ZCBUFF,'SKIPTO ')
C  IF "SKIPTO" NOT FOUND (IM1=0) DO SUBSTITUTION INSTEAD OF
C  SKIP
C
      IF(IM1.GT.0) GO TO 350
C  CONDITIONAL TEST PASSED; SUBSTITUTE COMMAND INTO LINE AND
C  INTERPRET
      IVX=IV2+2
      DO I=IVX,LBUFF
         IF(CBUFF(I:I).NE.' ') GO TO 344
      ENDDO
C  BLANK LINE
      RETURN

 344  CONTINUE
      ISHFT=I

      INUM=LBUFF-ISHFT+1
      DO I=1,INUM
         I2=I+ISHFT-1
         CBUFF(I:I)=CBUFF(I2:I2)
      ENDDO

      INUMP1=INUM+1
      DO I=INUMP1,LBUFF
         CBUFF(I:I)=' '
      ENDDO
C  PUT IN UNPACKED BUFFER ALSO
      CALL UDECOD
C  STUFF AFTER "?" TEST HAS BEEN SHIFTED TO CONSTITUTE ENTIRE
C  INPUT LINE. PROCEED TO INTERPRET LINE
      INTRP=1
      RETURN
C--
 350  CONTINUE
      IMST=IM1+6
C
C  REV DMC OCT 1990 -- MAKE SURE COMMENT IS EXCLUDED FROM MARKER FIELD
C
      ISEMI=IMST
      CALL UFCHAR(DBUFF,MSEMI,ISEMI,LBUFF)
      IF(ISEMI.GT.0) THEN
         ILINE=ISEMI-1
      ELSE
         ILINE=LBUFF
      ENDIF
C
      CALL FLDXTR(DBUFF,IMST,ILINE,' ',' ',IM1,IM2)
      IMN=IM2-IM1+1
      IF(IMN.LE.0) THEN
         WRITE(LUNTRM,9309) CBUFF
 9309    FORMAT(' ? DESTINATION MARKER MISSING FROM SKIPTO LINE:'/
     >        1X,A/' LINE IGNORED.'/)
         CALL UCLEAR
         RETURN
      ENDIF
C
C  SAVE MARKER
      LMARKN=IMN
      DO I=1,LMARKN
         IML=IM1+I-1
         URMARK(I:I)=DBUFF(IML)
      ENDDO
      call uspcap(urmark)
C
      RETURN

      contains
        subroutine chk_op(opword,ic,ia1,ia2)

          ! check for DEFINED or UNDEFINED operators

          character*(*), intent(in) :: opword  ! operator keyword, uppercase
          integer, intent(out) :: ic   ! location of opword; 0=none; -1=error
          integer, intent(out) :: ia1  ! position of start of operand string
          integer, intent(out) :: ia2  ! position of end of operand string

          !---------------------------
          integer :: iquest,inb,ia,ilenop
          !---------------------------

          ilenop = len(trim(opword))
          ia1=0
          ia2=0

          ic=index(zcbuff,opword)
          if(ic.le.0) then
             ic=0
             return
          endif

          ilen = len(trim(zcbuff))

          iquest=0
          do ia=1,ilen
             if(iquest.eq.0) then
                if(zcbuff(ia:ia).eq.'?') then
                   iquest=ia
                   cycle
                endif
             endif
             if(zcbuff(ia:ia).eq.' ') cycle
             if(zcbuff(ia:ia).eq.char(9)) cycle

             if(zcbuff(ia:ia+ilenop-1).ne.opword) then
                ic=0
             endif
             exit
          enddo

          if(ic.eq.0) return

          ia=ia+ilenop
          if((zcbuff(ia:ia).ne.' ').AND.(zcbuff(ia:ia).ne.char(9)).AND.
     >         (zcbuff(ia:ia).ne.'(')) then
             ! opword is only a substring
             ic=0
             return
          endif

          ! look for 1st non-blank after keyword

          do
             if((zcbuff(ia:ia).ne.' ').AND.
     >            (zcbuff(ia:ia).ne.char(9))) then
                if(zcbuff(ia:ia).ne.'(') ic=-1
                exit
             endif
             if(ia.eq.ilen) then
                ic=-1   ! argument not found
                exit
             endif
             ia=ia+1
          enddo
          if(ic.lt.0) go to 999

          ! dbuff is unpacked vsn of zcbuff
          call fldxtr(dbuff,ia,ilen,'(',')',ia1,ia2)
          if(ia2.lt.ia1) then
             ic=-1
             go to 999
          endif

          return

                                ! error
 999      continue

          write(luntrm,*) ' ?urdqcd: malformed '//
     >         trim(opword)//' operator:'
          write(luntrm,*) trim(zcbuff)
          call uclear
          return

        end subroutine chk_op

      END
C******************** END FILE URDQCD.FOR ; GROUP UREAD5 ***************
