C******************** START FILE UFSNAM.FOR ; GROUP UFILES ******************
C-----------------------------------------------------------
C  UFSNAM
C  NAME FILE FROM SHOT NUMBER, RETURN NONZERO ERROR
C  CODE IF THERE IS AN ERROR IN THE SHOT NUMBER
C
C  nb dmc 15 Dec 1998 -- Ufiles MDS+ data access -- this routine
C   cannot form a complete "filename", since actual object is
C   a server/tree/path combination.  Use UFSNAM_MDS to test if
C   ILUN is on an MDS+ channel.
C
      SUBROUTINE UFSNAM(ISHOT,ZFILE,IER,ILUN)
C
C  ISHOT-- 5 DIGIT SHOT NUMBER
C  ZFILE - OUTPUT FULL FILENAME W/ DEVICE, DIRECTORY (IF IT FITS)
C  IER-- ERROR RETURN CODE 0= SUCCESS
C  ILUN-- L.U.N. NUMBER TO ASSOCIATE WITH NAMED FILE
C
C
      USE UFILES
      CHARACTER*(*) ZFILE
      CHARACTER*10 ASHOT
 
      integer iln, ilt
C
      CALL UFINIT
      IER=0
C  FIND CHANNEL NUMBER; CHECK UNIT NUMBER
      CALL UFLCHK(ILUN,'UFSNAM    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
C
      idx=6
C
C  CHECK SHOT NO. IF USED IN FILENAME
      IF(LSHOTN(ICHAN)) THEN
        if(ufcprs(ichan).eq.-1) then
           id=1
           idx=10
           write(ashot,'(i10.10)') ishot
        else
           IF (ISHOT.LT.0 .OR. ISHOT.GT.999999) THEN
              WRITE(UFELUN,9001) ISHOT
 9001         FORMAT(' ? UFSNAM:  ILLEGAL SHOT NUMBER: ',I10)
              id=1
              ashot='******'
           else
C
              ISH = ISHOT
C  BREAK UP SHOT NUMBER INTO 5 CONSTITUENT DIGITS
C  INCL. LEADING ZEROES
              DO 10 II=6,1,-1
                 IDIG = MOD(ISH,10)
                 ASHOT(II:II) = CHAR(IDIG+48) ! INT ASCII CODE CONV.
                 ISH = ISH/10
 10           CONTINUE
C  DETERMINE WHETHER 5 OR 6 DIGIT SHOT NUMBER
              ID = 2
              IF(ASHOT(1:1).NE.'0') ID = 1
           endif
        endif
      ELSE
         ASHOT='      '
         ID=6
      ENDIF
C
      if(ufcprs(ichan).eq.-1) then
         ilt=len_trim(mds_tree(ichan))
         iln=len_trim(mds_node(ichan))
         zfile=' '
         write(zfile, '( ''MDS+:'',a,'':'',a,'':'',a)')
     1	mds_tree(ichan)(1:ilt),mds_node(ichan)(1:iln),ashot(id:idx)
      else
 
         CALL UFSNM0(ZFILE,
     >        UDEV(ICHAN),UFDIR(ICHAN),UPRFIX(ICHAN),ASHOT(ID:idx),
     >        UEXTND(ICHAN))
      endif
C  SAVE SHOT NUMBER
      UFSHOT(ICHAN)=ISHOT
      RETURN
      	END
C******************** END FILE UFSNAM.FOR ; GROUP UFILES ******************
C******************** START FILE UFSNAM.FOR ; GROUP UFILES ******************
C-----------------------------------------------------------
C  UFSNAM
C  NAME FILE FROM SHOT NUMBER, RETURN NONZERO ERROR
C  CODE IF THERE IS AN ERROR IN THE SHOT NUMBER
C
      SUBROUTINE UFSNAM_MDS(ILUN,imds)
C
C input:
C  ILUN-- L.U.N. NUMBER TO ASSOCIATE WITH NAMED FILE
C
C output:
C  imds=0 -- normal Ufile
C  imds=1 -- MDS+ data
C
      USE UFILES
C
      CALL UFINIT
      IER=0
C  FIND CHANNEL NUMBER; CHECK UNIT NUMBER
      CALL UFLCHK(ILUN,'UFSNAM    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
C
      if(ufcprs(ichan).eq.-1) then
         imds=1
      else
         imds=0
      endif
C
      return
      end
 
