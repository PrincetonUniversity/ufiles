C******************** START FILE UFSETR.FOR ; GROUP UFILES *************
C---------------------------------------------------------------------
C  UFSETR
C  SET/RESET FILE-NAMING CONVENTIONS, DEFAULT DEVICE, DIRECTORY
C
      SUBROUTINE UFSETR(ILUN,ZPRE,ZEXT,ZDEVI,ZFDIR)
C
C  SAMPLE CALL (VAX):
C	CALL UFSETR(24,'T','PCR','HX3:','TRANSP.PDX')
C  ASSOCIATING LOGICAL UNIT NUMBER 24 AND FILENAME PARTS
C  TO ACCESS FILES WITH NAMES OF FORM HX3:[TRANSP.PDX]T#####.PCR
C  WHERE ##### IS A STANDARD SHOT NUMBER
C
C  mod dmc Sep 1994 -- (a) the old LTSS cray code is deleted
C                      (b) the trailing "/" in unix directory specs
C                          is no longer required.
C
C  MOD DMC MAY 1990 -- THE COLON ":" IS NO LONGER REQUIRED TO TERMINATE
C  THE DEVICE ARGUMENT ZDEVI
C
C------------------------------------------------------
C
      USE UFILES
C
      CHARACTER*(*) ZPRE
C
      CHARACTER*(*) ZEXT
      CHARACTER*(*) ZFDIR
      CHARACTER*(*) ZDEVI
C
      character*6 ztest
      CHARACTER*15 ZERRL
C
      integer      i1, i2, i3, m1, lzfdir
      character*10 zpulse
C
C  ERROR LABEL
      DATA ZERRL/'UFILES>UFSETR: '/
C
C--------------------------------------------
C
      CALL UFINIT
      ICHAN=IUXCHA(ILUN)
      ier=0
      IF(ICHAN.EQ.0) CALL UFXCHL(ILUN,ICHAN,IER)
      IF(IER.NE.0) THEN
C
C  ERROR-- ASSIGNMENT TABLE OVERFLOW
        WRITE(UFELUN,9001) ILUN,UFCHAN
 9001   FORMAT(' ? UFSETR-- TABLE OVERFLOW ON L.U.N. ',I5/
     >       '   UFILES L.U.N. ASSIGNMENT FAILED'/
     >       '   MAXIMUM NUMBER OF L.U.N. ASSIGNMENTS: ',I3/)
        CALL UFPAUS(1)
        RETURN
      ENDIF
C
      IF(.NOT.LUFSET(ICHAN)) GO TO 20
      IF(UFSTAT(ICHAN).NE.0) THEN
        WRITE(UFELUN,9002) ILUN
 9002   FORMAT(' ? UFSETR-- ATTEMPT TO RESET OPEN L.U.N.:  ',I5/
     >       '   CALL UFCLOS TO CLOSE UNIT FIRST BEFORE REASSIGNMENT')
        RETURN
      ENDIF
C
 20   CONTINUE
      LUFSET(ICHAN)=.FALSE.
C
C  dmc 15 Dec 1998 -- test for MDS+ specification
C
      mds_shot(ichan)=0
      imx=min(6,len(zpre))
      ztest=zpre(1:imx)
      call uupper(ztest)
      if(ztest.eq.'MDS+::') go to 500
 
C
C  dmc -- resume old code
C
C  GET LENGTHS OF FILENAME ELEMENTS.
C  CHECK INPUT FILENAME PARTS:  LEFT JUSTIFIED, NO IMBEDDED BLANKS,
      LPRFIX(ICHAN)=IUFTRM(ZPRE,ZERRL//'PREFIX',IER1)
      LEXTND(ICHAN)=IUFTRM(ZEXT,ZERRL//'.EXT',IER2)
C
C  DEVICE NAME SHOULD END IN ":" BUT WILL LIVE IF NOT...
      ILDEV=IUFTRM(ZDEVI,ZERRL//'DEVICE NAME',IER3)
      ICOLON=1
      IF(ILDEV.GT.0) THEN
        IF(ZDEVI(ILDEV:ILDEV).NE.':') THEN
          ILDEV=ILDEV+1
            ICOLON=0
          ENDIF
      ENDIF
      LDEV(ICHAN)=ILDEV
C
      ILDIR=IUFTRM(ZFDIR,ZERRL//'DIRECTORY NAME',IER4)
      ISLASH=1
      IF(ILDIR.GT.0) THEN
        IF(ZFDIR(ILDIR:ILDIR).NE.'/') THEN
          ILDIR=ILDIR+1
          ISLASH=0
        ENDIF
      ENDIF
      LFDIR(ICHAN)=ILDIR
      IF((IER1+IER2+IER3+IER4).GT.0) RETURN
C
C  CHECK LENGTHS:  IF OK, INSERT NAMES IN COMMON
C
      CALL UFNPAD(
     >ZPRE,LPRFIX(ICHAN),UPRFIX(ICHAN),ZERRL//'PREFIX',IER1)
C
      CALL UFNPAD(
     >ZEXT,LEXTND(ICHAN),UEXTND(ICHAN),ZERRL//'.EXT',IER2)
C
      IF(ICOLON.EQ.1) THEN
        CALL UFNPAD(
     >ZDEVI,LDEV(ICHAN),UDEV(ICHAN),ZERRL//'DEVICE NAME',IER3)
      ELSE
        IL=ILDEV-1
        CALL UFNPAD(
     >      ZDEVI(1:IL)//':',
     >      LDEV(ICHAN),UDEV(ICHAN),ZERRL//'DEVICE NAME',IER3)
      ENDIF
C
      IF(ISLASH.EQ.1) THEN
        CALL UFNPAD(
     >ZFDIR,LFDIR(ICHAN),UFDIR(ICHAN),ZERRL//'DIRECTORY NAME',IER4)
      ELSE
        IL=ILDIR-1
        CALL UFNPAD(
     >      ZFDIR(1:IL)//'/',
     >      LFDIR(ICHAN),UFDIR(ICHAN),ZERRL//'DIRECTORY NAME',IER4)
      ENDIF
C
      IF((IER1+IER2+IER3+IER4).GT.0) RETURN
C
      imds=0
      go to 800
C
 500  continue
C
C  MDS+ coding
C
      lprfix(ichan)=iuftrm(zpre,zerrl//'mds_server',ier1)-6
C If TRANSP run (from add_ufnodes) mds_path = .INPUTS
      if (zdevi(1:5) .eq. 'PPPL@') then
         ldev(ichan)  =iuftrm('.INPUTS',zerrl//'mds_path',ier3)
      else
         ldev(ichan)  =iuftrm(zdevi,zerrl//'mds_path',ier3)
      endif
      lfdir(ichan) =iuftrm(zfdir,zerrl//'mds_node',ier4)
      lzfdir=lfdir(ichan)  ! save for PPPL TRANSP
      if((IER1+IER3+IER4).GT.0) RETURN
C
C check if TRANSP run Input
C zdevi = PPPL@<tok.yr>@<runid>    : Read from TRANSP Tree (add_ufnodes)
C zext  = <tree>@<tok.yr>@<runid>  : TRANSP
C         <tree>@<runid>           : BEAST/JET
C         <tree>(shot)             : encoded shot (e.g. from add_ufnodes)
C
      i1 = index(zext,'@')+1
      m1 = index(zext,'(')+1
      if (i1 .gt. 1) then
         i2=index(zext(i1:),'@')
         i3=index(zext,' ')-1
         if (i3 .lt. 0) i3=len(zext)
C TRANSP:  pass ufdir = PPPL@<tok.yr>@<runid> to ufopen_mds
         if (i2 .gt. 0) then     !  TRANSP run
            call mkbeastid(zext(i1+i2:i3),mds_shot(ichan),zpulse)
C BEAST/JET: translate now
         else                   ! BEAST/JET
            call mkbeastid(zext(i1:i3),mds_shot(ichan),zpulse)
         endif
         zext(i1-1:)=' '
C Encoded shot: extract
      else if (m1 .gt. 1) then
         i2 = index(zext,')')-1
          read(zext(m1:i2),'(i10)',iostat=ios)mds_shot(ichan)
          zext(m1-1:)=' '
      else
C Read from TRANSP Tree (add_ufnodes): zdevi = PPPL@<tok.yr>@<runid>
         if (zdevi(1:5) .eq. 'PPPL@') then
            ufdir(ichan) = zdevi
            lfdir(ichan) = ldev(ichan)
         else
            ufdir(ichan)='mds+'
         endif
      endif
      lextnd(ichan)=iuftrm(zext,zerrl//'mds_tree',ier2)
      if(IER2.GT.0) RETURN
      uprfix(ichan)='mds+'
      uextnd(ichan)='mds+'
      udev(ichan)='mds+'
 
C
      if(lprfix(ichan).le.0) then
         lprfix(ichan)=5
         mds_server(ichan)='local'
      else
         ilpr=lprfix(ichan)+6
         call ufnpad(zpre(7:ilpr),lprfix(ichan),mds_server(ichan),
     1	zerrl//'mds_server',ier1)
      endif
      call uupper(mds_tree(ichan))
      call ufnpad(zext,lextnd(ichan),mds_tree(ichan),
     1    zerrl//'mds_tree',ier2)
C If TRANSP run (from add_ufnodes) mds_path = .INPUTS
      if (zdevi(1:5) .eq. 'PPPL@') then
         call ufnpad('.INPUTS',ldev(ichan),mds_path(ichan),
     1    zerrl//'mds_path',ier3)
      else
         call ufnpad(zdevi,ldev(ichan),mds_path(ichan),
     1    zerrl//'mds_path',ier3)
      endif
      call ufnpad(zfdir,lzfdir,mds_node(ichan),
     1    zerrl//'mds_node',ier4)
      if((IER1+IER2+IER3+IER4).GT.0) RETURN
C
      imds=1
      go to 800
C
 800  continue
C  EVERYTHING OK
      LUFSET(ICHAN)=.TRUE.
      UFLUN(ICHAN)=ILUN
      UFSTAT(ICHAN)=0
C  CLEAR COMPRESSED BINARY DATA WRITE FLAG
      if(imds.eq.0) then
         UFCPRS(ICHAN)=0
      else
         UFCPRS(ICHAN)=-1
      endif
C  DEFAULT: FILE NAME INCLUDES ENCODED SHOT NO.:
      LSHOTN(ICHAN)=.TRUE.
C
C  enforce name capitalization
C
      IF(LUFCAP.eq.1) THEN
         CALL UFUPPER(ILUN)
      ELSE IF(LUFCAP.eq.0) THEN
         CALL UFLOWER(ILUN)
      ENDIF
C
      RETURN
      END
C******************** END FILE UFSETR.FOR ; GROUP UFILES ***************
