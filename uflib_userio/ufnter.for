C******************** START FILE UFNTER.FOR ; GROUP UFNTER *************
C-----------------------------------------------------------
C  UFNTER
C
C  USER INTERFACE ROUTINE-- SET NAMING CONVENTIONS FOR FILENAME
C
C  ALSO SET FILE RESIDENT DEVICE AND PPN
c
C
      SUBROUTINE UFNTER(ILUN,ZNAM)
C
C  TbT Aug 23, 1993 -- Added R & T selections.
C  TBT Aug 1993 -- Change Zext from C*3 to  C*16
c
 
      USE UFILES
C
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
C
      CHARACTER*20 ZNAM
      CHARACTER*16 ZPRE
      CHARACTER*16 ZEXT
      Character*3  Zext3     ! tbt
      CHARACTER*16 ZDEV
      CHARACTER*64 ZFDIR,ZFDIRP,ZWORK
      CHARACTER*512 ZINP
      CHARACTER*4 ZTOK
      CHARACTER*16 ZSUB
      LOGICAL INIT,ISHOTN
C
C--------
C  mds+ stuff (dmc 16 Dec 1998)
C
      character*50 zserver
      character*20 ztree
      character*120 zpath
      character*20 znode
      integer imds
C
C--------
C
      DATA ISHOTN/.TRUE./
C
      DATA ZDEV/'                '/
      DATA ZEXT/'ZZZ'/
      DATA ZFDIR/'                                '/
      data zwork/' '/
C
      data zserver/'local'/
      data ztree/'default'/
      data zpath/' '/
      data znode/'(default)'/
C
      data imds/0/
C
C---------------------------------------------------------------
C  MOD DMC 15 JULY 1986
C  IF CHANNEL IS OLD, USE OLD NAME AS DEFAULT
C
      CALL UFINIT
C
      luntrm=lnout(0)
C
      ICHAN=IUXCHA(ILUN)
C
      IF(ICHAN.EQ.0) THEN
        CALL UFNPAD('Z',1,ZPRE,'UFNTER',IER)
        zext=' '          ! clear obsolete
      ELSE
        ZPRE=UPRFIX(ICHAN)
        ZEXT=UEXTND(ICHAN)
        ZDEV=UDEV(ICHAN)
        ZFDIR=UFDIR(ICHAN)
      ENDIF
C
      ILPRE=LEN(ZPRE)
      ILEXT=LEN(ZEXT)
        Ilext3=Len(Zext3)
      ILDEV=LEN(ZDEV)
      ILDIR=LEN(ZFDIR)
C
      iupper=LUFCAP
      iupsave=-1
C
C  INPUT ZNAM - 20 CHAR IDENTIFICATION OF FILE TYPE
C   E.G. "INPUT FILES         "
C  ILUN-- LOGICAL UNIT NUMBER TO ASSOCIATE FILE NAME SET WITH.
C
      INIT=.TRUE.
 10   CALL NEUPAG
 11   CONTINUE
C
C  MDS+ logic
C
      if(imds.eq.1) then
         call ufnter_mds(ilun,znam,
     1	imds,iquit,zserver,ztree,zpath,znode)
         if(iquit.eq.1) then
            go to 100
         else
            go to 10
         endif
      endif
C
      CALL UFSETR(ILUN,ZPRE,ZEXT,ZDEV,ZFDIR)
      if(iupper.eq.1) call ufupper(ilun)
      if(iupper.eq.0) call uflower(ilun)
      IF(.NOT.ISHOTN) CALL UFNSHO(ILUN)
      IF(LXOUT(0)) THEN
        WRITE(LUNTRM,1000)
 1000 FORMAT(/' <<UFNTER>> SET UFILES FILENAME FORM FOR:'/)
        CALL UFSNMD(LUNTRM,ZNAM,ILUN)
        WRITE(LUNTRM,1002)
      ENDIF
 1002 FORMAT(/' WHERE ###### IS THE VARIABLE SHOT NUMBER;'/
     >' IF DISK OR DIRECTORY NOT SPECIFIED, RMS DEFAULT IS USED'/
     >  ' OPTIONS:'/
     >  '  C - case (unix only) set filename to upper or lower case'/
     >  '  D - CHANGE DISK (DEVICE) NAME'/
     >  '  E - CHANGE DIRECTORY NAME'/
     >  '  F - CHANGE FILENAME 1 CHAR PREFIX AND .EXT SUFFIX'/
     >  '  G - GENERATE LONGER UFILES FILENAME PREFIX AND SUFFIX'/
     >  '  H - GENERATE LONGER UFILES FILENAME (PREFIX AND SUFFIX)',
     1         ' (Delimiters)'/
     >'  I - INCLUDE SHOT NUMBER ENCODED IN UFILES FILE NAME (default)'/
     >  '  J - ACCESS UFILE WITHOUT SHOT NUMBER ENCODED IN FILE NAME'/
     >  '  N - SPECIFY COMPLETE UFILE NAME - SAVE SHOT NUMBER'/
     >  '  M - *** MDS+ Signal Data Access ***'/
     >  '  P - CHANGE SINGLE CHARACTER FILENAME PREFIX'/
     >  '  R - CHANGE Multiple CHARACTER FILENAME PREFIX'/
     >  '  S - CHANGE FILENAME SUFFIX (3 Characters)'/
     >  '  T - CHANGE FILENAME SUFFIX (Delimiter required)'/
     >  '  Q - QUIT'/)
      CALL UREAD('UFNTER:  ENTER OPCODE (D/E/F/P/S/Q... ):$')
      if(lchar('M')) then
         imds=1-imds
         go to 10
      endif
      IOPT=IXUOPC('DEFPSQGIJNTRHC$')
      INIT=.TRUE.
      CALL UCHKOP(IOPT,0,1,100,-1,*11)
      INIT=.FALSE.
      GO TO (16,16,30,30,35,100,80,50,55,85,40,80,80,12) IOPT
C-----
C  select filename case (unix only)
C
 12   continue
        if(LXOUT(0)) write(LUNTRM,1012)
 1012 format(' Prepend "+" for change to default case; otherwise'/
     >         ' option applies to this Ufile only.'//
     >         ' enter "M" for mixed upper/lower case.  Note:  in'/
     >         ' main menu use "H" option to set prefix and suffix,'/
     >     ' and enclose the prefix and suffix strings in dbl quotes.'/)
      ideflt=0
      call uread(
     >'UFNTER:  enter U for uppercase, L for lowercase, or Q:$$')
      if(lchar('+')) ideflt=1
      if(lchar('U')) then
         iupper=1
         if(ideflt.eq.1) call ufncap(1)
         if(ideflt.eq.1) iupsave=-1
      else if(lchar('L')) then
         iupper=0
         if(ideflt.eq.1) call ufncap(0)
         if(ideflt.eq.1) iupsave=-1
      else if(lchar('M')) then
         if(ideflt.eq.0) iupsave=LUFCAP
         iupper=2
         call ufncap(2)
      else if(lchar('Q')) then
         go to 10
      else
         call uermsg('UFNTER:  enter U, L or Q',' ',0)
         go to 12
      endif
      go to 10
C-----
 16   CONTINUE
      IF(LXOUT(0)) WRITE(LUNTRM,1016)
 1016 FORMAT(
     >' options:'/
     >'  T -- select standard tokamak root path for data'/
     >'  S -- select standard tokamak subdirectory path for data'/
     >'  P -- give full explicit pathname'/
     >'  Q -- quit this menu without doing anything')
      CALL UREAD('UFNTER: choose pathname option:$')
      IF(LCHAR('Q')) THEN
        GO TO 10
      ELSE IF(LCHAR('P')) THEN
        GO TO 20
      ELSE IF(LCHAR('T')) THEN
        CALL UREADW('UFNTER: enter tokamak name:$',ZTOK)
        ZINP=' '
        CALL SGET_ENV('DATADIR',ZINP)
        INB=index(zinp,' ')-1
        ZINP(INB+1:)='/'//ZTOK
        GO TO 21
      ELSE IF(LCHAR('S')) THEN
        CALL UREADW('UFNTER: enter tokamak name:$',ZTOK)
        CALL UREADW('UFNTER: enter subdirectory name:$',ZSUB)
        ZINP=' '
        CALL SGET_ENV('DATADIR',ZINP)
        INB=index(zinp,' ')-1
        ILT=ILNURD(ZTOK)
        CALL ULOWER(ZSUB)
        ZINP(INB+1:)='/'//ZTOK(1:ILT)//'/'//ZSUB
        GO TO 21
      ELSE
        CALL UERMSG('%UFNTER:  invalid path option.',' ',0)
        GO TO 16
      ENDIF
C
 20   CONTINUE
      INIT=.TRUE.
      IF(LXOUT(0)) WRITE(LUNTRM,1091) ILDIR
C
 1091 format(
     >' enter a filename directory or path.  This path'/
     >' should lead up to but not include the filename itself.'/
     >' Relative pathnames are OK; lowercase and "/" OK.'/
     >' Maximum pathname length = ',i3,' including trailing "/".')
      call unixpath(zwork,zinp,iinp)
C
 21   CONTINUE
      IF(ZINP(1:2).EQ.'Q ') GO TO 10
      IF(ZINP(1:2).EQ.'q ') GO TO 10
      IF(ZINP(1:2).EQ.'0 ') GO TO 10
      IF((ZINP(1:2).EQ.'D ').OR.(ZINP(1:2).EQ.'d ')) THEN
        CALL UFNPAD(' ',1,ZFDIR,'UFNTER',IER)
        GO TO 10
      ENDIF
      ZFDIRP=ZFDIR
      if(zinp.ne.' ') then
         ilast=index(zinp,' ')-1
         if(zinp(ilast:ilast).ne.'/') then
            ilp1=ilast+1
            zinp(ilp1:ilp1)='/'
         endif
      endif
        IL=IUFTRM(ZINP,'UFNTER',IER)
        CALL UFNPAD(ZINP,IL,ZFDIR,'UFNTER DIRECTORY NAME',IER)
      IF(IER.NE.0) ZFDIR=ZFDIRP
      GO TO 10
C
 30   CONTINUE
      ZPRE=' '
      CALL UREADC(
     >   'UFNTER:  ENTER FILE PREFIX, A SINGLE LETTER:$$',ZPRE(1:1))
C
      IF(IOPT.EQ.4) GO TO 10
 35   CONTINUE
      CALL UREAD(
     >   'UFNTER: ENTER FILE .EXT EXTENSION (3 LETTER COMBO):$$')
      CALL URDBUF(ZEXT3,Ilen3,0)
      Zext = Zext3
      CALL UROTOR(3)
      GO TO 10
C
 40   CONTINUE     ! New version 8/93  tbt
      Call UReadW(
     >   'UFNTER: ENTER FILE .EXT EXTENSION (16 LETTER COMBO):$$',
     1   Zext)
 
      GO TO 10
C
 50   CONTINUE
      ISHOTN=.TRUE.
      GO TO 10
C
 55   CONTINUE
      ISHOTN=.FALSE.
      GO TO 10
C
C	                                                    Read PREfix
 80   CONTINUE
      IF(LXOUT(0) .and. Iopt .Eq. 7) WRITE(LUNTRM,1080) ILPRE,ILEXT3
      IF(LXOUT(0) .and. Iopt .Eq.13) WRITE(LUNTRM,1080) ILPRE,ILEXT
 1080 FORMAT(' MAX PREFIX LENGTH:  ',I2,' CHARACTERS'/
     >         ' MAX SUFFIX LENGTH:  ',I2,' CHARACTERS')
      CALL UREAD(
     >'UFNTER:  ENTER UFILES FILENAME PREFIX (MULTI-CHARACTER):$')
      CALL CDCOD(ZINP)
      ILAST=INDEX(ZINP,' ')-1
      IF(ILAST.LE.0) ILAST=LEN(ZINP)
      IF(ILAST.GT.ILPRE) THEN
        CALL UERMSG('% FILENAME PREFIX LENGTH ERROR:',ZINP,1)
        GO TO 10
      ENDIF
      ZPRE(1:ILPRE)=ZINP(1:ILPRE)
      If (Iopt .Eq. 12) Go to 10      ! Iopt=12 => R Only prefix.
C                                                    Go read suffix.
      If (Iopt .Eq.  7) GO TO 35      ! Iopt= 7 => G No delimiters
      If (Iopt .Eq. 13) Go to 40      ! Iopt=13 => H Delimiters
      If (Lxout(0)) Write (Luntrm,1093) Iopt ! Shouldn't get here   tbt
 1093   Format(//' Ufnter error at 1093 - bad Iopt =', I10/)
      Go to 10
C
 85   CONTINUE
      IF(LXOUT(0)) WRITE(LUNTRM,1080) ILPRE,ILEXT
      IF(LXOUT(0)) WRITE(LUNTRM,1082)
C
 1082 format(/' enter full filename spec, save encoded shot number'/)
      call unixfile('UFNTER',ZINP)
      IF(ZINP(1:2).EQ.'Q ') GO TO 10
      IF(ZINP(1:2).EQ.'q ') GO TO 10
C
C  DISK
C
      ilminp=len(zinp)
      irbrk=0
      do ic=ilminp,1,-1
        if(zinp(ic:ic).eq.'/') then
          if(irbrk.eq.0) irbrk=ic
        endif
      enddo
      if(irbrk.gt.ildir) then
        call uermsg('%UFNTER:  directory name too long',zinp,1)
        go to 85
      else if(irbrk.eq.0) then
        CALL UFNPAD(' ',1,ZFDIR,'UFNTER',IER)
        IST=1
      else
        CALL UFNPAD(' ',1,ZFDIR,'UFNTER',IER)
        ZFDIR(1:IRBRK)=ZINP(1:IRBRK)
        IST=IRBRK+1
      endif
C
C  PREFIX
      IBLANK=INDEX(ZINP(IST:),' ')+IST-1
      IF(IBLANK.LT.IST) IBLANK=LEN(ZINP)+1
C
      IDLMAX=IST-1
C
      ILDOT=INDEX(ZINP(IST:),'.')+IST-1
      IDLMAX=MAX0(IDLMAX,ILDOT)
      IF(ILDOT.LT.IST) ILDOT=LEN(ZINP)+1
C
      ILNUM=INDEX(ZINP(IST:),'#')+IST-1
      IDLMAX=MAX0(IDLMAX,ILNUM)
      IF(ILNUM.LT.IST) ILNUM=LEN(ZINP)+1
C
      IDLM=MIN0(IBLANK,ILDOT,ILNUM)
      IF(IDLM.LE.IST) THEN
        CALL UERMSG('%UFNTER: UFILE NAME PREFIX NOT FOUND',ZINP,1)
        GO TO 10
      ELSE IF((IDLM-IST).GT.ILPRE) THEN
        CALL UERMSG('%UFNTER: UFILE NAME PREFIX LENGTH ERROR',ZINP,1)
        GO TO 85
      ENDIF
      CALL UFNPAD(' ',1,ZPRE,'UFNTER',IER)
      ZPRE(1:IDLM-IST)=ZINP(IST:IDLM-1)
C
      IF(IDLM.EQ.IBLANK) GO TO 35
      IST=IDLMAX+1
C
C  SUFFIX
      IL=IBLANK-IST
      IF(IL.LE.0) GO TO 35
      IF(IL.GT.ILEXT) THEN
        CALL UERMSG('%UFNTER: UFILE NAME .EXT LENGTH ERROR',ZINP,1)
        GO TO 35
      ENDIF
      CALL UFNPAD(' ',1,ZEXT,'UFNTER',IER)
      ZEXT(1:IL)=ZINP(IST:IBLANK-1)
C
      GO TO 10
C
 100  CONTINUE
C
      if(iupsave.gt.0) LUFCAP=iupsave
C
      RETURN
      END
C-----------------------------------------------------------------------
      subroutine ufnter_mds(ilun,znam,
     1    imds,iquit,zserver,ztree,zpath,znode)
C
C  mds+ signal access options  -- give server, tree, path, ...
C
      integer ilun		! Ufiles lun / channel id
      character*(*) znam	! channel tag
C
      integer imds		! mds/Ufiles toggle
      integer iquit		! return "1" to quit
      character*(*) zserver	! MDS+ server
      character*(*) ztree	! MDS+ tree name
      character*(*) zpath	! MDS+ path
      character*(*) znode	! MDS+ node (appended to path)
C
      character*120 zfull	! full node path
C For ufiles from TRANSP runs
      character*7   ztok
      character*10  zrunid
C
C--------------------------------------
C  UREAD logical functions declarations...
C
      LOGICAL, EXTERNAL :: LCHAR,LXOUT,LYES,LYESC
C
C--------------------------------------
C
      iquit=0
C
      luntrm=lnout(0)
      ilz=max(1,len_trim(znam))
C
      zrunid=' '
      ztok=' '
 10   continue
      ils=max(1,len_trim(zserver))
      ilt=max(1,len_trim(ztree))
      ilp=max(1,len_trim(zpath))
      iln=max(1,len_trim(znode))
      ilr=max(1,len_trim(zrunid))
      ily=max(1,len_trim(ztok))
      if (ily .gt. 1) then      ! TRANSP run
         call ufsetr(ilun,'MDS+::'//zserver(1:ils),
     >        ztree(1:ilt)//'@'//ztok(1:ily)//'@'//zrunid(1:ilr),
     1        zpath(1:ilp),znode(1:iln))
      else if (ilr .gt. 1) then      ! BEAST run
         call ufsetr(ilun,'MDS+::'//zserver(1:ils),
     >        ztree(1:ilt)//'@'//zrunid(1:ilr),
     1        zpath(1:ilp),znode(1:iln))
      else
         call ufsetr(ilun,'MDS+::'//zserver(1:ils),ztree(1:ilt),
     1        zpath(1:ilp),znode(1:iln))
      endif
C
      call ufsnmd_nod0(zpath,znode,zfull,ilf)
C
      if(lxout(0)) then
         if (ilr .gt. 1) write (luntrm,1002) zrunid(1:ilr),ztok(1:ily)
 1002    format(/' TRANSP runid:       ',a,'  ',a)
         write(luntrm,1001) znam(1:ilz),zserver(1:ils),ztree(1:ilt),
     1	zpath(1:ilp),znode(1:iln),zfull(1:ilf)
 1001    format(/
     1' MDS+ signal input for:  ',a//
     2' ...MDS+ server:     ',a/
     3' ...MDS+ tree name:  ',a/
     4' ...MDS+ path:       ',a/
     5' ...MDS+ node:       ',a/
     6' ...full node path:  ',a//
     7' UFNTER_MDS Command options:'/
     8'  S -- set MDS+ server name'/
     9'  T -- set MDS+ tree name'/
     1'  P -- set MDS+ path'/
     2'  R -- revise "tail" of path'/
     3'  N -- set MDS+ node'/
     4'  U -- switch to Ufiles input mode'/
     5'  X -- extra specs for TRANSP runs'/
     6'  Q -- quit:  exit UFNTER using current MDS+ settings.'/)
      endif
C
      call uread('UFNTER_MDS:  enter option:$')
C
      if(lchar('Q')) then
         iquit=1
         return
C
      else if(lchar('U')) then
         imds=1-imds
         return
C
      else if(lchar('S')) then
         call ureadw(
     1	'UFNTER_MDS:  enter server name, "." for local:$',
     2	zserver)
         if(zserver.eq.'.') zserver='local'
C
      else if(lchar('T')) then
         call ureadw(
     1	'UFNTER_MDS:  enter tree name:$', ztree)
C
      else if(lchar('P')) then
         call ureadw(
     1	'UFNTER_MDS:  enter path, "." for blank path:$',
     2	zpath)
         if(zpath.eq.'.') zpath=' '
C
      else if(lchar('N')) then
         call ureadw(
     1	'UFNTER_MDS:  enter node name, "." for blank:$',
     2	znode)
         if(znode.eq.'.') znode=' '
C
      else if(lchar('R')) then
         if(ilp.le.1) then
            ibeg=1
         else
            do ic=ilp-1,1,-1
      	 if(zpath(ic:ic).eq.':') go to 20
      	 if(zpath(ic:ic).eq.'.') go to 20
            enddo
            ic=0
 20         continue
            ibeg=ic+1
         endif
         ilen=len(zpath)
         call ureadw(
     1	'UFNTER_MDS:  enter new tail of path:$',
     2	zpath(ibeg:ilen))
C
      else if(lchar('X')) then
         call ureadw(
     1	'UFNTER_MDS:  enter RUNID, "." for blank:$', zrunid)
         if (zrunid(1:1) .eq. '.') zrunid=' '
         call ureadw(
     1	'UFNTER_MDS:  enter TOK.YR, "." for blank:$', ztok)
         if (ztok(1:1) .eq. '.') ztok=' '
C
      else
         call uermsg('UFNTER_MDS:  invalid menu option',' ',0)
      endif
C
      go to 10
C
      end
C******************** END FILE UFNTER.FOR ; GROUP UFNTER ***************
