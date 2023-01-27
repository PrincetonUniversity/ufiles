C******************** START FILE UFNPARS.FOR ; GROUP UFNPARS *************
C-----------------------------------------------------------
C  UFNPARS
C
C  parse a filename into its parts (dir, disk, prefix, shot no., suffix)
C
      SUBROUTINE UFNPARS(ZNAME,ZDEV,ZFDIR,ZPRE,ISHOT,ZEXT,IER)
C
      use UFILES
      implicit none
 
      character*(*) ZNAME   ! INPUT filename spec
      character*(*) ZDEV    ! OUTPUT disk spec
      character*(*) ZFDIR   ! OUTPUT dir spec
      character*(*) ZPRE    ! OUTPUT filename prefix
      integer ISHOT         ! OUTPUT shot no. in filename (-1 if none)
      character*(*) ZEXT    ! OUTPUT filename .ext
      integer IER           ! OUTPUT completion code, 0=OK
C
      character*10 zdigs
 
      integer  ILPRE, ILEXT,  ILDEV, ILDIR, ILNAME, ilminp, irbrk
      integer  ic,    IST,    IBLANK,IDLMAX,ILDOT,  IDLM,   jshot
      integer  idigs, idec,   idlm1, ick,   IL,     ICLN,   ilinp
      integer  ier1,  ier2,   ier3,  ier4
C
      data zdigs/'0123456789'/
C
C----------------------------------------------------
C
      ILPRE=LEN(ZPRE)
      ILEXT=LEN(ZEXT)
      ILDEV=LEN(ZDEV)
      ILDIR=LEN(ZFDIR)
C
      ILNAME=LEN(ZNAME)
C
C  defaults:
C
      zpre=' '
      zext=' '
      zdev=' '
      zfdir=' '
C
      ishot=-1
C
      ier=0
C
C-------------------------
C
      if (zname(1:4) .eq. 'MDS+') go to 5000
C
      ilminp=len(zname)
      irbrk=0
      do ic=ilminp,1,-1
        if(zname(ic:ic).eq.'/') then
          if(irbrk.eq.0) irbrk=ic
        endif
      enddo
      if(irbrk.gt.ildir) then
        call uermsg('%UFNPARS:  directory name too long',zname,1)
        go to 999
      else if(irbrk.eq.0) then
        CALL UFNPAD(' ',1,ZFDIR,'UFNPARS',IER)
        IST=1
      else
        CALL UFNPAD(' ',1,ZFDIR,'UFNPARS',IER)
        ZFDIR(1:IRBRK)=ZNAME(1:IRBRK)
        IST=IRBRK+1
      endif
C
C  PREFIX & SHOT NO.
      IBLANK=INDEX(ZNAME(IST:),' ')+IST-1
      IF(IBLANK.LT.IST) IBLANK=LEN(ZNAME)+1
C
      IDLMAX=IST-1
C
      ILDOT=INDEX(ZNAME(IST:),'.')+IST-1
      IDLMAX=MAX0(IDLMAX,ILDOT)
      IF(ILDOT.LT.IST) ILDOT=LEN(ZNAME)+1
C
      IDLM=MIN0(IBLANK,ILDOT)
C
C  decode shot number
C
      jshot=0
      idigs=0
      idec=1
C
 88   continue
      if(idlm.gt.1) then
         idlm1=idlm-1
         ick=index(zdigs,zname(idlm1:idlm1))
         if((ick.gt.0).and.(idigs.le.5)) then
            idigs=idigs+1
            idlm=idlm-1
            jshot=jshot+idec*(ick-1)
            idec=idec*10
            go to 88
         endif
      endif
C
      if(idigs.gt.0) then
         ishot=jshot
      endif
C
      IF(IDLM.LE.IST) THEN
        CALL UERMSG('%UFNPARS: UFILE NAME PREFIX NOT FOUND',ZNAME,1)
        GO TO 999
      ELSE IF((IDLM-IST).GT.ILPRE) THEN
        CALL UERMSG('%UFNPARS: UFILE NAME PREFIX LENGTH ERROR',ZNAME,1)
        GO TO 999
      ENDIF
      CALL UFNPAD(' ',1,ZPRE,'UFNPARS',IER)
      ZPRE(1:IDLM-IST)=ZNAME(IST:IDLM-1)
C
      IF(IDLM.EQ.IBLANK) then
         call uermsg('%UFNPARS:  no Ufile suffix',zname,1)
         GO TO 999
      endif
      IST=IDLMAX+1
C
C  SUFFIX
      IL=IBLANK-IST
      IF(IL.LE.0) then
         call uermsg('%UFNPARS:  no Ufile suffix',zname,1)
         GO TO 999
      endif
      IF(IL.GT.ILEXT) THEN
        CALL UERMSG('%UFNPARS: UFILE NAME .EXT LENGTH ERROR',ZNAME,1)
        GO TO 1000
      ENDIF
      CALL UFNPAD(' ',1,ZEXT,'UFNPARS',IER)
      ZEXT(1:IL)=ZNAME(IST:IBLANK-1)
C
      GO TO 1000
C
 999  CONTINUE
      ier=1
C
 1000 CONTINUE
C
      RETURN
C
 5000 continue
C MDSplus
C     pre            ext     dev               dir
C MDS+::<server>/Tree(shot)/path              /node
C MDS+::<server>/Tree      /pppl@tok.yr@runid/node
C Server = zpre
      idlm = index(zname,'/')-1
      call ufnpad(zname(1:idlm),idlm,zpre,'UFNPARS>mds: zpre',ier1)
C Tree = zext
      ist = idlm+2
      il = index(zname(ist:),'/') -1
      call ufnpad(zname(ist:ist+il-1),il,zext,
     >            'UFNPARS>mds: zext',ier2)
C path = zdev
      ist = ist+il+1
      icln = index(zname(ist:),'/') -1
      if (icln .le. 0) then      ! no path
         call ufnpad(' ',1,zdev,'UFNPARS>mds: zdev',ier3)
      else
         call ufnpad(zname(ist:ist+icln-1),icln,zdev,
     >        'UFNPARS>mds: zdev',ier3)
      endif
C node = zdir
      ist = ist+icln+1
      il = index(zname(ist:),' ')-1
      if (il .lt. 0) il=ilname
      call ufnpad(zname(ist:il),il-ist+1,zfdir,
     >            'UFNPARS>mds: zdir',ier4)
 
      if ((IER1+IER2+IER3+IER4).GT.0) go to 999
 
      goto 1000
 
      	END
C******************** END FILE UFNPARS.FOR ; GROUP UFNPARS ***************
