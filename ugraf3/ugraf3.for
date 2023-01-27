C******************** START FILE UGraf3.FOR ; GROUP UGraf3 ************
C--------------------------------------------------------------
C  UGraf3
C  PROGRAM TO GRAPH CONTENTS OF 3D USER DATA FILE
C
      PROGRAM UGraf3
 
C
C	Updates:
C
C       dmc 12 Aug 2000 -- rewrite for f90; use dynamic arrays
C                          use new trgraf routine grf4f1
C
C       05/10/95 JFS - Increased Mfz from 7 to 40 (makes total= 400 000)
C       10/06/93 TbT - Corrected MNX variable name to Mfx.
C       10/04/93 TBT - Small changes to Grf3F1.
C	09/23/93 TbT - Created from Ugraf2.for
 
C  DATA BUFFERS AND LABELS
      real, dimension(:), allocatable :: x,y,z,scv
      real, dimension(:,:,:), allocatable :: f
      character(30), dimension(:), allocatable :: sclab
      integer, dimension(:), allocatable :: iordsc
 
C  header info
      integer info(8)
 
      CHARACTER*10 ZSDATE
      character*30 xlab,ylab,zlab, flab
      Character*45 ZTITLE
 
      CHARACTER*4 Cdev
 
      CHARACTER*10 LYABL(5)
C
      logical lxout
C
      Character*6 Cshot
 
      DATA LYABL/'          ',' AVERAGED.',' SMOOTHED.',
     >             ' AVG+SM''D.',' SPECIAL. '/
C
C  L.U.N FOR DATA INPUT
C
      DATA ILUN/21/
C
C  TEMPORARY COMMENTS FILE
C
      DATA ILUNC/22/
      DATA ICOPN/0/
C
C	.Executable code:
C  -------------------------------------------------------------------------
C
      CALL UINITG("UGRAF3")
      call urdext('ifi')
      luntrm=lnout(0)
C
 10   CONTINUE
      CALL UFNTER(ILUN,'3D INPUT FILES      ')
C
 20   CONTINUE
      CALL NEUPAG
 21   CONTINUE
      CALL URSHOT('UGraf3',', "0" TO QUIT, "D" TO CHANGE NAMES','QD',
     >	  ISHOT,IOP)
C
      IF(IOP.EQ.1) GO TO 900
      IF(IOP.EQ.2) GO TO 10
      IF(ISHOT.EQ.0) GO TO 900
C
      Write (Cshot, 9123) Ishot
 9123 Format(I6)
 
      CALL UFOPRD(ILUN,ISHOT,IER)
      IF(IER.NE.0) CALL UCLEAR
      IF(IER.NE.0) GO TO 21
C
C  ** get dimensions **
C
      call ufhdck(ilun,info,ier)
      if((ier.ne.0).or.(info(1).ne.3)) then
         if(ier.ne.0) write(6,*) ' %ier=',ier,' after ufhdck call.'
         if(ier.eq.0) write(6,*) ' %ufile dimensionality:  ',info(1)
         call uermsg('?ugraf3:  not a 3d Ufile; use ugraf1 or ugraf2!',
     >      ' ',0)
         call ufclos(ilun)
         go to 10
      endif
      maxind=max(info(3),info(4))
      mfx=info(3)
      mfy=info(4)
      mfz=info(5)
      maxsc=info(8)
C
      if(allocated(f)) deallocate(f)
      allocate(f(mfx,mfy,mfz))
C
      if(allocated(x)) deallocate(x)
      allocate(x(mfx))
C
      if(allocated(y)) deallocate(y)
      allocate(y(mfy))
C
      if(allocated(z)) deallocate(z)
      allocate(z(mfz))
C
      if(allocated(scv)) deallocate(scv)
      allocate(scv(maxsc))
C
      if(allocated(sclab)) deallocate(sclab)
      allocate(sclab(maxsc))
C
      if(allocated(iordsc)) deallocate(iordsc)
      allocate(iordsc(maxsc))
C
C	IX1MAX=MAXTOT/MAXIND
      Iflag = 1
 
      CALL UF3DRD(ILUN,Cdev,ZSDATE,
     >              f,Mfx,Mfy,
     1              X,Mfx, Y,Mfy, Z,Mfz, IPROC,
     >              NXACT,NYACT,NZact, Iflag,
     >              FLAB,XLAB,YLAB,Zlab, MAXSC,NUMSC,SCV,SCLAB, IER)
C
      IF(IER.NE.0) THEN
        CALL UCLEAR
        CALL UFCLOS(ILUN)
        GO TO 20
      ENDIF
 
      IND=MIN0(5,(IPROC+1))
C
      Ztitle =  CDev  // Cshot // '  Shotdate: ' //
     1            Zsdate // Lyabl(Ind)
 
 
C  SPOOL COMMENTS TO TEMPORARY FILE
      IF(ICOPN.EQ.1) CLOSE(UNIT=ILUNC,STATUS='DELETE')
C	OPEN(UNIT=ILUNC,FILE='ZUFCMMTS.TMP',STATUS='SCRATCH',
      OPEN(UNIT=ILUNC,STATUS='SCRATCH',
     >       ACCESS='SEQUENTIAL')
      ICOPN=1
C
      CALL USPOOL(ILUN,ILUNC)
      CALL UFCLOS(ILUN)
C
C	-------------------------------------------------------------
 
 
      Fmin = f(1,1,1)
      Fmax = Fmin
      Do 2150 Iz=1,Nzact
        Do 2140 Iy=1,Nyact
          Do 2130 Ix=1,Nxact
             Fmin=min(Fmin,f(ix,iy,iz))
             Fmax=max(Fmax,f(ix,iy,iz))
 2130     Continue
 2140   Continue
 2150 Continue
 
      Write(Luntrm, 9103)  Flab
 9103   Format(/' 3D Ufile Successfully read' /
     1          ' 3D Ufile Label = ', A /)
      Write(Luntrm, 9101) 'X', Xlab, Nxact, X(1), X(Nxact)
      Write(Luntrm, 9101) 'Y', Ylab, Nyact, Y(1), Y(Nyact)
      Write(Luntrm, 9101) 'Z', Zlab, Nzact, Z(1), Z(Nzact)
 9101   Format( 1x,A1, ' axis   ', A / I15, ' values from ',
     1           1PE15.5, ' to ', 1PE15.5)
	
 
      Write(Luntrm, 9102) Fmin, Fmax
 9102   Format ( / ' Minimum data value = ', 1PE15.5 /
     1             ' Maximum data value = ', 1PE15.5 /)
 
 
 30   CONTINUE
C
      WRITE(LUNTRM,3001) NUMSC
 3001 FORMAT(' NUMBER OF ASSOCIATED SCALARS IN FILE:  ',I3)
      IF(LXOUT(0)) WRITE(LUNTRM,3003)
 3003 FORMAT(/' ** UGraf3 OPTIONS:'/
     >'  1 - PRESET PLOT SCALING/AXES DEFAULTS (2D PLOTS ONLY)'/
     >'  P - PLOT THE DATA'/
     >'  L - LIST THE ASSOCIATED SCALAR QUANTITIES'/
     >'  C - SPOOL FILE COMMENTS TO TTY (USE CTRL(Q), CTRL(S))'/
     >'  S - GET NEW SHOT'/
     >'  D - CHANGE FILENAMING CONVENTION'/
     >'  Q - QUIT UGraf3')
 
      CALL UREAD('UGraf3:  ENTER CONTROL CODE:$')
      IOP=IXUOPC('1PLCSDQ$')
      CALL UCHKOP(IOP,0,1,100,-1,*30)
      GO TO (35,100,50,40,20,10,900) IOP
C
 35   CONTINUE
      CALL GPRESET
      GO TO 30
C
C  SPOOL COMMENTS TO TTY
 40   CONTINUE
      REWIND ILUNC
      CALL NEUPAG
      CALL USPOOL(ILUNC,LUNTRM)
      CALL UPAUST(1)
      GO TO 30
C
 50   CONTINUE
      IF(NUMSC.GT.0)
     >     CALL UFSCLK(ILUN,ISHOT,NUMSC,SCV,SCLAB,IORDSC,0,0)
      GO TO 30
C
C------
 100  CONTINUE
      ISMOO=0
C
      call grf4f1(f,nxact,nyact, x,nxact,y,nyact,z,nzact,
     >   flab(1:20), xlab(1:20),ylab(1:20),zlab(1:20),
     >   flab(21:30), xlab(21:30),ylab(21:30),zlab(21:30),
     >   ztitle, iproc)
C
      GO TO 30
C  EXIT
 900  CONTINUE
        CALL NEUPAG
        CALL UEXITA(0)
        STOP
	
 
C
      	END
C******************** END FILE UGraf3.FOR ; GROUP UGraf3 ***************
