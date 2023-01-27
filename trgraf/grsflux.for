C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  dmc 11 Aug 1994
C   plot the bloated coordinate system used in TRANSP / fast ion code
C    use MOMRY for the expansion; this will also test the Fourier Spline
C
C   give user control over the flux coordinate range
C
c   mod dmc -- momry->zmomry, passed external routine
C
      SUBROUTINE GRSFLUX(zmomry,XUN,YUN,LBL1,LBL2,LBL3)

      use gscale_blk
      use plot1mmx
 
      external zmomry
 
C  Modified:
C   04/05/96  TBT  Added Logical Lchar
C
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  local arrays for contours plotted
C
      parameter(icmax=250)
      real zrlocl(icmax),zylocl(icmax)
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      logical lxout
      Logical Lchar
      integer, save :: iopcod=0
C
      real zjaco(2,2)
C
      integer maxjac
      parameter (maxjac=201)
      real x(maxjac),th(maxjac),jac(maxjac,maxjac),wk(maxjac)
      real gradx(maxjac,maxjac),gradth(maxjac,maxjac)
C
      character*64 wname
      character*40 gname,dname
      integer icounter
      character*15 zext
C
      character*20 zmode(2),altmode(3)
      data zmode/'mesh plot ',' '/
      data altmode/'jacobian plot','|grad(xi)| plot','|grad(th)| plot'/
C----------------------------------------------------------------
C
      icounter=0
C-------------------------------------------------------------
      ialt=1
      zmode(2)=altmode(ialt)
      imode=1
      imodj=0
      inth=40
      inxi=50
C
      zximin=0.0
      zximax=1.0
      zthmin=0.0
      zthmax=6.283185
C
      LUNTRM=LNOUT(0)
C
 10   continue
      if(lxout(0)) write(luntrm,1001) inth,inxi,altmode(ialt),
     >     zximin,zximax,
     >     zthmin,zthmax,zmode(imode),imodj.eq.1
 1001 format(/
     >  ' grsflux options:'/
     >  ' C - change line counts, presently nth=',i3,' nxi=',i3/
     >  ' G - alternate mode definition, currently set to:  ',a/
     >  ' X - change xi range, presently ximin/max =',2(1x,1pe11.4)/
     >  ' T - change th range, presently thmin/max =',2(1x,1pe11.4)/
     >  ' P - proceed to make plot'/
     >  ' M - change plot mode, currently set to:  ',a/
     >  ' J - include Jacobian calculation in spider plots (',L1,')'/
     >  ' S - suppress Jacobian calculation in spider plots'/
     >  ' Q - quit')
C
      call uread('grsflux:  control option:$$')
      if(lchar('Q')) then
         return
      else if(lchar('P')) then
         go to 99
      else if(lchar('M')) then
         imode=3-imode
      else if(lchar('G')) then
         if(lxout(0)) then
            do i=1,3
               write(luntrm,*) ' alt mode #',i,': ',trim(altmode(i))
            enddo
         endif
         call ureadi('grsflux:  alt mode plot selection (choose #):',
     >        ialt)
         ialt=max(1,min(3,ialt))
         zmode(2)=altmode(ialt)
      else if(lchar('C')) then
         if(lxout(0)) then
            write(luntrm,*) ' maximum allowed values: ',maxjac
         endif
         call ureadi('grsflux:  new nxi:$$',inxi)
         inxi=max(2,min(maxjac,inxi))
         call ureadi('grsflux:  new nth:$$',inth)
         inth=max(2,min(maxjac,inth))
      else if(lchar('X')) then
         zximin0=zximin
         zximax0=zximax
         call ureadf('grsflux:  new ximin:$$',zximin0)
         call ureadf('grsflux:  new ximax:$$',zximax0)
         zximin=min(zximin0,zximax0)
         zximax=max(zximin0,zximax0)
      else if(lchar('T')) then
         zthmin0=zthmin
         zthmax0=zthmax
         call ureadf('grsflux:  new thmin:$$',zthmin0)
         call ureadf('grsflux:  new thmax:$$',zthmax0)
         zthmin=min(zthmin0,zthmax0)
         zthmax=max(zthmin0,zthmax0)
      else if (lchar('S')) then
         imodj=0
      else if(lchar('J')) then
         imodj=1
      else
         call uermsg('grsflux:  unrecognized opcode.',' ',0)
      endif
      go to 10
C
C----------------------------------------------------------------
C
 99   continue
C
C  SCALE SET SWITCHE CLEARED:
C
      ILOCSC=0
C
C  GET DEFAULT AXIS SELECTION
C
      CALL GAXGDF(ISEL)
C
C----------------------------------------------------------------
C  AUTOMATIC GENERATION OF PLOT
C
 100  CONTINUE
      if(imode.eq.1) then
         icounter=icounter+1
      endif
C
C  SET AXES
C
      CALL GAXCUR(ISEL)
C
      N=icmax
      N4=icmax/4
      Ntmp=4*N4
C  build a bounding contour, for scale-setting purposes
C
      if(imode.eq.1) then
         zdetmin=1.e30
         zdetmax=-1.e30
         ic=0
         zth=zthmin
         do i4=1,n4
            zxi=zximin+float(i4-1)*(zximax-zximin)/float(n4-1)
            call zmomry(zxi,zth,imodj,zrout,zyout,zjaco)
            ic=ic+1
            zrlocl(ic)=zrout
            zylocl(ic)=zyout
            if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >         zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
         enddo
         do i4=1,n4
            zth=zthmin+float(i4)*(zthmax-zthmin)/float(n4)
            call zmomry(zxi,zth,imodj,zrout,zyout,zjaco)
            ic=ic+1
            zrlocl(ic)=zrout
            zylocl(ic)=zyout
            if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >         zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
         enddo
         do i4=1,n4
            zxi=zximax+float(i4)*(zximin-zximax)/float(n4)
            call zmomry(zxi,zth,imodj,zrout,zyout,zjaco)
            ic=ic+1
            zrlocl(ic)=zrout
            zylocl(ic)=zyout
            if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >         zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
         enddo
         do i4=1,n4
            zth=zthmax+float(i4)*(zthmin-zthmax)/float(n4)
            call zmomry(zxi,zth,imodj,zrout,zyout,zjaco)
            ic=ic+1
            zrlocl(ic)=zrout
            zylocl(ic)=zyout
            if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >         zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
         enddo
C
         IF(ILOCSC.EQ.0) THEN
C  COMPUTE X AND Y PLOT SCALE (USE GSCALE DEFAULTS)
            CALL GSCSET(zrlocl,zylocl,zylocl,0,Ntmp)
         ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
            CALL GSCMMX(zrlocl,Ntmp,zylocl,zylocl,0,Ntmp,
     >         1,1,1,1,Ntmp,XMIN,XMAX,1)
            CALL GSCYDF(1)
         ENDIF
C
         CALL GSCSEP(XMIN,XMAX)
         CALL GSCSEP(YMIN,YMAX)
C
         CALL GAXCHK(LUNTRM,ISEL,ICOD)  ! AXIS (LOG/LIN) CHECK
C
         CALL GSCREC                    ! RECORD PLOT SCALE IN MEMORY
      endif                             ! imode
C
C  plot xi lines
      if(imode.eq.1) then
         ict=0
         do ixi=inxi,1,-1
            zxi=zximin+float(ixi-1)*(zximax-zximin)/float(inxi-1)
            do i=1,n
               zth=zthmin+float(i-1)*(zthmax-zthmin)/float(n-1)
               call zmomry(zxi,zth,imodj,zrlocl(i),zylocl(i),zjaco)
               if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >            zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
            enddo
            ict=ict+1
            if(ict.eq.1) then
               call dplot1(zrlocl,zylocl,n,icod)
               call tsend
            else
               call dplot1(zrlocl,zylocl,n,1)
               call tsend
            endif
         enddo
      endif                             !imode
C
      ict=0
      do ith=1,inth
         ict=ict+1
         zth=zthmin+float(ith-1)*(zthmax-zthmin)/float(inth-1)
         if(imode.eq.1) then
            do i=1,n
               zxi=zximin+float(i-1)*(zximax-zximin)/float(n-1)
               call zmomry(zxi,zth,imodj,zrlocl(i),zylocl(i),zjaco)
               if(imodj.eq.1) call zjstats(zxi,zth,zjaco,
     >            zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
            enddo
            call dplot1(zrlocl,zylocl,n,1)
            call tsend
         else
            th(ith)=zth
            do ixi=1,inxi
               zxi=zximin+float(ixi-1)*(zximax-zximin)/float(inxi-1)
               x(ixi)=zxi
c
               call zmomry(zxi,zth,1,zrdum,zydum,zjaco)
               jac(ixi,ith)=zjaco(1,1)*zjaco(2,2)-zjaco(1,2)*zjaco(2,1)
               if(abs(jac(ixi,ith)).ne.0.0) then
                  gradx(ixi,ith)=sqrt(zjaco(2,1)**2+zjaco(2,2)**2)/
     >                 abs(jac(ixi,ith))
                  gradth(ixi,ith)=sqrt(zjaco(1,1)**2+zjaco(1,2)**2)/
     >                 abs(jac(ixi,ith))
               else
                  gradx(ixi,ith)=0.0
                  gradth(ixi,ith)=0.0
               endif
c
            enddo
         endif
      enddo
C
      if(imode.ne.1) then
         if(ialt.eq.1) then
            call grf3f1(x,jac,th,wk,inxi,inth,maxjac,
     >           'x','2x2 Jacobian','theta','x','J','theta',
     >           'grsflux jacobian debug plot','grsflux',0)
         else if(ialt.eq.2) then
            call grf3f1(x,gradx,th,wk,inxi,inth,maxjac,
     >           'x','|grad(x)|','theta','x','1/m','theta',
     >           'grsflux |grad(x)| debug plot','grsflux',0)
         else if(ialt.eq.3) then
            call grf3f1(x,gradth,th,wk,inxi,inth,maxjac,
     >           'x','|grad(th)|','theta','x','rad/m','theta',
     >           'grsflux |grad(th)| debug plot','grsflux',0)
         endif
         go to 10
      endif
C
      CALL GRALBL(XUN,YUN,LBL1,LBL2,LBL3)  ! LABEL THE PLOT
C
      CALL UPAUST(2)  ! PAUSE AFTER PLOT
c
      if(imodj.eq.1) then
         write(6,*) ' min det(J) = ',zdetmin
         write(6,*) ' ...at (xi,th)=',zxijmin,zthjmin
         write(6,*) ' max det(J) = ',zdetmax
         write(6,*) ' ...at (xi,th)=',zxijmax,zthjmax
      endif
c
      GO TO 200
C
C  POST PLOT OPTIONS
C
 200  CONTINUE
C
C  GENERATE MENU AND FETCH USER REPLY
C
      CALL GRAOPT('SA',ISEL,ILOCSC,IOPCOD,GRADUM)
C
C  ACT ON USER REPLY - REPLOT?
C
      IF(IOPCOD.EQ.1) THEN
        GO TO 100
      ELSE IF(IOPCOD.EQ.2) THEN
        go to 10
      ENDIF
C
      CALL UERMSI(' ?GRFSSYM *FATAL BUG* UNANTICIPATED OPERATION CODE',
     >              IOPCOD,1)
      call ufbad_exit
C
      contains
         subroutine make_zext(zpre)
         character*(*), intent(in) :: zpre

         zext=' '
         if(ict.le.9) then
            write(zext,'(a,I1)') trim(zpre),ict
         else if(ict.le.99) then
            write(zext,'(a,I2)') trim(zpre),ict
         else if(ict.le.999) then
            write(zext,'(a,I3)') trim(zpre),ict
         else if(ict.le.9999) then
            write(zext,'(a,I4)') trim(zpre),ict
         else if(ict.le.99999) then
            write(zext,'(a,I5)') trim(zpre),ict
         else if(ict.le.999999) then
            write(zext,'(a,I6)') trim(zpre),ict
         endif

         end subroutine make_zext
      END
c------------------------------------------------------------------------
      subroutine zjstats(zxi,zth,zjaco,
     >   zdetmin,zdetmax,zxijmin,zxijmax,zthjmin,zthjmax)
c
c  find min/max jacobian
c
      implicit NONE
c
      real zxi,zth,zjaco(2,2)           ! input
      real zdetmin,zdetmax              ! min, max det (in/out)
      real zxijmin,zxijmax              ! zxi of min, max (in/out)
      real zthjmin,zthjmax              ! zth of min, max
c
c------------------
c
      real zdet
c
c------------------
c
      zdet=zjaco(1,1)*zjaco(2,2)-zjaco(1,2)*zjaco(2,1)
c
      if(zdet.lt.zdetmin) then
         zdetmin=zdet
         zxijmin=zxi
         zthjmin=zth
      endif
c
      if(zdet.gt.zdetmax) then
         zdetmax=zdet
         zxijmax=zxi
         zthjmax=zth
      endif
c
      return
      end
