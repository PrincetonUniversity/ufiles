C******************** START FILE GRFUF1.FOR ; GROUP GRFUF1 *************
C  dmc 11 Aug 1994
C   plot the bloated coordinate system used in TRANSP / fast ion code
C   pass the moments arrays; expend them here
C
      SUBROUTINE GRFASYM(rcx,ycx,mtbl,mimom,lcentr,jmax,nmom
     >      ,XUN,YUN,LBL1,LBL2,LBL3)
C
      use gscale_blk
      use plot1mmx
C
C  DATA:
      REAL rcx(mtbl,0:mimom,2),ycx(mtbl,0:mimom,2)
      integer mtbl,mimom,lcentr,jmax,nmom
C
C  LABELS:
      CHARACTER*(*) XUN,YUN,LBL1,LBL2,LBL3
C
C  CONTROL:
      INTEGER IMENU
C
C  IF IMENU=1 JUMP DIRECTLY TO REPLOT MENU (DO NOT DRAW GRAPH 1ST)
C
      integer icounter,ict
      character*15 zext
C
C  local arrays for contours plotted
C
      parameter(icmax=250)
      parameter(nthmax=50)
      real zrlocl(icmax),zylocl(icmax)
      integer, save :: iopcod=0
C
      EXTERNAL GRADUM  ! FOR GRAOPT CALL INTERFACE
C
      DATA IMENU/0/
C
C--------------------------------------------------------------
C
      icounter=0
C-------------------------------------------------------------
C  SCALE SET SWITCHE CLEARED:
C
      ILOCSC=0
C
C  GET DEFAULT AXIS SELECTION
C
      CALL GAXGDF(ISEL)
C
      IF(IMENU.EQ.1) GO TO 200
C
C----------------------------------------------------------------
C  AUTOMATIC GENERATION OF PLOT
C
 100  CONTINUE
      icounter=icounter+1
C
C  SET AXES
C
      CALL GAXCUR(ISEL)
C
      ict=1
      call zxpnd_grfasym(rcx,ycx,mtbl,mimom,jmax,nmom,
     >        zrlocl,zylocl,icmax)
C
      N=icmax
C
      IF(ILOCSC.EQ.0) THEN
C  COMPUTE X AND Y PLOT SCALE (USE GSCALE DEFAULTS)
        CALL GSCSET(zrlocl,zylocl,zylocl,0,N)
      ELSE IF(ILOCSC.EQ.1) THEN
C  COMPUTE Y AUTOSCALE ONLY
        CALL GSCMMX(zrlocl,N,zylocl,zylocl,0,N,1,1,1,1,N,XMIN,XMAX,1)
        CALL GSCYDF(1)
      ENDIF
C
      CALL GSCSEP(XMIN,XMAX)
      CALL GSCSEP(YMIN,YMAX)
C
      LUNTRM=LNOUT(0)
      CALL GAXCHK(LUNTRM,ISEL,ICOD) ! AXIS (LOG/LIN) CHECK
C
      CALL GSCREC  ! RECORD PLOT SCALE IN MEMORY
C
      CALL DPLOT1(zrlocl,zylocl,N,ICOD) ! DRAW GRID AND F(X) CURVE
      do j=jmax-1,lcentr+1,-1
         ict=ict+1
         call zxpnd_grfasym(rcx,ycx,mtbl,mimom,j,nmom,
     >           zrlocl,zylocl,N)
         call dplot1(zrlocl,zylocl,N,1)
      enddo
C
      N2=jmax-lcentr+1
      ict=0
      do ith=1,nthmax
         zth=float(ith-1)*6.283185/float(nthmax)
         call zxpn2_grfasym(rcx,ycx,mtbl,mimom,lcentr,jmax,nmom,
     >            zth,zrlocl,zylocl,N2)
         call dplot1(zrlocl,zylocl,N2,1)
      enddo
C
      CALL GRALBL(XUN,YUN,LBL1,LBL2,LBL3)  ! LABEL THE PLOT
C
      CALL UPAUST(2)  ! PAUSE AFTER PLOT
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
         RETURN
      ENDIF
C
      CALL UERMSI(' ?GRFASYM *FATAL BUG* UNANTICIPATED OPERATION CODE',
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
c---------------
      subroutine zxpnd_grfasym(rcx,ycx,mtbl,mimom,isurf,nmom,
     >    zr,zy,N)
      real rcx(mtbl,0:mimom,2),ycx(mtbl,0:mimom,2)
      real zr(N),zy(N)
c
      real zsn(100),zcs(100)
c
      do ith=1,N
         zth=float(ith-1)*6.283185/float(N-1)
         call uf_sincos(zth,nmom,zsn,zcs)
         zr(ith)=rcx(isurf,0,1)
         zy(ith)=ycx(isurf,0,1)
         do im=1,nmom
            zr(ith)=zr(ith)+rcx(isurf,im,1)*zcs(im)+
     >                        rcx(isurf,im,2)*zsn(im)
            zy(ith)=zy(ith)+ycx(isurf,im,1)*zcs(im)+
     >                        ycx(isurf,im,2)*zsn(im)
         enddo
      enddo
c
      return
      end
c
      subroutine zxpn2_grfasym(rcx,ycx,mtbl,mimom,lcentr,jmax,nmom,
     >    zth,zr,zy,N)
      real rcx(mtbl,0:mimom,2),ycx(mtbl,0:mimom,2)
      real zr(N),zy(N)
c
      real zsn(100),zcs(100)
c
      call uf_sincos(zth,nmom,zsn,zcs)
c
      do isurf=lcentr,jmax
         ind=isurf-lcentr+1
         zr(ind)=rcx(isurf,0,1)
         zy(ind)=ycx(isurf,0,1)
         do im=1,nmom
            zr(ind)=zr(ind)+rcx(isurf,im,1)*zcs(im)+
     >                        rcx(isurf,im,2)*zsn(im)
            zy(ind)=zy(ind)+ycx(isurf,im,1)*zcs(im)+
     >                        ycx(isurf,im,2)*zsn(im)
         enddo
      enddo
c
      return
      end
