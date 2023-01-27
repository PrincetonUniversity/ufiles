      subroutine ufmds_size2(znode,zbuf,nbuf,zx,nxmax,
     >     nxact,ntact,iskip1,xlab,
     >     ier)
C
      use UFILES
C
C  this routine (called from uf2drd) deals with a situation where a 2d
C  MDS+ structure is accessed which has the structure
C
C  f(nx,nt) varies vs. x & t, but the shape of x is (nx,nt), i.e. the
C  x axis as well as f also varies with t.
C
C  (this is typical e.g. of TRANSP output when the geometry of the
C  experiment is time varying -- i.e. usually).
C
C  Since Ufiles cannot represent the t variation of x, a fallback must
C  be taken;  this is either
C
C    1) replace x with x @t0 only, if x is not "really" changing with t
C    2) replace x with the index vector [1,2, ..., nx] if x really does
C      have time variation.
C
C    in either case a warning message will be displayed, and the iskip1
C  flag will be set.  in the 2nd case the label "xlab" is also changed.
C
C---------------------------------
C History:
C 11/18/99 CAL: new calling arguments for mds_value
C
C---------------------------------
C
C  arguments:
C
      character*(*) znode       ! MDS+ path to signal (input)
      real zbuf(nbuf)           ! buffer to hold time varying x axis
      real zx(nxmax)            ! x axis data (may be set here)
C
      integer nxact             ! x axis size to be used
      integer ntact             ! t axis size
C
      integer iskip1            ! set =1 if x axis has been set here
      character*30 xlab         ! x label:  modified if x redefined as index
C
      integer ier               ! completion code  0 = normal
C
C---------------------------------
C
      character*128 zerrmsg
 
      integer     idescr_long, iretl
      save    itot
C      volatile    itot
C
C---------------------------------
C
C  executable code
C
      ier=0
      iskip1=0
C
      iln=index(znode,' ')-1
      if(iln.le.0) iln=len(znode)
C
      istat=mds_value('size('//znode(1:iln)//')',
     1      idescr_long(itot),iretl)
C
      if(mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_SIZE2',istat,ier,zerrmsg,ilm)
         write(ufelun,9508) znode(1:iln),zerrmsg(1:ilm)
 9508    format('  seeking size of MDS+ signal: ',a/2x,a)
      endif
      if(ier.ne.0) go to 9000
C
C  sizes of dimensions
C
      call ufmds_size(znode(1:iln),1,inumx,ier)
      if(ier.ne.0) go to 9000
C
      call ufmds_size(znode(1:iln),2,inumy,ier)
      if(ier.ne.0) go to 9000
C
      itest=0
      if(min(inumx,inumy,itot).le.0) then
         itest=-1               ! error
      else if((inumx*inumy).eq.itot) then
         itest=1                ! traditional situation
      else if((inumx.eq.itot).and.((itot/inumy)*inumy.eq.itot)) then
         itest=2                ! x varies as fcn of t
C  further tests on x
         irankx=0
         istat=mds_value('rank(dim_of('//znode(1:iln)//',0))',
     >        idescr_long(irankx),iretl)
         if((mod(istat,2).ne.1).or.(irankx.ne.2)) then
            write(ufelun,9509) istat,irankx
 9509       format('?UFMDS_SIZE2:  checking rank of x coordinate:'/
     >           '  istat=',i10,' rank=',i3)
            itest=-2
         else
            call ufmds_size('dim_of('//znode(1:iln)//',0)',2,inumy2,ier)
            if(ier.ne.0) go to 9000
            if(inumy2.ne.inumy) then
               write(ufelun,9511) inumy,inumy2
 9511          format(
     >              ' ?UFMDS_SIZE2:  coordinates shapes inconsistent:'/
     >              '  size(2nd_dim(function data)) = ',i6/
     >              '  size(2nd_dim(1st_dim data))  = ',i6)
               itest=-3
            endif
         endif
      endif
C
      if(itest.le.0) then
         write(ufelun,9520) inumx,inumy,itot
 9520    format(
     >' ?UFMDS_SIZE2:  inconsistent or unanticipated signal component ',
     >'sizes:'/'  nx=',i6,' ny=',i6,' itot=',i8)
         ier=1
      else if(itest.eq.1) then
C  traditional situation:  x axis and t axis are independent
         nxact=inumx
         ntact=inumy
         ier=0
      else
C  x depends on t -- must read and analyze dependence.
         nxact=itot/inumy
         ntact=inumy
         iskip1=1
         ier=0
C
C  if buffer size is exceeded, this will be caught by caller (uf2drd)
         if(itot.gt.nbuf) go to 9000
C
         call ufmds_read('dim_of('//znode(1:iln)//',0)',zbuf,itot,ier)
         if(ier.ne.0) go to 9000
C
         call ufmds_svck(zbuf,nxact,ntact,idif,xlab)
C
         if(idif.eq.0) then
            do ix=1,nxact
               zx(ix)=zbuf(ix)
            enddo
         else
            write(ufelun,9550)
 9550       format(
     >' %UFMDS_SIZE2:  1st independent coordinate not independent'/
     >           '  of 2nd independent coordinate ==> 1st coordinate'/
     >           '  REPLACED with grid index specifier.')
            do ix=1,nxact
               zx(ix)=float(ix)
            enddo
            xlab='grid index          index     '
         endif
      endif
C
 9000 continue
      return
      end
C
C-------------------------
C
      subroutine ufmds_svck(zbuf,inx,iny,idif,xlab)
C
C  compare rows of array for near-equality
C  return idif=1 if different, idif=0 if identical
C
      real zbuf(inx,iny)
C
      character*(*) xlab
C
C-----------------------------
C
      idif=0
C
C  use 1.0e-6 x (the max modulus of the first row of elements)
C  as a minimum difference criterion
C
      iascend=1
      zmindif=1.0e-35
      do ix=1,inx
         zmindif=max(zmindif,1.0e-6*abs(zbuf(ix,1)))
         if(ix.gt.1) then
            if(zbuf(ix,1).le.zbuf(ix-1,1)) iascend=0
         endif
      enddo
C
      do iy=2,iny
         do ix=1,inx
            zdif=abs(zbuf(ix,iy)-zbuf(ix,1))
            if(zdif.gt.zmindif) then
               idif=1
               go to 100
            endif
         enddo
      enddo
C
C  no differences -- if btw 0 and 1 this is "almost certainly" a
C  TRANSP flux surface x axis -- add some code to detect this case.
C
      if((iascend.eq.1).and.(idif.eq.0)) then
         if(xlab.eq.'(dim 0)') then
            zxmin=zbuf(1,1)
            zxmax=zbuf(inx,1)
            if((0.0.le.zxmin).and.(zxmin.le.1.0)) then
               zdx=1.0/float(inx)
               zdx2=0.5*zdx
C
               if(abs(zdx-zxmin).le.zmindif) then
                  if(abs(1.0-zxmax).le.zmindif) then
                     xlab='xb'
                  endif
               else if(abs(zdx2-zxmin).le.zmindif) then
                  if(abs((1.0-zxmax)-zdx2).le.zmindif) then
                     xlab='x'
                  endif
               endif
            endif
         endif
      endif
C
 100  continue
      return
      end
 
