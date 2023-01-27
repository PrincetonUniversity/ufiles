      subroutine mds_rdwr(lunerr, tr_socket, tr_tree, tr_pulse,
     >                    signam, zfile, device, opt, ier)
C Read a ufile and write into MDSplus
C 02/09/01 CAL
C
      implicit none
C Input
      integer       lunerr    ! lun for error messages
      integer       tr_socket ! TRANSP-run mds_socket
      character*(*) tr_tree   ! TRANSP-run Tree name
      integer       tr_pulse  ! TRANSP-run mds_pulse
      character*(*) signam    ! MDS signal name path
      character*(*) zfile     ! Ufile Name
      character*(*) device    ! Tokamak
      integer       opt       ! 1 = 1st time: connect to server
C                             ! 0 = don't connect
C Output
      integer       ier       ! = 0 success
C                             ! = 2 Ufile read error
C                             ! = 4 Ufile Write error
C                             ! other = error
C
      character*20  label     ! output = TRDAT Label
 
      integer       ilnurd    ! ufencod function
C
C  OUTPUT from UFHDCK
C    INFO -- INFORMATION BLOCK
C    INFO(1)=FILE DIMENSIONALITY
C    INFO(2)=TOTAL # OF WORDS IN DEPENDENT DATA ARRAY
C    INFO(3)=SIZE OF FIRST DIMENSION OF DATA ARRAY
C    INFO(4)=SIZE OF SECOND DIMENSION OF DATA ARRAY (IF APPLICABLE)
C     ...
C    INFO(8)=NUMBER OF SCALARS IN DATA FILE
C
C    IER = 0 IF ROUTINE COMPLETES EXECUTION SUCCESSFULLY
      integer info(8)
 
      character*50  zdev,zpre,zext
      character*128 zdir
 
      integer ilun,icdf
      integer ishot
 
C to read ufile
      integer  maxsc, maxd, maxx, maxz
      parameter(maxsc=200)
      parameter(maxx=300000)
      parameter(maxz=200)
      parameter(maxd=8000000)
c
      real         scval(maxsc)
      character*30 sclab(maxsc)
c
      real         f(maxd), x(maxx), y(maxx), z(maxz)
      character*30 xlab, ylab, zlab, flab
c
      integer      iproc
      character*10 shdate
      character*4  tokdev
      !AEP related
      integer :: isiza,npack,MKAEP,MTAEP
      integer :: MdEAEP,MdpAEP,MEAEP,MpAEP,MmuAEP !dimensions for AEP
      real, dimension(:), allocatable :: dat_buf
      character*30 ::  sclab_buf(3)
      character*9 :: ctunits,ceunits !units for Time and Energy from Ufile  
 
      integer max1, inx, iny, inz, iflag, l, stat
C MDSplus
      integer      MdsSetSocket
C-----------------------------------------------------------------------
c
C  initialize
C
      call uinita(0)
C
      call ufinit
      call ufncap(2)
      call ufnpars(zfile,zdev,zdir,zpre,ishot,zext,ier)
      if(ier.ne.0) return
C
      ilun=97
C
C Open file
      call ufsetr(ilun,zpre,zext,zdev,zdir)
      if(ishot.lt.0) call ufnsho(ilun)
C MDSplus
      if (zpre(1:4) .eq. 'MDS+') then
         if (opt .eq. 1) then                ! 1st time connect to server
            call ufoprd(ilun,max(0,ishot),ier)
         else                                ! follow up
            call ufoprd(ilun,min(ishot,-ishot),ier)
         endif
         if(ier.ne.0) then
             print '(1x,a,'':  open error'')', zfile
             ier=-77
             return
         endif
C NOT MDSplus
      else
          call ufoprd(ilun,max(0,ishot),ier)
          if(ier.ne.0) then
C Ripple file
              if (zext .eq. 'RPL' .or. zext .eq. 'rpl') then
                  call ufsetr(ilun,zpre,zext,' ',' ')
                  call ufnsho(ilun)
                  call ufoprd(ilun,min(ishot,-ishot),ier)
              endif
          endif
          if(ier.ne.0) then
              print '(1x,a,'':  open error'')', zfile
              return
          endif
      endif
C
C Read Header
      if (zext .eq. 'AEP' .or. zext .eq. 'aep') then
!AEP operator  spcial treatment
!  read data
!  get dimensions, netcdf Ufile will be closed if an error occured
         call ufcdfrd_aep_dim(lunerr,ilun,icdf,npack,MKAEP,MTAEP,MdEAEP,
     >        MdpAEP,MEAEP,MpAEP,MmuAEP,ier)
         if(ier.gt.0)then
            return
         endif
         isiza = 8 + MTAEP + MKAEP*(17+MTAEP) + 2*npack
         if(allocated(dat_buf)) deallocate(dat_buf)
         allocate(dat_buf(isiza))
         sclab_buf(1)="AEP operator"
         !get AEP data into a buffer and close ilun file
         call UFCDFRD_AEP_DATA(lunerr,icdf,ishot,npack,MKAEP,MTAEP,
     >        MdEAEP,MdpAEP,MEAEP,MpAEP,MmuAEP,isiza,dat_buf,
     >        ctunits,ceunits,ier) 
         sclab_buf(2)=ctunits
         sclab_buf(3)=ceunits
         if(ier.ne.0) then
            print '(1x,a,'':  open error'')', zfile
            ier=2
            return
         endif
         info(1)=0
!MG         goto 101
      else
         call ufhdck(ilun,info,ier)
      endif
      if(ier.ne.0) then
          print '(1x,a,'':  read error'')', zfile
          goto 100
      endif
      flab=' '
      if (info(1) .eq. 1) then
	  call uf1drd(ilun,tokdev,shdate,
     >      x,y,maxx,inx,iproc,xlab,ylab,
     >      maxsc,info(8),scval,sclab,ier)
      else if (info(1) .eq. 2) then
	  iflag=1
	  max1=maxd/maxx
	  call uf2drd(ilun,tokdev,shdate,
     >      f,max1,x,maxx,y,maxx,iproc,
     >      inx,iny,iflag,flab,xlab,ylab,
     >      maxsc,info(8),scval,sclab,ier)
      else if (info(1) .eq. 3) then
	  iflag=1
	  max1=maxd/maxz/maxz
	  call uf3drd(ilun,tokdev,shdate,
     >      f,max1,maxz,x,maxx,y,maxx,z,maxz,iproc,
     >      inx,iny,inz,iflag,flab,xlab,ylab,zlab,
     >      maxsc,info(8),scval,sclab,ier)
      endif
      if(ier .ne.0) then
         print*,'uf*drd error #',ier
         call ufclos(ilun)
         ier=2
         return
      endif
      l=1
      do while (l .le. 20 .and. flab(l:l) .lt. char(48))
         l=l+1
      end do
      if (l.lt.20) label= flab(l:20)
 100  call ufclos(ilun)
      if (ier .ne. 0) return
C
C re-set socket for TRANSP Tree
C
 101  stat = MdsSetSocket(tr_socket)
 
      if (info(1) .eq. 1) then
         call add1duf(lunerr,signam,
     >      x,y,maxx,iproc,
     >      inx, iflag, xlab,ylab,
     >      maxsc,info(8),scval,sclab,ier)
      else if (info(1) .eq. 2) then
         call add2duf(lunerr,signam,
     >      f,max1,x,maxx,y,maxx,iproc,
     >      inx,iny,iflag,flab,xlab,ylab,
     >      maxsc,info(8),scval,sclab,ier)
      else if (info(1) .eq. 3) then
         call add3duf(lunerr,signam,
     >      f,maxd,x,maxx,y,maxx,z,maxz,iproc,
     >      inx,iny,inz,iflag,flab,xlab,ylab,zlab,
     >      maxsc,info(8),scval,sclab,ier)
      else if (info(1) .eq. 0.and.
     >        (zext .eq. 'AEP' .or. zext .eq. 'aep')) then
         call addaepuf(lunerr,signam,isiza,isiza,dat_buf,sclab_buf,ier)
         if(allocated(dat_buf)) deallocate(dat_buf)
      endif
 
C
      if(ier .ne.0) then
         print*,'add*duf error #',ier
         ier = 4
      endif
      return
      end
 
 
 
 
 
