      subroutine ufopen_mds(ichan,ishot,ier,option)
C
C History:
C 10/17/01 CAL: Extracted from ufoprd
C 11/05/01 CAL: Handle TRANSP/Beast
C
C  open MDS+ tree
C
      use UFILES
      implicit none
C
      integer ichan		! Ufiles channel (set by caller, UFOPRD)
      integer ishot		! Shot number (of tree to open)
      integer ier		! (output) completion code, 0 = normal
      integer option            ! 1 = read
C
      integer       ilnurd      ! ufencod function
C
      character*128 zerrmsg
      character*128 znode
      character*128 zserver
      integer       irank
      integer       istat, ilm, ilt, ils, iln
      integer       try,   l,   m1,  m2,  m3,  notree, fnf
C for mdsplus
      integer       MdsSetSocket, mdsfconnect, MdsValue
      integer       mds_open,      mds_value
      integer       dsc,idescr_long
      integer       mdsstat, size,   pulse
      character*100 expr
      character*10  cshot
C
C      notree = -1882091606
      notree = '8fd18faa'x    ! MDSplus NoTree
      fnf    = 98962          ! MDSplus FileNotFound
C-----------------------------------------------------
C  open MDS+ signal; test accessibility.
C
C  steps:  1.  set server
C          2.  open tree
C          3.  fetch signal -- test by accessing rank of signal
C
C 02/01/01: CAL
C If negative ishot: called from trdat or mds_rdwr: Don't connect / open tree
C it's already open
C If ier = 77: called from trdat: mds_socket is not available
C              trdat = TRDATA  --vs-- UFILES
C
      if (ishot .lt. 0) then   ! Tree already open
         if (ier .ne. 77) then
            istat = MdsSetSocket(mds_socket(ichan))
         endif
         ier=0
         go to 2000
      endif
C
C Check if already connected
      ier=0
      if (mds_socket(ichan) .ne. 0) then
         istat = MdsSetSocket(mds_socket(ichan))
      else
C  step 1:  connect to SERVER
         istat = MdsSetSocket(-1)     ! save current setting
         ils=max(1,len_trim(mds_server(ichan)))
         if (mds_server(ichan)(1:ils) .ne. 'local' .and.
     >        mds_server(ichan)(1:ils) .ne. 'LOCAL') then
            write(ufelun,
     >           '('' %UFOPEN_MDS:  connecting to MDS+ server:  '',a)')
     1           mds_server(ichan)(1:ils)
            zserver=mds_server(ichan)(1:ils)
            zserver(ils+1:ils+1) = char(0)
            istat=mdsfconnect(zserver)
            if(istat.lt.0) then
               write(ufelun,9001) mds_server(ichan)(1:ils)
 9001          format(
     1         ' %UFOPEN_MDS:  mdsfconnect:  failed to connect to ',a)
               ier=1
               go to 1000
            endif
            mds_socket(ichan) = istat
            istat = MdsSetSocket(mds_socket(ichan))  ! set new connection
         else
            mds_socket(ichan) = -1
         endif
      endif
C
C  step 2:  open TREE
C
      try = 0
      ilt=max(1,len_trim(mds_tree(ichan)))
C
C If ufile is from TRANSP run .INPUTS, ishot is not mds pulse number
      if(mds_shot(ichan).ne.0) then
          pulse=mds_shot(ichan)
      else
          pulse=ishot
      endif
C
C If Read: open readonly
C -----------------------
      if (option .eq. 1) then
          if (ufdir(ichan)(1:5) .eq. 'PPPL@') then
C Standard TRANSP tree
C ufdir = 'PPPL@<tok.yr>@<runid>
C 01/25/07 CLF: this is obsolete - convert runid to pulse
              m1=index(ufdir(ichan)(6:),'.')+6
              m2=index(ufdir(ichan)(6:),'@')+6
              m3=index(ufdir(ichan),' ')-1
              call mkbeastid(ufdir(ichan)(m2:m3),pulse,cshot)
              if (pulse .gt. 0) then
                 mds_shot(ichan) = pulse
              else
                 ier=1
                 write(ufelun,'('' ?UFOPEN_MDS: error converting '',
     &                 a)') ufdir(ichan)(m2:m3)
                 return
              endif
           endif
C all other Trees
           write(ufelun,'('' %UFOPEN_MDS:  opening tree '',a,
     >          '' shot '',i10)') mds_tree(ichan),pulse
           istat=mds_open(mds_tree(ichan)(1:ilt),pulse)
           if(mod(istat,2).ne.1) then
              call ufmds_err('UFOPEN_MDS',istat,ier,zerrmsg,ilm)
              write(ufelun,9002) mds_tree(ichan)(1:ilt),
     >             zerrmsg(1:ilm)
 9002         format('  while opening MDS+ tree:  ',a/2x,a)
              ier=1
              go to 1000
           endif
 
C
C If write: open tree for edit
C-----------------------------
      else if (option .eq. 2) then
          write(cshot,'(i10)',iostat=ier) pulse
 2001     expr = 'tcl("EDIT '//mds_tree(ichan)(1:ilt) //
     >          '/SHOT=' //cshot// '")' // CHAR(0)
          l=ilnurd(expr)
          print*, '...',expr(1:l)
          mdsstat=0
          dsc = idescr_long(mdsstat)
          istat = MdsValue(expr, dsc, 0, size)
          if (mod(istat,2).ne.1) go to 9000
          if(mod(mdsstat,2).ne.1) then
              if (try .eq. 0 .and.
     >        (mdsstat .eq. notree .or. mdsstat .eq. fnf)) then
C     if tree does not exist create it:
C
C Set Tree:
                  dsc = idescr_long(mdsstat)
                  expr = 'tcl("SET TREE ' // mds_tree(ichan)(1:ilt) //
     >            '")' // CHAR(0)
                  l=ilnurd(expr)
                  print*, '...',expr(1:l)
                  istat = MdsValue(expr, dsc, 0, size)
                  if(mod(istat,2).ne.1) go to 9000
                  if(mod(mdsstat,2).ne.1) go to 9100
C Create it:
                  expr = 'tcl("CREATE PULSE ' // cshot// '")' // CHAR(0)
                  l=ilnurd(expr)
                  print*, '...',expr(1:l)
                  mdsstat=0
                  dsc = idescr_long(mdsstat)
                  istat = MdsValue(expr, dsc, 0, size)
                  if(mod(istat,2).ne.1) go to 9000
                  if(mod(mdsstat,2).ne.1) go to 9100
                  try = try+1
                  go to 2001
              else
                  go to 9100
              endif
          endif                 ! end create pulse
      endif                     ! end open for write
 
C
 
C
 2000 continue
C
C Read:
C     step 3:  get nodename, test by requesting rank of node
      if (option .eq. 1) then
         call ufsnmd_nod0(mds_path(ichan),mds_node(ichan),znode,iln)
         call ufmds_rank(znode(1:iln),irank,ier)
         if(ier.eq.1) go to 1000
         UFSTAT(ICHAN)=3           ! ready for read
C Write:
      else if (option .eq. 2) then
         UFSTAT(ICHAN)=1           ! ready for write
      endif
C
 1000 continue
      UFSHOT(ICHAN)=abs(ishot)
      return
C Errors:
 
 9000 ier=1
      call mdsmsg(ufelun,'%ufopen_mds: '//expr(1:l)//' istat:  ',istat)
      go to 1000
C
 9100 ier=1
      call ufmds_err('UFOPEN_MDS',mdsstat,ier,zerrmsg,ilm)
      write(ufelun,9003) expr(1:l), zerrmsg(1:ilm)
 9003 format('  while opening MDS+ tree:  ',a/2x,a)
      go to 1000
 
 
      end
 
C******************** END FILE UFOPEN_MDS.FOR ; GROUP UFILES ******************
 
 
 
