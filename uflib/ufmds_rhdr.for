      subroutine ufmds_rhdr(ichan,idev,zsdate,ishot,idim,znode,iln,
     >           iproc, ier)
C
C  read MDS+ "Ufile" header
C
C---------------------------------
C History:
C 10/22/01 CAL: get iproc
C 11/18/99 CAL: new calling arguments for mds_value
C
C------------------------------
      use UFILES
      implicit none
C  input:
      integer ichan             ! Ufiles channel id
C  output:
      character*(*) idev        ! 4 character tokamak id
      character*(*) zsdate      ! 10 character shot date label
      integer ishot             ! shot number
      integer idim              ! dimensionality of data signal
      character*(*) znode       ! full node path
      integer iln               ! nonblank length of znode
      integer iproc             ! proc code
      integer ier               ! status
C
      character*128 zerrmsg
      character*128 zstr, zpath
C
      integer     idescr_long,  idescr_cstring, mds_value
      integer     iretl, iyy,   ilt,   ios
      integer     nsca,  istat, ilm, ilns
      logical     tr_run
C
C----------------------------------
C
C      write(ufelun,7701)
C 7701 format(' %UFMDS_RHDR MDS+ data access.'/
C     +     '  Labels may be incorrect.')
      NSCA=0
      idev='MDS+'
      zsdate='MDS+:date?'
      ishot=ufshot(ichan)
C
      istat=0
      if (mds_tree(ichan)(1:6).eq.'TRANSP') then
C     >   mds_tree(ichan)(1:3).eq.'JET' .or.
C     >   mds_tree(ichan)(1:5).eq.'BEAST') then
         tr_run = .true.
      else
         tr_run = .false.
      endif
C
C make path:node -> znode
      call ufsnmd_nod0(mds_path(ichan),mds_node(ichan),znode,iln)
C
      ilns=len_trim(mds_server(ichan))
      if(mds_server(ichan)(ilns-6:ilns).eq.'MIT.EDU') then
         idev='CMOD'
         call uf_cmod_sdate(ishot,zsdate)
C
C      else if(mds_tree(ichan).eq.'TFTR') then
C         idev='TFTR'
C      else if(mds_tree(ichan).eq.'NSTX') then
C         idev='NSTX'
      else if(tr_run .or.
     >       mds_tree(ichan)(1:3).eq.'TRW') then
C Get DEVICE
         if (tr_run) then
            zpath = ':DEVICE'
         else
            zpath = znode(1:iln)//':DEVICE'
         endif
         istat=mds_value(zpath,idescr_cstring(zstr),iretl)
         if(mod(istat,2).eq.1) then
            idev=zstr
         else
            call ufmds_err('UFMDS_RHDR',istat,ier,zerrmsg,ilm)
            write(ufelun,7703) zerrmsg(1:ilm)
 7703       format('  seeking MDS+ device: ',/2x,a)
         endif
         if (tr_run) then
C Get date
            istat=mds_value(':date_began',idescr_cstring(zsdate),iretl)
C            istat=mds_value(':shot_idate',idescr_long(iyy),iretl)
C            if(mod(istat,2).eq.1) then
C               write(zsdate,'(a4,''.'',i2.2)',iostat=ios) idev,iyy
C            endif
 
         else
            iln = index(mds_node(ichan),' ')-1
            istat=mds_value(mds_node(ichan)(1:iln)//':DATE',
     >            idescr_cstring(zsdate),iretl)
         endif
         if(mod(istat,2).eq.0) then
            call ufmds_err('UFMDS_RHDR',istat,ier,zerrmsg,ilm)
            write(ufelun,7702) zerrmsg(1:ilm)
 7702       format('  seeking MDS+ date: ',/2x,a)
         endif
 
C Get proc_code
            iproc=0
            istat=mds_value(znode(1:iln)//':PROC_CODE',
     >            idescr_long(iproc),iretl)
C           ignore error for now
      endif
C
C
      call ufmds_rank(znode(1:iln),idim,ier)
C
      return
      end
C------------------------
      subroutine uf_cmod_sdate(ishot,sdate)
C
C  translate CMOD yymmddnnn shot# into 10 char date
C
      integer ishot             ! CMOD 9 digit shot # (input)
      character*10 sdate        ! date string (output)
C
C  date string format:  ddmon-yyyy
C
      character*3 mons(12),month
      data mons/'jan','feb','mar','apr','may','jun','jul','aug',
     >     'sep','oct','nov','dec'/
C
C-------------------
C
      idate=ishot/1000
      iyear=idate/10000
      imond=idate-10000*iyear
C
C  possible note Y2K issue here -- IF CMOD runs past 2080!
C
      if(iyear.le.80) then
         iyear=2000+iyear
      else
         iyear=1900+iyear
      endif
C
      imon=imond/100
      iday=imond-100*imon
C
      month='???'
      if((imon.ge.1).and.(imon.le.12)) then
         month=mons(imon)
      endif
C
      write(sdate,'(i2,a3,''-'',i4)') iday,month,iyear
C
      return
      end
