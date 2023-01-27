      program uftype
C
C History:
C 08/11/97 : CAL Add HDF and netCDF options
C
C  driver for a utility to open and read Ufile headers and provide a
C  summary output giving the type of Ufile
C
C  subsequent modifications -- can convert from one Ufile type to
C  another.
C
C----------------
C
      use UFILES
C
      character*256 zfile,ztfile,zfile2,zlfile
      character*32 zdev,zpre,zext,zext2
      character*128 zdir
C
      integer ishot
C
      integer info(8)
C
      logical lxout
C
C----------------
C
C  initialize
C
      call uinita(0)
C
      call ufinit
      call ufncap(2)
C
      lunold=88
      lunnew=89
C
      itemp=0
C
      icmpr=-1                          ! default -- report only
C
      iforce=0                          ! default on write:  only if format
C                                       ! changes
C
      if(lxout(0)) then
         write(6,1001)
 1001    format(/
     >' UFTYPE -- UFILES format analysis / conversion tool.'/
     >'  *to see the format of a UFILE just enter its filename; to get'/
     >'     lowercase letters inclose the name in dbl quotes.'/
     >'  *to see the format of a list of UFILES given in a list file,'/
     >'     enter .LIST followed by the list filename.'/
     >'  *enter .ASCII, .COMPRESS, .NETCDF or .HDF to request'/
     >'     conversion of file formats.'/
     >'  *enter .CONCAT followed by list filename and output filename'/
     >'     to join a sequence of Ufiles, given in the list, into a'/
     >'     single concatenated ascii file.'/
     >'  *enter .UNPACK followed by filename to unpack a file that was'/
     >'     created by a prior .CONCAT command.'/
     >'  *enter "Q" to quit.')
      endif
C
 10   continue
      call ureadw('uftype:  enter filename:$$',zfile)
      ilfile=index(zfile,' ')-1
C
      ilist=0                           ! =1 read filenames from a list file
      iconcat=0                         ! =1 and write to concatenated file
C
      if(zfile(1:10).eq.'.COMPRESS ') then
         icmpr=2                        ! write loss free compressed binary
         go to 10
      else if(zfile(1:5).eq.'.HDF ') then
         icmpr=3                        ! write HDF
         go to 10
      else if(zfile(1:8).eq.'.NETCDF ') then
         icmpr=4                        ! write NetCDF
         go to 10
      else if(zfile(1:8).eq.'.ASCII ') then
         icmpr=0                        ! write ASCII
         go to 10
      endif
C
      if(zfile(1:7).eq.'.FORCE ') then
         iforce=1                       ! force write even if no change
         go to 10
      endif
C
C  capture messages
C
      if(itemp.eq.0) then
         if(zfile(1:5).eq.'.TMP ') then
            call ureadw('uftype:  enter scratch filename:$$',ztfile)
            ilbl=index(ztfile,' ')-1
            open(unit=95,file=ztfile(1:ilbl),status='NEW')
            call ufemsg(-95)
            itemp=1
            go to 10
         else
            open(unit=95,status='SCRATCH')
            call ufemsg(-95)
            itemp=1
         endif
      endif
C
      if(zfile(1:2).eq.'Q ') then
         close(unit=95,status='DELETE')
         call uexita(0)
         stop
      endif
C
      if(zfile(1:6).eq.'.LIST ') then
         call ureadw('uftype:  enter list filename:$$',zlfile)
         ilunlist=96
         call ufopen(ilunlist,zlfile,1,ier,0)
         if(ier.ne.0) then
            call uermsg('?uftype:  list file open error',zlfile,0)
            go to 10
         endif
         ilist=1
      endif
C
      if(zfile(1:8).eq.'.CONCAT ') then
         call ureadw('uftype:  enter concat list filename:$$',zlfile)
         call ureadw('uftype:  enter concat output filename:$$',zfile2)
         call ufopen(lunnew,zfile2,2,ier,0)
         if(ier.ne.0) then
            call uermsg('?uftype:  output file open error',zfile2,0)
            go to 10
         endif
         ilunlist=96
         call ufopen(ilunlist,zlfile,1,ier,0)
         if(ier.ne.0) then
            call uermsg('?uftype:  list file open error',zlfile,0)
            close(unit=lunnew,status='delete')
            go to 10
         endif
         iconcat=1
         ilist=1
         iforce=1
         icmpr=0
      endif
c
      if(zfile(1:8).eq.'.UNPACK ') then
         call ureadw(
     >      'uftype:  enter input file (concatenated Ufiles):$',zfile2)
         call ufopen(lunold,zfile2,1,ier,0)
         if(ier.ne.0) then
            call uermsg('?uftype:  input file open error',zfile2,0)
            go to 10
         endif
         call ufunpk(lunold,lunnew)
         go to 10
      endif
C
 20   continue
      if(ilist.eq.1) then
         read(ilunlist,'(A)',end=90) zfile
         ilfile=index(zfile,' ')-1
      endif
C
      call ufnpars(zfile,zdev,zdir,zpre,ishot,zext,ier)
C
      if(ier.ne.0) then
         if(ilist.eq.0) then
            call uermsi('?uftype:  ufnpars error',ier,0)
         else
            call uermsg('?uftype:  ufnpars error on',zfile,0)
         endif
      else
C
         ilun=97
C
         iok=-1
         call ufsetr(ilun,zpre,zext,zdev,zdir)
         if(ishot.lt.0) call ufnsho(ilun)
         call ufoprd(ilun,max(0,ishot),ier)
         if(ier.ne.0) then
            write(6,'(1x,a,'':  open error'')') zfile(1:ilfile)
         else
            call uflchk(ilun,'uftype    ',ichan,ier)
            call ufhdck(ilun,info,ier)
            if(ier.ne.0) then
               write(6,'(1x,a,'':  read error'')') zfile(1:ilfile)
            else
               call uftype_report(zfile,ilfile,ufcprs(ichan),info,
     >            icmpr,iforce,iok)
            endif
         endif
         call ufclos(ilun)
C
         if(iok .ge. 0) then
            call uftype_convert(lunold,lunnew,info,zfile,zfile2,
     >         zdev,zdir,zpre,ishot,zext,zext2,iok,iconcat)
         endif
C
      endif
C
      if(ilist.eq.1) go to 20
      go to 10
C
 90   continue
      close(unit=ilunlist)
      go to 10
C
      end
C---------------------------------
C
      subroutine uftype_report(zfile,ilfile,kmprs,info,icmpr,iforce,iok)
C
      character*(*) zfile    ! filename
      integer ilfile         ! non-blank length of zfile
      integer kmprs          ! format code (0=ascii,...)
      integer info(8)        ! dimensioning data
C
      integer icmpr          ! flag that file conversion is desired
      integer iforce         ! flag that file compression is forced
      integer iok            ! output flag that conversion is needed
C
      character*256 zreport
C
      integer ilfmtstr(5)
C
      character*16 fmtstr(5)
C
      data fmtstr/'ascii           ',
     >            '16bit_binary    ',
     >            'portable_binary ',
     >            'HDF             ',
     >            'netCDF          '/
C
      data ilfmtstr/5,12,15,3,6/
C
      character*20 zbufc,zbuf2
C
C-----------------
C
      iok=-1
      if((icmpr.eq.2).and.((kmprs.ne.2).or.(iforce.eq.1))) then  ! compress
         iok=2
      else if((icmpr.eq.3).and.((kmprs.ne.3).or.(iforce.eq.1))) then  ! HDF
         iok=3
      else if((icmpr.eq.4).and.((kmprs.ne.4).or.(iforce.eq.1))) then  ! netCDF
         iok=4
      else if((icmpr.eq.0).and.((kmprs.ne.0).or.(iforce.eq.1))) then  ! ascii
         iok=0
      end if
C
      zbufc=' '
      write(zbufc,'(i1,''d'')') info(1)
C
      ikmprs=max(1,min(5,kmprs+1))
C
C  filename:  format  dimensionality
C
      zreport=zfile(1:ilfile)//':  '//
     >    fmtstr(ikmprs)(1:ilfmtstr(ikmprs))//' '//zbufc
C
      ilen=ilfile+9+ilfmtstr(ikmprs)
C
      zbufc=' '
      write(zbufc,'(i6)') info(8)
      do ic=1,6
         if(zbufc(ic:ic).ne.' ') go to 10
      enddo
      ic=6
 10   continue
C
      zbuf2='nsc='//zbufc(ic:6)
C
      ilb2=index(zbuf2,' ')-1
C
C  no. of scalars
C
      ilp1=ilen+1
      zreport(ilp1:)=zbuf2(1:ilb2)
      ilen=ilen+ilb2+2
C
C  dimensioning information
C
      if(info(1).gt.0) then
C
         write(zbufc,'(i8)') info(2)
         do ic=1,8
            if(zbufc(ic:ic).ne.' ') go to 20
         enddo
         ic=8
 20      continue
C
         zbuf2='npt='//zbufc(ic:8)
         ilb2=index(zbuf2,' ')-1
C
C  no. of data pts
C
         ilp1=ilen+1
         zreport(ilp1:)=zbuf2(1:ilb2)
         ilen=ilen+ilb2+2
C
         if(info(1).gt.1) then
            ilast=4+info(1)-2
            write(zbufc,'(i8)') info(3)
            do ic=1,8
               if(zbufc(ic:ic).ne.' ') go to 30
            enddo
            ic=8
 30         continue
C
            zbuf2=zbufc(ic:8)//' '
            ilb2=index(zbuf2,' ')-1
C
            do ii=4,ilast
               write(zbufc,'(i8)') info(ii)
               do ic=1,8
                  if(zbufc(ic:ic).ne.' ') go to 40
               enddo
               ic=8
 40            continue
               zbuf2(ilb2+1:)='x'//zbufc(ic:8)
               ilb2=index(zbuf2,' ')-1
            enddo
C
C  shape of data
C
            ilp1=ilen+1
            zreport(ilp1:)=zbuf2(1:ilb2)
            ilen=ilen+ilb2+1
         endif
C
      endif  ! dim .gt. 0
C
      if(iok .gt. -1) then
         ilp=ilen
         zreport(ilp+1:ilp+10)=' *rewrite*'
         ilen=ilen+10
      endif
C
      write(6,'(1x,a)') zreport(1:ilen)
C
      return
      end
C-----------------------------
      subroutine uftype_convert(lunold,lunnew,info,zfile,zfile2,
     >   zdev,zdir,zpre,ishot,zext,zext2,iopt,iconcat)
c
      integer info(8),ishot,iopt
c
      character*(*) zfile,zfile2
c
      character*(*) zdev,zdir,zpre,zext,zext2
c
c  convert a ufile to :
C            iopt = 2 :  portable compressed binary
C            iopt = 3 :  HDF
C            iopt = 4 :  netCDF
c
c  iconcat = 1:  writing into concatenated file, already open on lunnew
c
c  info  -- ufile dimensioning information
c  zfile -- orig filename
c  zfile2-- backup filename
c
c  zdev,zdir,zpre,ishot,zext -- ufile name components
c    "   "    "     "  ,zext2-- backup ufile name components
c
c  local buffers
c
      parameter(maxsc=1000)
      parameter(maxd=1000000)
c
      real sc(maxsc)
      character*30 sclab(maxsc)
c
      real datbuf(maxd)
      character*30 xlab,ylab,zlab,flab
c
      integer iproc
      character*10 shdate
      character*4 tokdev
c
c  local luns
c
      integer lunold,lunnew
c
c-------------------------------
c  check dimensioning limits
c
      ierd=0
      idim=info(1)
      insc=info(8)
      if(insc.gt.maxsc) then
         write(6,'('' uftype_convert:  scalar list too large'')')
         go to 999
      endif
c
      iu=0
      ierd=0
      if(idim.gt.0) then
         iaf=1
         iu=info(2)
         iax=iu+1
         iu=iu+info(3)
         inx=info(3)
         if(inx.le.0) ierd=1
         if(idim.gt.1) then
            iay=iu+1
            iu=iu+info(4)
            iny=info(4)
            if(iny.le.0) ierd=1
            if(idim.gt.2) then
               iaz=iu+1
               iu=iu+info(5)
               inz=info(5)
               if(inz.le.0) ierd=1
            endif
         endif
      else
         if(insc.le.0) ierd=1
      endif
c
      if(ierd.eq.1) then
         write(6,'('' uftype_convert:  unexpected zero dimension.'')')
         go to 999
      endif
c
      if(iu.gt.(maxd+1)) then
         write(6,'('' uftype_convert:  local data buffer too small'')')
         go to 999
      endif
c
      ilf=index(zfile,' ')
      if(ilf.le.0) ilf=len(zfile)
c
      zfile2=zfile
      zext2=zext
c
      zfile2(ilf:ilf)='~'
      call ufnpars(zfile2,zdev,zdir,zpre,ishot,zext2,ier)
      if(ier.ne.0) then
         write(6,'('' ... uftype_convert:  backup filename error.'')')
         go to 999
      endif
c
      ilf=index(zfile,' ')-1
      if(ilf.le.0) ilf=len(zfile)
c
      ilf2=index(zfile2,' ')-1
      if(ilf2.le.0) ilf2=len(zfile2)
c
c  rename old file (unix only)
c
      call frename(zfile(1:ilf),zfile2(1:ilf2),istat)
c
c  open and read old file
c
      call ufsetr(lunold,zpre,zext2,zdev,zdir)
      if(ishot.lt.0) call ufnsho(lunold)
      call ufoprd(lunold,max(0,ishot),ier)
      if(ier.ne.0) then
         write(6,'(1x,a,'':  read open error'')') zfile2(1:ilf2)
         go to 990
      endif
c
      if(idim.eq.0) then
         call uf0drd(lunold,tokdev,shdate,sc,sclab,max(1,insc),insc,ier)
         if (insc<=0) ier=1
      else if(idim.eq.1) then
         call uf1drd(lunold,tokdev,shdate,
     >      datbuf(iax),datbuf(iaf),inx,inx,iproc,xlab,flab,
     >      max(1,insc),insc,sc,sclab,ier)
      else if(idim.eq.2) then
         iflag=1
         call uf2drd(lunold,tokdev,shdate,
     >      datbuf(iaf),inx,datbuf(iax),inx,datbuf(iay),iny,iproc,
     >      inx,iny,iflag,flab,xlab,ylab,
     >      max(1,insc),insc,sc,sclab,ier)
      else if(idim.eq.3) then
         iflag=1
         call uf3drd(lunold,tokdev,shdate,
     >      datbuf(iaf),inx,iny,
     >      datbuf(iax),inx,datbuf(iay),iny,datbuf(iaz),inz,iproc,
     >      inx,iny,inz,iflag,flab,xlab,ylab,zlab,
     >      max(1,insc),insc,sc,sclab,ier)
      endif
      if(ier.ne.0) then
         write(6,'(1x,a,'':  read error'')') zfile2(1:ilf2)
         go to 990
      endif
c
c  open and write new file
c
      if(iconcat.eq.0) then
         call ufsetr(lunnew,zpre,zext,zdev,zdir)
         if(ishot.lt.0) call ufnsho(lunnew)
         call ufcmpr(lunnew,iopt)       ! loss free compressed portable binary
         call ufopwr(lunnew,max(0,ishot),ier)
      else
         call ufcchd(lunnew,zpre,zext,zdev,zdir,ishot,ier)
      endif
      if(ier.ne.0) then
         write(6,'(1x,a,'':  write open error'')') zfile2(1:ilf2)
         go to 980
      endif
c
      if(idim.eq.0) then
         call uf0dwr(lunnew,tokdev,shdate,sc,sclab,insc,ier)
      else if(idim.eq.1) then
         call uf1dwr(lunnew,tokdev,shdate,
     >      datbuf(iax),datbuf(iaf),inx,iproc,xlab,flab,
     >      insc,sc,sclab,ier)
      else if(idim.eq.2) then
         iflag=1
         call uf2dwr(lunnew,tokdev,shdate,
     >      datbuf(iaf),inx,datbuf(iax),inx,datbuf(iay),iny,iproc,
     >      flab,xlab,ylab,
     >      insc,sc,sclab,ier)
      else if(idim.eq.3) then
         iflag=1
         call uf3dwr(lunnew,tokdev,shdate,
     >      datbuf(iaf),inx,iny,
     >      datbuf(iax),inx,datbuf(iay),iny,datbuf(iaz),inz,iproc,
     >      flab,xlab,ylab,zlab,
     >      insc,sc,sclab,ier)
      endif
      if(ier.ne.0) then
         write(6,'(1x,a,'':  write error'')') zfile2(1:ilf2)
         go to 990
      endif
c
c  spool the comments
c
      call uspool(lunold,lunnew)
c
c  close and exit
c
      if(iconcat.eq.0) call ufclos(lunnew)
      call ufclos(lunold)
c     delete or restore the old file:
      if(iconcat.eq.0) then
         call fdelete(zfile2(1:ilf2),istat)
      else
         call frename(zfile2(1:ilf2),zfile(1:ilf),istat)
      endif
c
      return
c
c  close output file after error
c
 980  continue
      call ufclos(lunnew)
      close(unit=lunnew,err=981)
 981  continue
c
 990  continue
c
c  close input file after error
c  rename back (unix only)
c
      call ufclos(lunold)
      close(unit=lunold,err=991)
 991  continue
c
      call frename(zfile2(1:ilf2),zfile(1:ilf),istat)
c
 999  continue
      write(6,'('' ...uftype_convert:  conversion failed.'')')
c
 1000 continue
      return
      end
c-------------------------------------
      subroutine ufunpk(lunold,lunnew)
c
c  read concatenated ascii ufile, unpack into separate Ufiles.
c  this is done by copying ascii lines, not by Ufiles library software.
c
      character*140 zline               ! input buffer
      character*120 zfile
c
      character*16 zpre,zext,zdev
      character*80 zdir
c
c--------------------------
c
      ilen=len(zline)
      iopen=0
c
 10   continue
      call ufunpk_next(lunold,zpre,zext,zdev,zdir,ishot,ieof)
      if(ieof.eq.1) go to 90
c
c  new filename info found
c
 20   continue
      if(iopen.eq.1) then
         close(unit=lunnew)
         iopen=0
      endif
c
c  form filename
c
      zfile=' '
      ilpre=max(1,(index(zpre,' ')-1))
      ilext=max(1,(index(zext,' ')-1))
      if((ishot.lt.0).or.(ishot.gt.999999)) then
         zfile=zpre(1:ilpre)//'.'//zext(1:ilext)
      else if(ishot.le.99999) then
         write(zfile,'(a,i5.5,''.'',a)') zpre(1:ilpre),
     >      ishot,zext(1:ilext)
      else
         write(zfile,'(a,i6,''.'',a)') zpre(1:ilpre),
     >      ishot,zext(1:ilext)
      endif
c
c  open file
c
      ilfile=index(zfile,' ')-1
      call ufopen(lunnew,zfile,2,ier,0)
      if(ier.ne.0) then
         write(6,'('' uftype:  output open failure:  '',a)')
     >      zfile(1:ilfile)
         go to 10                       ! look for another filename
      else
         iopen=1
         write(6,'('' uftype:  unpacking Ufile:  '',a)') zfile(1:ilfile)
      endif
c
c  open OK, now copy data
c
 30   continue
      read(lunold,'(A)',end=90) zline
c
c  check for new filename, start of new file
c
      if(zline(1:10).eq.'>>ufcchd: ') then
         call ufunpk_pars(zline,zpre,zext,zdev,zdir,ishot,ier)
         if(ier.eq.0) then
            go to 20                    ! new filename
         else
            go to 10                    ! look for new filename
         endif
      endif
c
c  normal line, get length
c
      do ic=ilen,1,-1
         if(zline(ic:ic).ne.' ') go to 40
      enddo
      ic=1
 40   continue
      iline=ic
c
      write(lunnew,'(A)') zline(1:iline)
c
      go to 30
c--------------------------
 90   continue
      if(iopen.eq.1) close(unit=lunnew)
      close(unit=lunold)
      return
      end
c------------------------------------------------------------------
c  look for special line containing filename info
c
      subroutine ufunpk_next(lunold,zpre,zext,zdev,zdir,ishot,ieof)
c
      integer lunold                    ! input lun
      character*(*) zpre,zext,zdev,zdir ! filename parts (output)
      integer ishot                     ! shot number (output)
      integer ieof                      ! eof flag (output, 1=EOF)
c
      character*140 zline
c
c-------------------------------------
c
      ieof=0
 10   continue
      read(lunold,'(A)',end=90) zline
      if(zline(1:10).eq.'>>ufcchd: ') then
         call ufunpk_pars(zline,zpre,zext,zdev,zdir,ishot,ier)
         if(ier.eq.0) then
            go to 20                    ! new filename found
         else
            go to 10                    ! look for new filename
         endif
      else
         go to 10                       ! keep looking for new filename
      endif
c
 20   continue
      return
c
c EOF exit
c
 90   continue
      ieof=1
      return
c
      end
c------------------------------------------------------------------
c  parse line containing filename info
c
      subroutine ufunpk_pars(zline,zpre,zext,zdev,zdir,ishot,ier)
c
      character*(*) zpre,zext,zdev,zdir ! filename parts (output)
      integer ishot                     ! shot number (output)
      integer ier                       ! error flag (output, 0=no error)
c
      character*140 zline               ! input line
      character*1 zcomma,zblank
c
c-------------------------------------
c
      ier=0
c
      zpre=' '
      zext=' '
      zdev=' '
      zdir=' '
      ishot=-99
c
      if(zline(1:10).ne.'>>ufcchd: ') then
         write(6,*) 'ufunpk call error, zline=',zline
         ier=1
         return
      endif
c
      zcomma=','
      zblank=' '
c
      call ufgfld(zline,'pre=',zcomma,i1,i2,ier)
      if(ier.ne.0) return
      zpre=zline(i1:i2)
c
      call ufgfld(zline,'shot=',zcomma,i1,i2,ier)
      if(ier.ne.0) return
      read(zline(i1:i2),'(i6)') ishot
c
      call ufgfld(zline,'ext=',zcomma,i1,i2,ier)
      if(ier.ne.0) return
      zext=zline(i1:i2)
c
      call ufgfld(zline,'dev=',zcomma,i1,i2,ier)
      if(ier.ne.0) return
      zdev=zline(i1:i2)
c
      call ufgfld(zline,'dir=',zblank,i1,i2,ier)
      if(ier.ne.0) return
      zdir=zline(i1:i2)
c
      return
      end
c---------------------------------
      subroutine ufgfld(zline,zkey,zdelim,i1,i2,ier)
c
c  find value matching specified key in input line
c  return limits of value field; value field terminated by
c  passed delimiter character
c
c  input:
c   zline -- input line to be scanned for a field
c   zkey  -- substring identifying start of field
c            should be terminated with = and have no trailing blanks
c   zdelim-- character that terminates field
c
      character*(*) zline,zkey,zdelim
c
c  output:
c   i1 -- character index, start of field
c   i2 -- character index, end of field
c   ier -- completion code, 0 = normal
c
      integer i1,i2,ier
c
c  for example, if on input:
c    zline=' xyz abc fielda=TEST, fieldb=TEST2
c     zkey='fielda='
c   zdelim=','
c
c  then on output we get ier=0, i1=17, and i2=20
c  with zline(i1:i2)='TEST'.
c
c  ier is the completion code, 0 indicates normal.
c
c---------
c
      ier=0
      i1=0
      i2=0
c
      ilkey=len(zkey)
      ikey=index(zline,zkey)
      if(ikey.eq.0) then
         write(6,1001) zkey,zline
 1001    format(' %ufgfld(uftype.for):  key "',a,'" not foud in line:'/
     >      1x,a)
         ier=1
         return
      endif
c
      ilen=len(zline)
c
      i1=min(ilen,(ikey+ilkey))
      do ic=i1+1,ilen
         if(zline(ic:ic).eq.zdelim(1:1)) go to 20
      enddo
      ic=ilen+1
 20   continue
      i2=ic-1
c
      return
      end
 
