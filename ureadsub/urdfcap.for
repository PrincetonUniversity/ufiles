C------------------------------------------------------------------------------
c  dmc 14 Apr 1997 -- identify portion of input line that specifies a filename
c     and so should be protected from uppercase conversion
c
      subroutine urdfcap(zbuff,lcap,inbuf)
c
      character*1 zbuff(inbuf)  ! characters (input)
      logical lcap(inbuf)       ! cap flags (output)
c
      character*1 zblank
      character*1 ztab
      character*1 zquot
c
      logical iunix
c--------------------------------
c  goal is to limit the automatic capitalization in ulosup.for
c    ...to protect unix case sensitive filenames
c  input: xyz @foon/zork.ifi @foon/zork2.ifi/input=7
c   want: XYZ @foon/zork.ifi @foon/zork2.ifi/INPUT=7
c
      zblank=' '
      ztab=char(9)
      zquot='"'
c
      do i=1,inbuf
         lcap(i)=.true.
      enddo
c
      iunix=.true.              !unix
c
      iquot=0
      iblpre=1
      istart=1
c
 10   continue
c
      do i=istart,inbuf-1
         if(zbuff(i).eq.zquot) then
            iquot=1-iquot
         else if(iquot.eq.0) then
            if((zbuff(i).eq.'@').or.(zbuff(i).eq.'>')) then
               if((iquot.eq.0).and.(iblpre.eq.1)) then
                  ifiln=i+1
                  go to 20
               endif
            endif
            if((zbuff(i).ne.zblank).and.(zbuff(i).ne.ztab).and.
     >           (zbuff(i).ne.',')) then
               iblpre=0
            else
               iblpre=1
            endif
         endif
      enddo
c
c  no more filenames:  done
c
      return
c
c  filename detected (syntax)
c  if not quoted, protect from case conversion
c  if quoted, only the quoted string following is taken as the filename
c
c    @/u/mccune/foo.ifi/input=7  --> /u/mccune/foo.ifi is filename,
c    @"/u/mccune/foo.ifi"/zork/grom --> same filename, zork & grom are
c       arguments.
c
c    filenames to be protected against case conversion
c
 20   continue
c
      if(zbuff(ifiln).ne.zquot) go to 24
      lcap(ifiln)=.false.
      do ic=ifiln+1,inbuf
         lcap(ic)=.false.
         if(zbuff(ic).eq.zquot) then
            iscan=ic+1
            go to 110
         endif
      enddo
      iscan=inbuf+1
      go to 110
c
 24   continue
      islash=0
      iscan=ifiln-1
 25   continue
      iscan=iscan+1
      if(iscan.gt.inbuf) go to 100   ! terminate with end of buffer
      if(zbuff(iscan).eq.zblank) go to 100   ! or blank
      if(zbuff(iscan).eq.ztab) go to 100     ! or tab
      if(zbuff(iscan).eq.',') go to 100      ! or comma
      if((.not.iunix).and.(zbuff(iscan).eq.'/')) go to 100
      if(iunix.and.(iscan.lt.inbuf)) then
         if((zbuff(iscan).eq.'/').and.(zbuff(iscan+1).eq.'$')) then
            go to 100           ! /$ and unix
         endif
      endif
c
      if(zbuff(iscan).eq.zquot) go to 300    ! skip stuff btw dbl quotes
c
      if(islash.gt.0) then
         if(zbuff(iscan).eq.'=') then
            iscan=islash
            go to 100                    ! /name=val syntax detected
         endif
      endif
c
      if(zbuff(iscan).eq.'/') islash=iscan
      go to 25
c
c  end of filename detected
c
 100  continue
      do i=ifiln,iscan-1
         lcap(i)=.false.    ! protect filename against capitalization
      enddo
c
 110  continue
      istart=iscan          ! look for more filenames
      iblpre=0
      go to 10
c--------------------------------------------------------
c  skip ahead to next quot & resume scan loop
 300  continue
      do i=iscan+1,inbuf
         if(zbuff(i).eq.zquot) go to 310
      enddo
      i=inbuf
 310  continue
      iscan=i
      go to 25
c
      end
