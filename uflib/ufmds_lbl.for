      subroutine ufmds_lbl(ichan,arg,lbl,kdep,default,ier)
C
C  get label information from MDS+ node, in so far as possible
C
C---------------------------------
C History:
C 11/18/99 CAL: new calling arguments for mds_value
C
C------------------------------
      use UFILES
      implicit none
 
      integer ichan             ! Ufiles channel id
      character*(*) arg         ! node pathname or expression (input)
      character*(*) lbl         ! 20/10 char Ufile style label/units (output)
      integer kdep              ! independent var. flag (input)
      character*(*) default     ! default label (input) (use if nothing better)
      integer       ier
C
      character*128 zerrmsg
C
      character*40 zunits
C
      integer      mds_value
      integer      idescr_cstring, iretl, iretl_zunits
      integer      ilen,  ilenp, ilent, ilend, ila,   ilz, istat
      integer      ilm,   ifind, ifinp, ifint, istt, istp, istd
      integer      isum2, isum3
      logical      tr_run
C
C  if kdep=1, label is for function itself;
C  if kdep=0, label os for an independent coordinate.
C
C-----------------------------------
C
      ier=0
      lbl=' '
      ilen=len(lbl)
      if(ilen.lt.30) then
         write(ufelun,9503) ilen
 9503    format(' ?UFMDS_LBL:  programming error, len(lbl).lt.30:',i4)
         ier=1
         return
      endif
C
      ila=max(1,len_trim(arg))
      istat=mds_value('units('//arg(1:ila)//')',
     >      idescr_cstring(zunits),iretl_zunits)
      if(istat.ne.1) then
         call ufmds_err('UFMDS_LBL',istat,ier,zerrmsg,ilm)
         write(ufelun,9504) arg(1:ila),zerrmsg(1:ilm)
 9504    format('  seeking units of MDS+ signal: ',a/2x,a)
         if(ier.eq.1) go to 9000
      endif
C
C  figuring out the labeling well ... is going to be hairy.
C
      if (mds_tree(ichan)(1:6).eq.'TRANSP' .or.
     >   mds_tree(ichan)(1:3).eq.'JET' .or.
     >   mds_tree(ichan)(1:5).eq.'BEAST') then
         tr_run = .true.
      else
         tr_run = .false.
      endif
      if((kdep.eq.1).and.
     >    (tr_run .or. mds_tree(ichan)(1:3) .eq. 'TRW')) then
         istat=mds_value(arg(1:ila)//':LABEL',
     >      idescr_cstring(lbl),iretl)
         lbl(21:) = zunits(1:iretl_zunits)
      else if((kdep.eq.0).and.((zunits.eq.'s').or.(zunits.eq.'S')
     >     .or.(zunits(1:3).eq.'sec').or.(zunits(1:3).eq.'SEC')
     >     .or.(zunits(1:3).eq.'Sec'))) then
         lbl = 'time                seconds   '
      else if((kdep.eq.0).and.(zunits.eq.'r/a')) then
         lbl = 'x                   r/a       '
      else
         ilz=max(1,len_trim(zunits))
         if(ilz.le.10) then
            call ufmds_lword(default,ilend,istd,ifind)
            if(kdep.eq.1) then
               call ufmds_lword(mds_tree(ichan),ilent,istt,ifint)
               call ufmds_lword(mds_path(ichan),ilenp,istp,ifinp)
            else
               ilent=1000
               ilenp=1000
            endif
C
            isum3=ilent+ilenp+ilend
            isum2=ilent+ilend
            if(isum3.le.16) then
               lbl=mds_tree(ichan)(istt:ifint)//'...'//
     >              mds_path(ichan)(istp:ifinp)//':'//
     >              default(istd:ifind)
            else if(isum3.le.18) then
               lbl=mds_tree(ichan)(istt:ifint)//'>'//
     >              mds_path(ichan)(istp:ifinp)//':'//
     >              default(istd:ifind)
            else if(isum2.le.17) then
               lbl=mds_tree(ichan)(istt:ifint)//'...'//
     >              default(istd:ifind)
            else if(isum2.le.19) then
               lbl=mds_tree(ichan)(istt:ifint)//'>'//
     >              default(istd:ifind)
            else
               lbl=default(istd:ifind)
            endif
C
            lbl(21:30)=zunits
C
         else
            lbl=zunits
         endif
      endif
C
 9000 continue
      return
      end
C--------------------------------------
      subroutine ufmds_lword(dstr,ilen,ist,ifin)
C
C  find last "clause" in passed string
C
      character*(*) dstr
C
      ild=max(1,len_trim(dstr))
      ifin=ild
      if(dstr(ifin:ifin).eq.':') ifin=ifin-1
      if(dstr(ifin:ifin).eq.'.') ifin=ifin-1
C
      ilimd=1
      do id=ifin,ilimd,-1
         if(dstr(id:id).eq.'.') go to 10
         if(dstr(id:id).eq.':') go to 10
      enddo
      id=ilimd-1
 10   continue
      ist=id+1
C
      ilen=ifin-ist+1
C
      return
      end
