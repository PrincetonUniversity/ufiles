subroutine urunid(itype,file_path,runid,mds_server,tree_name,tokyy,shotno, &
     trread_id,ierr)
!
!  prompt user, get enough information for full identification of a TRANSP
!  run, either filename or MDS+ path
!
  implicit NONE
!
! arguments (all output; input is from terminal or script)
!
!   on exit:  itype=-1  -- user quit without giving runid
!                  =0   -- user gave file path & runid
!                  =1   -- user gave server, tree & shot no.
!                  =2   -- user gave server, tree, tok.yy & runid
!
  integer itype
!
  character*(*) file_path
  character*(*) runid
  character*(*) mds_server
  character*(*) tree_name
  character*(*) tokyy
  integer shotno
!
  character*(*) trread_id  ! composite id for trread kconnect subroutine...
!
  integer ierr  ! completion code:  0 if a runid was acquired.
!
! *** caution *** this routine does not check validity of runid entered!
!
!------------------------------------
!
  logical lchar,lxout
  integer ilunt,lnout
  integer il,ilr,ils,ilt,ila,ily
  integer i,iok,iert,idcod,iservr
!
  character*10 ashot
!
  integer, parameter :: iservrs = 3
  integer, dimension(iservrs) :: itypes
  character*40, dimension(iservrs) :: servrs
  character*2 cab
!
  servrs(1)='TRANSPGRID.PPPL.GOV'
  itypes(1)=2                ! treename="TRANSP", tok.yy & runid
  servrs(2)='ALFA60.PSFC.MIT.EDU'
  itypes(2)=1                ! variable treename, shot no.
  servrs(3)='ATLAS.GAT.COM'
  itypes(3)=2                ! treename="TRANSP", tok.yy & runid
!
  cab='AB'
!
!------------------------------------
!
  ilunt=lnout(0)
!
  ierr=0
  file_path=' '
  runid=' '
  mds_server=' '
  tree_name=' '
  tokyy=' '
  shotno=0
  trread_id=' '
!
  call uread( &
       'urunid:  enter "F" for TRANSP data files, "M" for MDS+, "Q" to quit:$')
  if(lchar('Q')) then
     ierr=1
     itype=-1
  else if(lchar('F')) then
     itype=0
     call unixfile('urunid:  path to directory containing run',file_path)
     il=len_trim(file_path)
     if(file_path(il:il).ne.'/') then
        il=il+1
        file_path(il:il)=':'
     endif
     call ureadw('urunid:  specify TRANSP runid:$',runid)
     ilr=len_trim(runid)
!
     trread_id='FILE:'//file_path(1:il)//runid(1:ilr)
!
  else if(lchar('M')) then
     iok=0
     do while(iok.eq.0)
        if(lxout(0)) then
           write(ilunt,*) &
             ' type A servers:  transp runs identified by tree name & shot no.'
           write(ilunt,*) &
             ' type B servers:  transp runs identified by tok.yy & runid.'
           write(ilunt,*) ' '
           write(ilunt,*) ' known servers:'
           write(ilunt,*) ' #   type   name'
           do i=1,iservrs
              write(ilunt,'(1x,i2,t7,a1,4x,a)') i,cab(itypes(i):itypes(i)), &
                   servrs(i)
           enddo
           write(ilunt,*) ' '
           write(ilunt,*) ' enter server #  --OR--'
           write(ilunt,*) ' enter "S" to specify a different server  --OR--'
           write(ilunt,*) ' enter "Q" to quit.'
           write(ilunt,*) ' '
        endif
        call uread('urunid:  server option:$')
        if(lchar('Q')) then
           ierr=1
           itype=-1
           iok=1
        else if(lchar('S')) then
           call ureadw('urunid:  specify server name:$',mds_server)
           if(lxout(0)) then
              write(ilunt,*) &
             ' type A servers:  transp runs identified by tree name & shot no.'
              write(ilunt,*) &
             ' type B servers:  transp runs identified by tok.yy & runid.'
           endif
           call uread('urunid:  server type code "A" or "B":')
           if(lchar('A')) then
              itype=1
              iok=2
           else if(lchar('B')) then
              itype=2
              iok=2
           else
              call uermsg('?urunid -- unrecognized server type code',' ',0)
              iok=0
           endif
        else
           iservr=idcod(iert)
           if(iert.eq.0) then
              if((iservr.gt.0).and.(iservr.le.iservrs)) then
                 mds_server=servrs(iservr)
                 itype=itypes(iservr)
                 iok=2
              endif
           endif
        endif
!
        if(iok.eq.2) then
           if(itype.eq.1) then
              call ureadw('urunid:  TRANSP tree name:$',tree_name)
              call ureadi('urunid:  shot number:$',shotno)
              ils=len_trim(mds_server)
              ilt=len_trim(tree_name)
              ashot=' '
              write(ashot,'(i10)') shotno
              ila=len_trim(ashot)
              do i=1,ila
                 if(ashot(i:i).ne.' ') exit
              enddo
              trread_id='MDS+:'//mds_server(1:ils)//':'// &
                   tree_name(1:ilt)//'('//ashot(i:ila)//')'
           else
              tree_name='TRANSP'
              call ureadw('urunid:  tok.yy code (e.g. "TFTR.95"):$',tokyy)
              call ureadw('urunid:  runid:$',runid)
              ils=len_trim(mds_server)
              ilt=len_trim(tree_name)
              ily=len_trim(tokyy)
              ilr=len_trim(runid)
              trread_id='MDS+:'//mds_server(1:ils)//':'// &
                   tree_name(1:ilt)//'('//tokyy(1:ily)//','// &
                   runid(1:ilr)//')'
           endif
        endif
!
     enddo
  endif
!
  if(itype.ge.0) write(ilunt,*) &
       ' urunid:  run path:  ',trread_id(1:len_trim(trread_id))
  return
end subroutine urunid
