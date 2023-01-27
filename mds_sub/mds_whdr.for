      subroutine mds_whdr(lun,path,signam,comment,ufname,toff,ier)
 
C Write header info to MDSplus
C 02/09/01 CAL
C
      implicit none
C
      integer       lun       ! lun for error messag
      character*(*) path      ! MDS path
      character*(*) signam    ! MDS signal name
      character*(*) comment
      character*(*) ufname    ! Ufile name
      real          toff
      integer       ier
 
C For MDSplus
      integer       MdsPut,   idescr_float
      integer       descr, idescr_cstring, mds__cstring
      character*90  expr
      integer       dscr,      stat
      integer       ilp, l
 
C-------------------------------------------------------------------
 
C Write toff
      if (path .eq. '.INPUTS:') then     ! TRANSP Tree only
C         expr='\TOP:T_OFFSET'//CHAR(0)
         expr='T_OFFSET'//CHAR(0)
         stat=MdsPut(expr,'$'//char(0),
     >        idescr_float(toff),0)
         if (mod(stat,2).ne.1) then
            call mdsmsg(lun,expr//' put status:  ',stat)
            ier=1
         endif
      endif
 
C Write comment
      expr=path//signam//':COMMENT'//CHAR(0)
      dscr = descr(mds__cstring(), comment, 0, len(comment))
      stat=MdsPut(expr,'$'//char(0),
C     >     idescr_cstring(comment),0)
     >     dscr,0)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,expr//' put status:  ',stat)
         if (stat .eq. 265388144) then  !  TreeNNF
            ier=2
         endif
      endif
C Write Ufile Name
      expr=path//signam//':FILENAME'//CHAR(0)
      dscr = descr(mds__cstring(), ufname, 0, len(ufname))
      stat=MdsPut(expr,'$'//char(0),
     >     dscr,0)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,expr//' put status:  ',stat)
         if (stat .eq. 265388144) then  !  TreeNNF
            ier=2
         endif
      endif
 
      return
      end
