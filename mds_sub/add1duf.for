C  add 1D ufile to MDSplus
C  01/26/01 C.Ludescher
 
      subroutine add1duf(lun,signam,
     >     X,Y,NX,IPROC,
     >     NXACT,IFLAG,
     >     XLAB,YLAB,
     >     NSCD,NSCA,SCVAL,SCLAB,
     >     IER)
 
      implicit none
 
      integer       lun
      character*(*) signam
      integer       NX, IPROC
      integer       NXACT,IFLAG
      REAL          X(NX)        ! independent array
      REAL          Y(NX)        ! dependent array
      REAL          SCVAL(*)     ! SCALAR QUANTITIES
      CHARACTER*30  XLAB, YLAB, SCLAB(*)
      integer       NSCD, NSCA
      integer       ier
C
C For MDSplus
      integer       MdsPut,  idescr_floatarr
      integer       mds_create_node,  idescr_cstring
      integer       dscx, dscy, dscxl, dscyl
      integer       idims(2)
      integer       size
      character*10  ux, uy
      character*21  zunits
      character*90  expr
      integer       stat
 
 
C-----------------------------------------------------
 
C Create signal node in .INPUTS
      stat=mds_create_node(signam,'signal')
      if (mod(stat,2).ne.1) then
C         if (stat .ne. 265388168 .and.   ! TreeALREADY_THERE
C     >       stat .ne. 1073807874) then  ! Generic MDSDCL Error
         if (stat .ne. 265388168) then    ! TreeALREADY_THERE
            call mdsmsg(lun,signam//' node create status:  ',stat)
            ier=1
            go to 9000
         endif	
      endif
C Create Label
      stat=mds_create_node(signam//':LABEL','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then   ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':LABEL create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif	
      expr=signam//':LABEL'//CHAR(0)
      stat=MdsPut(expr,'$'//char(0),
     >     idescr_cstring(ylab(1:20)),0)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,expr//' put status:  ',stat)
      endif
 
C Create Comment
      stat=mds_create_node(signam//':COMMENT','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then   ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':COMMENT create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif	
 
C Create FILENAME
      stat=mds_create_node(signam//':FILENAME','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then   ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':FILENAME create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif	
 
C Write data
      idims(1)=nxact
 
      zunits='build_with_units($,$)'
      expr='build_signal('//zunits//',*,'//zunits//')'
     >          // CHAR(0)
 
      dscy=idescr_floatarr(y,idims,1)
      dscx=idescr_floatarr(x,idims,1)
      uy = ylab(21:30)
      ux = xlab(21:30)
      dscyl=idescr_cstring(uy)
      dscxl=idescr_cstring(ux)
	stat=MdsPut(signam//CHAR(0),expr,
     >       dscy,dscyl,dscx,dscxl,0)
 
C     >	     idescr_floatarr(y,idims,1),idescr_cstring(uy),
C     >       idescr_floatarr(x,idims,1),idescr_cstring(ux),0)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,expr//' put status:  ',stat)
      endif
C
C Add scalars
      call add0duf(lun,signam,nscd,nsca,scval,sclab,ier)
      return
C
 9000  continue
       return
      end
