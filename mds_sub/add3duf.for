C  add 3D ufile to MDSplus
C  01/26/01 C.Ludescher
 
      subroutine add3duf(lun,signam,
     >     F,NF,X,NX,Y,NY,z,nz,IPROC,
     >     NXACT,NYACT,nzact,IFLAG,
     >     FLAB,XLAB,YLAB,zlab,
     >     NSCD,NSCA,SCVAL,SCLAB,
     >     IER)
 
      implicit none
 
      integer       lun
      character*(*) signam
      integer       NF, NX, NY, nz, IPROC
      integer       NXACT,NYACT,nzact,IFLAG
      REAL          X(NX),F(NF),Y(NY),z(nz),SCVAL(*)
      CHARACTER*30  XLAB, YLAB, zlab, FLAB, SCLAB(*)
      integer       NSCD, NSCA
      integer       ier
C
C For MDSplus
      integer       MdsPut,  idescr_floatarr
      integer       mds_create_node,  idescr_cstring
      integer       idims(3)
      integer       size
      character*10  uf, ux, uy, uz
      character*21  zunits
      character*120 expr
      integer       stat
 
 
 
C-----------------------------------------------------
 
C Create signal node in .INPUTS
      stat=mds_create_node(signam,'signal')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then   ! TreeALREADY_THERE
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
     >     idescr_cstring(flab(1:20)),0)
      if (mod(stat,2).ne.1) then
         call mdsmsg(lun,expr//' put status:  ',stat)
      endif
 
C Create Comment
      stat=mds_create_node(signam//
     >     ':COMMENT','text')
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
      idims(2)=nyact
      idims(3)=nzact
      uf=flab(21:30)
      ux=xlab(21:30)
      uy=ylab(21:30)
      uz=zlab(21:30)
 
      zunits='build_with_units($,$)'
      expr='build_signal('//zunits//',*,'//zunits//','//zunits//
     >          ','//zunits// ')' //CHAR(0)
 
	stat=MdsPut(signam//CHAR(0),expr,
     >       idescr_floatarr(f,idims,3),idescr_cstring(uf),
     >	     idescr_floatarr(x,idims,1),idescr_cstring(ux),
     >       idescr_floatarr(y,idims(2),1),idescr_cstring(uy),
     >       idescr_floatarr(z,idims(3),1),idescr_cstring(uz),0)
 
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
 
 
 
 
 
 
