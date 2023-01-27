C  add 2D ufile to MDSplus
C  01/26/01 C.Ludescher
 
      subroutine add2duf(lun,signam,
     >     F,NF1,X,NX,Y,NY,IPROC,
     >     NXACT,NYACT,IFLAG,
     >     FLAB,XLAB,YLAB,
     >     NSCD,NSCA,SCVAL,SCLAB,
     >     IER)
 
      implicit none
C      include 'mdslib.inc'
      integer IDTYPE_FLOAT
      parameter (IDTYPE_FLOAT=10)

      integer       lun
      character*(*) signam
      integer       NF1, NX, NY, IPROC
      integer       NXACT,NYACT,IFLAG
      REAL          X(NX),F(NF1,NY),Y(NY),SCVAL(*)
      CHARACTER*30  XLAB, YLAB, FLAB, SCLAB(*)
      integer       NSCD, NSCA
      integer       ier
C
C For MDSplus
      integer       MdsPut,  idescr_floatarr
      integer       mds_create_node,  idescr_cstring
      integer       idims(2)
      integer       size
      character*10  uf, ux, uy
      character*21  zunits
      character*90  expr
      integer       stat
      integer       dsc, dscx, dscy, descr
 
 
C-----------------------------------------------------
 
C Create signal node
      stat=mds_create_node(signam,'signal')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,signam//' node create status:  ',stat)
            ier=1
            go to 9000
         endif	
      endif
C Create Label
      stat=mds_create_node(signam//':LABEL','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
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
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':COMMENT create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif	
 
C Create FILENAME
      stat=mds_create_node(signam//':FILENAME','text')
      if (mod(stat,2).ne.1) then
         if (stat .ne. 265388168) then  ! TreeALREADY_THERE
            call mdsmsg(lun,signam//':FILENAME create status:  ',stat)
            ier=1
            go to 9000
         endif
      endif	
 
C Write data
      idims(1)=nxact
      idims(2)=nyact
      uf=flab(21:30)
      ux=xlab(21:30)
      uy=ylab(21:30)
 
      zunits='build_with_units($,$)'
      expr='build_signal('//zunits//',*,'//zunits//','//zunits//')'
     >          // CHAR(0)
 
      dsc = descr(IDTYPE_FLOAT,f,idims(1),idims(2),0)
      dscx = descr(IDTYPE_FLOAT,x,idims(1),0)
      dscy = descr(IDTYPE_FLOAT,y,idims(2),0)
	stat=MdsPut(signam//CHAR(0),expr,
     >       dsc,idescr_cstring(uf),
     >	     dscx,idescr_cstring(ux),
     >       dscy,idescr_cstring(uy),0)
C	stat=MdsPut(signam//CHAR(0),expr,
C     >       idescr_floatarr(f,idims,2),idescr_cstring(uf),
C     >	     idescr_floatarr(x,idims,1),idescr_cstring(ux),
C     >       idescr_floatarr(y,idims(2),1),idescr_cstring(uy),0)
 
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
 
 
 
 
 
 
