C  Read Scalars of a MDSplus U-file
C
C 02/05/01 CAL
C History:
C 01/18/02 CAL: fix "check" for TRANSP handling
C--------------------------------------------------------------------------
      subroutine ufmds_0rd(ichan,scval,sclab,n_sc,nsca,znode,ier)
C
      use UFILES
      implicit none
 
C Input:
      integer ichan             ! Ufiles channel id
      integer n_sc              ! size of scalar data buffers
      character*30  sclab(n_sc)
      real          scval(n_sc)
      character*(*) znode
 
C Output:
      integer nsca              ! Number of scalars
      integer ier
C
      integer mds_value
      integer idescr_long, idescr_floatarr, idescr_cstringarr
C C Local
      character*30  zsclab2(200)
      integer       nsca2
      character*10  zsclab(3,200)
      integer       idims(2)
      character*128 zerrmsg
      integer       istat, iretl
      integer       j,  i,     ilm
      logical       transp
      character*128 zunode
C-------------------------------------------------------------------------
      ier=0
      transp=.false.
 
      istat=mds_value('size('//znode//':SCVAL)',
     >      idescr_long(nsca),iretl)
      if (mod(istat,2).ne.1) then
         if (istat .ne. 265388144 .and.   ! TREE$-W-NNF
     >       istat .ne. 265388258 .and.   ! TreeNODATA
     >       istat .ne. 0 ) then          ! SS_FACILITY Error
            call ufmds_err('UFMDS_0RD',istat,ier,zerrmsg,ilm)
            write(ufelun,7701) znode,zerrmsg(1:ilm)
 7701       format('  seeking MDS+ size(',a,':SCVAL ',/2x,a)
         endif
         nsca = 0
         return
      endif
      zunode = znode
      call uupper(zunode)
      if (nsca .eq. 0) return
      if (mds_server(ichan) .eq. 'BIRCH.PPPL.GOV' .and.
     >    mds_tree(ichan) .eq. 'TRANSP' ) transp = .true.
C TRANSP stores "methodes" instead of actual data
C mdsvalue' returned "size" is misleading
C      if (index(zunode,'.INPUTS:') .gt. 0) transp=.true.
C      if (transp) then  ! TRANSP is written different
C         if (nsca .eq. 1) then  ! no scalars
C            nsca = 0
C            return
C         endif
C         nsca=nsca/3
C      endif
 
      istat=mds_value(znode//':SCVAL',
     >      idescr_floatarr(scval,nsca,1),iretl)
      if (mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_0RD',istat,ier,zerrmsg,ilm)
         write(ufelun,7702) znode,zerrmsg(1:ilm)
 7702    format('  seeking MDS+ ',a,':SCVAL ',/2x,a)
      endif
       
      if (transp) then  ! TRANSP is written different
         zsclab(1,1) = ' '
         idims(1) = 3
         idims(2) = nsca
         istat=mds_value(znode//':SCLAB',
     >        idescr_cstringarr(zsclab,idims,2),iretl)
      else
         istat=mds_value(znode//':SCLAB',
     >        idescr_cstringarr(sclab,nsca,1),iretl)
      endif
      if (mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_0RD',istat,ier,zerrmsg,ilm)
         write(ufelun,7703) znode,zerrmsg(1:ilm)
 7703    format('  seeking MDS+ ',a,':SCLAB ',/2x,a)
      endif
C check if bad sclab
      if (nsca .gt. 1) then
         j=0
         nsca2 = (nsca+2)/3+1
         do i=nsca2,nsca
            if (sclab(i)(1:10) .eq. ':         ') j=j+1
         end do
         if (j .gt. 0)  write(ufelun,*) 
     >    '%ufmds_0rd: looks like incorrect SCLAB'
         if (j .eq. nsca-nsca2+1) then
            write(ufelun,*) '   ...correcting SCLAB'

            do i=1,nsca2-1
               zsclab2(i) = sclab(i)
            enddo
C            istat=mds_value(znode//':SCLAB',
C     >           idescr_cstringarr(zsclab2,nsca2-1,1),iretl)
C            if (mod(istat,2).ne.1) then
C               call ufmds_err('UFMDS_0RD',istat,ier,zerrmsg,ilm)
C               write(ufelun,7703) znode,zerrmsg(1:ilm)
C               nsca = 0
C               return
C            endif
            j=0
            do i=1,nsca2-1
               j=j+1
               sclab(j)=zsclab2(i)(1:10)
               j=j+1
               sclab(j)=zsclab2(i)(11:20)
               j=j+1
               sclab(j)=zsclab2(i)(21:30)
            enddo         
         endif
      endif
      if (transp) then  ! TRANSP is written different
C                       ! returns size = 1 if it is really 0
         if (nsca .eq. 1 .and. zsclab(1,1) .eq. ' '
     >       .and. scval(1) .eq. 0.0) then
            nsca = 0
            return
         endif
         do i=1,nsca
            sclab(i)(1:10)=zsclab(1,i)
            sclab(i)(11:20)=zsclab(2,i)
            sclab(i)(21:30)=zsclab(3,i)
         enddo
      endif
C
      return
      end
 
 
 
 
