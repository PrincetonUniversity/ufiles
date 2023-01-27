C  Get all dimensions of a MDSplus U-file
C
C 02/05/01 CAL
C--------------------------------------------------------------------------
      subroutine ufmds_dim(ichan,ndim,dims,nsca,znode,ier)
C
      use UFILES
      implicit none
 
C Input:
      integer ichan             ! Ufiles channel id
      integer ndim              ! rank of data signal
      character*(*) znode       ! full node path
C Output:
      integer nsca              ! Number of scalars
      integer dims(ndim)
      integer ier
C C Local
      integer       mds_value
      integer       idescr_long
      character*128 zerrmsg
      integer       istat, iretl
      integer       i,     ilm
C-------------------------------------------------------------------------
 
      do i=1,ndim
         call ufmds_size(znode,i,dims(i),ier)
      enddo
      istat=mds_value('size('//znode//':SCLAB)',
     >      idescr_long(nsca),iretl)
      if (mod(istat,2).ne.1) then
         if (istat .ne. 265388144 .and.   ! TREE$-W-NNF
     >       istat .ne. 265388258 .and.   ! TreeNODATA
     >       istat .ne. 0 ) then          ! SS_FACILITY Error
            call ufmds_err('UFMDS_DIM',istat,ier,zerrmsg,ilm)
            write(ufelun,7703) znode,zerrmsg(1:ilm)
 7703       format('  seeking MDS+ size(',a,':SCVAL ',/2x,a)
         endif
         nsca = 0
C 02/23/2010 CLF: we don't need this anymore since we write actual data
C                 this was to handle incorrect size of TDI function
C      else
C         if (nsca .eq. 1) then  ! no scalars
C            nsca = 0
C         else
C            nsca=nsca/3
C         endif
      endif
 
C
      return
      end
 
 
 
 
