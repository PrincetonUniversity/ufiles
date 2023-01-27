      subroutine ufmds_3rd(icode,znode,zf,nf1,nf2,nz,zx,zy,zz,
     >           nxact,nyact,nzact,iflag,ier)
C
C---------------------------------
C History:
C 02/07/01 CAL
C
C  icode = 1: get size only
C        = 2: read 3d array with dimensions
C        = 3: read dimensions only
C  return error code ier, 0 = normal
C
C------------------------------
      use UFILES
      implicit none
 
C Input:
      integer       icode
      character*(*) znode
      integer       nf1,nf2,nz
      real          zf(nf1,nf2,nz)
      real          zx(*), zy(*), zz(*)
      integer       iflag
C Output
      integer       nxact,nyact,nzact
      integer       ier
C
      character*128 zerrmsg
C
      integer       mds_value
      integer       idescr_floatarr, iretl
      integer       istat,  iln, ilm
      integer       idims(3),        ntot
C
C----------------------------------
C
 
      ier=0
C     iln=max(1,len_trim(znode))
      iln=index(znode,' ')-1
      if (iln .le. 0) iln=len(znode)
C
      if (icode .eq. 1) then
C Get size of dimensions
         call ufmds_size(znode(1:iln),1,nxact,ier)
         if(ier.ne.0) go to 9000
         call ufmds_size(znode(1:iln),2,nyact,ier)
         if(ier.ne.0) go to 9000
         call ufmds_size(znode(1:iln),3,nzact,ier)
         if(ier.ne.0) go to 9000
         return
      else
C
C Read 3D array
C
         if (iflag .ne. 1) then
            idims(1)=nxact
            idims(2)=nyact
            idims(3)=nzact
            istat=mds_value(znode(1:iln),
     >           idescr_floatarr(zf,idims,3),iretl)
 
            if(istat.ne.1) then
               call ufmds_err('UFMDS_3RD',istat,ier,zerrmsg,ilm)
               write(ufelun,9510) znode(1:iln),zerrmsg(1:ilm)
               ier=1
               return
 9510          format('  reading MDS+ signal data: ',a/2x,a)
            endif
         endif
C
C Read Dimensions
C
         call ufmds_read('dim_of('//znode(1:iln)//',0)',zx,nxact,
     1	   ier)
         if(ier.ne.0) go to 9000
         call ufmds_read('dim_of('//znode(1:iln)//',1)',zy,nyact,
     1	   ier)
         if(ier.ne.0) go to 9000
         call ufmds_read('dim_of('//znode(1:iln)//',2)',zz,nzact,
     1	   ier)
         if(ier.ne.0) go to 9000
      endif
C
      return
C
 9000 ier=1
      return
      end
