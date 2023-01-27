      subroutine ufmds_size(znode,idimn,nact,ier)
      use UFILES
C
C  get size of dimension
C
      character*(*) znode       ! signal path (input)
      integer idimn             ! dimension # (input)  1,2,... (see note)
C
      integer nact              ! size of dimension (output)
      integer ier               ! return code, 0 = normal (output)
C
C  note:  ufmds_size counts 1 as 1st dim of signal, 2 as 2nd dim, ...
C  but MDS+ counts from 0!
C
C---------------------------------
C History:
C 11/18/99 CAL: new calling arguments for mds_value
C
C------------------------------
C
      character*128 zerrmsg
      character*1 zdimn
      integer     idescr_long, iretl
C
C------------------------------
C
      ier=0
      iln=max(1,len_trim(znode))
      idimc=idimn-1
      if((idimc.lt.0).or.(idimc.gt.9)) then
         ier=1
         write(ufelun,9507) idimn
 9507    format(' ?UFMDS_SIZE:  programming error, idimn=',i9)
         return
      endif
C
      write(zdimn,'(i1)') idimc
C
      istat=mds_value(
     +     'size(dim_of('//znode(1:iln)//','//zdimn//'))',
     +     idescr_long(nact),iretl)
      if(mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_SIZE',istat,ier,zerrmsg,ilm)
         write(ufelun,9508) idimc,znode(1:iln),zerrmsg(1:ilm)
 9508    format('  seeking size of MDS+ dim ',i1,': ',a/2x,a)
      endif
C
      return
      end
 
