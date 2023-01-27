      subroutine ufmds_rank(arg,idim,ier)
C
C  routine to get rank (dimensionality) of a signal
C
C---------------------------------
C History:
C 11/18/99 CAL: new calling arguments for mds_value
C
C------------------------------
C
      use UFILES
C
      character*(*) arg         ! signal path (input)
      integer idim              ! dimensionality (output)
C
      character*128 zerrmsg
C
      integer     idescr_long, iretl
C--------------------------------
C
      ier=0
      ila=max(1,len_trim(arg))
      istat=mds_value('rank('//arg(1:ila)//')',idescr_long(idim),iretl)
      if(mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_RANK',istat,ier,zerrmsg,ilm)
         write(ufelun,9503) arg(1:ila),zerrmsg(1:ilm)
 9503    format('  seeking dimensionality of MDS+ signal: ',a/2x,a)
      endif
C
      return
      end
 
 
 
