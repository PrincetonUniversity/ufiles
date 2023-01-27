      subroutine ufmds_read(arg,array,numa,ier)
C
C  read data at expression (arg) into array (array)
C  return error code ier, 0 = normal
C
C---------------------------------
C History:
C 11/18/1999 CAL: new calling arguments for mds_value
C 04/05/2010 DMC: NaN scan added
C------------------------------
      use UFILES
C
      character*(*) arg
      real array(numa)
      integer ier
C
      character*128 zerrmsg
C
      integer     idescr_floatarr, iretl, icount
C
C----------------------------------
C
      ier=0
      ila=max(1,len_trim(arg))
C
      istat=mds_value(arg(1:ila),idescr_floatarr(array,numa,1),iretl)
      if(mod(istat,2).ne.1) then
         call ufmds_err('UFMDS_READ',istat,ier,zerrmsg,ilm)
         write(ufelun,9510) arg(1:ila),zerrmsg(1:ilm)
 9510    format('  reading MDS+ signal data: ',a/2x,a)
      else

         ! MDS+ can hold NaNs; check...

         icount=0
         call nanscan_r4(ufelun,arg(1:ila),numa,array,icount)
         if(icount.gt.0) ier=100

      endif
C
      return
      end
