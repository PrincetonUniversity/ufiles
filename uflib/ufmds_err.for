      subroutine ufmds_err(clbl,istat,ier,zerrmsg,ilm)
C
      use UFILES
C
      integer istat,ier,ilm
      character*(*) clbl,zerrmsg
C
C  get information on an MDS+ error
C
C  input:  clbl -- name of calling routine
C         istat -- MDS+ status code
C
C output:   ier -- error code:  0 if istat is a message, 1 if an error
C       zerrmsg -- the message string returned by the MDS+ server
C           ilm -- non-blank length of zerrmsg
C
C--------------------------------
C
      if((2*(istat/2)).eq.istat) then
         ier=1
         write(ufelun,9001) clbl
 9001    format(' %',a,' MDS+ error:')
      else
         ier=0
         write(ufelun,9002) clbl
 9002    format(' %',a,' MDS+ message:')
      endif
      itmp=mds_errstr(istat,zerrmsg)
      ilm=max(1,len_trim(zerrmsg))
C
      return
      end
 
