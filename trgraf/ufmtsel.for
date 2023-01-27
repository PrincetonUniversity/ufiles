      subroutine ufmtsel(iformat)
C
C  dmc 6 Nov 1997:
C
C  this routine prompts the user to select a Ufile output format.
C  the format code is suitable as use as argument to a UFCMPR call.
C
C  iformat = 0 -- ascii
C          = 2 -- portable loss free compressed binary
C          = 3 -- HDF
C          = 4 -- NetCDF
C
C  if the user quits out, iformat is returned *unchanged*
C  provided it came in with a legal value.
C
      logical lxout,lchar
C
      character*7 zfmt(0:4)
C
      data zfmt/'ascii','??????','binary','HDF','NetCDF'/
C------------------------------------------------
C
C  set default value -- depends on value coming in --
C
      if(iformat .eq. -1) then
         print*,'ufmtsel: can not change MDSplus'
         return
      endif
      if(iformat.lt.0) iformat=0
      if(iformat.gt.4) iformat=4
      if(iformat.eq.1) iformat=2
C
C  user options
C
 10   continue
      if(lxout(0)) then
         write(lnout(0),1001) zfmt(iformat)
 1001    format(/
     >' current Ufile output format:  ',a//
     >' format select options:'/
     >'  A -- Ascii'/
     >'  B -- Binary (loss-free, compressed, portable Ufile format)'/
     >'  C -- NetCDF'/
     >'  D -- HDF'/
     >'  Q -- quit without changing format setting.')
      endif
C
      call uread('ufmtsel:  enter format option A/B/C/D/Q:$')
      if(lchar('Q')) then
         return
      else if(lchar('A')) then
         iformat=0
      else if(lchar('B')) then
         iformat=2
      else if(lchar('C')) then
         iformat=4                      ! yes, 4 for NetCDF
      else if(lchar('D')) then
         iformat=3                      ! yes, 3 for HDF
      else
         call uermsg('unrecognized format code',' ',0)
         go to 10
      endif
C
      if(lxout(0)) write(lnout(0),1002) zfmt(iformat)
 1002 format(' %ufmtsel:  format set to:  ',a)
C
      return
      end
