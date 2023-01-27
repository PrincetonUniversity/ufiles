      subroutine ufsclab_ck(sclab,nsca)
c
c  check scalar labels after read
c  issue warning and patch if there is an error
c
      use UFILES
c
      integer, intent(in) :: nsca
      character*30, intent(inout) :: sclab(nsca)
c
      character*10 ztest
c
      integer i,j,iok,iblank,ilen
c-------------------
c
      do i=1,nsca
         ztest=sclab(i)(1:10)
         ilen=len_trim(ztest)
         iok=1
         iblank=0
         do j=1,ilen
            if(ichar(ztest(j:j)).lt.32) iok=0
            if(ztest(j:j).eq.' ') iblank=j
         enddo
         if(iok.eq.0) then
            write(ufelun,*) ' %ufsclab_ck: Ufile scalar #',i,
     >         ' label contains unprintable characters.'
         else
            if(ztest(ilen:ilen).ne.':') then
               write(ufelun,*) ' %ufsclab_ck: Ufile scalar #',i,
     >            ' keyword label lacks ":".'
               iok=0
            endif
            if(iblank.gt.0) then
               write(ufelun,*) ' %ufsclab_ck: Ufile scalar #',i,
     >            ' keyword label has imbedded blanks.'
               iok=0
            endif
c
c  patch keyword label...
c
            if(iok.eq.0) then
               write(ufelun,*) ' keyword: "',ztest,
     >            '" label: "',sclab(i)(11:20),'" units: "',
     >            sclab(i)(21:30),'" -- patched.'
c
               if(ztest(ilen:ilen).ne.':') then
                  if(ilen.eq.10) ilen=ilen-1
                  ilen=ilen+1
                  ztest(ilen:ilen)=':'
                  sclab(i)(ilen:ilen)=':'
               endif
c
               if(iblank.gt.0) then
                  do j=1,ilen
                     if(ztest(j:j).eq.' ') sclab(i)(j:j)='_'
                  enddo
               endif
c
            endif                       ! end patch
c
         endif                          ! label characters check
c
      enddo                             ! loop over scalar labels
c
      return
      end
 
