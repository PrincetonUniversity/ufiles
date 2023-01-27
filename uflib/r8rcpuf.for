      subroutine r8rcpuf(bufreal,bufr8,isize)
C
C  copy a real array to a real*8 array.
C  caution-- these are likely to be at the same address!  so, copy
C  in reverse order to accomodate expansion w/o overwrite.
C
      real bufreal(isize)
      real*8 bufr8(isize)
C
      do i=isize,1,-1
         bufr8(i)=bufreal(i)
      enddo
C
      return
      end
