      subroutine get_arg(i,arg)
C
C  return i'th argument on the command line.
C
      implicit none
      integer i                         ! argument wanted (in)
      character*(*) arg                 ! i'th argument returned (out)
C
      integer j

      j=i

      call getarg(j,arg)
 
      call str_pad(arg)
 
      return
      end
