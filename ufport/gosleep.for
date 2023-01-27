      subroutine gosleep(isecs)
C
      integer isecs                     ! number of seconds to sleep
C
C  this subroutine causes the program to go to sleep for `isecs' seconds!
C
      call ufcsleep( max(0,isecs) )
      return
      end
