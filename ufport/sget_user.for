      subroutine sget_user(zname)
      character*(*) zname
C
C  get the current username
C
      call ugetlog(zname)
      return
      end
