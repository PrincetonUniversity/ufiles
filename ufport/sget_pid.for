      subroutine sget_pid(ipid)
C
      implicit none
C
      integer ipid                      ! process id number (output)
C
C     return the process id number
C
      integer getpid
C
      ipid = getpid()
C
      return
      end
C------------------------
      subroutine sget_pid_str(pidstr,ilen)
C
      implicit none
C
C  return the process id number as a character string
C
C  output:
      character*(*) pidstr      ! the PID, left justified, blank padded
      integer ilen              ! the non-blank length of pidstr returned.
C
C----------
C  local:
      integer ilenstr,ipid,icp,icz
      character*12 zbuf
C----------
C
      pidstr=' '
      ilenstr=len(pidstr)
C
      call sget_pid(ipid)
C
      write(zbuf,'(I12)') ipid
C
      icp=0
      do icz=1,12
         if(zbuf(icz:icz).ne.' ') then
            icp=icp+1
            if(icp.le.ilenstr) pidstr(icp:icp)=zbuf(icz:icz)
         endif
      enddo
      ilen=icp
C
      return
      end
