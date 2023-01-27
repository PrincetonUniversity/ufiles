      subroutine ufascio2(lun,f,nf1,nx,ny,code,iend,ierr)
C
C  ascii read or write of 2d dimensioned array
C    actually reads/writes by ufascio routine via buffer array
C
C    fortran format loop being avoided because of portability issues
C
C  arguments
C
      integer lun                 ! Fortran i/o channel
      real f(nf1,ny)              ! 2d data array
      integer nx,ny               ! dimensions of data to write in f
      character*1 code            ! 'W' to write, 'R' to read
      integer iend                ! set =1 if EOF encountered on read
      integer ierr                ! set =1 if error encountered on read
C
C---------------------------
C
C  buffer
C
      parameter (inbuf=600)  ! multiple of 6 (cf ufascio)
      common/uscrbufr/ ub(inbuf)
C
C---------------------------
C
      itot=nx*ny
C
      if(code.eq.'W') then
         ibuf=0
         idon=0
      else
C  read first buffer full
         ibuf=min(inbuf,itot)
         call ufascio(lun,ub,ibuf,code,iend,ierr)
         if(iend.gt.0) go to 1999
         if(ierr.gt.0) go to 1999
         idon=ibuf
         icur=1
      endif
C
C  data loop
C
      do iy=1,ny
         do ix=1,nx
            if(code.eq.'W') then
C  store in buffer
               ibuf=ibuf+1
               ub(ibuf)=f(ix,iy)
               if(ibuf.eq.inbuf) then
C  write full buffer
                  call ufascio(lun,ub,ibuf,code,iend,ierr)
                  idon=idon+ibuf
                  ibuf=0
               endif
            else
C  save from buffer
               f(ix,iy)=ub(icur)
               icur=icur+1
               if(icur.gt.ibuf) then
C  read a buffer full
                  ibuf=min(inbuf,(itot-idon))
                  if(ibuf.gt.0) then
                     call ufascio(lun,ub,ibuf,code,iend,ierr)
                     if(iend.gt.0) go to 1999
                     if(ierr.gt.0) go to 1999
                     idon=idon+ibuf
                     icur=1
                  endif
               endif
            endif   ! R/W
         enddo  ! ix
      enddo  ! iy
C
      if(ibuf.gt.0) then
         call ufascio(lun,ub,ibuf,code,iend,ierr)  ! write partial
      endif
C
 1999 continue
      return
      end
