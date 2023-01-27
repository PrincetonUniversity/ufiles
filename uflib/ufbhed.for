      subroutine ufbhed(ilun,ishot,idev,idim,ikmprs,zwdate,zsdate,
     >                  iwrite,ier)
c
c  Ufiles i/o
c  read/write header -- portable binary format
C
C  ILUN - UNIT TO READ/WRITE
C  ISHOT - UP TO 6 DIGIT SHOT NUMBER
C  IDEV - 4 CHAR TOKAMAK CODE
C  IDIM - UFILES FILE DIMENSIONALITY
C  IKMPRS - DATA COMPRESSION CODE
C  ZWDATE - FILE WRITE DATE
C  ZSDATE - SHOT (DATA ORIGIN) DATE
C
C  IWRITE: = 0 ** READ **
C          = 1 ** WRITE **
C
C  OUTPUT IER ERROR CODE 0=NORMAL, 1=ERROR
C   ROUTINE NOW CHECKS FOR ERROR ON READ
C
c
      character*4 idev
      character*10 zwdate,zsdate
c
      ier=0
C
      if(iwrite.eq.1) then
         call ubw3int(ilun,ishot)
         call ubwchar(ilun,idev)
         call ubw3int(ilun,idim)
         ikmprs=2
         call ubw3int(ilun,ikmprs)
         call ubwchar(ilun,zwdate)
         call ubwchar(ilun,zsdate)
      else if(iwrite.eq.0) then
         call ubr3int(ilun,ishot,ier)
         if(ier.ne.0) go to 8999
         call ubrchar(ilun,idev,ier)
         if(ier.ne.0) go to 8999
         call ubr3int(ilun,idim,ier)
         if(ier.ne.0) go to 8999
         call ubr3int(ilun,ikmprs,ier)
         if(ier.ne.0) go to 8999
         call ubrchar(ilun,zwdate,ier)
         if(ier.ne.0) go to 8999
         call ubrchar(ilun,zsdate,ier)
         if(ier.ne.0) go to 8999
      endif
C
      return
C
 8999 continue
      ier=1
      return
C
      end
