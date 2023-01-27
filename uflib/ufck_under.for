      subroutine ufck_under(luntty,lbl,data_in,data_out,n)
c
c  uflib utility -- prevent denormal numbers
c  anything with absolute value < 1e-38 set to zero
c  dmc 7 Aug 2003
c
      implicit NONE

      integer, intent(in) :: luntty     ! unit for warning message
      character*(*), intent(in) :: lbl  ! for warning message

      integer, intent(in) :: n          ! array dimension

      real, intent(in) :: data_in(n)    ! raw data
      real, intent(out) :: data_out(n)  ! data with denormals removed
c
c-------------------
c
      integer, parameter :: nwmax=6

      integer iwarn
      common/ufck_under_common/ iwarn

      integer i,jwarn
      real, parameter :: ZERO = 0.0
      real, parameter :: ZMIN = 2.0e-38
c
c-------------------
c
      jwarn=0
      do i=1,n
         if(data_in(i).eq.ZERO) then
            data_out(i)=ZERO
         else if(abs(data_in(i)).ge.ZMIN) then
            data_out(i)=data_in(i)
         else
            !  underflow reset...
            data_out(i)=ZERO
            if(jwarn.eq.0) then
               jwarn=1
               iwarn=iwarn+1
               if(iwarn.le.nwmax) then
                  write(luntty,*) 
     >               ' %ufck_under -- underflow cleared to zero in "',
     >               trim(lbl),'".'
               endif
            endif
         endif
      enddo

      return
      end
c-----------------------------------------------------
      subroutine ufck_reset
c
c  clear COMMON warning counter to zero; should be done before first
c  call to ufck_under
c

      integer iwarn
      common/ufck_under_common/ iwarn

c
c-----------------
c
      iwarn=0

      return
      end
