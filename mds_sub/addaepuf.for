      subroutine addaepuf(lun,signam,nscd,nsca,scval,sclab,ier)
 
C Add AEP as Scalars to MDSplus
C
      implicit none
C
      integer       lun          ! lun for error messages
      character*(*) signam
      integer       nscd,nsca    ! size of array / actual
      real          scval(nscd)  ! Scalar values
      character*30  sclab(3)  ! scalar labels
      integer       ier, S_CVAL(1)
      character*30  s_clab(1),XLAB, YLAB
      integer :: IPROC,IFLAG,ilen
      ier=0
      IPROC=0 !raw data
      IFLAG=0
      S_CVAL(1)=0.0
      s_clab(1)=trim(adjustl(sclab(1)))//','//trim(adjustl(sclab(3)))
      xlab=" "
      ylab=" "
      ilen=len(trim(adjustl(sclab(2))))
      XLAB(21:21+ilen-1)= trim(adjustl(sclab(2)))
      ilen=len(trim(adjustl(sclab(3))))
      YLAB(21:21+ilen-1)=trim(adjustl(sclab(3)))
      call add1duf(lun,signam,
     >     SCVAL,SCVAL,nscd,IPROC,
     >     nsca,IFLAG,
     >     XLAB,YLAB,
     >     1,0,S_CVAL,S_CLAB,
     >     IER)
      return
      end
