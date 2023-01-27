C---------------------------------------
C  unpack i*4 stream into two i*2 streams
C  preliminary for IEEE FP compression
C
      subroutine unpki2(iray,i2hi,i2lo,ibuf)
C
      integer*2 i2hi(ibuf),i2lo(ibuf)
      integer*2 iray(2,ibuf)
C
      call packordr(iray,ibuf,1)  ! standardize byte order
C
      do i=1,ibuf
         i2hi(i)=iray(1,i)
         i2lo(i)=iray(2,i)
      enddo
      return
      end
C---------------------------------------
