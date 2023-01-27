C---------------------------------------
C  repack 2 i*2 streams into an i*4 stream
C  final step in IEEE FP decompression
C
      subroutine repki2(iray,i2hi,i2lo,ibuf)
C
      integer*2 i2hi(ibuf),i2lo(ibuf)
      integer*2 iray(2,ibuf)
C
      do i=1,ibuf
         iray(1,i)=i2hi(i)
         iray(2,i)=i2lo(i)
      enddo
C
      call packordr(iray,ibuf,0)  ! local byte order
C
      return
      end
C---------------------------------------
