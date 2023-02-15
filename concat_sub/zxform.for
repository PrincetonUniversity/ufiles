C******************** START FILE ZXFORM.FOR ; GROUP CONCAT ******************
C==============================
C  ZXFORM
C
C  FORM Amm FILENAME EXTENSION
C
      SUBROUTINE ZXFORM(ZEXT,IIN,istart,idigs)
C
      CHARACTER*(*) ZEXT
C
C  break out `idigs' digits from IIN (with leading zeroes if necessary
C  and encode these into zext(istart:)
C
      integer idigits(idigs)
C
      idigits=0
      iwork=iin
      do i=1,idigs
         I1=iwork/10
         I2=iwork-10*I1
         idigits(idigs+1-i)=i2
         iwork=i1
      enddo
 
      do i=1,idigs
         j=istart+i-1
         write(zext(j:j),'(I1)') idigits(i)
      enddo
 
      RETURN
      END
C******************** END FILE ZXFORM.FOR ; GROUP CONCAT ******************
