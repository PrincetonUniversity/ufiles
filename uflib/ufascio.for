 
 
      subroutine ufascio(lun,f,n,code,iend,ierr)
 
 
 
C 08/22/94  TBT  Put in internal read to buffer to decide input format.
C                Some lines have a blank in the first position (1st line)
C                others have no blank.
 
C  portable 1PE13.6 decode routine -- 6 fp#s per line
C
C   -- experience has shown that format statement loops that span
C      multiple lines work differently on different machines; to
C      avoid this portability problem UFLIB uses this routine, based
C      on line by line decoding
C
C  arguments:
      integer lun             ! fortran i/o unit number
      real f(n)               ! data to read or write
      character*1 code        ! "W" to write, "R" to read
      integer iend            ! set to 1 if EOF occurs on read
      integer ierr            ! set to 1 if i/o error occurs
C
      Character*80 Zbuffer
C---------------------------------
C
C
      iend=0
      ierr=0
C
      ilines=(n-1)/6 + 1
      do il=1,ilines
         iadr1=(il-1)*6 + 1
         iadr2=min(n,(iadr1+5))
         if(code.eq.'W') then
            write(lun,3009) (f(iadr),iadr=iadr1,iadr2)
         else
            read(lun,3001,end=8000,err=9000) Zbuffer
 
          Ir=0
          Do 50 Ia=1,80
C              Find first period (.)
               If (Zbuffer(Ia:Ia) .eq. '.') Then
                  Ir=Ia
      	  Go to 51
               End If
   50       Continue
   51       Continue
 
            If (Ir .Eq. 4) Then
C              Decimal point in column 4 - leading blank.
               read(Zbuffer,3009,end=8000,err=9000)
     1             (f(iadr),iadr=iadr1,iadr2)
            Else If (Ir .Eq. 3) Then
C              Decimal point in column 3
               read(Zbuffer,3010,end=8000,err=9000)
     1             (f(iadr),iadr=iadr1,iadr2)
            Else
C              .Unrecognized Format
               Go to 9000
            End If
 
         endif
      enddo
C
 3001 Format(A80)
 3009 format(1x,6(1pe13.6))    ! Old First line in data read
 3010 Format(   6(1pe13.6))    ! Subsequent lines in data read
C
      return
C
 8000 continue
      iend=1
      return
C
 9000 continue
      ierr=1
      return
C
      end
