C******************** START FILE UF2RUT2.FOR ; GROUP UFILES ******************
C---------------------------------------------------------------------
C  UF2RUT2
C  READ 2D DATA IN CONSECUTIVELY
C
 
      SUBROUTINE UF2RUT2(ILUN,F,N,Iformat,IERR)
      DIMENSION F(N)
C
C  Updated:
C  08/24/93 TBT - Created from UF2RUT - added Iformat
C
C	Input:
C         Iformat - 0 => Old format
C                   1 => 8/93 format
 
 
      If (Iformat .Eq. 0) Then
        READ(ILUN,3008,END=9000,ERR=9000) (F(IJ),IJ=1,N)
      Else If (Iformat .Eq. 1) Then
        call ufascio(ilun,f,n,'R',iend,ierr)
        if((iend.eq.1).or.(ierr.eq.1)) go to 9000
      Else
        Go To 9000
      End If   ! Iformat
 
 3008 FORMAT(6(1X,1PE11.4))  ! old format, 5 digit accuracy
 
      IERR=0
      RETURN
C
 9000 CONTINUE
      IERR=1
      RETURN
C
      END
C******************** END FILE UF2RUT2.FOR ; GROUP UFILES ******************
