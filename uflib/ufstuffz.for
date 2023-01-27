	
      Subroutine UFstuffZ ( F3d, F2d, Imax, Jmax, Kmax, IQ)
 
C	Stuff F3d(1:Imax, 1:Jmax, Iq) into F2d(1:Imax,1:Jmax)
 
      Real F3d(*)
      Real F2d(*)
 
 
      Do 300 J=1,Jmax
        Do 200 I=1,Imax
 
          Ind3 = I + (J-1)*Imax + (Iq-1)*(Imax*Jmax)
          Ind2 = I + (J-1)*Imax
 
          F2d(Ind2) = F3d(Ind3)
 
  200   Continue
  300   Continue
 
      Return
      End
