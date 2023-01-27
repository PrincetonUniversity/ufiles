	
      Subroutine UFstuffX ( F3d, F2d, Imax, Jmax, Kmax, IQ)
 
C	Stuff F3d(IQ, 1:Jmax, 1:Kmax) into F2d(1:Jmax,1:Kmax)
 
      Real F3d(*)
      Real F2d(*)
 
 
      Do 300 K=1,Kmax
        Do 200 J=1,Jmax
 
          Ind3 = IQ + (J-1)*Imax + (K-1)*(Imax*Jmax)
          Ind2 = J + (K-1)*Jmax
 
          F2d(Ind2) = F3d(Ind3)
 
  200   Continue
  300   Continue
 
      Return
      End
