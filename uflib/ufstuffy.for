	
      Subroutine UFstuffY ( F3d, F2d, Imax, Jmax, Kmax, IQ)
 
C	Stuff F3d( 1:Imax, IQ, 1:Kmax) into F2d(1:Imax,1:Kmax)
 
      Real F3d(*)
      Real F2d(*)
 
 
      Do 300 K=1,Kmax
        Do 200 I=1,Imax
 
          Ind3 = I + (Iq-1)*Imax + (K-1)*(Imax*Jmax)
          Ind2 = I + (K-1)*Imax
 
          F2d(Ind2) = F3d(Ind3)
 
  200   Continue
  300   Continue
 
      Return
      End
