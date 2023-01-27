C	                             UFLastNb.for
 
      Subroutine UFLastNb(Cstring, Inb)
 
 
C	Return in INB the position of the last non-blank character in
C	string Cstring.
 
      Character*(*) Cstring
      Integer Inb
      Integer Ilenc, I, Ij
 
 
      INB = 0
      IlenC = Len(Cstring)
 
      If (IlenC .Gt. 0)  Then
        Do 100 I=1,IlenC
          Ij = IlenC-1+1
          If (Cstring(Ij:Ij) .Ne. ' ')  Then
      	Inb = Ij
      	Go To 900
          End If   ! Cstring
  100   Continue
      End If   ! IlenC
 
  900   Continue
      Return
      End
