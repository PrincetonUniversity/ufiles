C  grev1 -- reverse order of 1d fcn arrays (X(i),Y(i),i=1 to N)
C
      subroutine grev1(x,y,n)
C
      real x(n)   ! x axis data
      real y(n)   ! y axis data
C
C--------------------------------------
C
      n2=n/2
C
      do i=1,n2
         io=n-i+1
         temp=x(i)
         x(i)=x(io)
         x(io)=temp
         temp=y(i)
         y(i)=y(io)
         y(io)=temp
      enddo
C
      return
      end
