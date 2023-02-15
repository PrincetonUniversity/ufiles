C  grev2 -- reverse the order of an independent coordinate, and
C   remap the dependent array occordingly -- for numeric f(x,y)
C   representation
C
      subroutine grev2(x,y,f,nx,ny,nf1,iax)
C
      real x(nx)  ! 1st indep coord  -> 1st dim of f
      real y(ny)  ! 2nd indep coord  -> 2nd dim of f
      real f(nf1,ny)  ! data array
C
      integer iax ! =1=> invert x;  =2=> invert y
C
C------------------------
C
      if(iax.eq.1) then
C  reverse the x axis and corresponding f data ordering
         n2=nx/2
         do i=1,n2
            io=nx-i+1
            temp=x(i)
            x(i)=x(io)
            x(io)=temp
            do j=1,ny
               temp=f(i,j)
               f(i,j)=f(io,j)
               f(io,j)=temp
            enddo
         enddo
      else if(iax.eq.2) then
C  reverse the y axis and corresponding f data ordering
         n2=ny/2
         do j=1,n2
            jo=ny-j+1
            temp=y(j)
            y(j)=y(jo)
            y(jo)=temp
            do i=1,nx
               temp=f(i,j)
               f(i,j)=f(i,jo)
               f(i,jo)=temp
            enddo
         enddo
      endif
C
      return
      end
