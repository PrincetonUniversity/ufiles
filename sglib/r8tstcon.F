c---- tstcon.for	sg demo				21-jan-82
c
c  test sglib contour routines
c
	program contour
	integer   ipole
	character back*1
c
	ipole = 0
	call at_bint('Number of poles plots',ipole,back)
	do n=1,ipole
	  call poles
	enddo
c
	end

	subroutine poles
	USE SGLIB_MOD
c
c---- this contour plot was submitted by hedi selberg.
c
	parameter (mx=51)
	parameter (my=101)
	real*8 x(mx),y(my),th(mx),rr(my)
	real*8 x2(mx,my),y2(mx,my),a(mx,my),c(40)
        real*8 xmax
cx	integer kolors(6) /2,3,4,5,6,7/
	integer icall
	real cfirs
	integer nx, ny
	data icall /1/
	data cfirs /-.05/
	data nx /12/, ny /12/
 
	external polpro
c
c undefined elements in these arrays can cause the wrapper 
c routines to fail when they perform a copy to real*4 arrays
c
	x=0.d0 ; y=0.d0 ; th=0.d0 ; rr=0.d0
	x2=0.d0 ; y2=0.d0 ; a=0.d0 ; c=0.d0
c
	call at_int('Rcontr (1), Contur (2)',icall)
	call at_real('First contour level',cfirs)
	call at_int('Number of x points',nx)
	call at_int('Number of y points',ny)
	p2=6.2831852
	dx=1./nx
	do i=1,nx
	  x(i)=p2*i*dx
	enddo
	dy=1./ny
	do j=1,ny
	  y(j)=p2*j*dy
	enddo
	dr=1./(ny)
	dth=p2/(nx)
	do j=1,ny
	  yy=p2*(j)*dy
	  rr(j)=(j)*dr
	  do i=1,nx+1
	    th(i)=(i-1)*dth
	    xx=p2*(i)*dx
	    a(i,j)=sin(yy)*cos(xx)
	    x2(i,j)=rr(j)*cos(th(i))
	    y2(i,j)=rr(j)*sin(th(i))
	  enddo
	enddo
c
	call initt(120)
cx	call binitt
cx	call slimx(150,750)	!default is 150,900
cx	call slimy(100,500)	!default is 125,700
cx	call agrcfr(x,1,nx,2,y,1,ny,2,1)
	c(1)=cfirs	!first solid (fnc min = -2.05)
	c(2)=.1		!increment
	k1=0		!c(1)=f0, c(2)=df, z<c(1) is dotted
	k2=0
	if(icall.eq.1) then
	  call agraph(150,750,100,500,
     >		x(1),x(nx-1),y(1),y(ny-1),'X','Y')
	  call rcontr(k1,c,k2,a,mx,x,1,nx,2,y,1,ny,2)
	elseif(icall.eq.2) then
	  xmax=rr(ny)*.8
	  call agraph(150,750,100,500,
     >		-xmax,xmax,-xmax,xmax,'X','Y')
	  call contur(k1,c,k2,a,mx,x2,1,nx+1,1,y2,1,ny,1)
	elseif(icall.eq.3) then
	  xmax=rr(ny)*.8*.6
	  call agraph(150,750,100,500,
     >		-xmax,xmax,-xmax,xmax,'X','Y')
	  call contur(k1,c,k2,a,mx,x2,1,nx+1,1,y2,1,ny,1)
	endif
	call agxmhs(350,600,'POLES: A CONTOUR PLOT$')
	call upause(0)
	call finitt(0,0)
	return
	end
	subroutine polpro(x,y,px,py)
	px=y*cos(x)
	py=y*sin(x)
	end
