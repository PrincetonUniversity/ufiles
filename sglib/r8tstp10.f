c file: tstp10.for	sglib demo plots
c
c  *********************************************
c  demo plots from tektronix ag manual for sglib
c  *********************************************
c
	program tstp10
	character back*1
c
	ip=0
	do while (ip.ge.0 .and. ip.le.7)
	  call at_bint('plot number (zero for all, -1 to quit)',ip,back)
	  if(back.ne.' ') then
	    call at_msg(
     >	    ' 1=ticval,2=enlarg,3=tclip,4=loglin,5=dual,6=bars,7=rc')
	    go to 100		!cycle
	  endif
	  if(ip.eq.0.or.ip.eq.1) call ticval	! tic density test
	  if(ip.eq.0.or.ip.eq.2) call enlarg	! plot with an enlargement
	  if(ip.eq.0.or.ip.eq.3) call tclip	! two curves, one is clipped
	  if(ip.eq.0.or.ip.eq.4) call loglin	! log & linear on same plot
	  if(ip.eq.0.or.ip.eq.5) call dual	! dual scales (uses dinity)
	  if(ip.eq.0.or.ip.eq.6) call bars	! bar charts graph
	  if(ip.eq.0.or.ip.eq.7) call rc	! rc circuit program
 100	enddo
	end
c%
	subroutine ticval
	USE SGLIB_MOD
c
c-- tic neat test
c
	real*8 xdata(4), ydata(7)
	data   xdata/-1.,6.,22.,2.5/
	data   ydata/6.,211.,114.,306.,354.,291.,325./
c
	call initt(240)
	call binitt
	call agtmhs(200,760,'TICVAL:  TICK NEAT ROUTINES$',0)
	call xneat(0)	!to raw data
	call yneat(1)	!to minor tics
	call check(xdata,ydata)
	call dsplay(xdata,ydata)
	call frame
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	subroutine enlarg
	USE SGLIB_MOD
c
c-- make plot with enlargement
c
	real*8 x1, x2, y1, y2
	real*8 xpts2(15),ypts2(15)
	data xpts2/14.,-1.52,-1.21,-1.05,-.81,-.62,-.60,-.57,-.41,-.32,
     1  -.09,.1,.3,.5,.7/
	data ypts2/14.,-.70,-.27,-.12,.17,.28,.31,.32,.20,.18,.10,-.08,
     1  -.35,-.71,-1.22/
c
c--- the main curve
	call initt(240)
	call binitt
	call agtmhs(200,760,'ENLARG: A PLOT AND AN ENLARGEMENT$',0)
	call slimx(100,400)
	call slimy(200,600)
	call ylab(2)		!label ends only
	call check(xpts2,ypts2)
	call dsplay(xpts2,ypts2)
	call agtchs('THE PLOT$',0)
	call agtcvs('THE LABEL')
c--- draw box around part to be enlarged
	x1=-.8
	x2=-.4
	y1=.2
	y2=.4
	call movea(x1,y1)
	call drawa(x2,y1)
	call drawa(x2,y2)
	call drawa(x1,y2)
	call drawa(x1,y1)
	call anmode
c----- the enlargement
	call binitt
	call dlimx(x1,x2)
	call dlimy(y1,y2)
	call xlab(2)	!label ends only
	call slimx(500,950)
	call slimy(200,700)
	call check(xpts2,ypts2)
	call dsplay(xpts2,ypts2)
	call agtchs('THE ENLARGEMENT$',0)
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	subroutine tclip
	USE SGLIB_MOD
c
c-- two curves, one is clipped
c
	real*8 xdata3(4), ydata3(7), ydata4(7)
	data xdata3/-1.,6.,1.,1./
	data ydata3/6.,211.,114.,306.,354.,291.,325./
	data ydata4/6.,367.,458.,168.,238.,620.,210./
c
	call initt(240)
	call binitt
	call agtmhs(200,760,'TCLIP: TWO CURVES, ONE IS CLIPPED$',0)
	call check(xdata3,ydata3)
	call dsplay(xdata3,ydata3)
	call line(34)
	call cplot(xdata3,ydata4)
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	subroutine loglin
	USE SGLIB_MOD
c
c-- make log and linear on same plot
c
	real*8 xpts(11),ypts(11)
c
	xpts(1)=10
	ypts(1)=10
	do i=2,11
	  xpts(i)=float(i-1)
	  ypts(i)=float(i-1)**3
	enddo
c
	call initt(240)
	call chrsiz(3)
	call binitt
	call check(xpts,ypts)
	call dsplay(xpts,ypts)
	call agtchs('AGTCHS',6)
	call agtcvs('AGTCVS',6)
	call symbl(1)
	call dlimy(0.,0.)
	call yfrm(2)
	call ytype(2)
	call ylocrt(0)
	call xlab(0)
	call xfrm(0)
	call check(xpts,ypts)
	call dsplay(xpts,ypts)
	call chrsiz(1)
	call agtmhs(700,10,'AGTMHS(700,10)',0)
	call agtcgt(760,'LOGLIN: LOG AND LINEAR PLOTS.  '//
     >	'AGTCGT',0)
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	subroutine dual
	USE SGLIB_MOD
c
c---	dual scales (uses dinity)
c
	real*8 xdual(7), y1dual(7), y2dual(7)
	data xdual/6.,22.0,24.5,27.0,29.5,32.0,34.5/
	data y1dual/6.,211.,114.,306.,354.,291.,325./
	data y2dual/6.,366.,458.,198.,235.,620.,478./
c
	call initt(240)
	call term(2,4096)
	call binitt
	call agtmhs(200,760,'DUAL: DUAL SCALES  4096 RESOLUTION',0)
	call slimx(200,800)
	call check(xdual,y1dual)
	call dsplay(xdual,y1dual)
c---- the second curve has a different y range. reinitialize.
	call dinitx
	call dinity
	call xloctp(0)
	call xlab(0)	!no numbers on top x ticks
	call ylocrt(0)
	call line(34)
	call check(xdual,y2dual)
	call dsplay(xdual,y2dual)
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	subroutine bars
	USE SGLIB_MOD
c
c-- draw a bar chart
c
	real*8 xdata(4),ydata(13)
        real*8 xmin, xmax
	data xdata /-1.,12.,1.,1./
	data ydata /12.,328.,632.,421.,780.,632.,472.,390.,562.,
     >		680.,852.,729.,645./
c
	call initt(240)
	call binitt
	call vbarst(8,0,0)
	call xfrm(1)
	call xneat(0)	!to raw data
	xmin=1.e10
	xmax=0.
	call mnmx(xdata,xmin,xmax)
	call dlimx(xmin,xmax+.5)
	call xzero(0)	!include zero
	call check(xdata,ydata)
	call dsplay(xdata,ydata)
	call agtmhs(200,740,'BARS: BAR CHART$')
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,0)
	return
	end
c%
	block data comblk
	common/com/em,r,c,omega,xlamda,ec
	data ec/0.0/
	end
 
	subroutine rc
	USE SGLIB_MOD
c
c-- real user program written by dale herron  03-sep-79
c  sample use of deq on 1st order eqn, solves rc series circuit
c   with a diode in the circuit and an input em*sin(wt+lamda).
c
	real*8 tx, t, h, test
	external der300
	external comblk
	real*8 y(2),yp(2),temp(2),w(1000,10),xt(1000),xy(1000),
     >	  xe(1000),xm(1000)
	common/com/em,r,c,omega,xlamda,ec
	data temp,t,y,yp/7*0.0/
c
	em=100.
	r=100.
	c=.0001
	prnt=.01
	h=.001
	tfin=1.0
	freq=60.
	xlamda=0.
	omega = 6.2832*freq
	xlamda = xlamda*(3.14159/180.)
	w(1,1)=0.0
	w(1,2)=0.0
	w(1,3)=0.0
	w(1,4)=0.0
	y(1)=0.0
	yp(1)=0.0
	index=0
	nn=2
	tx=tfin/prnt+1.5
	ntx=int(tx)
	t=0.
	call deqset
150     index=index+1
	test=float(index)*prnt-(0.5*h)
	do while (t.lt.test)
	  call deq1(t,h,2,y,yp,temp,der300)
	enddo
	w(nn,1)=t
	w(nn,2)= ec
	w(nn,3)= em * sin(omega*t + xlamda)
	w(nn,4) = y(1)
	nn=nn+1
	if(nn.le.ntx .and. t.lt.tfin) go to 150
	xt(1) = w(1,1)
	xy(1) = w(1,2)
	xe(1) = w(1,4)
	xm(1) = w(1,3)
	do i=2,ntx
	  xt(i) = w(i,1)
	  xy(i) = w(i,2)
	  xe(i) = w(i,4)
	  xm(i) = w(i,3)
	enddo
	if (ntx .lt. 1000) then
	   print*,'ntx =',ntx
	   do i=ntx+1, 1000
	      xt(i)=0.
	      xy(i)=0.
	      xe(i)=0.
	      xm(i)=0.
	   enddo
	endif
c
c---- this is the plot-10 code.
	call initt(240)
	call term(2,1024)
	call binitt
	call xtics(5)	!ticks at .2 intervals
	call ytics(9)	!guarantee same # ticks on each side
	call ywdth(4)	!limit tic label width
	call agyband(10.)
	call npts(ntx)
	call check(xt,xy)
	call dsplay(xt,xy)
	call agtcvs('CAP VOLTAGE IN VOLTS$')
	call agtchs('TIME IN SECONDS$')
	call dinity
	call ylocrt(0)
	call line(12)
	call xlab(0)
	call xfrm(0)
	call ytics(9)
	call ywdth(4)
	call check(xt,xe)
	call dsplay(xt,xe)
	call agtcgt(0,'R C CIRCUIT RESPONSE$')
	call agtcvs('CURRENT IN AMPS$')
	call upause(0)
c	call bell	! ring the tty bell
c	call tinput(i)
	call finitt(0,800)
	return
	end
c
c---- these subroutines are used by the 'rc' subroutine above.
	subroutine deq1(x,h,n,y,yp,q,der)
	real*8 x, h, y(n), yp(n), q(n)
	real*8 a(4),b(4),c(4)
	data a/.5,.2928932188,1.707106781,.1666666666/
     >	  ,b/1.,.2928932188,1.707106781,.33333333333/
     >	  ,c/.5,.2928932188,1.707106781,.5/
	integer kgo
	real qx
	data kgo /0/
	external der
	save qx

	do j=1,4
	  call der(x,y,yp)
	  if(kgo.eq.0) then
	    qx=0.0
	    do i=1,n
	      q(i)=0.0
	    enddo
	    kgo=1
	    return
	  endif
	  do i=1,n
	    d=h*yp(i)
	    r=d*a(j)-b(j)*q(i)
	    y(i)=y(i)+r
	    q(i)=q(i)+3.0*r-c(j)*d
	  enddo
	  r=h*a(j)-b(j)*qx
	  x=x+r
	  qx=qx+3.0*r-c(j)*h
	enddo
	return
c
	entry deqset
	kgo=0
	return
	end
	subroutine der300(t,y,yp)
	real*8 t, y(1), yp(1)
	common/com/ em,r,c,omega,xlamda,ec
	ec	 = (em*sin(omega*t+xlamda) - y(1))/ r
	if (ec .lt. 0.0) ec= 0.0
	yp(1)  = ec / c
	return
	end


