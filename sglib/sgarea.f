c  sgarea.for   area fill routines derived from ncar libarary
c
c  xx-xxx-89	created module from
c
c----------------------------------------------------------------/aredam
	subroutine aredam (iam,xca,yca,lca,igi,idl,idr)
	implicit none
c
	integer  lca,igi,idl,idr
	integer iam(*)
	real    xca(*),yca(*)
	include  'sgarea_inc'
	integer iare, ixn,iyn,ign,iln,irn
	common /ncocom/ iare
	common /arecom/ ixn,iyn,ign,iln,irn
	logical more
	real    ffl,ffr,ffb,fft,ful,fur,fub,fut
	integer ill,ilx,ily,ipx
	integer lam, ipn, ixl, iyl, igl, ica
c
c the routine aredam allows the caller to add an edge, separating two
c areas from each other, to an existing area map.  the input arguments
c are as follows:
c
c iam is an integer array (dimensioned as specified by a prior call to
c the routine arinam) in which resides the area map to which an edge is
c to be added.  the user should make no direct changes in the area map;
c only routines in the package areas should be allowed to modify it.
c
c xca is an array of x coordinates of edge points.
c
c yca is an array of y coordinates of edge points.
c
c lca is the number of edge points defined by xca and yca.
c
c igi is the identifier of the group to which this edge belongs.
c
c idl is the identifier of the area to the left of the new edge.
c
c idr is the identifier of the area to the right of the new edge.
c
c upon return from aredam, all arguments are unchanged except iam, which
c contains the augmented area map.
c
c pull out the length of the area map and do a simple consistency check.
c
	lam=iam(1)
	if (iam(lam).ne.lam) then
	  call ageset ('aredam: area buffer uninitialized',1,1)
	  return
	endif
c
c pull out the current value of the pointer ipx.
c
	ipx=iam(3)
c
c use getset to set up parameters allowing us to map x and y coordinates
c from the user system to the local integer system.
c
	call getset (ffl,ffr,ffb,fft,ful,fur,fub,fut,ill)
	ilx=(ill-1)/2
	ily=mod(ill-1,2)
c
c add the group identifier to the list at the end of the area map (if
c it's not already there) and set ign to its index.  if the identifier
c is a new one, provide a boundary rectangle.
c
	ign=lam
	more=.true.
	do while (more)
	  ign=ign-1
	  if (ign.ge.iam(6)) then
	    if (mod(iam(ign),2).eq.0.and.iam(ign)/2.eq.igi) more=.false.
	  else
	    if (ign.le.iam(5)) then
	    call ageset ('aredam: area buffer overflow',2,1)
	    return
	    endif
	    iam(6)=ign
	    iam(ign)=igi*2
	    iln=-1
	    irn=0
	    ixn=0
	    iyn=0
	    call arisub1(iam,ipn,ipx)
	    if(iare.ne.0) return
	    iam(ipn+7)=0
	    iam(ipn+8)=0
	    iyn=int(lc)
	    call arisub1(iam,ipn,ipx)
	    if(iare.ne.0) return
	    ixn=int(lc)
	    call arisub1(iam,ipn,ipx)
	    if(iare.ne.0) return
	    iyn=0
	    call arisub1(iam,ipn,ipx)
	    if(iare.ne.0) return
	    ixn=0
	    call arisub1(iam,ipn,ipx)
	    if(iare.ne.0) return
	    iam(7)=iam(7)+1
	    more=.false.
	  endif
	enddo
c
c add the area identifiers to the list at the end of the area map (if
c they're not already there) and set iln and irn to their indices.
c
	if (idl.le.0) then
	  iln=idl
	else
	  iln=lam
	  more=.true.
	  do while (more)
	    iln=iln-1
	    if (iln.ge.iam(6)) then
	      if (mod(iam(iln),2).eq.1.and.iam(iln)/2.eq.idl)more=.false.
	    else
	      if (iln.le.iam(5)) then
	      call ageset ('aredam: area buffer overflow',3,1)
	      return
	      endif
	      iam(6)=iln
	      iam(iln)=idl*2+1
	      more=.false.
	    endif
	  enddo
	endif
c
	if (idr.le.0) then
	  irn=idr
	else
	  irn=lam
	  more=.true.
	  do while(more)
	    irn=irn-1
	    if (irn.ge.iam(6)) then
	      if (mod(iam(irn),2).eq.1.and.iam(irn)/2.eq.idr)more=.false.
	    else
	      if (irn.le.iam(5)) then
	      call ageset ('aredam: area buffer overflow',4,1)
	      return
	      endif
	      iam(6)=irn
	      iam(irn)=idr*2+1
	      more=.false.
	    endif
	  enddo
	endif
c
c proceed with adding the user's edge to the area map.  ixl and iyl are
c the coordinates of the last point inserted in the area map.  igl is
c the group identifier of the last point.
c
	ixl=iam(iam(22)+1)
	iyl=iam(iam(22)+2)
	igl=iam(iam(22)+7)
c
c loop through the points in the user list.
c
	do ica=1,lca
c
c get the x and y coordinates of the next point.
c
	if (ilx.eq.0) then
	  ixn=max(0,min(int(lc),
     +	  int(lc*(ffl+(ffr-ffl)*(xca(ica)-ful)/(fur-ful))+.5)))
	else
	  ixn=max(0,min(int(lc),
     +	  int(lc*(ffl+(ffr-ffl)*(alog(xca(ica))-alog(ful))/
     +	  (alog(fur)-alog(ful)))+.5)))
	endif
	if (ily.eq.0) then
	  iyn=max(0,min(int(lc),
     +	  int(lc*(ffb+(fft-ffb)*(yca(ica)-fub)/(fut-fub))+.5)))
	else
	  iyn=max(0,min(int(lc),
     +	  int(lc*(ffb+(fft-ffb)*(alog(yca(ica))-alog(fub))/
     +	  (alog(fut)-alog(fub)))+.5)))
	endif
c
c if the point to be inserted is distinct from the old point, add it.
c if it's the first point, zero its group and area identifiers.  update
c the descriptors of the last point.
c
	if (ixn.ne.ixl.or.iyn.ne.iyl.or.ign.ne.igl) then
	  call arisub1(iam,ipn,ipx)
	  if(iare.ne.0) return
	  if (ica.eq.1) then
	    iam(ipn+7)=0
	    iam(ipn+8)=0
	    iam(ipn+9)=0
	  endif
	  ixl=ixn
	  iyl=iyn
	  igl=ign
	endif
c
	enddo
c
c restore the value of the pointer ipx to its position in the area map.
c
	iam(3)=ipx
c
c set the map state to say that an edge has been entered.
c
	iam(4)=0
c
	return
	end
c
	subroutine arisub1(iam,ipn,ipx)
c
c this internal procedure adds a new point to the area map.
c
	implicit none
	integer ipn,ipx
	integer iam(*)
	include  'sgarea_inc'
	integer iyn, ixn, iare, irn, iln, ign
	common /ncocom/ iare
	common /arecom/ ixn,iyn,ign,iln,irn
	logical more,morf
c
	ipn=iam(5)+1
	if (iam(5)+10.ge.iam(6)) then
	call ageset ('aredam: area buffer overflow',5,1)
	return
	endif
	iam(5)=iam(5)+10
	iam(ipn)=iam(iam(22))+4
	iam(ipn+1)=ixn
	iam(ipn+2)=iyn
	iam(ipn+3)=18
	iam(ipn+4)=iam(22)
	iam(iam(22)+3)=ipn
	iam(22)=ipn
	more=.true.
	do while (more)
	  if (iam(ipn+1).lt.iam(ipx+1)) then
	    ipx=iam(ipx+6)
	  elseif (iam(ipn+1).gt.iam(iam(ipx+5)+1)) then
	    ipx=iam(ipx+5)
	  else
	    morf=.true.
	    do while (morf)
	    if (iam(ipn+1).eq.iam(ipx+1).and.
     &                            iam(ipn+2).lt.iam(ipx+2)) then
	      ipx=iam(ipx+6)
	    elseif (iam(ipn+1).eq.iam(iam(ipx+5)+1).and.
     +	  	  iam(ipn+2).gt.iam(iam(ipx+5)+2)) then
	      ipx=iam(ipx+5)
	    else
	      morf=.false.
	    endif
	    enddo
	    more=.false.
	  endif
	enddo
	iam(ipn+5)=iam(ipx+5)
	iam(ipn+6)=iam(iam(ipx+5)+6)
	iam(iam(ipx+5)+6)=ipn
	iam(ipx+5)=ipn
	iam(ipn+7)=ign
	iam(ipn+8)=iln
	iam(ipn+9)=irn
	return
c
	end
c
c
c----------------------------------------------------------------/arinam
	subroutine arinam (iam,lam)
c
	implicit none
	integer lam
	integer iam(lam)
	include  'sgarea_inc'
	integer iare
	common /ncocom/ iare

c
c the routine arinam is called to initialize a given area map.  it must
c be called prior to any aredam call for that area map.  input arguments
c are as follows:
c
c iam is the integer array in which an area map is to be initialized.
c
c lam is the length of the array iam.
c
c on output, elements of iam have been changed.
c
c
c  clear the error code.
c log an error if the user's array is too small.
c
	iare=0
	if (lam.le.27) then
	  call ageset ('arinam: area buffer is too small',1,1)
	  return
	endif
c
c proceed with initialization.  store the length of the array as its
c first element and as its last; this allows for later error checking.
c
	iam(1)=lam
	iam(lam)=lam
c
c zero the maximum-distance parameter.
c
	iam(2)=0
c
c initialize the value of ipx, which preserves from call to call the
c approximate position of the last node with which we did anything.
c
	iam(3)=8
c
c initialize the map state.
c
	iam(4)=0
c
c set the pointers indicating the locations of the last cells used at
c the beginning and end of the area buffer.
c
	iam(5)=27
	iam(6)=lam
c
c zero the number of groups.
c
	iam(7)=0
c
c set up two dummy nodes to serve as "anchors", preventing searches
c from going past them.
c
	iam(8)=0
	iam(9)=-1
	iam(10)=-1
	iam(11)=18
	iam(12)=0
	iam(13)=18
	iam(14)=0
	iam(15)=0
	iam(16)=0
	iam(17)=0
c
	iam(18)=0
	iam(19)=lc+1.
	iam(20)=lc+1.
	iam(21)=0
	iam(22)=8
	iam(23)=0
	iam(24)=8
	iam(25)=0
	iam(26)=0
	iam(27)=0
c
c done.
c
	return
	end
c
c
c----------------------------------------------------------------/arpram
	subroutine arpram (iam,if1,if2,if3)
c
	implicit none
	integer iam(*)
	integer if1,if2,if3
	include  'sgarea_inc'

	integer iare
	integer kf1,isx,isy,iqb,iqe
	common /ncocom/ iare
	common /arpcom/ kf1,isx,isy,iqb,iqe
	logical lloop,ledge
	integer kf3, ipx, nxl, ipt, ixl, ipi, ndo, iyl
	real    fxd, fyd
	integer i, ix0, iy0, ipn, isu, lam
	integer isp, iqt, ix1, iy1, ix2, iy2, iql, iqu
	real    dsq, xci, yci, yco, ytm
	integer isl, isr, isd, ip1, ip2, isq, ilw, igi
	integer ipu, ipq, ipr, ips, ipm, ipv, iph, ipp
	integer iof, iax, iai, iap, iaq
c
c examine the area map.  find points of intersection, delete redundant
c edge segments, and adjust the area identifiers.
c
c iam is the array which holds an area map previously initialized by a
c call to arinam and augmented by calls to aredam.
c
c if1 specifies what kind of search is made for intersections.  if if1
c is zero, all pairs of edge segments which could possibly intersect
c are examined for actual intersection, a very time-consuming process.
c if if1 is one, a pair is examined only if one of its members has a
c left or a right identifier less than or equal to zero; this saves a
c lot of time and is intended for use with contour lines.
c
c if2 specifies what kind of action is taken to remove unclosed edges.
c if if2 is zero, a search is made for such edges and they are simply
c removed from the area map.  if if2 is one, no such search is made;
c all edges are assumed closed.
c
c if3 specifies what kind of search is made for area identifiers to be
c changed.  if if3 is zero, all edges of all subareas are examined in
c complete detail, a very time-consuming process.  if if3 is one, only
c those edges having a zero or negative area identifier are examined
c (all others being assumed correct) and holes are ignored, which saves
c a lot of time; this is intended for use with contour lines.
c
c
c
c declare certain variables to be real or double precision.
c
	real ang,ant,apq,aqr,fxl,fx1,fx2,fy1,fy2,tmp,fsx,fsy
c
c pull out the length of the area map and do a simple consistency check.
c
	lam=iam(1)
	if (iam(lam).ne.lam) then
	  call ageset ('arpram: area buffer uninitialized',1,1)
	  return
	endif
c
c copy the fast-path flags to internal variables.
c
	kf1=if1
cx	kf2=if2
	kf3=if3
c
c initialize the pointer used in determining the coordinate ordering of
c new nodes.
c
	ipx=8
c
c first, find the average length of the projection on the x axis of the
c line segments in the area map.
c
	nxl=0
	fxl=zero
	ipt=8
	do while(iam(ipt+3).ne.18)
	  ipt=iam(ipt+3)
	  if (iam(ipt+7).ne.0) then
	    nxl=nxl+1
	    fxl=fxl+real(abs(iam(ipt+1)-iam(iam(ipt+4)+1)))
	  endif
	enddo
	if (nxl.eq.0) then
	  call ageset ('arpram: no edges in area map',2,1)
	  return
	endif
	ixl=int(fxl/real(nxl))
c
c decide what the maximum such length should be and save it.  adjust
c the x and y coordinates in the dummy nodes at the ends of the list.
c
	iam(2)=max(2,min(int(lc), 2*ixl))
	iam(9)=-(iam(2)+1)
	iam(10)=-(iam(2)+1)
	iam(19)=int(lc)+(iam(2)+1)
	iam(20)=int(lc)+(iam(2)+1)
c
c now, break up any edge segments whose projections on the x axis are
c greater than the maximum.
c
	ipi=8
	do while(iam(ipi+3).ne.18)
	  ipi=iam(ipi+3)
	  if (iam(ipi+7).ne.0) then
	    ndo=1+(abs(iam(ipi+1)-iam(iam(ipi+4)+1))-1)/iam(2)
	    if (ndo.gt.1) then
	      ixl=iam(iam(ipi+4)+1)
	      iyl=iam(iam(ipi+4)+2)
	      fxd=real(iam(ipi+1)-ixl)/real(ndo)
	      fyd=real(iam(ipi+2)-iyl)/real(ndo)
	      do i=1,ndo-1
		ix0=ixl+int(real(i)*fxd+.5)
		iy0=iyl+int(real(i)*fyd+.5)
		call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
		if(iare.ne.0) return
	      enddo
	    endif
	  endif
	enddo
c
c
c now, look for points of intersection between edges.  the algorithm
c used should run in o(nlogn) time, rather than o(n**2) time.  a
c vertical sweep line is passed over the plane from left to right.
c a list is kept of all the edge segments intersected by the sweep
c line, sorted in order of increasing y coordinate.  whenever a new
c edge segment is added to the list, a check is made for intersection
c of it with the edge segments on either side of it.  whenever an edge
c segment is removed from the list, a check is made for intersection
c of the edge segments which were on either side of it.  there are
c various complications, mostly having to do with the fact that points
c of intersection are incorporated in the area map as they are found
c and that we are working with a discrete grid, rather than with the
c classic euclidean plane.
c
c the list is kept at the upper end of the area map.  each five-word
c node in it represents one edge segment intersecting the sweep line
c and has the following format:
c
c   word 0:  a pointer to the next node in the list (0 if no more).
c   word 1:  a pointer to the last node in the list (0 if no more).
c   word 2:  a pointer to the left end of the edge segment.
c   word 3:  a pointer to the right end of the edge segment.
c   word 4:  a pointer to the drawing end of the edge segment.
c
  101 continue
c
c save the index of the last element used at the upper end of the area
c map.
c
	isu=iam(6)
c
c iqb points to the beginning of the edge-segment list and iqe to a
c linked list of nodes removed from the edge-segment list and available
c for re-use.  both start out empty.
c
	iqb=0
	iqe=0
c
c isp points to the node in the area map defining the current position
c of the sweep line.
c
	isp=8
c
c a while loop moves the sweep line, one point position at a time,
c across the area map.
c
	do while (iam(isp+5).ne.18)	!sweep
c
	isp=iam(isp+5)
c
c pull out the x and y coordinates of the point defining the current
c position of the sweep line.
c
	isx=iam(isp+1)
	fsx=real(isx)
	isy=iam(isp+2)
	fsy=real(isy)
c
c interpolate the new point along any line segment which passes within
c one unit of it and, at the same time, remove any line segment having
c the current point, or any with the same coordinates, as a right
c endpoint.
c
	iqt=iqb
	do while (iqt.ne.0)
c
	  ix1=iam(iam(iqt+2)+1)
	  fx1=real(ix1)
	  iy1=iam(iam(iqt+2)+2)
	  fy1=real(iy1)
	  ix2=iam(iam(iqt+3)+1)
	  fx2=real(ix2)
	  iy2=iam(iam(iqt+3)+2)
	  fy2=real(iy2)
c
	  tmp=max(zero,min(one,
     +	    ((fsx-fx1)*(fx2-fx1)+(fsy-fy1)*(fy2-fy1))/
     +	    ((fx2-fx1)*(fx2-fx1)+(fy2-fy1)*(fy2-fy1))))
	  dsq=(fx1+(fx2-fx1)*tmp-fsx)**2+(fy1+(fy2-fy1)*tmp-fsy)**2
	  if ((dsq.lt.1.).and.(ix1.ne.isx.or.iy1.ne.isy).and.
     +	    (ix2.ne.isx.or.iy2.ne.isy)) then
	    ipi=iam(iqt+4)
	    ix0=isx
	    iy0=isy
	    call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	    if(iare.ne.0) return
	    iam(iqt+3)=ipn
	    if (iam(iqt+4).ne.iam(iqt+2)) iam(iqt+4)=ipn
	  endif
c
	  if (iam(iam(iqt+3)+1).eq.isx.and.iam(iam(iqt+3)+2).eq.isy) then
	  if (iam(iqt+1).eq.0) then
	    iqb=iam(iqt)
	    if (iqb.ne.0) iam(iqb+1)=0
	    iam(iqt)=iqe
	    iqe=iqt
	    iqt=iqb
	  elseif (iam(iqt).eq.0) then
	    iam(iam(iqt+1))=0
	    iam(iqt)=iqe
	    iqe=iqt
	    iqt=0
	  else
	    iql=iam(iqt+1)
	    iqu=iam(iqt)
	    iam(iql)=iqu
	    iam(iqu+1)=iql
	    iam(iqt)=iqe
	    iqe=iqt
	    if (iam(iam(iqu+3)+1).ne.isx.or.iam(iam(iqu+3)+2).ne.isy)
     +           then
	    call arpsub2(iam,iql,iqu,ipi,ipn,ipx)
	    if(iare.ne.0) return
	    endif
	    iqt=iqu
	  endif
	else
	  iqt=iam(iqt)
	endif
c
	enddo
c
c add line segments having the current point as a left endpoint.
c
	if (iam(isp+7).ne.0.and.(iam(isp+1).lt.iam(iam(isp+4)+1).or.
     +	    (iam(isp+1).eq.iam(iam(isp+4)+1).and.
     +	    iam(isp+2).lt.iam(iam(isp+4)+2)))) then
	  isl=isp
	  isr=iam(isp+4)
	  isd=isp
	  call arpsub3(iam,ipi,ipn,ipx,isl,isr,isd)
	  if(iare.ne.0) return
	endif
c
	if (iam(iam(isp+3)+7).ne.0.and.(iam(isp+1).lt.iam(iam(isp+3)+1)
     +	    .or.(iam(isp+1).eq.iam(iam(isp+3)+1)
     +	    .and.iam(isp+2).lt.iam(iam(isp+3)+2)))) then
	  isl=isp
	  isr=iam(isp+3)
	  isd=isr
	  call arpsub3(iam,ipi,ipn,ipx,isl,isr,isd)
	  if(iare.ne.0) return
	endif
c
	enddo	!sweep
c
c restore the index of the last element used at the upper end of the
c area map.
c
	iam(6)=isu
c
c now look for coincident line segments in the list and remove them.
c
	isp=8
	do while (iam(isp+5).ne.18)
	isp=iam(isp+5)
	if (iam(isp+7).ne.0) then
	if (iam(iam(isp+3)+7).ne.0.and.
     +     iam(iam(isp+3)+1).eq.iam(iam(isp+4)+1)
     +	  .and.iam(iam(isp+3)+2).eq.iam(iam(isp+4)+2)) then
	  ip1=isp
	  ip2=iam(isp+3)
	  call arpsub4(iam,ip1,ip2)
	endif
	isq=iam(isp+5)
	do while (iam(isq+1).eq.iam(isp+1).and.iam(isq+2).eq.iam(isp+2)
     +	    .and.iam(isp+7).ne.0)
	if (iam(isq+7).ne.0.and.iam(iam(isq+4)+1).eq.iam(iam(isp+4)+1)
     +	    .and.iam(iam(isq+4)+2).eq.iam(iam(isp+4)+2)) then
	  ip1=isp
	  ip2=isq
	  call arpsub4(iam,ip1,ip2)
	endif
	if (iam(iam(isq+3)+7).ne.0.and.
     +       iam(iam(isq+3)+1).eq.iam(iam(isp+4)+1)
     +	    .and.iam(iam(isq+3)+2).eq.iam(iam(isp+4)+2)) then
	  ip1=isp
	  ip2=iam(isq+3)
	  call arpsub4(iam,ip1,ip2)
	endif
	isq=iam(isq+5)
	enddo
	endif
	if (iam(iam(isp+3)+7).ne.0) then
	isq=iam(isp+5)
	do while (iam(isq+1).eq.iam(isp+1).
     +      and.iam(isq+2).eq.iam(isp+2).and.
     +	    iam(iam(isp+3)+7).ne.0)
	if (iam(isq+7).ne.0.and.
     +    iam(iam(isq+4)+1).eq.iam(iam(isp+3)+1).and.
     +	  iam(iam(isq+4)+2).eq.iam(iam(isp+3)+2)) then
	  ip1=iam(isp+3)
	  ip2=isq
	  call arpsub4(iam,ip1,ip2)
	endif
	if (iam(iam(isq+3)+7).ne.0.and.
     +    iam(iam(isq+3)+1).eq.iam(iam(isp+3)+1)
     +	  .and.iam(iam(isq+3)+2).eq.iam(iam(isp+3)+2)) then
	  ip1=iam(isp+3)
	  ip2=iam(isq+3)
	  call arpsub4(iam,ip1,ip2)
	endif
	isq=iam(isq+5)
	enddo
	endif
	enddo
cf2c
cf2c look for unclosed edges, if that is to be done.
cf2c
cf2	if (kf2eq.0) then
cf210043 continue
cf2	npf=0
cf2	ipt=8
cf210044 continue
cf2	if (.not.(iam(ipt+3).ne.18)) go to 10045
cf2	ipt=iam(ipt+3)
cf2	if (iam(ipt+7).ne.0.and.iam(iam(ipt+4)+7).eq.0) then
cf2	igi=abs(iam(ipt+7))
cf2	ip1=iam(ipt+4)
cf2	ip2=ip1
cf210046 continue
cf2	if (.not.(iam(iam(ip2+6)+1).eq.iam(ip1+1).and.iam(iam(ip2+6)+2).eq
cf2     +.iam(ip1+2))) go to 10047
cf2	ip2=iam(ip2+6)
cf2	go to 10046
cf210047 continue
cf2	ipf=0
cf210048 continue
cf2	if (ip2.ne.ip1.and.(abs(iam(ip2+7)).eq.igi.or.abs(iam(iam(ip2+3)+7
cf2     +)).eq.igi)) then
cf2	ipf=1
cf2	go to 10049
cf2	endif
cf2	ip2=iam(ip2+5)
cf2	if (iam(ip2+1).ne.iam(ip1+1).or.iam(ip2+2).ne.iam(ip1+2))
cf2     +go to 10049
cf2	go to 10048
cf210049 continue
cf2	if (ipf.eq.0) then
cf2	iam(ipt+7)=0
cf2	npf=npf+1
cf2	endif
cf2	endif
cf2	go to 10044
cf210045 continue
cf2	ipt=18
cf210050 continue
cf2	if (.not.(iam(ipt+4).ne.8)) go to 10051
cf2	ipt=iam(ipt+4)
cf2	if (iam(ipt+7).ne.0.and.iam(iam(ipt+3)+7).eq.0) then
cf2	igi=abs(iam(ipt+7))
cf2	ip1=ipt
cf2	ip2=ip1
cf210052 continue
cf2	if (.not.(iam(iam(ip2+6)+1).eq.iam(ip1+1).and.iam(iam(ip2+6)+2).eq
cf2     +.iam(ip1+2))) go to 10053
cf2	ip2=iam(ip2+6)
cf2	go to 10052
cf210053 continue
cf2	ipf=0
cf210054 continue
cf2	if (ip2.ne.ip1.and.(abs(iam(ip2+7)).eq.igi.or.abs(iam(iam(ip2+3)+7
cf2     +)).eq.igi)) then
cf2	ipf=1
cf2	go to 10055
cf2	endif
cf2	ip2=iam(ip2+5)
cf2	if (iam(ip2+1).ne.iam(ip1+1).or.iam(ip2+2).ne.iam(ip1+2))
cf2     +go to 10055
cf2	go to 10054
cf210055 continue
cf2	if (ipf.eq.0) then
cf2	iam(ipt+7)=0
cf2	npf=npf+1
cf2	endif
cf2	endif
cf2	go to 10050
cf210051 continue
cf2	if (.not.(npf.eq.0)) go to 10043
cf2	endif
c
c adjust the area identifiers for all edge segments in the map.  we
c first make a pass over the entire area map, looking for holes and
c eliminating them by the insertion of some temporary connecting lines.
c
c save the pointer to the last word of the last node, so that we can
c remove the nodes implementing the temporary connecting lines before
c returning to the caller.
c
	ilw=iam(5)
c
c each pass through the following loop traces the boundary of one
c connected loop.  in some cases (for contour maps, for example),
c this step can be omitted; in those cases, there is a small chance
c of failure, in which case we have to come back and force the step
c to occur.
c
  104 if (kf3.eq.0) then
c
	ipt=8
c
	lloop=.true.
	do while (lloop)
c
c move to the right across the area map, looking for an edge segment
c that has not yet been completely processed.  if no such segment can
c be found, all subareas have been done.
c
	do while (mod(iam(ipt),4).eq.3.or.abs(iam(ipt+7)).lt.iam(6))
	  ipt=iam(ipt+5)
	  if (ipt.eq.18) go to 10059	!exit lloop
	enddo
c
c pull out the group identifier for the segment.
c
	igi=abs(iam(ipt+7))
c
c decide whether to scan the subarea to the left of the edge being
c traced (ipu=1) or the one to the right (ipu=2).
c
	if (mod(iam(ipt),2).eq.0) then
	  ipu=1
	else
	  ipu=2
	endif
c
c ipq points to the node defining the beginning of the edge segment,
c ipr to the node defining the end of the edge segment, and ips to
c the node defining the beginning of the edge, so that we can tell
c when we've gone all the way around it.
c
	ipq=iam(ipt+4)
	ipr=ipt
	ips=ipq
	ipm=ipr
	ipv=ipu
c
c aqr holds the angle which the current edge segment makes with the
c positive x axis.
c
	aqr=atan2(real(iam(ipr+2)-iam(ipq+2)),
     +    real(iam(ipr+1)-iam(ipq+1)))
c
c we need to keep track of the highest point found along the loop and
c the total change in direction.  initialize the variables involved.
c
	iph=ipq
	ant=zero
c
c each pass through the following loop moves one step along the edge of
c the subarea.
c
	ledge=.true.
	do while (ledge)
c
c update the pointer to the highest point found along the loop.
c
	if (iam(ipr+2).gt.iam(iph+2)) iph=ipr
c
c move ipq to ipp, ipr to ipq, and aqr to apq.  adjust the range of apq.
c
	ipp=ipq
	ipq=ipr
	apq=aqr
	if (apq.lt.-pi) apq=apq+tp
	if (apq.gt.+pi) apq=apq-tp
c
c back up ipr to the beginning of the group of nodes which have the
c same x and y coordinates as it does.
c
	do while (iam(ipr+1).eq.iam(iam(ipr+6)+1).and.
     +	    iam(ipr+2).eq.iam(iam(ipr+6)+2))
	  ipr=iam(ipr+6)
	enddo
c
c go through the group of nodes, examining all the possible ways to
c move from the current position to a new one.  pick the direction
c which is leftmost (if ipu=1) or rightmost (if ipu=2).
c
	ip1=ipr
	ip2=ipr
	ipr=0
	if (ipu.eq.1) then
	  aqr=apq-pi
	else
	  aqr=apq+pi
	endif
c
	do while (iam(ip2+1).eq.iam(ip1+1).and.iam(ip2+2).eq.iam(ip1+2))
	if (abs(iam(iam(ip2+3)+7)).eq.igi.
     +    and.(iam(iam(ip2+3)+1).ne.iam(ipp+1)
     +	  .or.iam(iam(ip2+3)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+3)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+3)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=1
	  endif
	else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=2
	  endif
	endif
	endif
	if (abs(iam(ip2+7)).eq.igi.and.(iam(iam(ip2+4)+1).ne.iam(ipp+1)
     +	  .or.iam(iam(ip2+4)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+4)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+4)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=2
	  endif
	else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=1
	  endif
	endif
	endif
	ip2=iam(ip2+5)
	enddo
c
c if no possible exit was found, reverse direction.
c
	  if (ipr.eq.0) then
	  ipr=ipp
	  ipv=3-ipv
	  if (ipu.eq.1) then
	    aqr=apq-pi
	  else
	    aqr=apq+pi
	  endif
	endif
c
c update the total angular change.
c
	ant=ant+aqr-apq
c
c set the marker for the edge segment picked.  if the marker is set
c already, either go back and do a slow-path intersection search or
c log an error.
c
	if (ipv.eq.1.and.mod(iam(ipm),2).eq.0) then
	iam(ipm)=iam(ipm)+1
	elseif (ipv.eq.2.and.mod(iam(ipm)/2,2).eq.0) then
	iam(ipm)=iam(ipm)+2
	else
	ipt=iam(5)-9
	do while (ipt.gt.ilw)
	  iam(iam(ipt+4)+3)=iam(ipt+3)
	  iam(iam(ipt+3)+4)=iam(ipt+4)
	  iam(iam(ipt+6)+5)=iam(ipt+5)
	  iam(iam(ipt+5)+6)=iam(ipt+6)
	  ipt=ipt-10
	enddo
	iam(5)=ilw
	do ipt=8,iam(5)-9,10
	  iam(ipt)=4*(iam(ipt)/4)
	enddo
	if (kf1.ne.0) then
	  kf1=0
	  go to 101
	else
	  call ageset ('arpram: algorithm failure',4,1)
	  return
	endif
	endif
c
c exit if we're passing the start of the subarea.
c
	if (iam(ipq+1).eq.iam(ips+1).and.iam(ipq+2).eq.iam(ips+2)
     +	    .and.iam(ipr+1).eq.iam(ipt+1).and.iam(ipr+2).eq.iam(ipt+2))
     +	    ledge=.false.
c
	enddo	!ledge
c
c if the closed loop just traced was a hole, insert a temporary
c connecting line to get rid of the hole.
c
	if ((ipu.eq.1.and.ant.lt.zero).or.
     +     (ipu.eq.2.and.ant.gt.zero)) then
	iof=0
	xci=real(iam(iph+1))
	yci=real(iam(iph+2))
	yco=lc+1.
	ip1=iph
	do while (iam(iam(ip1+5)+1).eq.iam(iph+1))
	  ip1=iam(ip1+5)
	enddo
	do while (iam(ip1+1).ge.iam(iph+1)-iam(2))
	  if (abs(iam(ip1+7)).eq.igi.and.iam(iam(ip1+4)+1).gt.iam(ip1+1)
     +	    .and.iam(iam(ip1+4)+1).ge.iam(iph+1)) then
	  ytm=real(iam(ip1+2))+(xci-real(iam(ip1+1)))*
     +	    (real(iam(iam(ip1+4)+2)-iam(ip1+2))/
     +	    real(iam(iam(ip1+4)+1)-iam(ip1+1)))
	  if (ytm.gt.yci.and.ytm.lt.yco) then
	    iof=ip1
	    yco=ytm
	  endif
	  endif
	  if (abs(iam(iam(ip1+3)+7)).eq.igi.and.
     + iam(iam(ip1+3)+1).gt.iam(ip1+1)
     +	    .and.iam(iam(ip1+3)+1).ge.iam(iph+1)) then
	  ytm=real(iam(ip1+2))+(xci-real(iam(ip1+1)))*
     +	    (real(iam(iam(ip1+3)+2)-iam(ip1+2))/
     +	    real(iam(iam(ip1+3)+1)-iam(ip1+1)))
	  if (ytm.gt.yci.and.ytm.lt.yco) then
	    iof=iam(ip1+3)
	    yco=ytm
	  endif
	  endif
	  ip1=iam(ip1+6)
	enddo
	if (iof.ne.0) then
	  ix0=iam(iph+1)
	  iy0=iam(iph+2)
	  if (int(yco).ne.iy0) then
	  ipi=18
	  call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  iam(ipn+7)=0
	  iam(ipn+8)=0
	  iam(ipn+9)=0
	  iy0=int(yco)
	  call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  iam(ipn+7)=lam-igi
	  iam(ipn+8)=0
	  iam(ipn+9)=0
	endif
	if ((ix0.ne.iam(iof+1).or.iy0.ne.iam(iof+2)).and.
     +	    (ix0.ne.iam(iam(iof+4)+1).or.iy0.ne.iam(iam(iof+4)+2))) then
	  ipi=iof
	  call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	endif
	endif
c
	endif
c
	enddo	!lloop
10059 continue
c
c zero the lower bits in the markers in all the nodes.
c
	do ipt=8,iam(5)-9,10
	  iam(ipt)=4*(iam(ipt)/4)
	enddo
c
	endif
c
c now, make another pass through the area map, tracing one subarea at a
c time and setting the area identifiers in each.
c
c each pass through the following loop traces the boundary of one
c subarea.
c
c
	ipt=8
c
	lloop=.true.
	do while (lloop)
c
c move to the right across the area map, looking for an edge segment
c that has not yet been completely processed.  if no such segment can
c be found, all subareas have been done.
c
	do while (mod(iam(ipt),4).eq.3.or.abs(iam(ipt+7)).lt.iam(6).or.
     +	  (kf3.ne.0.and.((mod(iam(ipt),2).ne.0.or.iam(ipt+8).gt.0).and.
     +	  (mod(iam(ipt)/2,2).ne.0.or.iam(ipt+9).gt.0))))
	  ipt=iam(ipt+5)
	  if (ipt.eq.18) go to 10080	!exit lloop
	enddo
c
c pull out the group identifier for the segment.
c
	igi=abs(iam(ipt+7))
c
c decide whether to scan the subarea to the left of the edge being
c traced (ipu=1) or the one to the right (ipu=2) and initialize the
c area identifier.
c
	if (mod(iam(ipt),2).eq.0.and.(kf3.eq.0.or.iam(ipt+8).le.0)) then
	  ipu=1
	else
	  ipu=2
	endif
c
	iax=0
	iai=0
	iap=0
c
c ipq points to the node defining the beginning of the edge segment,
c ipr to the node defining the end of the edge segment, and ips to
c the node defining the beginning of the edge, so that we can tell
c when we've gone all the way around it.
c
	ipq=iam(ipt+4)
	ipr=ipt
	ips=ipq
	ipm=ipr
	ipv=ipu
c
c aqr holds the angle which the current edge segment makes with the
c positive x axis.  for the sake of efficiency, during this pass, its
c value is only computed when absolutely necessary.
c
	aqr=tenten
c
c each pass through the following loop moves one step along the edge of
c the subarea.
c
	ledge=.true.
	do while (ledge)
c
c move ipq to ipp, ipr to ipq, and aqr to apq.
c
	ipp=ipq
	ipq=ipr
	apq=aqr
c
c back up ipr to the beginning of the group of nodes which have the
c same x and y coordinates as it does.
c
	do while (iam(ipr+1).eq.iam(iam(ipr+6)+1).and.
     +	  iam(ipr+2).eq.iam(iam(ipr+6)+2))
	  ipr=iam(ipr+6)
	enddo
c
c if there is only one node in the group, the exit path is obvious.
c
	if (iam(ipr+1).ne.iam(iam(ipr+5)+1).or.
     +	    iam(ipr+2).ne.iam(iam(ipr+5)+2)) then
	if (iam(iam(ipr+3)+1).ne.iam(ipp+1).or.
     +	    iam(iam(ipr+3)+2).ne.iam(ipp+2)) then
	if (iam(iam(ipr+3)+7).eq.lam-igi.or.
     +	    abs(iam(iam(ipr+3)+7)).eq.igi) then
	  ipm=iam(ipr+3)
	  ipr=ipm
	  ipv=ipu
	  aqr=tenten
	else
	  ipr=0
	endif
	else
	if (iam(ipr+7).eq.lam-igi.or.abs(iam(ipr+7)).eq.igi) then
	  ipm=ipr
	  ipr=iam(ipr+4)
	  ipv=3-ipu
	  aqr=tenten
	else
	  ipr=0
	endif
	endif
c
c otherwise, go through the group of nodes, examining all the possible
c ways to move from the current position to a new one.  pick the
c direction which is leftmost (if ipu=1) or rightmost (if ipu=2).
c
	else
c
	ip1=ipr
	ip2=ipr
	ipr=0
	if (apq.eq.tenten) then
	  apq=atan2(real(iam(ipq+2)-iam(ipp+2)),
     +	    real(iam(ipq+1)-iam(ipp+1)))
	endif
	if (apq.lt.-pi) apq=apq+tp
	if (apq.gt.+pi) apq=apq-tp
	if (ipu.eq.1) then
	  aqr=apq-pi
	else
	  aqr=apq+pi
	endif
c
	do while (iam(ip2+1).eq.iam(ip1+1).and.iam(ip2+2).eq.iam(ip1+2))
	if ((iam(iam(ip2+3)+7).eq.lam-igi.or.
     +      abs(iam(iam(ip2+3)+7)).eq.igi)
     +	    .and.(iam(iam(ip2+3)+1).ne.iam(ipp+1).or.
     +	    iam(iam(ip2+3)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+3)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+3)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=1
	  endif
	else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=2
	  endif
	endif
	endif
	if ((iam(ip2+7).eq.lam-igi.or.abs(iam(ip2+7)).eq.igi).and.
     +	    (iam(iam(ip2+4)+1).ne.iam(ipp+1).or.
     +	    iam(iam(ip2+4)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+4)+2)-iam(ip2+2)),
     + real(iam(iam(ip2+4)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	if (ang.gt.aqr.and.ang.lt.apq+pi) then
	ipr=iam(ip2+4)
	aqr=ang
	ipm=ip2
	ipv=2
	endif
	else
	if (ang.lt.aqr.and.ang.gt.apq-pi) then
	ipr=iam(ip2+4)
	aqr=ang
	ipm=ip2
	ipv=1
	endif
	endif
	endif
	ip2=iam(ip2+5)
	enddo
c
	endif
c
c if no possible exit was found, reverse direction.
c
	if (ipr.eq.0) then
	  ipr=ipp
	  ipv=3-ipv
	  aqr=tenten
	endif
c
c update the markers for the edge segment picked.
c
	if (ipv.eq.1.and.mod(iam(ipm),2).eq.0) then
	  iam(ipm)=iam(ipm)+1
	  iaq=ipm+8
	elseif (ipv.eq.2.and.mod(iam(ipm)/2,2).eq.0) then
	  iam(ipm)=iam(ipm)+2
	  iaq=ipm+9
	else
	  call ageset ('arpram: algorithm failure',5,1)
	  return
	endif
c
	if (iam(iaq).le.0.or.iam(iaq).ge.iam(6)) then
	if (iam(iaq).lt.0) then
	  iai=-1
	  iax=lam*4
	elseif (iam(iaq).ge.iam(6).and.iam(ipm).gt.iax) then
	  iai=iam(iaq)
	  iax=iam(ipm)
	endif
	iam(iaq)=iap
	iap=iaq
	endif
c
c exit if we're passing the start of the subarea.
c
	if (iam(ipq+1).eq.iam(ips+1).and.iam(ipq+2).eq.iam(ips+2).and.
     +	    iam(ipr+1).eq.iam(ipt+1).and.iam(ipr+2).eq.iam(ipt+2))
     +	    ledge=.false.
c
	enddo	!ledge
c
c go through the chain of area identifiers, updating them.
c
	do while (iap.ne.0)
	  iaq=iam(iap)
	  iam(iap)=iai
	  iap=iaq
	enddo
c
c if a zero identifier was selected for the area, see if the search for
c holes was suppressed and, if so, re-do it.
c
	if (iai.eq.0.and.kf3.ne.0) then
	  do ipt=8,iam(5)-9,10
	    iam(ipt)=4*(iam(ipt)/4)
	  enddo
	  kf3=0
	  go to 104
	endif
c
	enddo	!lloop
10080 continue
c
c delete the nodes used to put in the temporary connecting lines.
c
	ipt=iam(5)-9
	do while (ipt.gt.ilw)
	  iam(iam(ipt+4)+3)=iam(ipt+3)
	  iam(iam(ipt+3)+4)=iam(ipt+4)
	  iam(iam(ipt+6)+5)=iam(ipt+5)
	  iam(iam(ipt+5)+6)=iam(ipt+6)
	  ipt=ipt-10
	enddo
	iam(5)=ilw
c
c zero the markers in all the remaining nodes.
c
	do ipt=8,iam(5)-9,10
	  iam(ipt)=0
	enddo
c
c update the map state.
c
	iam(4)=1
c
c done.
c
	return
	end
c
	subroutine arpsub4(iam,ip1,ip2)
	implicit none
	integer iam(*)
	integer ip1,ip2
	integer ip3
c
c this internal procedure processes coincident pairs of segments found.
c if both members of the pair belong to the same group, the earlier one
c is deleted.  if they belong to different groups, they are marked in
c such a way as to avoid algorithmic problems.
c
	if (iam(ip1).lt.iam(ip2)) then
	  ip3=ip1
	else
	  ip3=ip2
	endif
	if (abs(iam(ip1+7)).eq.abs(iam(ip2+7))) then
	  iam(ip1+8)=0
	  iam(ip1+9)=0
	  iam(ip2+8)=0
	  iam(ip2+9)=0
	  iam(ip3+7)=0
	else
	  iam(ip3+7)=-abs(iam(ip3+7))
	endif
	return
	end
c
	subroutine arpsub3(iam,ipi,ipn,ipx,isl,isr,isd)
	implicit none
	integer iam(*)
	integer ipi,ipn,ipx,isl,isr,isd
	include  'sgarea_inc'
	integer iare
	common /ncocom/ iare
	integer kf1,isx,isy,iqb,iqe
	common /arpcom/ kf1,isx,isy,iqb,iqe
	integer iqs, iqt, ilx, iqi, iql, iqu
	real    rly, sll, slp, rty
c
c this internal procedure adds a segment to the list.  it also
c checks for intersection of that segment with the segments on either
c side of it in the list.
c
	iqs=0
	iqt=iqb
	ilx=iam(isl+1)
	rly=real(iam(isl+2))
c
	if (iam(isl+1).ne.iam(isr+1)) then
	  sll=real(iam(isr+2)-iam(isl+2))/real(iam(isr+1)-iam(isl+1))
	else
	  sll=lc+1.
	endif
c
	do while (iqt.ne.0)
	  if (iam(iam(iqt+2)+1).ne.iam(iam(iqt+3)+1)) then
	    slp=real(iam(iam(iqt+3)+2)-iam(iam(iqt+2)+2))/
     +	        real(iam(iam(iqt+3)+1)-iam(iam(iqt+2)+1))
	    rty=real(iam(iam(iqt+2)+2))+real(ilx-iam(iam(iqt+2)+1))*slp
	  else
	    slp=lc+1.
	    rty=real(iam(iam(iqt+2)+2))
	  endif
	  if (rly.lt.rty.or.(rly.eq.rty.and.sll.lt.slp)) go to 10021
	  iqs=iqt
	  iqt=iam(iqt)
	enddo
10021 continue
c
	if (iqe.ne.0) then
	  iqi=iqe
	  iqe=iam(iqe)
	else
	  if (iam(6)-5.le.iam(5)) then
	  call ageset ('arpram: area buffer overflow',3,1)
	  return
	  endif
	  iqi=iam(6)-5
	  iam(6)=iqi
	endif
c
	iam(iqi)=iqt
	iam(iqi+1)=iqs
	iam(iqi+2)=isl
	iam(iqi+3)=isr
	iam(iqi+4)=isd
c
	if (iqs.ne.0) iam(iqs)=iqi
	if (iqt.ne.0) iam(iqt+1)=iqi
c
	if (iqb.eq.iqt) iqb=iqi
c
	if (iqs.ne.0) then
	  iql=iqs
	  iqu=iqi
	  call arpsub2(iam,iql,iqu,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  if(iare.ne.0) return
	endif
c
	if (iqt.ne.0) then
	  iql=iqi
	  iqu=iqt
	  call arpsub2(iam,iql,iqu,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  if(iare.ne.0) return
	endif
c
	return
	end
c
	subroutine arpsub2(iam,iql,iqu,ipi,ipn,ipx)
	integer iam(*)
	integer iql,iqu,ipi,ipn,ipx
	include  'sgarea_inc'
	integer iare
	integer kf1,isx,isy,iqb,iqe
	common /ncocom/ iare
	common /arpcom/ kf1,isx,isy,iqb,iqe
	integer ix1, ix2, ix3, ix4, iy1, iy2, iy3, iy4
	integer ix0, iy0
	real    fy1, fy2, fy3, fy4, fx1, fx2, fx3, fx4, fx0, fy0
c
c this internal procedure looks for intersection between the
c segments identified by the pointers iql and iqu.  when such points of
c intersection are found, they are interpolated along the intersecting
c line segments and any line segments which pass within one unit.
c
	if (kf1.eq.0.or.iam(iam(iql+4)+8).le.0.or.iam(iam(iql+4)+9).le.0
     +	  .or.iam(iam(iqu+4)+8).le.0.or.iam(iam(iqu+4)+9).le.0) then
c
	  ix1=iam(iam(iql+2)+1)
	  fx1=real(ix1)
	  iy1=iam(iam(iql+2)+2)
	  fy1=real(iy1)
	  ix2=iam(iam(iql+3)+1)
	  fx2=real(ix2)
	  iy2=iam(iam(iql+3)+2)
	  fy2=real(iy2)
	  ix3=iam(iam(iqu+2)+1)
	  fx3=real(ix3)
	  iy3=iam(iam(iqu+2)+2)
	  fy3=real(iy3)
	  ix4=iam(iam(iqu+3)+1)
	  fx4=real(ix4)
	  iy4=iam(iam(iqu+3)+2)
	  fy4=real(iy4)
c
	  tmp=(fx4-fx3)*(fy1-fy2)-(fx2-fx1)*(fy3-fy4)
c
	  if (abs(tmp).gt.tenth) then
c
	  ix0=int(((fx4-fx3)*(fx2*fy1-fx1*fy2)
     +	  -(fx2-fx1)*(fx4*fy3-fx3*fy4))/tmp+half)
	  iy0=int(((fy4-fy3)*(fx2*fy1-fx1*fy2)
     +	  -(fy2-fy1)*(fx4*fy3-fx3*fy4))/tmp+half)
c
	  if (ix0.eq.isx.and.iy0.lt.isy) ix0=ix0+1
c
	  fx0=real(ix0)
	  fy0=real(iy0)
c
	  if ((fx0-fx1)*(fx0-fx2).le.zero
     +	  .and.(fy0-fy1)*(fy0-fy2).le.zero
     +	  .and.(fx0-fx3)*(fx0-fx4).le.zero
     +	  .and.(fy0-fy3)*(fy0-fy4).le.zero) then
c
	  inp=0
c
	  if ((ix0.ne.ix1.or.iy0.ne.iy1).and.(ix0.ne.ix2.or.iy0.ne.iy2))
     +	  then
	    inp=1
	    ipi=iam(iql+4)
	    call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	    if(iare.ne.0) return
	    if(iare.ne.0) return
	    iam(iql+3)=ipn
	    if (iam(iql+4).ne.iam(iql+2)) iam(iql+4)=ipn
	  endif
c
	  if ((ix0.ne.ix3.or.iy0.ne.iy3).and.(ix0.ne.ix4.or.iy0.ne.iy4))
     +	  then
	    inp=1
	    ipi=iam(iqu+4)
	    call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	    if(iare.ne.0) return
	  if(iare.ne.0) return
	    iam(iqu+3)=ipn
	    if (iam(iqu+4).ne.iam(iqu+2)) iam(iqu+4)=ipn
	  endif
c
	if (inp.ne.0) then
	iqt=iqb
	do while (iqt.ne.0)
	  ix1=iam(iam(iqt+2)+1)
	  fx1=real(ix1)
	  iy1=iam(iam(iqt+2)+2)
	  fy1=real(iy1)
	  ix2=iam(iam(iqt+3)+1)
	  fx2=real(ix2)
	  iy2=iam(iam(iqt+3)+2)
	  fy2=real(iy2)
	  tmp=max(zero,min(one,
     +	    ((fx0-fx1)*(fx2-fx1)+(fy0-fy1)*(fy2-fy1))/
     +	    ((fx2-fx1)*(fx2-fx1)+(fy2-fy1)*(fy2-fy1))))
	  dsq=(fx1+(fx2-fx1)*tmp-fx0)**2+(fy1+(fy2-fy1)*tmp-fy0)**2
	  if ((dsq.lt.1.).and.(ix1.ne.ix0.or.iy1.ne.iy0).and.
     +	    (ix2.ne.ix0.or.iy2.ne.iy0)) then
	    ipi=iam(iqt+4)
	    call arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	    if(iare.ne.0) return
	  if(iare.ne.0) return
	    iam(iqt+3)=ipn
	    if (iam(iqt+4).ne.iam(iqt+2)) iam(iqt+4)=ipn
	  endif
	  iqt=iam(iqt)
	enddo
	endif
c
	endif
c
	endif
c
	endif
c
	return
	end
c
	subroutine arpsub1(iam,ix0,iy0,ipi,ipn,ipx)
	implicit none
	integer ix0,iy0,ipi,ipn,ipx
	integer iam(*)
	integer iare
	common /ncocom/ iare
	logical more,morf
c
c this internal procedure adds a new point in the existing part of the
c area map.  note that add-a-point puts a new point which has the same
c coordinates as an old point after the old point in the area map (in
c coordinate order); this is important.
c
	ipn=iam(5)+1
	if (iam(5)+10.ge.iam(6)) then
	  call ageset ('arpram: area buffer overflow',6,1)
	  return
	endif
	iam(5)=iam(5)+10
	iam(ipn)=iam(ipi)
	iam(ipn+1)=ix0
	iam(ipn+2)=iy0
	iam(ipn+3)=ipi
	iam(ipn+4)=iam(ipi+4)
	iam(iam(ipi+4)+3)=ipn
	iam(ipi+4)=ipn
	more=.true.
	do while (more)
	if (iam(ipn+1).lt.iam(ipx+1)) then
	  ipx=iam(ipx+6)
	elseif (iam(ipn+1).gt.iam(iam(ipx+5)+1)) then
	  ipx=iam(ipx+5)
	else
	  morf=.true.
	  do while (morf)
	    if (iam(ipn+1).eq.iam(ipx+1).and.
     +        iam(ipn+2).lt.iam(ipx+2)) then
	      ipx=iam(ipx+6)
	    elseif (iam(ipn+1).eq.iam(iam(ipx+5)+1).and.
     +		iam(ipn+2).gt.iam(iam(ipx+5)+2)) then
	      ipx=iam(ipx+5)
	    else
	      do while (iam(iam(ipx+5)+1).eq.iam(ipn+1).and.
     +		iam(iam(ipx+5)+2).eq.iam(ipn+2))
	        ipx=iam(ipx+5)
	      enddo
	      morf=.false.
	    endif
	  enddo	  !morf
	  more=.false.
	endif
	enddo	!more
c
	iam(ipn+5)=iam(ipx+5)
	iam(ipn+6)=iam(iam(ipx+5)+6)
	iam(iam(ipx+5)+6)=ipn
	iam(ipx+5)=ipn
	iam(ipn+7)=iam(ipi+7)
	iam(ipn+8)=iam(ipi+8)
	iam(ipn+9)=iam(ipi+9)
	return
	end
c
c----------------------------------------------------------------/arscam
c
	subroutine arscam (iam,xcs,ycs,mcs,iai,iag,mai,apr)
c
	implicit none
	integer mai, mcs
	integer iam(*),iai(mai),iag(mai)
	real    xcs(mcs),ycs(mcs)
	include  'sgarea_inc'
	integer iare
	common /ncocom/ iare
	logical lloop,ledge
	external apr
	integer lam, ill, ipx, ilw, ipt, ipu, ipq, ips, ipm, ipv
	integer iph, ipp, ip1, ip2, iof, ix0, iy0, ipi, ipn, iaq
	integer nai, iaf, ncs, idu, ixc, igi, ip3, ipr
	real    fut, fub, fur, ful, fft, ffb, ffr, ffl, xci, yci
	real    yco, ytm, xco
c
c the routine arscam is called to scan an area map created by calls to
c arinam and aredam.  for each subarea of the map, the user routine apr
c is called.
c
c iam is the array holding the area map, created by prior calls to the
c routines arinam and aredam.
c
c the arrays xcs and ycs are used, in a call to apr, to hold the x
c and y coordinates of the points defining a particular subarea.  each
c is dimensioned mcs.
c
c the arrays iag and iai are used, in a call to apr, to hold the
c group and area identifiers of the subarea defined by xcs and ycs.
c each is dimensioned mai.
c
c apr is the user's area-processing routine.  it must be declared in
c an external statement in the routine which calls arscam.  it will be
c called using a fortran statement like
c
c       call apr (xcs,ycs,ncs,iai,iag,nai)
c
c where xcs and ycs hold the normalized device coordinates of ncs points
c defining a single subarea (point number ncs being a duplicate of point
c number 1) and iai and iag hold nai area-identifier/group-identifier
c pairs for that subarea.
c
c
c declare certain variables to be real or double precision.
c
	real ang,ant,apq,aqr
c
c pull out the length of the area map and do a simple consistency check.
c
	lam=iam(1)
c
	if (iam(lam).ne.lam) then
	  call ageset ('arscam: area buffer uninitialized',1,1)
	  return
	endif
c
c save the current user-system mapping and reset it as required for
c calls to apr.
c
	call getset (ffl,ffr,ffb,fft,ful,fur,fub,fut,ill)
cgks	call set (0.,1.,0.,1.,0.,1.,0.,1.,1)
c
c initialize ipx, which is used to position nodes in coordinate order.
c
	ipx=8
c
c if it has not already been done, find points of intersection and
c incorporate them into the map and then adjust area identifiers.
c
	if (iam(4).eq.0) call arpram(iam,0,0,0)
c
c we first make a pass over the entire area map, looking for holes and
c eliminating them by the insertion of some temporary connecting lines.
c
c save the pointer to the last word of the last node, so that we can
c remove the nodes implementing the temporary connecting lines before
c returning to the caller.
c
	ilw=iam(5)
c
c each pass through the following loop traces the boundary of one
c connected loop.
c
	ipt=8
c
	lloop=.true.
	do while (lloop)
c
c move to the right across the area map, looking for an edge segment
c that has not yet been completely processed.  if no such segment can
c be found, all subareas have been done.
c
	do while (iam(ipt).ge.3.or.iam(ipt+7).le.1)
	  ipt=iam(ipt+5)
	  if (ipt.eq.18) go to 10003	!exit lloop
	enddo
c
c decide whether to scan the subarea to the left of the edge being
c traced (ipu=1) or the one to the right (ipu=2).
c
	if (iam(ipt).eq.0.or.iam(ipt).eq.2) then
	  ipu=1
	else
	  ipu=2
	endif
c
c ipq points to the node defining the beginning of the edge segment,
c ipr to the node defining the end of the edge segment, and ips to
c the node defining the beginning of the edge, so that we can tell
c when we've gone all the way around it.
c
	ipq=iam(ipt+4)
	ipr=ipt
	ips=ipq
	ipm=ipr
	ipv=ipu
c
c aqr holds the angle which the current edge segment makes with the
c positive x axis.
c
	aqr=atan2(real(iam(ipr+2)-iam(ipq+2)),
     +            real(iam(ipr+1)-iam(ipq+1)))
c
c we need to keep track of the highest point found along the loop and
c the total change in direction.  initialize the variables involved.
c
	iph=ipq
	ant=zero
c
c each pass through the following loop moves one step along the edge of
c the subarea.
c
	ledge=.true.
	do while (ledge)
c
c update the pointer to the highest point found along the loop.
c
	if (iam(ipr+2).gt.iam(iph+2)) iph=ipr
c
c move ipq to ipp, ipr to ipq, and aqr to apq.  adjust the range of apq.
c
	ipp=ipq
	ipq=ipr
	apq=aqr
	if (apq.lt.-pi) apq=apq+tp
	if (apq.gt.+pi) apq=apq-tp
c
c back up ipr to the beginning of the group of nodes which have the
c same x and y coordinates as it does.
c
	do while (iam(ipr+1).eq.iam(iam(ipr+6)+1).and.
     +	    iam(ipr+2).eq.iam(iam(ipr+6)+2))
	  ipr=iam(ipr+6)
	enddo
c
c go through the group of nodes, examining all the possible ways to
c move from the current position to a new one.  pick the direction
c which is leftmost (if ipu=1) or rightmost (if ipu=2).
c
	ip1=ipr
	ip2=ipr
	ipr=0
	if (ipu.eq.1) then
	aqr=apq-pi
	else
	aqr=apq+pi
	endif
c
	do while (iam(ip2+1).eq.iam(ip1+1).and.iam(ip2+2).eq.iam(ip1+2))
	if (iam(iam(ip2+3)+7).gt.1.and.(iam(iam(ip2+3)+1).ne.iam(ipp+1)
     +	    .or.iam(iam(ip2+3)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+3)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+3)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	if (ang.gt.aqr.and.ang.lt.apq+pi) then
	ipr=iam(ip2+3)
	aqr=ang
	ipm=ipr
	ipv=1
	endif
	else
	if (ang.lt.aqr.and.ang.gt.apq-pi) then
	ipr=iam(ip2+3)
	aqr=ang
	ipm=ipr
	ipv=2
	endif
	endif
	endif
	if (iam(ip2+7).gt.1.and.(iam(iam(ip2+4)+1).ne.iam(ipp+1).or.
     +	    iam(iam(ip2+4)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+4)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+4)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	  if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=2
	    endif
	  else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=1
	  endif
	endif
	endif
	ip2=iam(ip2+5)
	enddo
c
c if no possible exit was found, reverse direction.
c
	if (ipr.eq.0) then
	  ipr=ipp
	  ipv=3-ipv
	  if (ipu.eq.1) then
	    aqr=apq-pi
	  else
	    aqr=apq+pi
	  endif
	endif
c
c update the total angular change.
c
	ant=ant+aqr-apq
c
c update the markers for the edge segment picked.
c
	if (ipv.eq.1) then
	  if (mod(iam(ipm),2).eq.0) then
	    iam(ipm)=iam(ipm)+1
	  else
	    call ageset ('arscam: algorithm failure',2,1)
	    go to 10010
	  endif
	else
	  if (mod(iam(ipm)/2,2).eq.0) then
	    iam(ipm)=iam(ipm)+2
	  else
	    call ageset ('arscam: algorithm failure',3,1)
	    go to 10010
	  endif
	endif
c
c exit if we're passing the start of the subarea.
c
	if (iam(ipq+1).eq.iam(ips+1).and.iam(ipq+2).eq.iam(ips+2).and.
     +	    iam(ipr+1).eq.iam(ipt+1).and.iam(ipr+2).eq.iam(ipt+2))
     +	     ledge=.false.
c
	enddo	!ledge
c
c if the closed loop just traced was a hole, insert a temporary
c connecting line to get rid of the hole.
c
	if ((ipu.eq.1.and.ant.lt.zero).or.
     +       (ipu.eq.2.and.ant.gt.zero)) then
	iof=0
	xci=real(iam(iph+1))
	yci=real(iam(iph+2))
	yco=lc+1.
	ip1=iph
	do while (iam(iam(ip1+5)+1).eq.iam(iph+1))
	  ip1=iam(ip1+5)
	enddo
	do while (iam(ip1+1).ge.iam(iph+1)-iam(2))
	if (iam(ip1+7).gt.1.and.iam(iam(ip1+4)+1).gt.iam(ip1+1).and.
     +	    iam(iam(ip1+4)+1).ge.iam(iph+1)) then
	  ytm=real(iam(ip1+2))+(xci-real(iam(ip1+1)))*
     +	    (real(iam(iam(ip1+4)+2)-iam(ip1+2))/
     +	    real(iam(iam(ip1+4)+1)-iam(ip1+1)))
	  if (ytm.gt.yci.and.ytm.lt.yco) then
	    iof=ip1
	    yco=ytm
	  endif
	endif
	if (iam(iam(ip1+3)+7).gt.1.and.iam(iam(ip1+3)+1).gt.iam(ip1+1)
     +	    .and.iam(iam(ip1+3)+1).ge.iam(iph+1)) then
	  ytm=real(iam(ip1+2))+(xci-real(iam(ip1+1)))*
     +	    (real(iam(iam(ip1+3)+2)-iam(ip1+2))/
     +	    real(iam(iam(ip1+3)+1)-iam(ip1+1)))
	  if (ytm.gt.yci.and.ytm.lt.yco) then
	    iof=iam(ip1+3)
	    yco=ytm
	  endif
	endif
	ip1=iam(ip1+6)
	enddo
	if (iof.ne.0) then
	  ix0=iam(iph+1)
	  iy0=iam(iph+2)
	  if (int(yco).ne.iy0) then
	  ipi=18
	  call arssub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  iy0=int(yco)
	  call arssub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  iam(ipn+7)=1
	endif
	if ((ix0.ne.iam(iof+1).or.iy0.ne.iam(iof+2)).and.
     +	    (ix0.ne.iam(iam(iof+4)+1).or.iy0.ne.iam(iam(iof+4)+2))) then
	  ipi=iof
	  call arssub1(iam,ix0,iy0,ipi,ipn,ipx)
	  if(iare.ne.0) return
	  iam(ipn)=iam(ipi)
	  iam(ipn+7)=iam(ipi+7)
	  iam(ipn+8)=iam(ipi+8)
	  iam(ipn+9)=iam(ipi+9)
	endif
	endif
c
	endif
c
	enddo	!lloop
10003 continue
c
c zero the markers for all the nodes.
c
	do ipt=8,iam(5)-9,10
	  iam(ipt)=0
	enddo
c
c now, make a pass through the area map, tracing one subarea at a time
c and calling the routine apr to do with it what the user wants.
c
c each pass through the following loop traces the boundary of one
c subarea.
c
	ipt=8
c
	lloop=.true.
	do while (lloop)
c
c move to the right across the area map, looking for an edge segment
c that has not yet been completely processed.  if no such segment can
c be found, all subareas have been done.
c
	do while (iam(ipt).ge.3.or.iam(ipt+7).le.1)
	  ipt=iam(ipt+5)
	  if (ipt.eq.18) go to 10025	!exit lloop
	enddo
c
c decide whether to scan the subarea to the left of the edge being
c traced (ipu=1) or the one to the right (ipu=2).
c
	if (iam(ipt).eq.0.or.iam(ipt).eq.2) then
	  ipu=1
	  iaq=ipt+8
	else
	  ipu=2
	  iaq=ipt+9
	endif
c
c store the first group identifier and area identifier for the subarea
c and clear the flag that is set when all identifiers have been found.
c
	nai=1
	iai(nai)=iam(iaq)
	if (iai(nai).ge.iam(6)) iai(nai)=iam(iai(nai))/2
	iag(nai)=iam(iam(ipt+7))/2
	iaf=0
c
c ipq points to the node defining the beginning of the edge segment,
c ipr to the node defining the end of the edge segment, and ips to
c the node defining the beginning of the edge, so that we can tell
c when we've gone all the way around it.
c
	ipq=iam(ipt+4)
	ipr=ipt
	ips=ipq
	ipm=ipr
	ipv=ipu
c
c aqr holds the angle which the current edge segment makes with the
c positive x axis.  for the sake of efficiency, during this pass, its
c value is only computed when absolutely necessary.
c
	aqr=tenten
c
c put the first point in the list defining this subarea.
c
	ncs=1
	xcs(1)=real(iam(ipq+1))/lc
	ycs(1)=real(iam(ipq+2))/lc
c
c each pass through the following loop moves one step along the edge of
c the subarea.
c
	ledge=.true.
	do while (ledge)
c
c add the end of the current segment to the description of the subarea.
c
	if (ncs.lt.mcs) then
	  ncs=ncs+1
	  xcs(ncs)=real(iam(ipr+1))/lc
	  ycs(ncs)=real(iam(ipr+2))/lc
	else
	  call ageset ('arscam: pl buffer overflow',4,1)	!mcs too small
	  go to 10010
	endif
c
c if the group/area identifier information is incomplete and the current
c edge segment spans a portion of the x axis of non-zero length, scan
c outwards from the subarea for group/area identifier information.
c
	if (iaf.eq.0.and.iam(ipq+1).ne.iam(ipr+1)) then
c
	if (iam(ipq+1).lt.iam(ipr+1)) then
	  ip1=ipq
	  ip2=ipr
	  idu=3-ipu
	else
	  ip1=ipr
	  ip2=ipq
	  idu=ipu
	endif
c
	ixc=int(.5*(real(iam(ip1+1))+real(iam(ip2+1))))
	xco=real(ixc)+.5
	yco=real(iam(ip1+2))+(xco-real(iam(ip1+1)))*
     +	  (real(iam(ip2+2)-iam(ip1+2))/real(iam(ip2+1)-iam(ip1+1)))
c
	igi=lam
c
	do while (igi.gt.iam(6))
	igi=igi-1
	if (mod(iam(igi),2).eq.0.and.iam(ipt+7).ne.igi) then
	iaf=0
	if (idu.eq.1) then
	yci=lc+1.
	else
	yci=-1.
	endif
	ip3=iam(ip2+6)
	do while (iam(ip3+1)+iam(2).gt.ixc)
	if (iam(ip3+1).le.ixc) then
	if (abs(iam(ip3+7)).eq.igi.and.iam(ip3+1).lt.iam(iam(ip3+4)+1)
     +	    .and.ixc.lt.iam(iam(ip3+4)+1)) then
	ytm=real(iam(ip3+2))+(xco-real(iam(ip3+1)))*
     +	  (real(iam(iam(ip3+4)+2)-iam(ip3+2))/
     +	  real(iam(iam(ip3+4)+1)-iam(ip3+1)))
	if (idu.eq.1.and.ytm.lt.yci.and.ytm.ge.yco) then
	iaf=ip3
	iaq=iaf+8
	yci=ytm
	endif
	if (idu.eq.2.and.ytm.gt.yci.and.ytm.le.yco) then
	iaf=ip3
	iaq=iaf+9
	yci=ytm
	endif
	endif
	if (abs(iam(iam(ip3+3)+7)).eq.igi.and.
     +  iam(ip3+1).lt.iam(iam(ip3+3)+1)
     +	    .and.ixc.lt.iam(iam(ip3+3)+1)) then
	ytm=real(iam(ip3+2))+(xco-real(iam(ip3+1)))*
     +	  (real(iam(iam(ip3+3)+2)-iam(ip3+2))/
     +	  real(iam(iam(ip3+3)+1)-iam(ip3+1)))
	if (idu.eq.1.and.ytm.lt.yci.and.ytm.ge.yco) then
	iaf=iam(ip3+3)
	iaq=iaf+9
	yci=ytm
	endif
	if (idu.eq.2.and.ytm.gt.yci.and.ytm.le.yco) then
	iaf=iam(ip3+3)
	iaq=iaf+8
	yci=ytm
	endif
	endif
	endif
	ip3=iam(ip3+6)
	enddo
	if (iaf.ne.0) then
	if (nai.lt.mai) then
	nai=nai+1
	if (iam(iaq).lt.iam(6)) then
	iai(nai)=iam(iaq)
	else
	iai(nai)=iam(iam(iaq))/2
	endif
	iag(nai)=iam(igi)/2
	else
	call ageset ('arscam: mai too small',5,1)
	go to 10010
	endif
	endif
	endif
	enddo
	iaf=1
c
	endif
c
c move ipq to ipp, ipr to ipq, and aqr to apq.
c
	ipp=ipq
	ipq=ipr
	apq=aqr
c
c back up ipr to the beginning of the group of nodes which have the
c same x and y coordinates as it does.
c
	do while (iam(ipr+1).eq.iam(iam(ipr+6)+1).and.
     +		iam(ipr+2).eq.iam(iam(ipr+6)+2))
	  ipr=iam(ipr+6)
	enddo
c
c if there is only one node in the group, the exit path is obvious.
c
	if (iam(ipr+1).ne.iam(iam(ipr+5)+1).or.
     +	    iam(ipr+2).ne.iam(iam(ipr+5)+2)) then
	if (iam(iam(ipr+3)+1).ne.iam(ipp+1).or.
     +	    iam(iam(ipr+3)+2).ne.iam(ipp+2)) then
	if (iam(iam(ipr+3)+7).gt.0) then
	  ipm=iam(ipr+3)
	  ipr=ipm
	  ipv=ipu
	  aqr=tenten
	else
	  ipr=0
	endif
	else
	if (iam(ipr+7).gt.0) then
	  ipm=ipr
	  ipr=iam(ipr+4)
	  ipv=3-ipu
	  aqr=tenten
	else
	  ipr=0
	endif
	endif
c
c otherwise, go through the group of nodes, examining all the possible
c ways to move from the current position to a new one.  pick the
c direction which is leftmost (if ipu=1) or rightmost (if ipu=2).
c
	else
c
	ip1=ipr
	ip2=ipr
	ipr=0
	if (apq.eq.tenten) then
	  apq=atan2(real(iam(ipq+2)-iam(ipp+2)),
     +    real(iam(ipq+1)-iam(ipp+1)))
	endif
	if (apq.lt.-pi) apq=apq+tp
	if (apq.gt.+pi) apq=apq-tp
	if (ipu.eq.1) then
	  aqr=apq-pi
	else
	  aqr=apq+pi
	endif
c
	do while (iam(ip2+1).eq.iam(ip1+1).and.iam(ip2+2).eq.iam(ip1+2))
	if (iam(iam(ip2+3)+7).gt.0.and.
     +     (iam(iam(ip2+3)+1).ne.iam(ipp+1).or.
     +	  iam(iam(ip2+3)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+3)+2)-iam(ip2+2)),
     +	  real(iam(iam(ip2+3)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=1
	  endif
	else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+3)
	    aqr=ang
	    ipm=ipr
	    ipv=2
	  endif
	endif
	endif
	if (iam(ip2+7).gt.0.and.(iam(iam(ip2+4)+1).ne.iam(ipp+1)
     +	    .or.iam(iam(ip2+4)+2).ne.iam(ipp+2))) then
	ang=atan2(real(iam(iam(ip2+4)+2)-iam(ip2+2)),
     +	     real(iam(iam(ip2+4)+1)-iam(ip2+1)))
	if (ang.lt.apq-pi) ang=ang+tp
	if (ang.gt.apq+pi) ang=ang-tp
	if (ipu.eq.1) then
	  if (ang.gt.aqr.and.ang.lt.apq+pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=2
	  endif
	else
	  if (ang.lt.aqr.and.ang.gt.apq-pi) then
	    ipr=iam(ip2+4)
	    aqr=ang
	    ipm=ip2
	    ipv=1
	  endif
	endif
	endif
	ip2=iam(ip2+5)
	enddo
c
	endif
c
c if no possible exit was found, reverse direction.
c
	if (ipr.eq.0) then
	ipr=ipp
	ipv=3-ipv
	aqr=tenten
	endif
c
c update the markers for the edge segment picked.
c
	if (ipv.eq.1) then
	if (mod(iam(ipm),2).eq.0) then	!?iand(iam(ipm),1)
	  iam(ipm)=iam(ipm)+1
	else
	  call ageset ('arscam: algorithm failure',6,1)
	  go to 10010
	endif
	else
	if (mod(iam(ipm)/2,2).eq.0) then
	  iam(ipm)=iam(ipm)+2
	else
	  call ageset ('arscam: algorithm failure',7,1)
	  go to 10010
	endif
	endif
c
c exit if we're passing the start of the subarea.
c
	if (iam(ipq+1).eq.iam(ips+1).and.iam(ipq+2).eq.iam(ips+2).and.
     +	    iam(ipr+1).eq.iam(ipt+1).and.iam(ipr+2).eq.iam(ipt+2))
     +	    ledge=.false.
c
	enddo 	!ledge
c
c a complete subarea has been found.  let the user do what he wants
c with it.
c
	if (nai.eq.iam(7)) then
	  call apr (xcs,ycs,ncs,iai,iag,nai)
	else
	  call ageset ('arscam: algorithm failure',8,1)
	  go to 10010
	endif
c
	enddo	!lloop
10025 continue
c
c delete the nodes used to put in the temporary connecting lines.
c
	ipt=iam(5)-9
	do while (ipt.gt.ilw)
	  iam(iam(ipt+4)+3)=iam(ipt+3)
	  iam(iam(ipt+3)+4)=iam(ipt+4)
	  iam(iam(ipt+6)+5)=iam(ipt+5)
	  iam(iam(ipt+5)+6)=iam(ipt+6)
	  ipt=ipt-10
	enddo
	iam(5)=ilw
c
c zero the markers in all the remaining nodes.
c
	do ipt=8,iam(5)-9,10
	  iam(ipt)=0
	enddo
c
c restore the original set parameters and return to the caller.
c
10010 continue
cgks	call set (ffl,ffr,ffb,fft,ful,fur,fub,fut,ill)
	return
	end
c
	subroutine arssub1(iam,ix0,iy0,ipi,ipn,ipx)
	implicit none
	integer ix0,iy0,ipi,ipn,ipx
	integer iam(*)
	logical more,morf
c
c this internal procedure adds a new point in the existing part of the
c area map.
c
	ipn=iam(5)+1
	if (iam(5)+10.ge.iam(6)) then
	  call ageset ('arscam: area buffer overflow',9,1)
	  return	!go to 10010
	endif
	iam(5)=iam(5)+10
	iam(ipn)=0
	iam(ipn+1)=ix0
	iam(ipn+2)=iy0
	iam(ipn+3)=ipi
	iam(ipn+4)=iam(ipi+4)
	iam(iam(ipi+4)+3)=ipn
	iam(ipi+4)=ipn
	more=.true.
	do while (more)
	  if (iam(ipn+1).lt.iam(ipx+1)) then
	    ipx=iam(ipx+6)
	  elseif (iam(ipn+1).gt.iam(iam(ipx+5)+1)) then
	    ipx=iam(ipx+5)
	  else
	    morf=.true.
	    do while (morf)
	      if (iam(ipn+1).eq.iam(ipx+1).and.
     +        iam(ipn+2).lt.iam(ipx+2)) then
		ipx=iam(ipx+6)
	      elseif (iam(ipn+1).eq.iam(iam(ipx+5)+1).and.
     +		  iam(ipn+2).gt.iam(iam(ipx+5)+2)) then
		ipx=iam(ipx+5)
	      else
	        morf=.false.
	      endif
	    enddo
	    more=.false.
	  endif
	enddo
	iam(ipn+5)=iam(ipx+5)
	iam(ipn+6)=iam(iam(ipx+5)+6)
	iam(iam(ipx+5)+6)=ipn
	iam(ipx+5)=ipn
	iam(ipn+7)=0
	iam(ipn+8)=0
	iam(ipn+9)=0
	return
	end
c
	subroutine ageset(stext,i1,i2)
	implicit none
	integer i1, i2
	character stext*(*),gtext*(*),text*72
	integer iare
	common /ncocom/ iare
	save text
	integer ierr, narg
c
	call agtmhs(100,20,stext)
	text=stext
	iare=i1
	return
c
	entry ageget(narg,ierr,gtext)
c
	ierr=iare
	if(narg.gt.1) gtext=text
	end
c
	subroutine getset(ffl,ffr,ffb,fft,ful,fur,fub,fut,ill)
	implicit none
	real    ffl,ffr,ffb,fft,ful,fur,fub,fut
	integer ill
	real    xmin,xmax,ymin,ymax

	ffl=0.
	ffr=1.
	ffb=0.
	fft=1.
	call seedw(xmin,xmax,ymin,ymax)
	ful=xmin	!minx
	fur=xmax	!maxx
	fub=ymin	!miny
	fut=ymax	!maxy
	ill=1
	end
c
cgks	subroutine set
cgks	end
