subroutine ck_sepseg(xsegs, nsegs, norder, iorder)
  implicit none
  integer, intent(in) :: nsegs       ! no. of segments
  real, intent(in) :: xsegs(2,nsegs) ! segments; xsegs(1,i).le.xsegs(2,i)
  integer, intent(out) :: norder     ! ordering flag
                                     ! +1 -- ordering OK
                                     !  0 -- segments not disjoint
                                     ! -1 -- invalid segment
  integer, intent(out) :: iorder(nsegs) ! ordering
  !
  !  returning ordering of disjoint segments if possible
  !  a segment is a pair of numbers x1,x2 satisfying x1.le.x2
  !  (x1a,x2a) is disjoint from (x1b,x2b) if x1a.gt.x2b or x2a.lt.x1b
  !
  !  if all segments are disjoint, their ordering is returned in iorder(...)
  !  so that xsegs(1:2,iorder(1)) is the segment with lowest values,
  !          xsegs(1:2,iorder(2)) the next lowest, etc.
  !
  !  caution: N^2 algorithm is used, number of segments is assumed to be small!
  !
  integer ii,jj,inside,iord(nsegs)
  !--------------------------
  !
  norder=-1
  do ii=1,nsegs
    if(xsegs(1,ii).gt.xsegs(2,ii)) return  ! invalid segment
  enddo
  !
  ! simple answer if only one segment
  !
  if(nsegs.eq.1) then
    norder=1
    iorder(norder)=1
    return
  endif
  !
  ! scan multiple segments -- n**2 algorithm
  !
  norder=0
  iord=1
  inside=0
  !
  do jj=1,nsegs
    do ii=1,nsegs
      if(ii.eq.jj) cycle
      if((xsegs(2,jj).lt.xsegs(1,ii)).or. &
           (xsegs(1,jj).gt.xsegs(2,ii))) then
        if(xsegs(1,jj).gt.xsegs(2,ii)) then
          iord(jj)=iord(jj)+1
        endif
      else
        inside=ii
        exit
      endif
    enddo
    if(inside.ne.0) exit
  enddo
  !
  if(inside.ne.0) then
    continue                       ! not disjoint
  else
    norder=nsegs
    iorder=iord
  endif
  !
  return
end subroutine ck_sepseg
