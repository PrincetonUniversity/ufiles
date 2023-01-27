subroutine bu_eval(lbl3,lbl4,irast)

  ! compute number of additional lines for labels at bottom of graph:
  !   if len(trim(lbl3)) > 60, add 1
  !   if lbl4 non-blank, add 1

  use bottoms_up
  implicit NONE

  character*(*), intent(in) :: lbl3,lbl4
  integer, intent(out) :: irast  ! raster count for raising items

  !------------------------------
  integer :: indx,indx2,ilen,ii,jj
  character*100 ztest
  character*1 :: ztab
  !------------------------------

  ztab = char(9)

  ibu_raise = 0

  ! check lbl3

  ilen = len(trim(lbl3))
  if(ilen.gt.60) then
     ibu_raise = ibu_raise + 1

     ztest = lbl3(1:min(100,ilen))
     call uupper(ztest)
     indx = index(ztest,' VS ')
     indx2 = index(ztest,' VS. ')
     if(indx.gt.0) then
        ibrk_lbl3 = indx
     else if(indx2.gt.0) then
        ibrk_lbl3 = indx2
     else
        ibrk_lbl3 = 60

        ! try to find blank-delimitted breakpoint

        do ii=61,1,-1
           if((lbl3(ii:ii).eq.' ').OR.(lbl3(ii:ii).eq.ztab)) then
              jj=ii
              exit
           endif
        enddo

        do ii=jj-1,1,-1
           if((lbl3(ii:ii).ne.' ').AND.(lbl3(ii:ii).ne.ztab)) then
              ibrk_lbl3=ii
              exit
           endif
        enddo
     endif
  endif

  ! check lbl4

  if(lbl4.ne.' ') then
     ibu_raise = ibu_raise + 1
  endif

  ! raster measure of the raising of lower edge of graph:

  irast = ibu_raise*idybu

end subroutine bu_eval

subroutine bu_reset

  ! reset the data to zero...

  use bottoms_up
  implicit NONE

  ibu_raise = 0
  ibrk_lbl3 = 0

end subroutine bu_reset
