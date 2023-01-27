subroutine trgraf_slist_dlimit(inum)

  ! return display limit (max no. of profiles or curves in a display)
  ! stored here

  implicit NONE

  integer, intent(out) :: inum

  integer, parameter :: display_limit = 15

  !--------------------

  inum = display_limit

end subroutine trgraf_slist_dlimit

subroutine trgraf_slist_num(inum_orig,inum_slist)

  ! return the original list length or the display limit, whichever is lower

  implicit NONE

  integer, intent(in) :: inum_orig   ! orig. list length
  integer, intent(out) :: inum_slist ! returned list length

  !------------------

  integer :: inum  ! display limit

  integer :: luntrm,lnout

  !------------------

  call trgraf_slist_dlimit(inum)

  inum_slist = min(inum_orig,inum)

  if(inum_slist.lt.inum_orig) then

     luntrm = lnout(0)
     write(luntrm,*) ' '
     write(luntrm,*) ' %trgraf_slist: dataset count exceeds display limit;'
     write(luntrm,*) '  the first ',inum_slist, &
          ' items are displayed out of ',inum_orig,'.'
     write(luntrm,*) '  => use post-plot menu option "+" to change the', &
          ' selected display subset.'
     write(luntrm,*) ' '

  endif

end subroutine trgraf_slist_num

subroutine trgraf_slist_init(inum_orig,isel_orig,lbl_orig,tshif_orig, &
     inum_slist,isel_slist,lbl_slist,tshif_slist)

  ! initialize the sublist (a) to be the whole list if the sizes are the
  ! same, or (b) be the first (inum_slist) values, if inum_slist < inum_orig.

  ! it is expected that inum_slist.le.inum_orig

  implicit NONE

  integer, intent(in) :: inum_orig
  integer, intent(in) :: isel_orig(inum_orig)
  character*(*), intent(in) :: lbl_orig(inum_orig)
  real, intent(in) :: tshif_orig(inum_orig)

  integer, intent(in) :: inum_slist
  integer, intent(out) :: isel_slist(inum_slist)
  character*(*), intent(out) :: lbl_slist(inum_slist)
  real, intent(out) :: tshif_slist(inum_slist)

  !---------------------------------
  integer :: ii
  !---------------------------------

  if(inum_slist.gt.inum_orig) then
     write(6,*) ' ?trgraf_slist: inum_slist > inum_orig unexpected:'
     write(6,*) '  inum_orig = ',inum_orig,'  inum_slist = ',inum_slist
     isel_slist(inum_orig+1:inum_slist) = 0
     lbl_slist(inum_orig+1:inum_slist) = ' '
     tshif_slist(inum_orig+1:inum_slist) = 0.0
  endif

  do ii=1,inum_slist
     isel_slist(ii) = isel_orig(ii)
     lbl_slist(ii) = lbl_orig(ii)
     tshif_slist(ii) = tshif_orig(ii)
  enddo

end subroutine trgraf_slist_init

subroutine trgraf_slist_edit(inum_orig,isel_orig,lbl_orig,tshif_orig, &
     inum_slist,isel_slist,lbl_slist,tshif_slist,inum_cur)

  ! interactive routine to edit the display sublist
  ! ordering is preserved: if item J comes after item I in the ORIG list
  ! and both are in the sublist SLIST, then J comes after I in SLIST also.

  ! it is expected that inum_slist.le.inum_orig -- if not, exit w/warning.

  implicit NONE

  integer, intent(in) :: inum_orig
  integer, intent(in) :: isel_orig(inum_orig)
  character*(*), intent(in) :: lbl_orig(inum_orig)
  real, intent(in) :: tshif_orig(inum_orig)

  integer, intent(in) :: inum_slist
  integer, intent(inout) :: isel_slist(inum_slist)
  character*(*), intent(inout) :: lbl_slist(inum_slist)
  real, intent(inout) :: tshif_slist(inum_slist)
  integer, intent(out) :: inum_cur

  !---------------------------
  integer :: lnout,luntrm
  logical :: lchar,lxout
  logical :: idisplay(inum_orig)
  logical :: iadd

  integer :: ii,jj,imatch,indx

  character*20 test1,test2
  character*10 :: digits = '0123456789'
  !---------------------------

  luntrm = lnout(0)
  if(inum_slist.gt.inum_orig) then
     write(luntrm,*) ' ?trgraf_slist: inum_slist > inum_orig unexpected:'
     write(luntrm,*) '  inum_orig = ',inum_orig,'  inum_slist = ',inum_slist
     write(luntrm,*) '  EDIT operation skipped.'
     inum_cur=inum_orig
     return
  endif

5 continue

  idisplay=.FALSE.

  ! small lists, do not worry abount O(N^2) algorithms...

  do ii=1,inum_orig
     do jj=1,inum_slist
        if(isel_orig(ii).eq.isel_slist(jj)) then
           idisplay(ii)=.TRUE.
           exit
        endif
     enddo
  enddo

  do
     call idisp_count(inum_cur)

     if(lxout(0)) then
        write(luntrm,*) ' '
        do ii=1,inum_orig
           if(idisplay(ii)) then
              write(luntrm,1001) ii,lbl_orig(ii)
           else
              write(luntrm,1002) ii,lbl_orig(ii)
           endif
        enddo
1001    format(4x,i2,'. ',a,' Display=True.')
1002    format(4x,i2,'. ',a,' Display=FALSE.')
        write(luntrm,*) ' '
        write(luntrm,*) ' minimum number of displayed items on exit: 1'
        write(luntrm,*) ' maximum number of displayed items on exit: ', &
             inum_slist
        write(luntrm,*) ' current number of displayed items: ',inum_cur
        write(luntrm,*) ' '
        write(luntrm,*) ' enter name or number to ADD item to list;'
        write(luntrm,*) ' enter -name or -number to REMOVE item from list;'
        write(luntrm,*) ' enter "D" to REMOVE everything from the list;'
        write(luntrm,*) ' enter "I" to re-initialize list;'
        write(luntrm,*) ' enter "Q" to exit.'
        write(luntrm,*) ' '
     endif

     test1=' '
     call ureadw('trgraf_slist_edit option or item name to add/remove:', &
          test1)
     call uupper(test1)

     ! collective operations:

     if(test1.eq.'D') then
        idisplay = .FALSE.
        cycle

     else if(test1.eq.'I') then
        call trgraf_slist_init(inum_orig,isel_orig,lbl_orig,tshif_orig, &
             inum_slist,isel_slist,lbl_slist,tshif_slist)
        go to 5

     else if(test1.eq.'Q') then
        if(inum_cur.eq.0) then
           call uermsg('%trgraf_slist: at least one item must be selected.', &
                ' ',0)
           cycle
        else if(inum_cur.gt.inum_slist) then
           call uermsg('%trgraf_slist: too many display items: remove some.', &
                ' ',0)
           cycle
        else
           exit
        endif
     endif

     ! operation on individual item (apparently)

     iadd=.TRUE.
     if(test1(1:1).eq.'-') then
        iadd=.FALSE.
        test1 = trim(test1(2:))
     endif

     imatch = 0

     ! try name recognition

     do ii=1,inum_orig
        test2=lbl_orig(ii)
        if(index(' +-',test2(1:1)).gt.0) test2=lbl_orig(ii)(2:)
        call uupper(test2)
        if(test1.eq.test2) then
           imatch=ii
           exit
        endif
     enddo

     if(imatch.eq.0) then

        ! try integer decode

        do jj=1,len(trim(test1))
           indx=index(digits,test1(jj:jj))
           if(indx.lt.1) then
              imatch=0
              exit  ! not an integer
           else
              ! OK digit
              imatch = 10*imatch + (indx-1)
           endif
        enddo

        if(imatch.gt.inum_orig) then
           call uermsi('%trgraf_slist: integer out of range:',imatch,0)
           cycle
        else if(imatch.le.0) then
           call uermsg('%trgraf_slist: item identifier unrecognized:', &
                test1,0)
           cycle
        endif
     endif

     if(iadd) then
        idisplay(imatch)=.TRUE.
     else
        idisplay(imatch)=.FALSE.
     endif

  enddo

  ! OK, assemble sublist

  jj = 0
  do ii=1,inum_orig
     if(idisplay(ii)) then
        jj=jj + 1
        lbl_slist(jj) = lbl_orig(ii)
        isel_slist(jj) = isel_orig(ii)
        tshif_slist(jj) = tshif_orig(ii)
     endif
  enddo

  inum_cur = jj

CONTAINS
  subroutine idisp_count(icount)

    integer, intent(out) :: icount

    ! count number of TRUE display flags

    icount = 0
    do ii=1,inum_orig
       if(idisplay(ii)) icount = icount + 1
    enddo

  end subroutine idisp_count

end subroutine trgraf_slist_edit
