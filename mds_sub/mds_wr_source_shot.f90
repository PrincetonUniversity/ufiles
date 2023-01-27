subroutine mds_wr_source_shot(ztok,nshot,istat)

  ! write SOURCE_SHOT
  ! but if ztok = "CMOD" and SOURCE_SHOT is already defined, do not overwrite!

  implicit NONE

  character*(*), intent(in) :: ztok
  integer, intent(in) :: nshot

  integer, intent(out) :: istat  ! MDS style, return =1 for normal exit

  !---------------------------------------------
  character*4 ztoka
  integer :: idescr_long,ishot,ilun,jstat,isize
  integer :: mds_value  ! mdstransp
  integer :: MdsPut     ! native MDSplus
  integer :: lnout  ! ureadsub LUN function
  !---------------------------------------------

  istat = 1  ! default: no error

  ztoka=ztok
  call uupper(ztoka)

  if(ztoka.eq.'CMOD') then
     ilun = lnout(0)
     jstat = mds_value(':SOURCE_SHOT',idescr_long(ISHOT),isize)

     if (mod(jstat,2).eq.1) then
        if(ishot.gt.0) then
           write(ilun,*) ' CMOD tree SOURCE_SHOT = ',ishot,' not changed.'
           RETURN
        else
           write(ilun,*) ' CMOD tree SOURCE_SHOT = ',ishot,' reset to: ',nshot
        endif
     else
        write(ilun,*) ' CMOD tree SOURCE_SHOT could not be read; set to: ',nshot
     endif
  endif

  istat = MdsPut('SOURCE_SHOT'//CHAR(0),'$'//CHAR(0),idescr_long(nshot),0, &
       isize)

end subroutine mds_wr_source_shot
