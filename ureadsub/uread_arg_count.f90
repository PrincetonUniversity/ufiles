  subroutine uread_arg_count( nargu, nargc )
  !.. uread_arg_count                  Returns count of args not used by uread
  !                                    codesys/source/ureadsub/uread_arg_count.
  !
  !  Usage :-
  !    call  uread_arg_count( nargu, nargc)
  !    l_arg: do iargc=nargu+1,nargc
  !      ..
  !    enddo l_arg
  !---------------------------------------------------------------------------------
  !
  !  Author                            Jim.Conboy@ccfe.ac.uk
  !  Version                           1.00,  20May2010
  !  Modifications
  !
  !  1.00  20May2010                   Jim.Conboy@ccfe.ac.uk
  !                                    Written
  !----_^---------------------------------=========================================|
  !
  implicit  none
  !
  !..GLOBAL
  !variable                          :: name           ! description
  !
  !..Arguments
  !
  integer, intent(out)               :: nargu &        ! argument index, terminates uread args
                                       ,nargc          ! total number of args
  !
  !..local
      character(len=2), parameter    :: carg_delim='-!'
  !                                     terminates uread args on command line

  character(len=255)                 :: carg           ! buffer for argument inspection
  !
  character(len=*),parameter         :: cSr='ureadsub/uread_arg_count'
  !----------------------------------::================!.........................|
  !
  call get_arg_count( nargc )
  if( nargc == 0 )   then
    nargu = 0
  else
    l_arg: do nargu=1,nargc
       call get_arg( nargu, carg )
       if( carg(1:2) == carg_delim )    exit l_arg
    enddo l_arg
  endif
#ifdef _DEBUG
  print *,cSr,' -D- nargu, nargc : ',nargu,nargc
#endif
  return
  end subroutine uread_arg_count
