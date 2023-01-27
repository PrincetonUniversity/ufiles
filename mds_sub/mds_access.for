      module mmds_access

*.. mmds_access                         manipulate mdsplus access settings
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.01,  07Feb2007
*     Modifications
*
*     1.01  07Feb2007                   jim.conboy@jet.uk
*                                       getarg -> get_arg ( lib/portlib.a )
*     1.00  06Apr2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
*
      implicit  none
*
*..GLOBAL
*
      type tmds_access
                                                      ! mdsplus server access
         character(len=4)             :: cSite        ! site where running 
         character(len=64)            :: cserver      ! server name
         character(len=24)            :: cTree        ! Tree name
         integer                      :: ishot,       ! Tok shot #
     +                                   imds_shot,   ! mds plus shot #
     +                                   imds_option  ! open option
                                                      ! 1 = read output (restore)
                                                      ! 2 = read output (no-wait)
                                                      ! 3 = read (input only)
                                                      ! 4 = edit (no-restore)
                                                      ! 5 = Create
                                                      ! 6 = connect only
*
         character(len=3)             :: cRun         ! transp run extn
         character(len=4)             :: cTok         ! Tokamak name
         character(len=256)           :: cCDFnm       ! Assoc netcdf file
         integer                      :: iyear        ! Shot year
         logical                      :: lConnect     ! is connected
         integer                      :: ierr         ! (last) error
      end type tmds_access

      character(len=1)                :: cgdlm='/'    ! file name delimiter
*
      character(len=*), parameter     :: zcM = 'mmds_access'
      contains

      subroutine mds_acc_def( tmds_acc, cSite, cProg, istat )
*.. mds_acc_def                         Set site specific mdsplus defaults
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  06Apr2006
*     Modifications
*
*     1.00  06Apr2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              :: tmds_acc       ! Access parameters
      character(len=*)               :: cSite,         ! set defaults for site
     +                                  cProg          ! & program
      integer, intent(out)           :: istat          ! return status, 0=OK
*
*..local
      integer                        :: i              ! loop index 
*
*--   JET defaults
*
      character(len=*), parameter    :: cJET_server=
     +                                     'mdsplus.jet.efda.org',
     +                                  cJETRun='   ',
     +                                  cJETTok='JET '
*--
      character(len=*),parameter     :: zcr='mds_acc_def'
*     character(len=*),parameter     :: cStop=cM//cr
*----_^------------------------------::================!.........................|
*
      istat = 1                             ! assume the worst..
      Site: select case( cSite(1:3))
         case('   ')                        ! flag when not used
            tmds_acc%cSite     = '    '
            tmds_acc%cserver   = '    '
            tmds_acc%ctree     = '    '
            tmds_acc%ishot     = 0
            tmds_acc%imds_shot = 0
            tmds_acc%cRun      = cJETRun
            tmds_acc%cTok      = cJETTok 
            tmds_acc%iyear     = -1         ! not used
         case('JET','jet')
            tmds_acc%cSite     = 'JET '
            tmds_acc%cserver   = cJET_server
            tmds_acc%ishot     = 0
            tmds_acc%imds_shot = 0
            tmds_acc%cRun      = cJETRun
            tmds_acc%cTok      = cJETTok 
            tmds_acc%iyear     = -1         ! not used 
            istat              = 0
            if( cProg == 'mdsplot' )   then
                tmds_acc%ctree     = 'trout'
            endif
         case default
            print *,zcr,' -E- No defaults coded for site ',cSite
      end select site
*
      end subroutine mds_acc_def


      subroutine mds_acc_args( tmds_acc, istat )
*.. mds_acc_args                        Parse arguments related to mdsplus
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  06Apr2006
*     Modifications
*
*     1.00  06Apr2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              :: tmds_acc       ! Access parameters
      integer, intent(out)           :: istat          ! error flag
*
*..Local
      integer                        :: nargs,         ! # of arguments
     +                                  iarg           ! Arg count
*
      character(len=64)              :: carg,          ! current arg
     +                                  cargl          ! previous arg
*
      character(len=*),parameter     :: zcr='mds_acc_args'
      character(len=*),parameter     :: cStop=zcM//zcr
*----_^------------------------------::================!.........................|

      call get_arg_count( nargs )
*
      if( nargs < 1 )            then
          carg = '?'
      else
         call get_arg( 1, carg )
      endif
*
      if( nargs < 4 .or. index( carg,'?' ) > 1 )          then
         write(6,600) '-p -s','<pulse>',' '
         write(6,600) '-r ', '<runid>', 'a1i2'
         write(6,600) '-srv','<mds server>', ''
         write(6,600) '-o',  'Open option ', '5 = create'
*
         istat  =  -1 ;                                  return
                                                         endif
 600  format(8x,a,t16,a,t32,a)
*         
      cargl = ''
      l_arg: do iarg=1,nargs
         call get_arg(iarg, carg)

         select case (cargl)

            case ('-P','-S','-p','-s')
               read(carg,'(i9)') tmds_acc%ishot

            case('-SRV', '-srv' )
               tmds_acc%cserver = trim(carg )

            case ('-O','-o')
               read(carg,'(i9)') tmds_acc%imds_option

            case ('-R','-r')
               tmds_acc%cRun = trim( carg )

            case default
         end select
         cargl  =  carg
      end do l_arg
*
      print *,zcr,': Shot, option ',
     +               tmds_acc%ishot, tmds_acc%imds_option
*
      if( tmds_acc%ishot < 1 )     then
         print *,zcr,' -F- Invalid shot number ',tmds_acc%ishot
         call ufbad_exit(cStop)
      endif
*
      if(      tmds_acc%imds_option < 1 
     +    .or. tmds_acc%imds_option > 6 )  then
         print *,zcr,' -F- Invalid option ',tmds_acc%imds_option
         call ufbad_exit(cStop)
      endif
*
      print *,'mds_acc_args not implemented'
      end subroutine mds_acc_args


      subroutine mds_acc_local( tmds_acc, istat )
*.. mds_acc_local                        Parse arguments related to mdsplus
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  06Apr2006
*     Modifications
*
*     1.00  06Apr2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              :: tmds_acc       ! Access parameters
      integer, intent(out)           :: istat          ! error flag
*
*..Local
      integer                        :: nargs,         ! # of arguments
     +                                  iarg           ! Arg count
*
      character(len=64)              :: carg,          ! current arg
     +                                  cargl          ! previous arg
*
      character(len=54)              :: cABC =
     +   ' aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
      character(len=*),parameter     :: zcr='mds_acc_local'
      character(len=*),parameter     :: cStop=zcM//zcr
*----_^------------------------------::================!.........................|
*
      select case( tmds_acc%cSite)
         case('JET ')
            if( tmds_acc%cRun /= '   ' )                  then
*--   Fix mds shot #
               read(tmds_acc%cRun(2:3),'(i2)') 
     +                        tmds_acc%imds_shot
                tmds_acc%imds_shot  =
     +             tmds_acc%imds_shot
     +           + tmds_acc%iShot*10000
     +           + 100*(index(cABC, tmds_acc%cRun(1:1))/2)
                                                          endif
*
         case default
      end select

      end subroutine mds_acc_local

      subroutine mds_acc_open( tmds_acc, istat )
*.. mds_acc_open                        open tree ( just calls mds_conopn for now )
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  06Apr2006
*     Modifications
*
*     1.00  06Apr2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              :: tmds_acc       ! Access parameters
      integer, intent(out)           :: istat          ! error flag
*
*..Local
      integer                        :: lunlog = 6,
     +                                  imds_socket    ! not used
*
      character(len=10)              :: cShot
*
      character(len=*),parameter     :: zcr='mds_acc_open'
      character(len=*),parameter     :: cStop=zcM//zcr
*----_^------------------------------::================!.........................|
*
      write(cShot,'(I10)') tmds_acc%ishot
*
      call mds_conopn( lunlog,
     +                 tmds_acc%cserver,
     +                 tmds_acc%ctree,
     +                 trim(cShot)//tmds_acc%crun,
     +                 tmds_acc%cTok,              ! not used
     +                 tmds_acc%iyear,
     +                 tmds_acc%imds_shot,
     +                 imds_socket,
     +                 tmds_acc%imds_option,
     +                 tmds_acc%ierr           )
*
      print *,zcr,': mds_conopn returns ',tmds_acc%ierr
*
      if( tmds_acc%ierr /= 0 )   call ufbad_exit(zcr)
*

      end subroutine mds_acc_open

      subroutine mds_acc_cdfnm( tmds_acc, cdir, istat )
*.. mds_acc_cdfnm                       TEMPORARY - fix up cdf filename ( for testing )
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  25May2006
*     Modifications
*
*     1.00  25May2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              :: tmds_acc       ! mds spec
      character(len=*), intent(in)   :: cdir           ! directory spec
      integer, intent(out)           :: istat          ! return status, 0=OK
*
*..local
*
      character(len=512)             :: cbuf           ! work buffer
      integer                        :: il             ! cbuf index 
*
      character(len=*),parameter     :: zcr='mds_acc_cdfnm'
*     character(len=*),parameter     :: cStop=cM//cr
*----_^------------------------------::================!.........................|
*
      if( cdir(1:1) == '$' )           then
         call sget_env( cdir(2:), cbuf )
      else
         cbuf  =  trim(cdir)
      endif
*
      cbuf  =              trim(cbuf)
     +         // cgdlm // trim(tmds_acc%cTok)
     +         // cgdlm
*
      il  =  len_trim( cbuf ) + 1
      print *,zcr,' :',cbuf(:il)
*
      write(cbuf(il:),'(i5.5)')    tmds_acc%ishot
*
      cbuf  =              trim(cbuf)
     +         // cgdlm // trim(tmds_acc%cRun)
     +         // cgdlm // trim(tmds_acc%cRun) // '.CDF' 
*
      tmds_acc%cCDFnm  =  trim(cbuf)
*
      print *,zcr,' :',tmds_acc%cCDFnm
*
      end subroutine mds_acc_cdfnm


      subroutine mds_acc_get(  tmds_acc, istat, cRunId, cCDFnm )
*.. mds_acc_get                         Return variable(s) from tmds_acc dtatype
*
*
*     Author                            jim.conboy@jet.uk
*     Version                           1.00,  25May2006
*     Modifications
*
*     1.00  25May2006                   jim.conboy@jet.uk
*                                       Written
*----_^---------------------------------=========================================|
      implicit  none
*
*..GLOBAL
*--   variable                       :: name           ! description
*
*..Arguments
*
      type(tmds_access)              ::  tmds_acc
      integer, intent(out)           :: istat
*
      character(len=*), 
     +   optional, intent(out)       :: cRunId,        ! RunID - nnnnnxnn
     +                                  cCDFnm         ! CDF file name
*
*..local
      integer                        :: i              ! loop index 
      character(len=6)               :: cshot
*
      character(len=*),parameter     :: zcr='mds_acc_get'
*     character(len=*),parameter     :: cStop=cM//cr
*----_^------------------------------::================!.........................|
*
      istat  =  1
*     if( .not. associated( tmds_acc ))                 return
      istat  =  0
*
      write(cshot,'(i5.5)')    tmds_acc%ishot
      if( present ( cRunId))            then
         cRunID  =  trim( cshot)//trim( tmds_acc%cRun)//' '
      endif
*
      if( present( cCDFnm ))
     +    cCDFnm  =  trim(  tmds_acc%cCDFnm )//' '
*
      end subroutine mds_acc_get


      end module mmds_access
