subroutine uf1f2_renorm(x,nx,y,ny,f,ilunu,jshot,nmax,zwk,idimr,luntrm,ier)

  ! Ufile based renormalizer routine with f77-style static array interface
  ! (called from gsmoo2)

  !  f(x,y) -> r(x)*f(x,y)   (if idimr=1)
  !  f(x,y) -> r(y)*f(x,y)   (if idimr=2)

  implicit NONE

  integer :: nx,ny      ! Ufile data dimensions

  real :: x(nx),y(ny)   ! Ufile independent coordinates
  real :: f(nx,ny)      ! Ufile data

  integer :: ilunu      ! Ufile channel for renormalization data
  !  (setup by caller e.g. with a ufnter call)

  integer :: jshot      ! Ufile shot number

  integer :: nmax       ! max 1d array dimension
  real :: zwk(nmax,4)   ! workspace

  integer :: idimr      ! independent coordinate of renormalization factor
  !  1 for x,  2 for y

  integer :: luntrm     ! LUN for messages

  integer, intent(out) :: ier   ! status return code, 0=OK

  !--------------------------------
  character*4 :: idev
  character*10 :: zsdate
  character*10 :: zxlab(3),zylab(3)

  integer :: inum,iproc,numsc,ii
  integer, parameter :: maxsc = 10
  real :: scval(maxsc)
  character*10 :: sclab(3,maxsc)
  !--------------------------------
  ier = 0

  if(nx.gt.nmax) then
     ier=1
     write(luntrm,*) ' ?uf1f2_renorm: nx > nmax: ',nx,nmax
  endif

  if(ny.gt.nmax) then
     ier=1
     write(luntrm,*) ' ?uf1f2_renorm: ny > nmax: ',ny,nmax
  endif

  call chka(x,nx,'X')
  call chka(y,ny,'Y')

  if(ier.gt.0) return

  call ufoprd(ilunu,jshot,ier)
  if(ier.ne.0) then
     write(luntrm,*) ' ?uf1f2_renorm: renormalization Ufile open failure.'
     return
  endif

  call uf1drd(ilunu,idev,zsdate, &
       zwk(:,1),zwk(:,2),nmax,inum,iproc, &
       zxlab,zylab,maxsc,numsc,scval,sclab,ier)

  call ufclos(ilunu)
  if(ier.ne.0) then
     write(luntrm,*) ' ?uf1f2_renorm: renormalization Ufile read failure.'
     return
  endif

  call chka(zwk(:,1),inum,'Xrenorm')
  if(ier.ne.0) return

  if(idimr.eq.1) then
     call xmap(x)
     do ii=1,nx
        f(ii,1:ny) = zwk(ii,3)*f(ii,1:ny)
     enddo
  else
     call xmap(y)
     do ii=1,ny
        f(1:nx,ii) = zwk(ii,3)*f(1:nx,ii)
     enddo
  endif

CONTAINS
  subroutine chka(xx,inxx,lbl)

    integer :: inxx  ! dimension
    real :: xx(inxx) ! array
    character*(*) :: lbl  ! label

    integer :: ii

    ! check that array is strictly non-descending

    ier = 0
    do ii=1,inxx-1
       if(xx(ii+1).lt.xx(ii)) ier = ier + 1
    enddo

    if(ier.gt.0) then
       write(luntrm,*) ' ?uf1f2_renorm: coordinate "'//trim(lbl)// &
            '" not ascending at ',ier,' pts.'
    endif

  end subroutine chka

  subroutine xmap(cwk)

    real, dimension(:) :: cwk

    integer :: iwk,ii,izon

    iwk=size(cwk)

    izon=0
    do ii=1,iwk
       if(cwk(ii).le.zwk(1,1)) then
          zwk(ii,3)=zwk(1,2)
       else if(cwk(ii).ge.zwk(inum,1)) then
          zwk(ii,3)=zwk(inum,2)
       else
          do 
             if(zwk(izon+1,1).lt.cwk(ii)) then
                izon=izon+1
                cycle
             endif

             if(zwk(izon,1).eq.cwk(ii)) then
                zwk(ii,3)=zwk(izon,2)
             else if(zwk(izon+1,1).eq.cwk(ii)) then
                zwk(ii,3)=zwk(izon+1,2)
             else
                zwk(ii,3)=zwk(izon,2) + (cwk(ii)-zwk(izon,1))* &
                     (zwk(izon+1,2)-zwk(izon,2))/(zwk(izon+1,1)-zwk(izon,1))
             endif

             exit
          enddo
       endif
    enddo

  end subroutine xmap

end subroutine uf1f2_renorm
