      real function pintdiff(xwant,ihint,x,f,ngot,coef,nd1,
     >   iorder,ibase,ixtrap,ixflag)
C
C  dmc 8 Oct 1998
C  evaluate interpolation function using coefficients previously
C  computed in pintcoef
C
C  input:
      real xwant                        ! target x value desired
      integer ihint                     ! hint as to zone containing x
      real x(ngot),f(ngot)              ! data points of fcn being interpolated
      integer ngot                      ! number of data points
      real coef(nd1,ngot)               ! polynomial coefficients (see below)
      integer nd1                       ! 1st dimension of coef array
      integer iorder                    ! order of polynomial pieces
      integer ibase                     ! choice of piece mixing function
      integer ixtrap                    ! extrapolation control
C
C  output:
      integer ixflag                    ! completion code
C                       = 0 normal
C                       = 1 extrapolated high (xwant.gt.x(ngot))
C                       =-1 extrapolated low  (xwant.lt.x(1))
C
C                       =-99 some other error
C
C  this routine does the following (some portions are done in pintchk(...):
C
C    (a) make sure that input values are consistent -- exit with error
C        message if not
C    (b) check for and evaluate extrapolations -- set flag
C    (c) find the zone [x(k),x(k+1)] in which x is located (use ihint)
C    (d) evaluate P[k](xwant) and P[k+1](xwant) and the blending functions
C        alpha(xi), alpha-bar(xi) = 1-alpha, xi=(xwant-x(k))/(x(k+1)-x(k))
C    (e) return interpolant value
C          alpha-bar*P[k] + alpha*P[k+1]
C
C        each P[k] is defined from x(k-1) to x(k+1) and
C        P[k](xwant) = f(k) + sum[j=1 to iorder] (delx**j * coef(j,k))
C              where delx = xwant-x(k)
C
C-----------------------
C  statement fcns
C
      include 'pint_fcns.blk'
C
C-----------------------
C
      pintdiff=0.0
      call pintchk(xwant,ihint,x,ngot,nd1,iorder,ibase,ixtrap,ixflag)
      if(ixflag.eq.-99) go to 1000
      if(ixflag.eq.1) then
         pintdiff=0.0
         go to 1000
      else if(ixflag.eq.-1) then
         pintdiff=0.0
         go to 1000
      endif
C
      k=ihint
C
C  evaluate mixing functions
C
      xi=(xwant-x(k))/(x(k+1)-x(k))
      if(ibase.eq.1) then
         alpha=bs1(xi)
         dadx=bs1d1(xi)
      else if(ibase.eq.2) then
         alpha=bs2(xi)
         dadx=bs2d1(xi)
      else
         alpha=bs3(xi)
         dadx=bs3d1(xi)
      endif
      alphabar=1.0-alpha
      dadxbar=-dadx
C
C  evaluate P[k],P[k+1]
C
      pksum=f(k)
      pk1sum=f(k+1)
      dksum=0.0
      dk1sum=0.0
      delx=xwant-x(k)
      delx1=xwant-x(k+1)
      prod=1.0
      prod1=1.0
      do j=1,iorder
         prodp=prod
         prod=prod*delx
         prod1p=prod1
         prod1=prod1*delx1
C
         pksum=pksum+prod*coef(j,k)
         pk1sum=pk1sum+prod1*coef(j,k+1)
C
         dksum=dksum+prodp*j*coef(j,k)
         dk1sum=dk1sum+prod1p*j*coef(j,k+1)
      enddo
C
C  evaluate result
C
      ans=dadxbar*pksum + alphabar*dksum +
     >   dadx*pk1sum + alpha*dk1sum
C
      pintdiff=ans
C
 1000 continue
      return
      end
