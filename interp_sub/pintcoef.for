      subroutine pintcoef(x,f,n,coef,nd1,iorder,ifit,ier)
C
C  dmc 8 Oct 1998 -- experimental interpolation scheme
C    pintcoef -- set up interpolation coefficients
C    pintcval -- do interpolations
C
      real x(n),f(n)
C
C  x(n) -- input function absciscae **ASSUMED INCREASING ORDER**
C  f(n) -- input function values
C    n  --  number of points
C
      real coef(nd1,n)
C
C  coef -- output interpolation coefficients
C     about x(k), fint(x) = f(k) + sum[j = 1 to iorder] delx**j * coef(j,k)
C       where delx = x-x(k)
C
C  iorder -- order of fit pieces (2 = quadratic) (3 = cubic) (input)
C  ifit -- number of data points to use on either side of each (x(k),f(k))
C          to use in evaluation e.g. by least squares fit of coef array
C          (input)
C
C  ier  -- output completion code, 0 = normal.
C
C     the interpolation formula using coef...
C     forms discontinuous pieces in each interval [x(k-1),x(k+1)]
C     which are joined smoothly by "blending functions" (see pintcval.for)
C
C-----------------------------
C
      ier=0
      if(iorder.gt.2) then
         write(6,*) '?pintcoef -- order 2 fits only at present...'
         ier=1
         return
      endif
      if(ifit.ne.1) then
         write(6,*) '?pintcoef -- only ifit=1 is allowed at present...'
         ier=2
         return
      endif
      if(n.lt.3) then
         write(6,*) '?pintcoef -- n.ge.3 required...'
         ier=3
         return
      endif
      if(iorder.gt.nd1) then
         write(6,*) '?pintcoef -- nd1.ge.iorder required...'
         ier=4
         return
      endif
C
C  with iorder=1 or 2 and ifit=1, no least squares fitting is needed; the
C  function coefficients can be evaluated directly (the data point
C  (x(k),f(k)) and its two neighbours determine a quadratic uniquely).
C
      if((iorder.le.2).and.(ifit.eq.1)) then
         do i=1,n
            if(i.eq.1) then
               dxa=x(3)-x(1)
               dfa=f(3)-f(1)
            else
               dxa=x(i-1)-x(i)
               dfa=f(i-1)-f(i)
            endif
            if(i.eq.n) then
               dxb=x(n-2)-x(n)
               dfb=f(n-2)-f(n)
            else
               dxb=x(i+1)-x(i)
               dfb=f(i+1)-f(i)
            endif
            det=(dxa*dxb)*(dxa-dxb)
            zanum=dxb*dfa-dxa*dfb
            zbnum=dxa*dxa*dfb-dxb*dxb*dfa
            coef(1,i)=zbnum/det
            coef(2,i)=zanum/det
         enddo
      endif
C
      return
      end
