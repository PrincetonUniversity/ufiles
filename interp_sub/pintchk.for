      subroutine pintchk(xwant,ihint,x,ngot,nd1,iorder,ibase,ixtrap,
     >   ixflag)
C
C  perform error check and find zone containing xwant, for pintcval and
C  any related routines
C
C  the arguments are a subset of the arguments of subroutine pintcval
C  see pintcval comments for description of arguments.
C
      real x(ngot)
C
C------------------------------
C
      ixflag=0
      if(iorder.gt.nd1) then
         write(6,*) '?pintchk:  order exceeds coefficient array dim.'
         write(6,*) iorder,nd1
         ixflag=-99
      endif
      if((ibase.lt.1).or.(ibase.gt.3)) then
         write(6,*) '?pintchk:  ibase control value out of range'
         write(6,*) ibase
         ixflag=-99
      endif
      if(ixflag.eq.-99) return
C
      if(xwant.lt.x(1)) then
         ixflag=-1
         ihint=0
         return
      endif
C
      if(xwant.gt.x(ngot)) then
         ixflag=1
         ihint=ngot
         return
      endif
C
C  OK xwant is in range
C
      imax=(ngot-1)
      imin=(1)
      ihint=max(imin,min(imax,ihint))
C
 5    continue
      if(xwant.lt.x(ihint)) go to 10
      if(xwant.gt.x(ihint+1)) go to 20
C
C  already have correct zone
C
      go to 100
C
C  look down one zone, or binary search
C
 10   continue
      if(xwant.ge.x(ihint-1)) then
         ihint=ihint-1
         go to 90
      else
         imax=ihint-1
         xi=(xwant-x(imin))/(x(imax)-x(imin))
         ihint=imin+xi*(imax-imin)
         if((ihint.lt.imin).or.(ihint.gt.imax)) go to 99
         go to 5
      endif
C
C  look up one zone, or binary search
C
 20   continue
      if(xwant.le.x(ihint+2)) then
         ihint=ihint+1
         go to 90
      else
         imin=ihint+2
         xi=(xwant-x(imin))/(x(imax)-x(imin))
         ihint=imin+xi*(imax-imin)
         if((ihint.lt.imin).or.(ihint.gt.imax)) go to 99
         go to 5
      endif
C
C  check range
C
 90   continue
      if((ihint.lt.imin).or.(ihint.gt.imax)) go to 99
C  OK
      go to 100
C
 99   continue
C
C  apparent error
C
      ixflag=-99
      write(6,*) '?pintchk:  search failure, probably x(k) out of order'
C
C  done
C
 100  continue
      return
      end
 
