! 05may1999 fgtok -impno -s r8_precision.sub foo.sub "r8con.csh conversion"
      subroutine ugr8pair(zminv,zmaxv,prompt1,prompt2,zval1,zval2,iflag)
C
C  get a pair of numbers, which may have to be distinct.  Both numbers
C  must be in the range (zminv,zmaxv)
C
      IMPLICIT NONE
C============
C idecl:  explicitize implicit INTEGER declarations:
      INTEGER lnout
C============
C idecl:  explicitize implicit REAL declarations:
      REAL*8 zloc1,zloc2
C============
      REAL*8 zminv,zmaxv                  ! validity range (input)
      character*(*) prompt1, prompt2    ! prompts for zval1 & zval2 (input)
      REAL*8 zval1,zval2                  ! values returned (output, default in)
      integer iflag                     ! processing code...
C
C  iflag= 0 -- just get two numbers
C  iflag= 1 -- min/max pair.  require zval1.ne.zval2 and
C     enforce zval1.lt.zval2
C
C--------------------------------------
C
 10   continue
      zloc1=zval1
      zloc2=zval2
      call ugr8rng(zminv,zmaxv,prompt1,zloc1)
      call ugr8rng(zminv,zmaxv,prompt2,zloc2)
C
      if(iflag.eq.1) then
C
C  enforce min/max rules...
C
         if(zloc1.eq.zloc2) then
            write(lnout(0),1001) zloc1
 1001       format(' %ugetpair:  both values entered as:  ',1pd18.11/
     >         '  distinct values must be entered.')
            call uclear
            go to 10
         endif
         zval1=min(zloc1,zloc2)
         zval2=max(zloc1,zloc2)
      else
C
C  default rule
C
         zval1=zloc1
         zval2=zloc2
      endif
C
      return
      end
