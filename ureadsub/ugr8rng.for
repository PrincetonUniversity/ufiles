! 05may1999 fgtok -impno -s r8_precision.sub foo.sub "r8con.csh conversion"
      subroutine ugr8rng(zminv,zmaxv,prompt,zvalue)
C
C  prompt for a value -- enforce range constraints
C
C    zminv.le.zvalue  &  zvalue.le.zmaxv
C
      IMPLICIT NONE
C============
C idecl:  explicitize implicit INTEGER declarations:
      INTEGER lnout
C============
C idecl:  explicitize implicit REAL declarations:
      REAL*8 zlocl
C============
      REAL*8 zminv,zmaxv                  ! range limits (input)
      character*(*) prompt              ! Uread prompt (input)
      REAL*8 zvalue                       ! value returned (default is input)
C
C---------------------------------------
C
 10   continue
      zlocl=zvalue
      call ureadr8(prompt,zlocl)
      if((zlocl.lt.zminv).or.(zlocl.gt.zmaxv)) then
         write(lnout(0),1001) zlocl,zminv,zmaxv
 1001    format(' %ugetrng:  the input value ',1pd18.11/
     >      '  is not in range ',1pd18.11,' to ',1pd18.11,'.')
         call uclear
         go to 10
      endif
C
      zvalue=zlocl
C
      return
      end
