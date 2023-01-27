      subroutine ugetrng(zminv,zmaxv,prompt,zvalue)
C
C  prompt for a value -- enforce range constraints
C
C    zminv.le.zvalue  &  zvalue.le.zmaxv
C
      real zminv,zmaxv                  ! range limits (input)
      character*(*) prompt              ! Uread prompt (input)
      real zvalue                       ! value returned (default is input)
C
C---------------------------------------
C
 10   continue
      zlocl=zvalue
      call ureadf(prompt,zlocl)
      if((zlocl.lt.zminv).or.(zlocl.gt.zmaxv)) then
         write(lnout(0),1001) zlocl,zminv,zmaxv
 1001    format(' %ugetrng:  the input value ',1pe13.6/
     >      '  is not in range ',1pe13.6,' to ',1pe13.6,'.')
         call uclear
         go to 10
      endif
C
      zvalue=zlocl
C
      return
      end
