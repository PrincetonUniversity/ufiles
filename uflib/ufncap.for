      subroutine ufncap(icap)
C
C  set default capitalization switch for Ufile filenames
C
      use UFILES
C
      integer icap  ! =1 for capitalized names, =0 for lower case names
C
C---------------
C
      call ufinit
      if(icap.eq.1) then
         LUFCAP=1
      else if(icap.eq.0) then
         LUFCAP=0
      else if(icap.eq.2) then
         LUFCAP=2
      else
         write(6,1001) icap
 1001    format(' ?ufncap:  invalid argument value icap=',i6)
      endif
C
      return
      end
