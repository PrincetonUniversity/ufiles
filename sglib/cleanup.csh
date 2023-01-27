#/bin/csh
#
# dmc -- cleanup script for sglib area (3 Aug 1993)
#
  if ( $#argv == 0 ) then
    rm *.o *.a 
    csh cleanup.csh tstp10 
    csh cleanup.csh xplot 
  else
    if ( -f $1 ) then
      rm $1
    endif
  endif
