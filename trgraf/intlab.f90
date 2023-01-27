module intlab
  !---------------------------------------------------
  !  DECLARE 17 CHAR FCN INTEGRAL OR DIFFERIENTIATION LABELS FOR
  !  TRANSP PLOTTING
  !
  !  USED BY VARIOUS PLOTTING ROUTINES
  !

  ! LONG LABELS ...
  CHARACTER(len=17), dimension(16), parameter :: ZLINT = &
       (/' TIME DERIVATIVE ', &
       '  SCALE LENGTH   ', &
       '1/(SCALE LENGTH) ', &
       '    GRADIENT     ', &
       '                 ', &
       'VOLUME INTEGRAL  ', &
       'FLUX INTEGRAL    ', &
       'AREA INTEGRAL    ', &
       'LINE AVERAGE     ', &
       'VOLUME AVERAGE   ', &
       'RMS. VOL. AVERAGE', &
       'DBL INV LINE AVG ', &
       'DBL INV VOL AVG  ', &
       'CTR RENORMALIZED ', &
       'MAX RENORMALIZED ', &
       'H(R) RENORMALIZED'/)

  ! SHORT LABELS ...
  CHARACTER(LEN=10), dimension(16), parameter :: ZLINT2 = &
       (/'D/DT of   ', &
       'SCALE LEN.','INV.SC.LEN','GRADIENT  ', &
       '          ','VOL. INT. ','FLUX INT. ', &
       'AREA INT. ','LINE AVG  ','VOL. AVG. ', &
       'RMS VOL AV','DI LINE AV','DI VOL AVG', &
       'CTR NORM  ','MAX NORM  ','H(R) NORM '/)

end module intlab
