      subroutine sget_env(varname,value)
C
C  get the value of an environment variable
C
      implicit none
C
C  input:
      character*(*) varname             ! name to be translated
C  output:
      character*(*) value               ! translation returned
C
C  local:
C
C  value=' ' on exit, if the environment variable (logical name) is
C  undefined.
C
      integer ilvar
C
      ilvar=max(1,len_trim(varname))
C
      value=' '
C
      call getenv(varname(1:ilvar),value)
      call str_pad(value)
      return
      end
