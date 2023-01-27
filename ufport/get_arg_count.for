      subroutine get_arg_count(numargs)
C
      integer numargs
C
C  return the number of arguments on the command line
C
      integer iargc

      numargs = iargc()

      return
      end
