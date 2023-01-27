      subroutine lmds_errstat
 
      write(6,*) ' ?dummy "lmds_errstat" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end

      subroutine mds_setserver
 
      write(6,*) ' ?dummy "mds_setserver" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end

      subroutine idescr_cstring
 
      write(6,*) ' ?dummy "idescr_cstring" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end

      subroutine idescr_cstringarr
 
      write(6,*) ' ?dummy "idescr_cstringarr" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end
 
      subroutine idescr_long
 
      write(6,*) ' ?dummy "idescr_long" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end
 
      subroutine idescr_float
 
      write(6,*) ' ?dummy "idescr_foat called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end
      subroutine idescr_floatarr
 
      write(6,*) ' ?dummy "idescr_foatarr" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end
 
      subroutine idescr_doublearr
 
      write(6,*) ' ?dummy "idescr_doublearr" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end
 
      subroutine idescr_longarr
 
      write(6,*) ' ?dummy "idescr_longarr" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
 
      return
      end

      integer function mds_create_node(node, usage)
      implicit none
      character*(*) node, usage
      write(6,*) ' ?dummy "mds_create_node" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
      mds_create_node=0
      return
      end

      integer function mds_errstr(ierr, str)
      implicit none
      integer ierr
      character*(*) str
      write(6,*) ' ?dummy "mds_errstr" called.'
      write(6,*) '  software was not properly linked to mdstransp.'
      mds_errstr=0
      return
      end

      integer function mds_add_node(node, usage, stat)
      implicit none
      character*(*) node, usage
      integer stat
      write(6,*) ' ?dummy "mds_add_node" called'
      write(6,*) '  software was not properly linked to mdstransp.'
      mds_add_node = -1
      return
      end

      subroutine mdsCacheUnConnect

      write(6,*) ' ?mdsCacheUnConnect: dummy call '
      return

      end subroutine mdsCacheUnConnect

      subroutine mdsCacheDisConnect

      write(6,*) ' ?mdsCacheDixConnect: dummy call '
      return

      end subroutine mdsCacheDisConnect
