      subroutine ufmds_inf(ilun,string,opt,ier)
C
C  get tree information from MDS+ node
C
C---------------------------------
C History:
C 10/23/01 CAL:
C
C------------------------------
      use UFILES
      implicit none
 
      integer       ilun        ! Ufiles LUN
      character*(*) string      ! output
      integer       opt         ! = (-)1 Mds Tree name
C                               ! = (-)2 Node Name
C                               ! if negative: do not return error
                                ! return -77
      integer       ier
C
C
      integer       ichan       ! Ufiles channel id
      integer       ils, l
C-----------------------------------
C
C  CHECK CHANNEL STATUS
C
      CALL UFINIT
      CALL UFLCHK(ILUN,'UFMDS_INF ',ICHAN,IER)
      IF(IER.NE.0) GO TO 9000
C
C MDSplus ?
      if ( UFCPRS(ICHAN) .ne. -1) then
         if (opt .lt. 0) then    ! caller does not know
            ier = -77            ! -77 = not MDS+
            return
         else
            write(ufelun,9001) ilun
 9001       format(' ? UFMDS_INF-- lun',i5,' not open for MDS+')
            go to 9000
         endif
      endif
 
      ier=0
      ils=len(string)
      if (abs(opt) .eq. 1) then      ! Tree
         l=lextnd(ichan)
         if (ils .lt. l) then
         write(ufelun,9002) mds_tree(ichan)(1:l),
     >         mds_tree(ichan)(1:ils)
 9002    format(' ? UFMDS_INF-- Tree Name truncated from ',/,
     >          a,' to ',a)
            l=ils
         endif
         string = mds_tree(ichan)(1:l)
      else if (abs(opt) .eq. 2) then      ! Node
         l=lfdir(ichan)
         if (ils .lt. l) then
         write(ufelun,9003) mds_node(ichan)(1:l),
     >         mds_node(ichan)(1:ils)
 9003    format(' ? UFMDS_INF-- Node Name truncated from ',/,
     >          a,' to ',a)
            l=ils
         endif
         string = mds_node(ichan)(1:l)
      endif
      return
C
 9000 ier=1
      return
      end
 
 
