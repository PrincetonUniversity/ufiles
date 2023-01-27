      subroutine ufsc_lbck(zsdate_i,zsdate,idev_i,idev,
     >     sclab_i,sclab,nsc)
c
c  standardize labels:  length & remove nulls or other unprintable
c    characters
c
      integer nsc
      character*(*) zsdate_i,zsdate,idev_i,idev
      character*30 sclab_i(*),sclab(*)
c
c-------------------------
      character*12 lbck_lbl
c-------------------------
c
      call uf_lbck('shot-date',zsdate_i,zsdate)
      call uf_lbck('device',idev_i,idev)
c
      do i=1,nsc
         write(lbck_lbl,1001) i
 1001    format('sclab(',i5,')')
         call uf_lbck(lbck_lbl,sclab_i(i),sclab(i))
      enddo
c
      return
      end
