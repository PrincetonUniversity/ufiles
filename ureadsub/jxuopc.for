C******************** START FILE JXUOPC.FOR ; GROUP URDUTS ******************
C-----------------------------------------------------------
C& JXUOPC UREAD INTERNAL INTEGER FCN CHECK FOR CHAR IN STRING
C
      INTEGER FUNCTION JXUOPC(ZSTRNG,ZCHAR)
C
      implicit NONE
      CHARACTER*(*), intent(in) :: ZSTRNG  ! STRING
      CHARACTER*1, intent(in) :: ZCHAR     ! CHARACTER BEING CHECKED
C
      integer icharl,ic,ichar
      character*1 zzchar,zzzchar
C
      ICHARL=INDEX(ZSTRNG,'$')-1
      IF(ICHARL.LE.0) ICHARL=LEN(ZSTRNG)
      do
         if(icharl.eq.1) exit
         if((zstrng(icharl:icharl).ne.' ').and.
     >      (zstrng(icharl:icharl).ne.char(9))) exit
         icharl=icharl-1
      enddo

      zzzchar=zchar
      call uspcap(zzzchar)

      ICHAR=0
      do ic=1,icharl
         zzchar=zstrng(ic:ic)
         call uspcap(zzchar)
         if(zzchar.eq.zzzchar) then
            ICHAR=ic
            exit
         endif
      enddo
C
      JXUOPC=ICHAR
C
      RETURN
      END
C******************** END FILE JXUOPC.FOR ; GROUP URDUTS ******************
