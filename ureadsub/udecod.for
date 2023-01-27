C******************** START FILE udecod.FOR ; GROUP URDUTS *************
c-------------------------------------------------
c&  udecod    unpack uread input line
c
c  rev dmc Jan 1985:  common switch LEVCMD=2:  text
c  mode input, no left-justify
c
c  case conversion is *no longer* done by UREAD system -- only by
c  output conversion routines as needed.
c
      subroutine udecod
c
      USE USERIO
c
      CHARACTER*8 UCNTRL
      character*512 zzbuff	! debug
c
      character*1 cchar
c
c--------------------------------------------------
c
      ilev=levcmd
c
c  text mode input is left justified only if it has
c   UREAD command syntax
c  all other input is *always* left justified...
c
      if(ilev.eq.2) then
         ilas=ilnurd(cbuff)
         if(ilas.gt.0) then
            do ic=1,ilas
               if((cbuff(ic:ic).ne.' ').and.(cbuff(ic:ic).ne.char(9)))
     >            go to 20
            enddo
 20         continue
C   GENERATE SPECIAL CHARACTER STRING
            CALL URCSTR(UCNTRL)
            cchar=cbuff(ic:ic)
            call uupper(cchar)
            if(index(ucntrl,cchar).ne.0) ilev=1
         endif
      endif
c
      READ(CBUFF,1002) (dbuff(ij),ij=1,LBUFF)
 1002 format(512A1)
c
      call ulosup(dbuff,LBUFF,INUM,ilev)
c
      WRITE(CBUFF,1002) (dbuff(ij),ij=1,LBUFF)
      NCHARC=INUM
      NCP1C=INUM+1
c
      return
      end
C******************** END FILE udecod.FOR ; GROUP URDUTS ***************
