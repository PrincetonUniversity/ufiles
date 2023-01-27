      subroutine uf_lbck(lbl_name,lbl_i,lbl)
c
c  convert label-as-passed to label-as-used
c  output warnings if label-as-passed contains apparent garbage
c  (non-printable characters)
c
      use UFILES
c
      character*(*) lbl_name,lbl_i,lbl
c---------------------------------------------
c
      ilen_lbl=len(lbl)
      ilen_i=len(lbl_i)
c
      lbl=' '
      do i=1,ilen_i
         ic=ichar(lbl_i(i:i))
         if((ic.lt.32).or.(ic.ge.127)) then
            write(ufelun,1001) lbl_name,lbl(1:max(1,i-1))
 1001       format(' %uf_lbck: non-ascii character in label "',a,'"'/
     >           '  label starts:  "',a,'" (standard fixup applied).')
            exit
         else if(i.le.ilen_lbl) then
            lbl(i:i)=lbl_i(i:i)
         else
            if(ic.ne.32) then
               write(ufelun,1002) lbl_name
 1002          format(' %uf_lbck:  label "',a,'" truncated.'/
     >              '  label starts:  "',a,'".')
               exit
            endif
         endif
      enddo
c
      return
      end
