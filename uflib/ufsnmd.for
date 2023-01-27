C******************** START FILE UFSNMD.FOR ; GROUP UFILES ******************
C------------------------------------------------------------
C  UFSNMD
C  DISPLAY UFILES FILENAME FORM ON OUTPUT CHANNEL LUNO
C  WITH LABEL ZLABL (20 CHARS)
C    UFILES FILE I/O LUN LUNF
C
      SUBROUTINE UFSNMD(LUNO,ZLABL,LUNF)
C
      USE UFILES
C
      CHARACTER*(*) ZLABL
      CHARACTER*128 ZFILE
C
      character*128 znode
C
      CALL UFINIT
      IER=0
      CALL UFLCHK(LUNF,'UFSNMD    ',ICHAN,IER)
      IF(IER.NE.0) RETURN
C
      if(ufcprs(ichan).ge.0) then
         IF(LSHOTN(ICHAN)) THEN
            CALL UFSNM0(ZFILE,
     1	   UDEV(ICHAN),UFDIR(ICHAN),UPRFIX(ICHAN),
     2	   '######',UEXTND(ICHAN))
         ELSE
            CALL UFSNM0(ZFILE,
     1	   UDEV(ICHAN),UFDIR(ICHAN),UPRFIX(ICHAN),
     2	   '      ',UEXTND(ICHAN))
         ENDIF
      endif
C  DISPLAY - IGNORE TRAILING, LEADING BLANKS IN LABEL
      ILL=LEN(ZLABL)
C
      DO 11 IL1=1,ILL
         IF(ZLABL(IL1:IL1).NE.' ') GO TO 15
 11   CONTINUE
C
      IL1=ILL
      IL2=ILL
      GO TO 16
C
 15   CONTINUE
C
      DO 13 IL2=ILL,IL1,-1
         IF(ZLABL(IL2:IL2).NE.' ') GO TO 16
 13   CONTINUE
C
 16   CONTINUE
C
      if(ufcprs(ichan).ge.0) then
         CALL UFSND0(LUNO,ZLABL(IL1:IL2),ZFILE)
      else
         call ufsnmd_node(ichan,znode,ilnode)
         ils=len_trim(mds_server(ichan))
         ilt=len_trim(mds_tree(ichan))
         write(luno,1001) mds_server(ichan)(1:ils),
     1	mds_tree(ichan)(1:ilt),znode(1:ilnode)
 1001    format(' mds+ "',a,'" at ',a,' tree: ',a/' path: ',a)
      endif
C
      RETURN
      END
C-------------------
      subroutine ufsnmd_node(ichan,znode,ilnode)
      use UFILES
      character*(*) znode
C
      call ufsnmd_nod0(mds_path(ichan),mds_node(ichan),znode,ilnode)
      return
      end
C
C------------------
C
      subroutine ufsnmd_nod0(mds_path,mds_node,znode,ilnode)
      character*(*) mds_path,mds_node	! path & node strings (input)
      character*(*) znode	! combined string (output)
      integer ilnode		! non-blank string length (output)
C
      if(mds_node(1:1).eq.' ') then
         znode=mds_path
      else if(mds_path(1:1).eq.' ') then
         znode=mds_node
      else
         ilp=len_trim(mds_path)
         iln=len_trim(mds_node)
         icolon=1
         if(mds_path(ilp:ilp).eq.'.') icolon=0
         if(mds_path(ilp:ilp).eq.':') icolon=0
         if(mds_node(1:1).eq.'.') icolon=0
         if(mds_node(1:1).eq.':') icolon=0
         if(icolon.eq.1) then
            znode = mds_path(1:ilp)//':'//mds_node(1:iln)
         else
            znode = mds_path(1:ilp)//mds_node(1:iln)
         endif
      endif
      ilnode = len_trim(znode)
      return
      end
