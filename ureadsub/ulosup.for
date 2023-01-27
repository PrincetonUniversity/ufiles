C******************** START FILE ulosup.FOR ; GROUP URDUTS ******************
c--------------------------------------------------------------
c&  ulosup  uread left justify
c
c  *** case conversion suppressed ***
c      case conversion deferred to output translation routines
c      dmc Mar 2004
c
C  REV-- JUL 81.  ALSO DROP LEADING BLANKS FROM LINE
C  RETURN LENGTH INUM OF STRING NOT INCL. TRAILING BLANKS AFTER
C  LEFT-JUSTIFICATION
c
C  REV DMC MAY 1986 - CONVERT TABS TO BLANKS
C
C  REV CAL July 97 -- need also to left justif urdfcap
c
      subroutine ulosup(zbuff,INBUF,INUM,ILEVCMD)
C
      use USERIO
C
      character*1 zbuff(INBUF)
      character*1 zupper(27),zlower(27)
      data zlower/'a','b','c','d','e','f','g',
     >         'h','i','j','k','l','m','n','o',
     >         'p','q','r','s','t','u','v',
     >         'w','x','y','z','	'/  ! tab char
      data zupper/'A','B','C','D','E','F','G',
     >         'H','I','J','K','L','M','N','O',
     >         'P','Q','R','S','T','U','V',
     >         'W','X','Y','Z',' '/
c------------------------------------------------------------------
c  executable code:
c
      zlower(27)=char(9)	! make sure it has a tab
c
      if(ilevcmd.gt.1) go to 50
c
      inblnk=0
      ict=0
C
      if(ilevcmd.eq.0) ilclim=27	! tab conversion only
      if(ilevcmd.eq.1) ilclim=27	! *always*
C
c  do not revive the old way-- other things have changed...
cccccccccc      if(ilevcmd.eq.1) ilclim=1	! uppercase and tab conversion
C
      IUP=1
      do 10 i=1,INBUF
        if((zbuff(i).ne.' ').AND.(zbuff(i).ne.char(9))) inblnk=1
        if(inblnk.eq.0) go to 10
        ict=ict+1
        zbuff(ict)=zbuff(i)
        IF(ZBUFF(ICT).EQ.'"') IUP=1-IUP
        IF(IUP.EQ.1) THEN
          ilp=ilclim
          do 9 j=ilp,27
            if(zbuff(ict).eq.zlower(j)) zbuff(ict)=zupper(j)
 9        continue
        ENDIF
c
 10   continue
c
c  fill up remainder of lines with blanks if necessary
      if(ict.ge.INBUF) GO TO 50
      icp1=ict+1
      do 20 i=icp1,INBUF
        zbuff(i)=' '
 20   continue
c
 50   CONTINUE
c
      DO 60 I=INBUF,1,-1
        INUM=I
        IF(ZBUFF(INUM).NE.' ') GO TO 70
 60   CONTINUE
      INUM=0
 70   CONTINUE
c
      RETURN
      end
C******************** END FILE ulosup.FOR ; GROUP URDUTS ******************
