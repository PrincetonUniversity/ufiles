C******************** START FILE ugbuf0.FOR ; GROUP UREAD5 ******************
c--------------------------------------------------------------
c&  ugbuf0 load uread multi-line buffer from indirect input file
c  read in the buffer
c  if istart=1, reading first page fresh after open.  if entire
c  file fits in buffer set flag itotal
c
      subroutine ugbuf0(istart,zbuff,isiz,ilasl,itotal,iendl)
c
      USE USERIO
      logical itotal
      character*(urdbufl) zbuff(isiz)
c
c  ilasl-- location of last line read
c  iendl-- location of end-of-file or 0 if e-o-f not found
c
      iendl=0
      itotal=.false.
c
      i0=1
      if(ilasl.lt.isiz) i0=ilasl+1
c
      do 50 i=i0,isiz
          read(luntin(nlunin),5001,end=60) zbuff(i)
          call urd_cln_input(zbuff(i))
c
c 	  check for indirect input file symbol-- break
          ind1=index(zbuff(i),'@')
          if(ind1.eq.0) then
c	  mod dmc June 1986 - ">" also an input file symbol if followed
c	  immediately by a non-delimitter character
            ind1=index(zbuff(i),'>')
            if(ind1.gt.0) then
      	ind1p=ind1+1
      	if(index('	 ,',zbuff(i)(ind1p:ind1p)).ne.0) ind1=0
            endif
          endif
          indcom=index(zbuff(i),MSEMI)
          if(indcom.eq.0) indcom=len(zbuff(1))+1
          if((ind1.gt.0).and.(ind1.lt.indcom)) go to 100
 50   continue
c
      istart=2
      ilasl=isiz
      return
c
c e-o-f detected
 60   continue
      ilasl=i-1
      iendl=i
      if(istart.eq.1) itotal=.true.
      return
c
c  indirect file reference break pt detected
 100  continue
      ilasl=i
      return
 5001 format(a)
c
      end
C******************** END FILE ugbuf0.FOR ; GROUP UREAD5 ******************
