c========================================================================
c
      subroutine rtoi(linlog, nbits, lauto, cmin, cmax, nwords, chan
     >	    ,scale, offset, ichan, ierr)
c
c routine to convert an array of real*4 words into integer*2 words of
c    given bit significance
c  the transformation is of the form:
c	r = s * i + o	(linear), or
c	r = s * 10**i + o	(log)
c  where s is the scale factor, i is the integer datum, o is the offset,
c    and r is the resultant real number.
c
c*15-nov-83 ---rtoi.for, itor.for---  r. wieland
c*14-aug-89  m. thompson: renamed rtoi->rtoi,etc.
c
c  arguments:
c  =========
c  linlog	(input) integer flag
c		0 for linear transformation
c		1 for log transformation
c  nbits	(input) number of significant bits in integer i
c  lauto	(input) auto scale flag
c		0 to allow rtoi to find min/max of the data
c		1 to make rtoi use the values cmin/cmax for the range
c  cmin	(input) lower limit of data - if lauto=1
c  cmax	(input) upper limit of data - if lauto=1
c  nwords	(input) length of input array
c  chan	(input) array of real*4 data items [r]
c		(output) changed if any points lie outside the auto scale
c		range provided
c  ichan	(output) array of integer*2 data [i]
c  nclip	(output) number of values clipped
c  scale	(output) resulting scale factor [s]
c  offset	(output) resulting offset [o]
c  ierr		(output) error flag
c		0 if everything ok
c		3 if data outside of (-1.e36,+1.e36)
c
c-----------------------------------------------------------------------
c
      integer linlog, nbits, lauto
      real*4  cmin, cmax
      real*4  chan(*)
      real*4  scale, offset
      integer*2 ichan(*)
      integer   nclip
      integer ierr
c
c------------------------------------------------------------------------
c
      lclip = lauto
      go to 100
c
c----------------------------------------------------------------------
c
      entry rtoi_c (linlog, nbits, nclip, cmin, cmax, nwords, chan
     >	    ,scale, offset, ichan, ierr)
c
c----------------------------------------------------------------------
c
      lclip = -1
 100  ierr = 0
      nscale = 2**nbits - 1
      scale = 1.
      offset = 0.
      if(nwords.eq.0) go to 8000
c
c  find range
c
      if(lclip.ne.0) then
c				cmin/cmax provided - clip data
        ncl = 0
        dmin = cmax
        dmax = cmin
        do ii=1,nwords
          if(chan(ii).gt.cmax .or. chan(ii).lt.cmin) ncl = ncl+1
          chan(ii) = min(chan(ii),cmax)
          chan(ii) = max(chan(ii),cmin)
          dmin = min(dmin,chan(ii))
          dmax = max(dmax,chan(ii))
        enddo
        if(lclip.lt.0) nclip = ncl
      else
c				calculate dmin/dmax
        dmin = chan(1)
        dmax = dmin
        do ii=2,nwords
          dmax = max(dmax,chan(ii))
          dmin = min(dmin,chan(ii))
        enddo
        if(dmin.eq.0 .and. dmax.eq.0) go to 400
      endif
c
      if(abs(dmax).gt.1.e+36) then
cx     >	.or. (abs(dmin).gt.0. .and. abs(dmin).lt.1.e-36)) then
        ierr = 3
        return
      endif
c
c  set scale and offset
c
      if(linlog.eq.0) then
c				linear
        offset = dmin
        range = dmax-dmin
        scale = range/float(nscale)
      else
c				log
        if(dmin.gt.0.) then
          offset = 0.
        elseif(dmin.eq.0.) then
          offset = -1.
        else
          offset = dmin*10.
        endif
        range = log(dmax-offset)
        scale = range/float(nscale)
      endif
c
c  convert data
c
 400  continue
      if(scale.ne.0.) then
        scin = 1./scale
      else
        scin = 1.
      endif
c
      if(linlog.eq.0) then
        do ii=1,nwords
          ichan(ii) = (chan(ii)-offset) * scin
        enddo
      else
        do ii=1,nwords
          ichan(ii) = log(chan(ii)-offset) * scin
        enddo
      endif
c
 8000 return
      end
c
c========================================================================
c
      subroutine itor(linlog, ichan, nwords, scale, offset, chan, ierr)
c
c  takes an array of integer*2 words and converts them to an array of
c    real*4 words, using the transformation given for rtoi.
c
c  arguments:
c  =========
c  linlog	(input) integer flag
c		0 for linear transformation
c		1 for log transformation
c  nwords	(input) length of input array
c  ichan	(input) array of integer*4 data [i]
c  scale	(input) resulting scale factor [s]
c  offset	(input) resulting offset [o]
c  chan		(output) array of real*4 data items [r]
c  ierr		(output) error flag
c		0 if everything ok
c
c-----------------------------------------------------------------------
c
      integer linlog
      integer*2 ichan(*)
      real    scale, offset
      real	chan(*)
c
      real	aln10
c
      data aln10/2.302585/
c
c-----------------------------------------------------------------------
c
      ierr=0
c
c  convert from integer to real
c
      if(linlog.eq.0) then
        do ii=1,nwords
          chan(ii) = scale*ichan(ii) + offset
        enddo
c
      else
        do ii=1,nwords
          chan(ii) = exp(scale*ichan(ii)) + offset
        enddo
      endif
c
      return
      end
c
c========================================================================
c
      subroutine cmprsi(nbits, nitems, pack, items, bit, maxp, ierr)
c
c+an algorithm to compress waveforms by expressing them
c as differences and storing only the number of bits needed
c to express the values over a power of two range.
c look ahead is limited to two levels but
c rescan is restarted with each output
c
c nbits	  number of significant bits in each item
c	  negative for sign extension on expansion
c pack	  output packed array
c items	  input packed array
c nitems  number of values to be packed
c bit	  starting left-to-right bit number in pack
c	  returns last occupied (starts at 0)
c	  if 0 on entry, pack is zeroed; note values are ored into pack
c maxp	  dimension of pack
c ierr	  returned error code (0 = ok)
c	  16 if  nbits > 16 or nbits = 0
c	  15 if  unpacked value too large to store
c	  14 if  dimension of pack too small
c
c*25-mar-83 ---cmprsi.for, xpandi.for---
c*16-nov-83  m. thompson (ppl) - return error if nbits > 16 rather than stop
c	check dimension of pack
c*24-jan-84  m. thompson (ppl) - store count word in 16 bits vs 12
c	check for data values too large to store
c*18-jan-93  m. thompson (ppl) - rearrange logic in cmprsi
c*26-feb-92  m. thompson (ppl) - replace macro packi/unpki with in-line
c	code using shifts and masks; increment bit size by 2 (vs 1)
c
c-----------------------------------------------------------------------
c

      implicit integer(a-z)
c
      parameter (shift=16)	!was 12 (2/93, met)
      parameter (two=2*shift,ko256=shift+256)
c
      integer pack(*)
      integer*2 items(*)
      integer*4 iold,it
c
      integer power(17)
      integer sig(65)
      integer*2 is(ko256)
c
      integer*4 masks(0:16)
c
      data power/
     >    0,1,2,4,8,16,32,64,128,256,512,
     >    1024,2048,4096,8192,16384,32768/
      data sig/
     >    0,2,2,2*4,4*4,8*6,16*6,32*7/
#if __F90
      data masks/ Z"00000000",
     >	  Z"00000001",Z"00000003",Z"00000007",Z"0000000f",
     >	  Z"0000001f",Z"0000003f",Z"0000007f",Z"000000ff",
     >	  Z"000001ff",Z"000003ff",Z"000007ff",Z"00000fff",
     >	  Z"00001fff",Z"00003fff",Z"00007fff",Z"0000ffff"/
#else
      data masks/ '00000000'x,
     >	  '00000001'x,'00000003'x,'00000007'x,'0000000f'x,
     >	  '0000001f'x,'0000003f'x,'0000007f'x,'000000ff'x,
     >	  '000001ff'x,'000003ff'x,'000007ff'x,'00000fff'x,
     >	  '00001fff'x,'00003fff'x,'00007fff'x,'0000ffff'x/
#endif
c-----------------------------------------------------------------------
c
      nb=abs(nbits)
      if(nb.gt.16 .or. nb.eq.0) then
       ierr = 16
       go to 90
      endif
c
      if(bit.eq.0) then
        do ii=1,maxp
          pack(ii)=0
        enddo
      endif
c first difference to remove most of value
      offset=power(nb+1)
      mask=2*offset-1
      imask=mask
      if(nbits.lt.0) imask=imask/2
      x0=0
      iold=0
      kptr=bit/32+1
      kpos=mod(bit,32)
      kpos=32-kpos
c loop 1 to nitems+1, limited to 256 items
      do while (x0.lt.nitems)
        nn = min(nitems-x0,ko256)
        y1=-1		!number of bits
        y2=-1
        y12=0
        x2=0		!count at this bit size
        do x3=0,nn
          if(x3.eq.nn) then
c hit the count wall
            y3=64
            go to 7
          endif
c significance
          it=items(x3+x0+1)
          if(abs(it).gt.imask) then
            ierr=15
            go to 90
          endif
          i=iand(it-iold+offset,mask)-offset
          iold=it
          is(x3+1)=i
          if(i.le.0) i=not(i)
          if(i.lt.64) then
c speed up for small differences
            y3=sig(i+2)
          else
            y3=8
            do while (y3.le.nb .and. i.ge.power(y3+1))
      	y3=y3+2	!was +1 (2/93, met)
            enddo
          endif
 7        y32=y3-y2
          if(y32.eq.0) go to 20	!cycle
          if(y12.eq.0) go to 18
          if(y12.lt.0) then
            if(y32.gt.0) then
c rise
      	x32=min(x3-x2,shift)
      	x20=min(x2,shift)
c low mid high --> mid high, low high
      	if(x20*y12+x32*y32.lt.0) then
      	  y2=y3
      	  y12=y1-y2
      	endif
            endif
c output if good enough
            if(y12*x2+shift.lt.0) go to 21
            y1=y2
            go to 18
          endif
c
          if(y32.lt.0) then
c drop
            if(y12*(x3-x2)+y12.gt.two) go to 21
c high mid low --> high low or high mid
            if(y12*(x3-x2)+y32.ge.0) go to 20
          else	!y32.gt.0
c dip
            if(y32.gt.y12) y32=y12
            y32=(x3-x2)*y32
            if(y32.gt.two) go to 21
            if(y32.gt.shift .and. x3.gt.256) go to 21
c high low mid --> high mid
c mid low high --> mid high
            if(y3-y1.lt.0) go to 19
          endif
 18       x2=x3
 19       y2=y3
          y12=y1-y2
 20     enddo
c
        if(y1.lt.0) then
          y1=y2
          x2=nn
        endif
 21     x2=min(x2,256)
        if((y1*x2+bit)/32.gt.maxp) then
          ierr=14
          go to 90
        endif
        if(y1.gt.16) then
          ierr=13
          go to 90
        endif
        ival=x2-1		!low order byte = count (max 255)
        ival=ior(ival,ishft(y1,8))	!high order byte = size
        ipak=pack(kptr)
        kpos=kpos-16
        itmp=0
        if(kpos.lt.0) itmp=ishft(ival,kpos+32)	!shift left
        if(kpos.ne.0) ival=ishft(ival,kpos)	!+=left, -=right
        if(kpos.gt.0) then
          ipak=ior(ipak,ival)
        else
          pack(kptr)=ior(ipak,ival)
          kptr=kptr+1
          ipak=itmp
          kpos=kpos+32
        endif
        pack(kptr)=ipak
        bit=bit+16
        if(y1.eq.0) go to 80
        kbits=y1
        do k=1,x2	!n=x2
          ival=is(k)
          ival=iand(ival,masks(kbits))	!drop sign extension
          kpos=kpos-kbits
          itmp=0
          if(kpos.lt.0) itmp=ishft(ival,kpos+32)	!shift left
          if(kpos.ne.0) ival=ishft(ival,kpos)	!+=left, -=right
          if(kpos.gt.0) then
            ipak=ior(ipak,ival)
          else
            pack(kptr)=ior(ipak,ival)
            kptr=kptr+1
            ipak=itmp
            kpos=kpos+32
          endif
          pack(kptr)=ipak
        enddo
        bit=bit+kbits*x2
 80     continue
        x0=x0+x2
        iold=items(x0)
      enddo
      ierr=0
c
 90   return
      end
c
      subroutine xpandi(nbits, pack, start, unpack, first, last, ierr)
c
c+expand compressed data, inverse of cmprsi
c
c nbits	  is number of bits maximum per item
c pack	  is packed array
c start	  is starting bit (0,...) in pack
c unpack  is unpacked array
c first	  is index(1,n) of item to place in unpack(1)
c last	  is index of last item placed in unpack
c ierr	  is returned error code (0=ok)
c	  16 if  bits > 16 or bits = 0
c	  13 if  invalid data (internal nbits > 16)
c
c=======================================================================
c
      implicit integer(a-z)
c
      integer*2 unpack(*)
      integer pack(*)
c
      integer power(18)
c
      integer*4 masks(0:16)
c
      data power/
     >	  0,1,2,4,8,16,32,64,128,256,512,
     >    1024,2048,4096,8192,16384,32768,65536/
c
#if __F90
      data masks/ Z"00000000",
     >	  Z"00000001",Z"00000003",Z"00000007",Z"0000000f",
     >	  Z"0000001f",Z"0000003f",Z"0000007f",Z"000000ff",
     >	  Z"000001ff",Z"000003ff",Z"000007ff",Z"00000fff",
     >	  Z"00001fff",Z"00003fff",Z"00007fff",Z"0000ffff"/
#else
      data masks/ '00000000'x,
     >	  '00000001'x,'00000003'x,'00000007'x,'0000000f'x,
     >	  '0000001f'x,'0000003f'x,'0000007f'x,'000000ff'x,
     >	  '000001ff'x,'000003ff'x,'000007ff'x,'00000fff'x,
     >	  '00001fff'x,'00003fff'x,'00007fff'x,'0000ffff'x/
#endif
c-----------------------------------------------------------------------
c
      lowmask=masks(8)
c
      bit=start
      nb=abs(nbits)
      if(nb.gt.16 .or. nb.eq.0) then
        ierr=16
        go to 90
      endif
      index=0
      ii=0
      value=0
      lower=0
      modulo=power(nb+2)
      if(nbits.lt.0) lower=-power(nb+1)
      upper=lower+modulo-1
      ierr=0
      do while(index.lt.last)
C				!get next 16 bits
        kptr=bit/32+1		!ptr into pack
        kpos=mod(bit,32)	!offset in word
        kpos=kpos+16
        if(kpos.lt.32) then
          ival=ishft(pack(kptr),-(32-kpos))	!shift right
          ival=iand(ival,masks(16))
        else
          kpos=kpos-32
          ival=iand(pack(kptr),masks(16-kpos))
          kptr=kptr+1
          if(kpos.gt.0) then
            ntx=ishft(pack(kptr),-(32-kpos))	!shift right
            ntx=iand(ntx,masks(kpos))
            ival=ior(ishft(ival,kpos),ntx)	!shift left
          endif
        endif
        bit=bit+16
        bits=ishft(ival,-8)		!high order byte = size
        bits=-bits
        n=iand(ival,lowmask)+1		!low order byte = count
        n=min(n,last-index)
        if(bits.gt.16) then
          ierr=13
          return
        endif
        if(bits.eq.0) then
          do k=1,n
          index=index+1
          if(index.ge.first) then
            ii=ii+1
            unpack(ii)=value
          endif
          enddo
          go to 80
        endif
        kbits=abs(bits)
        do k=1,n
          kpos=kpos+kbits
          if(kpos.lt.32) then
            ival=ishft(pack(kptr),-(32-kpos))	!shift right
            ival=iand(ival,masks(kbits))
          else
            kpos=kpos-32
            ival=iand(pack(kptr),masks(kbits-kpos))
            kptr=kptr+1
            if(kpos.gt.0) then
      	ntx=ishft(pack(kptr),-(32-kpos))	!shift right
      	ntx=iand(ntx,masks(kpos))
      	ival=ior(ishft(ival,kpos),ntx)	!shift left
            endif
          endif
cx	    if(bits.lt.0) then	!bits is always negative
            imask=not(masks(kbits-1))		!extend sign bit
            if(iand(imask,ival).ne.0) ival=ior(ival,imask)
cx	    endif
          value=value+ival
          if(value.lt.lower) value=value+modulo
          if(value.gt.upper) value=value-modulo
          index=index+1
          if(index.ge.first) then
            ii=ii+1
            unpack(ii)=iand(value,masks(16))
          endif
        enddo
        bit=bit+kbits*n
 80   continue
      enddo
c
 90   return
      end
