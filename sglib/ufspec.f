	subroutine ufspec(fspec,fpref,fextn,fdisk,farea,fshot,jshot)
	character fspec*48,fpref*24,fextn*4,fdisk*24,farea*24,fshot*5
	logical first /.true./
	if(first) then
	  open(1,status='scratch',iostat=ios)
	  call ufemsg(0)
	  call ufimsg(-1)
	  first = .false.
	endif
	fpref=' '
	fextn=' '
	ixd=index(fspec,':')
	fdisk=fspec(1:ixd)
	ixd=ixd+1
	farea=' '
	if(fspec(ixd:ixd).eq.'[') then
	  ixa=index(fspec,']')
	  farea=fspec(ixd+1:ixa-1)
	  ixd=ixa+1
	endif
	ixp=index(fspec(ixd:),'.')+ixd-1
	fshot=fspec(ixp-5:ixp-1)
	fpref=fspec(ixd:ixp-6)
	fextn=fspec(ixp+1:)
	read(fshot,'(i10)',iostat=ios) jshot
	end
