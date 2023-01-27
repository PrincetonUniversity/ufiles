	program test
	character*20 dsknam
	integer ldnam
	data ldnam /0/
	call usr$trnlog(' ','PLOT',dsknam,ldnam,igood)
	if(ldnam.eq.0) dsknam='stdout'//char(0)
!	type '(a)', dsknam
	call initt(240)
	call movabs(100,100)
	call drwabs(600,100)
	call drwabs(600,600)
	call drwabs(100,600)
	call drwabs(100,100)
	call movabs(350,650)
	call anstr(3,'box')
	call upause(0)
	call finitt(0)
	end
