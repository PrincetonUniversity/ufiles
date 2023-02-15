      program wiggle
c
c	Dimension Statements for the UFILES programs
c
      parameter (max_x=16384)
      parameter (max_x1=65536)
      parameter (max_y=64)
      real xarray(max_x1), yarray(max_y), work(max_x,2)
      real xarray2(max_x1), yarray2(max_y)
      integer iwork(max_x,2)
      real data2(max_x, max_y), out2(max_x, max_y)
      real sigma2(max_x,max_y,2), sigma_a(max_x1), sigma_b(max_x1)
      real data1(max_x1), out1(max_x1)
      equivalence (data1,data2), (out1,out2)
      equivalence (sigma_a, sigma_b)
      integer info(8), isig(2)
 
      character*30	flab, xlab, ylab, glab, siglab,sxlab,sylab
      character*1	cmd
      character*3	suffix
      CHARACTER*40 ZFILE, Sfile_1, Sfile_2
 
      logical lxout, mode_b, NB, umode_M
 
      parameter (nscmax=50)
      real scval(nscmax),scval2(nscmax)
      character*4 tdev
      character*10 shdate, shtime,shdate2
      character*10 sclab(3,nscmax),sclab2(3,nscmax)
      character*1 ans, ans2, ans_um
 
      logical done
 
      call uinita(0)
c     call random_init(iseed)
 
      mode_w = -1
      mode_c = -1
      mode_t = -1
 
      mode_b = .false.
      NB = .false.
      umode_m = .false.
 
      write(6,*) 'This program reads a UFile (1d or 2d), and writes out'
      write(6,*)
     >'perturbed versions of the data for uncertainty propagation'
      write(6,*)
     >'analysis.  The user is given a menu of options for controlling'
      write(6,*) 'the perturbation algorythm'
      write(6,*) ' '
 
10    call ufnter(1,'INPUT filename              ')
      call ufnter(3,'OUTPUT filename             ')
 
      NB = .false.
 
20    call ureadi('Shot number (<=0 to stop)',ishot)
      if(ishot .le. 0) stop
 
      idim = 2
 
      	call ufoprd(1,ishot,ierr)
 
      if( ierr .eq. 0) then
 
          call ufhdck(1, info, ierr)
      endif
 
      if( ierr .eq. 0 .and. info(1) .eq. 2) then
          CALL UFSNAM(ISHOT,ZFILE,IER,1)
          nmax_x = max_x
          call uf2drd( 1,tdev,shdate,data2,nmax_x,xarray,nmax_x,yarray,
     1	    max_y,iproc, num_x, num_y, 0, flab, xlab, glab,
     2	    nscmax, nsc, scval, sclab, ierr)
      	    call ufclos(1)
 
      else if( ierr .eq. 0 .and. info(1) .eq. 1 ) then
          CALL UFSNAM(ISHOT,ZFILE,IER,1)
          nmax_x = max_x1
          call uf1drd( 1, tdev,shdate,xarray,data1,nmax_x,num_x,
     1	    iproc, xlab, flab,
     2	    nscmax, nsc, scval, sclab, ierr)
      	    call ufclos(1)
 
          idim = 1
 
      else if( ierr .eq. 0) then
          write(6,*) 'Can not handle data sets of this dimension'
          goto 10
 
      endif
 
 
      if( ierr .ne. 0) then
          write(6,*) 'Error reading input data'
          goto 10
 
      endif
 
      if( lxout(0) ) call neupag
 
21    if( lxout(0) ) then
          if( idim.eq.1) then
      	write(6,*) '*** 1-D data ***'
          else
      	write(6,*) '*** 2-D data ***'
          endif
          write(6,*) ' '
          write(6,*) '[W] to set overall systematic variance'
          IF (mode_w .eq. 0) THEN
      	write(6,*) '	now: MULTIPLICITIVE, Sigma=',sigma_w,
     1		'  max=',sigma_max_w
	
          ELSE IF (mode_w .eq. 1) THEN
      	write(6,*)
     >    '	now: ADDITIVE, Sigma=',sigma_w,'  max=',sigma_max_w
          END IF
 
          if( idim.eq.2) then
             write(6,*)
     >       '[C] to set systematic variances for each spatial channel'
      	IF (mode_c .eq. 0) THEN
      	    write(6,*) '	now: MULTIPLICITIVE, Sigma=',sigma_c,
     1		    '  max=',sigma_max_c
	
      	ELSE IF (mode_c .eq. 1) THEN
      	    write(6,*) '	now: ADDITIVE, Sigma=',sigma_c,
     1		    '  max=',sigma_max_c
 
                ELSE IF (mode_c .eq. 2) THEN
      	  if( umode_m) then
                    write(6,*)
     >          '    now: MULTIPLICITIVE, set by Ufile input'
 
      	  else
                    write(6,*) '    now: ADDITIVE, set by Ufile input'
      	  endif
 
      	END IF
 
      	write(6,*) ' '
      	if( NB ) then
      	    write(6,*) '[N] to turn OFF special handling for beam data'
      	else
      	    write(6,*)
     >        '[N] to process data in Neutral Beam special format'
      	endif
          endif
 
          write(6,*) '[B] to set baseline value'
          if( mode_b) then
      	write(6,*) 'Currently on, at ',baseline
          endif
 
          write(6,*) ' '
          write(6,*) '[G] to go and generate variant output files'
c	    write(6,*) '[P] to plot input data'
          write(6,*) ' '
          write(6,*) '[S] for new shot'
          write(6,*) '[D] for new file names'
          write(6,*) '[Q] to quit'
      endif
!
      call ureadc('Wiggle:  Enter choice:$',cmd)
 
      IF (cmd .eq. 'Q') THEN
          call uexita(0)
	
      ELSE IF (cmd .eq. 'S') THEN
          goto 20
	
      ELSE IF (cmd .eq. 'D') THEN
          goto 10
	
      ELSE IF (cmd .eq. 'N' .and. idim.eq.2 ) THEN
          nb = .not. nb
	
      ELSE IF (cmd .eq. 'W') THEN
          if( lxout(0) ) then
      	write(6,*) ' '
      	write(6,*) ' ** Overall systematic variance **'
          endif
25        call ureadc(
     1       'Choose Multiplicitive or Additive variation [M,A]$',
     1        ans)
          IF (ans .eq. 'M') THEN
      	mode_w = 0
      	if( lxout(0) ) then
      	    write(6,*) 'Ratio will be  1 + abs(gaussian) or'
      	    write(6,*) 'or inverse with equal probability'
      	endif
      	call ureadf('Set standard deviation of gaussian:$',sigma_w)
250   	call ureadf('Set maximum ratio (or inverse; 0 for no limit)$',
     1		    sigma_max_w)
      	IF (sigma_max_w .eq. 1) THEN
      	    write(6,*)  'A ratio limit of 1. makes no sense!'
      	    goto 250
	
      	ELSE IF (sigma_max_w .gt. 0 .and. sigma_max_w .lt. 1) THEN
      	    sigma_max_w = 1. / sigma_max_w
 
      	END IF
 
 
          ELSE IF (ans .eq. 'A') THEN
      	mode_w = 1
      	if( lxout(0) ) 	write(6,*) 'Perturbation is taken as a gaussian'
      	call ureadf('Set standard deviation of gaussian:$',sigma_w)
      	call ureadf('Set maximum perturbation (0 for no limit)$',
     1		    sigma_max_w)
	
          ELSE
      	write(6,*)  'Incorrect choice !'
      	goto 25
          END IF
	
      ELSE IF (cmd .eq. 'C' .and. idim .eq. 2) THEN
          if( lxout(0) ) then
                write(6,*) ' '
                write(6,*) '**Independent variation of each point**'
                write(6,*) ' Choose between:'
                write(6,*) ' [M] multiplicitive variance'
                write(6,*) ' [A] additive variance'
                write(6,*)
     >             ' [U] Ufile specification of variance'
                write(6,*)
     >             '     this option allows UFile specification of'
                write(6,*)
     >             '     systematic and statistic variance for each'
                write(6,*)
     >             '     data point.'
                write(6,*)
     >             '     Systematic Variance can be multiplicative.'
                write(6,*) ' '
          endif
 
26          call ureadc('Choose variation type [M,A,U]$', ans)
 
          IF (ans .eq. 'M') THEN
      	mode_c = 0
      	if( lxout(0) ) then
      	    write(6,*) 'Ratio will be  1 + abs(gaussian) or'
      	    write(6,*) 'or inverse with equal probability'
      	endif
      	call ureadf('Set standard deviation of gaussian:$',sigma_c)
260   	call ureadf('Set maximum ratio (or inverse; 0 for no limit)$',
     1		    sigma_max_c)
      	IF (sigma_max_c .eq. 1) THEN
      	    write(6,*)  'A ratio limit of 1. makes no sense!'
      	    goto 260
	
      	ELSE IF (sigma_max_c .gt. 0 .and. sigma_max_c .lt. 1) THEN
      	    sigma_max_c = 1. / sigma_max_c
 
      	END IF
 
          ELSE IF (ans .eq. 'A') THEN
      	mode_c = 1
      	if( lxout(0) ) write(6,*) 'Perturbation is taken as a gaussian'
      	call ureadf('Set standard deviation of gaussian:$',sigma_c)
      	call ureadf('Set maximum perturbation (0 for no limit)$',
     1		    sigma_max_c)
	
            ELSE IF (ans .eq. 'U') THEN
                mode_c = 2
      	isig(1) = 2
      	isig(2) = 0
 
      	if( lxout(0) ) then
      	    write(6,*) 'UFile input of systematic standard deviation'
      	endif
      	call ufnter(4,'INPUT systematic sigma')
	
      		call ufoprd(4,ishot,ierr)
 
      	if( ierr .eq. 0) then
      	    call ufhdck(4, info, ierr)
      	endif
      	if( ierr .eq. 0 .and. info(1) .eq. 2) then
      	    CALL UFSNAM(ISHOT,sfILE_1,IER,4)
      	    nmax_x = max_x
      	    call uf2drd( 4,tdev,shdate2,sigma2,nmax_x,xarray2,nmax_x,
     1			yarray2,
     1		    max_y,iproc, num_x2, num_y2, 0, siglab,sxlab,sylab,
     2		    nscmax, nsc2, scval2, sclab2, ierr)
      		    call ufclos(4)
 
      	else if( ierr .eq. 0 .and. info(1) .eq. 1 ) then
      	    CALL UFSNAM(ISHOT,sFILE_2,IER,4)
      	    nmax_x = max_x1
      	    call uf1drd( 4, tdev,shdate2,xarray2,sigma_a,nmax_x,num_x2,
     1		    iproc, sxlab, siglab,
     2		    nscmax, nsc2, scval2, sclab2, ierr)
      		    call ufclos(4)
 
      	    isig(1) = 1
 
      	else if( ierr .eq. 0) then
      	    write(6,*) 'Can not handle data sets of this dimension'
      	    goto 26
 
      	endif
 
 
      	if( ierr .ne. 0) then
      	    write(6,*) 'Error reading sigma data'
      	    goto 26
 
      	endif
 
c	check that sigma and data are the same shape...
 
      	IF (isig(1) .eq. 1 .and. num_x2 .ne. num_y) THEN
      	    write(6,*)
     >        'Sigma dimension not the same as data spatial dim'
      	    goto 26
	
      	ELSE IF (isig(1).eq.2 .and. (num_x2.ne.num_x .or.
     1				     num_y2.ne.num_y)) THEN
      	    write(6,*) 'Sigma not dimensioned the same as the data'
      	    goto 26
      	END IF
 
      	IF (isig(1) .eq. 1) THEN
      	    DO j=1,num_y
      		DO i=1, num_x
      		    sigma2(i,j,1)=sigma_a(j)
      		END DO
      	    END DO
      	END IF
 
225   	if( lxout(0) ) then
      	    write(6,*) ' '
      	    write(6,*)
     >         'Use UFile as Additive or Multiplicitive Varience?'
      	    write(6,*) ' '
      	endif
      	call ureadc('Choose systematic varience type [A,M]$',ans_um)
      	if( ans_um .eq. 'A') then
      	    umode_m = .false.
      	else if( ans_um .eq. 'M') then
      	    umode_m = .true.
      	else
      	    write(6,*) 'Illegal choice!!'
      	    goto 225
      	endif
 
 
27    	if( lxout(0) ) then
      	    write(6,*) ' '
      	    write(6,*) 'The user may read in a 2nd UFile giving'
      	    write(6,*) 'the statistical uncertainty of each data point'
      	    write(6,*) ' '
      	endif
 
        call ureadc('Input statistical sigma UFile [y,n]$', ans2)
 
      	if( ans2 .eq. 'Y') then
 
      	    call ufnter(5,'INPUT statistical sigma')
	
      		    call ufoprd(5,ishot,ierr)
 
      	    if( ierr .eq. 0) then
      		call ufhdck(5, info, ierr)
      	    endif
      	    if( ierr .eq. 0 .and. info(1) .eq. 2) then
      		CALL UFSNAM(ISHOT,sfILE_2,IER,5)
      		nmax_x = max_x
      		call uf2drd( 5,tdev,shdate2,sigma2(1,1,2),nmax_x,
     1			    xarray2,nmax_x,yarray2,
     1			max_y,iproc, num_x2, num_y2,0,
     1			siglab,sxlab,sylab,
     2			nscmax, nsc2, scval2, sclab2, ierr)
      			call ufclos(5)
 
      		isig(2) = 2
 
      	    else if( ierr .eq. 0 .and. info(1) .eq. 1 ) then
      		CALL UFSNAM(ISHOT,sfILE_2,IER,5)
      		nmax_x = max_x1
      		call uf1drd( 5, tdev,shdate2,xarray2,
     1			sigma_b,nmax_x,num_x2,
     1			iproc, sxlab, siglab,
     2			nscmax, nsc2, scval2, sclab2, ierr)
      			call ufclos(5)
 
      		isig(2) = 1
 
      	    else if( ierr .eq. 0) then
      		write(6,*) 'Can not handle data sets of this dimension'
      		goto 27
 
      	    endif
 
      	    if( ierr .ne. 0) then
      		write(6,*) 'Error reading statistical sigma data'
      		goto 27
 
      	    endif
 
c	    check that sigma and data are the same shape...
 
      	    IF (isig(2) .eq. 1 .and. num_x2 .ne. num_y) THEN
      		write(6,*)
     1		    'Sigma dimension not the same as data spatial dim'
      		goto 27
	
      	    ELSE IF (isig(2).eq.2 .and. (num_x2.ne.num_x .or.
     1					 num_y2.ne.num_y)) THEN
      		write(6,*) 'Sigma not dimensioned the same as the data'
      		goto 27
      	    END IF
 
 
      	    IF (isig(2) .eq. 1) THEN
      		DO j=1,num_y
      		    DO i=1, num_x
      			sigma2(i,j,2)=sigma_b(j)
      		    END DO
      		END DO
      	    END IF
 
      	endif
 
          ELSE
      	write(6,*)  'Incorrect choice !'
      	goto 26
          END IF
	
      ELSE IF (cmd .eq. 'B') THEN
          mode_b = .false.
 
          write(6,*)
     >       'Setting a basline value clamps the value from below'
 
          call ureadc('Turn on baseline [y/n]$',ans)
          if( ans .eq. 'Y') then
      	mode_b = .true.
      	call ureadf('Set baseline value$',baseline)
          endif
 
c	ELSE IF (cmd .eq. 'P') THEN
c	    {executable_statement}...
	
      ELSE IF (cmd .eq. 'G') THEN
 
          num_y2 = num_y
          if( idim.eq.2 .and. NB) then
      	num_y2 = num_y/4	! how many rows to vary?
      	if( num_y2*4 .ne. num_y) then
      	    write(6,*)
     >        '**** File format incorrect for Neutral Beam Data!!'
      	    write(6,*) '**** Try again!****'
      	    goto 21
      	endif
 
      	DO j=num_y2+1,num_y
      	    DO i=1,num_x
      		out2(i,j) = data2(i,j)
      	    END DO
      	END DO
 
          endif
 
400       call ureadi('Number of files to generate$',nfiles)
          if( nfiles .le. 0) goto 400
 
410       call ureadi('Starting number in suffix$',isuffix)
          if( isuffix .lt. 0 .or. isuffix+nfiles-1 .gt. 999) goto 410
	
          DO ifile=0, nfiles-1
      	jsuf = isuffix + ifile
      	IF (jsuf .lt. 10) THEN
      	    write(suffix,'(a,i1)') 'E0',jsuf
	
      	ELSE IF (jsuf .lt. 100) THEN
      	    write(suffix,'(a,i2)') 'E',jsuf
	
      	ELSE IF (jsuf .lt. 1000) THEN
      	    write(suffix,'(i3)') jsuf
	
      	ELSE
      	    write(6,*) ' ***GAK:  illegal suffix number'
      	    stop
      	END IF
 
      	call ufset_ext(3,suffix)
      	call ufcmpr(3,2)
      	call ufopwr(3,ishot,ierr)
      	if( ierr .ne. 0) then
      	    write(6,*)  '*** Open error !! ***'
      	    goto 21
      	endif
 
      	call ufopcf(21,ier)
      	WRITE(21,4001) ZFILE
 4001 	FORMAT(' Produced by WIGGLE '/
     >		       '   from input data file:  "',A,'"' /
     2	       ' Using the following control parameters:')
 
      	if(mode_b) write(21,*) 'Baseline value',baseline
 
      	factor = 1.
      	offset = 0.
 
      	IF (mode_w .eq. 0) THEN
420   	    factor = gauss(sigma_w)
      	    if( abs(factor)+1. .gt. sigma_max_w
     1		.and. sigma_max_w .ne. 0.) goto 420
      	    IF (factor .gt. 0) THEN
      		factor = 1. + factor
	
      	    ELSE
      		factor = 1./(1.+abs(factor))
      	    END IF
 
      	    write(21,*) ' Global : MULTIPLICITIVE, Sigma=',sigma_w,
     1	    '  max=',sigma_max_w
	
      	ELSE IF (mode_w .eq. 1) THEN
422   	    offset = gauss(sigma_w)
      	    if( abs(offset) .gt. sigma_max_w
     1		.and. sigma_max_w .ne. 0.) goto 422
 
      	    write(21,*) ' Global: ADDITIVE, Sigma=',sigma_w,
     1		    '  max=',sigma_max_w
      	END IF
 
      	IF (idim .eq. 2) THEN
      	    IF (mode_c .eq. 0) THEN
      		write(21,*) ' Local: MULTIPLICITIVE, Sigma=',sigma_c,
     1		'  max=',sigma_max_c
	
      	    ELSE IF (mode_c .eq. 1) THEN
      		write(21,*) ' Local: ADDITIVE, Sigma=',sigma_c,
     1		'  max=',sigma_max_c
 
      	    ELSE IF (mode_c .eq. 2 .and. isig(2) .eq. 0) THEN
      	      if( umode_m) then
      		write(21,*) ' Local: MULTIPLICITIVE, from UFile ',
     1			    sfile_1
      	      else
      		write(21,*) ' Local: ADDITIVE, from UFile ',sfile_1
      	      endif
 
      	    ELSE IF (mode_c .eq. 2 .and. isig(2) .ne. 0) THEN
      	      if( umode_m) then
      		write(21,*) ' Local: MULTIPL., from systematic sigma',
     1			    ' UFile ',sfile_1
 
      		write(21,*) '        and statistical sigma from UFile ',
     1			    sfile_2
 
      	      else
      		write(21,*) ' Local: ADDITIVE, from systematic sigma',
     1			    ' UFile ',sfile_1
 
      		write(21,*) '        and statistical sigma from UFile ',
     1			    sfile_2
 
      	      endif
      	    END IF
 
      	    IF (mode_c .ne. 2) THEN
      	      DO j=1, num_y2
      		factor2 = 1.
      		offset2 = 0.
 
      		IF (mode_c .eq. 0) THEN
430   		    factor2 = gauss(sigma_c)
      		    if( abs(factor2)+1. .gt. sigma_max_c
     1			.and. sigma_max_c .ne. 0.) goto 430
      		    IF (factor2 .gt. 0) THEN
      			factor2 = 1. + factor2
	
      		    ELSE
      			factor2 = 1./(1.+abs(factor2))
      		    END IF
	
      		ELSE IF (mode_c .eq. 1) THEN
432   		    offset2 = gauss(sigma_c)
      		    if( abs(offset2) .gt. sigma_max_c
     1			.and. sigma_max_c .ne. 0.) goto 432
      		END IF
      	        if( mode_b) then
      		  DO i=1,num_x
      		    out2(i,j) = max(baseline,
     1			      data2(i,j)*factor*factor2
     1			    + offset+offset2)
      		  END DO
      	        else
      		  DO i=1,num_x
      		    out2(i,j) = data2(i,j)*factor*factor2
     1			      + offset+offset2
      		  END DO
      	        endif
      	      END DO
 
      	    ELSE IF( mode_b .and. isig(2) .eq. 0) THEN
      		factor2 = gauss(1.)
      		DO j=1,num_y2
      		    DO i=1,num_x
      			offset2 = factor2*sigma2(i,j,1)
      		        out2(i,j) = max(baseline,
     1			      data2(i,j)*factor+offset+offset2)
      		    END DO
      		END DO
 
      	    ELSE IF( isig(2) .eq. 0) THEN
      		factor2 = gauss(1.)
      		DO j=1,num_y2
      		    DO i=1,num_x
      			offset2 = factor2*sigma2(i,j,1)
      		        out2(i,j) = data2(i,j)*factor+offset+offset2
      		    END DO
      		END DO
 
      	    ELSE IF( mode_b .and. isig(2) .ne. 0) THEN
      		factor2 = gauss(1.)
      		DO j=1,num_y2
      		    DO i=1,num_x
      			offset2 = gauss(sigma2(i,j,2))
     1				+ factor2*sigma2(i,j,1)
      		        out2(i,j) = max(baseline,
     1			      data2(i,j)*factor+offset+offset2)
      		    END DO
      		END DO
 
      	    ELSE IF( isig(2) .ne. 0) THEN
      		factor2 = gauss(1.)
      		DO j=1,num_y2
      		    DO i=1,num_x
      			offset2 = gauss(sigma2(i,j,2))
     1				+ factor2*sigma2(i,j,1)
      		        out2(i,j) = data2(i,j)*factor+offset+offset2
      		    END DO
      		END DO
 
 
      	    END IF
 
      	    nmax_x = max_x
      	    call uf2dwr(3,tdev,shdate,out2,nmax_x,xarray,num_x,yarray,
     1		num_y,iproc, flab, xlab, glab,
     2		nsc, scval, sclab, ierr)
 
      	ELSE
      	  if( mode_b) then
      	    DO i=1,num_x
      		out1(i) = max(baseline, data1(i)*factor + offset)
      	    END DO
      	  else
      	    DO i=1,num_x
      		out1(i) = data1(i)*factor + offset
      	    END DO
                endif
 
      	    nmax_x = max_x1
      	    call uf1dwr( 3, tdev,shdate,xarray,out1,num_x,
     1		    iproc, xlab, flab,
     2		    nsc, scval, sclab, ierr)
      	END IF
 
      	call ucsend(21, 3)
c		call uspool( 1, 3)
      		call ufclos(3)
      	if( ierr .ne. 0) then
      	    write(6,*) '**** Error writing !! ****'
      	    goto 21
      	endif
          END DO
 
      ELSE
          write(6,*) 'Illegal choice, try again'
      END IF
 
      goto 21
 
      end
!
C******************** START FILE UFSET_ext ; GROUP UFILES *************
C---------------------------------------------------------------------
C  UFSET_ext
C  SET/RESET FILE extension
C
      SUBROUTINE UFSET_ext(ILUN,ZEXT)
C
C  SAMPLE CALL (VAX):
C	CALL UFSET_ext(24,'PCR')
C
C
c
c	modified by MCZ  May 1991 from UFSETR
C------------------------------------------------------
C
      USE UFILES
 
      CHARACTER*(*) ZEXT	!@
C
      CHARACTER*15 ZERRL
C
C  ERROR LABEL
      DATA ZERRL/'UFILES>UFSETR: '/
      CALL UFINIT
      ICHAN=IUXCHA(ILUN)
      IF(ICHAN.EQ.0) CALL UFXCHL(ILUN,ICHAN,IER)
      IF(IER.NE.0) THEN
C
C  ERROR-- ASSIGNMENT TABLE OVERFLOW
        WRITE(UFELUN,9001) ILUN,UFCHAN
 9001   FORMAT(' ? UFSETR-- TABLE OVERFLOW ON L.U.N. ',I5/
     >	  '   UFILES L.U.N. ASSIGNMENT FAILED'/
     >	  '   MAXIMUM NUMBER OF L.U.N. ASSIGNMENTS: ',I3/)
        CALL UFPAUS(1)
        RETURN
      ENDIF
C
      IF(.NOT.LUFSET(ICHAN)) GO TO 20
      IF(UFSTAT(ICHAN).NE.0) THEN
        WRITE(UFELUN,9002) ILUN
 9002   FORMAT(' ? UFSETR-- ATTEMPT TO RESET OPEN L.U.N.:  ',I5/
     >	  '   CALL UFCLOS TO CLOSE UNIT FIRST BEFORE REASSIGNMENT')
        RETURN
      ENDIF
C
 20   CONTINUE
C
C  GET LENGTHS OF FILENAME ELEMENTS.
C  CHECK INPUT FILENAME PARTS:  LEFT JUSTIFIED, NO IMBEDDED BLANKS,
      LEXTND(ICHAN)=IUFTRM(ZEXT,ZERRL//'.EXT',IER2)	!@
C
      IF(IER2.GT.0) RETURN		!@
C
C  CHECK LENGTHS:  IF OK, INSERT NAMES IN COMMON
C
      CALL UFNPAD(						!@
     >ZEXT,LEXTND(ICHAN),UEXTND(ICHAN),ZERRL//'.EXT',IER2)	!@
 
      RETURN
      END
!
c ***************************************************************************
      FUNCTION GAUSS(S)
C
C  taken from hx2:[gold]roter.for  on 9/15/88
C
      DATA I/1/,C/1.414214/
C
      save Y2
C
C-------------------------------------
C
      GOTO(1,2)I
 
 1    CALL RANDOM_NUMBER(Y)
      IF(Y.EQ.0.00E00) GOTO 1
 
      CALL RANDOM_NUMBER(Z)
      D=SQRT(-ALOG(Y))*C
      Z1=6.283186*Z
      Y1=D*COS(Z1)
      Y2=D*SIN(Z1)
      GAUSS=S*Y1
      I=2
 
      RETURN
C
 2    GAUSS=S*Y2
      I=1
 
      RETURN
 
      END
