C******************** START FILE UFINIT.FOR ; GROUP UFILES *************
C-----------------------------------------------------------------
C  UFILES
C  ROUTINES FOR ASCII DATA FILE SYSTEM
C  ASCII FILES TO HANDLE 1- AND 2-D NUMERIC DATA IN A SIMPLE
C  MANNER WITH MINIMAL ASSOCIATED SUPPORT INFORMATION
C
C  SYSTEM IS NOT INSTANTANEOUSLY TRANSPORTABLE FROM COMPUTER TO
C  BUT AIM IS TO KEEP SYSTEM SIMPLE ENOUGH THAT CONVERSION
C  TIME IS SMALL
C
C  CONVERTED FROM DAS-10 ORIGINAL FOR USE ON DAS VAX
C  D. MC CUNE 8 DEC 1981
C-----------------------------------------------------------------
C
      SUBROUTINE UFINIT
C
      USE UFILES
      LOGICAL LFINIT
C
      CHARACTER*512 ZJUNKA
C
      DATA LFINIT/.FALSE./
C
C  INITIALIZE COMMON
C
      IF(LFINIT) RETURN
      LFINIT=.TRUE.
C
      LUFCAP=1  ! default:  Ufile names capitalized.
C
C  SET RECORD SIZE DEFAULT -- dmc Jan 1992 -- maintaining 2000 as
C  the default, while expanding UFMAXR
C
      UFRSIZ=min(2000,UFMAXR)
C
      UFCHAN=UFPACH
      DO 10 I=1,UFCHAN
        LUFSET(I)=.FALSE.
        mds_socket(i)=0
        mds_shot(i)=0
 10   CONTINUE
C
      UDDEV='                '
      LDDEV=0
C  ERROR OUTPUT L.U.N. (DEFAULT TO TTY)
      UFELUN=6
C
      CALL UJUNK('TMPDIR_TR',ZJUNKA,IER)
      ilen=len(zjunka)
      do ic=1,ilen
          if(zjunka(ic:ic).ne.' ') go to 15
      enddo
      ic=1
 15   continue
      ilnb=ic
      ujunka=zjunka(ilnb:ilen)
C
      IF(IER.EQ.1) THEN
        iluj=ilnurd(ujunka)
        WRITE(UFELUN,9001) ujunka(1:iluj)
 9001   FORMAT(/
     >' %UFINIT -- using root directory for temporary files:'/1x,a)
        CALL UFPAUS(1)
      ELSE IF(IER.GT.1) THEN
        WRITE(UFELUN,9002) IER
 9002   FORMAT(/
     >' ?UFINIT -- FATAL ERROR IER=',I4,' DEFINING TEMPORARY FILE AREA')
        CALL UFPAUS(1)
      ENDIF
C
C  COMPRESSION DATA LOSS - ERROR STATS
      UFCLUN=19
C
C  INFORMATIVE MESSSAGE OUTPUT = DEFAULT
C  FULL ERROR MESSAGE OUTPUT = DEFAULT
      UFILEV=21
C
      RETURN
C
      END
C******************** END FILE UFINIT.FOR ; GROUP UFILES ***************
