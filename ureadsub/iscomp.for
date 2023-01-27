C******************** START FILE ISCOMP.FOR ; GROUP URDUTS ******************
C
C=========================================================
C&  ISCOMP  UREAD COMPARE INPUT STRING WITH LIST, SIGN FEATURE
C
C  COMPARE INPUT STRING WITH LIST, LOOK FOR A MATCH
C   SPECIAL CHARACTERS-- '+' AND '-' ALGEBRAIC
C    MULTIPLY ANSWER BY +/- 1
C
C  FUNCTION VALUE RETURNED POINTS TO LOCATION OF MATCH
C  IN LIST; NO MATCH ==> 0 RETURNED AS FCN VALUE
C
C
      INTEGER FUNCTION ISCOMP(ABLIS,N,NMAX)
C  ABLIS-- LIST OF STRINGS
C  N    -- # OF ELEMENTS IN LIST
C  NMAX -- MAX # OF CHARACTERS PER ELEMENT IN LIST
C
C
      USE USERIO
      CHARACTER*(*) ABLIS(N)
      character*(urdbufl) zcbuff
      character*1 zdbuff(urdbufl)
      integer ilen

C  REV. JULY 1982 -- USE SUBFUNCTION WHICH DOESNT INCLUDE
C  USERIO COMMON
      zcbuff=cbuff
      call uspcap(zcbuff)

      zdbuff=' '
      ilen=max(1,len_trim(zcbuff))
      do i=1,ilen
         zdbuff(i)=zcbuff(i:i)
      enddo
C
      ISCOMP=ISCMP0(ZDBUFF,ABLIS,N,NMAX,ILC)
      IF(ISCOMP.NE.0) CALL UROTOR(ILC)
      RETURN
C
      END
C******************** END FILE ISCOMP.FOR ; GROUP URDUTS ******************
