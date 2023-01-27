C& UREADI  ISSUE UREAD PROMPT, RETURN INTEGER
C
      SUBROUTINE UREADI(PROMPT,I)
c
c  original code DMC -- probably from VAX, ca. 1980
c
      USE USERIO
C
      CHARACTER*(*) PROMPT
      INTEGER I
C
      IF(NLXOUT) WRITE(LUNTRM,1001) I
 1001 FORMAT(' [OLD VALUE:  ',I10,']')
C  SAVE DEFAULT VALUE
      II=I
C
 10   CONTINUE
C  RESTORE DEFAULT VALUE AFTER ERROR
      I=II
C
      CALL UREAD(PROMPT)
C
C  ACCEPT DEFAULT (OLD VALUE)?
      IDFLT=IXUOPC('QX,$')
      IF(IDFLT.NE.0) THEN
        IF(NLXOUT) WRITE(LUNTRM,1002)
 1002 FORMAT(' % OLD VALUE RETAINED')
        GO TO 20
      ENDIF
C
      I=IDCOD(IER)
      IF(IER.NE.0) GO TO 10
C
 20   CONTINUE
      RETURN
      END

c--------------------

      subroutine ureadi_nrun(prompt,irun)
c
c  prompt for an integer run number; but also allow translation
c  of a string of form nnnnnAmm -> nnnnn01mm
c
c  this routine does not include 'USERIO'...

      implicit NONE

      character*(*), intent(in) :: prompt
      integer, intent(inout) :: irun     ! old value may be retained

c----------------
      integer :: ii  ! old value, saved...
      character*26 :: alphab = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character*20 :: input_word,zincod
      character*2 :: zi2
      integer :: ic,id,ilen,lnout,ialph,istat,ixuopc,idflt
      logical :: lxout
c----------------

      ii=irun  ! save old value

      IF(LXOUT(0)) WRITE(LNOUT(0),1001) II
 1001 FORMAT(' [OLD VALUE:  ',I10,']'//
     >     '  "runid" translations to integer are supported:'/
     >     '  nnnnnAmm -> nnnnn01mm, nnnnnBmm -> nnnnn02mm, etc.'/)

      do
         irun=ii  ! restore old value (e.g. after error)

         CALL UREAD(PROMPT)
C
C  ACCEPT DEFAULT (OLD VALUE)?
         IDFLT=IXUOPC('QX,$')
         IF(IDFLT.NE.0) THEN
            IF(LXOUT(0)) WRITE(LNOUT(0),1002)
 1002       FORMAT(' % OLD VALUE RETAINED')
            exit
         ENDIF
C
C  user input -- try to parse...
C
         call cdcod(input_word)
C
         ilen=len(trim(input_word))
         if(ilen.gt.15) then
            call uermsg('"RUNID" number too long',input_word,0)
            cycle
         endif
C
         ialph=0
         zincod=' '
         id=21

         do ic=ilen,1,-1
            if(ic.lt.(ilen-3)) ialph=99  ! set if too late for letter code
            if((ialph.gt.0).or.(ic.ge.(ilen-1))) then
                                ! just copy character
               id=id-1
               zincod(id:id)=input_word(ic:ic)
            else
               ialph=index(alphab,input_word(ic:ic))
               if(ialph.eq.0) then
                                ! just copy character
                  id=id-1
                  zincod(id:id)=input_word(ic:ic)
               else
                                ! encode letter
                  write(zi2,'(I2.2)') ialph
                  zincod(id-2:id-1)=zi2
                  id=id-2
                                ! ialph remains set; just 1 letter allowed.
               endif
            endif
         enddo

         read(zincod,'(I20)',iostat=istat) irun
         if(istat.ne.0) then
            call uermsg('"RUNID" integer decode error',input_word,0)
            cycle
         endif

         exit
      enddo

      return
      end

