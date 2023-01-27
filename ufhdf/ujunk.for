C---------------------------------------------------------------------
C  UJUNK - CHECK LOGICAL NAME FOR A JUNK FILE DIRECTORY
C
      SUBROUTINE UJUNK(ZLNAME,ZAREA,IER)
C
C  ZLNAME (INPUT)  LOGICAL NAME POINTING TO "JUNK AREA"
C    WHERE TEMPORARY FILES MAY BE WRITTEN
C
C  ZAREA (OUTPUT)  IF ENVIRONMENT NAME IS DEFINED,
C    THE STRING "name_translation/" IS RETURNED; OTHERWISE "/tmp/"
C    IS RETURNED
C
C  OUTPUT:
C  IER = 0  -- LOGICAL NAME IS DEFINED, RETURNED
C  IER = 1  -- "SYS$LOGIN:" RETURNED
C  IER = 3 OR 4  LENGTH ERROR IN OUTPUT STRING ZAREA
C
      CHARACTER*(*) ZLNAME
      CHARACTER*(*) ZAREA
C
      CHARACTER*512 ZBUFF
C
C--------------------------------
C
      ILNAME=len_trim(ZLNAME)
C
      ILAREA=LEN(ZAREA)
C
      ZAREA=' '
C
      ier=0
      call sget_env(zlname,zbuff)
      ilen=len_trim(zbuff)
 
      ilname = ilen
      IF(ilname.eq.0) THEN
         IER=1
         if(ilarea.ge.5) then
            zarea(ilarea-4:ilarea)='/tmp/'
         else
            ier=4
         endif
      else
         IF(ILAREA.GT.ilname) THEN
            ILAM9=ILAREA-ilname
            ZAREA(ILAM9:ILAREA)=zbuff(1:ilname)//'/'
         ELSE
            IER=4
         ENDIF
      ENDIF
 
 1000 CONTINUE
      RETURN
      END
