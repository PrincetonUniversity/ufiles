      subroutine uupper(STR)
C
C  CAPITALIZE STR
C
      CHARACTER*(*) STR
C
      CHARACTER*26 ALPHAB(2)
C
      DATA ALPHAB/'abcdefghijklmnopqrstuvwxyz',
     >              'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C
C  EXECUTABLE CODE
C
      L=LEN(STR)
C
      DO I=1,L
        IA=INDEX(ALPHAB(1),STR(I:I))
        IF(IA.NE.0) STR(I:I)=ALPHAB(2)(IA:IA)
      END DO
C
      return
      end subroutine uupper
