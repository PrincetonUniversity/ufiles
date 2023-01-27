C******************** START FILE URDMOD.FOR ; GROUP USINIT ******************
C---------------------------------------------
C&  URDMOD  SPECIFY UREAD INPUT MODE: COMMAND OR TEXT
C
      SUBROUTINE URDMOD(IMOD)
C
      USE USERIO
C
c  mod dmc Mar 2004 -- IMOD=0 and IMOD=1 have the same effect
c  automatic case conversion no longer done by UREAD system; 
c  uppercase conversion deferred to data conversion routiens
c  as needed...
c
c--------------------------------------------------------------
c
      LEVCMD=IMOD		! SWITCH IN COMMON BLOCK
C
      IF((IMOD.EQ.1).or.(IMOD.EQ.0)) THEN
C  COMMAND MODE
        CALL UAHEAD		! ENABLE TYPEAHEAD
        NLNULL=NLCRI  	! NULL LINES ACCEPTED IFF NLCRI SET
        CALL URDCHR		! FULL CONTROL CHARACTER SET
      ELSE IF(IMOD.EQ.2) THEN
        CALL USINGL		! DISABLE TYPEAHEAD
        NLNULL=.TRUE.		! NULL LINES ARE ACCEPTED
        CALL URDTCH		! TEXT MODE CHARACTER SET
      ELSE
        CALL UERMSI(' ? URDMOD:  ILLEGAL INPUT MODE:',IMOD,1)
        CALL URDABO
      ENDIF
C
      RETURN
      END
C******************** END FILE URDMOD.FOR ; GROUP USINIT ******************

      subroutine urdmod_get(IMOD)

      !  return current LEVCMD value

      use USERIO

      IMOD = LEVCMD

      return
      end
