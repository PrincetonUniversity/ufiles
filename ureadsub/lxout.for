C******************** START FILE LXOUT.FOR ; GROUP URDUTS ******************
C
C------------------
C&  LXOUT  -- UREAD LOGICAL FUNCTION .TRUE. FOR INFORMATIONAL OUTPUT
C            I.E. FETCH OUT OF COMMON
      LOGICAL FUNCTION LXOUT(IDUM)
C
      USE USERIO
C
      LXOUT=NLXOUT
      RETURN
      END
C******************** END FILE LXOUT.FOR ; GROUP URDUTS ******************
