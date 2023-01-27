      subroutine uread_cmd_enable
      use USERIO

      nlcmd = .TRUE.

      return
      end

      subroutine uread_cmd_disable
      use USERIO

      nlcmd = .FALSE.

      return
      end


C--------------------------------------------------------------------
C&  URCSTR  GENERATE UREAD SPECIAL CONTROL CHARACTER STRING
C
      SUBROUTINE URCSTR(UCNTRL)
C
      USE USERIO
C
      CHARACTER*(*) UCNTRL
C
      if(nlcmd) then
C              comment // input file // output file // command //
         UCNTRL=MCOMMT  //  MFILI  //  MFILO  //  MCMD //
     >        MPARAM  //  MBRNCH //  MTARGT //  MDOLLR
C         parameter defn// branch // target label// terminator
C
      else
         UCNTRL=MCOMMT  //  MFILI  //  MFILO  //  MCOMMT //
     >        MPARAM  //  MBRNCH //  MTARGT //  MDOLLR
      endif
C
      RETURN
      END
C******************** END FILE URCSTR.FOR ; GROUP UREAD5 ******************
