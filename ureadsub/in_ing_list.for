c...................................................................
C	LOGICAL FUNCTION IN_ING_LIST
 
      logical function in_ing_list(word, token_list, ntokens, iptr)
 
 
C      PROGRAM DESCRIPTION:
C
C	THIS ROUTINE IS A GENERAL TEST OF THE QUESTION:
C	"DOES THE TOKEN <word> APPEAR AS ONE OF THE TOKENS
C	IN THE LIST <token_list> (MADE UP OF <ntokens> ITEMS) ?? "
C	>> IN_ING_LIST is true/false depending on whether it is there or not;
C	>> IPTR tells you where in the list it is.
C
C
C      AUTHORS:
C
C		R.M. WIELAND
C
C      CREATION DATE: 	PRE-1987
C
C
C                      C H A N G E   L O G
C
C      Date     | Name  | Description
C---------------+-------+-----------------------------------------------------
C     8-APR-1987 WIE     GO OVER TO CASE INSENSITIVE CHECKS
C
C    29-MAY-1992 DMC     use STR_CBC instead of STR$CASE_BLIND_COMPARE
C                        because the latter is not in the Unix RTL
C                        STR_CBC is in the ufencod library
C
C
      integer the_same
        parameter               (THE_SAME=0)
 
      character*(*)		word
      character*(*)		token_list(*)
      integer			ntokens
      integer			iptr
 
C	FUNCTION DECLARATION
      integer, external :: str_cbc
 
      if ( ntokens .eq. 0) go to 150
 
      do 100 k = 1, ntokens
      iptr = k
        IF (str_cbc(
     >            token_list(k)(1:len_trim(token_list(k))),
     >            word)
     >    .eq.THE_SAME) THEN
c..........found a match
         in_ing_list = .true.
         return
        END IF
100   continue
 
c..	fall thru; no match
150   in_ing_list = .false.
      iptr = 0
 
        return
      end
 
 
