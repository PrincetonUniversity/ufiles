C Find_image_symbol - routine to call LIB$FIND_IMAGE_SYMBOL with the
C  sig_to_ret condtion handler.  This is done because lib$find_image_symbol
C  signals errors instead of returning them.
C
C  Josh Stillerman 12/5/94
C
      Integer*4 function Find_image_symbol(image, routine, addr)
 
      Integer*4 STATUS
 
      status = 0
 
      Find_image_symbol = status
      return
 
      end
