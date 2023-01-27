/* entries for sg routines with variable length argument lists 
   the length of a character string is passed by value at the end of
   the argument list
*/
/* note: if((len = va_arg(alist, int))==0) to test for the existence of
   the last argument doesn't always work on the DECstation, so instead
   assume that a string is always less that 256 characters
*/
#include <stdlib.h>
#include <stdarg.h>

static int zero=0;


agtchs_(char *str, ...)
{

#include "sgvarg_code.h"

  agtchsl_(str, lenp, len);
}

agtcgt_(int *iy, char *str, ...)
{
#include "sgvarg_code.h"

  agtcgtl_(iy, str, lenp, len);
}


agtmhs_(int* ix, int* iy, char *str, ...)
{

#include "sgvarg_code.h"

  agtmhsl_(ix,iy, str, lenp, len);
}

agths_(char *str, ...)
{

#include "sgvarg_code.h"

  agthsl_(str, lenp, len);
}

agtcvs_(char *str, ...)
{

#include "sgvarg_code.h"

  agtcvsl_(str, lenp, len);
}

agtmvs_(int* ix, int* iy, char *str, ...)
{

#include "sgvarg_code.h"

  agtmvsl_(ix,iy, str, lenp, len);
}

agtvs_(char *str, ...)
{

#include "sgvarg_code.h"

  agtvsl_(str, lenp, len);
}

agtmrs_(int* ix, int* iy, int* irot, char *str, ...)
{

#include "sgvarg_code.h"

  agtmrsl_(ix,iy, irot, str, lenp, len);
}

agtrs_(int* irot, char *str, ...)
{

#include "sgvarg_code.h"

  agtrsl_(irot, str, lenp, len);
}
