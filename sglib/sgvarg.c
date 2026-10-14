
/*
 * sgvarg.c
 *
 * Variable-length character argument wrappers for SGLIB.
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.2.0 / 13.3.0
 *
 * C standard: C11
 *
 * Fortran character string lengths are passed by value
 * at the end of the argument list.
 *
 * The original sgvarg_code.h is retained to preserve
 * the legacy argument-length extraction mechanism.
 */

#include <stdlib.h>
#include <stdarg.h>

/*
 * Used by the legacy sgvarg_code.h implementation.
 */
static int zero = 0;

/*
 * Explicit declarations of the underlying SGLIB routines.
 *
 * These functions are assumed to be Fortran subroutines
 * with no return value.
 *
 * The final two arguments are the length pointer and
 * extracted string length, respectively.
 */

extern void agtchsl_(char *str, int *lenp, int len);

extern void agtcgtl_(int *iy, char *str,
                     int *lenp, int len);

extern void agtmhsl_(int *ix, int *iy, char *str,
                     int *lenp, int len);

extern void agthsl_(char *str, int *lenp, int len);

extern void agtcvsl_(char *str, int *lenp, int len);

extern void agtmvsl_(int *ix, int *iy, char *str,
                     int *lenp, int len);

extern void agtvsl_(char *str, int *lenp, int len);

extern void agtmrsl_(int *ix, int *iy, int *irot,
                     char *str, int *lenp, int len);

extern void agtrsl_(int *irot, char *str,
                    int *lenp, int len);


/* ------------------------------------------------------------
 * AGTCHS
 * ------------------------------------------------------------ */

void agtchs_(char *str, ...)
{
#include "sgvarg_code.h"

    agtchsl_(str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTCGT
 * ------------------------------------------------------------ */

void agtcgt_(int *iy, char *str, ...)
{
#include "sgvarg_code.h"

    agtcgtl_(iy, str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTMHS
 * ------------------------------------------------------------ */

void agtmhs_(int *ix, int *iy, char *str, ...)
{
#include "sgvarg_code.h"

    agtmhsl_(ix, iy, str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTHS
 * ------------------------------------------------------------ */

void agths_(char *str, ...)
{
#include "sgvarg_code.h"

    agthsl_(str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTCVS
 * ------------------------------------------------------------ */

void agtcvs_(char *str, ...)
{
#include "sgvarg_code.h"

    agtcvsl_(str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTMVS
 * ------------------------------------------------------------ */

void agtmvs_(int *ix, int *iy, char *str, ...)
{
#include "sgvarg_code.h"

    agtmvsl_(ix, iy, str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTVS
 * ------------------------------------------------------------ */

void agtvs_(char *str, ...)
{
#include "sgvarg_code.h"

    agtvsl_(str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTMRS
 * ------------------------------------------------------------ */

void agtmrs_(int *ix, int *iy, int *irot,
             char *str, ...)
{
#include "sgvarg_code.h"

    agtmrsl_(ix, iy, irot, str, lenp, len);
}


/* ------------------------------------------------------------
 * AGTRS
 * ------------------------------------------------------------ */

void agtrs_(int *irot, char *str, ...)
{
#include "sgvarg_code.h"

    agtrsl_(irot, str, lenp, len);
}

/* End of sgvarg.c */
