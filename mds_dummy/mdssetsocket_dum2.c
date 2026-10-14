
/*
 * mdssetsocket.c
 *
 * Dummy MDSplus MdsSetSocket interface.
 *
 * Originally moved from mdsdummy.c by Pletzer.
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.2.0
 *
 * C standard: C11
 *
 * All functions return -1 because MdsSetSocket
 * is not available in this dummy implementation.
 */

#include <stdio.h>

#define MDS_DUMMY_ERROR (-1)

/* ------------------------------------------------------------
 * Function prototypes
 * ------------------------------------------------------------ */

int MdsSetSocket(void);
int mdssetsocket(void);
int mdssetsocket_(void);

/* ------------------------------------------------------------
 * Dummy MdsSetSocket implementation
 * ------------------------------------------------------------ */

int MdsSetSocket(void)
{
    puts(" __mdsdummy.c:  dummy MdsSetSocket call.");
    puts("   link with a more recent version of MdsLib.");

    return MDS_DUMMY_ERROR;
}

/* ------------------------------------------------------------
 * C-compatible lowercase wrapper
 * ------------------------------------------------------------ */

int mdssetsocket(void)
{
    return MdsSetSocket();
}

/* ------------------------------------------------------------
 * Fortran-compatible wrapper
 * ------------------------------------------------------------ */

int mdssetsocket_(void)
{
    return MdsSetSocket();
}

/* End of mdssetsocket.c */
