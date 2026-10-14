

/*
 * c_ufsetr.c
 *
 * C wrapper for MDSplus to call the UFILES
 * Fortran routine UFSETR.
 *
 * Called by:
 *   tr_tdi/read_ufile.fun
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * Preserves the original exported interface:
 *
 *   void c_ufsetr(int *ilun, char *prefix,
 *                 char *ext, char *dev, char *dir)
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "f77name.h"

/* ------------------------------------------------------------
 * Fortran UFILES interface
 *
 * Explicit declaration replaces the legacy implicit
 * function declaration.
 *
 * The trailing CHARACTER lengths are passed by value,
 * preserving the original wrapper's argument order.
 *
 * IMPORTANT:
 * Verify the hidden Fortran CHARACTER-length ABI
 * for the selected compiler.
 * ------------------------------------------------------------ */

extern void F77NAME(ufsetr)(
    int  *ilun,
    char *prefix,
    char *ext,
    char *dev,
    char *dir,
    int   lp,
    int   le,
    int   ldev,
    int   ldir
);

/* ------------------------------------------------------------
 * MDSplus wrapper
 * ------------------------------------------------------------ */

void c_ufsetr(
    int  *ilun,
    char *prefix,
    char *ext,
    char *dev,
    char *dir)
{
    size_t nprefix;
    size_t next;
    size_t ndev;
    size_t ndir;

    int lp;
    int le;
    int ldev;
    int ldir;

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    if (ilun   == NULL ||
        prefix == NULL ||
        ext    == NULL ||
        dev    == NULL ||
        dir    == NULL) {

        fprintf(stderr,
            "c_ufsetr: NULL argument pointer\n");

        return;
    }

    /* --------------------------------------------------------
     * Calculate character lengths
     * -------------------------------------------------------- */

    nprefix = strlen(prefix);
    next    = strlen(ext);
    ndev    = strlen(dev);
    ndir    = strlen(dir);

    /* --------------------------------------------------------
     * Validate conversion to Fortran integer lengths
     * -------------------------------------------------------- */

    if (nprefix > INT_MAX ||
        next    > INT_MAX ||
        ndev    > INT_MAX ||
        ndir    > INT_MAX) {

        fprintf(stderr,
            "c_ufsetr: character length exceeds INT_MAX\n");

        return;
    }

    lp   = (int)nprefix;
    le   = (int)next;
    ldev = (int)ndev;
    ldir = (int)ndir;

    /* --------------------------------------------------------
     * Preserve original diagnostic output
     * -------------------------------------------------------- */

    printf("c_ufsetr: dir = %s, %d\n",
           dir, ldir);

    /* --------------------------------------------------------
     * Call UFILES Fortran routine
     * -------------------------------------------------------- */

    F77NAME(ufsetr)(
        ilun,
        prefix,
        ext,
        dev,
        dir,
        lp,
        le,
        ldev,
        ldir
    );
}

/* End of c_ufsetr.c */
