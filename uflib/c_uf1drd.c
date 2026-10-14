
/*
 * c_uf1drd.c
 *
 * C wrapper for MDSplus/TDI to call the UFILES
 * Fortran routine UF1DRD.
 *
 * Called by:
 *   TDI function tr_tdi/read_ufile.fun
 *
 * Compatible with:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * Preserves the original exported interface:
 *   c_uf1drd
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "f77name.h"

/* ------------------------------------------------------------
 * Fortran interface
 *
 * Explicit declaration prevents implicit-function errors
 * with modern C compilers.
 *
 * Character lengths are passed by value, preserving
 * the original C wrapper.
 * ------------------------------------------------------------ */

extern void F77NAME(uf1drd)(
    int   *ilun,
    char  *idev,
    char  *zsdate,
    float *x,
    float *f,
    int   *ndim,
    int   *nact,
    int   *iproc,
    char  *xlab,
    char  *flab,
    int   *nscd,
    int   *nsca,
    float *scval,
    char  *sclab,
    int   *ier,
    int    ldev,
    int    ldate,
    int    lxl,
    int    lfl,
    int    lsclab
);

/* ------------------------------------------------------------
 * MDSplus / TDI wrapper
 * ------------------------------------------------------------ */

void c_uf1drd(
    int   *ilun,
    char  *idev,
    char  *zsdate,
    float *x,
    float *f,
    int   *ndim,
    int   *nact,
    int   *iproc,
    char  *xlab,
    char  *flab,
    int   *nscd,
    int   *nsca,
    float *scval,
    char  *sclab,
    int   *ier)
{
    size_t ndev;
    size_t ndate;
    size_t nxl;
    size_t nfl;

    int ldev;
    int ldate;
    int lxl;
    int lfl;
    int lsclab;

    /* --------------------------------------------------------
     * Validate required pointers
     * -------------------------------------------------------- */

    if (ilun == NULL ||
        idev == NULL ||
        zsdate == NULL ||
        x == NULL ||
        f == NULL ||
        ndim == NULL ||
        nact == NULL ||
        iproc == NULL ||
        xlab == NULL ||
        flab == NULL ||
        nscd == NULL ||
        nsca == NULL ||
        scval == NULL ||
        sclab == NULL ||
        ier == NULL) {

        fprintf(stderr,
            "c_uf1drd: NULL argument pointer\n");

        if (ier != NULL) {
            *ier = 1;
        }

        return;
    }

    /* --------------------------------------------------------
     * Validate scalar dimensions
     * -------------------------------------------------------- */

    if (*nscd < 0 || *nscd > INT_MAX / 3) {

        fprintf(stderr,
            "c_uf1drd: invalid nscd value %d\n",
            *nscd);

        *ier = 1;
        return;
    }

    /* --------------------------------------------------------
     * Calculate character lengths
     * -------------------------------------------------------- */

    ndev  = strlen(idev);
    ndate = strlen(zsdate);
    nxl   = strlen(xlab);
    nfl   = strlen(flab);

    if (ndev > INT_MAX ||
        ndate > INT_MAX ||
        nxl > INT_MAX ||
        nfl > INT_MAX) {

        fprintf(stderr,
            "c_uf1drd: character length exceeds INT_MAX\n");

        *ier = 1;
        return;
    }

    ldev   = (int)ndev;
    ldate  = (int)ndate;
    lxl    = (int)nxl;
    lfl    = (int)nfl;

    /*
     * Preserve the original scaling factor.
     *
     * Original code:
     *   lsclab = *nscd * 3;
     */

    lsclab = (*nscd) * 3;

    /* --------------------------------------------------------
     * Call the UFILES Fortran routine
     * -------------------------------------------------------- */

    F77NAME(uf1drd)(
        ilun,
        idev,
        zsdate,
        x,
        f,
        ndim,
        nact,
        iproc,
        xlab,
        flab,
        nscd,
        nsca,
        scval,
        sclab,
        ier,
        ldev,
        ldate,
        lxl,
        lfl,
        lsclab
    );

    /* --------------------------------------------------------
     * Preserve legacy diagnostic output
     * -------------------------------------------------------- */

    printf("c_uf1drd: idev = %s, %d\n",
           idev, ldev);

    printf("c_uf1drd: xlab = %s, %d\n",
           xlab, lxl);
}

/* End of c_uf1drd.c */
