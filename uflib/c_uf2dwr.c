
/*
 * c_uf2dwr.c
 *
 * C wrapper for MDSplus to call the UFILES
 * Fortran routine UF2DWR.
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * Preserves the original exported interface:
 *
 *   void c_uf2dwr(...)
 *
 * Fortran routine:
 *   UF2DWR
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "f77name.h"

/* ------------------------------------------------------------
 * Fortran interface
 *
 * Explicit declaration replaces legacy implicit
 * function declarations.
 *
 * Character lengths are passed by value, matching
 * the original C wrapper.
 *
 * IMPORTANT:
 * Verify the hidden Fortran CHARACTER-length ABI
 * for the selected compiler.
 * ------------------------------------------------------------ */

extern void F77NAME(uf2dwr)(
    int   *ilun,
    char  *idev,
    char  *zsdate,
    float *f,
    int   *nf1,
    float *x,
    int   *nx,
    float *y,
    int   *ny,
    int   *iproc,
    char  *flab,
    char  *xlab,
    char  *ylab,
    int   *nsc,
    float *scval,
    char  *sclab,
    int   *ier,
    int    ldev,
    int    ldate,
    int    lfl,
    int    lxl,
    int    lyl,
    int    lsclab
);

/* ------------------------------------------------------------
 * MDSplus wrapper
 * ------------------------------------------------------------ */

void c_uf2dwr(
    int   *ilun,
    char  *idev,
    char  *zsdate,
    float *f,
    int   *nf1,
    float *x,
    int   *nx,
    float *y,
    int   *ny,
    int   *iproc,
    char  *flab,
    char  *xlab,
    char  *ylab,
    int   *nsc,
    float *scval,
    char  *sclab,
    int   *ier)
{
    size_t ndev;
    size_t ndate;
    size_t nfl;
    size_t nxl;
    size_t nyl;

    int ldev;
    int ldate;
    int lfl;
    int lxl;
    int lyl;
    int lsclab;

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    if (ilun   == NULL ||
        idev   == NULL ||
        zsdate == NULL ||
        f      == NULL ||
        nf1    == NULL ||
        x      == NULL ||
        nx     == NULL ||
        y      == NULL ||
        ny     == NULL ||
        iproc  == NULL ||
        flab   == NULL ||
        xlab   == NULL ||
        ylab   == NULL ||
        nsc    == NULL ||
        scval  == NULL ||
        sclab  == NULL ||
        ier    == NULL) {

        fprintf(stderr,
            "c_uf2dwr: NULL argument pointer\n");

        if (ier != NULL) {
            *ier = 1;
        }

        return;
    }

    /* --------------------------------------------------------
     * Validate dimensions
     * -------------------------------------------------------- */

    if (*nf1 < 0 ||
        *nx  < 0 ||
        *ny  < 0 ||
        *nsc < 0) {

        fprintf(stderr,
            "c_uf2dwr: invalid dimensions\n");

        *ier = 1;
        return;
    }

    if (*nsc > INT_MAX / 3) {

        fprintf(stderr,
            "c_uf2dwr: nsc exceeds supported range\n");

        *ier = 1;
        return;
    }

    /* --------------------------------------------------------
     * Diagnostic output
     *
     * Original code incorrectly printed pointer values.
     * -------------------------------------------------------- */

    printf("c_uf2dwr: nf1= %d, nx= %d, ny= %d\n",
           *nf1, *nx, *ny);

    printf("c_uf2dwr: nsc= %d\n", *nsc);

    /* --------------------------------------------------------
     * Calculate character lengths
     * -------------------------------------------------------- */

    ndev  = strlen(idev);
    ndate = strlen(zsdate);
    nfl   = strlen(flab);
    nxl   = strlen(xlab);
    nyl   = strlen(ylab);

    /* --------------------------------------------------------
     * Validate conversion to Fortran integer lengths
     * -------------------------------------------------------- */

    if (ndev  > INT_MAX ||
        ndate > INT_MAX ||
        nfl   > INT_MAX ||
        nxl   > INT_MAX ||
        nyl   > INT_MAX) {

        fprintf(stderr,
            "c_uf2dwr: character length exceeds INT_MAX\n");

        *ier = 1;
        return;
    }

    ldev  = (int)ndev;
    ldate = (int)ndate;
    lfl   = (int)nfl;
    lxl   = (int)nxl;
    lyl   = (int)nyl;

    /*
     * Preserve original scalar-label length calculation.
     */

    lsclab = (*nsc) * 3;

    /* --------------------------------------------------------
     * Call UFILES Fortran routine
     * -------------------------------------------------------- */

    F77NAME(uf2dwr)(
        ilun,
        idev,
        zsdate,
        f,
        nf1,
        x,
        nx,
        y,
        ny,
        iproc,
        flab,
        xlab,
        ylab,
        nsc,
        scval,
        sclab,
        ier,
        ldev,
        ldate,
        lfl,
        lxl,
        lyl,
        lsclab
    );

    /* --------------------------------------------------------
     * Preserve legacy diagnostic output
     * -------------------------------------------------------- */

    printf("c_uf2dwr: idev = %s, %d\n",
           idev, ldev);

    printf("c_uf2dwr: xlab = %s, %d\n",
           xlab, lxl);
}

/* End of c_uf2dwr.c */
