
/*
 * c_uf1dw.c
 *
 * C wrapper for IDL to write a one-dimensional UFILE
 * using the UFILES Fortran library.
 *
 * Original: 06/18/2002 CAL
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * Preserves the original IDL calling interface:
 *
 *   int c_uf1dw(int argc, void *argv[])
 *
 * Arguments (17):
 *
 *   argv[0]   disk
 *   argv[1]   dir
 *   argv[2]   pre
 *   argv[3]   shot
 *   argv[4]   ext
 *   argv[5]   idev
 *   argv[6]   zsdate
 *   argv[7]   n
 *   argv[8]   x
 *   argv[9]   y
 *   argv[10]  iproc
 *   argv[11]  xlab
 *   argv[12]  ylab
 *   argv[13]  nsc
 *   argv[14]  scval
 *   argv[15]  sclab
 *   argv[16]  ier
 *
 * Return:
 *   0 - success
 *   1 - error
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "f77name.h"

/* ------------------------------------------------------------
 * Fortran UFILES interfaces
 *
 * Explicit declarations replace legacy implicit
 * function declarations.
 *
 * Trailing character lengths are passed by value,
 * matching the original C wrapper.
 *
 * NOTE:
 * The exact hidden-length ABI must be verified
 * against the compiled Fortran implementation.
 * ------------------------------------------------------------ */

extern void F77NAME(ufsetr)(
    int  *lun,
    char *pre,
    char *ext,
    char *disk,
    char *dir,
    int   lpre,
    int   lext,
    int   ldisk,
    int   ldir
);

extern void F77NAME(ufopwr)(
    int *lun,
    int *shot,
    int *ier
);

extern void F77NAME(uf1dwr)(
    int   *lun,
    char  *idev,
    char  *zsdate,
    float *x,
    float *y,
    int   *n,
    int   *iproc,
    char  *xlab,
    char  *ylab,
    int   *nsc,
    float *scval,
    char  *sclab,
    int   *ier,
    int    ldev,
    int    ldate,
    int    lxl,
    int    lyl,
    int    lsclab
);

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_uf1dw(int argc, void *argv[])
{
    char *disk;
    char *dir;
    char *pre;
    char *ext;

    char *idev;
    char *zsdate;
    char *xlab;
    char *ylab;
    char *sclab;

    int *shot;
    int *n;
    int *iproc;
    int *nsc;
    int *ier;

    float *x;
    float *y;
    float *scval;

    int lun = 91;

    size_t ndisk;
    size_t ndir;
    size_t npre;
    size_t next;

    size_t ndev;
    size_t ndate;
    size_t nxl;
    size_t nyl;

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    int ldev;
    int ldate;
    int lxl;
    int lyl;
    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 17 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf1dw args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate, nx, FLOAT(x), FLOAT(y), iproc,\n"
            "xlab, ylab, nsc, scval, sclab, ierr\n");

        return 1;
    }

    /* --------------------------------------------------------
     * Map IDL arguments
     * -------------------------------------------------------- */

    disk   = (char *)argv[0];
    dir    = (char *)argv[1];
    pre    = (char *)argv[2];
    shot   = (int *)argv[3];
    ext    = (char *)argv[4];

    idev   = (char *)argv[5];
    zsdate = (char *)argv[6];

    n      = (int *)argv[7];
    x      = (float *)argv[8];
    y      = (float *)argv[9];

    iproc  = (int *)argv[10];

    xlab   = (char *)argv[11];
    ylab   = (char *)argv[12];

    nsc    = (int *)argv[13];
    scval  = (float *)argv[14];
    sclab  = (char *)argv[15];

    ier    = (int *)argv[16];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    for (int i = 0; i < argc; ++i) {

        if (argv[i] == NULL) {

            fprintf(stderr,
                "c_uf1dw: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate dimensions
     * -------------------------------------------------------- */

    if (*n < 0 || *nsc < 0) {

        fprintf(stderr,
            "c_uf1dw: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*nsc > INT_MAX / 3) {

        fprintf(stderr,
            "c_uf1dw: nsc exceeds maximum supported value\n");

        *ier = 1;
        return 1;
    }

    /* --------------------------------------------------------
     * Calculate path string lengths
     * -------------------------------------------------------- */

    ndisk = strlen(disk);
    ndir  = strlen(dir);
    npre  = strlen(pre);
    next  = strlen(ext);

    /* --------------------------------------------------------
     * Calculate header string lengths
     * -------------------------------------------------------- */

    ndev  = strlen(idev);
    ndate = strlen(zsdate);
    nxl   = strlen(xlab);
    nyl   = strlen(ylab);

    /* --------------------------------------------------------
     * Check integer conversion limits
     * -------------------------------------------------------- */

    if (ndisk > INT_MAX ||
        ndir  > INT_MAX ||
        npre  > INT_MAX ||
        next  > INT_MAX ||
        ndev  > INT_MAX ||
        ndate > INT_MAX ||
        nxl   > INT_MAX ||
        nyl   > INT_MAX) {

        fprintf(stderr,
            "c_uf1dw: string length exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

    ldev  = (int)ndev;
    ldate = (int)ndate;
    lxl   = (int)nxl;
    lyl   = (int)nyl;

    /*
     * Preserve the original scalar-label length calculation.
     */

    lsclab = (*nsc) * 3;

    /* --------------------------------------------------------
     * Set UFILE path
     * -------------------------------------------------------- */

    F77NAME(ufsetr)(
        &lun,
        pre,
        ext,
        disk,
        dir,
        lpre,
        lext,
        ldisk,
        ldir
    );

    /* --------------------------------------------------------
     * Open UFILE for writing
     * -------------------------------------------------------- */

    F77NAME(ufopwr)(&lun, shot, ier);

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf1dw: ufopwr returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Write one-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf1dwr)(
        &lun,
        idev,
        zsdate,
        x,
        y,
        n,
        iproc,
        xlab,
        ylab,
        nsc,
        scval,
        sclab,
        ier,
        ldev,
        ldate,
        lxl,
        lyl,
        lsclab
    );

    /* --------------------------------------------------------
     * Close UFILE
     * -------------------------------------------------------- */

    F77NAME(ufclos)(&lun);

    /* --------------------------------------------------------
     * Check write status
     * -------------------------------------------------------- */

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf1dw: uf1dwr returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf1dw.c */
