
/*
 * c_uf1dr.c
 *
 * C wrapper for IDL to read a one-dimensional UFILE
 * using the UFILES Fortran library.
 *
 * Original: 08/04/2003 CAL
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * IDL calling convention:
 *
 *   int c_uf1dr(int argc, void *argv[])
 *
 * Required arguments (19):
 *
 *   argv[0]   disk
 *   argv[1]   dir
 *   argv[2]   pre
 *   argv[3]   shot
 *   argv[4]   ext
 *   argv[5]   idev
 *   argv[6]   zsdate
 *   argv[7]   xlab
 *   argv[8]   ylab
 *   argv[9]   nmax
 *   argv[10]  n
 *   argv[11]  x
 *   argv[12]  y
 *   argv[13]  iproc
 *   argv[14]  nscmax
 *   argv[15]  nsc
 *   argv[16]  scval
 *   argv[17]  sclab
 *   argv[18]  ier
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
 * Character lengths are passed by value, preserving
 * the original wrapper's calling convention.
 *
 * IMPORTANT:
 * These declarations assume the legacy Fortran ABI
 * and must be checked against the actual Fortran
 * implementations when changing compilers.
 * ------------------------------------------------------------ */

extern void F77NAME(ufsetr)(
    int *lun,
    char *pre,
    char *ext,
    char *disk,
    char *dir,
    int lpre,
    int lext,
    int ldisk,
    int ldir
);

extern void F77NAME(ufoprd)(
    int *lun,
    int *shot,
    int *ier
);

extern void F77NAME(uf1drd)(
    int *lun,
    char *idev,
    char *zsdate,
    float *x,
    float *y,
    int *nmax,
    int *n,
    int *iproc,
    char *xlab,
    char *ylab,
    int *nscmax,
    int *nsc,
    float *scval,
    char *sclab,
    int *ier,
    int ldev,
    int ldate,
    int lxl,
    int lyl,
    int lsclab
);

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_uf1dr(int argc, void *argv[])
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
    int *nmax;
    int *n;
    int *iproc;
    int *nscmax;
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

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    int ldev = 5;
    int ldate = 11;
    int lxl = 31;
    int lyl = 31;
    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 19 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf1dr args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate, xlab, ylab,\n"
            "nmax, nx, FLOAT(x), FLOAT(y), iproc,\n"
            "nscmax, nsc, scval, sclab, ierr\n");

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

    xlab   = (char *)argv[7];
    ylab   = (char *)argv[8];

    nmax   = (int *)argv[9];
    n      = (int *)argv[10];

    x      = (float *)argv[11];
    y      = (float *)argv[12];

    iproc  = (int *)argv[13];

    nscmax = (int *)argv[14];
    nsc    = (int *)argv[15];

    scval  = (float *)argv[16];
    sclab  = (char *)argv[17];

    ier    = (int *)argv[18];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    for (int i = 0; i < argc; ++i) {

        if (argv[i] == NULL) {

            fprintf(stderr,
                "c_uf1dr: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate array dimensions
     * -------------------------------------------------------- */

    if (*nmax < 0 || *nscmax < 0) {

        fprintf(stderr,
            "c_uf1dr: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*nscmax > INT_MAX / 30) {

        fprintf(stderr,
            "c_uf1dr: nscmax is too large\n");

        *ier = 1;
        return 1;
    }

    lsclab = (*nscmax) * 30;

    /* --------------------------------------------------------
     * Calculate file-path string lengths
     * -------------------------------------------------------- */

    ndisk = strlen(disk);
    ndir  = strlen(dir);
    npre  = strlen(pre);
    next  = strlen(ext);

    if (ndisk > INT_MAX ||
        ndir  > INT_MAX ||
        npre  > INT_MAX ||
        next  > INT_MAX) {

        fprintf(stderr,
            "c_uf1dr: path string exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

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
     * Open UFILE for reading
     * -------------------------------------------------------- */

    F77NAME(ufoprd)(&lun, shot, ier);

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf1dr: ufoprd returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Read one-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf1drd)(
        &lun,
        idev,
        zsdate,
        x,
        y,
        nmax,
        n,
        iproc,
        xlab,
        ylab,
        nscmax,
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
     * Terminate character fields for IDL/C
     *
     * Preserve the original string layout.
     *
     * These writes require the caller to provide
     * writable buffers of the expected sizes.
     * -------------------------------------------------------- */

    idev[ldev - 1] = '\0';
    zsdate[ldate - 1] = '\0';

    xlab[30] = '\0';
    ylab[30] = '\0';

    /* --------------------------------------------------------
     * Close UFILE
     * -------------------------------------------------------- */

    F77NAME(ufclos)(&lun);

    /* --------------------------------------------------------
     * Check read status
     * -------------------------------------------------------- */

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf1dr: uf1drd returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf1dr.c */
