
/*
 * c_uf2dw.c
 *
 * C wrapper for IDL to write a two-dimensional UFILE
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
 *   int c_uf2dw(int argc, void *argv[])
 *
 * Arguments (21):
 *
 *   argv[0]   disk
 *   argv[1]   dir
 *   argv[2]   pre
 *   argv[3]   shot
 *   argv[4]   ext
 *   argv[5]   idev
 *   argv[6]   zsdate
 *   argv[7]   f
 *   argv[8]   nf1
 *   argv[9]   x
 *   argv[10]  nx
 *   argv[11]  y
 *   argv[12]  ny
 *   argv[13]  iproc
 *   argv[14]  flab
 *   argv[15]  xlab
 *   argv[16]  ylab
 *   argv[17]  nsc
 *   argv[18]  scval
 *   argv[19]  sclab
 *   argv[20]  ier
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
 * The hidden Fortran CHARACTER-length ABI must be
 * verified for the selected compiler.
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

extern void F77NAME(uf2dwr)(
    int   *lun,
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

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_uf2dw(int argc, void *argv[])
{
    char *disk;
    char *dir;
    char *pre;
    char *ext;

    char *idev;
    char *zsdate;
    char *flab;
    char *xlab;
    char *ylab;
    char *sclab;

    int *shot;
    int *nf1;
    int *nx;
    int *ny;
    int *iproc;
    int *nsc;
    int *ier;

    float *f;
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
    size_t nfl;
    size_t nxl;
    size_t nyl;

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    int ldev;
    int ldate;
    int lfl;
    int lxl;
    int lyl;
    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 21 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf2dw args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate, FLOAT(f), nf1, FLOAT(x), nx,\n"
            "FLOAT(y), ny, iproc,\n"
            "flab, xlab, ylab, nsc, scval, sclab, ierr\n");

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

    f      = (float *)argv[7];
    nf1    = (int *)argv[8];

    x      = (float *)argv[9];
    nx     = (int *)argv[10];

    y      = (float *)argv[11];
    ny     = (int *)argv[12];

    iproc  = (int *)argv[13];

    flab   = (char *)argv[14];
    xlab   = (char *)argv[15];
    ylab   = (char *)argv[16];

    nsc    = (int *)argv[17];
    scval  = (float *)argv[18];
    sclab  = (char *)argv[19];

    ier    = (int *)argv[20];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    for (int i = 0; i < argc; ++i) {

        if (argv[i] == NULL) {

            fprintf(stderr,
                "c_uf2dw: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate dimensions
     * -------------------------------------------------------- */

    if (*nf1 < 0 ||
        *nx < 0 ||
        *ny < 0 ||
        *nsc < 0) {

        fprintf(stderr,
            "c_uf2dw: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*nsc > INT_MAX / 3) {

        fprintf(stderr,
            "c_uf2dw: nsc exceeds maximum supported value\n");

        *ier = 1;
        return 1;
    }

    /* --------------------------------------------------------
     * Calculate file-path string lengths
     * -------------------------------------------------------- */

    ndisk = strlen(disk);
    ndir  = strlen(dir);
    npre  = strlen(pre);
    next  = strlen(ext);

    /* --------------------------------------------------------
     * Calculate UFILE header string lengths
     * -------------------------------------------------------- */

    ndev  = strlen(idev);
    ndate = strlen(zsdate);

    nfl   = strlen(flab);
    nxl   = strlen(xlab);
    nyl   = strlen(ylab);

    /* --------------------------------------------------------
     * Validate conversion to Fortran integer lengths
     * -------------------------------------------------------- */

    if (ndisk > INT_MAX ||
        ndir  > INT_MAX ||
        npre  > INT_MAX ||
        next  > INT_MAX ||
        ndev  > INT_MAX ||
        ndate > INT_MAX ||
        nfl   > INT_MAX ||
        nxl   > INT_MAX ||
        nyl   > INT_MAX) {

        fprintf(stderr,
            "c_uf2dw: string length exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

    ldev  = (int)ndev;
    ldate = (int)ndate;

    lfl   = (int)nfl;
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
            "c_uf2dw: ufopwr returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Write two-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf2dwr)(
        &lun,
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
     * Close UFILE
     * -------------------------------------------------------- */

    F77NAME(ufclos)(&lun);

    /* --------------------------------------------------------
     * Check write status
     * -------------------------------------------------------- */

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf2dw: uf2dwr returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf2dw.c */
