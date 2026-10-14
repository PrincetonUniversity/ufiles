
/*
 * c_uf2dr.c
 *
 * C wrapper for IDL to read a two-dimensional UFILE
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
 *   int c_uf2dr(int argc, void *argv[])
 *
 * Arguments (25):
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
 *   argv[10]  max_x
 *   argv[11]  y
 *   argv[12]  max_y
 *   argv[13]  iproc
 *   argv[14]  nx
 *   argv[15]  ny
 *   argv[16]  iflag
 *   argv[17]  flab
 *   argv[18]  xlab
 *   argv[19]  ylab
 *   argv[20]  max_sc
 *   argv[21]  nsc
 *   argv[22]  scval
 *   argv[23]  sclab
 *   argv[24]  ier
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
 * preserving the original wrapper's call structure.
 *
 * IMPORTANT:
 * Verify the hidden Fortran CHARACTER-length ABI
 * for the selected compiler.
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

extern void F77NAME(ufoprd)(
    int *lun,
    int *shot,
    int *ier
);

extern void F77NAME(uf2drd)(
    int   *lun,
    char  *idev,
    char  *zsdate,
    float *f,
    int   *nf1,
    float *x,
    int   *max_x,
    float *y,
    int   *max_y,
    int   *iproc,
    int   *nx,
    int   *ny,
    int   *iflag,
    char  *flab,
    char  *xlab,
    char  *ylab,
    int   *max_sc,
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

int c_uf2dr(int argc, void *argv[])
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
    int *max_x;
    int *max_y;
    int *iproc;
    int *nx;
    int *ny;
    int *iflag;
    int *max_sc;
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

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    const int ldev  = 5;
    const int ldate = 11;
    const int lfl   = 31;
    const int lxl   = 31;
    const int lyl   = 31;

    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 25 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf2dr args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate,\n"
            "FLOAT(f), nf, FLOAT(x), max_x, FLOAT(y), max_y,\n"
            "iproc, nx, ny, iflag,\n"
            "flab, xlab, ylab, max_sc, nsc, scval, sclab, ierr\n");

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
    max_x  = (int *)argv[10];

    y      = (float *)argv[11];
    max_y  = (int *)argv[12];

    iproc  = (int *)argv[13];
    nx     = (int *)argv[14];
    ny     = (int *)argv[15];
    iflag  = (int *)argv[16];

    flab   = (char *)argv[17];
    xlab   = (char *)argv[18];
    ylab   = (char *)argv[19];

    max_sc = (int *)argv[20];
    nsc    = (int *)argv[21];

    scval  = (float *)argv[22];
    sclab  = (char *)argv[23];

    ier    = (int *)argv[24];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    for (int i = 0; i < argc; ++i) {

        if (argv[i] == NULL) {

            fprintf(stderr,
                "c_uf2dr: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate array dimensions
     * -------------------------------------------------------- */

    if (*nf1 < 0 ||
        *max_x < 0 ||
        *max_y < 0 ||
        *max_sc < 0) {

        fprintf(stderr,
            "c_uf2dr: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*max_sc > INT_MAX / 30) {

        fprintf(stderr,
            "c_uf2dr: max_sc exceeds supported range\n");

        *ier = 1;
        return 1;
    }

    /* --------------------------------------------------------
     * Calculate string lengths
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
            "c_uf2dr: path string exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

    /*
     * Preserve the original scalar-label buffer length.
     */

    lsclab = (*max_sc) * 30;

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
            "c_uf2dr: ufoprd returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Read two-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf2drd)(
        &lun,
        idev,
        zsdate,
        f,
        nf1,
        x,
        max_x,
        y,
        max_y,
        iproc,
        nx,
        ny,
        iflag,
        flab,
        xlab,
        ylab,
        max_sc,
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
     * Preserve legacy string termination
     *
     * Caller must provide writable buffers with
     * the required capacity.
     * -------------------------------------------------------- */

    idev[ldev - 1]   = '\0';
    zsdate[ldate - 1] = '\0';

    flab[30] = '\0';
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
            "c_uf2dr: uf2drd returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf2dr.c */
