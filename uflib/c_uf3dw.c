
/*
 * c_uf3dw.c
 *
 * C wrapper for IDL to write a three-dimensional UFILE
 * using the UFILES Fortran library.
 *
 * Original: 08/14/2003 CAL
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * IDL calling interface:
 *
 *   int c_uf3dw(int argc, void *argv[])
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
 *   argv[9]   nf2
 *   argv[10]  x
 *   argv[11]  nx
 *   argv[12]  y
 *   argv[13]  ny
 *   argv[14]  z
 *   argv[15]  nz
 *   argv[16]  iproc
 *   argv[17]  flab
 *   argv[18]  xlab
 *   argv[19]  ylab
 *   argv[20]  zlab
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
 * Explicit declarations replace implicit function
 * declarations rejected by modern C compilers.
 *
 * The trailing CHARACTER lengths are passed by value,
 * preserving the original wrapper's argument order.
 *
 * IMPORTANT:
 * Verify the actual hidden Fortran CHARACTER-length
 * ABI for the selected compiler.
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

extern void F77NAME(uf3dwr)(
    int   *lun,
    char  *idev,
    char  *zsdate,
    float *f,
    int   *nf1,
    int   *nf2,
    float *x,
    int   *nx,
    float *y,
    int   *ny,
    float *z,
    int   *nz,
    int   *iproc,
    char  *flab,
    char  *xlab,
    char  *ylab,
    char  *zlab,
    int   *nsc,
    float *scval,
    char  *sclab,
    int   *ier,
    int    ldev,
    int    ldate,
    int    lfl,
    int    lxl,
    int    lyl,
    int    lzl,
    int    lsclab
);

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_uf3dw(int argc, void *argv[])
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
    char *zlab;
    char *sclab;

    int *shot;
    int *nf1;
    int *nf2;
    int *nx;
    int *ny;
    int *nz;
    int *iproc;
    int *nsc;
    int *ier;

    float *f;
    float *x;
    float *y;
    float *z;
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
    size_t nzl;

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    int ldev;
    int ldate;
    int lfl;
    int lxl;
    int lyl;
    int lzl;
    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 25 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf3dw args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate, FLOAT(f), nf1, nf2,\n"
            "FLOAT(x), nx, FLOAT(y), ny, FLOAT(z), nz,\n"
            "iproc, flab, xlab, ylab, zlab,\n"
            "nsc, scval, sclab, ierr\n");

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
    nf2    = (int *)argv[9];

    x      = (float *)argv[10];
    nx     = (int *)argv[11];

    y      = (float *)argv[12];
    ny     = (int *)argv[13];

    z      = (float *)argv[14];
    nz     = (int *)argv[15];

    iproc  = (int *)argv[16];

    flab   = (char *)argv[17];
    xlab   = (char *)argv[18];
    ylab   = (char *)argv[19];
    zlab   = (char *)argv[20];

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
                "c_uf3dw: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate dimensions
     * -------------------------------------------------------- */

    if (*nf1 < 0 ||
        *nf2 < 0 ||
        *nx < 0 ||
        *ny < 0 ||
        *nz < 0 ||
        *nsc < 0) {

        fprintf(stderr,
            "c_uf3dw: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*nsc > INT_MAX / 3) {

        fprintf(stderr,
            "c_uf3dw: nsc exceeds supported range\n");

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

    nfl = strlen(flab);
    nxl = strlen(xlab);
    nyl = strlen(ylab);
    nzl = strlen(zlab);

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
        nyl   > INT_MAX ||
        nzl   > INT_MAX) {

        fprintf(stderr,
            "c_uf3dw: string length exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

    ldev  = (int)ndev;
    ldate = (int)ndate;

    lfl = (int)nfl;
    lxl = (int)nxl;
    lyl = (int)nyl;
    lzl = (int)nzl;

    /*
     * Preserve original scalar-label length calculation.
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
            "c_uf3dw: ufopwr returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Write three-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf3dwr)(
        &lun,
        idev,
        zsdate,
        f,
        nf1,
        nf2,
        x,
        nx,
        y,
        ny,
        z,
        nz,
        iproc,
        flab,
        xlab,
        ylab,
        zlab,
        nsc,
        scval,
        sclab,
        ier,
        ldev,
        ldate,
        lfl,
        lxl,
        lyl,
        lzl,
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
            "c_uf3dw: uf3dwr returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf3dw.c */
