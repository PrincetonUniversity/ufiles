
/*
 * c_uf3dr.c
 *
 * C wrapper for IDL to read a three-dimensional UFILE
 * using the UFILES Fortran library.
 *
 * Original: 08/13/2003 CAL
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * Preserves the original IDL interface:
 *
 *   int c_uf3dr(int argc, void *argv[])
 *
 * Arguments (30):
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
 *   argv[11]  max_x
 *   argv[12]  y
 *   argv[13]  max_y
 *   argv[14]  z
 *   argv[15]  max_z
 *   argv[16]  iproc
 *   argv[17]  nx
 *   argv[18]  ny
 *   argv[19]  nz
 *   argv[20]  iflag
 *   argv[21]  flab
 *   argv[22]  xlab
 *   argv[23]  ylab
 *   argv[24]  zlab
 *   argv[25]  max_sc
 *   argv[26]  nsc
 *   argv[27]  scval
 *   argv[28]  sclab
 *   argv[29]  ier
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
 * Explicit declarations replace implicit declarations.
 *
 * Trailing character lengths are passed by value,
 * preserving the original wrapper call structure.
 *
 * IMPORTANT:
 * Verify the hidden Fortran CHARACTER-length ABI
 * against the selected compiler and library.
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

extern void F77NAME(uf3drd)(
    int   *lun,
    char  *idev,
    char  *zsdate,
    float *f,
    int   *nf1,
    int   *nf2,
    float *x,
    int   *max_x,
    float *y,
    int   *max_y,
    float *z,
    int   *max_z,
    int   *iproc,
    int   *nx,
    int   *ny,
    int   *nz,
    int   *iflag,
    char  *flab,
    char  *xlab,
    char  *ylab,
    char  *zlab,
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
    int    lzl,
    int    lsclab
);

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_uf3dr(int argc, void *argv[])
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
    int *max_x;
    int *max_y;
    int *max_z;
    int *iproc;
    int *nx;
    int *ny;
    int *nz;
    int *iflag;
    int *max_sc;
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

    int ldisk;
    int ldir;
    int lpre;
    int lext;

    const int ldev  = 5;
    const int ldate = 11;
    const int lfl   = 31;
    const int lxl   = 31;
    const int lyl   = 31;
    const int lzl   = 31;

    int lsclab;

    /* --------------------------------------------------------
     * Validate argument count
     * -------------------------------------------------------- */

    if (argc != 30 || argv == NULL) {

        fprintf(stderr,
            " --> *ERR* c_uf3dr args should be:\n"
            "disk, dir, pref, shot, ext,\n"
            "mach, shdate, FLOAT(f), nf1, nf2,\n"
            "FLOAT(x), max_x, FLOAT(y), max_y,\n"
            "FLOAT(z), max_z, iproc, nx, ny, nz, iflag,\n"
            "flab, xlab, ylab, zlab,\n"
            "max_sc, nsc, scval, sclab, ierr\n");

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
    max_x  = (int *)argv[11];

    y      = (float *)argv[12];
    max_y  = (int *)argv[13];

    z      = (float *)argv[14];
    max_z  = (int *)argv[15];

    iproc  = (int *)argv[16];

    nx     = (int *)argv[17];
    ny     = (int *)argv[18];
    nz     = (int *)argv[19];
    iflag  = (int *)argv[20];

    flab   = (char *)argv[21];
    xlab   = (char *)argv[22];
    ylab   = (char *)argv[23];
    zlab   = (char *)argv[24];

    max_sc = (int *)argv[25];
    nsc    = (int *)argv[26];

    scval  = (float *)argv[27];
    sclab  = (char *)argv[28];

    ier    = (int *)argv[29];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    for (int i = 0; i < argc; ++i) {

        if (argv[i] == NULL) {

            fprintf(stderr,
                "c_uf3dr: NULL argument at index %d\n",
                i);

            return 1;
        }
    }

    *ier = 0;

    /* --------------------------------------------------------
     * Validate array dimensions
     * -------------------------------------------------------- */

    if (*nf1 < 0 ||
        *nf2 < 0 ||
        *max_x < 0 ||
        *max_y < 0 ||
        *max_z < 0 ||
        *max_sc < 0) {

        fprintf(stderr,
            "c_uf3dr: invalid array dimensions\n");

        *ier = 1;
        return 1;
    }

    if (*max_sc > INT_MAX / 30) {

        fprintf(stderr,
            "c_uf3dr: max_sc exceeds supported range\n");

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

    if (ndisk > INT_MAX ||
        ndir  > INT_MAX ||
        npre  > INT_MAX ||
        next  > INT_MAX) {

        fprintf(stderr,
            "c_uf3dr: path string exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    ldisk = (int)ndisk;
    ldir  = (int)ndir;
    lpre  = (int)npre;
    lext  = (int)next;

    /*
     * Preserve original scalar-label buffer length.
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
            "c_uf3dr: ufoprd returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Read three-dimensional UFILE
     * -------------------------------------------------------- */

    F77NAME(uf3drd)(
        &lun,
        idev,
        zsdate,
        f,
        nf1,
        nf2,
        x,
        max_x,
        y,
        max_y,
        z,
        max_z,
        iproc,
        nx,
        ny,
        nz,
        iflag,
        flab,
        xlab,
        ylab,
        zlab,
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
        lzl,
        lsclab
    );

    /* --------------------------------------------------------
     * Preserve original character termination
     *
     * Caller must supply writable buffers with
     * sufficient capacity.
     * -------------------------------------------------------- */

    idev[ldev - 1] = '\0';
    zsdate[ldate - 1] = '\0';

    flab[30] = '\0';
    xlab[30] = '\0';
    ylab[30] = '\0';
    zlab[30] = '\0';

    /* --------------------------------------------------------
     * Close UFILE
     * -------------------------------------------------------- */

    F77NAME(ufclos)(&lun);

    /* --------------------------------------------------------
     * Check read status
     * -------------------------------------------------------- */

    if (*ier != 0) {

        fprintf(stderr,
            "c_uf3dr: uf3drd returned error %d\n",
            *ier);

        return 1;
    }

    return 0;
}

/* End of c_uf3dr.c */
