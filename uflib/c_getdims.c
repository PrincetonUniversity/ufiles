
/*
 * c_getdims.c
 *
 * C wrapper for IDL to obtain header information
 * from a UFILE using the UFILES Fortran library.
 *
 * Original: 08/08/2003 CAL
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.x
 *
 * C standard: C11
 *
 * IDL calling convention:
 *
 *   int c_getdims(int argc, void *argv[])
 *
 * Arguments:
 *   argv[0]  disk  - disk name
 *   argv[1]  dir   - directory
 *   argv[2]  pre   - prefix
 *   argv[3]  shot  - shot number
 *   argv[4]  ext   - extension
 *   argv[5]  info  - header information array
 *   argv[6]  ier   - error status
 *
 * Return values:
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
 * The string lengths are passed explicitly by value,
 * matching the original C wrapper.
 *
 * These declarations assume the legacy Fortran ABI
 * used by UFILES.
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

extern void F77NAME(ufhdck)(
    int *lun,
    int *info,
    int *ier
);

extern void F77NAME(ufclos)(
    int *lun
);

/* ------------------------------------------------------------
 * IDL wrapper
 * ------------------------------------------------------------ */

int c_getdims(int argc, void *argv[])
{
    char *disk;
    char *dir;
    char *pre;
    char *ext;

    int *shot;
    int *info;
    int *ier;

    int lun = 91;

    size_t npre;
    size_t next;
    size_t ndisk;
    size_t ndir;

    int lpre;
    int lext;
    int ldisk;
    int ldir;

    /* --------------------------------------------------------
     * Validate number of arguments
     * -------------------------------------------------------- */

    if (argc != 7 || argv == NULL) {

        fprintf(stderr,
            "c_getdims: invalid arguments\n"
            "Expected: disk, dir, pre, shot, ext, info, ier\n");

        return 1;
    }

    /* --------------------------------------------------------
     * Map IDL arguments
     * -------------------------------------------------------- */

    disk = (char *)argv[0];
    dir  = (char *)argv[1];
    pre  = (char *)argv[2];
    shot = (int *)argv[3];
    ext  = (char *)argv[4];
    info = (int *)argv[5];
    ier  = (int *)argv[6];

    /* --------------------------------------------------------
     * Validate pointers
     * -------------------------------------------------------- */

    if (disk == NULL ||
        dir  == NULL ||
        pre  == NULL ||
        shot == NULL ||
        ext  == NULL ||
        info == NULL ||
        ier  == NULL) {

        fprintf(stderr,
            "c_getdims: NULL argument pointer\n");

        return 1;
    }

    /* --------------------------------------------------------
     * Initialize error status
     * -------------------------------------------------------- */

    *ier = 0;

    /* --------------------------------------------------------
     * Calculate string lengths
     * -------------------------------------------------------- */

    npre  = strlen(pre);
    next  = strlen(ext);
    ndisk = strlen(disk);
    ndir  = strlen(dir);

    /*
     * Legacy Fortran interface expects integer lengths.
     */

    if (npre  > INT_MAX ||
        next  > INT_MAX ||
        ndisk > INT_MAX ||
        ndir  > INT_MAX) {

        fprintf(stderr,
            "c_getdims: string length exceeds INT_MAX\n");

        *ier = 1;
        return 1;
    }

    lpre  = (int)npre;
    lext  = (int)next;
    ldisk = (int)ndisk;
    ldir  = (int)ndir;

    /* --------------------------------------------------------
     * Set up UFILE path
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
            "c_getdims: ufoprd returned error %d\n",
            *ier);

        return 1;
    }

    /* --------------------------------------------------------
     * Read UFILE header information
     * -------------------------------------------------------- */

    F77NAME(ufhdck)(&lun, info, ier);

    if (*ier != 0) {

        fprintf(stderr,
            "c_getdims: ufhdck returned error %d\n",
            *ier);
    }

    /* --------------------------------------------------------
     * Close UFILE
     *
     * Preserve the header error status because ufclos
     * does not receive ier.
     * -------------------------------------------------------- */

    F77NAME(ufclos)(&lun);

    if (*ier != 0) {
        return 1;
    }

    return 0;
}

/* End of c_getdims.c */
