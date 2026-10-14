
/*
 * mdsdummy.c
 *
 * Dummy MDSplus interface for UFILES / TRANSP.
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.2.0
 *
 * C standard: C11
 *
 * Purpose:
 *   Provide placeholder MDSplus functions when the
 *   real MDSplus library is not linked.
 *
 * All functions:
 *   - Preserve the original exported symbol names.
 *   - Print the original diagnostic message.
 *   - Return -1 to indicate unavailable functionality.
 *
 * No actual MDSplus operations are performed.
 */

#include <stdio.h>

/* ------------------------------------------------------------
 * Return code for unsupported MDSplus operations.
 * ------------------------------------------------------------ */

#define MDS_DUMMY_ERROR (-1)

/* ------------------------------------------------------------
 * Internal diagnostic helper.
 * ------------------------------------------------------------ */

static int mds_dummy_error(const char *message)
{
    puts(message);
    return MDS_DUMMY_ERROR;
}

/* ------------------------------------------------------------
 * Forward declarations.
 *
 * Explicit prototypes prevent implicit-function-declaration
 * errors with modern C compilers.
 * ------------------------------------------------------------ */

int mds__cstring(void);
int mds__cstring_(void);

int mds__long(void);
int mds__long_(void);

int mdsput(void);
int mdsput_(void);
int MdsPut(void);

int mdsdisconnect(void);
int mdsdisconnect_(void);
int MdsDisconnect(void);

int mdsconnect(void);
int mdsconnect_(void);
int MdsConnect(void);

int mdsvalue(void);
int mdsvalue_(void);
int MdsValue(void);

int mdsopen(void);
int mdsopen_(void);
int MdsOpen(void);

int mdsclose(void);
int mdsclose_(void);
int MdsClose(void);


/* ============================================================
 * MDS__CSTRING
 * ============================================================ */

int mds__cstring(void)
{
    return mds__cstring_();
}

int mds__cstring_(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy mds__cstring call (MDSplus)."
    );
}


/* ============================================================
 * MDS__LONG
 * ============================================================ */

int mds__long(void)
{
    return mds__long_();
}

int mds__long_(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy mds__long call (MDSplus)."
    );
}


/* ============================================================
 * MDSPUT
 * ============================================================ */

int mdsput(void)
{
    return MdsPut();
}

int mdsput_(void)
{
    return MdsPut();
}

int MdsPut(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsPut call."
    );
}


/* ============================================================
 * MDSDISCONNECT
 * ============================================================ */

int mdsdisconnect(void)
{
    return MdsDisconnect();
}

int mdsdisconnect_(void)
{
    return MdsDisconnect();
}

int MdsDisconnect(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsDisconnect call."
    );
}


/* ============================================================
 * MDSCONNECT
 * ============================================================ */

int mdsconnect(void)
{
    return MdsConnect();
}

int mdsconnect_(void)
{
    return MdsConnect();
}

int MdsConnect(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsConnect call."
    );
}


/* ============================================================
 * MDSVALUE
 * ============================================================ */

int mdsvalue(void)
{
    return MdsValue();
}

int mdsvalue_(void)
{
    return MdsValue();
}

int MdsValue(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsValue call."
    );
}


/* ============================================================
 * MDSOPEN
 * ============================================================ */

int mdsopen(void)
{
    return MdsOpen();
}

int mdsopen_(void)
{
    return MdsOpen();
}

int MdsOpen(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsOpen call."
    );
}


/* ============================================================
 * MDSCLOSE
 * ============================================================ */

int mdsclose(void)
{
    return MdsClose();
}

int mdsclose_(void)
{
    return MdsClose();
}

int MdsClose(void)
{
    return mds_dummy_error(
        " __mdsdummy.c:  dummy MdsClose call."
    );
}

/* End of mdsdummy.c */
