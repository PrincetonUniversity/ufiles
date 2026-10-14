/*
 * sun_error.c
 *
 * Legacy compatibility source for SGLIB/UFILES.
 *
 * Modernized for:
 *   GCC 13.2.0 / 13.3.0
 *   Intel oneAPI 2023 (icx)
 *
 * The original implementation provided a replacement
 * for strerror() on older Sun systems.
 *
 * Modern Linux systems provide strerror() in libc.
 * No replacement is necessary.
 */

#include <string.h>

/*
 * Intentionally empty.
 *
 * Use the standard strerror() provided by libc.
 */
