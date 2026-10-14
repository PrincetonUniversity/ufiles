
/*
 * sgsys.c
 *
 * SGLIB system-level I/O functions for Linux.
 *
 * Modernized for:
 *   Intel oneAPI 2023 (icx)
 *   GCC 13.2.0 / 13.3.0
 *
 * C language standard: C11
 *
 * Legacy Fortran entry points are retained.
 */

#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <termios.h>

#include "sgbuff.h"

#define MAXCHAR 128
#define FALSE 0
#define TRUE  1
#define DISK_MODE 0744

/* ------------------------------------------------------------
 * Global variables retained for legacy compatibility.
 * ------------------------------------------------------------ */

char dskfil[MAXCHAR];
char ttyfil[MAXCHAR];
char mbxfil[MAXCHAR];

int dsk_nr = 0;
int tmp_nr = 0;
int tty_nr = 0;
int inp_nr = 0;

static FILE *temp_file = NULL;

static int sd_mbx = -1;
static int sd_gin = -1;

/* ------------------------------------------------------------
 * Function declarations.
 * ------------------------------------------------------------ */

int to_C_nam(char *c_str, const char *f_str, int flen);
int to_FORT_nam(char *f_str, int flen, const char *c_str);
void to_up_str(char *str);

void usr_trnlog_(char *table, char *loglnam, char *physnam,
                 int *ltran, int *igood,
                 int ltabl, int llog, int lphys);

void usr_getdvi_(char *devnam, int *klass,
                 int *igood, int ldevn);

void usr_dclexh_(void);

void tkeep_disk_(void);
void tback_disk_(void);
void tmark_disk_(void);

void tsend_(void);
void tsend_disk_(void);
void tsend_term_(void);

void tname_term_(char *ttynam, int ltty);
void topen_term_(int *allo);

void topen_auxl_(int *allo);

void tback_term_(void);

void tname_auxl_(char *mbxnam, int lmbx);
void tname_disk_(char *dsknam, int ldsk);

void topen_disk_(int *allo);
void tview_disk_(void);

void tread_disk_(char *fbuf, int *mfbuf,
                 int *inblk, int *ngot);

/* Explicit declarations required by C11. */
int lib_get_vm_(int *nbytes, char **addr);
int lib_free_vm_(int *nbytes, char **addr);


/* ------------------------------------------------------------
 * Error reporting.
 * ------------------------------------------------------------ */

static void report_errno(const char *operation)
{
    fprintf(stderr, "SGLIB %s: %s\n",
            operation, strerror(errno));
}


/* ------------------------------------------------------------
 * Reliable write helper.
 * ------------------------------------------------------------ */

static void write_all(int fd, const void *buffer, size_t count)
{
    const unsigned char *p =
        (const unsigned char *)buffer;

    size_t done = 0;

    while (done < count) {

        ssize_t n = write(fd, p + done, count - done);

        if (n < 0 && errno == EINTR)
            continue;

        if (n <= 0) {

            if (n == 0)
                errno = EIO;

            report_errno("write");
            exit(EXIT_FAILURE);
        }

        done += (size_t)n;
    }
}


/* ------------------------------------------------------------
 * Reliable socket send helper.
 * ------------------------------------------------------------ */

static void send_all(int fd, const void *buffer, size_t count)
{
    const unsigned char *p =
        (const unsigned char *)buffer;

    size_t done = 0;

    while (done < count) {

        ssize_t n = send(fd, p + done, count - done, 0);

        if (n < 0 && errno == EINTR)
            continue;

        if (n <= 0) {

            if (n == 0)
                errno = EIO;

            report_errno("send");
            exit(EXIT_FAILURE);
        }

        done += (size_t)n;
    }
}


/* ------------------------------------------------------------
 * Convert Fortran string to C string.
 * ------------------------------------------------------------ */

int to_C_nam(char *c_str, const char *f_str, int flen)
{
    int i;
    int n;

    if (c_str == NULL || f_str == NULL || flen < 0)
        return FALSE;

    n = (flen < MAXCHAR - 1) ? flen : MAXCHAR - 1;

    for (i = 0; i < n && f_str[i] != '\0'; ++i)
        c_str[i] = f_str[i];

    while (i > 0 && c_str[i - 1] == ' ')
        --i;

    c_str[i] = '\0';

    return (i < flen) ? TRUE : FALSE;
}


/* ------------------------------------------------------------
 * Convert C string to Fortran string.
 * ------------------------------------------------------------ */

int to_FORT_nam(char *f_str, int flen, const char *c_str)
{
    size_t n;
    size_t capacity;

    if (f_str == NULL || c_str == NULL || flen < 0)
        return FALSE;

    capacity = (size_t)flen;
    n = strlen(c_str);

    if (n > capacity)
        n = capacity;

    if (n > 0)
        memcpy(f_str, c_str, n);

    if (n < capacity)
        memset(f_str + n, ' ', capacity - n);

    return (c_str[n] == '\0') ? TRUE : FALSE;
}


/* ------------------------------------------------------------
 * Convert string to uppercase.
 * ------------------------------------------------------------ */

void to_up_str(char *str)
{
    if (str == NULL)
        return;

    for (; *str != '\0'; ++str)
        *str = (char)toupper((unsigned char)*str);
}


/* ------------------------------------------------------------
 * Environment-variable translation.
 * ------------------------------------------------------------ */

void USR_TRNLOG(char *table, char *loglnam, char *physnam,
                int *ltran, int *igood,
                int ltabl, int llog, int lphys)
{
    usr_trnlog_(table, loglnam, physnam,
                ltran, igood, ltabl, llog, lphys);
}


void usr_trnlog_(char *table, char *loglnam, char *physnam,
                 int *ltran, int *igood,
                 int ltabl, int llog, int lphys)
{
    char loc_nam[MAXCHAR];
    const char *variable;
    char *copy = NULL;

    (void)table;
    (void)ltabl;

    if (ltran == NULL || igood == NULL)
        return;

    *ltran = 0;
    *igood = FALSE;

    if (loglnam == NULL || physnam == NULL)
        return;

    to_C_nam(loc_nam, loglnam, llog);

    variable = getenv(loc_nam);

    if (variable == NULL) {
        to_FORT_nam(physnam, lphys, " ");
        return;
    }

    if (strcmp(loc_nam, "TERMINAL_TYPE") == 0) {

        copy = strdup(variable);

        if (copy == NULL) {
            report_errno("strdup");
            to_FORT_nam(physnam, lphys, " ");
            return;
        }

        to_up_str(copy);
        variable = copy;
    }

    to_FORT_nam(physnam, lphys, variable);

    {
        size_t len = strlen(variable);

        *ltran = (len > INT_MAX) ?
                 INT_MAX : (int)len;
    }

    *igood = TRUE;

    free(copy);
}


/* ------------------------------------------------------------
 * Device classification.
 * ------------------------------------------------------------ */

void USR_GETDVI(char *devnam, int *klass,
                int *igood, int ldevn)
{
    usr_getdvi_(devnam, klass, igood, ldevn);
}


void usr_getdvi_(char *devnam, int *klass,
                 int *igood, int ldevn)
{
    char loc_nam[MAXCHAR];

    if (klass == NULL || igood == NULL)
        return;

    *klass = 2;
    *igood = FALSE;

    if (devnam == NULL)
        return;

    to_C_nam(loc_nam, devnam, ldevn);

    if (strstr(loc_nam, "xtc_mbx_") != NULL)
        *klass = 5;

    else if (strchr(loc_nam, '.') != NULL)
        *klass = 1;

    *igood = TRUE;
}


/* ------------------------------------------------------------
 * SGLIB fatal-error message.
 * ------------------------------------------------------------ */

void usr_putmsg_(int *igood)
{
    fprintf(stderr, "Fatal error in SGLIB %d\n",
            (igood != NULL) ? *igood : 0);
}


/* ------------------------------------------------------------
 * Exit handler.
 * ------------------------------------------------------------ */

void USR_DCLEXH(void)
{
    usr_dclexh_();
}


void usr_dclexh_(void)
{
    /* Legacy no-op on Linux. */
}


/* ------------------------------------------------------------
 * Copy temporary page to disk.
 * ------------------------------------------------------------ */

void TKEEP_DISK(void)
{
    tkeep_disk_();
}


void tkeep_disk_(void)
{
    char buffer[8192];
    off_t remaining;

    if (tmp_nr <= 0 || dsk_nr <= 0)
        return;

    remaining = lseek(tmp_nr, 0, SEEK_CUR);

    if (remaining < 0) {
        report_errno("temporary-file position");
        return;
    }

    if (lseek(tmp_nr, 0, SEEK_SET) < 0) {
        report_errno("temporary-file rewind");
        return;
    }

    while (remaining > 0) {

        size_t requested =
            remaining > (off_t)sizeof(buffer) ?
            sizeof(buffer) : (size_t)remaining;

        ssize_t n = read(tmp_nr, buffer, requested);

        if (n < 0 && errno == EINTR)
            continue;

        if (n < 0) {
            report_errno("temporary-file read");
            break;
        }

        if (n == 0)
            break;

        write_all(dsk_nr, buffer, (size_t)n);

        remaining -= n;
    }

    if (lseek(tmp_nr, 0, SEEK_SET) < 0)
        report_errno("temporary-file rewind");
}


/* ------------------------------------------------------------
 * Back up temporary disk page.
 * ------------------------------------------------------------ */

void TBACK_DISK(void)
{
    tback_disk_();
}


void tback_disk_(void)
{
    if (tmp_nr > 0) {

        if (lseek(tmp_nr, 0, SEEK_SET) < 0)
            report_errno("temporary-file rewind");
    }

    sg_buff_.kdptr = 0;
}


/* ------------------------------------------------------------
 * Mark disk.
 * ------------------------------------------------------------ */

void TMARK_DISK(void)
{
    tmark_disk_();
}


void tmark_disk_(void)
{
    /* Legacy no-op. */
}


/* ------------------------------------------------------------
 * Send output buffers.
 * ------------------------------------------------------------ */

void TSEND(void)
{
    tsend_();
}


void tsend_(void)
{
    if (sg_flag_.outdsk > 0)
        tsend_disk_();

    if (sg_flag_.outtrm > 0 ||
        sg_flag_.outaux > 0)
        tsend_term_();
}


/* ------------------------------------------------------------
 * Terminal name.
 * ------------------------------------------------------------ */

void TNAME_TERM(char *ttynam, int ltty)
{
    tname_term_(ttynam, ltty);
}


void tname_term_(char *ttynam, int ltty)
{
    to_C_nam(ttyfil, ttynam, ltty);
}


/* ------------------------------------------------------------
 * Open terminal.
 * ------------------------------------------------------------ */

void TOPEN_TERM(int *allo)
{
    topen_term_(allo);
}


void topen_term_(int *allo)
{
    if (strcmp(ttyfil, "stdout") == 0) {

        tty_nr = STDOUT_FILENO;

    } else {

        tty_nr = open(ttyfil, O_WRONLY);

        if (tty_nr < 0) {
            report_errno(ttyfil);
            return;
        }
    }

#ifndef __f90
    if (allo != NULL && *allo)
        sg_buff_.tbufa =
            malloc((size_t)sg_buff_.maxtb);
#else
    (void)allo;
#endif

    sg_flag_.outtrm = 1;
}


/* ------------------------------------------------------------
 * Open auxiliary XTC sockets.
 * ------------------------------------------------------------ */

void TOPEN_AUXL(int *allo)
{
    topen_auxl_(allo);
}


void topen_auxl_(int *allo)
{
    struct sockaddr_un socket_mbx = {0};
    struct sockaddr_un socket_gin = {0};

    char gin_name[MAXCHAR];

    int n;

    n = snprintf(socket_mbx.sun_path,
                 sizeof(socket_mbx.sun_path),
                 "/tmp/%s", mbxfil);

    if (n < 0 ||
        (size_t)n >= sizeof(socket_mbx.sun_path)) {

        fprintf(stderr,
                "SGLIB: auxiliary socket path too long\n");
        return;
    }

    snprintf(gin_name, sizeof(gin_name),
             "%s", mbxfil);

    if (strlen(gin_name) >= 7)
        memcpy(gin_name + 4, "gin", 3);

    n = snprintf(socket_gin.sun_path,
                 sizeof(socket_gin.sun_path),
                 "/tmp/%s", gin_name);

    if (n < 0 ||
        (size_t)n >= sizeof(socket_gin.sun_path)) {

        fprintf(stderr,
                "SGLIB: input socket path too long\n");
        return;
    }

    socket_mbx.sun_family = AF_UNIX;
    socket_gin.sun_family = AF_UNIX;

    sd_mbx = socket(AF_UNIX, SOCK_STREAM, 0);
    sd_gin = socket(AF_UNIX, SOCK_STREAM, 0);

    if (sd_mbx < 0 || sd_gin < 0) {
        report_errno("socket");
        return;
    }

    if (connect(sd_mbx,
                (struct sockaddr *)&socket_mbx,
                sizeof(socket_mbx)) < 0) {

        report_errno("connect auxiliary socket");
        return;
    }

    if (connect(sd_gin,
                (struct sockaddr *)&socket_gin,
                sizeof(socket_gin)) < 0) {

        report_errno("connect input socket");
        return;
    }

#ifndef __f90
    if (allo != NULL && *allo)
        sg_buff_.tbufa =
            malloc((size_t)sg_buff_.maxtb);
#else
    (void)allo;
#endif

    sg_flag_.outaux = 1;
}


/* ------------------------------------------------------------
 * Send terminal buffer.
 * ------------------------------------------------------------ */

void TSEND_TERM(void)
{
    tsend_term_();
}


void tsend_term_(void)
{
    int np = sg_buff_.ktptr;

    if (np <= 0)
        return;

    if (sg_flag_.outtrm == 1)
        write_all(tty_nr, sg_buff_.tbufa,
                  (size_t)np);

    if (sg_flag_.outaux == 1)
        send_all(sd_mbx, sg_buff_.tbufa,
                 (size_t)np);

    sg_buff_.ktptr = 0;
}


/* ------------------------------------------------------------
 * Terminal backspace.
 * ------------------------------------------------------------ */

void tback_term(void)
{
    tback_term_();
}


void TBACK_TERM(void)
{
    tback_term_();
}


void tback_term_(void)
{
    /* Legacy no-op. */
}


/* ------------------------------------------------------------
 * Auxiliary mailbox name.
 * ------------------------------------------------------------ */

void TNAME_AUXL(char *mbxnam, int lmbx)
{
    tname_auxl_(mbxnam, lmbx);
}


void tname_auxl_(char *mbxnam, int lmbx)
{
    to_C_nam(mbxfil, mbxnam, lmbx);
}


/* ------------------------------------------------------------
 * Disk file name.
 * ------------------------------------------------------------ */

void TNAME_DISK(char *dsknam, int ldsk)
{
    tname_disk_(dsknam, ldsk);
}


void tname_disk_(char *dsknam, int ldsk)
{
    to_C_nam(dskfil, dsknam, ldsk);
}


/* ------------------------------------------------------------
 * Open disk output.
 * ------------------------------------------------------------ */

void TOPEN_DISK(int *allo)
{
    topen_disk_(allo);
}


void topen_disk_(int *allo)
{
    dsk_nr = creat(dskfil, DISK_MODE);

    if (dsk_nr < 0) {
        report_errno(dskfil);
        return;
    }

    tmp_nr = 0;

    if (sg_flag_.outtrm != 0) {

        temp_file = tmpfile();

        if (temp_file == NULL) {
            report_errno("tmpfile");
            return;
        }

        tmp_nr = fileno(temp_file);
    }

#ifndef __f90
    if (allo != NULL && *allo)
        sg_buff_.dbufa =
            malloc((size_t)sg_buff_.maxdb);
#else
    (void)allo;
#endif

    sg_flag_.outdsk = 1;
}


/* ------------------------------------------------------------
 * Send disk buffer.
 * ------------------------------------------------------------ */

void TSEND_DISK(void)
{
    tsend_disk_();
}


void tsend_disk_(void)
{
    int np = sg_buff_.kdptr;
    int fd;

    if (np <= 0)
        return;

    fd = (tmp_nr > 0) ? tmp_nr : dsk_nr;

    if (fd <= 0) {
        fprintf(stderr,
                "SGLIB: invalid disk descriptor\n");
        return;
    }

    write_all(fd, sg_buff_.dbufa, (size_t)np);

    sg_buff_.kdptr = 0;
}


/* ------------------------------------------------------------
 * Open disk for reading.
 * ------------------------------------------------------------ */

void TVIEW_DISK(void)
{
    tview_disk_();
}


void tview_disk_(void)
{
    dsk_nr = open(dskfil, O_RDONLY);

    if (dsk_nr < 0)
        report_errno("tview_disk");
}


/* ------------------------------------------------------------
 * Read disk block.
 * ------------------------------------------------------------ */

void TREAD_DISK(char *fbuf, int *mfbuf,
                int *inblk, int *ngot)
{
    tread_disk_(fbuf, mfbuf, inblk, ngot);
}


void tread_disk_(char *fbuf, int *mfbuf,
                 int *inblk, int *ngot)
{
    ssize_t n;

    if (ngot == NULL)
        return;

    *ngot = 0;

    if (dsk_nr < 0 ||
        fbuf == NULL ||
        mfbuf == NULL ||
        inblk == NULL ||
        *mfbuf < 0 ||
        *inblk < 1)
        return;

    if (lseek(dsk_nr,
              ((off_t)*inblk - 1) * 512,
              SEEK_SET) < 0) {

        report_errno("tread_disk seek");
        return;
    }

    do {
        n = read(dsk_nr, fbuf, (size_t)*mfbuf);
    } while (n < 0 && errno == EINTR);

    if (n < 0) {

        report_errno("tread_disk read");
        *ngot = -1;

    } else {

        *ngot = (int)n;
    }
}


/* ------------------------------------------------------------
 * Close disk.
 * ------------------------------------------------------------ */

void tclos_disk_(void)
{
    if (dsk_nr > 0) {
        close(dsk_nr);
        dsk_nr = 0;
    }

    if (temp_file != NULL) {
        fclose(temp_file);
        temp_file = NULL;
        tmp_nr = 0;
    }

    if (sg_flag_.outps == 1) {

        char command[2 * MAXCHAR + 32];

        int n = snprintf(command, sizeof(command),
                         "tek2ps_sh '%s'", dskfil);

        if (n >= 0 &&
            (size_t)n < sizeof(command))
            (void)system(command);
    }
}


/* ------------------------------------------------------------
 * Close terminal.
 * ------------------------------------------------------------ */

void tclos_term_(void)
{
    /* Legacy no-op. */
}


/* ------------------------------------------------------------
 * Terminal input.
 * ------------------------------------------------------------ */

void tinput_(int *inchh)
{
    char trmnam[64];

    int lnam = 0;
    int igood = FALSE;

    if (inchh == NULL)
        return;

    *inchh = 0;

    tsend_term_();

    if (sg_flag_.inflg == 0) {

        char table[] = " ";
        char key[] = "TERMINAL_INPUT";

        usr_trnlog_(table, key, trmnam,
                    &lnam, &igood,
                    1, 14, (int)sizeof(trmnam));

        if (lnam >= (int)sizeof(trmnam))
            lnam = (int)sizeof(trmnam) - 1;

        if (lnam < 0)
            lnam = 0;

        trmnam[lnam] = '\0';

        if (strcmp(trmnam, "NONE") == 0) {

            sg_flag_.inflg = 3;

        } else if (sd_gin >= 0) {

            sg_flag_.inflg = 2;

        } else {

            inp_nr = open("/dev/tty", O_RDONLY);

            sg_flag_.inflg =
                (inp_nr >= 0) ? 1 : 3;
        }
    }

    if (sg_flag_.inflg == 1) {

        struct termios initial;
        struct termios modified;

        unsigned char ch;
        ssize_t n;

        if (tcgetattr(inp_nr, &initial) < 0) {
            report_errno("tcgetattr");
            return;
        }

        modified = initial;

        modified.c_lflag &=
            (tcflag_t)~(ECHO | ICANON);

        modified.c_cc[VMIN] = 1;
        modified.c_cc[VTIME] = 0;

        if (tcsetattr(inp_nr, TCSANOW,
                      &modified) < 0) {

            report_errno("tcsetattr");
            return;
        }

        do {
            n = read(inp_nr, &ch, 1);
        } while (n < 0 && errno == EINTR);

        (void)tcsetattr(inp_nr,
                        TCSANOW, &initial);

        if (n == 1)
            *inchh = (int)ch;

    } else if (sg_flag_.inflg == 2) {

        unsigned char ch;
        ssize_t n;

        do {
            n = recv(sd_gin, &ch, 1, 0);
        } while (n < 0 && errno == EINTR);

        if (n == 1)
            *inchh = (int)ch;
    }
}


/* ------------------------------------------------------------
 * Miscellaneous legacy entry points.
 * ------------------------------------------------------------ */

void iowait_(void)
{
}


void agtrap_(void)
{
}


void agtgbm_(char *msg, int *lmsg)
{
    (void)msg;

    if (lmsg != NULL)
        *lmsg = 0;
}


void agtpurg_(void)
{
}


/* ------------------------------------------------------------
 * Terminal information.
 * ------------------------------------------------------------ */

void ag_info_(int *mode, int *ipid,
              char *myterm, char *user,
              int lterm, int luser)
{
    if (mode != NULL)
        *mode = 3;

    if (ipid != NULL)
        *ipid = 0;

    if (myterm != NULL)
        to_FORT_nam(myterm, lterm, "stdout");

    if (user != NULL)
        to_FORT_nam(user, luser, "USER");
}


/* ------------------------------------------------------------
 * Memory allocation.
 * ------------------------------------------------------------ */

int lib_get_vm_(int *nbytes, char **addr)
{
    if (nbytes == NULL ||
        addr == NULL ||
        *nbytes < 0)
        return FALSE;

    *addr = malloc((size_t)*nbytes);

    return (*addr != NULL) ? TRUE : FALSE;
}


int lib_get_vm(int *nbytes, char **addr)
{
    return lib_get_vm_(nbytes, addr);
}


/* ------------------------------------------------------------
 * Memory release.
 * ------------------------------------------------------------ */

int lib_free_vm_(int *nbytes, char **addr)
{
    (void)nbytes;

    if (addr == NULL)
        return FALSE;

    free(*addr);
    *addr = NULL;

    return TRUE;
}


int lib_free_vm(int *nbytes, char **addr)
{
    return lib_free_vm_(nbytes, addr);
}


/* ------------------------------------------------------------
 * Floating-point size.
 * ------------------------------------------------------------ */

int nfloat_size_(void)
{
    return (int)sizeof(float);
}

/* End of sgsys.c */
