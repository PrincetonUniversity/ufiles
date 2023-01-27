#define      NCCS    19
#define      ICANON  0000002
#define ECHO    0000010
#define VMIN    4
#define VTIME   5
#define _TIOC   ('T'<<8)
#define TCGETS          (_TIOC|13)
#define TCSANOW         (_TIOC|14) /* same as TCSETS */

typedef unsigned int tcflag_t;
typedef unsigned char cc_t;

/*
 * Ioctl control packet
 */
struct termios {
        tcflag_t        c_iflag;        /* input modes */
        tcflag_t        c_oflag;        /* output modes */
        tcflag_t        c_cflag;        /* control modes */
        tcflag_t        c_lflag;        /* line discipline modes */
        cc_t            c_cc[NCCS];     /* control chars */
};

