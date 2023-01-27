/* sglib system level i/o functions
*/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

#include <strings.h>
#include <fcntl.h>
#include <sys/un.h>
#include <termios.h>

#include <sys/socket.h>


#include <errno.h>
#include "sgbuff.h"
#define MAXCHAR 128
#define FALSE 0
#define TRUE  1
#define rw__r__r_ 0744

char dskfil[MAXCHAR];
char ttyfil[MAXCHAR];
char mbxfil[MAXCHAR];
int dsk_nr, tmp_nr, tty_nr, inp_nr;

USR_TRNLOG(char *table, char *loglnam, char *physnam, int *ltran,
 int *igood, int ltabl, int llog, int lphys)
{
  usr_trnlog_(table, loglnam, physnam, ltran,igood, ltabl, llog, lphys);
}

usr_trnlog_(char *table, char *loglnam, char *physnam, int *ltran,
 int *igood, int ltabl, int llog, int lphys)
{

  char *variable, loc_nam[MAXCHAR], *getenv();
  /* convert to null delimited string */
  to_C_nam(loc_nam, loglnam, llog);
  /* lookup environmental variables */
  if((variable = getenv(loc_nam)) == NULL) {
    *igood = FALSE;
    to_FORT_nam(physnam, lphys, " ");
    *ltran = 0;
  } else {
    *igood = TRUE;
    if(strcmp(loc_nam,"TERMINAL_TYPE")==0)
      to_up_str(variable); /* SGLIB wants upper case terminal type */
    /* convert to FORTRAN character string */
    to_FORT_nam(physnam, lphys, variable);
    *ltran = strlen(variable);
  }
}

to_up_str(char *str)
{
  for( ; *str != '\0'; str++){
    if(islower(*str)) *str = toupper(*str);
  }
  return;
}

to_C_nam(char *c_str, char *f_str, int flen)
{
  /* convert from FORTRAN character variable to NULL delimited string */
  int i;
  for(i = 0; i < flen && f_str[i] != '\0'; i++) c_str[i] = f_str[i];
  for(i--; i > 0 && c_str[i] == ' '; i--);
  c_str[++i] = '\0';
  if(i == flen) return(FALSE);
  return(TRUE);
}

to_FORT_nam(char *f_str, int flen, char *c_str)
{
  /* convert from NULL delimited character string to FORTRAN character
     variable */
  int i,j;
  for(i = 0; c_str[i] != '\0' && i < flen; i++) f_str[i] = c_str[i];
  for(j = i; j < flen; j++) f_str[j] = ' ';
  if(i == flen) return(FALSE);
  return(TRUE);
}

USR_GETDVI(char *devnam, int *klass, int *igood, int ldevn)
{
  usr_getdvi_(devnam, klass, igood, ldevn);
}
usr_getdvi_(char *devnam, int *klass, int *igood, int ldevn)
{
  char loc_nam[MAXCHAR];
  to_C_nam(loc_nam, devnam, ldevn);
  if(strstr(loc_nam,"xtc_mbx_")) {
    *klass = 5;	/* xtc */
  } else {
  if(strchr(loc_nam,'.')) {
    *klass = 1;  /* disk file */
  } else {
    *klass = 2;  /* terminal */
  }}
  *igood = TRUE;  /* errno */
  return;
}

usr_putmsg_(int *igood)
{
  printf("Fatal error in SGLIB %i\n", igood);
  return;
}
USR_DCLEXH()
{
  usr_dclexh_();
}
usr_dclexh_()
{
  /* signal(SIGINT, &handler) */
}

TKEEP_DISK()
{
  tkeep_disk_();
}
tkeep_disk_()
{
int tsize, ntmp;
char *tcopy;
  if(tmp_nr==0) return;
  /* size of file = current position */
  tsize = lseek(tmp_nr, 0L, 1);
  /* rewind temp file */
  lseek(tmp_nr, 0L, 0);
  /* copy from temp file to disk file */
  tcopy = (char *) malloc(tsize);
  if(tcopy==0) {
    printf("Can't copy page to disk file\n");
  } else {
    ntmp = read(tmp_nr, tcopy, tsize);
    if(ntmp<0) printf("%s\n", strerror(errno));
    ntmp = write(dsk_nr, tcopy, tsize);
    if(ntmp<0) printf("%s\n", strerror(errno));
    free(tcopy);
  }
  /* rewind temp file */
  lseek(tmp_nr, 0L, 0);
}

TBACK_DISK()
{
  tback_disk_();
}
tback_disk_()
{
  if(tmp_nr==0) return;
  /* rewind temp file */
  lseek(tmp_nr, 0L, 0);
  sg_buff_.kdptr = 0;
}

TMARK_DISK()
{
  tmark_disk_();
}
tmark_disk_()
{
}

TSEND()
{
  tsend_();
}
tsend_()
{
  /* empty io buffer to output device(s) */
  if(sg_flag_.outdsk>0) tsend_disk_();
  if(sg_flag_.outtrm>0 || sg_flag_.outaux>0) tsend_term_();
  return;
}

TNAME_TERM(char *ttynam, int ltty)
{
  tname_term_(ttynam, ltty);
}
tname_term_(char *ttynam, int ltty)
{
  to_C_nam(ttyfil, ttynam, ltty);  /* ttyfil = ttynam */
  return;
}

TOPEN_TERM(int *allo)
{
  topen_term_(allo);
}
topen_term_(int *allo)
{
  if(strcmp(ttyfil,"stdout")==0) {
    tty_nr = 1;
  } else {
    tty_nr = open(ttyfil, O_WRONLY, 0);
    if(tty_nr<0) printf("%s: %s\n", strerror(errno),ttyfil);
  }
#ifndef __f90
  if (*allo) sg_buff_.tbufa = (char *) malloc(sg_buff_.maxtb);
#endif
  sg_flag_.outtrm = 1;
  return;
}

static int sd_mbx;	/* xtc output socket descriptor */
static int sd_gin;	/* xtc input socket descriptor */

TOPEN_AUXL(int *allo)
{
  topen_auxl_(allo);
}
topen_auxl_(int *allo)
{
  /* Fill in socket names.  Full path names for sockets are
  ** /tmp/xtc_mbx_#_username and /tmp/xtc_gin_#_username.  */

  /*
Note: 05/26/99 CAL - sa_data size is only 14 char
*/

  struct sockaddr_un socket_mbx;
  struct sockaddr_un socket_gin;

  sprintf( socket_mbx.sun_path, "/tmp/%s\0", mbxfil);
  sprintf( socket_gin.sun_path, "/tmp/%s\0", mbxfil);
  strncpy( &socket_gin.sun_path[9], "gin", 3);

  socket_mbx.sun_family = AF_UNIX;
  socket_gin.sun_family = AF_UNIX;
  sd_mbx = socket(AF_UNIX, SOCK_STREAM, 0);

  if(sd_mbx<0) printf("%s\n", strerror(errno));
  sd_gin = socket(AF_UNIX, SOCK_STREAM, 0);
  if(sd_gin<0) printf("%s\n", strerror(errno));

  if (connect(sd_mbx, &socket_mbx, sizeof(socket_mbx)) == -1)
    printf("%s\n", strerror(errno));
  if (connect(sd_gin, &socket_gin, sizeof(socket_gin)) == -1)
    printf("%s\n", strerror(errno));
#ifndef __f90
  if (*allo) sg_buff_.tbufa = (char *) malloc(sg_buff_.maxtb);
#endif
  sg_flag_.outaux = 1;
  return;
}


TSEND_TERM()
{
  tsend_term_();
}
tsend_term_()
{
  char ch;
  int np, n_written;
  /* empty io buffer to tty */
  if((np = sg_buff_.ktptr)==0) return;
  if(sg_flag_.outtrm==1) {
    if((n_written = write(tty_nr, &sg_buff_.tbufa[0], np)) != np) {
      printf("SGLIB WRITE ERROR: n_written = %d, np = %d\n",
  	   n_written, np);
      exit(1);
    }
  }
  if(sg_flag_.outaux==1) {
    if((n_written = send(sd_mbx, sg_buff_.tbufa, np, 0)) != np) {
      printf("SGLIB SEND ERROR: n_written = %d, np = %d\n",
  	   n_written, np);
      exit(1);
    }
  }
  sg_buff_.ktptr = 0;
  return;
}

tback_term()
{
  tback_term_();
}
TBACK_TERM()
{
  tback_term_();
}
tback_term_()
{
  /* clear the terminal buffer */
   return;
}

TNAME_AUXL(char *mbxnam, int lmbx)
{
  tname_auxl_(mbxnam, lmbx);
}
tname_auxl_(char *mbxnam, int lmbx)
{
  to_C_nam(mbxfil, mbxnam, lmbx);  /* mbxfil = mbxnam */
  return;
}

TNAME_DISK(char *dsknam, int ldsk)
{
  tname_disk_(dsknam, ldsk);
}
tname_disk_(char *dsknam, int ldsk)
{
  to_C_nam(dskfil, dsknam, ldsk);  /* dskfil = dsknam */
  return;
}

TOPEN_DISK(int *allo)
{
  topen_disk_(allo);
}
topen_disk_(int *allo)
{
  FILE *tfile;
  dsk_nr = creat(dskfil, rw__r__r_);
  if(dsk_nr<0) printf("%s: %s\n", strerror(errno),dskfil);
  tmp_nr = 0;
  /* if output to file and tty, open temp file for each page */
  if(sg_flag_.outtrm != 0) {
    tfile = tmpfile();
    tmp_nr = fileno(tfile);
    if(tmp_nr<0) printf("%s\n", strerror(errno));
  }
#ifndef __f90
  if (*allo) sg_buff_.dbufa = (char *) malloc(sg_buff_.maxdb);
#endif
  sg_flag_.outdsk = 1;
  return;
}

TSEND_DISK()
{
  tsend_disk_();
}
tsend_disk_()
{
  int np, n_written;
  /* empty io buffer to disk */
  if((np = sg_buff_.kdptr)==0) return;
  if(tmp_nr>0) {
    n_written = write(tmp_nr, sg_buff_.dbufa, np);
  } else {
    n_written = write(dsk_nr, sg_buff_.dbufa, np);
  }
  /* lseek(dsk_nr, 0L, 2);    set to end of file */
  if(n_written != np) {
    printf("SGLIB WRITE ERROR: n_written = %d, np = %d\n",
	   n_written, np);
    exit(1);
  }
  sg_buff_.kdptr = 0;
  return;
}

TVIEW_DISK()
{
  tview_disk_();
}
tview_disk_()
{
  dsk_nr = open(dskfil, O_RDONLY, 0 );
  if(dsk_nr<0){
    printf("%s\n", strerror(errno));
    printf("tview_disk: Error opening %s dsk_nr= %d\n",dskfil,dsk_nr);
  }
  return;
}

TREAD_DISK(char *fbuf, int *mfbuf, int *inblk, int *ngot)
{
  tread_disk_(fbuf, mfbuf, inblk, ngot);
}
tread_disk_(char *fbuf, int *mfbuf, int *inblk, int *ngot)
{

  int pos,n_read;
  if (dsk_nr <=0) {
    printf("tread_disk: Invalid File Descriptor =  %d\n",dsk_nr);
    *ngot = 0;
    return;
  }
  pos = lseek(dsk_nr, 0L, 1);
  pos = lseek(dsk_nr, (*inblk-1)*512, 0);
  n_read = read(dsk_nr, fbuf, *mfbuf);
  if(n_read<0){
    printf("%s\n", strerror(errno));
    printf("tread_disk: n_read =  %d\n",n_read);
  }
  *ngot = n_read;
  return;
}

void tclos_disk_()
{
  char command_string[100];
  close(dsk_nr);
  dsk_nr = 0;
  if (sg_flag_.outps == 1) {
   sprintf( command_string, "tek2ps_sh %s \0",dskfil );
   system( command_string ); 
  }
  return;
}

void tclos_term_()
{
  /* close terminal file ? */
  return;
}

struct termios init, new;

void tinput_(int *inchh)
{
  char trmnam[64];
  /* char *bufi[6]; */
  char bufi[6];  /* CAL 6/9/98 should be ? */
  int lnam,igood;
  int nin;

  tsend_term_();
  if(sg_flag_.inflg==0) {
    usr_trnlog_(" ","TERMINAL_INPUT",trmnam,&lnam,&igood,
	1,14,64);	/* string lengths */
    trmnam[lnam]='\0';
    if(strcmp(trmnam,"NONE")==0) {
      sg_flag_.inflg = 3;	/* no input */
      goto tin200;
    }
    if(sd_gin>0) {
      sg_flag_.inflg = 2;	/* XTC */
      goto tin200;
      }
    inp_nr = open("/dev/tty", O_RDONLY, 0);
    if(inp_nr>0)
      sg_flag_.inflg = 1;	/* tty input */
    else
      sg_flag_.inflg = 3;	/* error */
  }
tin200:
  if(sg_flag_.inflg==1) {
    /*  fflush(NULL); */
    /* get a single character */
    /* ocne(); set terminal to one character, no echo mode */
    tcgetattr(0, &init);
    tcgetattr(0, &new);
    new.c_lflag = new.c_lflag & ~(ECHO); /* turn off echo */
    new.c_lflag = new.c_lflag & ~(ICANON); /* turn off canonical */
    new.c_cc[VMIN] = 1;		/* one character input */
    new.c_cc[VTIME] = 0;	/* timer off */
    tcsetattr(0, TCSANOW, &new);
    *inchh = (int) getchar();
    /* canon(); reset to canonical mode */
    tcsetattr(0, TCSANOW, &init);
  } else if(sg_flag_.inflg==2) {
    /* read from sd_gin */
    /* nin = recv(sd_gin,inchh,1,0); */
    nin = recv(sd_gin,bufi,1,0);	/* bufi,6 */
    *inchh = (int) *bufi; 
    /* *inchh = (int) bufi[0];  CAL 06/09/98 shold be ? */
  }
  return;
}

void iowait_()
{
}

void agtrap_()
{
}

void agtgbm_(char *msg, int *lmsg)
{
  *lmsg = 0;
}

void agtpurg_()
{
}

void ag_info_(int *mode, int *ipid, char *myterm, char *user, int lterm,
  int luser)
{

/*
  mode = 0(other), 1(network), 2(batch), 3(interactive)
*/
  *mode = 3;	/* interactive */
  *ipid = 0;
  to_FORT_nam(myterm, lterm, "stdout");
  to_FORT_nam(user, luser, "USER");
}

lib_get_vm(int *nbytes, char **addr)
{
  return lib_get_vm_(nbytes, addr);
}
lib_get_vm_(int *nbytes, char **addr)
{
  *addr = (char *) malloc(*nbytes);
  return(TRUE);
}

lib_free_vm(int *nbytes, char **addr)
{
  return lib_free_vm_(nbytes, addr);
}
lib_free_vm_(int nbytes, char **addr)
{
  free(*addr);
}

int nfloat_size_()
{
  float x;
  return sizeof(x);
}
