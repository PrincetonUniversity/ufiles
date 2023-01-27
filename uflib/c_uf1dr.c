/* c wrapper for IDL to call
 uflib fortran routines:
   ufsetr_
   ufoprd_
   uf1drd_
   ufclos_
 
08/04/03 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"
 
int c_uf1dr( int argc , void *argv[] )
{
 
char  *disk;
char  *dir;
char  *pre;
int   *shot;
char  *ext;
char  *idev;
char  *zsdate;
char  *xlab;
char  *ylab;
int   *nmax;
int   *n;
float *x;
float *y;
int   *iproc;
int   *nscmax;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lxl, lyl, lsclab;
  int lun,  sts,   i;
 
  if (argc != 19)
    {
      printf(" --> *ERR* c_uf1dr args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, nmax, nx, FLOAT(x), FLOAT(y), iproc,\n");
      printf("xlab, ylab, nscmax, nsc, scval, sclab, ierr \n\n");
      return(1);
    }
 
   disk = argv[0];
   dir = argv[1];
   pre = argv[2];
   shot = argv[3];
   ext = argv[4];
   idev = argv[5];
   zsdate = argv[6];
   xlab = argv[7];
   ylab = argv[8];
   nmax = argv[9];
   n = argv[10];
   x = argv[11];
   y = argv[12];
   iproc = argv[13];
   nscmax = argv[14];
   nsc = argv[15];
   scval = argv[16];
   sclab = argv[17];
   ier = argv[18];

  /*set up file path */
  lun = 91;
  ldisk=strlen(disk);
  ldir=strlen(dir);
  lpre=strlen(pre);
  lext=strlen(ext);
 
  F77NAME(ufsetr)(&lun,pre,ext,disk,dir,lpre,lext,ldisk,ldir);
 
  /*open for read */
  F77NAME(ufoprd)(&lun,shot,ier);
  if (*ier) {
    printf("c_uf1dr: ufoprd_ returned error %d\n",*ier);
    return(1);
  }

  /* now read the 1d file */

  ldev=5;
  ldate=11;

  lxl=31;
  lyl=31;
  lsclab = *nscmax * 30;


  /* Input:lun, nmax, nscmax */ 
  F77NAME(uf1drd)(&lun, idev, zsdate,
	  x, y, nmax, n, iproc,
	  xlab, ylab,
	  nscmax, nsc, scval, sclab, ier,
	  ldev, ldate, lxl, lyl, lsclab);

  *(idev+ldev-1) = '\0';
  *(zsdate+ldate-1) = '\0';
  *(xlab+30) = '\0';
  *(ylab+30) = '\0';
  /*
  printf("c_uf1dr: %10.4e %10.4e %10.4e\n",x[0],x[1],x[*n-1]);
  printf("c_uf1dr: %10.4e %10.2e %10.4e\n",y[0],y[1],y[*n-1]);
  printf("c_uf1dr: idev = %s, %d\n",idev,ldev);
  printf("c_uf1dr: xlab = %s, %d\n",xlab,lxl);
  printf("c_uf1dr: ylab = %s, %d\n",ylab,lyl);
  */
  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf1dr: uf1drd_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
