/* c wrapper for IDL to call
 uflib fortran routines:
   ufsetr_
   ufopwr_
   uf1dwr_
   ufclos_
 
06/18/02 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"
 
int c_uf1dw( int argc , void *argv[] )
{
 
char  *disk;
char  *dir;
char  *pre;
int   *shot;
char  *ext;
char  *idev;
char  *zsdate;
int   *n;
float *x;
float *y;
int   *iproc;
char  *xlab;
char  *ylab;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lxl, lyl, lsclab;
  int lun,  sts;
 
  if (argc != 17)
    {
      printf(" --> *ERR* c_uf1dw args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, nx, FLOAT(x), FLOAT(y), iproc,\n");
      printf("xlab, ylab, nsc, scval, sclab, ierr \n\n");
      return(1);
    }
 
   disk = argv[0];
   dir = argv[1];
   pre = argv[2];
   shot = argv[3];
   ext = argv[4];
   idev = argv[5];
   zsdate = argv[6];
   n = argv[7];
   x = argv[8];
   y = argv[9];
   iproc = argv[10];
   xlab = argv[11];
   ylab = argv[12];
   nsc = argv[13];
   scval = argv[14];
   sclab = argv[15];
   ier = argv[16];
   /*
  printf("c_uf1dw: %10.4e %10.4e %10.4e\n",x[0],x[1],x[*n-1]);
  printf("c_uf1dw: %10.4e %10.2e %10.4e\n",y[0],y[1],y[*n-1]);
   */
  /*set up file path */
  lun = 91;
  ldisk=strlen(disk);
  ldir=strlen(dir);
  lpre=strlen(pre);
  lext=strlen(ext);
 
  F77NAME(ufsetr)(&lun,pre,ext,disk,dir,lpre,lext,ldisk,ldir);
 
  /*open for write */
  F77NAME(ufopwr)(&lun,shot,ier);
  if (*ier) {
    printf("c_uf1dw: ufopwr_ returned error %d\n",*ier);
    return(1);
  }
 
  /* now write the 1d file */
 
  ldev=strlen(idev);
  ldate=strlen(zsdate);
  lxl=strlen(xlab);
  lyl=strlen(ylab);
  lsclab = *nsc * 3;
  F77NAME(uf1dwr)(&lun, idev, zsdate,
	  x, y, n, iproc,
	  xlab, ylab,
	  nsc, scval, sclab, ier,
	  ldev, ldate, lxl, lyl, lsclab);
 
  /*
  printf("c_uf1dw: idev = %s, %d\n",idev,ldev);
  printf("c_uf1dw: xlab = %s, %d\n",xlab,lxl);
  */
  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf1dw: uf1dwr_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
