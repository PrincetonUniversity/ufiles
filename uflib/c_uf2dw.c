/* c wrapper for IDL to call
 uflib fortran routines:
   ufsetr_
   ufopwr_
   uf2dwr_
   ufclos_
 
06/18/02 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"

int c_uf2dw( int argc , void *argv[] )
{
 
char  *disk;
char  *dir;
char  *pre;
int   *shot;
char  *ext;
char  *idev;
char  *zsdate;
float *f;
int   *nf1;
float *x;
int   *nx;
float *y;
int   *ny;
int   *iproc;
char  *flab;
char  *xlab;
char  *ylab;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lfl, lxl, lyl, lsclab;
  int lun,  sts;
 
  if (argc != 21)
    {
      printf(" --> *ERR* c_uf2dw args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, f, maxx, FLOAT(x), nx, FLOAT(y), ny, iproc,\n");
      printf("flab, xlab, ylab, nsc, scval, sclab, ierr \n\n");
      return(1);
    }
 
   disk = argv[0];
   dir = argv[1];
   pre = argv[2];
   shot = argv[3];
   ext = argv[4];
   idev = argv[5];
   zsdate = argv[6];
   f = argv[7];
   nf1 = argv[8];
   x = argv[9];
   nx = argv[10];
   y = argv[11];
   ny = argv[12];
   iproc = argv[13];
   flab = argv[14];
   xlab = argv[15];
   ylab = argv[16];
   nsc = argv[17];
   scval = argv[18];
   sclab = argv[19];
   ier = argv[20];
   /*
  printf("c_uf2dw: shot = %d %d %d %d\n",*shot,*nf1,*nx,*ny);
  printf("c_uf2dw: %10.4e %10.4e %10.4e\n",x[0],x[1],x[*nx-1]);
  printf("c_uf2dw: %10.4e %10.2e %10.4e\n",y[0],y[1],y[*ny-1]);
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
    printf("c_uf2dw: ufopwr_ returned error %d\n",*ier);
    return(1);
  }
  /* now write the 2d file */
 
  ldev=strlen(idev);
  ldate=strlen(zsdate);
  lxl=strlen(xlab);
  lyl=strlen(ylab);
  lfl=strlen(flab);
  lsclab = *nsc * 3;
  F77NAME(uf2dwr)(&lun, idev, zsdate, f, nf1,
	  x, nx, y, ny, iproc,
	  flab, xlab, ylab,
	  nsc, scval, sclab, ier,
	  ldev, ldate, lfl, lxl, lyl, lsclab);
  /*
  printf("c_uf2dw: idev = %s, %d\n",idev,ldev);
  printf("c_uf2dw: xlab = %s, %d\n",xlab,lxl);
  */
  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf2dw: uf2dwr_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
