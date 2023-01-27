/* c wrapper for IDL to call
 uflib fortran routines:
   ufsetr_
   ufoprd_
   uf2drd_
   ufclos_
 
08/04/03 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"

int c_uf2dr( int argc , void *argv[] )
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
int   *max_x;
float *y;
int   *max_y;
int   *iproc;
int   *nx;
int   *ny;
int   *iflag;
char  *flab;
char  *xlab;
char  *ylab;
int   *max_sc;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lfl, lxl, lyl, lsclab;
  int lun,  sts,   i;

  if (argc != 25)
    {
      printf(" --> *ERR* c_uf2dr args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, \n");
      printf("FLOAT(f), nf, FLOAT(x), max_x, FLOAT(y), max_y,\n");
      printf("iproc, nx, ny,\n");
      printf("flab, xlab, ylab, max_sc, nsc, scval, sclab, ierr \n\n");
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
   max_x = argv[10];
   y = argv[11];
   max_y = argv[12];
   iproc = argv[13];
   nx = argv[14];
   ny = argv[15];
   iflag = argv[16];
   flab = argv[17];
   xlab = argv[18];
   ylab = argv[19];
   max_sc = argv[20];
   nsc = argv[21];
   scval = argv[22];
   sclab = argv[23];
   ier = argv[24];

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
    printf("c_uf2dr: ufoprd_ returned error %d\n",*ier);
    return(1);
  }
  /* now read the 2d file */
 
  ldev=5;
  ldate=11;
  lxl=31;
  lyl=31;
  lfl=31;
  lsclab = *max_sc * 30;

  F77NAME(uf2drd)(&lun, idev, zsdate, f, nf1,
	  x, max_x, y, max_y, iproc,
          nx, ny, iflag, 
	  flab, xlab, ylab,
	  max_sc, nsc, scval, sclab, ier,
	  ldev, ldate, lfl, lxl, lyl, lsclab);

  *(idev+ldev-1) = '\0';
  *(zsdate+ldate-1) = '\0';
  *(flab+30) = '\0';
  *(xlab+30) = '\0';
  *(ylab+30) = '\0';
  /*
  printf("c_uf2dr: idev = %s, %d\n",idev,ldev);
  printf("c_uf2dr: flab = %s, %d\n",flab,lfl);
  printf("c_uf2dr: xlab = %s, %d\n",xlab,lxl);
  printf("c_uf2dr: ylab = %s, %d\n",ylab,lyl);

  printf("c_uf2dr: shot = %d %d %d %d\n",*shot,*nf1,*nx,*ny);
  printf("c_uf2dr: %10.4e %10.4e %10.4e\n",x[0],x[1],x[*nx-1]);
  printf("c_uf2dr: %10.4e %10.2e %10.4e\n",y[0],y[1],y[*ny-1]);
  */
  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf2dr: uf2drd_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
