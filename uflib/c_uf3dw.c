/* c wrapper for IDL to write 3D Ufile
   uflib fortran routines called:
   ufsetr_
   ufopwr_
   uf3dwr_
   ufclos_
 
08/14/03 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"

int c_uf3dw( int argc , void *argv[] )
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
int   *nf2;
float *x;
int   *nx;
float *y;
int   *ny;
float *z;
int   *nz;
int   *iproc;
char  *flab;
char  *xlab;
char  *ylab;
char  *zlab;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lfl, lxl, lyl, lzl, lsclab;
  int lun,  sts;
 
  if (argc != 25)
    {
      printf(" --> *ERR* c_uf3dw args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, FLOAT(f), maxx, maxy, \n");
      printf("FLOAT(x), nx, FLOAT(y), ny, FLOAT(z), nz, iproc,\n");
      printf("flab, xlab, ylab, zlab, nsc, scval, sclab, ierr \n\n");
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
   nf2 = argv[9];
   x = argv[10];
   nx = argv[11];
   y = argv[12];
   ny = argv[13];
   z = argv[14];
   nz = argv[15];
   iproc = argv[16];
   flab = argv[17];
   xlab = argv[18];
   ylab = argv[19];
   zlab = argv[20];
   nsc = argv[21];
   scval = argv[22];
   sclab = argv[23];
   ier = argv[24];
   /*
  printf("c_uf3dw: shot = %d %d %d %d\n",*shot,*nf1,*nx,*ny);
  printf("c_uf3dw: %10.4e %10.4e %10.4e\n",x[0],x[1],x[*nx-1]);
  printf("c_uf3dw: %10.4e %10.2e %10.4e\n",y[0],y[1],y[*ny-1]);
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
    printf("c_uf3dw: ufopwr_ returned error %d\n",*ier);
    return(1);
  }
  /* now write the 3d file */
 
  ldev=strlen(idev);
  ldate=strlen(zsdate);
  lxl=strlen(xlab);
  lyl=strlen(ylab);
  lfl=strlen(flab);
  lzl=strlen(zlab);
  lsclab = *nsc * 3;
  F77NAME(uf3dwr)(&lun, idev, zsdate, f, nf1, nf2,
	  x, nx, y, ny, z, nz, iproc,
	  flab, xlab, ylab, zlab,
	  nsc, scval, sclab, ier,
	  ldev, ldate, lfl, lxl, lyl, lzl, lsclab);
  /*
  printf("c_uf3dw: idev = %s, %d\n",idev,ldev);
  printf("c_uf3dw: xlab = %s, %d\n",xlab,lxl);
  */
  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf3dw: uf3dwr_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
