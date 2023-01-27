/* C-wrapper for IDL to read 3D Ufile
   by calling uflib fortran routines:
   ufsetr_
   ufoprd_
   uf3drd_
   ufclos_
 
08/13/03 CAL
*/
#include <string.h>
#include <stdio.h>
#include "f77name.h"

int c_uf3dr( int argc , void *argv[] )
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
int   *max_x;
float *y;
int   *max_y;
float *z;
int   *max_z;
int   *iproc;
int   *nx;
int   *ny;
int   *nz;
int   *iflag;
char  *flab;
char  *xlab;
char  *ylab;
char  *zlab;
int   *max_sc;
int   *nsc;
float *scval;
char  *sclab;
int   *ier;
 
  int lpre, lext,  ldir,     ldisk;
  int ldev, ldate, lfl, lxl, lyl, lzl, lsclab;
  int lun,  sts,   i;

  if (argc != 30)
    {
      printf(" --> *ERR* c_uf3dr args should be: \n");
      printf("disk, dir, pref, shot, ext,\n");
      printf("mach, shdate, FLOAT(f), nf1, nf2\n");
      printf("FLOAT(x), max_x, FLOAT(y), max_y, FLOAT(z), max_z\n");
      printf("iproc, nx, ny, nz,\n");
      printf("flab, xlab, ylab, zlab, max_sc, nsc, scval, sclab, ierr \n\n");
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
   max_x = argv[11];
   y = argv[12];
   max_y = argv[13];
   z = argv[14];
   max_z = argv[15];
   iproc = argv[16];
   nx = argv[17];
   ny = argv[18];
   nz = argv[19];
   iflag = argv[20];
   flab = argv[21];
   xlab = argv[22];
   ylab = argv[23];
   zlab = argv[24];
   max_sc = argv[25];
   nsc = argv[26];
   scval = argv[27];
   sclab = argv[28];
   ier = argv[29];

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
    printf("c_uf3dr: ufoprd_ returned error %d\n",*ier);
    return(1);
  }
  /* now read the 3d file */
 
  ldev=5;
  ldate=11;
  lxl=31;
  lyl=31;
  lzl=31;
  lfl=31;
  lsclab = *max_sc * 30;

  F77NAME(uf3drd)(&lun, idev, zsdate, f, nf1, nf2,
	  x, max_x, y, max_y, z, max_z, iproc,
          nx, ny, nz, iflag, 
	  flab, xlab, ylab, zlab,
	  max_sc, nsc, scval, sclab, ier,
	  ldev, ldate, lfl, lxl, lyl, lzl, lsclab);

  *(idev+ldev-1) = '\0';
  *(zsdate+ldate-1) = '\0';
  *(flab+30) = '\0';
  *(xlab+30) = '\0';
  *(ylab+30) = '\0';
  *(zlab+30) = '\0';


  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_uf3dr: uf3drd_ returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
