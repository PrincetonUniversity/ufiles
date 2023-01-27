/* c wrapper for MDSplus */
 
#include <string.h>
#include <stdio.h>
#include "f77name.h"
 
void c_uf2dwr(int *ilun, char *idev, char *zsdate, float *f, int *nf1,
              float *x, int *nx, float *y, int *ny, int *iproc,
               char *flab, char *xlab, char *ylab,
              int *nsc, float *scval, char *sclab, int *ier)
{
 
  int ldev, ldate, lfl, lxl, lyl, lsclab;
  int sts;
 
  printf("c_uf2dwr: nf1= %d, %d, %d\n",nf1,nx,ny);
  printf("c_uf2dwr: nsc= %d\n",nsc);
  ldev=strlen(idev);
  ldate=strlen(zsdate);
  lxl=strlen(xlab);
  lyl=strlen(ylab);
  lfl=strlen(flab);
  lsclab = *nsc * 3;
  F77NAME(uf2dwr)(ilun, idev, zsdate, f, nf1,
	  x, nx, y, ny, iproc,
	  flab, xlab, ylab,
	  nsc, scval, sclab, ier,
	  ldev, ldate, lfl, lxl, lyl, lsclab);
 
  printf("c_uf2dwr: idev = %s, %d\n",idev,ldev);
  printf("c_uf2dwr: xlab = %s, %d\n",xlab,lxl);
}
