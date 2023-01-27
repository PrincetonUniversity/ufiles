/* c wrapper for MDSplus */
/*
 To be called by TDI Function tr_tdi/read_ufile.fun  
*/
  
#include <string.h>
#include <stdio.h>
#include "f77name.h"
 
void c_uf1drd(int *ilun, char *idev, char *zsdate, float *x, float *f,
              int *ndim, int *nact, int *iproc,
              char *xlab, char *flab,
              int *nscd, int *nsca, float *scval, char *sclab, int *ier)
{
  int ldev, ldate, lxl, lfl, lsclab;
  int sts;
     ldev=strlen(idev);
     ldate=strlen(zsdate);
     lxl=strlen(xlab);
     lfl=strlen(flab);
     lsclab = *nscd * 3;
     F77NAME(uf1drd)(ilun, idev, zsdate, x, f,
	     ndim, nact, iproc,
	     xlab, flab,
	     nscd, nsca, scval, sclab, ier,
	     ldev, ldate, lxl, lfl, lsclab);
 
     printf("c_uf1drd: idev = %s, %d\n",idev,ldev);
     printf("c_uf1drd: xlab = %s, %d\n",xlab,lxl);
}
