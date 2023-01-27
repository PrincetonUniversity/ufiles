/* c wrapper for MDSplus */
/*
 To be called by TDI Function tr_tdi/read_ufile.fun  
*/
 
#include <string.h>
#include <stdio.h>
#include "f77name.h"
 
void c_ufsetr(int *ilun, char *prefix, char *ext, char *dev, char *dir)
{
  int lp, le, ldev, ldir;
  int sts;
     lp=strlen(prefix);
     le=strlen(ext);
     ldev=strlen(dev);
     ldir=strlen(dir);
     printf("c_ufsetr: dir = %s, %d\n",dir,ldir);
 
     F77NAME(ufsetr)(ilun, prefix, ext, dev, dir, lp, le, ldev, ldir);
}
