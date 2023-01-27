/* C-wrapper for IDL to get header information of a Ufile
   by calling uflib fortran routines:
   ufsetr
   ufoprd
   ufhdck
   ufclos
 
08/08/03 CAL
*/
#include <string.h>
#include <stdio.h>

#include "f77name.h"
 
int c_getdims( int argc , void *argv[] )
{
char  *disk;
char  *dir;
char  *pre;
int   *shot;
char  *ext;
 int  *info;
int   *ier;
 
int lpre, lext,  ldir,     ldisk;
int i,    lun,   sts;
 
  if (argc != 7)
    {
      printf(" --> *ERR* c_getdims args should be: \n");
      printf("disk, dir, pre, shot, ext, info, ier\n");
      return(1);
    }
 
   disk = argv[0];
   dir  = argv[1];
   pre  = argv[2];
   shot = argv[3];
   ext  = argv[4];
   info = argv[5];
   ier  = argv[6];

  /*set up file path */
  lun = 91;
  ldir=strlen(dir);
  lpre=strlen(pre);
  lext=strlen(ext);
  ldisk=strlen(disk);

  F77NAME(ufsetr)(&lun,pre,ext,disk,dir,lpre,lext,ldisk,ldir);
 
  /*open for read */
  F77NAME(ufoprd)(&lun,shot,ier);
  if (*ier) {
    printf("c_getdims: ufoprd returned error %d\n",*ier);
    return(1);
  }

  /* now read header */

  F77NAME(ufhdck)(&lun, info, ier);
  if (*ier) {
    printf("c_getdims: ufhdck returned error %d\n",*ier);
  }

  /* close the file */
  F77NAME(ufclos)(&lun);
  if (*ier) {
    printf("c_getdims: ufhdck returned error %d\n",*ier);
    return(1);
  }
  return(0);
}
