/*
  dmc 4 June 1999--

  this is the body of code used repeatedly in sgvarg.c routines
  to use either an optional fortran int* passed length, or to use
  the character variables int length ... as the length of the string
  label used by the sglib AGT routines.

  ** implementation of variable no. of arguments -- portable on unix **

  History:
  02/10/00 cal -- check  0 < pval <= 256

 */

  va_list ap;
  int* lenp;
  long pval;
  int len;

  va_start(ap,str);

  lenp = va_arg(ap, int*);
  pval = (long)lenp;
  if ( pval >0 && pval <= 256 ) {
    len = pval;
    lenp = &zero;
  } else {
    len = va_arg(ap, int);
  }
  va_end(ap);
