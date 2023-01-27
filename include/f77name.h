#ifndef F77NAME_H  /*  F77NAME(c_name) make c_name fortran-callable */
#define F77NAME_H  /*  dmc 9 Apr 1999 */

#if __IBM  || __HP || __ABS || defined(__xlC__)
#define F77NAME(name) name
#endif

/* fallthru:  append the "_", which most systems want. */

#ifndef F77NAME
#define F77NAME(name) name##_
#endif

#endif
