/* GWH:  strerror is an ansi standard routine, but I cannot
find it on the SUN's, not even in -lgcc.  Here is a hack
substitute:
 */
char strtmp[32];
char *strerror(int errno)
{
  sprintf(strtmp, "Error in sgsys: %d", errno);
  return(strtmp);
}
