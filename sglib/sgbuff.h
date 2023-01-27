extern struct{
  int outflg;
  int inflg;
  int outdsk;
  int outtrm;
  int outaux;
  int outps;
} sg_flag_;


extern struct  {
#ifndef __f90
  char *tbufa;
  char *dbufa;
#endif
  int maxtb;
  int maxdb;
  int ktptr;
  int kdptr;
  int ktqio;
  int ktmbx;
  int kxqio;
  int kdqio;
  int kdsav[2];
  int keepc;
#ifdef __f90
  char tbufa[2048];
  char dbufa[512];
#endif
} sg_buff_;

