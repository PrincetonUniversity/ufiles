!--------------------------------------------------------------
!
!  UFILES MODULE
!
!  J. Breslau   18 Feb 2021 -- converted from common blocks to module
!  D. McCune    21 Jun 1999 -- removed declaration of byte arrays
!                              (portability issue)
!  C. Ludescher 13 Aug 1997 -- Added ufid(ufpach) for netCDF file id
!  T. Terpstra  20 Aug 1993 -- Changed Uextnd from C*3 to C*16
!  D. MC CUNE   19 NOV 1981
!  M. THOMPSON, 28 MAR 1984
!
! MODIF DMC MAR 1985 - EXPANDING FILENAME ELEMENTS
!  STORING NONBLANK LENGTH OF EACH NAME ELEMENT
!  SUPPORTING FILENAME FORMS WHICH DO NOT INCL. A SHOT NUMBER
!
!  COMMON BLOCKS FOR ASCII DATA FILE SYSTEM
!  PARAMETERS:  UFPACH-- MAX NO. OF LUN "CHANNELS"
!   =MAX NO. OF CALLS TO UFSETR WITH DISTINCT LUNS.
!               UFPASC-- NUMBER OF SCALARS TO READ, UF1DRE, UF2DRE
!
!  MOD DMC - ADDED VARIABLE UFILES COMPRESSED RECORD SIZE UFRSIZ
!   ADDED PARAMETER UFMAXR = MAX COMPRESSED RECORD SIZE
!   6/86
MODULE ufiles
  IMPLICIT NONE
  SAVE
!
  INTEGER, PARAMETER :: UFMAXR=8192  ! MAX COMPRESSED UFILE RECORD SIZE
  INTEGER, PARAMETER :: UFPACH=64    ! max no. of UFILE channels
  INTEGER, PARAMETER :: UFPASC=300   ! max no. of scalars for UFnDRE...
  INTEGER, PARAMETER :: UFBBUF=512   ! byte stream i/o buffer size
  INTEGER, PARAMETER :: UFCBUF=2048  ! char buffer size
!
  CHARACTER*16 UPRFIX(UFPACH)
  CHARACTER*16 UEXTND(UFPACH)
  CHARACTER*16 UDEV(UFPACH),UDDEV
  CHARACTER*512 UFDIR(UFPACH)
  CHARACTER*60 UJUNKA

!
!  dmc 15 Dec 1998 -- MDS+ data access option
!
  character*50 mds_server(ufpach)  !mds+ server
  character*20 mds_tree(ufpach)    !mds+ tree name
  character*120 mds_path(ufpach)   !mds+ path name
  character*20 mds_node(ufpach)    !mds+ node name
  integer      mds_socket(ufpach)  !mds+ server socket
  integer      mds_shot(ufpach)    !mds+ pulse number

!
  LOGICAL LUFSET(UFPACH),LSHOTN(UFPACH),LFCLUN
  INTEGER, DIMENSION(UFPACH) :: UFLUN,UFSTAT,UFSHOT,UFCPRS,LFDIR,LDEV,  &
                                LPRFIX,LEXTND,ufid
  INTEGER LUFCAP,UFCHAN,UFELUN,UFCLUN,UFILEV,LDDEV,UFRSIZ
  REAL UFCTOL(UFPACH)

!
!  new stuff added dmc 18 Mar 1996 -- support new compressed binary
!  format, loss-free compression & binary portability.
!
  integer niobuf(ufpach)      ! i/o buffer count
  integer nchbuf(ufpach)      ! char buffer count
!
  real ufieee(ufmaxr)         ! IEEE floating pt buffer
  integer*2 uffhi(ufmaxr)     ! IEEE fp compression buffer
  integer*2 ufflo(ufmaxr)     ! IEEE fp compression buffer
  integer ufpack(ufmaxr)      ! generic compression buffer
!
!  LUFSET(ICHAN)=.TRUE. ONLY IF CHANNEL HAS BEEN INITIALIZED
!   (SEE SUBROUTINE UFSETR, UFILES.FOR)
!  UPRFIX,UEXTND,UFDIR, AND UDEV ARE FILENAME PARTS, DEVICES
!   AND FILE DIRECTORY FOR EACH CHANNEL
!  UDDEV-- DEFAULT DEVICE (DISK) FOR FILES - SEE BLOCK DATA
!  UFLUN(ICHAN)= LOGICAL UNIT NUMBER ASSOCIATED WITH CHANNEL ICHAN
!  UFSTAT(ICHAN)= CHANNEL STATUS:
!   0-- CHANNEL CLOSED
!   1-- CHANNEL (FILE) OPEN FOR WRITE, NO DATA WRITTEN
!   2-- CHANNEL (FILE) OPEN FOR WRITE,DATA HAS BEEN WRITTEN
!   3-- CHANNEL (FILE) OPEN FOR READ ONLY
!   4-- open as temporary ascii file for comment spooling
!   5-- CHANNEL (FILE) OPEN FOR READ, DATA HAS BEEN READ.
!  UFCPRS-- COMPRESSION FLAG: 0=NO COMPRESSION,
!   1= LSL COMPRESSION OF INTERGIZED DATA
!  UFCTOL-- USED IF COMPRESSION FLAG IS SET.  ISSUE WARNING IF REAL
!   TO INTEGER CONVERSION RESULTS IN RELATIVE ERROR EXCEEDING UFCTOL
!   DUE TO LINEAR RANGE DIGITIZATION OF FLOATING POINT NUMBER.
!   IMBED WARNING IN COMMENTS OF OUTPUT FILE
!  UFCHAN-- # OF CHANNELS AVAILABLE-- SEE BLOCK DATA IN UFILES.FOR
!
!  UFELUN-- CHANNEL FOR ERROR MESSAGE OUTPUT
!  UFCLUN-- CHANNEL FOR INTERNALLY GENERATED COMMENTS TO SPOOL TO
!   END OF UFILES OUTPUT FILE
!  LFCLUN-- FLAG INDICATING PRESENCE OF INTERNALLY GENERATED COMMENTS.
!-----------
!  MAR 1985
!  LPRFIX - CHARACTER LENGTH OF FILENAME PREFIX
!  LEXTND - CHARACTER LENGTH OF FILENAME .EXT EXTENSION
!  LFDIR - CHARACTER LENGTH OF FILENAME DIRECTORY SPECIFICATION
!  LDEV - CHARACTER LENGTH OF FILE DEVICE SPECIFICATION
!  LDDEV - DITTO FOR DEFAULT DEVICE
!
!  LSHOTN - .TRUE. TO ENCODE SHOT NO. IN FILENAME
!  LUFCAP - =1 for capitalized filename NAME#####.EXT (default)
!           =0 for lower case name#####.ext.  Unix option.
!           =2 for fully user specified capitalization in name & ext
!
!----------------------------------------------------------------
END MODULE ufiles
