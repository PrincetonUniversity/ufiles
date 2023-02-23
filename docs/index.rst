.. ufiles documentation master file, created by
   sphinx-quickstart on Wed Feb 22 13:15:46 2023.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Ufiles Documentation
====================

.. toctree::
   :maxdepth: 2
   :caption: Contents:


Ufiles are a standard way for storing physics data across various magnetic confinement fusion devices (tokamaks). This software project started in the 1980s at the Princeton Plasma Physics Laboratory by Doug McCune and has been maintained and developed by many researchers since then. This software includes utility programs and service subroutines to support users. The utility programs are menu driven command line oriented interactive programs with Tektronix (SGLIB) graphics capability. Fortran subroutines for user written software are stored in subroutine libraries.

Ufiles are essentially an early data standard for single precision (32 bit) floating point data, that has been used heavily in tokamak and fusion energy research both in the USA and in Europe and Japan.

The Ufiles software was developed at Plasma Physics Laboratory of Princeton University, under U.S. DoE contract DE-AC02-76-CH0-3073.

Software Availability
=====================

Previously the Ufiles software was included as part of the TRANSP software repository. To give access to the scientific community it was also included in the National Transport Code Collaboration (NTCC) Modules Library effort and could be downloaded from the NTCC_ website. However, this software has been marked for deprecation from the TRANSP code and has been placed in the Ufiles_ software repository. This is a private repo but should be made public soon. Early access can be requested by sending an email to: transp_support@pppl.gov

.. _NTCC: https://w3.pppl.gov/NTCC
.. _Ufiles: https://github.com/PrincetonUniversity/ufiles


History
=======

Ufiles were originally developed to facilitate preparation of data for the TRANSP code analyses of PDX tokamak data at PPPL. The design aimed at two goals:

 #. Be able to write the data on the data acquisition DEC-10 machine but read it on a VAX 11/780;
 #. Be able to process data from different diagnostics using common utility programs for smoothing, averaging, etc.

The resulting standardization of physics results data (and removal of most format peculiarities specific to diagnostic, tokamak, or data acquisition system) has proven extremely useful, both for processing of the data and for sharing the data on a wide variety of machines. Ufiles have since been used to carry the data from many tokamaks to many computers around the world. Most of the Ufiles software was developed in the early 1980s on DEC VAX minicomputers.


Contributors
============

There have been many people who have contributed to the Ufiles software over many decades. A list of contributors was harvested from the TRANSP repository and is ordered by appearance in the commit log (oldest to newest). An honest attempt has been made to make this list as comprehensive as possible, so please let us know if there are any glaring omissions. This acknowledgment is meant for developers of the legacy version of Ufiles and not new work going forward.

Doug McCune (original architect); Marilee Thompson; JP Jeral; Ted Terpstra; Richard Wieland; PM Stubber; John Schivell; Christina Ludescher-Furth (most commits); Rob Andre; Eliot Feibush; Jim Conboy; Alex Pletzer; Robert Budny; Greg Colyer; Ben Bariteau; Marina Gorelenkova; Keshavamurthy Indireshkumar; Xingqiu Yuan; Johan Carlsson; Jai Sachdev; Garrett Wright; Joshua Breslau; Francesca Poli; Gopan Perumpilly


Precision
=========

The main Ufiles routines are all written in single precision; all floating point arrays are declared ``REAL``. The Ufiles data formats store single precision (32 bit) floating point data. However, a parallel interface is maintained for codes which use ``REAL*8`` to declare floating point arrays. For each named routine in the Ufiles library, that name prepended with ``R8_`` gives the ``REAL*8`` interface routine name. Thus, to call ``UF1DRD`` from a ``REAL*8`` code, use ``R8_UF1DRD``, replacing all ``REAL`` arguments and arrays with ``REAL*8`` arguments and arrays. Similarly, to call UF2DWR from a ``REAL*8`` code, use ``R8_UF2DWR``.


Data Formats
============

Five Ufiles file format variants are supported:

 * ASCII -- portable from machine to machine -- 7 decimal digits of precision.
 * Compressed binary -- NOT portable -- 16 bit precision -- fast and compact. Accuracy not sufficient for some applications. Not recommended for new Ufiles but many old TFTR Ufiles are in this format.
 * Loss-free portable compressed binary -- 100% bit for bit accurate network portable compressed binary Ufiles data -- transferable btw all brands of unix and VMS via binary ftp.
 * NetCDF -- NetCDF Ufiles may be written, if the Ufiles software is linked with the NetCDF binary library(s).
 * HDF -- HDF Ufiles may be written, if the Ufiles software is linked with the HDF binary library(s).

Ufiles reading software deals automatically with all formats. However, the user needs to be somewhat concerned when writing Ufiles as a format variant must be considered. The Ufiles software default is ASCII. However, many applications override this to set their own default. Standard Ufiles utilities offer user control over output file formats as a menu option.


Data Structures
===============

Ufiles are generally described as having *dimensionality*. A Ufile may be 0d, 1d, 2d, or 3d. A 0d Ufile contains scalars only. Higher dimensional Ufiles contain dimensioned data plus optional scalars.

A fundamental rule of Ufiles -- a kind of atomicity -- is that each Ufile contains at most one data structure of dimensionality 1 or more. This *one item per file* rule is the key to the simplicity, flexibility and portability of Ufiles.

 *  Scalar -- scalar numbers associated with keywords, labels and physical units. Any Ufile may contain zero or more associated scalars.
 *  1d -- discreet representation of data of the form ``f(x)`` where ``x`` is usually a monotonic increasing sequence. Ufiles do not require ``x`` to be monotonic increasing but some utility programs do depend on this.
 *  2d -- discreet representation of data of the form ``f(x,y)``.
 *  3d -- discreet representation of data of the form ``f(x,y,z)``.

The vast majority of existing Ufiles are either 1d or 2d. Software using Ufiles generally must know in advance the dimensionality of the Ufiles being read.


Filenames
=========

Ufiles filenames have 3 parts:

 #. A prefix (up to 16 characters long, uppercase letters)
 #. A shot number (5 digits)
 #. A suffix (up to 16 characters long, uppercase letters).

Example: ``T12345.CUR`` where ``T`` is the prefix, ``12345`` is the shot number, and ``CUR`` is the suffix.

Ufiles software also allows specification of a disk and directory name (VMS) or a path name (unix) -- see the section on applications programming. An option exists for suppressing the shot number field.


Utilities
=========

A rich collection of utility programs exist for the display and manipulation of Ufiles data. They share the following common features:

 * Ufiles data format input/output
 * ``sglib`` (Tektronix) graphics
 * Interactive menu driven command line user i/o using the UREAD system (described elsewhere) to allow scripting and automation of routine operations
 * Ufiles filename set up via interactive menu.
 * Utilities which write Ufiles generate comments describing the Ufile manipulation & copy comments from the input Ufile. Also, user comments are solicited. Thus the comment section of Ufiles contains a history of utility manipulations applied to the data.

The utility programs are intended to be largely self documenting through their menu interfaces. The following list summarizes Ufile utility programs currently in existence:

Display
*******

This utilities make Tektronix displays of Ufile data:

 * ``ulook0`` look at scalar Ufile data
 * ``ugraf1`` look at 1d Ufile data
 * ``ugraf2`` look at 2d Ufile data
 * ``ugraf3`` look at 3d Ufile data
 * ``uftype`` read a Ufile header, type out dimensionality and formatting information.

Averaging
*********

Utilities which average or sum data from multiple input Ufiles; output the average or sum to a single Ufile. Some interpolation capability is provided in ``gaver1`` and ``gaver2``: the first input file's grid becomes the interpolation target for subsequent input files in case their grids do not match.

 * ``gaver0`` average scalar lists
 * ``gaver1`` average/sum 1d Ufile data
 * ``gaver2`` average/sum 2d Ufile data

Smoothing
*********

Apply "local triangular weighted average" smoothing to Ufile data. Also permit limited arithmetic, e.g. linear transformations: ``x --> a*x + b`` on data axes (units transformations, e.g. ``a=1000``, ``b=0`` to take KeV to eV).

 * ``gsmoo1`` smooth 1d data
 * ``gsmoo2`` smooth 2d data

Smoothing programs also have some capability for deglitching data, by means of automatic algorithms and by means of interactive cutting and pasting. These features are not effective on all data.

Advanced Utilities
******************

 * ``concat`` -- concatenator, combine a sequence of 1d Ufiles to form a 2d Ufile. Or, add 1d slices to an existing 2d Ufile
 * ``extrac`` -- extractor, extract 1d slices from an existing 2d Ufile or extract a 2d subset from a 2d Ufile
 * ``splice`` -- splice a single 1d Ufile ``f(x)`` using selected intervals from input Ufiles ``f1(x)``, ``f2(x)``, ``f3(x)``, ...
 * ``splice2`` -- same as splice but using 2d Ufiles on input and output. All ufiles share the same grid in the direction not spliced
 * ``thin1`` -- thin the x coordinate of a 1d Ufile ``f(x)``, to make it smaller
 * ``thin2`` -- thin out the x and y coordinates of a 2d Ufile ``f(x,y)`` to make it smaller

Specialized Utilities
*********************

Discussion with experienced PPPL user is recommended prior to use of these programs.

* (``ecprog`` manipulate ECE data; frequency --> radius maps valid for simple tokamak geometries; recalibration and edge trimming options) - Not available as part of the Ufiles repository
* ``sawtoo`` apply pattern matching algorithm to detect sawtooth event times from input 1d Ufile data. Output 1d Ufile of sawtooth event times (suitable for input to TRANSP).


Utility Demo Scripts
====================

Note: these are not currently available in the Ufiles repo but we will try to provide them soon.

The NTCC Modules Library distribution includes a test directory with demo scripts that illustrate the capabilities of the Ufiles utilities.

These are ``uread`` scripts -- they encode the user side of a dialog with a ``uread`` menu driven interactive Fortran program. Each script contains contains a complete session with a specific Ufiles utility. Ufiles data required by the script sessions are included in the same directory with the scripts themselves.

Execution of the scripts results in the output of *Tektronix* style graphics (supported by ordinary unix xterm, or by the xtc application available with the NTCC Ufiles software distribution); also messages are written to the terminal window in which the demo is invoked. Typically, demo scripts will pause to allow the user to view each graph before going on to the next graph (but other behaviors are possible, depending on the user's configuration). The graphs illustrate the utilities' capabilities; the text provides some supporting explanation.

The scripts are ascii text. The user may want to print out a copy of the script and then follow along in an interactive session with the utility, in order to see the program menus and prompts.

What follows is a list of the demo scripts provided, and a summary of their contents. Assuming the utility binaries have been built and made available as commands (on the user's PATH in UNIX), the demo scripts are invoked by typing ``<utility-name> @<utility-name>.demo`` from the directory containing the scripts.

Display
*******

 * ``ugraf1 @ugraf1.demo`` -- plot 1d Ufiles -- show some 1d display style options and rescaling capability.

 * ``ugraf2 @ugraf2.demo`` -- plot 2d Ufiles -- show 2d display types including slice plots, isometric plots, contour plots, and multi-slice plots.

(All utilities have similar display capabilities).

Basic Averaging and Smoothing
*****************************

 * ``gaver1 @gaver1.demo`` -- demonstrate averaging of data -- average several 1d profiles.

 * ``gsmoo1 @gsmoo1.demo`` -- 1d smoothing -- smooth an ECE time trace with internal breaks inserted for preservation of pellet injection events.

 * ``gsmoo2 @gsmoo2.demo`` -- 2d smoothing -- smooth f(x,t) first in x, then in t.

Hand generated Profiles and Signals
***********************************

 * ``propane @propane.demo`` -- generate an analytic profile; generate a hand entered waveform.

Methods for Extracting, Combining, or Thinning Ufiles Data
**********************************************************

 * ``concat @concat.demo`` -- demonstrate concatenation of multiple 1d profiles to form a 2d profile evolution Ufile. Add extrapolated profiles at endpoints.

 * ``extrac @extrac.demo`` -- extract 1d slices and 2d sub-blocks from a 2d input Ufile.

 * ``splice @splice.demo`` -- join 1d Ufile time trace segments end to end, to form a new 1d Ufile timetrace.

 * ``splice2 @splice2.demo`` -- "splice" method applied to profile evolution 2d Ufiles.

 * ``thin1 @thin1.demo`` -- thin (resample at lower rate) 1d Ufile data, with averaging option.

 * ``thin2 @thin2.demo`` -- thin method applied to 2d Ufile profile evolution function.

Sawtooth Event Recognition
**************************

 * ``sawtoo @sawtoo.demo`` --  use a correlation method applied to smoothing residue to yield a sequence of sawtooth event times.


Applications Programming
========================

Ufiles are opened, read, and written via calls to subroutines in a Fortran subroutine library. The process involves:

 #. Setting up the filename                - see file_setup
 #. Opening the file                       - see file_setup
 #. Reading or writing the data            - see scalar/1d/2d
 #. (optionally) dealing with comment data - see comments
 #. Closing the file                       - see file_setup

Ancillary calls are available to define file format (ascii or compressed binary) or modify error handling. Ufiles software can also access MDSplus -- READ access only.

Linking to Ufiles
*****************

Linking to the Ufiles libraries depends on what the application needs. The NetCDF-4 libraries should be available on your system. You will need to specify the location of the NetCDF-4 include directory when compiling:

.. code-block:: bash

   -I`nf-config --includedir`

and this in the link line:

.. code-block:: bash

   -L` nf-config --flibs`
		
If MDSplus is available on your system then you will need to include this while compiling:

.. code-block:: bash

   -I$(MDSPLUS_DIR)/include

and this in the link line:

.. code-block:: bash

   -L$(MDSPLUS_DIR)/lib -lMdsLib

where ``MDSPLUS_DIR`` should point to the directory where MDSplus is installed.

Linking to the Ufiles libraries can be done as follows, where ``UFILES_ROOT`` specifies the location of the Ufiles installation, 

.. code-block:: bash

   -L$UFILES_ROOT/lib -luflib_userio -luflib -lr4smlib_userio -lr4smlib -lmds_sub -lufhdf \
		-lmdstransp -linterp_sub -lextrac_sub -ltrgraf -lureadsub -ltokid -lufencod \
		-lsg -ljc -lufport

This includes the MDSplus interface and the sglib graphics package. If you do not need the sglib graphics package then replace ``-lsg -ljc`` with ``-lsgdummy``. If you do not have MDSplus installed then replace ``-lmds_sub`` with ``-lmds_dummy``.

lun
***

The acronym "lun" stands for "logical unit number" and is used by Fortran to keep track of open files.

The lun is used similarly by the Ufiles system. Most Ufiles calls have the lun as the first argument; this tells the Ufiles software to which file the call applies. Applications can maintain multiple Ufile streams open in parallel; lun is used to distinguish between them.

Fortran allows lun values in the range from 0 to 99. However, it is recommended to use values in the range 21 to 99, because the lower numbers are typically used by the system (e.g. 5 and 6 for terminal i/o), and numbers between 10 and 20 may be used by the ``ureadsub`` library, if the application uses this.

If the application opens any file on any logical unit number, that lun should not be used for Ufiles until after the file is closed.

File Setup
**********

UFSETR is used to set up the naming and location convention
you will use. The disk and directory fields may be blank, in 
which case the files will default to the current working directory.

.. code-block:: fortran

        call UFSETR(lun,prefix,suffix,disk,directory)
              ! lun - integer
              ! prefix - character, max length 16
              ! suffix - character, max length 16
              ! disk - character, max length 16
              ! directory - character, max length 64

For example:

.. code-block:: fortran

        call UFSETR(lun,'T','CUR',' ','/usr/ufiles/data')
                                !To access /usr/ufiles/data/T*.CUR
                                !leave "disk" argument blank

``ufsetr`` does not set the entire filename: the shot number has not yet been given. This is given as an integer argument to the open subroutine (``ufoprd`` or ``ufopwr``). The shot number is a positive integer of no more than six digits.

then

.. code-block:: fortran

        call UFOPRD(lun,ishot,ierr)     !Open for READONLY access
        call UFOPWR(lun,ishot,ierr)     !Open for WRITE access
         ! ...check ierr for errors, ierr=0 indicates success

         ! ...read/write the data (code depends on what kind)

        call UFCLOS(lun)                !Close the file

See also the section on compression if you are writing Ufiles.


Suppress Shot Number
********************

Use a call to the subroutine ``UFNSHO`` to suppress the encoding of the shot number in the Ufile name.

Thus, to read a Ufile called ``ABC.DAT``, do:

.. code-block:: fortran

      call ufsetr(lun,'ABC','DAT',' ',' ')
      call ufnsho(lun)
      call ufoprd(lun,idummy,ierr)
       ! ... ...
      call ufclos(lun)

note the 2nd argument of ufoprd is ignored. The same is true for ``ufopwr`` if the Ufile is being written.

The normal sequence would have been

.. code-block:: fortran

      call ufsetr(lun,'ABC','DAT',' ',' ')
      ishot=12345
      call ufoprd(lun,ishot,ierr)
       ! ... ...
      call ufclos(lun)

which would read the Ufile ``ABC12345.DAT``.


MDSplus Setup
*************

``UFSETR`` is used to identify the MDSplus server, treename, path, and nodename of an MDSplus signal that is to be read by Ufiles software. (The applications program needs to know the dimensionality of the signal in advance).

The general form of such a call is:

.. code-block:: fortran

        call UFSETR(lun,server,tree_name,path,node)

Note this is the same routine as is used to setup access to standard Ufiles. To distinguish MDSplus access from standard Ufile access, the server string must start with ``MDS+::``. If it does not, then standard Ufile access is assumed.

.. code-block:: fortran

            lun - integer
            server - character, max length 55 including "MDS+::"
            tree_name - character, max length 20
            path - character, max length 120
            node - character, max length 20

The full path string to the desired data is 

.. code-block:: fortran

   path(1:ilp)//':'//node(1:iln)

where ilp is the non-blank length of "path" (stripping off also any trailing colon), and iln is the non-blank length of "node".

actual example:

.. code-block:: fortran

        call UFSETR(lun,
     >              'MDS+::TRANSPGRID.PPPL.GOV',
     >              'TRANSP_NSTX',
     >              'TRANSP_OUT',
     >              'PCUR')

PPPL TRANSP trees have their own numbering system; shot 1027 has data for the above path configuration.

Note that, as with traditional file access, ``UFSETR`` does not access the data. The shot number of the MDSplus tree has not been given yet. As with traditional file access, the shot number is given via a call to ``UFOPRD``, which in this mode calls the appropriate MDSplus client software to connect to the MDSplus server, and open up the named tree. ``UF1DRD`` or ``UF2DRD`` will be used to go to the indicated path in the tree and actually read the signal data. Access to data comments is not available at this time.

So:

.. code-block:: fortran

        call UFOPRD(lun,ishot,ierr)     !Open tree #ishot for READ
         ! ...check ierr for errors, ierr=0 indicates success

         ! ...use UF1DRD or UF2DRD or similar routine to read the signal; check status code.

         ! ...WRITE access is NOT available

        call UFCLOS(lun)                !Close the tree

Scalar
******

To write an array of N scalars:

.. code-block:: fortran

        call UF0DWR(lun,tdev*4,sdate*10,values(N),labels(3,N)*10,N,ierr)

``tdev`` labels the tokamak: 'TFTR', 'PDX', 'PLT', 'PBX',..., max of four characters, ``character*4``
``sdate`` should contain a ``character*10`` shot date e.g. ``'29-FEB-89 '``

for the i'th scalar, ``i = 1 to N``:

``values(i)`` is the i'th scalar value, ``real``

``label(1,i)`` is a 1-9 character keyword followed by a ":".
``label(2,i)`` is a 10 character descriptive label
``label(3,i)`` is the physical units.

all element of the label array are ``character*10``.

Similarly, to read scalar values:

.. code-block:: fortran

       call UF0DRD(lun,tdev,sdate,values,labels,Nmax,Nactual,ierr)

if the operation is successful ``ierr=0`` is set on return.

1-d Arrays
**********

To write a 1-d array, you need arrays for both ``x`` and ``f(x)``.

.. code-block:: fortran

       call UF1DWR(lun,tdev,sdate,x,f,n,iproc,xlab,flab,nsc,scval,sclab,ier)

The variables ``tdev``, ``sdate`` are as under Scalars. The variables at the end containing "sc" are like those defined under Scalars.

The ones special to a 1-d call are:

.. code-block:: fortran

   The real arrays x,f and their size n.
   IPROC: process code 0 for Translated from raw data (integer)
                       1 for Averaged
                       2 for Smoothed
                       3 for Smoothed and Averaged
                       4 for "other".

``XLAB``/``FLAB`` contain 20 characters of label, 10 characters of units -- total ``CHARACTER*30`` or 3 adjacent ``CHARACTER*10`` array elements.

To read the data, labels and scalars:

.. code-block:: fortran

       call UF1DRD(lun,tdev,sdate,x,f,nmax,n,iproc,xlab,flab,
                   nscmax,nsc,scval,sclab,ier)

To read just the data, and dispose of the rest to unit lunxt:

.. code-block:: fortran

       call UF1DRE(lun,x,f,nmax,n,ier,lunxt)

2-d Arrays
**********

To write 2-d data for a function ``f(x,y)`` where ``f`` is a 2d array (e.g. declared ``REAL f(nf1,nymax)`` with ``nf1``, ``nymax`` as array dimension parameters):

.. code-block:: fortran

       call UF2DWR(lun,tdev,sdate,            !just like for scalars
                   f,nf1,x,nx,y,ny,iproc,     !similar to 1-d arguments
                   flab,xlab,ylab,            !character*30, like 1-d args.
                   nsc,scval,sclab,ier)       !just like for scalars

``uf2dwr`` declares real arrays ``f(nf1,ny)``, ``x(nx)``, and ``y(ny)`` where ``nf1.ge.nx`` is required.

.. code-block:: fortran

call UF2DRD(lun,tdev,sdate,
            f,nf1,x,nxmax,y,nymax,iproc,      !as above
            nx,ny,                            !actuals returned
            iflag,                            ! ** see below **
            flab,xlab,ylab,                   !as above
            nscmax,nsc,scval,sclab,ier)       !as in scalars

``iflag`` controls the dimensioning of ``f`` as seen by ``uf2drd``.

* If ``iflag=0`` then ``uf2drd`` assumes the callers declares ``f(nf1,nymax)`` and reads the data using the loop specification ``read(...) ((f(ix,iy),ix=1,nx),iy=1,ny)``. It is expected that ``nx.le.nf1``, otherwise an error is reported. If ``nx.lt.nf1``, then, not all the elements of f will be set by the read statement.

* ``iflag=1`` then ``uf2drd`` assumes the caller declares ``f(nfmax)`` as a large buffer, and wants the data read *contiguously* into the ``f`` array. In this case, set ``nf1=nfmax/nymax`` on input. An error will occur if ``nx*ny`` is greater than ``nf1*nymax``, i.e. if reading the data would write past the end of the buffer in memory.

* For either value of iflag, the independent coordinate arrays ``x(nxmax)``, ``y(nymax)`` are declared, and an error occurs if in the data file either ``nx.gt.nxmax`` or ``ny.gt.nymax``.

On exit, ``ier=0`` indicates normal successful operation. Applications programs should check.

Format Variants
***************

In addition to the traditional Ascii Ufile format, there are four binary Ufile format variants:  16-bit compressed binary, 32-bit loss free portable compressed binary, NetCDF, and HDF.

The 16 bit Compressed binary Ufiles occupy ~ 1/10th the disk space of ascii Ufiles, and can be read in about 1/5th the time (or faster), with no significant loss of data accuracy in most cases (compression involves specifying scale factors and using a 16 bit digitization of the floating point data. Inaccuracy can occur in analyzed data with wide dynamic range for which this 16 bit digitization is inadequate).

 * 16-bit binary compressed Ufiles are not portable across machine architectures!
 * 16-bit binary compressed Ufiles are small and fast but their use involves loss of data precision!

Read access to Ufile data in any format is automatic; the application does not have to know the format of the Ufile it is reading.

To set up for a Ufile write, choosing a non-Ascii format variant:

*After* the ``UFSETR`` call, *prior* to the ``UFOPWR`` call, add

.. code-block:: fortran

      call UFCMPR(lun,iarg) !iarg = 1 for compressed binary (NOT RECOMMENDED due to data loss)
                            !iarg = 2 for loss free portable compressed binary
                            !iarg = 3 for HDF
                            !iarg = 4 for NetCDF
                            !iarg = 0 for Ascii

to set the output format for a Ufile to be written on unit ILUN.

.. code-block:: fortran

      call UFCMPR(lun,0)

clears the flag, allowing an old style ascii Ufiles data write. Note that ``UFCMPR`` must be called prior to each ``UFOPWR`` call. ``UFCMPR`` does not establish a permanent default.

Comments
********

Historically, comments were written to the end of Ufiles simply using formatted WRITE statements after the Ufile write call (``UF1DWR`` or ``UF2DWR``).

Since compressed Ufiles are binary/unformatted, direct formatted writes of comments appended to the end of the file after data write is no longer a reliable programming method. Instead, use

.. code-block:: fortran

      CALL UFOPCF(ILUNC,IER)

to open a scratch formatted comments file on unit ``ILUNC``. Write the comments on ``ILUNC``. Then,

.. code-block:: fortran

      CALL UCSEND(ILUNC,ILUN)

to send the comments in binary form to the binary compressed output file on unit ``ILUN``. ``UCSEND`` rewinds ``ILUNC`` and uses the ``USPOOL`` subroutine to copy the comments to the binary output file. ``USPOOL`` has been enhanced to support comment output to binary files.

The 16-bit Compressed data write calls generate statistics describing data (usually negligible) error associated with compression. These statistics are appended as comments in the compressed binary output file.

Error Flags
***********

Many Ufiles routines have an error return flag, usually called ``ier`` or ``ierr`` and usually the last argument of the subroutine call.

This flag is set to 0 if the subroutine call is successful. If there is an error, generally a message is written and ``ier`` is set to a value other than zero.

Applications programs should check the return value of ``ier``, especially at file open time and at data read time, since a number of errors can and do occur.

Error Messages
**************

By default the Ufiles subroutine library system will write error messages to stdout. If this is attached to an interactive terminal, the default is also that the user will be prompted to acknowledge the error.

Applications programs may choose different Ufiles system behaviour in response to error situations-- e.g. to prevent error messages from going to stdout.

Read Header
***********

The Ufiles routine ``UFHDCK`` can be used to read the header of an existing Ufile. This can be useful if you want information prior to committing to an attempt to read the Ufile, in particular:

 #. The dimensionality of the Ufile
 #. the size of the data array in the Ufile

``UFHDCK`` can only be called after calling ``UFOPRD`` to open the file, but before calling one of the read routines e.g. ``UF1DRD`` or ``UF2DRD``.

The syntax of the call is:

.. code-block:: fortran

  C  declaration:
      INTEGER INFO(8)

  C   ....

  C  read Ufile header:
        CALL UFHDCK(ILUN,INFO,IER)

This calls the routine ``UFHDCK``:

.. code-block:: fortran

  C  READ HEADER OF UFILE AND RETURN STATISTICAL INFORMATION
  C
      SUBROUTINE UFHDCK(ILUN,INFO,IER)
  C
  C  INPUT:
  C    ILUN -- L.U.N. OF A JUST - OPENED  UFILE
  C
  C  OUTPUT:
  C    INFO -- INFORMATION BLOCK
  C    INFO(1)=FILE DIMENSIONALITY
  C    INFO(2)=TOTAL # OF WORDS IN DEPENDENT DATA ARRAY
  C    INFO(3)=SIZE OF FIRST DIMENSION OF DATA ARRAY
  C    INFO(4)=SIZE OF SECOND DIMENSION OF DATA ARRAY (IF APPLICABLE)
  C     ...
  C    INFO(8)=NUMBER OF SCALARS IN DATA FILE
  C
  C    IER = 0 IF ROUTINE COMPLETES EXECUTION SUCCESSFULLY

``UFHDCK`` rewinds the Ufile after reading the header, so that on exit the Ufile is ready to be read by ``UF1DRD``, ``UF2DRD``, or whatever Ufile  read routine is appropriate.


How Ufiles Work
===============

Ufiles maintains the logical equivalent of a table, stored in COMMON (include file "Ufiles").

This table associates a fortran logical unit number (lun) with filename parts stored as character strings -- the ``ufsetr`` call establishes this association, which is referred to within Ufiles code internal documentation as a channel. Channels are indexed by the lun, and all subsequent calls to do with a given channel use the lun for identification.

Additional table entries track channel status, i.e. whether an actual file is open on a given channel, what kind of access (read or write), what format option (ascii, binary, NetCDF, etc.), and whether the data from an open file has yet been read or written.

Buffers are maintained for compression of the binary representation of floating point numbers, and for input/output of binary Ufiles data.

Up to 64 Ufiles channels can be simultaneously defined and independently maintained within an application.

A word of caution: Ufiles have not been used on multiprocessors for parallel i/o operations. Since Ufiles COMMON contains resources which are shared between channels, parallel i/o operations would probably be unreliable without further careful work on the software.


Working with IDL
================

Ufiles can be read and written from IDL. ``IDL_PATH`` must contain the location of the ``readuf.pro`` and ``w*duf.pro`` files. The ``LD_LIBRARY_PATH`` must contain the location of ``libufshr.so``.

Read Ufiles from IDL
********************

.. code-block::

        IDL> readuf
             lists parameters
  READBUF
  NAME:		READUF
 
  PURPOSE:	Read 1,2,or 3d Ufile, and return everything in the file
                arrays are dynamically allocated
 
  Inputs for Files on Disk:
        disk            - ' '
        dir             - directory enclosed in single quotes ' '
        pref            - file prefix, up to 16 characters, enclosed in ' '
        shot            - long integer shot #, 5(TFTR) or 6(PBXM) digits
        ext             - file suffix, enclosed in ' '

  Inputs for Files in MDSplus:
        disk            - mdspath -- e.g. '.INPUTS'
        dir             - Node Name -- e.g. 'CUR'
        pref            - 'MDS+::<server>' --  e.g. 'MDS+::TRANSPGRID.PPPL.GOV'

       >>if in a Tree where mds-pulse# = shot#:
        shot            - mds pulse number (long integer)
        ext             - MDSplus Tree -- e.g. 'TRW_BUDNY'
       >>if in a TRANSP Tree using PPPL shot-to-pulse translation:
        shot            -  long integer shot # "NOT" mds-pulse#
        ext             - '<tree>@<runid>' -- e.g. 'transp_tftr@37065Y01'
       >>if NOT PPPL && shot != pulse
        shot            -  long integer shot # "NOT" mds pulse number
        ext             - '<tree>(<pulse #>)' -- e.g. 'TRANSP01(12345678)'

 
  Outputs:
        shdate          - shot date
        mach            - tokamak device
        ierr            - 0=ok
        nsc             - number of scalars
        scval           - scalar values array
        sclab           - scalar array descriptions
        iproc           - type of
                          data,0=unprocessed,1=avg,2=smth,3=avg&smth
 
        f               - dependent variable array
        x               - first dimension of variable array
        nx              - number of 1st independent variable data pts
        flab            - label of dependent variable
        xlab            - label of 1st independent variable
 
  If 2D or 3D Ufile:
        y               - second dimension of variable array
        ny              - number of 2nd independent variable data pts
        ylab            - label of 2nd independent variable 
  If 3D Ufile:
        z               - third dimension of variable array
        nz              - number of 3nd independent variable data pts
        zlab            - label of 3nd independent variable 
 
 
  NOTE:   comment not implemented
 
  USAGE: (to read S_V765611.Q0T)
        idl> readuf,' ','/u/mydir','S_V7',65611,'Q0T',  $ ;input
             shdate,mach,ierr, $
             nsc,scval,sclab,iproc,f,x,nx,flab,xlab $
             ,y,ny,ylab                                   ;for 2,3D files
             ,z,nz,zlab                                   ;for 3D file
 

Example to read 1D Ufile from a MDSplus working tree:

.. code-block::

  disk='.inputs'
  dir='CUR'
  pref='MDS+::TRANSP.PPPL.GOV'
  ext='transp_nstx'
  shot=1144630102
  readuf, disk,dir,pref,shot,ext,shdate,mach,ierr,nsc,scval,sclab, $
          iproc,f,x,nx,flab,xlab

  plot,x,f

Write Ufiles from IDL
*********************

* ``W1DUF`` - Write a 1D Ufile, see W3DUF for parameters
* ``W2DUF`` - Write a 2D Ufile, see W3DUF for parameters

.. code-block::

  W3DUF
  NAME:		W3DUF
 
  PURPOSE:	Write 3d Ufile
 
  Inputs:
 	disk		- disk name enclosed in single quotes ' '
 	dir		- directory enclosed in ' '
 	pref		- file prefix, up to 16 characters, enclosed in ' '
 	shot		- long integer shot #, 5(TFTR) or 6(NSTX) digits
 	ext		- file suffix, enclosed in ' '
 	nx		- actual # pts in first dimension of F(x,y,z)
 	ny		- actual # pts in second dimension of F(x,y,z)
 	nz		- actual # pts in third dimension of F(x,y,z) 
 	nsc		- # of scalar values
 	f		- 3-dim array, F(x,y,z)
 	x		- 1st independent variable array
 	y		- 2nd independent variable array
 	z		- 3rd independent variable array
 	scval		- scalar values array
 	sclab		- scalar array descriptions = strarr(nsc)
 	iproc		- type of data,0=unprocessed,1=avg,2=smth,3=avg&smth
 	flab		- label of F(x,y,z)
                          First 20 characters -- NAME of F
                          Last  10 characters --  PHYSICAL UNITS of F
 	xlab		- label of 1st dimension of F(x,y,z)
 	ylab		- label of 2nd dimension of F(x,y,z)
 	zlab		- label of 3rd dimension of F(x,y,z)
 	sdate		- shot date
 	mach		- tokamak device
 	comment		- Ufile comment >>>> not implemented <<<<
  Outputs:
 	ierr		- 0=ok
 
  NOTE:   comment not implemented

  USAGE: (to write 3D /u/mydir/s_v765611.qpr)
 	idl> nx = 36
 	idl> ny = 12
 	idl> nz = 4
 	idl> nsc = 2
 	idl> w3duf,' ','/u/mydir','s_v7',65611,'qpr',nx,ny,nz,nsc, $
 	     f,x,y,z,scval,sclab,iproc,flab,xlab,ylab,zlab, $
             sdate,mach,comment,ierr

Example to create A12345.F1D in working directory, no scalars:

.. code-block::

  time = findgen(100)+1
  dflux = cos(time/5)
  nsc=0
  scval=0
  sclab=' '
  xlab='Time                  second  '
  ylab='Diagm Flx             Wb      '
  mach='NSTX'
  cmt=' '
  iproc=1
  nx=n_elements(time)
  st=systime(0)
  shdate = strmid(st,8,2)+strmid(st,4,3)+'-'+strmid(st,20,4)

  w1duf,' ',' ','a',12345,'f1d',nx,nsc,time,dflux,$
        scval,sclab,iproc,xlab,ylab,shdate,mach,cmt,ier

  print,ier

Example to create A22222.FOO in ``/u/mypath`` with 2 scalars:

.. code-block::

  shot=22222
  dsk=' '
  dir='/u/mypath'
  pref='A'
  ext='FOO'

  time = findgen(100)+1
  f = findgen(100,5)
  y = findgen(5)+1
  ntimes=n_elements(time)
  ny=n_elements(y)
  st=systime(0)
  shdate = strmid(st,8,2)+strmid(st,4,3)+'-'+strmid(st,20,4)

  mach='NSTX'
  cmt='No comment'
  ylab='Rmajor               cm       '
  xlab='Time                  s       '
  iproc=2	; smoothed data
  nsc=2
  scval=findgen(2)+2
  sclab=strarr(2)
  sclab[0] = 'value one:           units1   '
  sclab[1] = 'value two:           units2   '
  flab=      'Te                   eV       '

  w2duf,dsk,dir,pref,shot,ext,ntimes,ny,nsc,f,time,y,%
        scval,sclab,iproc,$
        flab,xlab,ylab,shdate,mach,cmnt,ierr

  print,ierr

Example to create A33333.TEST in ``/u/mypath`` with no scalars:

.. code-block::

  shot=33333
  dsk=' '
  dir='/u/mypath'
  pref='A'
  ext='TEST'

  time = findgen(10)+1
  f = findgen(10,5,3)
  y = findgen(5)+1
  z = findgen(3)+2
  ntimes=n_elements(time)
  ny=n_elements(y)
  nz=n_elements(z)
  shdate='07Aug-2003'
  mach='TFTR'
  cmt='No comment'
  flab='Te                   eV       '
  xlab='Time:                 s       '
  ylab='Rmajor:              cm       '
  zlab='Test:               none      '
  iproc=2	; smoothed data
  nsc=0
  scval=0.
  sclab=' '

  w3duf,dsk,dir,pref,shot,ext,ntimes,ny,nz,nsc,f,time,y,z,$
        scval,sclab,iproc,$
        flab,xlab,ylab,zlab,shdate,mach,cmt,ierr

  print,ierr
