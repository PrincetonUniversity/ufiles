# ufiles

This repository provides the UFILES routines and utilities developed at PPPL for the analysis of data collected from tokamak experiment, in particular, preparation of experimental data profiles as input for the TRANSP analysis code. These libraries and executables were part of the TRANSP software repository and were made available for download through the NTCC webpage. However, TRANSP now expects these routines to be provided as an already compiled program external to TRANSP. This repository will make it easier for other projects that require the UFILES routines to build and debug them on their own.

The legacy documentation is listed below. This is a legacy code through and through, this project was started in 1993 and has evolved over time. This repository is a work in progress as some of the old UFILE capabilities have not been included yet (they were removed from the TRANSP repository before this one was created and need to be restored). The documentation and the CI/CD are also a work in progress. Major rewrites of this code might be tempting but is strongly discouraged as more modern solutions for analyzing experimental data and preparing TRANSP input profiles exist.

## Building

If your platform is not included then following the examples in the `environ` script and the `environs/` directory.

To build:
```
source environ
make
```

If you want to also build the shared libraries:
```
export MAKE_SO=1
source environ
make
```

## Summary of Legacy Documentation

These modules are available through the NTCC Webpage, w3.pppl.gov/NTTC  under Modules Library, as compressed tar-files, <name>.tar.gz, and as zip archives, <name>.zip. Alternatively they can be obtained from ftp.pppl.gov in pub/NTCC/. These modules are described here. Detailed information of each module is provided in http://w3.pppl.gov/NTCC/Ufiles

Note: Installation Notes for Uread, Ufiles, TrGraf and PPPL_Utilities Modules are common in INSTALL, included in each <module>.{tar.gz,zip} archive. Installation notes for Sglib and XtcTek are separate in README_sglib and README_xtctek included in the modules.
```
UREAD         --     Terminal control/interactive program automation.

UFILES        --     Standard for storing physics data.

TRGRAF        --     Interactive Data Display Library.

PPL_UTILITIES --     Tools to study and analyze experimental data.
                     concat       -- Ufiles concatenator tool
                     extrac       -- Ufiles extractor tool
                     gaver{0,1,2} -- Ufile averaging utility
                     gsmoo{0,1,2} -- smoothing/units conversion
                     propane      -- generate Ufiles from hand entered data for TRANSP input
                     sawtoo       -- Ufile sawtooth analyzer
                     splice{2}    -- Ufiles splice utility
                     thin{1,2}    -- Ufile thinning utility
                     ugraf{1,2}   -- Ufiles display programs
                     ulook0       -- scalar Ufile display program
  
SGLIB         --     Scientific Graphics Library.
                     xplot        -- SGLIB output file plotting routine

XtcTek        --     Xtc          -- X-windows Tektronix Controller. (needs Motif)
                     tek2ps       -- convert tektronix to PS format.
```

Description of libraries and programs
```
   uread        ureadsub        -- i/o command interface
                 - nulltron     -- test program
   ufiles       uflib           -- Ufiles i/o subroutine library
                 - uftype       -- program
                mdspro          -- IDL functions to read/write Ufiles 
                mdstransp       -- MDSplus interface    
                ufhdf           -- netCDF/HDF interface
                ureadsub        -- i/o command interface
   trgraf       trgraf          -- display routines
                 - tgtest       -- test program for trgraf
                ureadsub        -- i/o command interface
                sglib           --  graphics library
   uutil        util            -- PPPL utilities
                test            -- demo scripts for each utility
                concat_sub      -- for concat
                extrac_sub      -- for extrac
                rngsel_sub      -- old control routines for Ufiles
                sawcor_sub      -- for sawtoo
                thinsub         -- for thin*
                r4smlib         -- for smoothing
                smlib           -- for smoothing
                interp_sub      -- linear or bilinear interpolation
                ureadsub        -- i/o command interface
                uflib           -- Ufiles i/o subroutine library
                ufhdf           -- netCDF/HDF interface
                mdstransp       -- MDSplus interface
                trgraf          -- display routines
                sglib           -- graphics library
   sglib        - README
                - Makefile         
                  sglib         -- graphics libraries (sg and jc)
                  - xplot       -- program to view a sglib plot file
                  - various test programs 
   xtctek       - README
                - Makefile
                - sources for xtc
                - sources for tek2ps
```

## Contributors

There have been many people who have contributed to the Ufiles suite over many decades. A list of contributors was harvested from the TRANSP repository and is ordered by appearance in the commit log (oldest to newest). Let us know if there are any errors.
```
Doug McCune (original architect)
Marilee Thompson
JP Jeral
Ted Terpstra
Richard	Wieland
PM Stubber
John Schivell
Christina Ludescher-Furth (most commits)
Rob Andre
Eliot Feibush
Jim Conboy
Alex Pletzer
Robert Budny
Greg Colyer
Ben Bariteau
Marina Gorelenkova
Keshavamurthy Indireshkumar	
Xingqiu Yuan
Johan Carlsson
Jai Sachdev
Garrett Wright
Joshua Breslau
Francesca Poli
Gopan Perumpilly
```

## Contributing

Contributions to this repo are welcome, particularly for bug fixes. All changes must be made with a pull request from a branch off of `main`. The pull request must:
- Pass the tests in the continuous integration via GitHub Actions
- Be labeled with `patch`, `minor`, or `major`
- Be reviewed by at least one of the CODEOWNERS

Updates to the `main` branch are automatically tagged with an updated version number. The labels tell the semantic versioning GitHub Action how to update the version number:
- `patch` update for trivial updates and bug fixes
- `minor` update for new functionality
- `major` update for significant new functionality that could break backwards compatibility
The CI will fail if an appropriate label is not used or if more than one is selected. The label can be changed.
