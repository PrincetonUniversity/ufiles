# Makefile include file for Ranger GCC toolset 13.3.0
#

ifndef MAKE_SO
	MAKE_SO = 0
endif
ifndef DEBUG
	DEBUG = 0
endif

ifeq ($(MAKE_SO),1)
	FPIC := -fPIC
endif
ifeq ($(MAKE_SO),TRUE)
	FPIC := -fPIC
endif

AR = ar rcs

# Safe generic tuning for Ranger.
# Do not use architecture-specific flags unless all nodes support them.
RANGER_FLG = -mtune=generic

# ----------------------------------------------------------------------
# Fortran flags
# ----------------------------------------------------------------------

FFLAGS_OPT = -c -g -O2 -m64 \
	-fallow-argument-mismatch \
	-fallow-invalid-boz \
	-fno-range-check \
	-fdollar-ok \
	$(FPIC) $(RANGER_FLG)

FFLAGS_DBG = -c -g -O0 -m64 \
	-ffpe-trap=invalid,zero,overflow \
	-fallow-argument-mismatch \
	-fallow-invalid-boz \
	-fno-range-check \
	-fdollar-ok \
	-fbacktrace \
	-finit-local-zero \
	-finit-real=zero \
	-finit-integer=0 \
	$(FPIC)

ifeq ($(DEBUG),0)
	FFLAGS = $(FFLAGS_OPT)
else
	FFLAGS = $(FFLAGS_DBG)
endif

FPP = -cpp
MFLAG = -J

# FLIBROOT should be set in the Ranger environment, for example:
#   export FLIBROOT=$(dirname "$(gcc -print-file-name=libgfortran.so)")
L_FLIBS = -Wl,-rpath,$(FLIBROOT) -L$(FLIBROOT) -lgfortran -lm

# ----------------------------------------------------------------------
# C/C++ flags
# ----------------------------------------------------------------------

CFLAGS_OPT = -c -g -O2 -m64 $(FPIC) $(RANGER_FLG)
CFLAGS_DBG = -c -g -O0 -m64 $(FPIC)

CXXFLAGS_OPT = -c -g -O2 -m64 $(FPIC) $(RANGER_FLG)
CXXFLAGS_DBG = -c -g -O0 -m64 $(FPIC)

ifeq ($(DEBUG),0)
	CFLAGS = $(CFLAGS_OPT)
	CXXFLAGS = $(CXXFLAGS_OPT)
else
	CFLAGS = $(CFLAGS_DBG)
	CXXFLAGS = $(CXXFLAGS_DBG)
endif

CPP = -cpp

L_CLIBS = -Wl,-rpath,$(FLIBROOT) -L$(FLIBROOT) -lstdc++ -lgcc_s -lgcc

# ----------------------------------------------------------------------
# Readline etc.
# Keep this separate and place it late in executable link lines.
# ----------------------------------------------------------------------

L_EDIT = -L/usr/lib64 -lreadline -lhistory -ltermcap

# ----------------------------------------------------------------------
# Default include directories
# ----------------------------------------------------------------------

FINCL =
NETCDF_MODDIR ?= /usr/lib64/gfortran/modules
NETCDF_F_HOME ?= /usr
NETCDF_C_HOME ?= /usr
HDF5_HOME     ?= /usr

FINCL +=  -I$(NETCDF_MODDIR) \
         -I$(NETCDF_F_HOME)/include \
         -I$(NETCDF_C_HOME)/include \
         -I$(HDF5_HOME)/include

CINCL :=

# ----------------------------------------------------------------------
# Default definitions
# ----------------------------------------------------------------------

FDEFS := -D__LINUX -D__UNIX -D__F90
CDEFS := -D__LINUX

#FDEFS := $(FDEFS) -D__NOMDSPLUS
#CDEFS := $(CDEFS) -D__NOMDSPLUS
