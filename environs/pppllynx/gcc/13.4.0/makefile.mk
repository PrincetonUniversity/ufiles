# Makefile include file for Lynx GCC 13.4.0
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

# Safe for Lynx login/compute nodes
# Do not use -march=znver4 on Lynx Zen2 nodes.
FLUX_FLG = -mtune=generic

# Fortran flags
FFLAGS_OPT = -c -g -O2 -m64 \
	-ffpe-trap=invalid,zero,overflow \
	-fallow-argument-mismatch \
	-fallow-invalid-boz \
	-fno-range-check \
	-fdollar-ok \
	$(FPIC) $(FLUX_FLG)

FFLAGS_DBG = -c -g -O0 -m64 \
	-ffpe-trap=invalid,zero,overflow \
	-fallow-argument-mismatch \
	-fallow-invalid-boz \
	-fno-range-check \
	-fdollar-ok \
	-fbacktrace \
	$(FPIC)

ifeq ($(DEBUG),0)
	FFLAGS = $(FFLAGS_OPT)
else
	FFLAGS = $(FFLAGS_DBG)
endif

FPP = -cpp
MFLAG = -J

# FLIBROOT is set in the Lynx GCC 13.4.0 environment as:
#   export FLIBROOT="${GCC_RUNTIME_ROOT}/lib"
L_FLIBS = -Wl,-rpath,$(FLIBROOT) -L$(FLIBROOT) -lgfortran -lm


# C/C++ flags
CFLAGS_OPT = -c -g -O2 -m64 $(FPIC) $(FLUX_FLG)
CFLAGS_DBG = -c -g -O0 -m64 $(FPIC)

CXXFLAGS_OPT = -c -g -O2 -m64 $(FPIC) $(FLUX_FLG)
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


# Readline etc.
# Keep this separate and place it late in executable link lines
# so /usr/lib64 does not override HDF5/NetCDF libraries.
L_EDIT = -L/usr/lib64 -lreadline -lhistory -ltermcap


# Default include directories
FINCL :=
CINCL :=


# Default definitions
FDEFS := -D__LINUX -D__UNIX -D__F90
CDEFS := -D__LINUX

#FDEFS := $(FDEFS) -D__NOMDSPLUS
#CDEFS := $(CDEFS) -D__NOMDSPLUS

